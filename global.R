#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Synthetic generation code by Deepak Mav, PhD (Sciome, LLC)
# Adapted for Shiny by Parker Combs (NIH/NIEHS)

library(data.table)
library(DT)
library(parallel)
library(reactable)
library(renv)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(uuid)
library(zip)

library(report)
#print(cite_packages())

# Increase file upload size
options(shiny.maxRequestSize=100*1024^2) 

# Save snapshot of libraries
#renv::snapshot()

# Number of cores to use when multiprocessing
N_CORES = 4

# Function to parse input log-transformed expression file
parse.file <- function(file, mode, input_mode, manual_input) {
    
    res <- list()
    # Set error code, if applicable
    res$err <- 0
    
    # This will now filter out non-null samples 
    df <- NULL
    
    # Check that input file is of the correct format
    try_read_file <- tryCatch({
        df <- fread(file=file, sep="\t", header=FALSE, stringsAsFactors=FALSE)
        df <- data.frame(df)
        # Add UUID to each sample name to handle rows with the same sample name
        for(x in seq_len(length(df$V1))){
            if(x > 2){
                df$V1[x] <- paste0(df$V1[x], "___", UUIDgenerate())
            }
        }
        
        row.names(df) <- lapply(seq_len(length(df$V1)), function(x){
            if(x == 1){
                return("Sample_ID")
            } else if(x == 2){
                return("Dose")
            }
            return(df$V1[x])
        })
        df$V1 <- NULL
        df
    }, error=function(cond){
        print(cond)
        NULL
    })
    
    # Fail gracefully if some type of I/O error reading file
    if(is.null(try_read_file)){
        res$err <- 2
        return(list(res=res, label_id=NULL, label_dose=NULL))
    }
    
    # Fail gracefully if file has no rows or columns
    if(nrow(df) < 1 | ncol(df) < 1){
        res$err <- 2
        return(list(res=res, label_id=NULL, label_dose=NULL))
    }
    
    label_id <- rownames(df)[1]
    label_dose <- rownames(df)[2]
    
    # If not supplementing existing samples, ignore anything in the input that isn't null
    if(mode != "supp"){
        tmp <- df[label_dose, ]
        tmp <- tmp[tmp != 0]
        if(length(tmp) > 0){
            print("Warning: non-null samples found in input. Only null samples supported in the current mode. Dropping non-null samples from input...")
            showNotification("Warning: non-null samples found in input. Only null samples supported in the current mode. Dropping non-null samples from input...", type="warning")
            df <- apply(df, 2, function(x){
                if(as.numeric(x[[label_dose]]) == 0){
                    return(x)
                }
                return(NULL)
            })
            
            # Fail gracefully if no null samples provided
            if(is.null(df)){
                res$err <- 3
                return(list(res=res, label_id=NULL, label_dose=NULL))
            }
            
            df <- df[!vapply(df, is.null, FUN.VALUE=logical(1))]
            df <- data.frame(do.call(cbind, df), stringsAsFactors=FALSE)
        }
    }
    
    res$sampleNames <- unname(unlist(df[label_id, ]))
    res$sampleNames <- paste0(res$sampleNames, "__orig") # In case the original samples are named like S1, S2, S3, ..., Sn
    res$nsamples <- length(res$sampleNames)
    res$phenoData <- data.frame(label_id=res$sampleNames, label_dose=as.numeric(unname(unlist(df[label_dose, ]))), stringsAsFactors=FALSE, row.names=res$sampleNames, check.names=FALSE)
    colnames(res$phenoData) <- c(label_id, label_dose)
    res$featureNames <- c(label_id, label_dose)
    res$nfeatures <- 2
    eD <- df[seq(3, nrow(df)), ]
    res$probeNames <- rownames(eD)
    res$nprobes <- length(res$probeNames)
    colnames(eD) <- NULL
    rownames(eD) <- NULL
    
    res$exprData <- do.call(cbind, lapply(eD, FUN=as.numeric))
    
    dimnames(res$exprData) <- list(res$probeNames, res$sampleNames)
    
    res$annoData <- data.frame(label_id=res$probeNames, row.names=res$probenames, stringsAsFactors=FALSE, check.names=FALSE)
    colnames(res$annoData) <- c(label_id)
    return(list(res=res, label_id=label_id, label_dose=label_dose))
}

### Row-wise Truncated Mean and Standard Deviation Functions
rowTMeans <- function(x, LOWER_LIMIT, UPPER_LIMIT) {
    apply(x, MARGIN=1, function(w) {
        w <- w[w > LOWER_LIMIT & w < UPPER_LIMIT]
        ifelse(length(w) > 0, mean(w), 0)
    })
}
rowTSds <- function(x, LOWER_LIMIT, UPPER_LIMIT) {
    apply(x, MARGIN=1, function(w) {
        w <- w[w > LOWER_LIMIT & w < UPPER_LIMIT]
        ifelse(length(w) > 1, sd(w), 0)
    })
}

# Function to generate data files
generate_sample_pool <- function(mode, input_mode, param__input_file, param__input_file_name, param__output_file_name, param__output_file_path, param__num_datafiles, param__sample_pool=1000, param__num_treatment, param__num_repd0, param__num_repnod0, param__num_topdose=0, param__dose_spacing, param__manual_input, param__replacement_orig=FALSE, param__no_negative){
    result <- withProgress(message="Generating data: ", value=0, {
        
        setProgress(0.1, detail="initializing")
        
        # Initialize necessary variables
        INPUT_FILE <- param__input_file
        OUTPUT_DIRECTORY_PARENT <- paste0("./Simulated-Datasets/")
        OUTPUT_DIRECTORY <- paste0("./Simulated-Datasets/", param__output_file_path)
        NUMBER_OF_SIMULATED_DATASETS <- param__num_datafiles
        SEED <- 12412
        NUMBER_OF_CONTROL_REPLICATES <- NULL
        NUMBER_OF_CONTROL_REPLICATES <- param__sample_pool
        NUMBER_OF_TREATMENT_REPLICATES <- param__sample_pool
        PREFIX <- param__output_file_name
        EPSILON <- 1e-8
        LOWER_LIMIT <- 0
        UPPER_LIMIT <- .Machine$integer.max
        
        # Posit Connect gets mad if we don't explicitly set TRUE or FALSE and just pass directly from the input for some reason
        param__replacement <- FALSE
        if(param__replacement_orig == TRUE){
            param__replacement <- TRUE
        }
        
        # Create output directory if it doesn't exist
        dir.create(OUTPUT_DIRECTORY_PARENT, showWarnings=FALSE)
        dir.create(OUTPUT_DIRECTORY, showWarnings=FALSE)
        
        # Set random number generator seed to ensure reproducibility
        set.seed(SEED)
        
        setProgress(0.3, detail="parsing input file")
        
        ## Parse Input File
        tmp_parse <- parse.file(file=INPUT_FILE, mode=mode, input_mode=input_mode, manual_input=param__manual_input)
        
        iarray <- tmp_parse[["res"]]
        label_id <- tmp_parse[["label_id"]]
        label_dose <- tmp_parse[["label_dose"]]
        
        # error checking with file parse process
        if(iarray$err == 1){ # bad manual dosages
            return(1)
        } else if(iarray$err == 2){ # mangled file
            return(2)
        } else if(iarray$err == 3){ # no null samples provided
            return(3)
        }
        
        setProgress(0.5, detail="identifying dose groups")
        
        # Deepak's generation code:
        
        ### Preprocess to identify dose groups and corresponding number of original replicates
        
        fD <- table(iarray$phenoData[, label_dose])
        
        DOSE_LEVELS <- as.numeric(names(fD)) # these are the names of each dose level in the original input file
        fD <- as.integer(fD) # these are the counts of each dose level (# reps) in the original input file
        NUMBER_OF_DOSES <- length(fD) # this is the number of unique dose categories in the original input file
        sD <- NULL
        dose.index <- NULL
        sD <- c(NUMBER_OF_CONTROL_REPLICATES, rep(NUMBER_OF_TREATMENT_REPLICATES, NUMBER_OF_DOSES - 1)) # these are final counts for each dose category after reps
        dose.index <- rep(seq(0, (NUMBER_OF_DOSES - 1)), c(NUMBER_OF_CONTROL_REPLICATES, rep(NUMBER_OF_TREATMENT_REPLICATES, NUMBER_OF_DOSES - 1))) # this assigns an index (starting at 0) to each rep based on category
        
        rep.index <- c()
        if(mode == "supp"){ # If supplementing, we want to add the samples onto what is already there
            rep.index <- c(seq_len(NUMBER_OF_CONTROL_REPLICATES), rep(seq_len(NUMBER_OF_TREATMENT_REPLICATES), NUMBER_OF_DOSES - 1))
        } else { # Otherwise we just care about the generated samples
            rep.index <- seq_len(NUMBER_OF_CONTROL_REPLICATES)
        }
        
        oarray <- iarray[c("probeNames", "probeNames", "nprobes", "featureNames", "nfeatures")]
        
        setProgress(0.7, detail="synthesizing data points")
        
        # Name the samples like S1, S2, S3, ..., Sn for standardization internally
        if(mode == "supp"){
            oarray$sampleNames <- unlist(lapply(seq_len(NUMBER_OF_CONTROL_REPLICATES + (NUMBER_OF_TREATMENT_REPLICATES * (NUMBER_OF_DOSES - 1))), function(x) paste0("S", x)))
        } else {
            oarray$sampleNames <- unlist(lapply(seq_len(NUMBER_OF_CONTROL_REPLICATES), function(x) paste0("S", x)))
        }
        
        for(d in seq_len(NUMBER_OF_DOSES)) {
            index.o <- which(dose.index == (d - 1))
            index.i <- which(iarray$phenoData[, label_dose] == DOSE_LEVELS[d])
            C.d <- min(sD[d], fD[d])
            oarray$sampleNames[index.o[seq_len(C.d)]] <- iarray$sampleNames[index.i[seq_len(C.d)]]
        }
        
        oarray$phenoData <- data.frame(label_id=oarray$sampleNames, label_dose=DOSE_LEVELS[dose.index + 1], row.names=oarray$sampleNames, stringsAsFactors=FALSE, check.names=FALSE)
        colnames(oarray$phenoData) <- c(label_id, label_dose)
        
        oarray$nsamples <- length(dose.index)
        
        sL <- split(iarray$sampleNames, iarray$phenoData[, label_dose])
        
        ### Compute dose-wise rate of out of bounds expression for each probe/gene/transcript
        pi.L <- do.call(cbind, lapply(sL, function(y) rowMeans(iarray$exprData[, y, drop=FALSE] <= LOWER_LIMIT)))
        pi.U <- do.call(cbind, lapply(sL, function(y) rowMeans(iarray$exprData[, y, drop=FALSE] >= UPPER_LIMIT)))
        mu <- do.call(cbind, lapply(sL, function(y) rowTMeans(iarray$exprData[, y, drop=FALSE], LOWER_LIMIT, UPPER_LIMIT)))
        sigma <- do.call(cbind, lapply(sL, function(y) rowTSds(iarray$exprData[, y, drop=FALSE], LOWER_LIMIT, UPPER_LIMIT)))
        residual <-do.call(cbind, lapply(sL, function(y) t(apply(iarray$exprData[, y, drop=FALSE], MARGIN=1, function(w) {
            iw <- which(w > LOWER_LIMIT & w < UPPER_LIMIT)
            if(length(iw) > 0) w[iw] <- w[iw] - mean(w[iw])
            if(length(iw) != length(w)) w[-iw] <- 0
            return(w)
        }))))
        
        sigma.star <- sqrt(rowMeans(residual^2) / (1 - pi.L - pi.U))
        sigma.star[sigma.star < EPSILON] <- EPSILON
        oarray$exprData <- matrix(NA, nrow=oarray$nprobes, ncol=oarray$nsamples, dimnames=list(oarray$probeNames, oarray$sampleNames))
        
        ## use existing samples;
        carried.samples <- intersect(oarray$sampleNames, iarray$sampleNames)
        oarray$exprData[, carried.samples] <- iarray$exprData[, carried.samples]
        
        ### Identify doses needing imputation;
        SELECT_DOSES <- which(sD > fD)
        
        ### Generating Imputed Replicates
        for(d in SELECT_DOSES) {
            nO <- min(sD[d], fD[d])
            index.o <- which(oarray$phenoData[label_dose] == DOSE_LEVELS[d])
            sigma.d <- sigma[, d]
            w.d <- which(sigma.d < EPSILON)
            if(length(w.d) > 0) sigma.d[w.d] <- sigma.star[w.d] ## Used overall variability when any dose category has only 1 real replicate;
            phi.a <- pnorm(LOWER_LIMIT, mean=mu[, d], sd=sigma.d)
            phi.b <- pnorm(UPPER_LIMIT, mean=mu[, d], sd=sigma.d)
            for(m in seq((nO + 1), sD[d])) {
                phi.z <- phi.a + runif(iarray$nprobes) * (phi.b - phi.a)
                z <- mu[, d] + sigma.d * qnorm(phi.z)
                u <- runif(iarray$nprobes)
                z[u <= pi.L[, d]] <- LOWER_LIMIT
                z[(pi.L[, d] <= u)  & (u <= (pi.U[, d] + pi.L[, d]))] <- UPPER_LIMIT
                z[is.na(z)] <- 0
                oarray$exprData[, index.o[m]] <- z
            }
        }
        
        setProgress(0.9, detail="writing to file(s)")
        
        if(mode == "supp"){
            full_df <- rbind.data.frame(label_dose=t(oarray$phenoData[label_dose]), oarray$exprData)
            colnames(full_df) <- unlist(lapply(seq_len(length(oarray$sampleNames)), function(x) paste0("S", x)))
            dose_counts <- fD
            names(dose_counts) <- DOSE_LEVELS
            rownames(full_df) <- c(label_dose, unlist(rownames(full_df)[seq(2, length(rownames(full_df)))]))
            spl_df <- as.data.frame(t(full_df))
            spl_df <- split(spl_df, spl_df[label_dose])
            spl_df <- lapply(spl_df, function(x) t(x))
            spl_df_orig <- lapply(names(spl_df), function(x) spl_df[[x]][, seq_len(dose_counts[[x]])])
            names(spl_df_orig) <- names(spl_df)
            
            spl_df_synth <- lapply(names(spl_df), function(x) spl_df[[x]][, seq(dose_counts[[x]] + 1, ncol(spl_df[[x]]))])
            names(spl_df_synth) <- names(spl_df)
            
            setProgress(0.95, detail="writing to file(s)")
            
            for(ds in seq_len(NUMBER_OF_SIMULATED_DATASETS)) {
                OUTPUT_FILE <- file.path(OUTPUT_DIRECTORY, paste0(PREFIX, "_", formatC(ds, format="d", flag="0", digits=floor(log10(NUMBER_OF_SIMULATED_DATASETS))), ".txt"))
                synth_cols <-  lapply(names(spl_df_synth), function(x) {
                    if(x == "0"){
                        return(sample(colnames(spl_df_synth[[x]]), param__num_repd0, replace=param__replacement))
                    }
                    return(sample(colnames(spl_df_synth[[x]]), param__num_repnod0, replace=param__replacement))
                })
                names(synth_cols) <- names(spl_df_synth)
                # Get original column names
                orig_cols <- lapply(seq_len(length(spl_df_orig)), function(x) colnames(spl_df_orig[[x]]))
                names(orig_cols) <- names(spl_df_orig)
                full_cols <- unlist(lapply(names(spl_df_orig), function(x) c(unlist(orig_cols[[x]]), unlist(synth_cols[[x]]))))
                rdm_sample_pool <- full_df[, full_cols]
                # Define names & doses
                sample_names <- t(data.frame(c(label_id, unlist(colnames(rdm_sample_pool))), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE))
                ### Saved imputed and original replicate file in tab delimited format
                
                # Remove UUIDs
                Sample_ID <- unlist(lapply(rownames(rdm_sample_pool), function(x) unlist(str_split(x, "___"))[1]))
                rdm_sample_pool <- cbind(Sample_ID, rdm_sample_pool)
                
                fwrite(sample_names, file=OUTPUT_FILE, row.names=FALSE, na="", sep="\t", quote=FALSE, append=FALSE, col.names=FALSE)
                fwrite(rdm_sample_pool, file=OUTPUT_FILE, row.names=FALSE, na="", sep="\t", quote=FALSE, append=TRUE, col.names=FALSE)
            }
        } else {
            OUTPUT_FILE <- file.path(OUTPUT_DIRECTORY, paste0("Null_Samples.txt"))
            
            # Generate N null synthetic samples "pool"
            sample_doses <- unlist(lapply(seq_len(param__sample_pool), function(x) 0))
            sample_doses <- data.frame(label_dose=label_dose, matrix(sample_doses, nrow=1, ncol=length(sample_doses), dimnames=list(label_dose, sample_doses)), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
            sample_pool <- data.frame(label_id=oarray$probeNames, oarray$exprData, row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
            
            colnames(sample_pool) <- c(label_id, unlist(lapply(seq(length(colnames(sample_pool)) - 1), function(x) paste0("S", x))))
            sample_names <- t(data.frame(colnames(sample_pool), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE))
            
            setProgress(0.95, detail="writing to file(s)")
            # Save null synthetic samples to file"
            # if "noedf" mode, just save all generated samples to file
            if(mode == "noedf"){
                fwrite(sample_names, file=OUTPUT_FILE, row.names=FALSE, na="", sep="\t", quote=FALSE, append=FALSE, col.names=FALSE)
                fwrite(sample_doses, file=OUTPUT_FILE,row.names=FALSE, na="", sep="\t", quote=FALSE, append=TRUE, col.names=FALSE)
     
                # Remove UUIDs
                sample_pool$Sample_ID <- lapply(sample_pool$Sample_ID, function(x) unlist(str_split(x, "___"))[1])
                
                # Write samples to expression data file(s)
                fwrite(sample_pool, file=OUTPUT_FILE, row.names=FALSE, na="", sep="\t", quote=FALSE, append=TRUE, col.names=FALSE)
                
            } else {
                
                # We don't want to include the Sample_ID column in our sample
                sample_pool[[label_id]] <- NULL
                
                # else if not noedf mode, randomly sample M times from the N samples into M separate files
                lapply(seq_len(NUMBER_OF_SIMULATED_DATASETS), function(ds) {
                    if(input_mode == "auto"){
                        # Take random sample
                        rdm_sample <- sample(colnames(sample_pool), (param__num_repd0 + (param__num_repnod0 * (param__num_treatment - 1))), replace=param__replacement)
                        rdm_sample_pool <- sample_pool[, rdm_sample]
                        rdm_sample_pool <- data.frame(label_id=oarray$probeNames, rdm_sample_pool, row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
                        colnames(rdm_sample_pool) <- c(label_id, unlist(colnames(rdm_sample_pool)[seq(2, length(colnames(rdm_sample_pool)))]))
                        # Calculate dosages
                        spacing_full <- c()
                        size_min_0 <- param__num_treatment - 1
                        if(param__dose_spacing == "tenth"){
                            spacing_full <- rev(unlist(lapply(seq_len(size_min_0), function(x) param__num_topdose / (10^(x-1)))))
                        } else if(param__dose_spacing == "third"){
                            spacing_full <- unlist(lapply(seq_len(size_min_0), function(x) {
                                tmp_1 <- param__num_topdose / (10^(x-1))
                                tmp_3 <- tmp_1 * 3.3
                                return(c(tmp_3, tmp_1))
                            }))
                            spacing_full<- spacing_full[spacing_full <= param__num_topdose]
                            spacing_full <- rev(unlist(lapply(seq_len(size_min_0), function(x) spacing_full[x])))
                        } else if(param__dose_spacing == "half"){
                            spacing_full <- rev(unlist(lapply(seq_len(size_min_0), function(x) param__num_topdose / (2^(x-1)))))
                        }
                        spacing_full <- unlist(lapply(spacing_full, function(x) rep.int(x, param__num_repnod0)))
                        spacing_full <- c(rep.int(0, param__num_repd0), spacing_full)
                        sample_doses <- data.frame(label_dose=label_dose, matrix(spacing_full, nrow=1, ncol=length(spacing_full), dimnames=list(label_dose, spacing_full)), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
                        
                        # Get sample names
                        sample_names <- t(data.frame(colnames(rdm_sample_pool), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE))
                    } else if(input_mode == "manual"){
                        manual_total <- sum(unname(unlist(lapply(param__manual_input, function(x) x$reps))))
                        # Take random sample
                        rdm_sample <- sample(colnames(sample_pool), manual_total, replace=param__replacement)
                        rdm_sample_pool <- sample_pool[, rdm_sample]
                        rdm_sample_pool <- data.frame(label_id=oarray$probeNames, rdm_sample_pool, row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
                        colnames(rdm_sample_pool) <- c(label_id, unlist(colnames(rdm_sample_pool)[seq(2, length(colnames(rdm_sample_pool)))]))
                        # map manually-input sample dosages
                        spacing_full <- unname(unlist(lapply(param__manual_input, function(x) rep.int(x$dose, x$reps))))
                        sample_doses <- data.frame(label_dose=label_dose, matrix(spacing_full, nrow=1, ncol=length(spacing_full), dimnames=list(label_dose, spacing_full)), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE)
                        
                        # Get sample names
                        sample_names <- t(data.frame(colnames(rdm_sample_pool), row.names=NULL, check.names=FALSE, stringsAsFactors=FALSE))
                    }
                    
                    # Write samples to expression data file(s)
                    file_name <- file.path(OUTPUT_DIRECTORY, paste0("Null__", param__input_file_name, "__", ds, ".txt"))
                    fwrite(sample_names, file=file_name, row.names=FALSE, na="", sep="\t", quote=FALSE, append=FALSE, col.names=FALSE)
                    fwrite(sample_doses, file=file_name,row.names=FALSE, na="", sep="\t", quote=FALSE, append=TRUE, col.names=FALSE)
                    
                    # Remove UUIDs
                    rdm_sample_pool$Sample_ID <- lapply(rdm_sample_pool$Sample_ID, function(x) unlist(str_split(x, "___"))[1])
                    
                    fwrite(rdm_sample_pool, file=file_name, row.names=FALSE, na="", sep="\t", quote=FALSE, append=TRUE, col.names=FALSE)
                })
            }
        }
        setProgress(1.00, detail="finishing")
        return(0)
    })
    print("... finished.")
    return(result)
}