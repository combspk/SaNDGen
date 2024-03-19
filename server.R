server <- function(input, output, session) {
    
    reac__uploaded_files <- reactiveValues(dt=data.frame("Filename"=character(0), "Size"=character(0), "Path"=character(0), "Remove"=character(0)))
    reac__manual_input <- reactiveValues(el=NULL)
    reac__manual_input_names <- reactiveValues(names=list())
    
    observeEvent(input$btn__add_dose, {
        new_id <- UUIDgenerate()
        
        reac__manual_input$el <- append(reac__manual_input$el,
                                        list(
                                            fluidRow(
                                                column(4,
                                                       numericInput(inputId=paste0("input__manual_dose__", new_id), label=paste0("Dose ", length(reac__manual_input$el) + 1), min=0, max=100000, step=0.0001, value=0)
                                                ),
                                                column(4,
                                                       numericInput(inputId=paste0("input__manual_reps__", new_id), label=paste0("# Reps"), min=1, max=100, step=1, value=1)
                                                ),
                                                column(4,
                                                       actionButton(inputId=paste0("btn__remove__", new_id), label=NULL, icon=icon("trash"), lib="glyphicons")
                                                )
                                            )
                                        )
        )
        reac__manual_input$el <- reac__manual_input$el[!vapply(reac__manual_input$el, is.null, FUN.VALUE=logical(1))]
        reac__manual_input_names$names <- append(reac__manual_input_names$names, new_id)
        names(reac__manual_input$el) <- reac__manual_input_names$names
        for(x in seq_len(length(names(reac__manual_input$el)))){
            # fix input labels
            updateNumericInput(session, inputId=paste0("input__manual_dose__", names(reac__manual_input$el)[x]), label=paste0("Dose ", x), value=input[[paste0("input__manual_dose__", names(reac__manual_input$el)[x])]])
            updateNumericInput(session, inputId=paste0("input__manual_reps__", names(reac__manual_input$el)[x]), label=paste0("# Reps"), value=input[[paste0("input__manual_reps__", names(reac__manual_input$el)[x])]])
        }
        
        observeEvent(input[[paste0("btn__remove__", new_id)]], {
            reac__manual_input$el <- reac__manual_input$el[names(reac__manual_input$el) != new_id]
            reac__manual_input_names$names <- reac__manual_input_names$names[reac__manual_input_names$names != new_id]
            for(x in seq_len(length(names(reac__manual_input$el)))){
                # fix input labels
                updateNumericInput(session, inputId=paste0("input__manual_dose__", names(reac__manual_input$el)[x]), label=paste0("Dose ", x), value=input[[paste0("input__manual_dose__", names(reac__manual_input$el)[x])]])
                updateNumericInput(session, inputId=paste0("input__manual_reps__", names(reac__manual_input$el)[x]), label=paste0("# Reps"), value=input[[paste0("input__manual_reps__", names(reac__manual_input$el)[x])]])
            }
        }, once=TRUE)
        
        output$container__manual_input <- renderUI({
            reac__manual_input$el
        })
        
    })
    
    observeEvent(input$input__dose_input_type, {
        reac__manual_input$el <- NULL
        reac__manual_input_names$names <- list()
        if(input$input__dose_input_type == "auto") {
            if(input$input__output_mode == "edf"){
                shinyjs::show("input__auto_edf")
                shinyjs::hide("input__auto_supp")
                shinyjs::hide("input__manual")
            } else if(input$input__output_mode == "supp"){
                shinyjs::hide("input__auto_edf")
                shinyjs::show("input__auto_supp")
                shinyjs::hide("input__manual")
            }
        } else if(input$input__dose_input_type == "manual") {
            shinyjs::hide("input__auto_edf")
            shinyjs::hide("input__auto_supp")
            shinyjs::show("input__manual")
        }
    })
    
    observeEvent(input$input__output_mode, {
        if(input$input__output_mode == "noedf"){
            shinyjs::hide("options__edf")
            shinyjs::show("options__noedf")
        } else {
            shinyjs::show("options__edf")
            shinyjs::hide("options__noedf")
            
            if(input$input__output_mode == "edf"){
                shinyjs::show("input__dose_input_type")
                
                if(input$input__dose_input_type == "auto") {
                    if(input$input__output_mode == "edf"){
                        shinyjs::show("input__auto_edf")
                        shinyjs::hide("input__auto_supp")
                        shinyjs::hide("input__manual")
                    } else if(input$input__output_mode == "supp"){
                        shinyjs::hide("input__auto_edf")
                        shinyjs::show("input__auto_supp")
                        shinyjs::hide("input__manual")
                    }
                } else if(input$input__dose_input_type == "manual") {
                    shinyjs::hide("input__auto_edf")
                    shinyjs::hide("input__auto_supp")
                    shinyjs::show("input__manual")
                }
                
            } else if(input$input__output_mode == "supp"){
                shinyjs::hide("input__dose_input_type")
                shinyjs::hide("input__auto_edf")
                shinyjs::show("input__auto_supp")
                shinyjs::hide("input__manual")
            }
        }
    })
    
    observeEvent(input$input__sample_file, {
        shinyjs::hide("btn__download_zip")
        shinyjs::show("file_display")
        shinyjs::enable("btn__go")
        
        # Process uploaded files for display
        uploaded_files_df <- input$input__sample_file
        
        uploaded_files_df$type <- NULL
        uploaded_files_df$size <- lapply(uploaded_files_df$size, function(x) {
            size_units <- "KB"
            tmp <- as.numeric(x) / 1000
            if(tmp > 1000){
                size_units <- "MB"
                tmp <- as.numeric(x) / 1000000
            }
            return(paste0(round(tmp, digits=2), size_units))
        }) # convert file size to kb/mb
        
        uploaded_files_df$remove <- lapply(seq_len(nrow(uploaded_files_df)), function(x){
            as.character(
                actionButton(
                    inputId=paste0("btn__remove_file__", x),
                    label=NULL,
                    icon=icon("trash"),
                    lib="glyphicon",
                    onclick=paste0("Shiny.setInputValue(\'remove_file\', ", x, ", {priority: 'event'})")
                )
            )
        })
        colnames(uploaded_files_df) <- c("Filename", "Size", "Path", "Remove")
        reac__uploaded_files$dt <- uploaded_files_df
        output$output__uploaded_files <- DT::renderDataTable({
            DT::datatable(
                uploaded_files_df[, c("Filename", "Size", "Remove")],
                caption="Uploaded files",
                options=list(dom="t"),
                selection="none",
                escape=FALSE
            )
        })
    })
    
    observeEvent(input$remove_file, {
        reac__uploaded_files$dt <- reac__uploaded_files$dt[(row.names(reac__uploaded_files$dt) != input$remove_file),]
        uploaded_files_df <- reac__uploaded_files$dt
        if(nrow(uploaded_files_df) < 1){
            shinyjs::hide("file_display")
        } else {
            output$output__uploaded_files <- DT::renderDataTable({
                DT::datatable(
                    uploaded_files_df[, c("Filename", "Size", "Remove")],
                    caption="Uploaded files",
                    options=list(dom="t"),
                    selection="none",
                    escape=FALSE
                )
            })
        }
    })
    
    output$output__file_viz <- renderUI({
        h4("Generate data first to view a preview here.")
    })
    
    dl_uuid <- reactiveValues(uuid=NULL)
    observeEvent(input$btn__go, {
        
        # Hide download button
        shinyjs::hide("btn__download_zip")
        shinyjs::disable("btn__go")
        
        if(nrow(reac__uploaded_files$dt) < 1){
            showNotification("Error: no input files provided.", type="error")
            shinyjs::enable("btn__go")
            return(FALSE)
        }
        
        if(input$input__output_mode != "supp" & nrow(reac__uploaded_files$dt) > 1){
            showNotification("Error: multiple input files are not supported in this mode.", type="error")
            shinyjs::enable("btn__go")
            return(FALSE)
        }
        
        # #Check if sufficient samples will be generated
        if(input$input__replacement == FALSE){
            if(input$input__dose_input_type == "auto"){
                if(input$input__output_mode == "edf"){
                    if(input$input__num_synthetic_samples_pool < (input$input__num_rep_d0 + (input$input__num_rep_nod0 * (input$input__num_treatment_groups- 1)))){
                        showNotification("Error: not enough samples will be generated to sample without replacement. Either increase the sample pool size or allow sampling with replacement.", type="error")
                        shinyjs::enable("btn__go")
                        return(FALSE)
                    }
                } else if(input$input__output_mode == "supp"){
                    if(input$input__num_synthetic_samples_pool < input$input__num_rep_d0_supp | input$input__num_synthetic_samples_pool < input$input__num_rep_nod0_supp){
                        showNotification("Error: not enough samples will be generated to sample without replacement. Either increase the sample pool size or allow sampling with replacement.", type="error")
                        shinyjs::enable("btn__go")
                        return(FALSE)
                    }
                }
            }
        }
        
        set_uuid <- UUIDgenerate()
        dl_uuid$uuid <- set_uuid
        
        if(Sys.info()["sysname"] == "Windows"){
            proc_status_list <- apply(reac__uploaded_files$dt, 1, function(x){
                # parse out manual input ids
                manual_input_list <- list()
                if(input$input__dose_input_type == "manual"){
                    manual_input_list <- lapply(seq_len(length(names(reac__manual_input$el))), function(x){
                        list(
                            dose=input[[paste0("input__manual_dose__", names(reac__manual_input$el)[x])]],
                            reps=input[[paste0("input__manual_reps__", names(reac__manual_input$el)[x])]]
                        )
                    })
                    names(manual_input_list) <- names(reac__manual_input$el)
                    # Show error if no dosage levels specified
                    if(length(manual_input_list) < 1){
                        showNotification("Error: no dosage levels specified.", type="error")
                        shinyjs::enable("btn__go")
                        return(1)
                    }
                    
                    if(input$input__replacement == FALSE){
                        bad_sets <- c()
                        if(input$input__output_mode == "edf"){
                            if(input$input__num_synthetic_samples_pool < sum(unlist(lapply(manual_input_list, function(x) x$reps)))){
                                showNotification(paste0("Error: not enough samples will be generated to sample without replacement for dosage level(s): ", paste0(bad_sets, collapse=", "), ". Either increase the sample pool size or allow sampling with replacement."), type="error")
                                shinyjs::enable("btn__go")
                                return(1)
                            }
                        } else if(input$input__output_mode == "supp"){
                            bad_sets <- unlist(lapply(manual_input_list, function(x){
                                if(x$reps > input$input__num_synthetic_samples_pool){
                                    return(x$dose)
                                }
                            }))
                            if(length(bad_sets) > 0){
                                showNotification(paste0("Error: not enough samples will be generated to sample without replacement for dosage level(s): ", paste0(bad_sets, collapse=", "), ". Either increase the sample pool size or allow sampling with replacement."), type="error")
                                shinyjs::enable("btn__go")
                                return(1)
                            }
                        }
                    }
                }
                
                if(input$input__output_mode == "noedf"){
                    proc_status <- generate_sample_pool(
                        mode="noedf",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=1,
                        param__sample_pool=input$input__num_synthetic_samples,
                        param__num_treatment=input$input__num_treatment_groups,
                        param__num_repd0=input$input__num_synthetic_samples,
                        param__num_repnod0=0,
                        param__num_topdose=input$input__num_top_dose,
                        param__dose_spacing=input$input__dose_spacing,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                } else if (input$input__output_mode == "edf"){
                    proc_status <- generate_sample_pool(
                        mode="edf",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=input$input__num_edf,
                        param__sample_pool=input$input__num_synthetic_samples_pool,
                        param__num_treatment=input$input__num_treatment_groups,
                        param__num_repd0=input$input__num_rep_d0,
                        param__num_repnod0=input$input__num_rep_nod0,
                        param__num_topdose=input$input__num_top_dose,
                        param__dose_spacing=input$input__dose_spacing,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                } else if (input$input__output_mode == "supp"){
                    proc_status <- generate_sample_pool(
                        mode="supp",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=input$input__num_edf,
                        param__sample_pool=input$input__num_synthetic_samples_pool,
                        param__num_treatment=input$input__num_treatment_groups_supp,
                        param__num_repd0=input$input__num_rep_d0_supp,
                        param__num_repnod0=input$input__num_rep_nod0_supp,
                        param__num_topdose=input$input__num_top_dose_supp,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                }
                
                # error message handling
                if(proc_status == 1){ # bad manual dosages
                    showNotification(paste0("Error (", x[["Filename"]], "): bad dosages supplied. Check that the dosage levels you have supplied match those in your input file and try again."), type="error")
                    shinyjs::enable("btn__go")
                } else if(proc_status == 2){ # bad manual dosages
                    showNotification(paste0("Error (", x[["Filename"]], "): bad input file format. Ensure this input file is of the correct format."), type="error")
                    shinyjs::enable("btn__go")
                } else if(proc_status == 3){ # no null samples provided
                    showNotification(paste0("Error (", x[["Filename"]], "): No null labels found in input file. You must supply null samples if synthesizing null data."), type="error")
                    shinyjs::enable("btn__go")
                }
                return(proc_status)
            })
        } else {
            proc_status_list <- apply(reac__uploaded_files$dt, 1, function(x) c(x["Filename"], x["Path"]))
            proc_status_list <- mclapply(proc_status_list, mc.silent=FALSE, mc.cores=N_CORES, function(x){
                
                # parse out manual input ids
                manual_input_list <- list()
                if(input$input__dose_input_type == "manual"){
                    manual_input_list <- lapply(seq_len(length(names(reac__manual_input$el))), function(x){
                        list(
                            dose=input[[paste0("input__manual_dose__", names(reac__manual_input$el)[x])]],
                            reps=input[[paste0("input__manual_reps__", names(reac__manual_input$el)[x])]]
                        )
                    })
                    names(manual_input_list) <- names(reac__manual_input$el)
                    # Show error if no dosage levels specified
                    if(length(manual_input_list) < 1){
                        showNotification("Error: no dosage levels specified.", type="error")
                        shinyjs::enable("btn__go")
                        return(1)
                    }
                    
                    if(input$input__replacement == FALSE){
                        bad_sets <- c()
                        if(input$input__output_mode == "edf"){
                            if(input$input__num_synthetic_samples_pool < sum(unlist(lapply(manual_input_list, function(x) x$reps)))){
                                showNotification(paste0("Error: not enough samples will be generated to sample without replacement for dosage level(s): ", paste0(bad_sets, collapse=", "), ". Either increase the sample pool size or allow sampling with replacement."), type="error")
                                shinyjs::enable("btn__go")
                                return(1)
                            }
                        } else if(input$input__output_mode == "supp"){
                            bad_sets <- unlist(lapply(manual_input_list, function(x){
                                if(x$reps > input$input__num_synthetic_samples_pool){
                                    return(x$dose)
                                }
                            }))
                            if(length(bad_sets) > 0){
                                showNotification(paste0("Error: not enough samples will be generated to sample without replacement for dosage level(s): ", paste0(bad_sets, collapse=", "), ". Either increase the sample pool size or allow sampling with replacement."), type="error")
                                shinyjs::enable("btn__go")
                                return(1)
                            }
                        }
                    }
                }
                
                if(input$input__output_mode == "noedf"){
                    proc_status <- generate_sample_pool(
                        mode="noedf",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=1,
                        param__sample_pool=input$input__num_synthetic_samples,
                        param__num_treatment=input$input__num_treatment_groups,
                        param__num_repd0=input$input__num_synthetic_samples,
                        param__num_repnod0=0,
                        param__num_topdose=input$input__num_top_dose,
                        param__dose_spacing=input$input__dose_spacing,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                } else if (input$input__output_mode == "edf"){
                    proc_status <- generate_sample_pool(
                        mode="edf",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=input$input__num_edf,
                        param__sample_pool=input$input__num_synthetic_samples_pool,
                        param__num_treatment=input$input__num_treatment_groups,
                        param__num_repd0=input$input__num_rep_d0,
                        param__num_repnod0=input$input__num_rep_nod0,
                        param__num_topdose=input$input__num_top_dose,
                        param__dose_spacing=input$input__dose_spacing,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                } else if (input$input__output_mode == "supp"){
                    proc_status <- generate_sample_pool(
                        mode="supp",
                        input_mode=input$input__dose_input_type,
                        param__input_file=x[["Path"]],
                        param__input_file_name=x[["Filename"]],
                        param__output_file_path=paste0(set_uuid),
                        param__output_file_name=paste0(set_uuid, "__", x[["Filename"]]),
                        param__num_datafiles=input$input__num_edf,
                        param__sample_pool=input$input__num_synthetic_samples_pool,
                        param__num_treatment=input$input__num_treatment_groups_supp,
                        param__num_repd0=input$input__num_rep_d0_supp,
                        param__num_repnod0=input$input__num_rep_nod0_supp,
                        param__num_topdose=input$input__num_top_dose_supp,
                        param__manual_input=manual_input_list,
                        param__replacement_orig=input$input__replacement,
                        param__no_negative=input$input__noneg
                    )
                }
                
                # error message handling
                if(proc_status == 1){ # bad manual dosages
                    showNotification(paste0("Error (", x[["Filename"]], "): bad dosages supplied. Check that the dosage levels you have supplied match those in your input file and try again."), type="error")
                    shinyjs::enable("btn__go")
                } else if(proc_status == 2){ # bad manual dosages
                    showNotification(paste0("Error (", x[["Filename"]], "): bad input file format. Ensure this input file is of the correct format."), type="error")
                    shinyjs::enable("btn__go")
                }
                return(proc_status)
            })
        }

        proc_status_list <- proc_status_list[proc_status_list == 0]
        
        if(length(proc_status_list) > 0){
            shinyjs::show("btn__download_zip")
            
            # Show previews for generated files
            outfiles <- Sys.glob(paste0("Simulated-Datasets/", dl_uuid$uuid, "/*"))
            output$output__file_viz <- renderUI({
                do.call(tabsetPanel,
                    lapply(paste0(outfiles), function(x){
                        tmp_panel <- tabPanel(
                            title=unlist(str_split(x, "/"))[3],
                            reactableOutput(outputId=paste0("tab__", x))
                        )
                        tryCatch({
                            tmp <- fread(x)
                            output[[paste0("tab__", x)]] <- renderReactable({
                                reactable(
                                    tmp,
                                    searchable=TRUE,
                                    showPageSizeOptions=TRUE,
                                    pageSizeOptions=c(10, 50, 100),
                                    defaultPageSize=10
                                )
                            })
                        }, error=function(cond){
                            output[[paste0("tab__", x)]] <- renderReactable({data.frame()})
                        })
                        return(tmp_panel)
                    })
                )
            })
        } else {
            output$output__file_viz <- renderUI({"No files were generated."})
        }
        shinyjs::enable("btn__go")
    })
    
    output$btn__download_example <- downloadHandler(
        filename=function() {
            "Example_control.txt"
        },
        content=function(file) {
            tmp <- fread(file="./examples/Example_control.txt", sep="\t", stringsAsFactors=FALSE)
            fwrite(tmp, file, row.names=FALSE, sep="\t", quote=FALSE)
        }
    )
    
    output$btn__download_example_mult <- downloadHandler(
        filename=function() {
            "Example_multiple_dose.txt"
        },
        content=function(file) {
            tmp <- fread(file="./examples/Example_multiple_dose.txt", sep="\t", stringsAsFactors=FALSE)
            fwrite(tmp, file, row.names=FALSE, sep="\t", quote=FALSE)
        }
    )
    
    output$btn__download_zip <- downloadHandler(
        filename=function() {
            paste0(dl_uuid$uuid, ".zip")
        },
        content=function(file) {
            # Zip files
            curr_wd <- paste0(getwd(), "/")
            files_to_zip <- paste0(curr_wd, "Simulated-Datasets/", dl_uuid$uuid, "/")
            zip_path <- unlist(lapply(list.files(paste0(curr_wd, "Simulated-Datasets/", dl_uuid$uuid, "/")), function(x) paste0(curr_wd, "Simulated-Datasets/", dl_uuid$uuid, "/", x))) 
            zipr(zipfile=paste0(curr_wd, "Simulated-Datasets/", dl_uuid$uuid, ".zip"), files=zip_path)
            file.copy(paste0(curr_wd, "Simulated-Datasets/", dl_uuid$uuid, ".zip"), file)
        },
        contentType = "application/zip"
    )
}
