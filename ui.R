ui <- fluidPage(
    
    useShinyjs(),
    
    # Tooltips
    bsTooltip(id=paste0("btn__download_example"), title=paste0("Download an example input file with only samples of a dose level of 0."), placement="bottom"),
    bsTooltip(id=paste0("btn__download_example_mult"), title=paste0("Download an example input file with samples of multiple dose levels."), placement="bottom"),
    bsTooltip(id=paste0("input__replacement"), title=paste0("Enable/disable sampling with replacement. If enabled, samples will not be removed from the population after being pulled out for the result file. Note that enabling this for small populations may cause samples to be duplicated in the result file!"), placement="bottom"),
    bsTooltip(id=paste0("input__num_synthetic_samples"), title=paste0("The total number of samples to include in the result file (# of synthetic samples + # of original samples)."), placement="bottom"),
    bsTooltip(id=paste0("input__dose_input_type"), title=paste0("Either allow the application to specify preset dose levels given a spacing and top dose or manually specify each level."), placement="bottom"),
    bsTooltip(id=paste0("input__num_synthetic_samples_pool"), title=paste0("The total population for EACH dose level to sample from (# of synthetic samples + # of original samples). Must be at least the size of the total number of replicates if sampling without replacement."), placement="bottom"),
    bsTooltip(id=paste0("input__num_edf"), title=paste0("Specify the number of expression data files to generate. Each file is sampled differently; however, if the population size is small, the same samples are more likely to be seen across files."), placement="bottom"),
    bsTooltip(id=paste0("input__num_treatment_groups"), title=paste0("Specify the number of treatment groups (dose levels) to be generated. Each dose level is separated by the factor specified in the dose spacing option."), placement="bottom"),
    bsTooltip(id=paste0("input__num_rep_d0"), title=paste0("Specify the total number of samples for the control dose level to be sampled from the population and included in each result file. These do not necessarily include the original input data samples. "), placement="bottom"),
    bsTooltip(id=paste0("input__num_rep_nod0"), title=paste0("Specify the total number of samples for all non-control dose level to be sampled from the population and included in each result file."), placement="bottom"),
    bsTooltip(id=paste0("input__num_top_dose"), title=paste0("Specify the maximum dose level to include in each result expression data file. The other (non-control) dose levels are determined with respect to this value using the spacing specified in the dose spacing options."), placement="bottom"),
    bsTooltip(id=paste0("input__dose_spacing"), title=paste0("Specify the spacing between dose levels. 1/10: ..., 0.01, 0.1, 1, 10, 100, ...; 1/3: 1, 3.3, 10, 33, ...; 1/2: 2, 4, 8, 16, 32, ..."), placement="bottom"),
    bsTooltip(id=paste0("input__num_rep_d0_supp"), title=paste0("Specify the total number of samples for the control dose level to include in each result file. This number is the number of original control samples + the number of samples to synthesize. This always contains the original control samples."), placement="bottom"),
    bsTooltip(id=paste0("input__num_rep_nod0_supp"), title=paste0("Specify the total number of samples for each non-control dose level to include in each result file. This number is the number of original non-control samples for each level + the number of samples to synthesize. This always contains the original non-control samples."), placement="bottom"),
    bsTooltip(id=paste0("input__noneg"), title=paste0("If checked, if the generation algorithm creates samples with negative column values, they will instead be rounded up to 0."), placement="bottom"),
    bsTooltip(id=paste0("btn__add_dose"), title=paste0("Specify a new dosage level with the corresponding number of samples to sample from the population."), placement="bottom"),
    
    titlePanel("Synthetic and Null Data Generator (SaNDGen)"),
    
    sidebarLayout(
        sidebarPanel(
            downloadButton(outputId="btn__download_example", label="Download example input file (control)"),
            downloadButton(outputId="btn__download_example_mult", label="Download example input file (multiple dose levels)"),
            fileInput(inputId="input__sample_file", label="Upload a control sample file", multiple=TRUE, accept=c(".csv", ".tsv", ".txt")),
            
            # uploaded files display
            hidden(
                fluidRow(id="file_display",
                         column(12,
                                DT::dataTableOutput(outputId="output__uploaded_files")
                         )
                )
            ),
            radioButtons(inputId="input__replacement", label="Sample with replacement?", choiceNames=c("No", "Yes"), choiceValues=c(FALSE, TRUE)),
            
            HTML(paste0("
                <p>
                Select a mode from the options below:
                <ul>
                <li><b>Generate synthetic null control samples</b>: Generate some number of synthetic samples using the input file. Only one input file is accepted. Input file must contain only control samples: other dose levels are ignored.</li>
                <li><b>Generate synthetic null control samples & create expression data files</b>: Generate some number of synthetic samples using the input file, take samples and output to expression data files, and arbitrarily assign dose levels. Only one input file is accepted. Input file must contain only control samples: other dose levels are ignored.</li>
                <li><b>Supplement existing data</b>: Expand the samples in each dose level given one or more input files and randomly sample synthetic data and output to expression data files. Separate pools of samples are generated for each dose level. Can accept multiple input files as a batch upload. Each result file will always contain the original samples in each dose level alongside the generated samples.</li>
                </ul>
                </p>
            ")),
            
            radioButtons(inputId="input__output_mode", label="Output mode", choiceNames=c("Generate synthetic null control samples", "Generate synthetic null control samples & create expression data files", "Supplement existing data"), choiceValues=c("noedf", "edf", "supp")),
            # Options for "generate synthetic samples and download"
            hidden(
                fluidRow(id="options__noedf",
                         column(12, style="background-color: #ccc;",
                                h4("Synthetic sample generation options"),
                                numericInput(inputId="input__num_synthetic_samples", label="Number of synthetic samples", min=1, max=1000000, step=1, value=1000),
                         )
                )
            ),
            # Options for "generate synthetic samples, create expression data files, and download"
            hidden(
                fluidRow(id="options__edf",
                         column(12, style="background-color: #ccc;",
                                h4("Expression data file generation options"),
                                
                                radioButtons(inputId="input__dose_input_type", label="Dose input mode", choiceNames=c("Automatic generation", "Manual input"), choiceValues=c("auto", "manual")),
                                
                                numericInput(inputId="input__num_synthetic_samples_pool", label="Number of synthetic samples per dose level", min=1, max=1000000, step=1, value=1000),
                                numericInput(inputId="input__num_edf", label="Number of expression data files", min=1, max=100, step=1, value=20),
                                hidden(
                                    div(id="input__auto_edf",
                                        numericInput(inputId="input__num_treatment_groups", label="Number of treatment groups (including D0)", min=1, max=1000000, step=1, value=10),
                                        numericInput(inputId="input__num_rep_d0", label="Number of replicates = D0", min=1, max=1000000, step=1, value=10),
                                        numericInput(inputId="input__num_rep_nod0", label="Number of replicates ≠ D0", min=1, max=1000000, step=1, value=10),
                                        numericInput(inputId="input__num_top_dose", label="Top dose", min=1, max=1000000, step=1, value=1000),
                                        radioButtons(inputId="input__dose_spacing", label="Dose spacing", choiceNames=c("1/10", "1/3", "1/2"), choiceValues=c("tenth", "third", "half"))
                                    )
                                ), 
                                
                                hidden(
                                    div(id="input__auto_supp",
                                        numericInput(inputId="input__num_rep_d0_supp", label="Number of replicates = D0", min=1, max=1000000, step=1, value=10),
                                        numericInput(inputId="input__num_rep_nod0_supp", label="Number of replicates ≠ D0", min=1, max=1000000, step=1, value=10)
                                    )
                                ),
                                
                                hidden(
                                    div(id="input__manual",
                                        actionButton(inputId="btn__add_dose", label="Add dose level", icon=icon("plus"), lib="glyphicons"),
                                        uiOutput(outputId="container__manual_input")
                                    )
                                )
                         )
                )
            ),
            
            br(),
            fluidRow(
                column(6,
                       actionButton(inputId="btn__go", label="GO!", icon=icon("chevron-right"), lib="glyphicons", style="background-color: #25f15d; border-color: #22c74f;")
                ),
                column(6,
                       hidden(
                           downloadButton(outputId="btn__download_zip", label="Download result file(s)")
                       )
                )
            ),
            HTML(paste0("<p style='color: #555;'>Icons from ", a("Glyphicons", href="https://glyphicons.com/"), "</p>"))
        ),
        
        # Show preview for generated files
        mainPanel(
            h1("Generated Samples Preview"),
            uiOutput(outputId="output__file_viz") %>% withSpinner()
        )
    )
)