Chapters1 <- c("Introduction",
               "Numerical analysis in a nutshell",
               "Linear equations and Least Squares problems",
               "Finite difference methods",
               "Binomial trees",
               "Generating random numbers",
               "Modeling dependencies",
               "A gentle introduction to financial simulation",
               "Financial simulation at work: some case studies",
               "Optimization problems in finance",
               "Basic methods",
               "Heuristic methods in a nutshell",
               "Portfolio optimization",
               "Econometric models",
               "Calibrating option pricing models")

Chapters2 <- c("Introduction",
               "Numerical analysis in a nutshell",
               "Linear equations and Least Squares problems",
               "Finite difference methods",
               "Binomial trees",
               "Generating random numbers",
               "Modeling dependencies",
               "A gentle introduction to financial simulation",
               "Financial simulation at work: some case studies",
               "Optimization problems in finance",
               "Basic methods",
               "Heuristic methods in a nutshell",
               "Heuristics: a tutorial",
               "Portfolio optimization",
               "Backtesting",
               "Econometric models",
               "Calibrating option pricing models")

showChapterNames <- function(edition = 2) {
    if (edition == 1)
        Chapters1
    else if (edition == 2)
        Chapters2
    else
        stop("unknown edition")
}

showExample <- function(file = "",
                        chapter = NULL,
                        showfile = TRUE,
                        includepaths= FALSE,
                        edition = 2,
                        search, ...,
                        ignore.case = TRUE) {

    if (edition == 1)
        Chapters <- Chapters1
    else if (edition == 2)
        Chapters <- Chapters2
    else
        stop("unknown edition")

    path <- system.file(package = "NMOF")
    if (edition == 1) {
        
        ChapterDirs <- c("Introduction",
                         "NumAnNutshell",
                         "LinEqsLSP",
                         "FiniteDifferences",
                         "BinomialTrees",
                         "RandomNumberGeneration",
                         "ModelingDependencies",
                         "FinancialSimulations",
                         "CaseStudies",
                         "OptProbFinance",
                         "BasicMethods",
                         "HeuristicsNutshell",
                         "PortfolioOptimization",
                         "EconometricModels",
                         "OptionCalibration")

        fpaths <- list.files(paste(path, "/book/1ed", sep = ""),
                             recursive = TRUE, full.names = TRUE)
        chnames <- gsub(".*/C-([a-zA-Z]+[^/])/.*", "\\1",
                        fpaths, ignore.case = TRUE)

    } else if (edition == 2) {

        ChapterDirs <- c("01_Introduction",
                         "02_Numerical_analysis_in_a_nutshell",
                         "03_Linear_equations_and_Least_Squares_problems",
                         "04_Finite_difference_methods",
                         "05_Binomial_trees",
                         "06_Generating_random_numbers",
                         "07_Modeling_dependencies",
                         "08_A_gentle_introduction_to_financial_simulation",
                         "09_Financial_simulation_at_work_-_some_case_studies",
                         "10_Optimization_problems_in_finance",
                         "11_Basic_methods",
                         "12_Heuristic_methods_in_a_nutshell",
                         "13_Heuristics_-_a_tutorial",
                         "14_Portfolio_optimization",
                         "15_Backtesting",
                         "16_Econometric_models",
                         "17_Calibrating_option_pricing_models")
        
        fpaths <- list.files(paste(path, "/book/2ed", sep = ""),
                             recursive = TRUE, full.names = TRUE)
        chnames <- gsub(".*/(.*)/R/[^/]+", "\\1",
                        fpaths, ignore.case = TRUE)               
    } 
    
    fnames <- basename(fpaths)    
    filematch <- grepl(file, fnames, ...,
                       ignore.case = ignore.case)
    
    if (is.null(chapter))
        chapmatch <- rep.int(TRUE, length(fnames))
    else {
        if (is.numeric(chapter))
            tmp <- ChapterDirs[chapter[1]]
        else if (is.character(chapter)) {
            chapmatch <- grepl(chapter, Chapters, ...,
                               ignore.case = ignore.case)
            tmp <- ChapterDirs[chapmatch]
        }
        chapmatch <- logical(length(fnames))
        for (i in seq_along(tmp))
            chapmatch <- (chapmatch | tmp[i] == chnames)
        
    }
    results <- chapmatch & filematch
    if (!length(which(results))) {
        message("no matches")
        flist <- data.frame(Chapter = character(0L),
                            File = character(0L))
    } else {
        flist <- data.frame(Chapter = chnames[results],
                            File = fnames[results])
        if (length(which(results)) == 1L)
            file.show(fpaths[results], title = "NMOF",
                      header = fnames[results])
        else
            message("found several files...")
    }
    if (includepaths)
        flist <- cbind(flist, Paths = fpaths[results])

    flist
}
