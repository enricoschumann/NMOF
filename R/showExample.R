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

showChapterNames <- function(edition) {
    if (edition == 1)
        Chapters1
    else if (edition == 2)
        Chapter2
    else
        stop("unknown edition")
}

showExample <- function(file = "",
                        chapter = NULL,
                        showfile = TRUE,
                        includepaths= FALSE,
                        edition = 2,
                        search, ...) {

    if (edition == 1)
        Chapters <- Chapters1
    else if (edition == 2)
        Chapters <- Chapters2
    else
        stop("unknown edition")
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

    path <- system.file(package = "NMOF")
    fpaths <- list.files(paste(path, "/book", sep = ""),
                         recursive = TRUE, full.names = TRUE)

    ## create file names and chapternames
    fnames <- gsub(".*/R/", "", fpaths, ignore.case = TRUE)
    fnames <- gsub(".*ChangeLog.*", "ChangeLog", fnames,
                   ignore.case = TRUE)
    chnames <- gsub(".*/C-([a-zA-Z]+[^/])/.*", "\\1",
                    fpaths, ignore.case = TRUE)
    chnames <- gsub(".*ChangeLog.*", "none", chnames, ignore.case = TRUE)

    filematch <- grepl(file, fnames, ...)

    if (is.null(chapter))
        chapmatch <- rep.int(TRUE, length(fnames))
    else {
        if (is.numeric(chapter))
            tmp <- ChapterDirs[chapter[1]]
        else if (is.character(chapter)) {
            chapmatch <- grepl(chapter, Chapters, ...)
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
