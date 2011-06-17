# path <- "C:/packages/NMOF/inst/unitTests"
if (require(RUnit, quietly = TRUE)) {
    path <- system.file(package = "NMOF", "unitTests")
    myTestSuite <- defineTestSuite("NMOF1", 
        dirs = path, testFileRegexp = "unitTests.R")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
        fileName = paste(file.path(path, "report"), ".txt", sep = ""))
}
