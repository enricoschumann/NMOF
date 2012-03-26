## path <- "C:/packages/RForge/Test/NMOF/inst/unitTests"
if (require(RUnit, quietly = TRUE)) {
    testParallel <- TRUE
    path <- system.file(package = "NMOF", "unitTests")
    myTestSuite <- defineTestSuite("NMOF1",
                                   dirs = path,
                                   testFileRegexp = "unitTests.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
