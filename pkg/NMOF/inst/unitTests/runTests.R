localTesting <- TRUE
require("NMOF")
if (require("RUnit", quietly = TRUE)) {
    if (localTesting)
        path <- "~/Packages/NMOF/NMOF/pkg/NMOF/inst/unitTests" else
    path <- system.file("unitTests", package = "NMOF")

    myTestSuite <- defineTestSuite("NMOF",
                                   dirs = path,
                                   testFileRegexp = "unitTests.+")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
