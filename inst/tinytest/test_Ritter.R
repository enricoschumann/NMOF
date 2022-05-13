if (Sys.getenv("ES_PACKAGE_TESTING_73179826243954") == "true") {

    archive.dir <- "~/Downloads/Ritter"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)

    data <- Ritter(archive.dir)

}
