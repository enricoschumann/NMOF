if (Sys.getenv("ES_PACKAGE_TESTING_73179826243954") == "true") {

    archive.dir <- "~/Downloads/French"
    if (!dir.exists(archive.dir))
        dir.create(archive.dir)
    
    dataset <- "Portfolios_Formed_on_ME_Daily_CSV.zip"
    data <- French(archive.dir, dataset, price.series = TRUE)


    ## 
    for (d in French())
        data <- French(archive.dir, d)

}
