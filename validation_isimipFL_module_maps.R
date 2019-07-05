my_mapCountryData <- 
function (mapToPlot = "", nameColumnToPlot = "", numCats = 7, 
          xlim = NA, ylim = NA, mapRegion = "world", catMethod = "quantiles", 
          colourPalette = "heat", addLegend = TRUE, borderCol = "grey", 
          mapTitle = "columnName", oceanCol = NA, aspect = 1, missingCountryCol = NA, 
          add = FALSE, nameColumnToHatch = "", lwd = 0.5, ...) 
{
    functionName <- as.character(sys.call()[[1]])
    new <- TRUE
    if (new) {
        mapToPlot <- rwmCheckAndLoadInput(mapToPlot, inputNeeded = "sPDF", 
                                          callingFunction = functionName)
    }
    else {
        if (class(mapToPlot) == "SpatialPolygonsDataFrame") {
            if (length(mapToPlot@data[, 1]) < 1) {
                stop("seems to be no data in your chosen file or dataframe in ", 
                     functionName)
                return(FALSE)
            }
        }
        else if (mapToPlot == "") {
            message(paste("using example data because no file specified in", 
                          functionName))
            mapToPlot <- getMap(resolution = "coarse")
            mapToPlot <- mapToPlot[-which(mapToPlot$ADMIN=="Antarctica"),]
            if (nameColumnToPlot == "") 
                nameColumnToPlot <- "POP_EST"
        }
        else {
            stop(functionName, " requires a SpatialPolygonsDataFrame object created by the joinCountryData2Map() function \n")
            return(FALSE)
        }
    }
    if (nameColumnToPlot == "") 
        nameColumnToPlot <- "POP_EST"
    if (is.na(match(nameColumnToPlot, names(mapToPlot@data)))) {
        stop("your chosen nameColumnToPlot :'", nameColumnToPlot, 
             "' seems not to exist in your data, columns = ", 
             paste(names(mapToPlot@data), ""))
        return(FALSE)
    }
    dataCategorised <- mapToPlot@data[[nameColumnToPlot]]
    if (!is.numeric(dataCategorised) && catMethod != "categorical") {
        catMethod = "categorical"
        message(paste("using catMethod='categorical' for non numeric data in", 
                      functionName))
    }
    if (length(catMethod) == 1 && catMethod == "categorical") {
        dataCategorised <- as.factor(dataCategorised)
        cutVector <- levels(dataCategorised)
        numColours <- length(levels(dataCategorised))
    }
    else if (is.numeric(catMethod)) {
        cutVector <- catMethod
        numColours <- -1 + length(catMethod)
        dataCategorised <- cut(dataCategorised, cutVector, include.lowest = TRUE)
    }
    else if (is.character(catMethod)) {
        cutVector <- rwmGetClassBreaks(dataCategorised, catMethod = catMethod, 
                                       numCats = numCats, verbose = TRUE)
        dataCategorised <- cut(dataCategorised, cutVector, include.lowest = TRUE)
        numColours <- length(levels(dataCategorised))
    }
    colNameRaw <- nameColumnToPlot
    colNameCat <- paste(colNameRaw, "categorised", sep = "")
    mapToPlot@data[[colNameCat]] <- dataCategorised
    colourVector <- rwmGetColours(colourPalette, numColours)
    dataCatNums <- as.numeric(dataCategorised)
    if (!is.na(missingCountryCol)) {
        colourVector <- c(colourVector, missingCountryCol)
        dataCatNums[is.na(dataCatNums)] <- length(colourVector)
    }
    hatchVar = NULL
    if (nameColumnToHatch == "") {
        if (!add) 
            rwmNewMapPlot(mapToPlot, mapRegion = mapRegion, xlim = xlim, 
                          ylim = ylim, oceanCol = oceanCol, aspect = aspect)
        plot(mapToPlot, col = colourVector[dataCatNums], border = borderCol, 
             add = TRUE, usePolypath = FALSE, lwd = lwd, ...)
    }
    else {
        hatchVar = mapToPlot@data[[nameColumnToHatch]]
        # hatchVar = (hatchVar - min(hatchVar, na.rm = TRUE))/(max(hatchVar, 
        #                                                         na.rm = TRUE)-min(hatchVar, na.rm=T))
        # hatchVar = 1 - hatchVar
        # hatchVar = (hatchVar * 20) + 80
        # hatchVar[hatchVar > 99] = NA
        hatchVar <- ifelse(hatchVar==0,-1,50)
        if (!add) 
            rwmNewMapPlot(mapToPlot, mapRegion = mapRegion, xlim = xlim, 
                          ylim = ylim, oceanCol = oceanCol, aspect = aspect)
        # plot(mapToPlot, col = colourVector[dataCatNums], border = borderCol, 
        #      add = TRUE, usePolypath = FALSE, lwd = lwd, ...)
        # plot(mapToPlot, col = NA, border = NA, 
        #      density = hatchVar, angle = 135, lty = 1, add = TRUE, 
        #      usePolypath = FALSE, lwd = 0.5, ...)
        plot(mapToPlot, col = colourVector[dataCatNums], border = NA,
             density = hatchVar, angle = 45, lty = 1, add = TRUE,
             usePolypath = FALSE, lwd = 0.7, ...)
        plot(mapToPlot, col = colourVector[dataCatNums], border = NA,
             density = hatchVar, angle = 135, lty = 1, add = TRUE,
             usePolypath = FALSE, lwd = 0.7, ...)
        plot(mapToPlot, col = NA, border = borderCol,
             add = TRUE, usePolypath = FALSE, lwd = lwd, ...)
    }
    if (addLegend) {
        if ((length(catMethod) == 1 && catMethod == "categorical")) {
            addMapLegendBoxes(colourVector = colourVector, cutVector = cutVector, 
                              catMethod = catMethod)
        }
        else {
            addMapLegend(cutVector = cutVector, colourVector = colourVector)
        }
    }
    if (mapTitle == "columnName") {
        title(nameColumnToPlot)
    }
    else {
        title(mapTitle)
    }
    invisible(list(colourVector = colourVector, cutVector = cutVector, 
                   plottedData = mapToPlot[[nameColumnToPlot]], catMethod = catMethod, 
                   colourPalette = colourPalette))
}

my_joinCountryData2Map <- 
function (dF, joinCode = "ISO3", nameJoinColumn = "ISO3V10", 
          nameCountryColumn = "Country", suggestForFailedCodes = FALSE, 
          mapResolution = "coarse", projection = NA, verbose = FALSE) 
{
    mapWithData <- getMap(resolution = mapResolution)
    mapWithData <- mapWithData[-which(mapWithData$ADMIN=="Antarctica"),]
    
    if (!is.na(projection)) 
        warning("the projection argument has been deprecated, returning Lat Lon, use spTransform from package rgdal as shown in help details or the FAQ")
    listJoinCodesNew <- c("ISO_A2", "ISO_A3", "FIPS_10_", "ADMIN", 
                          "ISO_N3")
    listJoinCodesOld <- c("ISO2", "ISO3", "FIPS", "NAME", "UN")
    listJoinCodes <- c(listJoinCodesOld, listJoinCodesNew)
    if (joinCode %in% listJoinCodes == FALSE) {
        stop("your joinCode (", joinCode, ") in joinCountryData2Map() is not one of those supported. Options are :", 
             paste(listJoinCodes, ""), "\n")
        return(FALSE)
    }
    joinCodeOld <- joinCode
    if (joinCode %in% listJoinCodesOld) {
        joinCode <- listJoinCodesNew[match(joinCode, listJoinCodesOld)]
    }
    if (is.na(match(nameJoinColumn, names(dF)))) {
        stop("your chosen nameJoinColumn :'", nameJoinColumn, 
             "' seems not to exist in your data, columns = ", 
             paste(names(dF), ""))
        return(FALSE)
    }
    dF[[joinCode]] <- as.character(dF[[nameJoinColumn]])
    dF[[joinCode]] <- gsub("[[:space:]]*$", "", dF[[joinCode]])
    if (joinCode == "ADMIN") {
        dF$ISO3 <- NA
        for (i in 1:nrow(dF)) dF$ISO3[i] = rwmGetISO3(dF[[joinCode]][i])
        joinCode = "ISO3"
        nameCountryColumn = nameJoinColumn
    }
    matchPosnsInLookup <- match(as.character(dF[[joinCode]]), 
                                as.character(mapWithData@data[[joinCode]]))
    failedCodes <- dF[[joinCode]][is.na(matchPosnsInLookup)]
    numFailedCodes <- length(failedCodes)
    numMatchedCountries <- nrow(dF) - numFailedCodes
    cat(numMatchedCountries, "codes from your data successfully matched countries in the map\n")
    failedCountries <- dF[[nameCountryColumn]][is.na(matchPosnsInLookup)]
    failedCountries <- cbind(failedCodes, failedCountries = as.character(failedCountries))
    cat(numFailedCodes, "codes from your data failed to match with a country code in the map\n")
    if (verbose) 
        print(failedCountries)
    matchPosnsInUserData <- match(as.character(mapWithData@data[[joinCode]]), 
                                  as.character(dF[[joinCode]]))
    codesMissingFromUserData <- as.character(mapWithData@data[[joinCode]][is.na(matchPosnsInUserData)])
    countriesMissingFromUserData <- as.character(mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)])
    numMissingCodes <- length(codesMissingFromUserData)
    cat(numMissingCodes, "codes from the map weren't represented in your data\n")
    mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData, 
                                                   ])
    invisible(mapWithData)
}
