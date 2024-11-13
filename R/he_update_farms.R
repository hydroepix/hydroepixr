he_update_farms <- function() {
  ## Depopulating diagnosed and tagged herds

  if (dim(depopQueue)[1] > 0) {
    ## Remove duplets and those already being depopulated
    tmpIndex    <- !duplicated(depopQueue[,1])
    depopQueue <<- depopQueue[tmpIndex,, drop = FALSE]

    if (CULLALL) {
      getCHR    <- unique(aHerd$FarmID[depopQueue[1]])
      extraCull <- aHerd$ID[aHerd$FarmID %in% getCHR]
      extraCull <- extraCull[!extraCull %in% depopQueue[1]]

      if (length(extraCull) > 0) {
        depopQueue  <<- rbind(depopQueue, cbind(extraCull, gTime))
        InfEC        <- extraCull[aHerd$status[extraCull] %in% c(2,3,4,7)]
        if (length(InfEC) > 0) {
          aHerd$Diagnosed[InfEC]     <<- TRUE
          aHerd$diagnosisTime[InfEC] <<- gTime
          aInfHerd$setDiagnosed(InfEC)
        }
      }
    }

    ## Move herds from queue to being depoped
    cullMat <- matrix(numeric(0), ncol = 3)
    cullMat <- cbind(depopQueue, aHerd$CageSizeCull[depopQueue[,1]])

    if (sum(cullMat[,3]) <= Capacity) {
      beingDepoped <<- rbind(beingDepoped, cbind(cullMat[,1], 1))
      toCull        <- 1:dim(cullMat)[2]

    } else {

      cumNumAnimals <- cumsum(cullMat[,3])
      cullCat       <- which(cumNumAnimals <= Capacity)
      partial       <- which(!(cumNumAnimals <= Capacity))[1]

      if (!is.null(partial)) {
        cullMat[partial, 3] <- (cumNumAnimals[partial] - Capacity)
        aHerd$CageSizeCull[cullMat[partial, 1]] <<- cullMat[partial, 3]
      }

      if (length(cullCat) > 0){
        beingDepoped <<- rbind(beingDepoped, cbind(cullMat[cullCat,1], 1))
      }

      toCull <- cullCat

    }

    if (length(toCull) > 0) depopQueue <<- depopQueue[-toCull,, drop = FALSE]

    if (dim(beingDepoped)[1] > 0) {

      tmpID <- beingDepoped[,1]
      aHerd$timeToRemoveZone[tmpID] <<- gTime
      aHerd$timeCulled[tmpID]       <<- gTime
      aHerd$status[tmpID]           <<- 5
      aInfHerd$delInf(tmpID, ignoreNotInfected = TRUE)

      gDepopulationCount[iteration] <<-
        gDepopulationCount[iteration] + length(tmpID)

      beingDepoped <<- matrix(numeric(0), ncol = 2)

      if (Detailed) {

        DepopMatOut <<- rbind(DepopMatOut, cbind(iteration, gTime, tmpID))

        if (dim(DepopMatOut)[1] >= DumpData) {

          ### NAME here will be exactly the same as that in the initialization file,
          ### so no worries; no overwriting will happen ;-) (TH)
          NAMED <- paste(runID, "DepopHerds.txt", sep = "-")

          write.table(DepopMatOut,
                      NAMED,
                      append = TRUE,
                      col.names = FALSE,
                      row.names = FALSE)

          DepopMatOut<<- matrix(numeric(0),ncol=3)
        }
      }
    }
  }

  ## Simulate one day of intra herd dynamics:
  aInfHerd$simDay()
}
