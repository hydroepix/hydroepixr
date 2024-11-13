he_survey_herds <- function() {

  ### Herds that should be included in Queuing for surveillance
  setInQueue <- unique(aHerd$FarmID[(aHerd$timeToFSV == gTime |
                                       aHerd$timeToSSV == gTime |
                                       aHerd$timeToASV == gTime) &
                                      !(aHerd$Diagnosed) &
                                      !(aHerd$status %in% 5:6)])

  ## Arrange the selected farms for visiting
  setQueue <- matrix(numeric(0), ncol = 2)

  if (length(setInQueue) > 0) setQueue <- cbind(setInQueue,gTime)
  if (dim(setQueue)[1] > 0) zoneQueue <<- rbind(zoneQueue,setQueue)
  if (dim(zoneQueue)[1] > 0) {
    zoneQueue <<- zoneQueue[!duplicated(zoneQueue[,1]),, drop = FALSE]
  }

  # Move farms from queue to being Surveyed
  if (dim(zoneQueue)[1] > 0) {

    SurvMat <- zoneQueue
    survInd <- 1:dim(SurvMat)[1]

    if (length(survInd) <= (CapSurvay)) {
      toSurv <- survInd
    } else {
      toSurv <- survInd[1:sum(1:length(survInd) <= CapSurvay)]
    }

    SurvMat2 <- SurvMat[toSurv,, drop = FALSE]

    aHerd$Visited[aHerd$FarmID %in% SurvMat2[,1]] <<-
      aHerd$Visited[aHerd$FarmID %in% SurvMat2[,1]] + 1

    toBeCulled <- which((aHerd$FarmID %in% SurvMat2[,1]) &
                          !(aHerd$Diagnosed) &
                          (aHerd$status %in% 3:4))

    if (length(toBeCulled) > 0) {
      TMP1     <- aInfHerd$getInfected(toBeCulled)
      FarmIDs  <- sort(unique(aHerd$FarmID[toBeCulled]))

      NumbSamples <- cbind(FarmIDs,
                           tapply(aHerd$NumSamp[aHerd$FarmID %in% FarmIDs],
                                  aHerd$FarmID[aHerd$FarmID %in% FarmIDs],
                                  mean))

      herdSize    <- cbind(FarmIDs,
                           tapply(aHerd$CageSizeVar[aHerd$FarmID %in% FarmIDs],
                                  aHerd$FarmID[aHerd$FarmID %in% FarmIDs],
                                  sum))

      indexNS1 <- match(aHerd$FarmID, NumbSamples[,1])
      indexNS  <- ifelse(is.na(indexNS1), FALSE, TRUE)
      indexNS1 <- indexNS1[!is.na(indexNS1)]
      indexHS1 <- match(aHerd$FarmID, herdSize[,1])
      indexHS  <- ifelse(is.na(indexHS1), FALSE, TRUE)
      indexHS1 <- indexHS1[!is.na(indexHS1)]

      aHerd$NumSamSurv[indexNS]    <<- NumbSamples[indexNS1, 2]
      aHerd$herdSizeSurv[indexHS]  <<- herdSize[indexHS1, 2]

      TMP2           <-  1 - (1 - (aHerd$NumSamSurv[toBeCulled] /
                                     (aHerd$herdSizeSurv[toBeCulled] -
                                        ((TMP1 - 1) / 2)))) ^ TMP1

      TMP2[TMP2 > 1] <- 1
      toBeCulled     <- toBeCulled[runif(length(toBeCulled)) <= TMP2]

      if (length(toBeCulled) > 0) {
        depopQueue <<-rbind(depopQueue,cbind(toBeCulled,gTime))
        aHerd$Diagnosed[toBeCulled]     <<- TRUE
        aHerd$DiagMode[toBeCulled]      <<- 1
        aHerd$diagnosisTime[toBeCulled] <<- gTime
        aInfHerd$setDiagnosed(toBeCulled)
      }
    }

    if (Detailed) {

      if(length(SurvMat2[,1]) > 0) {
        SurvMatOut  <<- rbind(SurvMatOut,cbind(iteration,gTime,SurvMat2[,1]))
      }

      if(dim(SurvMatOut)[1]>= DumpData){

        # NAME here will be exactly the same as that in the initialization file,
        # so no worries; no overwriting will happen ;-) (TH)
        NAMESH <- paste(runID,"SurvayedHerds.txt",sep="-")
        write.table(SurvMatOut,NAMESH,append=TRUE,col.names = F,row.names = F)
        SurvMatOut <<- matrix(numeric(0),ncol=3)
      }
    }

    aHerd$visitCount[SurvMat2[,1]]  <<- aHerd$visitCount[SurvMat2[,1]] + 1
    zoneQueue                       <<- zoneQueue[-toSurv,, drop = FALSE]

  }
}
