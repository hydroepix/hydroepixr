he_setup_iteration <- function() {

  if (verbose) cat("iteration",iteration,"\n")

  ## If seed is negative the seed is set at the beginning of each iteration:
  if (!is.null(seed)) {
    if(seed < 0) set.seed(iteration + abs(seed))
  }

  ## Determine the day the first detection will happen
  gDaysUntilBaseline <<- 0

  ## output variable to calculate the number of herds queuing for surveillance per day. does not affect anything (TH)
  TotNumQueue <<- 0

  ## Initialize waiting periods and transmission probabilities
  aHerd$taggedDur <<-rep(0, gMaxHerds)

  ## New additions TH ###

  aHerd$DetTaggedDep   <<- rep(FALSE, gMaxHerds)
  aHerd$timeVisited    <<- rep(0, gMaxHerds)
  aHerd$immuneTime     <<- rep(0, gMaxHerds)
  aHerd$timeCulled     <<- rep(0, gMaxHerds)
  aHerd$ToSurvDead     <<- rep(0, gMaxHerds)
  aHerd$DeadSampTest   <<- rep(0, gMaxHerds)
  aHerd$SampledDead    <<- rep(0, gMaxHerds)
  aHerd$DiagSurvDead   <<- rep(FALSE, gMaxHerds)
  aHerd$SubDeadSamp    <<- rep(0, gMaxHerds)
  aHerd$SusAgain       <<- rep(0, gMaxHerds)
  aHerd$Survived       <<- rep(0, gMaxHerds)
  aHerd$inSurZone      <<- rep(FALSE, gMaxHerds)

  ## New Variables TA and DP
  aHerd$Infectiousness <<- rep(0, gMaxHerds)
  aHerd$infMode        <<- rep(0, gMaxHerds)
  aHerd$CageSizeVar    <<- aHerd$CageSize
  aHerd$CageSizeCull   <<- aHerd$CageSize
  aHerd$DiagMode       <<- rep(0, gMaxHerds)
  aHerd$timeToFSV      <<- rep(0, gMaxHerds)
  aHerd$timeToSSV      <<- rep(0, gMaxHerds)
  aHerd$timeToASV      <<- rep(0, gMaxHerds)
  aHerd$FarmDiag       <<- rep(FALSE, gMaxHerds)
  # number of samples for surveillance- to be checked by DP
  aHerd$NumSamp        <<- round(rtriang(gMaxHerds, 5, 10, 20)) #rep(3, gMaxHerds)
  aHerd$NumSamSurv     <<- rep(0, gMaxHerds)
  aHerd$herdSizeSurv   <<- rep(0, gMaxHerds)
  aHerd$visitCount     <<- rep(0, gMaxHerds)

  FarmASVisit           <- round(runif(unique(aHerd$FarmID), 1, 90))
  aHerd$timeToASV      <<- FarmASVisit[aHerd$FarmID]

  ## This part works like list; it evaluates the text entries from the input
  ## table, whether those entries are numeric or R functions (like random
  ## variate selection), in order to set the waiting periods and transmission
  ## probabilities by herd type.
  for (i in herdtypes$herdTypeID) {
    herdIndex <- aHerd$herdType == i
    nType     <- sum(herdIndex)
    typeIndex <- herdtypes$herdTypeID == i
    ## Intra-herd interaction rate
    aHerd$K[herdIndex] <<- eval(herdtypes$K[typeIndex],list(n = nType))
    aHerd$RelSusceptibility[herdIndex] <<-
      eval(herdtypes$RelSusceptibility[typeIndex], list(n = nType))

  }

  ## Making sure that both 'Sus' and 'CageSize' are in 'aHerd'
  ##LEC: This should be changed so that only one name is used!
  if ("CageSize" %in% names(aHerd)) {
    aHerd$Sus <<- aHerd$CageSize
  } else {
    aHerd$CageSize <<- aHerd$Sus
  }


  ## Making sure that the above distributed numbers make sense in the model:
  aHerd$Sus[aHerd$Sus < 2]             <<- 2
  aHerd$K[aHerd$K < 0]                 <<- 0
  aHerd$taggedDur[aHerd$taggedDur < 0] <<- 0

  aHerd$p                              <<- aHerd$K # Reed-Frost probability
  aHerd$p[aHerd$p > 1]                 <<- 1

  ## Initializing infection methods
  for (i in 1:length(newInfMethods)) newInfMethods[[i]]$init()

  ## Initializing control measures
  # for (i in 1:length(controlMethods)) controlMethods[[i]]$init()

  #### Reset Values of other parameters
  gTime <<- 0

  aHerd$timeToTaggedForDepop <<- rep(Inf, gMaxHerds)
  aHerd$diagnosisTime        <<- rep(Inf, gMaxHerds)
  aHerd$Diagnosed            <<- rep(F, gMaxHerds)
  aHerd$DiagSurv             <<- rep(F, gMaxHerds)

  outbreakDetectedLast <<- outbreakDetected <<- FALSE

  depopQueue   <<- matrix(numeric(0), ncol = 2)
  beingDepoped <<- matrix(numeric(0), ncol = 2)
  zoneQueue    <<- matrix(numeric(0), ncol = 2)

  aHerd$timeToRemoveZone<<-rep(Inf,gMaxHerds)


  #### set initial values for index herd(s)

  ## SelectindexHerd(s) for next simulation
  aHerd$timeInfected <<- initTimeInfected
  aHerd$status       <<- initStatus


  if (ignoreStatus) {

    if(is.null(stepInFile)){

      indexHerd <<- indexHerdFunction(args = indexHerdSelect)
      aHerd$status[indexHerd]       <<- 2 + indexDirect
      aHerd$timeInfected[indexHerd] <<- 0

      if (indexDirect) {
        aInfHerd$addInf(indexHerd,
                        matrix(c(0,1,0),
                               byrow = TRUE,
                               ncol  = 3,
                               nrow  = length(indexHerd)),
                        1)
      } else {

        aInfHerd$addInf(indexHerd,
                        matrix(c(1,0,0), # matrix(c(round(rtriang(1, 1, 10, 100)),0,0),
                               byrow = TRUE,
                               ncol  = 3,
                               nrow  = length(indexHerd)),
                        0)
      }

    } else { ## Using stepInFile

      tmp <- read.table(stepInFile, sep = ",", header = TRUE)
      names(tmp)[1] <- "ID" #First column must be unique ID
      aHerdIndex    <- match(tmp$ID, aHerd$ID)

      assign("stepIn", cbind(aHerdIndex,tmp), envir=parent.env(environment()))

      if (any(is.na(aHerdIndex))) {
        stop("stepInFile: ",
             stepInFile,
             " contains IDs not in herdfile. \n Missing IDs are:",
             paste(stepIn$ID[is.na(aHerdIndex)],collapse=", "))
      }

      indexHerd <- aHerdIndex[stepIn$infectionDay == min(stepIn$infectionDay)]
      gDaysUntilBaseline <<- min(stepIn$diagnosedDay)

      if ("depopedDay" %in% names(stepIn)) {
        traced <- stepIn$diagnosedDay >= stepIn$depopedDay
        ## "diagnosing" the herds time to diagnosis before depop:
        stepIn$diagnosedDay <<- stepIn$depopedDay - aHerd$timeToDiagnosis[stepIn$aHerdIndex]
      }

      ##DK for (day in min(stepIn$infectionDay):(gDaysUntilBaseline-1)){
      for (day in min(stepIn$infectionDay):max(stepIn$infectionDay)) {
        gTime  <<- day
        newInf  <- which(stepIn$infectionDay == day)

        if(length(newInf) > 0){## Add these

          for (j in 1:length(newInf)){ ## One at a time

            if(stepIn$infType[newInf[j]] == 1){ ## Direct => subClin
              aInfHerd$addInf(aHerdIndex[newInf[j]], cbind(0,1,0), 1)
              aHerd$status[aHerdIndex[newInf[j]]] <<- 3      # subclin infection status
              ##DK aInfHerd$addInf(aHerdIndex[ newInf[j] ],cbind(1,0,0),1)
              ##DK Ups: the scenario dictates that the moved animal is latent

            } else {
              aInfHerd$addInf(aHerdIndex[ newInf[j] ],cbind(1,0,0),0)
              aHerd$status[aHerdIndex[newInf[j]]] <<- 2      # latent infection status
            }
          }
        }## Added newInfected herds

        aInfHerd$simDay()
      }

      aHerd$timeInfected[stepIn$aHerdIndex] <<- stepIn$infectionDay
      aHerd$infSource[stepIn$aHerdIndex]    <<- stepIn$infectionSource

      ### Time line!!!
      ### Set Time Diagnosed as to make sure depop happens as it should!!!
    }
  } else { ## What to do for infected herds given their time of diagnosis

    print("nothing was done given time of diagnosis")
  }

  vaccToday <<- 0
  Lock      <<- FALSE

  if(is.null(initHideMap))
    hideMap <<- TRUE

  BMAtmp <- round(runif(unique(aHerd$BMAID),120, 730))
  aHerd$BMATime       <<- rep(0, gMaxHerds)
  aHerd$FarmProdTime  <<- rep(0, gMaxHerds)
  aHerd$FarmActive    <<- rep(FALSE, gMaxHerds)
  aHerd$BMATime       <<- BMAtmp[aHerd$BMAID]
  aHerd$FarmProdTime  <<- aHerd$BMATime - round(runif(length(aHerd$BMATime),0, 120))
  ## force the farm of the index herd to be active
  IndexHerdFarmID <- aHerd$FarmID[indexHerd]
  aHerd$FarmProdTime[aHerd$FarmID%in%IndexHerdFarmID]  <<- round(runif(sum(aHerd$FarmID%in%IndexHerdFarmID),0, 120))
  aHerd$FarmProdTime[indexHerd] <<- 1

}
