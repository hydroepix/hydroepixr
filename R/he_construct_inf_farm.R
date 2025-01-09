# Replacing constructAInfHerd
# called once to create an infected herd object?
he_construct_infected_farm <- function() {
  ## Internal variables:
  ## VarDef herds    Matrix with info on infected farms
  ## VarDes Column names are in herdsCol. A matrix without names is faster
  nColHerds  <- 19
  herds      <- matrix(numeric(0), ncol = nColHerds)
  DeadMat    <- matrix(numeric(0), ncol = DaysDead)
  DeadMatSur <- matrix(numeric(0), ncol = DaysSurDead)
  MortMat    <- matrix(numeric(0), ncol = 5)

  ##Keeping deleted herds
  delHerds <- matrix(numeric(0), ncol = nColHerds)

  herdsCol <- c("Sus1",
                "latent2",
                "SubC3",
                "Clinic4",
                "Immune5",
                "Total6",
                "status7",
                "ID8",
                "p9",
                "NA.latDur10",
                "NA.SubDur11",
                "TClic12",
                "TDiag13",
                "Diagnosed14",
                "InfByDC15",
                "TInfected16",
                "herdType17",
                "Vaccinated18",
                "TLastAnCli19")

  dimnames(herds)[[2]] <- herdsCol

  MortMatCol <- c("iteration", "infFarmID", "infPenID", "mort", "day")

  #dimnames(MortMat)[[2]] <- MortMatCol

  ## VarDef latent,subclinical  Used to model transitions between stages
  ## VarDes Matrix with a row for each farm and columns for
  ## VarDes   each day latent or subclinical or clinical
  latent      <- matrix(numeric(0), ncol = ncol(herdtypes$latDurFreq))
  subclinical <- matrix(numeric(0), ncol = ncol(herdtypes$SubDurFreq))
  clinical    <- matrix(numeric(0), ncol = ncol(herdtypes$CliDurFreq))

  "rpoly2" <- function(np) {
    tabulate(sample(1:(length(np) - 1),
                    size    = np[1],
                    replace = TRUE,
                    prob    = np[-1]),
             nbins = (length(np) - 1))

  }

  list(
    # List holding all information to be accessed from outside
    #################################################################
    getIDs          = function() {return(herds[,8])},
    getstatus       = function() {return(herds[,7])},
    getTDiag        = function() {return(herds[,13])},
    getDelHerds     = function() {return(delHerds)},
    getHerds        = function() {return(herds)},
    setHerds        = function(newHerds) {herds <<- newHerds},
    getDiagnosed    = function() {return(herds[,14])},
    getDiagnosedIDs = function() {return(herds[herds[,14] == 1,8])},

    ### Time the herd showed clinical signs (TH)
    getTClic = function(IDs) {
      Herds <- herds
      index <- match(IDs, Herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <- Herds[index, 12]
      return(tmp)

    },

    setDiagnosed = function(IDs) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        # warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        # this warning is stopped, because some farmss will be culled and hence
        # treated as diagnosed if they have the same chr as a detected herd.
        index <- index[!is.na(index)]
      }

      herds[index, 14] <<- 1
    },

    getDelIDs = function() {return(delHerds[,8])},
    getInfness = function(IDs = herds[,8]) {
      index <- match(IDs,herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <- (herds[index,3] + herds[index,4]) /
        (herds[index,6] - (herds[index,5] * PerDeadAnim))
      return(tmp)
    },

    anyInf = function() return(nrow(herds) > 0),

    ##numInfAnimals: three column matrix for latent, subC, and Clin
    addInf = function(IDs, numInfAnimals, DC) {
      ##Only those that were not already infected
      tmpID <- (IDs %in% herds[,8])

      if (any(tmpID)) {
        IDs <- IDs[!tmpID]
        numInfAnimals <- numInfAnimals[!tmpID,, drop = FALSE]
        DC <- DC[!tmpID]
      }

      if (length(IDs) > 0) {
        tmp <- cbind(aHerd$Sus[IDs], 0, 0, 0, 0, aHerd$Sus[IDs], 1,
                     IDs, aHerd$p[IDs], 0, 0, Inf, Inf, 0, DC, gTime,
                     aHerd$herdType[IDs], 0, 0)

        tmp[,1:4] <- tmp[,1:4] + cbind(-rowSums(numInfAnimals), numInfAnimals)
        tmp[,7]   <- apply(tmp[, 1:4, drop = FALSE], 1, function(x) max((1:4)[x >= 1]))

        ## Updating diagnosis if initial status is 4 (Clinical
        if (is.na(tmp[1,1])) browser()

        if (any(tmpIndex <- (tmp[,7] == 4))) {
          ## Time herd showed clinical signs
          tmp[tmpIndex,12] <- gTime
        }

        herds <<- rbind(herds, tmp)

        ## Distributing the animals on No. days being latent or subclincal or clinical
        latent <<- rbind(latent,
                         t(apply(cbind(tmp[,2], herdtypes$latDurFreq[tmp[,17],, drop = FALSE]),
                                 1, rpoly2)))

        subclinical <<- rbind(subclinical,
                              t(apply(cbind(tmp[,3],herdtypes$SubDurFreq[tmp[,17],, drop = FALSE]),
                                      1, rpoly2)))

        clinical <<- rbind(clinical,
                           t(apply(cbind(tmp[,4],herdtypes$CliDurFreq[tmp[,17],, drop = FALSE]),
                                   1, rpoly2)))

        DeadMat <<- rbind(DeadMat,
                          t(sapply(IDs, function(x) rep(0, (DaysDead)))))

        DeadMatSur <<- rbind(DeadMatSur,
                             t(sapply(IDs, function(x) rep(0, (DaysSurDead)))))


      }

    },

    delInf = function(IDs, ignoreNotInfected = FALSE) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        if (!ignoreNotInfected)
          warning("ConstructAInfHerd: Cannot remove IDs not in aInfHerd!",
                  IDs,index)
        index <- index[!is.na(index)]
      }

      if(length(index)>0){
        delHerds    <<- rbind(delHerds,herds[index,])
        herds       <<- herds[-index,,drop=FALSE]
        latent      <<- latent[-index,,drop=FALSE]
        subclinical <<- subclinical[-index,,drop=FALSE]
        clinical    <<- clinical[-index,,drop=FALSE]
        DeadMat     <<- DeadMat[-index,,drop=FALSE]
        DeadMatSur  <<- DeadMatSur[-index,,drop=FALSE]
      }
    },

    simDay = function() {
      if (nrow(herds) > 0) {
        # Supressing warning: NAs generated due to complete cage mortality (fixed on line 553)
        rS2L <- suppressWarnings(
          rbinom(nrow(herds), herds[,1],
                 1 - exp(-herds[,9] * (1 - VaccEffPop) * ((herds[,3]) + (herds[,4])) /
                           (herds[,6] - herds[,5]))))

        ### Prevent model crash because of NAs due to complete cage mortality
        rS2L[is.na(rS2L)] <- 0

        ## Updating totals
        herds[,1:5] <<- herds[,1:5] + cbind(-rS2L,
                                            rS2L-latent[,1],
                                            latent[,1] - subclinical[,1],
                                            subclinical[,1] - clinical[,1],
                                            clinical[,1])

        ## Distributing new latent infected
        newLat <- apply(cbind(rS2L,
                              herdtypes$latDurFreq[herds[,17],, drop = FALSE]),
                        1, rpoly2)

        ## Distributing new subclinical infected
        newSubC <- apply(cbind(latent[,1],
                               herdtypes$SubDurFreq[herds[,17],, drop = FALSE]),
                         1, rpoly2)

        ## Distributing new clinical infected
        newCli <- apply(cbind(subclinical[,1],
                              herdtypes$CliDurFreq[herds[,17],, drop = FALSE]),
                        1, rpoly2)

        ## Update intra latent and subclinical and clinical waiting states
        latent      <<- cbind(latent[,-1, drop = FALSE], 0) + t(newLat)
        subclinical <<- cbind(subclinical[,-1, drop = FALSE], 0) + t(newSubC)
        DeadMat     <<- cbind(DeadMat, clinical[,1])
        DeadMatSur  <<- cbind(DeadMatSur, clinical[,1])
        DeadMat     <<- DeadMat[,-1, drop = FALSE]
        DeadMatSur  <<- DeadMatSur[,-1, drop = FALSE]
        clinical    <<- cbind(clinical[,-1, drop = FALSE], 0) + t(newCli)

        ## Checking for herds that changed status
        ## status 2 -> 3 : ## status latent to subclinical
        tmpIndex <- herds[,7] == 2 & herds[,3] >= 1

        if (sum(tmpIndex) > 0) {
          herds[tmpIndex, 7]               <<- 3
          aHerd$status[herds[tmpIndex, 8]] <<- 3
        }

        ## status 3 -> 4 : # status subclinical to clinical
        tmpIndex <- herds[,7] == 3 & herds[,4] >= 1

        if (sum(tmpIndex) > 0) {
          herds[tmpIndex, 7]               <<- 4
          herds[tmpIndex, 12]              <<- gTime
          aHerd$status[herds[tmpIndex, 8]] <<- 4
        }

        #        ## status 4 -> 7 : status clinical to recovered
        #        tmpIndex <- (herds[,7] == 4 & herds[,5] == herds[,6]) |
        #                    (herds[,7] == 4 &
        #                     herds[,2] == 0 &
        #                     herds[,3] == 0 &
        #                     herds[,4] == 0 &
        #                     herds[,5] >  0 &
        #                     rowSums(DeadMat) == 0)
        #        if (sum(tmpIndex) > 0) {
        #          herds[tmpIndex,7]                   <<- 7
        #          aHerd$status[herds[tmpIndex,8]]     <<- 7
        #          aHerd$immuneTime[herds[tmpIndex,8]] <<- gTime
        #        }
        #        ## status 7 -> 1 : status immune to susceptible
        #        tmpIndex <- herds[,7] == 7 & herds[,5] < herds[,6]
        #       if (sum(tmpIndex) > 0) {
        #          herds[tmpIndex, 7]                <<- 1
        #          aHerd$status[herds[tmpIndex,8]]   <<- 1
        #          aHerd$SusAgain[herds[tmpIndex,8]] <<- gTime
        #          aInfHerd$delInf(herds[tmpIndex,8])
        #
        #          ## Update the indexes of infectious herds
        #          tmpTagged <- which(aHerd$status == 6 &
        #                             aHerd$timeInfected < Inf &
        #                             aHerd$SusAgain == 0)
        #          infHerdNums <<- unique(c(tmpTagged,
        #                                   (aInfHerd$getIDs())[aInfHerd$getstatus() %in% 3:4]))
        #        }

        ## update the number sick and dead animals
        aHerd$Mortality[herds[,8]] <<- (herds[,4] + (herds[,5] * PerDeadAnim))
        aHerd$Survived[herds[,8]]  <<- (herds[,5] * (1 - PerDeadAnim))

        ### update herd size in the aHerd Matrix (TH + DP)
        aHerd$CageSizeVar[herds[,8]]  <<- herds[,6] - (herds[,5] * PerDeadAnim)

        ### Update Cage infectiousness (TH + DP)
        aHerd$Infectiousness[herds[,8]] <<- (herds[,4] + herds[,3]) /
          (herds[,6] - (herds[,5] * PerDeadAnim))

        #print(herds)
        # cat(paste0("Farm ID: ", aHerd$FarmID[herds[,8]],
        #              "; ID: ", aHerd$ID[herds[,8]],
        #              "; Mortality: ", aHerd$Mortality[herds[,8]],
        #              "; time: ", gTime,
        #              "; iter: ", iteration, "\n"))
        MortMat <<- rbind(MortMat, cbind(iteration,
                                         aHerd$FarmID[herds[,8]],
                                         aHerd$ID[herds[,8]],
                                         floor(aHerd$Mortality[herds[,8]]),
                                         gTime))
        fileName <- paste(runID, "InfPenMort.txt", sep = "-")
        write.table(MortMat,
                    fileName,
                    append = TRUE,
                    sep = " ",
                    col.names = FALSE,
                    row.names = FALSE)
        MortMat  <<- matrix(numeric(0), ncol = 5)

      }
    },

    # Get the dead animals animals during last week and sick animals today for the infected herds
    getDead = function(IDs) {
      index <- match(IDs, herds[,8])

      if (any(is.na(index))) {
        warning("ConstructAInfHerd: Some IDs in zones are not in aInfHerd")
        index <- index[!is.na(index)]
      }

      tmp <- round(rowSums(DeadMatSur)[index] * PerDeadAnim)
      return(tmp)
    },

    getInfected = function(IDs) {
      index<-match(IDs,herds[,8])

      if (any(is.na(index))) {
        #warning("ConstructAInfHerd: Some IDs in not in aInfHerd!")
        index <- index[!is.na(index)]
      }

      tmp <-round(herds[index,3] +
                    herds[index,4] +
                    ##Include infected animals that survived death
                    (herds[index,5] * (1 - PerDeadAnim)))

      return(tmp)
    },

    wipe = function() {
      delHerds             <<- matrix(numeric(0), ncol = nColHerds)
      herds                <<- matrix(numeric(0), ncol = nColHerds)
      dimnames(herds)[[2]] <<- herdsCol
      latent               <<- matrix(numeric(0), ncol = ncol(herdtypes$latDurFreq))
      subclinical          <<- matrix(numeric(0), ncol = ncol(herdtypes$SubDurFreq))
      clinical             <<- matrix(numeric(0), ncol = ncol(herdtypes$CliDurFreq))
      DeadMat              <<- matrix(numeric(0), ncol = DaysDead)
      DeadMatSur           <<- matrix(numeric(0), ncol = DaysSurDead)
    }
  )
}
