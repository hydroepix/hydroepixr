# Replacing createHEvars
# reads data and initializes variables
he_create_vars <- function() {
  ## Load Data
  if (verbose) cat("Loading Data\n")

  ## read in herd information from comma delimited infofile
  eval.parent(expression(aHerd <- NULL))
  aHerd <<- as.list(read.table(infofile,
                               sep = ",",
                               dec = ".",
                               header = TRUE))

  ## Making sure the ID column is named "ID"
  names(aHerd)[1] <<- "ID"

  ## Check aHerd for non-unique IDs
  if (length(unique(aHerd$ID)) < length(aHerd$ID)) {
    stop("Herd ID numbers are not unique. Simulations Fails",
         paste(unique(aHerd$ID[duplicated(aHerd$ID)]),collapse=", "))
  }

  ## read in herd types from comma delimited typesfile
  eval.parent(expression(herdtypes <- NULL))
  herdtypes <<- as.list(read.table(typesfile,sep=",",as.is=TRUE,header=TRUE))

  ## modify one column of herdtypes table if tornado plot option is selected
  if (!is.null(tornCol)) {

    if (is.numeric(herdtypes[[tornCol]])) {
      herdtypes[[tornCol]] <<- 0.9 * herdtypes[[tornCol]]
    } else {
      herdtypes[[tornCol]] <<- paste(as.character(tornMult),
                                     "*",herdtypes[[tornCol]])
    }
  }

  ## Parsing 'herdtypes' once and for all:
  for (i in 3:length(herdtypes)) {
    herdtypes[[i]] <<- parse(text = herdtypes[[i]])
  }

  ### Read the movement and category matrises for All and for weaners
  eval.parent(expression(DistMat <- NULL))
  DistMat <<- as.matrix(read.table(fileDistMat,
                                   sep = ";",
                                   dec = "."))


  ## Further transformation of latent and subclinical duration frequencies
  eval_x <- function(x) if (is.character(x)) eval(parse(text = x)) else eval(x)

  herdtypes$latDurFreq <<- t(sapply(herdtypes$latDurFreq, eval_x))
  herdtypes$SubDurFreq <<- t(sapply(herdtypes$SubDurFreq, eval_x))
  herdtypes$CliDurFreq <<- t(sapply(herdtypes$CliDurFreq, eval_x))

  ## Declaring some variables in the parent scope.(Accessible to all functions)
  eval.parent(expression(outbreakDetectedLast <- outbreakDetected <- FALSE))
  eval.parent(expression(depopQueue   <- NULL))
  eval.parent(expression(beingDepoped <- NULL))
  eval.parent(expression(indexHerd    <- NA))
  eval.parent(expression(iteration    <- NULL))
  eval.parent(expression(infHerdNums  <- NULL))
  eval.parent(expression(gTime        <- 0))
  eval.parent(expression(initHideMap  <- hideMap))

  if(is.null(initHideMap)) eval.parent(expression(hideMap <- T))

  eval.parent(expression(gMaxHerds <- length(aHerd$herdType)))
  aHerd$Sus <<- rep(NA, gMaxHerds)
  aHerd$K   <<- rep(NA, gMaxHerds)
  aHerd$cullEligible <<- rep(T, gMaxHerds)

  if (!identical(cullTypes,"all")) {
    aHerd$cullEligible <<- aHerd$herdType %in% cullTypes
  }

  ## choose functions
  eval.parent(expression(
    RFtrans <- switch(RFstoch + 1,
                      function(z,nn,pp){ nn*pp }, # if False
                      rbinom                      # if True
    )
  ))

  eval.parent(expression(
    Tinfs <- switch(Tstoch + 1,
                    function(contactHerds,dProbInfection,Sus) {
                      rbinom(length(contactHerds),1,dProbInfection)
                    },
                    function(contactHerds,dProbInfection,Sus) {
                      rbinom(length(contactHerds),aHerd$Sus[contactHerds],
                             1 - (1 - dProbInfection) ^ (1 / Sus[contactHerds]))
                    })
  ))

  ## Set initial herd status to 1 for every herd unless values taken from file
  if (ignoreStatus) {
    eval.parent(expression(initStatus       <- rep(1, gMaxHerds)))
    eval.parent(expression(initTimeInfected <- rep(Inf, gMaxHerds)))
  } else {
    eval.parent(expression(initStatus       <- aHerd$status))
    eval.parent(expression(initTimeInfected <- aHerd$timeInfected))
  }


  eval.parent(expression(AllInfCages <- NULL))
  AllInfCages <<- matrix(numeric(0), ncol = 8)
  NAMEInf      <- paste(runID,"AllInfCages.txt", sep = "-")
  write.table(AllInfCages, NAMEInf, sep = " ")

  eval.parent(expression(SumResOut  <- NULL))
  SumResOut  <<- matrix(numeric(0), ncol = 10)
  NAME <- paste(runID, "ISA.txt", sep = "-")
  write.table(SumResOut, NAME, sep = " ")

  ######################################################################
  ### initiate the Matrix that includes surveyed herds (TH) Oct 2015 ###
  ######################################################################
  if(Detailed){

    eval.parent(expression(SurvMatOut   <- NULL))
    eval.parent(expression(DepopMatOut  <- NULL))
    eval.parent(expression(PreEmpMatOut <- NULL))

    SurvMatOut   <<- matrix(numeric(0), ncol = 3)
    DepopMatOut  <<- matrix(numeric(0), ncol = 3)
    PreEmpMatOut <<- matrix(numeric(0), ncol = 3)

    NAMESH <- paste(runID,"SurvayedHerds.txt", sep = "-")
    NAMED  <- paste(runID,"DepopHerds.txt",    sep = "-")
    NAMEPE <- paste(runID,"PreEmpHerds.txt",   sep = "-")

    write.table(SurvMatOut,  NAMESH, col.names = FALSE, row.names = FALSE)
    write.table(DepopMatOut, NAMED,  col.names = FALSE, row.names = FALSE)
    write.table(PreEmpMatOut,NAMEPE, col.names = FALSE, row.names = FALSE)
  }
}
