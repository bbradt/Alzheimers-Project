#main runner for everything

init_workspace <- function(){
  library(knitr)
  #library(reshape2)
  library(GEOquery)
  #library(refGenome)
  #library(rtracklayer)
  #library(limma)
  #library(affy)
  library(stringr)
  #library(lattice)
  #library(ggplot2)
  #library(caret)
  #library(FactoMineR)
  library(FSelector)
  library(preprocessCore)
  library(audio)
  library(RWeka)
  source("main.R") #main functions
  source("preProcess.R") #preprocess functions
  source("enrich.R") #enrichment functions
  source("normalize.R") #normalization functions
  source("classify.R") #classification functions
  source("resulter.R")
  defDir <- getwd()
  setwd(defDir)
}

topp <- function(){
  setwd(defDir)
  setwd("PP")
}

tonorm <- function(){
  setwd(defDir)
  setwd("NORM")
}

toA <- function(){
  setwd(defDir)
  setwd("A")
}

toCM <- function(){
  setwd(defDir)
  setwd("CM")
}

toD <- function(){
  setwd(defDir)
}

defDir <- "C://Users//Brad//Desktop//rdir//" #default directoy - should be getwd()
setwd(defDir) #sets default directory
library(audio) #loads audio directory
death<-load.wave("LOZ_Fail.wav")
queryVector <- c("GSE4757","GSE28146", "GSE4226","GSE1297","GSE33000","GSE44768","GSE44770",
                 "GSE44771","GSE13214")
queryCF <- queryVector[!(queryVector == "GSE33000")]

defDir <- "C://Users//Brad//Desktop//rdir//"

celebration<- load.wave("LOZ_Small.wav")

celebrate <- function(){
  play(celebration)
}

refreshSrcs <- function(){
  gc() #run garbage collector      
  origin <- getwd()
  setwd(defDir) #set default directory
  source("main.R") #load source
  source("preProcess.R") #preprocess functions
  source("enrich.R") #enrichment functions
  source("normalize.R") #normalization functions
  source("classify.R") #classification functions
  source("resulter.R")
  setwd(origin)
}

runFullAnalysis <- function(namesOfSets,type="full",subs = 0, subType="groups"){
  
  setwd("C:\\Users\\Brad\\Desktop\\rdir")
  source("main.R")
  setwd(defDir)
  #bigEvent <- load.wave("LOZ_Big.wav")
  #smallEvent <- load.wave("LOZ_Small.wav")
  finished <- load.wave("LOZ_Theme.wav") #plays at the end of the program

  #play(bigEvent)
  #preprocess, downloading all datasets, converting them to csvs and writing them to files
  #switch function. Full runs full preprocessing stesp. Sub runs subsets. Nopp runs without preprocessing.
  group <- switch(type,
                  full = preProcess(namesOfSets),
                  sub = preProcessSub(namesOfSets,s = subs,ret = subType),
                  nopp = groupDataByExp(namesOfSets))
    
  #play(bigEvent)
  #reload the written csvs one-by-one (for memory preservation), and write their normalized equivalents
  loadNormalizeWrite(namesOfSets)
  
  #create the normalization directory
  dir.create("NORM")
  
  cat("\n","Beginning A compilation and enrichment...")
  
  for(i in 1:length(namesOfSets)){ #for all datasets 
    setwd("NORM")
    load <- read.csv(str_c(namesOfSets[i],"NORM.csv"),head=TRUE,sep=",") #read the file
    #play(smallEvent)
    aEnrichWriteAllMeas(load,namesOfSets[i]) #write and enrich them all
   # play(smallEvent)
    rm(load)
    gc() #remove the garbage collector
    setwd(defDir)
  }
 # play(bigEvent)
  
  setwd(defDir)
  
  cat("\n","Beginning B compilation and enrichment...")
  
  for(j in 1:length(group[1,])){ #for all datasets
    load <- bCompile(group[,j],row.names(group)) #load and compile the stuff from the csvs
    #play(smallEvent)
    bEnrichWriteAllMeas(load,names(group)[j]) #write all measures for the loaded set 
    #play(smallEvent)
  }
  
 # play(bigEvent)
  setwd(defDir)
  
  cat("\n","Beginning performance...")
  writeAllPerformances() #write all classification successes
  setwd(defDir)
  play(finished)
}