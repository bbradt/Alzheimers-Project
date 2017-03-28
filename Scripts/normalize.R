#this will be the normalization script

#plots datasets before normalization (Gaussian distribution)
#puts these plots onto a markdown 

#if curves are highly different, then it uses several kinds of normalization to get the difference to be minimized

#the logNorm function takes a set of samples, as well as the number of samples in the set,
#it then coerces the samples into numeric vectors without ordering them, to preserve the initial order of the dataset
#finally, it applies a logbase2 operation to the numeric vector in order to normalize dataset
#logbase2 is a common normalizing function for bioinformatic datasets...
logNorm <- function(sampleSet,nsam){
  cat("\n","casting as non-sorted numeric...")
  cast <- as.numeric(sampleSet[nsam,])
  #sast <- sort(cast)
  cat("\n","Casting as log...")
  l <- log2(cast)
  return(l)
}

loessNorm <- function(dataset){
  cat("\n","casting as non-sorted matrix...")
  cast <- data.matrix(dataset)
  cat("\n","applying Loess...")
  l <- loess.normalize(cast, subset = sample(1:((dim(cast)[1])),length(cast[,1])),log.it=FALSE)
  return(l)
}

#compileNormData is a convenience function used to mash together entire normalized datasets
#rbind use here might not be optimal
compileNormData <- function(sampleSet){
  cat("Normalizing and Compiling data...","\n") 
 # dat <- data.frame()
 # for(i in 1:length(sampleSet[,1])){
   # cat("\n","***","\n","converting row #",i," of ", length(sampleSet[,1]))
    #n <- logNorm(sampleSet,i)
    #n <- 
    #cat("\n","Binding rows to output...")
    #dat <- rbind(dat,n)
 # }
 ## dat[is.na(dat)[TRUE]] <- 0
  dat <- sampleSet[,!(names(sampleSet) == "LEVEL")]
  nam <- names(sampleSet)
  rnam <- row.names(sampleSet)
  #fin <- loessNorm(dat)
  #fin <- loessNorm(sampleSet)
  targ <- normalize.quantiles.determine.target(data.matrix(dat))
  fin <- normalize.quantiles.use.target(data.matrix(dat),targ)
  fin <- as.data.frame(fin)
  fin$LEVEL <- sampleSet$LEVEL
  row.names(fin) <- rnam
  names(fin) <- nam
  cat("\n","Finished...","\n")
  return(fin)  
}

#writeNormData writes normalized datasets to CSV files in order to keep datasets out of memory
writeNormData <- function(sampleSet,name){
  cat("\n","Writing normalized data to csv files for dataset ", name, "\n")
  #oldDir <- getwd()
#  if (!("normalizedNeurosets" %in% dir())){ #create a directory if it isn't extant
#    #dir.create("normalizedNeurosets")
#  }
  setwd(defDir)
  dir.create("NORM")
  setwd("NORM")
  write.table(compileNormData(sampleSet),file = str_c(name,"NORM.csv"),sep = ",",na="0")
  #celebrate()
  cat("\n","Data written to folder NORM in current directory...","\n")
  setwd(defDir) #reset directory
}

#calls normalization functions and writes normalized files to csvs
loadNormalizeWrite <- function(PPVector){
  setwd(defDir)
  #setwd(dire)
  setwd("PP")
  for(i in 1:length(PPVector)){
    cat("\n","...reading ",i," of ", length(PPVector), "datasets into memory...")
    nam <- str_c(PPVector[i],"PP.csv")
    setwd(defDir)
    setwd("PP")
    up <- read.csv(nam,head=TRUE,sep=",")
    #celebrate()
    nombre <- str_replace(PPVector[i],"PP.csv","")
    cat("\n","...working on dataset... ",nombre,"\n"," ...beginning conversion...","\n")
    setwd(defDir)
    writeNormData(up,nombre)
    rm(up)
    gc()
    setwd(defDir)
    setwd("PP")
    #celebrate()
  }
  setwd(defDir)
}

#*************************** PLOT FUNCTIONS ******************************************
#the below plot functions reperform the above normalization and should be used only for final plotting
#or they should be rewritten, or included 

#plotOldNorm plots the old version of a dataset versus the normalized dataset
#this function reperforms normalization and should be used only for graphic generation
plotOldNorm <- function(sampleSet,sample){
  s <- sort(as.numeric(sampleSet[sample,]))
  l <- log2(s)
  par(mfrow= c(2,1))
  plot(s,dnorm(s),type="l",main="Before Normalization",xlab="Signal(raw)",ylab="dnorm value")
  plot(l,dnorm(l), type="l",main = "After Normalization",xlab="Signal(log2)",ylab="dnorm value")
}

#plotOldNorms is a convenience function which plots all samples of datasets, normalized and not
#the plots generally go over one-another, and do not otherwise accomplish much
#this also reperforms normalization, and should be used sparingly 
plotOldNorms <- function(sampleSet,samples){
  myCol = c("red","blue","brown","gray","purple")
  s <- sort(as.numeric(sampleSet[1,]))
  l <- log2(s)
  par(mfrow= c(2,1))
  plot(s,dnorm(s),type="l",main="Before Normalization",xlab="Signal(raw)",ylab="dnorm value")
  v <- vector()
  for(i in 2:length(samples)){
    s <- sort(as.numeric(sampleSet[i,]))
    lines(s,dnorm(s),col=sample(myCol,length(myCol)))
    #print(v)
  }
  plot(l,dnorm(l), type="l",main = "After Normalization",xlab="Signal(log2)",ylab="dnorm value")
  for(j in 2:length(samples)){
    s <- sort(as.numeric(sampleSet[i,]))
    l <- log2(s)
    lines(s,dnorm(s),col=sample(myCol,length(myCol)))
  }
}


