#this will be the enrichment script 

#there will be four functions - metadata compiler, A enrichment, B enrichment and C enrichment

#initializes a vector with raw datasets

thresh <- 0 #the threshold for significance 



groupedByRegion <- data.frame()

#metadata compiler function - gets all metadata and places it in a table
  #data values - structure name, disease level, has controls, has age, has gender

getDescription <- function(name,raw){
  names(raw) <- "access"
  mt <- raw$access@phenoData@data$description
  return(mt[1])
}

groupDataByRegion <- function(query){
  setwd(defDir)
  regions <- read.table("REGIONTABLE.csv",header=TRUE,sep=",",row.names = queryVector, colClasses="logical")
  regions <- regions[query,]
  return(regions)
}

groupDataByExp <- function(query){
  setwd(defDir)
  exp <- read.table("EXPTABLE.csv",header=TRUE,sep=",",row.names=queryVector,colClasses="logical")
  exp <- exp[query,]
  return(exp)
}

# groupDataByRegion <- function(query, descVector){
#   groupHeaders <- c("tangles","hippo","parietal","frontal","micro","neo","visual",
#                     "bellum","blood","entorhinal","temporal")
#   cat(query," ",length(query))
#   tan <- vector()
#   hip <- vector()
#   pari <- vector()
#   front <- vector()
#   micro <- vector()
#   neo <- vector()
#   visual <- vector()
#   bellum <- vector()
#   blood <- vector()
#   entor <- vector()
#   tempor <- vector()
#   #prefront <- vector()
#   for (i in 1:length(query)){
#       cat(" ",query[i])
#       if(query[i]=="GSE4757"){tan <- append(tan,TRUE)}
#       else{tan <- append(tan,FALSE)}
#       #tan <- append(tan,str_detect(descVector[i],groupHeaders[1]))
#       hip <- append(hip,str_detect(descVector[i],groupHeaders[2]))
#       pari <- append(pari,str_detect(descVector[i],groupHeaders[3]))
#       front <- append(front,str_detect(descVector[i],groupHeaders[4]))
#       micro <- append(micro,str_detect(descVector[i],groupHeaders[5]))
#       neo <- append(neo,str_detect(descVector[i],groupHeaders[6]))
#       visual <- append(visual,str_detect(descVector[i],groupHeaders[7]))
#       bellum <- append(bellum,str_detect(descVector[i],groupHeaders[8]))
#       blood <- append(blood,str_detect(descVector[i],groupHeaders[9]))
#       entor <- append(entor,str_detect(descVector[i],groupHeaders[10]))
#       tempor <- append(tempor,str_detect(descVector[i],groupHeaders[11]))
#       #prefront <- append(prefront,str_detect(descVector[i],groupHeaders[12]))
#     
#   }
#   res <- as.data.frame(cbind(tan,hip,pari,front,micro,neo,visual,bellum,blood,entor,tempor))
#   row.names(res) <- query
#   return(res)
# }



evaluator <- function(subset,data) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(data))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- data[test.idx, , drop=FALSE]
    train <- data[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "LEVEL"), train)
    error.rate = sum(test$LEVEL != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

measureNames <- c("chi","infogain","oner","forst","symmU")

measure <- function(data,meas){
  res <- tAtOnce(data,meas,1000)
  return(res)              
}

tAtOnce <- function(data,meas,t){
  #assertError(t == 0, verbose=TRUE)
  mult<- length(data) - 1
  mulr <- 1
  while(!(mulr==0)){
    mult <- (mult - 1)
    mulr <- length(data)%%mult
  }
  sek <- seq(from=1,to=length(data),by=t-1)
  cat("\n",sek)
  missed <- data[,sek[length(sek)]:length(data)]
  str(missed)
  total<-data.frame(matrix(nrow=length(data[,1])))
  for(i in 1:((length(sek)-1))){
    subD <- data[,sek[i]:sek[i+1]]
    subD$LEVEL <- data$LEVEL  
    res <- switch(meas,
                  anova = subD[,cutoff.k(anovaScores(subD[,1:(length(subD)-1)],subD$LEVEL))],
                  cf = cfs(LEVEL~., subD),
                  chi = subD[,cutoff.biggest.diff(chi.squared(LEVEL~.,subD))],
                  cons = consistency(LEVEL~.,subD),
                  infogain = subD[,cutoff.biggest.diff(information.gain(LEVEL~.,subD))],
                  gainratio = subD[,cutoff.biggest.diff(gain.ratio(LEVEL~.,subD))],
                  symmU = subD[,cutoff.biggest.diff(symmetrical.uncertainty(LEVEL~.,subD))],
                  lCorr = subD[,cutoff.biggest.diff(linear.correlation(LEVEL~.,subD))],
                  rCorr = subD[,cutoff.biggest.diff(rank.correlation(LEVEL~.,subD))],
                  oner = subD[,cutoff.biggest.diff(oneR(LEVEL~.,subD))],
                  forst = subD[,cutoff.biggest.diff(random.forest.importance(LEVEL~.,subD,importance.type=1))],
                  relief = subD[,cutoff.biggest.diff(relief(LEVEL~.,subD,neighbours.count=5,sample.size=length(subD[,1])))]
    )
    total <- cbind(total,res)
    i <- i + 1
    
  }
  subD <- missed
  res <- switch(meas,
                anova = subD[,cutoff.k(anovaScores(subD[,1:(length(subD)-1)],subD$LEVEL))],
                cf = cfs(LEVEL~., subD),
                chi = subD[,cutoff.biggest.diff(chi.squared(LEVEL~.,subD))],
                cons = consistency(LEVEL~.,subD),
                infogain = subD[,cutoff.biggest.diff(information.gain(LEVEL~.,subD))],
                gainratio = subD[,cutoff.biggest.diff(gain.ratio(LEVEL~.,subD))],
                symmU = subD[,cutoff.biggest.diff(symmetrical.uncertainty(LEVEL~.,subD))],
                lCorr = subD[,cutoff.biggest.diff(linear.correlation(LEVEL~.,subD))],
                rCorr = subD[,cutoff.biggest.diff(rank.correlation(LEVEL~.,subD))],
                oner = subD[,cutoff.biggest.diff(oneR(LEVEL~.,subD))],
                forst = subD[,cutoff.biggest.diff(random.forest.importance(LEVEL~.,subD,importance.type=1))],
                relief = subD[,cutoff.biggest.diff(relief(LEVEL~.,subD,neighbours.count=5,sample.size=length(subD[,1])))]
  )
  total<- cbind(total,res)
  return(total)
}

#A-enrichment functions
  #creates several types of signifance measure for the dataset
  #creates several new datasets with removed gene for each measure 
  #stores measures for each dataset into statistics table with set ID and other relevant values
  #outputs files for each A-enriched dataset
aEnrichWriteAllMeas<- function(data,nameD){
  setwd(defDir)
  dir.create("A")
  setwd("A")
  len <- vector()
  
  for (i in 1:length(measureNames)){
    gc()
    cat("\n","Measuring with ",measureNames[i]," ...")
    cat("\n","Total Measure is ",length(measureNames))
    cat("\n","WAT",nameD," ")
    dat <- measure(data,measureNames[i])
    #celebrate()
    cat("\n",measureNames[i])
    cat("\n","MADE IT HERE1")
    out <- str_c(nameD,measureNames[i],"EA.csv")
    cat("MADE IT HERE2")
    cat("\n","writing ",out," to file...")
    dat$LEVEL <- data$LEVEL
    write.table(dat,file=out,sep=",")
    #celebrate()
    len <- append(len,length(dat))
  }
  setwd(defDir)
  return(len)
  
}


#B-enrichment functions
  #for each preprocessed dataset
  #check the metadatatables and look for groups with compatible metadata
  #compile these raw datasets into larger datasets
  #calculate new threshold
  #remove statistically significant genes

bCompile<-function(groupFrameCol,rowNames){
  setwd(defDir)
  setwd("PP")
  
  grpData <- data.frame(check.names=FALSE,check.rows=FALSE)
  cat("\n","Checking for experiment from ",names(groupFrameCol))
  for(i in 1:length(groupFrameCol)){
    if(groupFrameCol[i] == TRUE){
      nam <- str_c(rowNames[i],"PP.csv")
      cat("\n","Match at ", rowNames[i],"...")
      toBnd <- read.csv(nam)
      whereNoMatch <- names(toBnd[,!(names(toBnd)==names(grpData))])
      lev <- vector()
      if(!(length(whereNoMatch) == 0)){
        cat("\n","Disparate for ... ", as.character(whereNoMatch))
        toBndMatch <- toBnd[,(names(toBnd)==names(grpData))]
        cat("\n","to bind matches with ",length(toBndMatch), " records and has ", length(toBndMatch[,1]), " samples")
        toBndNoMatch <- toBnd[,!(names(toBnd)==names(grpData))]
        cat("\n","to bind doesn't match with ",length(toBndNoMatch), " records and has ", length(toBndNoMatch[,1]), " samples")
        grpMatch <- grpData[,(names(toBnd)==names(grpData))]
        cat("\n","binder matches with ",length(grpMatch), " records and has ", length(grpMatch[,1]), " samples")
        grpNoMatch <- grpData[,!(names(toBnd)==names(grpData))]
        grpNoMatch2 <- grpNoMatch
        cat("\n","binder doesne't match with ",length(grpNoMatch), " records and has ", length(grpNoMatch[,1]), " samples")
        grpNoMatch <- grpNoMatch[1:length(toBndMatch[,1]),]
        cat("\n","binder no match has been shortened to row length ",length(grpNoMatch[,1]))
        grpNoMatch[,1:length(grpNoMatch)] <- rep(0,length(grpNoMatch[,1]))
        cat("\n","binder no match has been filled like so ")
        toBndMatch <- cbind(toBndMatch,grpNoMatch)
        cat("\n","to bind has been bound with binder no match for length ", length(toBndMatch))
        toBndMatch <- cbind(toBndMatch,toBndNoMatch)
        cat("\n","to bind has been bound with its disparate values...")
        toBndNoMatch[,1:length(toBndNoMatch)] <- rep(0,length(toBndNoMatch[,1]))
        cat("\n","to bind no match has been filled... ")
        #toBndNoMatch[length(toBndNoMatch[,1]):length(grpMatch[,1]),] <- rep(0,length(grpMatch[,1]))
        blanks <- rep(0,length(grpMatch[,1]))
        for(i in 1:(length(grpMatch[,1]) - length(toBndNoMatch[,1]))){toBndNoMatch <- rbind (toBndNoMatch,blanks)}
        cat("\n","to bind no match has been extended to  ",length(toBndNoMatch[,1]))
        grpMatch <- cbind(grpMatch,toBndNoMatch) 
        grpMatch <- cbind(grpMatch,grpNoMatch2)
        cat("\n","bind match has been bound with to bind no match")
        cat("\n","trying to bind grpMatch with ", length(grpMatch)," columns with toBnd with ", length(toBndMatch))
        grpData <- rbind(grpMatch,toBndMatch)
        cat("\n","result has been compiled")
        names(grpData)[length(grpData)] <- "LEVEL"
      #cat("reached...")
      }
      else{
         lev1 <- grpData$LEVEL
         lev2 <- toBnd$LEVEL
         levs <- c(lev1,lev2)
         grpData <- rbind(grpData,toBnd)
         grpData$LEVEL <- levs
      }
      #celebrate()
    }
  }
  setwd(defDir)
  dir.create("B")
  setwd("B")
  write.table(grpData,file = str_c("LASTBCOMPILE.csv"), sep=",")
  setwd(defDir)
  grpData[is.na(grpData)] <- 0
 # names(grpData)[,length(grpData)] <- "LEVEL"
  return(grpData)
}

bEnrichWriteAllMeas <- function(data,name){
  setwd(defDir)
  dir.create("B")
  setwd("B")
  lev <- data$LEVEL
  len <- vector()
  #data <- compileNormData(data)
  #celebrate()
  for (i in 1:length(measureNames)){
    setwd(defDir)
    setwd("B")
    dat <- measure(data,measureNames[i])
    dat$LEVEL <- lev
    out <- str_c(name,measureNames[i],"EB.csv")
    cat("\n","writing ",out," to file...")
    write.table(dat,file=out,sep=",")
    #celebrate()
    len <- append(len,length(dat))
  }
  setwd(defDir)
  return(len)
}

#C-enrichment functions
  #works on a-enriched and b-enriched datasets
  #remove all Non-AD Data (including controls)
  #(optional) remove all data not relevant to mid-level AD.
  #compile all enriched-b datasets into one file
  #remove genes which are in non-tangle but not in tangle
  #calculate 'significance' for each gene between all significant structures
  #remove less insignificant genes

locEnrichedData <- function(){
  res <- vector()
  setwd(defDir)
  #setwd("A")
  #res <- append(res,dir())
  #setwd(defDir)
  setwd("B")
  res <- append(res.dir())
  setwd(defDir)
  return(res)
}

cCompile <-function(enrichedNames){
  setwd(defDir)
  grdData <- data.frame()
  for(i in 1:length(enrichedNames)){
    dat <- read.csv(enrichedNames[i])
    dat <- dat[dat$LEVEL == FALSE,]
    grpData <- rbind(grpData,dat)
  }
  return(grpData)
}

cEnrichWriteAllMeas <- function(data,name){
  setwd(defDir)
  dir.create("C")
  setwd("C")
  len <- vector()
  for (i in 1:length(measureNames)){
    dat <- measure(data,measureNames[i])
    out <- str_c(name,measureNames[i],"EC.csv")
    cat("\n","writing ",out," to file...")
    write.table(dat,file=out,sep=",")
    len <- append(len,length(dat))
  }
  setwd(defDir)
  return(len)
}
  