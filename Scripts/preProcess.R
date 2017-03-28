loadDatasets <- function(query){
  res <- vector()
  for (i in 1:length(query)){
    namer <- getGEO(query[i])
    res <- append(res,namer)
  }
  return(res)
}

loadDatasetsAndSub <- function(query){
  res <- list(length=14)
  for (i in 1:length(query)){
    namer <- getGEO(query[i])
    conv <- as.data.frame(namer)
    if(length(conv[1,]) >= 1000){
      sub <- conv[,1:1000]
      res[i] <- c(res[i],sub)}
    else{
      sub <- cat("\n",query[i]," is a dud...")
    }
    
  }
  return(res)
}

#cast datasets into data frames
convertDatasets <- function(fullVector){
  res <- list()
  for (i in 1:length(fullVector)){
    cat("\n","casting as data frame...","\n")
    cast <- as.data.frame(fullVector[i])
    cat("\n","appending")
    res <- c(res,cast)
  }
  return(res)
}

#preprocess - change column names for easier access
fixColNames <- function(allNames,setName){
  check <- "_series_matrix.txt.gz."
  check <- str_c(setName,check)
  res <- vector()
  for(s in 1:length(allNames)){
    res <- append(res,change <- str_replace(allNames[s],check,""))
  }
  return(res)
}

getDiagnoses <- function(raw){
  res <- vector()
  for(i in 1:length(raw)){
    #nombre <- str_replace(query[i],"PP.csv","")
    #raw <- getGEO(nombre)
    names(raw) <- "access"
    titles <- as.character(raw$access@phenoData@data$title)
    res <- append(titles,res)
  }
  return(res)
}

searchForDiagnoses <- function(raw){
  victim <- getDiagnoses(raw)
  names(raw) <- "access"
  logic <- c(TRUE)
  numTrue <- length(logic[logic == TRUE])
  numFalse <- length(logic[logic == FALSE])
  i <- 1
  path <- names(raw$access@phenoData@data)
  cat("\n",path)
  #altPath <- as.character(1:length(altPath))
  #names(raw$access@phenoData@data) <- altPath
 # cat("\n","New Paths = ",names(raw$access@phenoData@data))
 # str(raw)
 
  while(((numTrue >= (length(logic)-5) | (numFalse >= length(logic) -5 ) )) && i <= length(path)){
  victim <- as.character(raw$access@phenoData@data[i][,1])
  logic <- !(str_detect(victim,"control") | str_detect(victim,"Control") | str_detect(victim,"normal") | str_detect(victim,"NEC") |str_detect(victim,"Huntington"))
  numTrue <- length(logic[logic == TRUE])
  numFalse <- length(logic[logic == FALSE])
  cat("\n","Number true = ",numTrue)
  cat("\n","Length of diagnosis = ",length(logic))
  
  #cat("\n",as.character(raw$access@phenoData@data[i]))
  #cat("\n",slot(raw$access@phenoData@data, as.character(i)))
  victim <- as.character(raw$access@phenoData@data[i][,1])
  #cat("\n","Victim = ",victim)
  i <- i + 1
  cat("\n",i)
  
  }
  return(logic)
}

applyDiagnoses <- function(diagn,data){
  #for(i in 1:length(diagn)){
    victim <- diagn#[i]
    logic <- diagn
    #logiq <- !(str_detect(victim,"control") | str_detect(victim,"Control") | str_detect(victim,"normal") | str_detect(victim,"NEC"))
    data <- cbind(data,as.factor(logic))
    names(data)[length(data)] <- "LEVEL"
  #}
    return(data)
}

preProcess <- function(query){
  setwd(defDir)
  dir.create("PP")
  setwd("PP")
  #groupData <- vector()
  for(i in 1:length(query)){  
    cat("\n","working with dataset ",query[i])
    load <- loadDatasets(query[i])
   # celebrate()
    cat("\n","finished loading ",query[i],", getting diagnoses...")
    d <- searchForDiagnoses(load)
    cat("\n","getting description...")
    desc <- getDescription(query[i],load)
    #groupData <- append(groupData,desc)
    cat("\n","converting to data frame...")
    conv <- as.data.frame(convertDatasets(load))
    cat("\n","fixing column names...")
    names(conv) <- fixColNames(names(conv),query[i])
    cat("\n","applying diagnoses to ",query[i],", under LEVEL column...")
    conv <- applyDiagnoses(d, conv)
    cat("\n","writing as csv...")
    write.table(conv, file=str_c(query[i],"PP.csv"),sep = ",")
    cat("\n","clearing workspace...")
    #celebrate()
    rm(load)
    rm(conv)
    gc()
  }
  res <- groupDataByExp(query)
  setwd(defDir)
  return(res)
}

preProcessSub <- function(query,s,write="write",ret="groups"){
  setwd(defDir)
  dir.create("PP")
  setwd("PP")
  #groupData <- vector()
  for(i in 1:length(query)){  
    cat("\n","working with dataset ",query[i])
    load <- loadDatasets(query[i])
    cat("\n","finished loading ",query[i],", getting diagnoses...")
    #celebrate()
    d <- searchForDiagnoses(load)
    cat("\n","getting description...")
    #desc <- getDescription(query[i],load)
    # groupData <- append(groupData,desc)
    cat("\n","converting to data frame...")
    conv <- as.data.frame(convertDatasets(load))
    conv <- conv[,1:s]
    cat("\n","fixing column names...")
    names(conv) <- fixColNames(names(conv),query[i])
    cat("\n","applying diagnoses to ",query[i],", under LEVEL column...")
    conv <- applyDiagnoses(d, conv)
    cat("\n","writing as csv...")
    write.table(conv, file=str_c(query[i],"PP.csv"),sep = ",")
    #celebrate()
    cat("\n","clearing workspace...")
    
  }
  res <- switch(ret, 
               groups = groupDataByExp(query),
               data = conv)
  rm(load)
  rm(conv)
  gc()
  setwd(defDir)
  return(res)
}

preProcessShort <- function(name, raw = NULL, sub = NULL){
  cat("\n","getting diagnoses...")
  d <- getDiagnoses(raw)
  #cat("\n","converting into data frames...")
  #conv <- as.data.frame(convertDatasets(data))
  cat("\n","fixing column names...")
  names(sub) <- fixColNames(names(sub),name)
  cat("\n","applying diagnoses...")
  sub <- applyDiagnoses(d, sub)
  cat("\n","at this point, diagnoses column is: ",sub$LEVEL)
  cat("\n","writing as document...")
  write.table(sub, file=str_c(name,"TESTPP.csv"),sep = ",")
  cat("\n","clearing memory...")
  #rm(load)
  #rm(conv)
  gc()
}