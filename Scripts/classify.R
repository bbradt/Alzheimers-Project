#classification jokes

#build training and testing sets
getIndices <- function(percentTrain,dataset){
  actual <- percentTrain*0.01 #puts percentage as decimal
  trSize <- as.integer(length(dataset[,1])*actual) #training size formula
  cat("\n","training size is ",trSize)
  #trSize <- as.numeric(trSize)
  train <- sample.int(length(dataset[,1]),size = trSize) 
  #train <- dataset[trainInd,]
  v <- c(1:20)
  test <- v[!(v %in% train)]
  #test <- dataset[testInd,]
  return(list(train,test)) #return indices as list
}

#access training and testing sets using the indices function
setTrainAndTest <- function(percentTrain,dataset){
  indDF <- getIndices(percentTrain,dataset)
  train <- dataset[as.numeric(unlist(indDF[1])),]
  test <- dataset[as.numeric(unlist(indDF[2])),]
  return(list(train,test))
}

#run KNN
runKNN <- function(p,set){
    cat("\n","running knn...")
    seps <- setTrainAndTest(p,set) #runs train and test
    tr <- as.data.frame(seps[1]) #casts as data frame
    te <- as.data.frame(seps[2]) #casts as data frame
    r <- knn(tr[,1:1000],te[,1:1000],tr$LEVEL) #run knn
    s <- compClassRes(r,te$LEVEL) #compares results of knn predictions
    cat("\n",s," correct predictions","\n")
    return(s)
}

compClassRes <- function(res,actual){
  ret <- res[actual]
  return(length(ret[ret == TRUE]))
}

classNames <- c("ibk","j48","lmt","stump",
                "ada","bag")

classify <- function(data,type){
  res <- switch(type,
                ibk = IBk(LEVEL~.,data), 
                lbr = LBR(LEVEL~., data),
                j48 = J48(LEVEL~.,data),
                lmt = LMT(LEVEL~.,data),
                msp = M5P(LEVEL~.,data), 
                stump = DecisionStump(LEVEL~.,data),
                logi = Logistic(LEVEL~.,data),
                smo = SMO(LEVEL~.,data),
                ada = AdaBoostM1(LEVEL~.,data),
                bag = Bagging(LEVEL~.,data),
                lBoost = LogitBoost(LEVEL~.,data),
                mBoost = MultiBoostAB(LEVEL~.,data),
                stack = Stacking(LEVEL~.,data),
                cs = CostSensitiveClassifier(LEVEL~.,data))
  return(res)
}

perform <- function(data,these){
  ini <- vector(length = 150)
  res <- data.frame(ini)
  nameVec <- vector()
  for(i in 1:length(these)){
    cat("\n",i)
    performance <- vector()
    cat("\n","Classifying with ", these[i],"...")
    performer <- classify(data,these[i])
    #celebrate()
    nameVec <- append(nameVec,these[i])
    predictions <- performer$predictions
    predictions <- as.data.frame(predictions == 1)
    if(!(i==1)){res <- cbind(res,predictions)}
       else{res <- data.frame(predictions)}
  }
  names(res) <- nameVec
  return(res)
}

rate <- function(performer,performance,col,classCol){
  cat("\n","What the hell is this: ",performance[,col])
  cat("\n","And this: ",performer[,classCol])
  #if(length(performance[FALSE] == 0)){return(data.matrix(1:100,"FAIL"))}
  pred <- as.factor(performance[,col])
  cat("\n",levels(pred))
  act <- as.factor(performer[,classCol])
  cat("\n",levels(act))
  if(!(length(levels(pred)) == length(levels(act)))){
    levels(pred) <- levels(act)
  }
  res <- confusionMatrix(pred,act)

  return(res)
}

writeAPerformances <- function(){
  cat("\n","Beginning to write performances...")
  setwd(defDir)
  setwd("A")
  set <- dir()[str_detect(dir(),"EA")]
  cat("\n","Readable files are: ", set)
  for(i in 1:length(set)){
    setwd(defDir)
    setwd("A")
    cat("\n","Attempting to read ",set[i])
    thisData <- read.csv(set[i])
    thisName <- str_replace(set[i],".csv","")
    thisData <- thisData[sapply(thisData,function(thisData) !any(is.na(thisData)))]
    thisAct <- perform(thisData,classNames)
    for(j in 1:length(thisAct[1,])){
      cat("\n","Beginning judgment...")
      thisSucc <- rate(thisData,thisAct,j,length(thisData))
      setwd(defDir)
      dir.create("CM")
      setwd("CM")
      cat("\n","Writing Judgment... for ",names(thisAct)[j])
      newName <- str_c(thisName,names(thisAct)[j],".csv")
      cat("\n",newName)
      thisSucc <- as.table(thisSucc)
      write.table(thisSucc,file=newName,sep=",")
      #celebrate()
    }
    # celebrate()
  }
  setwd(defDir)
  celebrate()
}



writeAllPerformances <- function(){
  cat("\n","Beginning to write performances...")
  setwd(defDir)
  setwd("A")
  set <- dir()[str_detect(dir(),"EA")]
  cat("\n","Readable files are: ", set)
  for(i in 1:length(set)){
    setwd(defDir)
    setwd("A")
    cat("\n","Attempting to read ",set[i])
    thisData <- read.csv(set[i])
    thisName <- str_replace(set[i],".csv","")
    
    thisAct <- perform(thisData,classNames)
    for(j in 1:length(thisAct[1,])){
      cat("\n","Beginning judgment...")
      thisSucc <- rate(thisData,thisAct,j,length(thisData))
      setwd(defDir)
      dir.create("CM")
      setwd("CM")
      cat("\n","Writing Judgment... for ",names(thisAct)[j])
      newName <- str_c(thisName,names(thisAct)[j],".cm")
      cat("\n",newName)
      write.matrix(thisSucc,file=newName,sep=",")
      #celebrate()
    }
   # celebrate()
  }
  setwd(defDir)
  setwd("B")
  set <- dir()[str_detect(dir(),"EB")]
  for(i in 1:length(set)){
    setwd(defDir)
    setwd("B")
    thisData <- read.csv(set[i])
    thisName <- str_replace(set[i],".csv","")
    thisAct <- perform(thisData,classNames)
    for(j in 1:length(thisAct[1,])){
      thisSucc <- rate(thisData,thisAct,j,length(thisData))
      setwd(defDir)
      dir.create("CM")
      setwd("CM")
      cat("\n","Writing Judgment... for ",names(thisAct)[j])
      write.matrix(thisSucc,file=str_c(thisName,names(thisAct)[j],".cm"),sep=",")
      #celebrate()
    }
   # celebrate()
  }
}