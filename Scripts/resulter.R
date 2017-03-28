# for compiling and plotting

successMeasures <- c("accuracy","precision","recall")



pullAndCompileResults <- function(evalName,measures, dataName = "."){
  setwd(defDir)
  setwd("CM")
  #dir.create("COMP")
  set <- dir()[str_detect(dir(),evalName)]
  set2 <- set[str_detect(set,dataName)]
  if(length(set2) <= 0){
    return(NA)
  }
  set2 <- set2[!str_detect(set2,"NA")]
  res <- measureMultiple(measures,set2)
  return(res)
}


cfMeasure <- function(type,mat){
  a <- mat[1,1]
  c <- mat[1,2]
  b <- mat[2,1]
  d <- mat[2,2]
  total <- a + b + c + d
  res <- switch(type,
                accuracy = (a + d)/(total),
                precision = (a)/(a + c),
                recall = (a)/(a + b))
  return(res)
}

measureMultiple <- function(types, matrixNames){
  res <- vector()
  typeAll <- rep_len(types,length(matrixNames))
  for(i in 1:length(matrixNames)){
    setwd(defDir)
    setwd("CM")
    #cat(matrixNames[i])
    data <- read.csv(matrixNames[i])
    res <- append(res,cfMeasure(typeAll[i],data))
  }
  
  return(res)
}

makeFrame <- function(xFactors,successMeasure,principle){
  setwd(defDir)
  setwd("CM")
  for(i in 1:length(xFactors)){  
    setwd(defDir)
    setwd("CM")
    
    puller <- pullAndCompileResults(xFactors[i],successMeasure,dataName = principle)
    if(!is.na(puller)){
      if(i ==1){fram <- data.frame(matrix(ncol = length(xFactors),nrow=length(puller)))}
      if(!(length(puller) == length(fram[,1]))){
        diff <- length(fram[,1]) - length(puller)
        cat("\n",diff)
        filler <- rep_len(puller[1:diff],diff)
        puller <- append(puller,filler)
      }
      fram[,i] <- puller
      names(fram)[i] <- xFactors[i]
    }
  }
  #cat("\n",fram)
  fram <- fram[sapply(fram,function(fram) !any(is.na(fram)))]
  #row.names(fram) <- xFactors
}

plotMeans <- function(query,successMeasure,against){
  res <- data.frame(matrix(nrow=8))
  colrs <- randomColors(8)
  for(i in 1:length(against)){
    frame <- makeFrame(query,successMeasure,against[i])
    avg <- sapply(frame,function(frame) mean(frame))
    if(i == 1){
      plot(avg,main=successMeasure,type="b",col=colrs[i],ylim=c(0,1))
      legend(2000,9.5, c("Health","Defense")) 
    }
    else{
      lines(avg,col=colrs[i])
      points(avg,col=colrs[i])
    }
  
  }
  
}

plotMeans2 <- function(query,successMeasures,against){
  
  colrs <- randomColors(3)
  par(mfrow=c(length(against),length(successMeasures)),mar = rep(2, 4),cex.axis=0.5)
  #labs <- as.vector(sapply(query,function(query) str_replace(query,"GSE","")))
  for(j in 1:length(successMeasures)){
    for(i in 1:length(against)){
      frame <- makeFrame(query,successMeasures[j],against[i])
      avg <- sapply(frame,function(frame) mean(frame))
      avg <- avg[!(avg==0)]
      title <- str_c("Mean ",successMeasures[j], " for ", against[i])
      plot(avg,main=title,type="b", col=colrs[j],ylim=c(0,1),axes=FALSE)
      axis(2)
      axt <- names(frame)
      axt <- sapply(axt,function(axt) str_replace(axt,"GSE",""))
      axis(1,at=1:length(avg),labels=axt)
    }
  }
}


randomColor <- function(){
  
  return(sample(colors(),1))
}

randomColors <- function(s){
  
  return(sample(colors(),s))
}

makeManyBoxes <- function(query,successMeasures,against){
  
  par(mfrow=c(length(against),1),mar = rep(2, 4),cex.axis=0.5)
  #labs <- as.vector(sapply(query,function(query) str_replace(query,"GSE","")))
  colrs <- c("red","green","blue")
  colrs <- col2rgb(colrs)
  colrs <- rgb(colrs[,1],colrs[,2],colrs[,3],20,max=255)
  for(i in 1:length(against)){
    for(j in 1:length(successMeasures)){
        frame <- makeFrame(query,successMeasures[j],against[i])
        avg <- frame
        title <- str_c("Success collection for ", against[i])
        if(j == 1){      boxplot(avg,main=title,type="b", col=colrs[j],axes=FALSE,outline=FALSE,border=colrs[j],ylim=c(0,1))
                         axis(2)
                         axt <- names(frame)
                         axt <- sapply(axt,function(axt) str_replace(axt,"GSE",""))
                         axis(1,at=1:length(avg),labels=axt)
                         }else{
        boxplot(avg, col=colrs[j],ylim=c(0,1),axes=FALSE, add=TRUE,outline=FALSE,border=colrs[j])}
        
    }
  }
}

makeBoxPlot <- function(xList, measure,name, xName= ""){
  setwd(defDir)
  setwd("CM")
  for(i in 1:length(xList)){  
    
    puller <- pullAndCompileResults(xList[i],measure,dataName = name)
    if(i ==1){fram <- data.frame(matrix(ncol = length(xList),nrow=length(puller)))}
    if(!(length(puller) == length(fram[,1]))){
      diff <- length(fram[,1]) - length(puller)
      cat("\n",diff)
      filler <- rep_len(0,diff)
      puller <- append(puller,filler)
    }
    fram[,i] <- puller
    names(fram)[i] <- xList[i]
  }
  #cat("\n",fram)
  fram <- fram[sapply(fram,function(fram) !any(is.na(fram)))]
  #cat("\n",fram)
  boxplot(fram,main = measure,xlab=xName,ylab="Measure")
}