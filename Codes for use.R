```{r setup, include = FALSE}
knitr::opts_chunk$set(echo=FALSE, include=TRUE, fig.align="center")
library("papaja")
library("foreign")
#library("Hmisc")
#library("memisc")
library("haven")
library("tidyr")
library("knitr")
library("plyr")
library("stats")
#library("gmodels")
library("nnet")
library("ggplot2")
library("reshape2")
library("flextable")
library("ReporteRs")
library("officer")
library("olsrr")
library("labelled")
library("broman")
library("xtable")
library("huxtable")

options(xtable.floating = FALSE)
r_refs(file = "r-references6.bib")
my_citation <- cite_r(file = "r-references6.bib")

data <- foreign::read.spss("Awesome data file.sav", to.data.frame = T, na.strings = "999")
data2 <- read_sav("Awesome data file.sav")


```
```{r functions, echo=FALSE}

rbind.all.columns <- function(inputx, inputy) {
  x.diff <- setdiff(colnames(inputx), colnames(inputy))
  y.diff <- setdiff(colnames(inputy), colnames(inputx))
  
  inputx[, c(as.character(y.diff))] <- NA
  inputy[, c(as.character(x.diff))] <- NA
  return(rbind(inputx, inputy))
}
create.SD <- function(input1, lable) {
  lable <- summary(input1, na.rm = TRUE)
  lable1 <- round(as.numeric(lable[4]), 1)
  lable2 <- round(sd(as.numeric(input1), na.rm = TRUE), 1)
  lable3 <- paste0(lable1, " ", "(", lable2, ")")
  return(lable3)
}
create.freq <- function(input1, lable) {
  lable <- plyr::count(na.omit(input1))
  lable2 <- round(lable[,2]/sum(lable[,2])*100, 1)
  lable3 <- paste0(lable[,2], " (", lable2, "%)")
  return(cbind(as.character(lable[,1]), lable3))
}
rbind.data.small <- function(inputx, inputy) {
  x.diff <- length(setdiff(colnames(inputx), colnames(inputy)))
  y.diff <- length(setdiff(colnames(inputy), colnames(inputx)))
  
  if(x.diff < 0){
    for (i in 1:x.diff){
      inputx <-cbind(inputx,NA)
    }
    colnames(inputx) <- colnames(inputy)
  }
  else{
    for(i in 1:y.diff){
      inputy <- cbind(inputy,NA)
    }
    colnames(inputy) <- colnames(inputx)
  }
  
  return(rbind(inputx, inputy))
}
mod.freq <- function(input1) {
  tempDF <- data.frame(table(input1))
  lable2 <- round(tempDF[,2]/sum(tempDF[,2])*100, 1)
  lable3 <- paste0(tempDF[,2], " (", lable2, "%)")
  return(cbind(as.character(tempDF[,1]), lable3))
}
rbind.test <- function(inputx, inputy) {
  x.diff <- length(setdiff(colnames(inputx), colnames(inputy)))
  y.diff <- length(setdiff(colnames(inputy), colnames(inputx)))
  
  if(x.diff > 0){
    for (i in 1:x.diff){
      inputy <-cbind(inputy,NA)
    }
    colnames(inputy) <- colnames(inputx)
  }
  else{
    for(i in 1:y.diff){
      inputx <- cbind(inputx,NA)
    }
    colnames(inputx) <- colnames(inputy)
  }
  
  return(rbind(inputx, inputy))
}
table.prep <- function (input1, input2, input3){
  input2.x <- data.frame(mod.freq(input1, input2))
  names(input2.x) <- c("codes", "lable3")
  input3.x <- as.character(input3[2])
  input2 <- cbind(input3.x, spread(input2.x, codes, lable3))
  return(input2)
}
mean.for.sum<- function(input1) {
  lable <- summary(as.numeric(input1, na.rm = TRUE))
  lable1 <- round(as.numeric(lable[4]), 1)
}

p.round <- function(x){
  if(x < .001) return(paste('<', '.001'))
  if(x > .250) return(paste('>', '.250'))
  paste('=', myround(x,3))
}

myttest <-function(x, y){
  round(t.test(x~y, conf.level = 0.95)$p.value, 3)
}
mychi <- function(x, y) {
  round(suppressWarnings(chisq.test(x, y)$p.value), 3)
}
```
```{r functions 2, echo= FALSE}
param_est <- function(char_vector){
  
  start = grep("Parameter Estimates",char_vector) + 2
  end = which("" == char_vector)
  for (x in end){
    if (x > start){
      end <- x - 2
      break
    }
  }
  
  info <- char_vector[start:end]
  info <- info[-2]
  info <- gsub("^\\s+|\\s+$", "", info)
  info <- gsub("\\s{2,9}", "\t", info)
  
  DF <- data.frame(do.call(rbind, strsplit(info, "\t", fixed=FALSE)), stringsAsFactors=FALSE)
  colnames(DF) <- as.character(DF[1,])
  DF <- DF[-1:-2,]
  DF <- data.frame(DF[,1], sapply(DF[,2:8], as.numeric))
  
  return (DF)
}
getData<-function(rows,refDF,present){
  
  ##Error checking to ensure that present is only 0 or 1
  ## any rows chosen, therefore, must be a binary variable
  if (present!=1 && present !=0){
    print("###Present much be either 0 or 1")
    return
  }
  
  ##Usage you must pass in a vector using the names of the columns of interest,
  ## these will be reported out as the row names, as well as the data.frame of interest
  # Example:  
  # rows<-c("na_13_BloodPressure","na_13_Cholesterol","na_13_Glucose","na_13_STI_HIV","na_13_Asthma",
  #              "na_13_HeartDisease_Stroke","na_13_Colon_Cancer","na_13_Prostate_Cancer", "na_13_Mammography",
  #              "na_13_Pap","na_13_MntlHealthCond")
  
  
  cols<-c("Overall","Sex","Sex")
  
  ##Initialize the data.frame to receive data
  newDF<-data.frame()
  
  ##For each heading listed in the rows vector...
  for(i in rows){
    ##For each number from 1 to the number of columns in cols
    ## check each condition to sort the data
    ## these conditions are specific and will need to be updated if the conditions change
    ## for example: if the age range changes, 45 will need to be updated
    for(j in 1:length(cols)){
      
      if (j==1){
        output<-nrow(refDF[which(refDF[,i]==present),])
      }
      else if (j==2){
        output<- c(output,nrow(refDF[which(refDF[,cols[j]]==1 & refDF[,i]==present),]))
      }
      else if (j==3){
        output<- c(output,nrow(refDF[which(refDF[,cols[j]]==2 & refDF[,i]==present),]))
      }
      
    }
    ##One row has been completed, save the information to a data.frame
    newDF<- rbind(newDF,output)
  }
  
  ##Set the column and row names of the data.frame
  colnames(newDF)<-cols
  rownames(newDF)<-rows
  
  ##Return the data.frame from this function
  newDF
}
GetNBdata <- function(list, rows, refDF){
  
  #Parameters:
  ## list: a list of data.frame columns to be processed
  ##    Example: list(data2$Sexual_ID,data2$Monthly_Income)
  ## rows: a vector of column names to assist in processing,
  ##    it must match which, and the order of, columns used to make the list
  ##    Example: if list is as described above, then rows must be
  ##        c("Sexual_ID","Monthly_Income")
  ## refDF: the data.frame used to get the data
  ##    Example: if the list is made using data2$Sexual_ID, then the refDF will be data2
  
  ## This function is expecting these columns to be of class labelled,
  ##    so the labelled library needs to be imported
  #library(labelled)
  
  ## Set the columns of refDF that are of interest
  cols<-c("Overall","Sex","Sex")
  
  ##Initialize the data.frame and rNames to receive data
  newDF<-data.frame()
  rNames<-c()
  
  ## Iterate over the number of items contained in the list
  for(i in 1:length(list)){
    
    # Each item in the list is of class labelled, so it needs to
    #   be converted to a factor level variable.
    #   Here, we store the number values to identify particular rows 
    #   as well as the labels to label the rows later.
    vals<-levels(to_factor(list[[i]], levels="values"))
    names<-levels(to_factor(list[[i]], levels="labels"))
    
    # Iterate over the number of levels
    for (k in 1:length(vals)){
      #Find the information below in each of the columns and store it
      #  to a temp variable to build the data.frame
      for(j in 1:length(cols)){
        
        if (j==1){
          output<-nrow(refDF[which(refDF[,rows[i]]==as.numeric(vals[k])),])
        }
        else if (j==2){
          # Coding for Males
          output<- c(output,nrow(refDF[which(refDF[,cols[j]]==1 & refDF[,rows[i]]==as.numeric(vals[k])),]))
        }else if (j==3){
          # Coding for Females
          output<- c(output,nrow(refDF[which(refDF[,cols[j]]==2 & refDF[,rows[i]]==as.numeric(vals[k])),]))
        }
        
      }
      ##One row has been completed, save the information to a data.frame
      newDF<- rbind(newDF,output)
    }
    #Build the rownames based on the factor labels for the current section
    rNames<-c(rNames,names)
    
    
  }
  
  ##Set the column and row names of the data.frame
  colnames(newDF)<-cols
  rownames(newDF)<-rNames
  
  ##Return the data.frame from this function
  newDF
}
dataNBPercent<-function(inputDF, refDF){
  
  ##Requires data.frame obtained from getData(), and the original data.frame
  ## passed to getData()
  
  ##inputDF format: Overall, Condition 1 (grouped over 2 cols),
  ##  Condition 2 (grouped over 2 cols)...etc
  
  cols<-c("Overall","Sex","Sex")
  
  
  ##Calculate the totals for each category from the original data
  totalN<-nrow(refDF[,1])
  totalMale<-nrow(refDF[which(refDF[,cols[2]]==1),])
  totalFemale<-nrow(refDF[which(refDF[,cols[3]]==2),])
  
  ##Initialize the output data.frame
  outputDF<-data.frame()
  
  ##Use string operations to glue the count data with the percentage and '%'
  overall<-paste(inputDF[,1]," (",round(inputDF[,1]/totalN*100,1),"%)", sep="")
  male<-paste(inputDF[,2]," (",round(inputDF[,2]/totalMale*100,1),"%)", sep="")
  female<-paste(inputDF[,3]," (",round(inputDF[,3]/totalFemale*100,1),"%)", sep = "")
  
  #Build the data.frame from the individual columns created above
  outputDF<-cbind(overall, male, female)
  
  #Name the rows in the data.frame
  rownames(outputDF)<-rownames(inputDF)
  
  #Return the data.frame
  outputDF
}
dataPercent<-function(inputDF, refDF){
  
  ##Requires data.frame obtained from getData(), and the original data.frame
  ## passed to getData()
  
  ##inputDF format: Overall, Condition 1 (grouped over 2 cols),
  ##  Condition 2 (grouped over 2 cols)...etc
  
  cols<-c("Overall","Sex","Sex")
  
  
  ##Calculate the totals for each category from the original data
  totalN<-nrow(refDF[,1])
  totalMale<-nrow(refDF[which(refDF[,cols[2]]==1),])
  totalFemale<-nrow(refDF[which(refDF[,cols[3]]==2),])
  
  ##Initialize the output data.frame
  outputDF<-data.frame()
  
  ##Use string operations to glue the count data with the percentage and '%'
  overall<-paste(inputDF[,1]," (",round(inputDF[,1]/totalN*100,1),"%)", sep="")
  male<-paste(inputDF[,2]," (",round(inputDF[,2]/totalMale*100,1),"%)", sep="")
  female<-paste(inputDF[,3]," (",round(inputDF[,3]/totalFemale*100,1),"%)", sep = "")
  
  #Build the data.frame from the individual columns created above
  outputDF<-cbind(overall, male, female)
  
  #Name the rows in the data.frame
  rownames(outputDF)<-rownames(inputDF)
  
  #Return the data.frame
  outputDF
}
naAttachStars<-function(inputDF, starDF){
  
  ##InputDF is the percentage DF, starDF is the converted ChiSq p.values into asterisks
  ## p.values are in pairs of columns starting at col 2
  
  ##String operations to glue the previously obtained count (num%) with
  ## the significance level stars
  ## changing this slightly, only want the stars on the second item, not both
  overall<-paste(inputDF[,1], sep="")
  #male<-paste(inputDF[,2],starDF[,2], sep="")
  male<-paste(inputDF[,2], sep="")
  female<-paste(inputDF[,3],starDF[,2], sep="")
  
  ##Build the data.frame by column binding the vectors created above
  outputDF<-cbind(overall, male, female)
  
  ##Set the row names
  rownames(outputDF)<-rownames(inputDF)
  
  ##Return the data.frame
  outputDF
}
naChiSq<-function(inputDF, rows){
  
  ##Input the original data.frame and the list of columns.
  ## This then performs the chisq.test between the sub-categories age, gender, etc.
  
  ##Initialize the storage vectors
  gender<-c()
  
  ##For each name in rows...
  for(i in rows){
    ##Calculate the p.value for various chisq.tests
    ## ensure that the first parameter is entered as a binary term
    ## For example: inputDF$na_3_Age < 45 will return TRUE or FALSE
    
    gender<-c(gender,round(chisq.test(table(inputDF$na_2_Gender,as.data.frame(inputDF[,i])[,1]))$p.value,3))
  }
  
  ##Build the data.frame by binding each of the vectors created above as a column
  output<-cbind(age,gender,insured,attend)
  
  ##Set the row names
  rownames(output)<-rows
  
  ##Return the data.frame
  output
  
}
naGetStars<-function(inputDF){
  
  ##Input the p.values obtained from naChiSq to get a data.frame of
  ## asterisks correspdonding to the level of significance.
  ## 0.05  = *
  ## 0.01  = **
  ## 0.001 = ***
  
  numCols<-ncol(inputDF)
  
  ifelse(inputDF[,1:numCols]<=0.05,ifelse(inputDF[,1:numCols]<=0.01,ifelse(inputDF[,1:numCols]<=0.001,'***','**'),'*'),'')
}
Getmeansd<- function(rows,refDF){
  ## Example: contin.  variables 
  ## rows<- c("Age", "Member_Length", "relig_sum")
  
  cols<-c("Overall","Sex","Sex")
  
  ##Initialize the data.frame to receive data
  newDF1<-data.frame()
  
  ##For each heading listed in the rows vector...
  for(i in rows){
    
    for(j in 1:length(cols)){
      
      if (j==1){
        output<- c(paste(round(mean(refDF[i][[1]], na.rm = TRUE),1), " (", round(sd(refDF[i][[1]],na.rm = TRUE) ,1), ")", sep = ""))
      }
      else if (j==2){
        output<- c(output,paste(round(mean(refDF[i][[1]][which(refDF[cols[j]]==1)], na.rm = TRUE),1), " (", round(sd(refDF[i][[1]][which(refDF[cols[j]]==1)],na.rm = TRUE) ,1), ")", sep = ""))
      }
      else if (j==3){
        output<- c(output,paste(round(mean(refDF[i][[1]][which(refDF[cols[j]]==2)], na.rm = TRUE),1), " (", round(sd(refDF[i][[1]][which(refDF[cols[j]]==2)],na.rm = TRUE) ,1), ")", sep = ""))
      }
      
    }
    ##One row has been completed, save the information to a data.frame
    newDF1<- rbind(newDF1, output, stringsAsFactors= FALSE)
  }
  ##Set the column and row names of the data.frame
  colnames(newDF1)<-cols
  rownames(newDF1)<-rows
  
  ##Return the data.frame from this function
  newDF1
}
```