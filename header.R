library(WindR)
library(xlsx)
library(zoo)
library(xts)
library(purrr)
library(dplyr)
library(readxl)
library(lubridate)
library(PerformanceAnalytics)
library(randomForest)
library(rpart)
library(sandwich)
library(lmtest)
library(fUnitRoots)
library(rpart.plot)
library(mailR)
library(quantmod)
library(reshape2)
library(quantstrat)
library(tseries)
library(ggplot2)

mypath <- getwd()
upPath <- paste0(substr(mypath,1,3),"shares/Work/Project/QuantYou/")
toolPath <- paste0(upPath,"0000-00-01ToolBox")
#dataPath <- paste0(upPath,"0000-00-02Data")
sapply(paste0(toolPath,"/",dir(toolPath,pattern = ".R")),source)

mypath.R="./R/"
mypath.data="./data/"
if(length(dir(mypath.R,pattern = ".fun"))>0){
  sapply(paste0(mypath.R,dir(mypath.R,pattern = ".fun")),source)
}


loadRData("secName")

