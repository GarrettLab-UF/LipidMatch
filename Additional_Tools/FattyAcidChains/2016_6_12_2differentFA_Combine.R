rm(list = ls())

if(!require(installr)) {
  install.packages("installr"); install.packages("stringr"); require(installr)}
library(installr)
if(check.for.updates.R()){
  updateR() 
  stop("You must update R to run LipidMatch, sorry for the inconvenience!")
}
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("compiler" %in% rownames(installed.packages()) == FALSE) {install.packages("compiler")}
if("gWidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("gWidgets")}
if("gWidgetstcltk" %in% rownames(installed.packages()) == FALSE) {install.packages("gWidgetstcltk")}
require(gWidgets)
require(gWidgetstcltk)
options(guiToolkit="tcltk") 
library(compiler)
library(sqldf)

FA1_directory <- choose.files(caption="fatty acid list 1 (csv)",multi=FALSE)
FA2_directory <- choose.files(caption="fatty acid list 2 (different from 1) (csv)",multi=FALSE)
Output_directory <- paste(choose.dir(caption="Output directory to save combinations of two fatty acids"),"\\",sep="")

FA1<-read.csv(FA1_directory,sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
FA2<-read.csv(FA2_directory,sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
FA1<-as.matrix(FA1)
FA2<-as.matrix(FA2)
Combination<-matrix(0,(nrow(FA1)*nrow(FA2)),4)
## for first two columns different
Row<-0
for (i in 1:nrow(FA1)){
  x<-0
  while (x<(nrow(FA2))){
    x<-x+1
    Row<-Row+1
    Combination[Row,1]<-FA1[i,1]
    Combination[Row,2]<-FA1[i,2]
    Combination[Row,3]<-FA2[x,1]
    Combination[Row,4]<-FA2[x,2]
  }
}
write.table(Combination, paste(Output_directory,"2diffFAcomb.csv",sep=""), sep=",",col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")