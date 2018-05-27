#Author: Nick Kroeger, Jeremy Koelmel
#Date: 06/12/2016

##remove all R objects from memory, this program can be memory intensive as it is dealing with huge datasets
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

#Mandatory Parameters
Original_dataFrame_file_directory <- choose.files(caption="Feature Table (csv) in which IDs will be appended",multi=FALSE)
ToBeAppended_dataFrame_file_directory <- choose.files(caption="Feature Table (csv) containing IDs to be appended",multi=FALSE)
Output_directory <- paste(choose.dir(caption="Output Directory to save Feature Tables"),"\\",sep="")

ID_Column_Original <- ginput(message="What is the column containing IDs in the feature table \n(the table IDs will be appended to) \nInput should be numeric", title="ID Column",icon="question")
ID_Column_Original<-as.numeric(ID_Column_Original)
MZ_Column_Original <- ginput(message="What is the column containing m/z ratios in the feature table \n(the table IDs will be appended to) \nInput should be numeric", title="m/z Column",icon="question")
MZ_Column_Original<-as.numeric(MZ_Column_Original)
RT_Column_Original <- ginput(message="What is the column containing retention times in the feature table \n(the table IDs will be appended to) \nInput should be numeric", title="Retention Time Column",icon="question")
RT_Column_Original<-as.numeric(RT_Column_Original)
Data_Start_Row_Original <- ginput(message="In what row does your first m/z and RT appear in the feature table \n(the table IDs will be appended to) \nInput should be numeric", title="Data Row",icon="question")
Data_Start_Row_Original<-as.numeric(Data_Start_Row_Original)
ID_Column_ToBeAppended <- ginput(message="What is the column containing Lipid IDs (or other information to be appended) in the feature table \n(the table containing information to append) \nInput should be numeric", title="ID Column",icon="question")
ID_Column_ToBeAppended<-as.numeric(ID_Column_ToBeAppended)
MZ_Column_ToBeAppended <- ginput(message="What is the column containing m/z ratios in the feature table \n(the table containing information to append) \nInput should be numeric", title="m/z Column",icon="question")
MZ_Column_ToBeAppended<-as.numeric(MZ_Column_ToBeAppended)
RT_Column_ToBeAppended <- ginput(message="What is the column containing retention times in the feature table \n(the table containing information to append) \nInput should be numeric", title="Retention Time Column",icon="question")
RT_Column_ToBeAppended<-as.numeric(RT_Column_ToBeAppended)
Data_Start_Row_ToBeAppended <- ginput(message="In what row does your first m/z and RT appear in the feature table \n(the table containing information to append) \nInput should be numeric", title="Data Row",icon="question")
Data_Start_Row_ToBeAppended<-as.numeric(Data_Start_Row_ToBeAppended)
ppm_Window <- ginput(message="What is the ppm window for matching m/z of features from the two files? \n(e.g. 10 => +/- 5ppm) \nInput should be numeric", title="ppm window",icon="question")
ppm_Window<-as.numeric(ppm_Window)
RT_Window <- ginput(message="What is the retention time window for matching features from the two files? \n(e.g. 0.3 => +/- 0.15 minutes) \nInput should be numeric", title="retention time window",icon="question")
RT_Window <-as.numeric(RT_Window)

ID_Method <- ginput(message="What do you want to name the appended data (column header) \nInput should be a word or phrase without spaces or special characters", title="New Column Name",icon="question")
ID_Method <- as.character(ID_Method)

output_file <- paste(ID_Method,"_Appended.csv",sep="")

#Code - DO NOT CHANGE
PPM_CONST <- (10^6 + ppm_Window/2) / 10^6



Original_df <- read.csv(Original_dataFrame_file_directory, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=TRUE, check.names = FALSE)
ToBeAppended_df <- read.csv(ToBeAppended_dataFrame_file_directory, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE, check.names = FALSE)
nrowOriginal <- nrow(Original_df)
nrowToBeAppended <- nrow(ToBeAppended_df)

Matched_IDs_vector <- vector(mode = "character", length = nrow(Original_df)) #initializes empty vector
Matched_IDs_vector[1] = paste("IDs_by_", ID_Method, sep="") #creates column name

#These 6 following lines were moved out of the loop to reduce processing time
ToBeAppended_df[, ID_Column_ToBeAppended] <- as.character(ToBeAppended_df[, ID_Column_ToBeAppended])
ToBeAppended_df[, MZ_Column_ToBeAppended] <- as.numeric(as.character(ToBeAppended_df[, MZ_Column_ToBeAppended]))
ToBeAppended_df[, RT_Column_ToBeAppended] <- as.numeric(as.character(ToBeAppended_df[, RT_Column_ToBeAppended]))

Original_df[, ID_Column_Original] <- as.character(Original_df[, ID_Column_Original])
Original_df[, MZ_Column_Original] <- as.numeric(as.character(Original_df[, MZ_Column_Original]))
Original_df[, RT_Column_Original] <- as.numeric(as.character(Original_df[, RT_Column_Original]))

for(o in Data_Start_Row_Original:nrowOriginal){
  IDs_Original <- Original_df[o, ID_Column_Original]
  MZ_Original <- Original_df[o, MZ_Column_Original]
  RT_Original <- Original_df[o, RT_Column_Original]
  for(tba in Data_Start_Row_ToBeAppended:nrowToBeAppended){
    IDs_ToBeAppended <- ToBeAppended_df[tba, ID_Column_ToBeAppended]
    MZ_ToBeAppended <- ToBeAppended_df[tba, MZ_Column_ToBeAppended]
    RT_ToBeAppended <- ToBeAppended_df[tba, RT_Column_ToBeAppended]      
    MZ_Conditional <- (MZ_ToBeAppended - abs(MZ_ToBeAppended - MZ_ToBeAppended*PPM_CONST) < MZ_Original) && (MZ_Original < (MZ_ToBeAppended + abs(MZ_ToBeAppended - MZ_ToBeAppended*PPM_CONST)))
    RT_Conditional <- ((RT_ToBeAppended - RT_Window/2) < RT_Original) && (RT_Original < (RT_ToBeAppended + RT_Window/2))
    if(MZ_Conditional && RT_Conditional){
      if(Matched_IDs_vector[o] == ""){
        Matched_IDs_vector[o] <- as.character(ToBeAppended_df[tba, ID_Column_ToBeAppended])
      } else{
        firstMatch <- Matched_IDs_vector[o]
        Matched_IDs_vector[o] <- paste(firstMatch, "|", as.character(ToBeAppended_df[tba, ID_Column_ToBeAppended]), sep="")
      }
    }   
  }
}

Original_df <- cbind(Original_df, Matched_IDs_vector)

write.table(Original_df, paste(Output_directory, output_file, sep=""), sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
