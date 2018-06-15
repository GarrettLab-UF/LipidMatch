##Jeremy Koelmel  jeremykoelmel@gmail.com 
##Nick Kroeger    nkroeger.cs@gmail.com

#READ ME#
#Rules for inputs:

#Feature Table-
#1 All peak heights/areas are adjacent to one another
#Folder Structure-
#1 Only one Feature Table per mode per folder 
#2 Make sure you have 'neg' or'n' at the end of a negative mode file, and 'pos' or 'p' at the end of a positive mode file.
#3 If you have AIF data, the .ms1 and .ms2 files must have the same file name
# Example:
# InputFolder
# --Shrimp
# ----shrimpFeatureTableNeg.csv
# ----featureTableshrimpPos.csv
# ----Shrimp14_AIFp.ms1
# ----Shrimp14_AIFp.ms2
# ----Shrimp14_ddMSpos.ms2
# ----Shrimp14_ddMSNeg.ms2
# ----.
# ----.
# ----.
# --Cricket
# ----featureTableCricketNeg.csv
# ----featureTableCricketPos.csv
# ----Cricket2_AIFn.ms1
# ----Cricket2_AIFn.ms2
# ----Cricket14_ddMSpos.ms2
# ----Cricket22_ddMSNeg.ms2
# ----.
# ----.
# ----.
#4 Do not worry about creating an output folder. I will auto create one.
#5 Only have .ms2 and .csv file types in the subfolders. (or .ms1 in addition if you have AIF mode data)

rm(list = ls()) #remove all R objects from memory, this program can be memory intensive as it is dealing with huge datasets



#### Mandatory Parameter to Change ####
#If you want to manually input your variables... 
# 1. Set ManuallyInputVariables <- TRUE (all caps)
# 2. Assign variables under the next if statement, "if (ManuallyInputVariables==TRUE)"
csvInput <- FALSE
ManuallyInputVariables <- FALSE

#### END Mandatory Parameter to Change #### 
#### END "Read Me" Section ####


#Checks for updates, installs packagaes: "installr" "stringr" "sqldf" "gWidgets" "gWidgetstcltk" and "compiler"
if(!require(installr)) {
  install.packages("installr"); install.packages("stringr"); require(installr)}
library(installr)

if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
# if("compiler" %in% rownames(installed.packages()) == FALSE) {install.packages("compiler")}
if("gWidgets" %in% rownames(installed.packages()) == FALSE) {install.packages("gWidgets")}
if("gWidgetstcltk" %in% rownames(installed.packages()) == FALSE) {install.packages("gWidgetstcltk")}
require(gWidgets)
require(gWidgetstcltk)
options(guiToolkit="tcltk") 
library(compiler)
library(sqldf)
options(warn=-1)#suppress warning on

errorBox <- function(message) {
  window <- gwindow("Confirm")
  group <- ggroup(container = window)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE, icon="error")
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("ok", handler=function(h,...) dispose(window), container=button.group)
  return()
}

if (ManuallyInputVariables==TRUE){
  
  #Retention Time plus or minus
  RT_Window <- .2 #window of .2 => +/- .1
  
  #parts-per-million window for matching the m/z of fragments obtained in the library to those in experimentally obtained
  ppm_Window <- 10 #window of 10 => +/- 5 ppm error
  
  #Tolerance for mass-to-charge matching at ms1 level (Window)
  PrecursorMassAccuracy<-0.005
  
  #Plus minus range for the mass given after targeting parent ions portrayed in excalibur to match exact mass of Lipid in library
  SelectionAccuracy<-0.4
  
  #Threshold for determining that the average signal intensity for a given MS/MS ion should be used for confirmation
  intensityCutOff<-1000
  
  #Feature Table information
  CommentColumn <- 1
  MZColumn <- 2
  RTColumn <- 3
  RowStartForFeatureTableData <- 2 #Look at your Feature Table (.csv)...What row do you first see numbers?
  
  
  #ddMS data? set this parameter. If not? Leave it.
  #The minimum number of scans required for the result to be a confirmation
  ScanCutOff<-1
  
  #Have AIF data? set these parameters. If not? Leave them.
  corrMin <-.6
  minNumberOfAIFScans <- 5
  
  # Input Directory for Feature Table and MS2 files ...must have \\ (double backslash) at the end of the directory
  InputDirectory<-"C:\\Users\\Nick Kroeger\\Code\\R\\Jeremy\\AIFEnhanced\\TestRuns\\Shrimp\\"
  
  # Input Directory for Libraries ...must have \\ (double backslash) at the end of the directory
  InputLibrary<-"C:\\Users\\Nick Kroeger\\Code\\R\\Jeremy\\LibrariesReducedAdducts\\"
  
  #Check your parameters and see that they are all correct.
  #Then press ctrl+shift+s to execute all code.
  ####################### END MANUALLY INPUTTING VARIABLES SECTION #######################
  
}else if(csvInput == TRUE){
  ####################### Get input from csv VARIABLES SECTION ###############################
  #parametersDirectory
parametersDir <- "C:/Users/Jeremy/Desktop/Desktop/Instrumentation/Software/MSms/LipidMatch_Workflow/LipidMatch_Flow_Versions/LipidMatch_Flow_Portable/LipidMatch_Flow/LipidMatch_Distribution/"
parametersFile <- paste(parametersDir, "LIPIDMATCH_PARAMETERS_Agilent_QTOF_6530.csv", sep="")
  parametersInput_csv <- read.csv(parametersFile, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
  parametersInput_csv <- as.matrix(parametersInput_csv)
  ErrorOutput<-0
  #Retention Time plus or minus
  RT_Window <- as.numeric(parametersInput_csv[2,2]) #window of .2 => +/- .1
  
  #parts-per-million window for matching the m/z of fragments obtained in the library to those in experimentally obtained
  ppm_Window <- as.numeric(parametersInput_csv[4,2]) #window of 10 => +/- 5 ppm error
  
  #Tolerance for mass-to-charge matching at ms1 level (Window)
  PrecursorMassAccuracy <- as.numeric(parametersInput_csv[3,2])
  
  #Plus minus range for the mass given after targeting parent ions portrayed in excalibur to match exact mass of Lipid in library
  SelectionAccuracy <- as.numeric(parametersInput_csv[5,2])
  
  #Threshold for determining that the average signal intensity for a given MS/MS ion should be used for confirmation
  intensityCutOff <- as.numeric(parametersInput_csv[7,2])
  
  #Feature Table information
  CommentColumn <- as.numeric(parametersInput_csv[10,2])
  MZColumn <- as.numeric(parametersInput_csv[11,2])
  RTColumn <- as.numeric(parametersInput_csv[12,2])
  RowStartForFeatureTableData <- as.numeric(parametersInput_csv[13,2]) #Look at your Feature Table (.csv)...What row do you first see numbers?
  
  #ddMS data? set this parameter. If not? Leave it.
  #The minimum number of scans required for the result to be a confirmation
  ScanCutOff<-as.numeric(parametersInput_csv[6,2])
  
  #Have AIF data? set these parameters. If not? Leave them.
  corrMin <-as.numeric(parametersInput_csv[9,2])
  minNumberOfAIFScans <- as.numeric(parametersInput_csv[8,2])
  
  # Input Directory for Feature Table and MS2 files ...must have \\ (double backslash) at the end of the directory
  InputDirectory<-as.character(parametersInput_csv[14,2])
  
  # Input Directory for Libraries ...must have \\ (double backslash) at the end of the directory
  InputLibrary<-as.character(parametersInput_csv[15,2])
}else{
  ####################### Pop-up boxes input VARIABLES SECTION ###############################
  
  # check if R version is equal to, or between, version 2.0.3 and 3.3.3, otherwise present pop-up box warning 
  if(!((as.numeric(paste(version$major,version$minor,sep=""))>=20.3) && (as.numeric(paste(version$major,version$minor,sep=""))<=33.3))) {
    errorBox(message=paste("ERROR: R version must be equal to, or between, 2.0.3 and 3.3.3. Please download 3.3.3. You are using version: ", paste(version$major,version$minor,sep=".")))
    stop(paste("R version must be equal to, or between, 2.0.3 and 3.3.3. Please download 3.3.3. You are using version: ", paste(version$major,version$minor,sep=".")))
  }
  
  ## Input Directory that holds each folder of .ms2 files & feature table (contains features, peak heights/areas, etc.) (.csv file)
  InputDirectory<-paste(choose.dir(caption="Input Directory of MS2 + Feature Tables"),"\\",sep="")
  if(InputDirectory=="NA\\"){
    stop()
  }
  foldersToRun <- list.dirs(path=InputDirectory, full.names=FALSE, recursive=FALSE)
  ErrorOutput<-0
  for (i in seq_len(length(foldersToRun))){
    if(foldersToRun[i] == "Output"){
      ErrorOutput<-1
      errorBox(message=paste("ERROR: Remove your 'Output' folder\nfrom the current Input Directory:\n", InputDirectory))
      stop("Warning: Remove your 'Output' folder from the current Input Directory: ", InputDirectory)
    }
  }
  
  ## Input Directory for Libraries
  InputLibrary<-paste(choose.dir(caption="Input Directory of libraries (came with LipidMatch)"),"\\",sep="")
  if(InputLibrary=="NA\\"){
    stop()
  }
  
  GetInputAndErrorHandle <- function(typeOfVariable, message, title){
    isValidInput <- FALSE
    inputVariable <- ginput(message=message, title=title,icon="question")
    
    while(!isValidInput){
      ##Retention Time plus or minus
      if(inputVariable == "d" || inputVariable == "D"){
        if(typeOfVariable=="RT"){ inputVariable <- 0.3
        }else if(typeOfVariable=="ppm"){ inputVariable <- 10
        }else if(typeOfVariable=="precMassAccuracy"){ inputVariable <- 0.01
        }else if(typeOfVariable=="selectionAccuracy"){ inputVariable <- 1
        }else if(typeOfVariable=="maxInt"){ inputVariable <- 1000
        }else if(typeOfVariable=="scanCutOff"){ inputVariable <- 1 
        }else if(typeOfVariable=="FeatRT"){inputVariable<-0.1
        }else if(typeOfVariable=="FeatMZ"){inputVariable<-0.006
        }else if(typeOfVariable=="corrMin"){inputVariable<-0.6
        }else if(typeOfVariable=="minAIFScans"){inputVariable<-5}
        isValidInput <- TRUE
      }else if(suppressWarnings(!is.na(as.numeric(inputVariable)))){
        inputVariable <- as.numeric(inputVariable)
        isValidInput <- TRUE
      }else{
        inputVariable <- ginput(message=paste("Error! Invalid Input.\n\n",message), title=title, icon="error")
      }
    }
    return(inputVariable)
  }
  
  RT_Window <- GetInputAndErrorHandle("RT", message="Retention Time Window\n(Window of .3 => +/- .15)\nOr type \"d\" for default: .3", title="RT_Window")
  ppm_Window <- GetInputAndErrorHandle("ppm", message="Parts-per-million window for matching experimental and in-silico fragments m/z \n(Window of 10 => +/- 5)\nOr type \"d\" for default value: 10", title="ppm_Window")
  PrecursorMassAccuracy <- GetInputAndErrorHandle("precMassAccuracy", message="Mass accuracy window for matching experimental and in-silico precursors m.z\nfor full scan (precursor) mass matching \n(Window of .01 Da => +/- .005 Da)\nOr type \"d\" for default: .01", title="PrecMassAccuracyWindow")
  SelectionAccuracy <- GetInputAndErrorHandle("selectionAccuracy", message="MS/MS Isolation Window (Da) \n(For determining MS/MS scans for each feature)\nOr type \"d\" for default: 1", title="SelectionAccuracy")
  intensityCutOff <- GetInputAndErrorHandle("maxInt", message="Threshold for determining what the minimum signal intensity cut off for a\ngiven MS/MS ion should be (used for confirmations)\nOr type \"d\" for default: 1000", title="intensityCutOff")
  CommentColumn <- GetInputAndErrorHandle("CommentCol", message= "Feature Table Info: Comment Column/Row ID \n(look at feature table .csv, first column is 1)\nNote that the column should be the same in negative and positive feature tables \nNo default value.",title="CommentColumn")
  MZColumn <- GetInputAndErrorHandle("MZCol", message= "Feature Table Info: Mass-to-Charge Column \n(look at feature table .csv, first column is 1)\nNote that the column should be the same in negative and positive feature tables \nNo default value.", title="MZColumn")
  RTColumn <- GetInputAndErrorHandle("RTCol", message= "Feature Table Info: Retention Time Column \n(look at feature table .csv, first column is 1)\nNote that the column should be the same in negative and positive feature tables \nNo default value.", title="RTColumn")
  RowStartForFeatureTableData <- GetInputAndErrorHandle("RowStart", message= "Feature Table Info: What row does your numeric data start? \n(First row of .csv starts counting at 1)\nNote that the row should be the same in negative and positive feature tables \nNo default value.", title="RowStartForFeatureTableData")
  
  hasAIF <- FALSE
  hasdd <- FALSE
  
  foldersToRun <- list.dirs(path=InputDirectory, full.names=FALSE, recursive=FALSE)
  if(length(foldersToRun)==0){
    lengthFoldersToRun <- 1 #if there are no subfolders, that means you have the faeture table and ms2s in that current directory, therefore, run analysis on those files.
  }else{
    lengthFoldersToRun <- length(foldersToRun)#run analysis on all subfolders
  }
  #checks for output folder, we don't want to overwrite your files
  for (i in seq_len(lengthFoldersToRun)){
    numOfAIFFiles <- vector()
    if(length(foldersToRun)==0){#we're in current (and only) folder that contains feature table and ms2 
      fpath <- InputDirectory
    }else if(foldersToRun[i] == "Output"){
      fpath <- InputDirectory
      print(paste("Warning: Remove your 'Output' folder from the current Input Directory:", InputDirectory))
    }else{
      fpath <- paste(InputDirectory, foldersToRun[i], "\\", sep="")
    }
    
    #separate ddMS and AIF
    numOfddFiles <- length(list.files(path=fpath, pattern="[dD][dD]", ignore.case=FALSE))
    numOfAIFFiles <- length(list.files(path=fpath, pattern="[AIFaif][AIFaif][AIFaif]", ignore.case=FALSE))
    if(numOfAIFFiles > 0){ #if # of AIF .ms2 or .ms1 > 0, ask for variable input
      hasAIF <- TRUE
    }else{
      print(paste("Warning: Couldn't find .ms1 or .ms2 files. Make sure you have 'aif' in your .ms1 or .ms2 file name (for data independent analysis)."))
    }
    if(numOfddFiles > 0){
      hasdd <- TRUE
    }else{
      print(paste("Warning: Couldn't find data dependent .ms2 files. Make sure you have 'dd' in your .ms2 file name (for data dependent analysis)."))
    }
  }
  if(hasAIF){
    corrMin <- GetInputAndErrorHandle("corrMin", message= "Minimum adjusted R2 value for AIF confirmation\n(Used in correlating fragment ms1 and ms2 intensities).\nOr type \"d\" for default: 0.6", title="corrMin")
    minNumberOfAIFScans <- GetInputAndErrorHandle("minAIFScans", message= "Minimum number of scans for confirming fragments (AIF)\nOr type \"d\" for default: 5", title="minNumberOfAIFScans")
  }
  if(hasdd){#then has ddMS
    ScanCutOff <- GetInputAndErrorHandle("scanCutOff", message="Minimum number of scans required for the result to be a confirmation (ddMS)\nOr type \"d\" for default: 1", title="ScanCutOff")
  }
  
} ####################### END AUTO INPUTTING VARIABLES SECTION ####################
#Error handling for manual input section... specifically the Input Directory. Checks for Output folder, and stops.
if(ManuallyInputVariables==TRUE || csvInput == TRUE){
  #checks for output folder, we don't want to overwrite your files
  foldersToRun <- list.dirs(path=InputDirectory, full.names=FALSE, recursive=FALSE)
  if(length(foldersToRun)==0){
    lengthFoldersToRun <- 1 #if there are no subfolders, that means you have the faeture table and ms2s in that current directory, therefore, run analysis on those files.
  }else{
    lengthFoldersToRun <- length(foldersToRun)#run analysis on all subfolders
  }
  for (i in seq_len(length(foldersToRun))){
    if(foldersToRun[i] == "Output"){
      errorBox(message=paste("Warning: Remove your 'Output' folder\nfrom the current Input Directory:\n", InputDirectory))
      stop("Warning: Remove your 'Output' folder from the current Input Directory: ", InputDirectory)
    }
    if(length(foldersToRun)==0){#we're in current (and only) folder that contains feature table and ms2 
      fpath <- InputDirectory
    }else if(foldersToRun[i] == "Output"){
      fpath <- InputDirectory
      print(paste("Warning: Remove your 'Output' folder from the current Input Directory:", InputDirectory))
    }else{
      fpath <- paste(InputDirectory, foldersToRun[i], "\\", sep="")
    }
    numOfAIFFiles <- length(list.files(path=fpath, pattern="[AIFaif][AIFaif][AIFaif]", ignore.case=FALSE))
    if(numOfAIFFiles %% 2 == 1){ #if # of AIF .ms2 or .ms1 are odd, STOP!
      stop("Warning: You should have a one-to-one relationship between AIF.ms1 and AIF.ms2 files...We found ", numOfAIFFiles," AIF .ms1 and/or .ms2 files. You should have an even number of AIF files.",sep="")
    }#else, do nothing.
  }
}
#### END VARIABLE DECLARATIONS SECTION ####


#### CODE - DO NOT TOUCH ####

#Library Information
# NAME AND DIRECTORY for exact mass library
ImportLibPOS<-paste(InputLibrary, "Precursor_Library_POS.csv", sep="")
ImportLibNEG<-paste(InputLibrary, "Precursor_Library_NEG.csv", sep="")
LibCriteria<- paste(InputLibrary, "LIPID_ID_CRITERIA.csv", sep="")
LibColInfo<-1  #Look at your Library. What column are your IDs? (Columns to retrieve ID information and align with data)
ParentMZcol_in<-2 #Look at your Library. What column are your precursor masses in? (Columns to retrieve ID information and align with data)
ddMS2_Code<-"1"
AIF_Code<-"2"
Class_Code<-"3"
ExactMass_Code<-"4"
NoID_Code<-"5"
PrecursorMassAccuracy<-PrecursorMassAccuracy/2

PPM_CONST <- (10^6 + ppm_Window/2) / 10^6

################### FUNCTIONS ###################
RunAIF <- function(ms1_df, ms2_df, FeatureList, LibraryLipid_self, ParentMZcol, OutputDirectory, ExtraFileNameInfo, ConfirmORcol, ConfirmANDcol, OutputInfo){
  tStart<-Sys.time()
  ColCorrOR <- ConfirmORcol
  ColCorrAND <- ConfirmANDcol
  
  if(!dir.exists(paste(OutputDirectory,"//Confirmed_Lipids",sep=""))){
    dir.create(paste(OutputDirectory,"//Confirmed_Lipids",sep=""))
  }
  if(!dir.exists(paste(OutputDirectory,"//Additional_Files",sep=""))){
    dir.create(paste(OutputDirectory,"//Additional_Files",sep=""))
  }
  
  LibraryLipid<-read.csv(LibraryLipid_self, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
  libraryLipidMZColumn <- ParentMZcol
  FeatureListMZColumn <- 1
  
  ## Create a Library of Lipids and fragments for those masses in Feature List
  FeatureListHeader <- FeatureList[1,]
  LibraryLipidHeader <- LibraryLipid[1,]
  NewLibraryLipidHeader <- cbind(LibraryLipidHeader, FeatureListHeader)
  
  sqlMerged <- sqldf(paste("select * from LibraryLipid lib inner join FeatureList iList on lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "-", PrecursorMassAccuracy, "<= iList.", colnames(FeatureList)[FeatureListMZColumn], " and iList.", colnames(FeatureList)[FeatureListMZColumn], " <= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "+", PrecursorMassAccuracy, sep = ""))
  # sqlMerged <- sqldf(paste("select * from LibraryLipid lib inner join FeatureList iList on iList.", colnames(FeatureList)[FeatureListMZColumn], " >= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], " -.005 and iList.", colnames(FeatureList)[FeatureListMZColumn], " <= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "+ .005", sep = ""))
  NewLibraryLipid <- as.matrix(sqlMerged)
  NewLibraryLipid <- rbind(NewLibraryLipidHeader,NewLibraryLipid)
  NewLibraryLipid <- NewLibraryLipid[,-(ncol(LibraryLipid)+1)] #removes mz from Feature list
  
  #Removes 1 rt col and creates RT_min and RT_max cols based on RT_Window
  RT_Col <- NewLibraryLipid[2:nrow(NewLibraryLipid),ncol(NewLibraryLipid)-1]
  NewLibraryLipid <- NewLibraryLipid[,-(ncol(NewLibraryLipid)-1)]
  RT_Window_Vector <- rep(RT_Window/2, each=nrow(NewLibraryLipid)-1)
  RT_min <- as.numeric(levels(RT_Col))[RT_Col] - as.numeric(RT_Window_Vector)
  RT_max <- as.numeric(levels(RT_Col))[RT_Col] + as.numeric(RT_Window_Vector)
  RT_min <- c("RT_min",RT_min)
  RT_max <- c("RT_max",RT_max)
  Comment <- NewLibraryLipid[,ncol(NewLibraryLipid)]
  NewLibraryLipid <- cbind(NewLibraryLipid[,1:(ncol(NewLibraryLipid)-1)], RT_min, RT_max, Comment)
  colnames(NewLibraryLipid) <- as.character(as.matrix(NewLibraryLipid[1,]))
  NewLibraryLipid <- NewLibraryLipid[-1,]
  NewLibraryLipid <- NewLibraryLipid[!is.na(NewLibraryLipid[,1]),!is.na(NewLibraryLipid[1,])]
  
  startRTCol <- ncol(NewLibraryLipid)-2
  endRTCol <- startRTCol + 1
  LibParentHits_df <- NewLibraryLipid #name change
  nrowLibParentHits <- nrow(LibParentHits_df)
  ncolLibParentHits <- ncol(LibParentHits_df)
  NoMatches_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_NoIDs.csv",sep="")
  if(nrowLibParentHits == 0){#No hits between Feature List and Library
    write.table("No Matches Found between Library and Feature List", NoMatches_dir, col.names=FALSE, row.names=FALSE, quote=FALSE)
  }else{#If there was at least one match between Feature List and the Library
    #Output dataframes. Used to build the All Confirmed
    ConfirmedFragments_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    RTMaxIntensity_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    MaxIntensity_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    NumOfScans_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    AverageMZ_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    
    outputHeader <- colnames(LibParentHits_df) #get header from LibParentHits_df
    colnames(ConfirmedFragments_df) <- outputHeader
    colnames(RTMaxIntensity_df) <- outputHeader
    colnames(MaxIntensity_df) <- outputHeader
    colnames(NumOfScans_df) <- outputHeader
    colnames(AverageMZ_df) <- outputHeader
    
    outputLibData <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    ConfirmedFragments_df[,c(1,startRTCol:ncolLibParentHits)] <- outputLibData
    RTMaxIntensity_df[,c(1,startRTCol:ncolLibParentHits)] <- outputLibData
    MaxIntensity_df[,c(1,startRTCol:ncolLibParentHits)] <- outputLibData
    NumOfScans_df[,c(1,startRTCol:ncolLibParentHits)] <- outputLibData
    AverageMZ_df[,c(1,startRTCol:ncolLibParentHits)] <- outputLibData
    
    ########################################################################################
    # loop through lib parent hits M+H column, see                                         #
    #   1) if the precursor(from ms2) falls within this targetAcccuracy mz window          #
    #     2) if so... does this ms2 RT fall within the start and end RT from LibParentHits?#
    #     3) if mz frag from ms2 falls within ppm of LibParentHits                         #
    #   Then fragment is confirmed.                                                        #  
    #                                                                                      #
    #   ms2_df[[1, 3]][1, 1]                                                               #  
    #         ^matrix ^[row, column] from matrix                                           #
    ########################################################################################
    
    AIFms2SubsettedRTList <- list()
    for(p in seq_len(nrowLibParentHits)){
      RTConditional <- as.numeric(as.character(LibParentHits_df[p,startRTCol])) <= as.numeric(ms2_df[,2]) & as.numeric(ms2_df[,2]) <= as.numeric(as.character(LibParentHits_df[p,endRTCol]))
      AIFms2SubsettedRTList[[p]] <- subset(ms2_df, RTConditional)
    }
    AIFms1SubsettedRTList <- list()
    for(p in seq_len(nrowLibParentHits)){
      RTConditional <- as.numeric(as.character(LibParentHits_df[p,startRTCol])) <= as.numeric(ms1_df[,2]) & as.numeric(ms1_df[,2]) <= as.numeric(as.character(LibParentHits_df[p,endRTCol]))
      AIFms1SubsettedRTList[[p]] <- subset(ms1_df, RTConditional)
    }
    
    ############################################################################
    #Building All_Confirmed data frame for output (ddMS and/or AIF)            #
    # Creates a list of dataframes for each section of the All_Confirmed.csv.  #
    #   Why? To organize our data.                                             # 
    # Each list's element corresponds to a row from LibParentHits              #
    # Each list's element holds a fragment found in the scans                  #  
    ############################################################################
    #building ALL_Confirmed for AIF
    for(c in ParentMZcol:((startRTCol - ParentMZcol)+1)){ #loop all fragment columns
      subsettedFragList <- list() #has nrow(LibParentHits_df) elements
      for(p in 1:nrowLibParentHits){ #loop through all AIFms2SubsettedRTList elements
        # if(nrow(AIFms2SubsettedRTList[[p]]) != 0){ #don't loop if the list is empty! (for loops run once when the size is 0... :(...)
        subsettedFrag_df <- data.frame(scans=numeric(), rt=numeric(), frags=numeric(), intensity=numeric())
        scans <- vector()
        rt <- vector()
        frags <- vector()
        intensity <- vector()
        for(d in seq_len(nrow(AIFms2SubsettedRTList[[p]]))){ #loop through each element in the list
          subsettedFrag <- vector(length=0) 
          #Take the theoretical LibParentHits fragment m/z and look within a ppm window through the scan's ms2 fragments
          fragConditional <- as.numeric(as.character(LibParentHits_df[p, c])) - abs(as.numeric(as.character(LibParentHits_df[p, c])) - as.numeric(as.character(LibParentHits_df[p, c]))*PPM_CONST) <= as.numeric(AIFms2SubsettedRTList[[p]][,3][[d]][,1]) & as.numeric(AIFms2SubsettedRTList[[p]][,3][[d]][,1]) <= as.numeric(as.character(LibParentHits_df[p, c])) + abs(as.numeric(as.character(LibParentHits_df[p, c])) - as.numeric(as.character(LibParentHits_df[p, c]))*PPM_CONST)
          subsettedFrag <- subset(AIFms2SubsettedRTList[[p]][,3][[d]], fragConditional)
          if(nrow(subsettedFrag)>1){#more than one matching frag
            for(s in 1:nrow(subsettedFrag)){
              scans <- append(scans, as.numeric(AIFms2SubsettedRTList[[p]][d,1])) #gets scan number
              rt <- append(rt, as.numeric(AIFms2SubsettedRTList[[p]][d,2])) #get rt
            }
          }else if(nrow(subsettedFrag)==1){#only 1 mataching frag
            scans <- append(scans, as.numeric(AIFms2SubsettedRTList[[p]][d,1])) #gets scan number
            rt <- append(rt, as.numeric(AIFms2SubsettedRTList[[p]][d,2])) #get rt
          }#else, length is 0. no fragments found. do nothing.
          frags <- append(frags, as.numeric(subsettedFrag[,1])) #gets fragment from ms2
          intensity <- append(intensity, as.numeric(subsettedFrag[,2])) #gets intensity associated with that fragment
        }
        subsettedFrag_df <- data.frame(scans, rt, frags, intensity)
        subsettedFragList[[p]] <- subsettedFrag_df
        
        #Stores information into respective dataframes for output as All_Confirmed
        if(nrow(subsettedFragList[[p]])>0){#loop over list elements that have values
          #Max Intensity
          maxIntensity <- max(subsettedFragList[[p]][,4])
          MaxIntensity_df[p, c] <- maxIntensity
          
          #Number of Scans
          numScans <- nrow(subsettedFragList[[p]])
          NumOfScans_df[p, c] <- numScans
          
          #Confirming fragments
          if(sum(subsettedFragList[[p]][,4] > intensityCutOff) > 0 & (numScans >= minNumberOfAIFScans)){ #at least one fragment's intensity is above user inputed threshold and number of scans is greater than or equal to user inputed, ScansCutOff
            ConfirmedFragments_df[p, c] <- 1 #1 for yes, 0 for no. (ConfirmedFragments_df was initialized with all 0s)
          }
          
          #RT at max intensity
          maxIntIndex <- which.max(subsettedFragList[[p]][,4])
          RTMaxInt <- subsettedFragList[[p]][maxIntIndex,2]
          RTMaxIntensity_df[p, c] <- RTMaxInt
          
          
          #Average m/z for confirmed fragment
          avgMZ <- mean(subsettedFragList[[p]][,3])
          AverageMZ_df[p, c] <- avgMZ
        }
      }
    }
    
    #Creates df for output "AllConfirmed"
    AllConfirmed_df <- data.frame(matrix("", ncol = ncolLibParentHits*5, nrow = nrowLibParentHits))
    
    blankColumn <- rep("", nrowLibParentHits)
    
    AllConfirmed_df <- cbind(blankColumn, ConfirmedFragments_df, blankColumn, RTMaxIntensity_df, blankColumn, NumOfScans_df, blankColumn, MaxIntensity_df, blankColumn, AverageMZ_df, blankColumn, LibParentHits_df)
    
    colnames(AllConfirmed_df)[1] <- paste("Confirmed fragments if intensity is above", intensityCutOff,"and number of scans is greater than or equal to", minNumberOfAIFScans)
    colnames(AllConfirmed_df)[ncolLibParentHits+2] <- "RT at Max Intensity"
    colnames(AllConfirmed_df)[ncolLibParentHits*2+3] <- "Number of Scans"
    colnames(AllConfirmed_df)[ncolLibParentHits*3+4] <- "Max Intensity"
    colnames(AllConfirmed_df)[ncolLibParentHits*4+5] <- "Average m/z"
    colnames(AllConfirmed_df)[ncolLibParentHits*5+6] <- "Theoretical, Library Parent Hits"
    
    #DON'T WRITE THIS OUT. WE want to write out the final confirmed with Adj R2 and Slope
    # ConfirmedAll_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_All_confirmed.csv",sep="")
    # write.table(AllConfirmed_df, ConfirmedAll_dir, sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
    
    ## Code to reduce the dataset to lipids containing necessary fragments 
    ## Start with both true, if no inputs then all rows are retained
    AllConfirmed_matrix<-as.matrix(AllConfirmed_df)
    OR_BinTruth<-1
    AND_BinTruth<-1
    ## For any given confirmed fragment/precursor 
    ## add an extra row to avoid error in an apply function encase of a one row matrix
    DataCombined<-rbind(AllConfirmed_matrix,((1:ncol(AllConfirmed_matrix))*0))
    if ((length(ConfirmORcol)>0)&&(is.matrix(DataCombined[1:nrow(DataCombined),ConfirmORcol+1]))) {
      ## Sum all the elements from the binary confirmation table, if any fragment is 1 (above threshold & minimum # of scans) the element is TRUE 
      OR_BinTruth<-as.numeric(c((apply(apply(DataCombined[1:nrow(DataCombined),ConfirmORcol+1],2,as.numeric),1,sum)>0)))
    }
    if ((length(ConfirmANDcol)>0)&&(is.matrix(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))) {
      ## Sum all the elements from the binary confirmation table, if ALL fragments are 1 (above threshold & minimum # of scans) the element is TRUE
      AND_BinTruth<-as.numeric(c((apply(apply(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1],2,as.numeric),1,sum)>(length(ConfirmANDcol)-1))))
    }
    if ((length(ConfirmANDcol)>0)&&(is.vector(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))) {
      AND_BinTruth<-as.numeric(c(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))
    }
    ## Reduce the data set to those values which are TRUE for both the OR and AND fragments calculated in the two IF statements above
    DataCombinedConfirmed<-DataCombined[(OR_BinTruth+AND_BinTruth)>1,,drop=FALSE]
    
    NoMatches_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_NoIDs.csv",sep="")
    if(nrow(DataCombinedConfirmed)==0){
      write.table("No Confirmed Lipids Found",NoMatches_dir, col.names=FALSE, row.names=FALSE, quote=FALSE)
    }else{
      # ConfirmedReduced_dir <- paste(OutputDirectory,"Confirmed_Lipids//",ExtraFileNameInfo,"_reduced_confirmed.csv",sep="")
      # write.table(DataCombinedConfirmed, ConfirmedReduced_dir, sep=",",col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
      
      rowsToKeep <- as.numeric(row.names(DataCombinedConfirmed))-1 #used to reduce AIFms1SubsettedList and AIFms2SubsettedList
      AIFms1SubsettedRTList <- AIFms1SubsettedRTList[rowsToKeep]
      AIFms2SubsettedRTList <- AIFms2SubsettedRTList[rowsToKeep]
      
      DataCombinedConfirmed_df <- as.data.frame(DataCombinedConfirmed)#reduced confirmed df
      nrowReduced <- nrow(DataCombinedConfirmed_df)
      ncolReduced <- ncol(DataCombinedConfirmed_df)
      
      #gets the original theoretical library that was reduced
      LibInfoFromDataComb_df<- DataCombinedConfirmed_df[,(ncolReduced-ncolLibParentHits+1):ncolReduced]
      
      colsToCorr <- c(ColCorrAND, ColCorrOR)
      numColsForAIF <- 4 + length(colsToCorr) #4 because ID, RTmin, RTmax, Comment
      nameOfMiddleColsForAIF <- vector()
      for(c in colsToCorr){#get "precursor vs fragment" header for output
        nameOfMiddleColsForAIF <- append(nameOfMiddleColsForAIF, paste(colnames(LibParentHits_df[ParentMZcol]), " vs ", colnames(LibParentHits_df[c]), sep=""))
      }
      
      AIFHeader <- c(colnames(LibParentHits_df[1]), nameOfMiddleColsForAIF, colnames(LibParentHits_df[startRTCol:ncolLibParentHits])) #get header from LibParentHits_df
      
      AdjR2_df <- data.frame(matrix(0, ncol = numColsForAIF, nrow = nrowReduced))
      Slope_df <- data.frame(matrix(0, ncol = numColsForAIF, nrow = nrowReduced))
      colnames(AdjR2_df) <- AIFHeader
      colnames(Slope_df) <- AIFHeader
      outputLibData <- LibInfoFromDataComb_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from DataCombinedConfirmed_df
      AdjR2_df[,c(1,(numColsForAIF-2):numColsForAIF)] <- outputLibData
      Slope_df[,c(1,(numColsForAIF-2):numColsForAIF)] <- outputLibData
      
      #correlate precursor from ms1 against predefined columns to correlate against from ms2
      for(c in colsToCorr){ #loop fragment columns to correlate against ms1
        for(p in seq_len(nrowReduced)){#loop all rows of reduced confirmed data frame
          fragCorr_df<-NULL
          ms1ScanNum <- vector()
          ms2ScanNum <- vector()
          ms1Intensity <- vector()
          ms2Intensity <- vector()
          for(d in seq_len(length(AIFms1SubsettedRTList[[p]][,1]))){#store ms1 scan number and PRECURSOR intensity
            subsettedFrag <- vector(length=0)
            
            #Take the theoretical LibParentHits fragment m/z and look within a ppm window through the scan's ms2 fragments
            AIFms1fragConditional <- (as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol])) - abs(as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol])) - as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol]))*PPM_CONST) <= as.numeric(AIFms1SubsettedRTList[[p]][,3][[d]][,1])) & (as.numeric(AIFms1SubsettedRTList[[p]][,3][[d]][,1]) <= as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol])) + abs(as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol])) - as.numeric(as.character(LibInfoFromDataComb_df[p, ParentMZcol]))*PPM_CONST))
            subsettedFrag <- subset(AIFms1SubsettedRTList[[p]][,3][[d]], AIFms1fragConditional)
            
            #error handling if you find 2 fragments masses within a ppm window. Then what?
            if(nrow(subsettedFrag)>1){
              LibMass<-as.numeric(as.character(LibInfoFromDataComb_df[p, c]))
              fragMass<-as.numeric(subsettedFrag[,1])
              closestMass<-which(abs(LibMass-fragMass)==min(abs(LibMass-fragMass)))
              subsettedFrag<-subsettedFrag[closestMass,]
              subsettedFrag<-matrix(subsettedFrag,1,2)
              if(length(closestMass)>1){#edge case: if the two fragment masses are equidistant from the lib mass... then avg their intensities
                #average the intensities (and masses, but I don't use the mass later, so it doesn't matter)
                subsettedFrag<-matrix(c(mean(as.numeric(subsettedFrag[,1])),mean(as.numeric(subsettedFrag[,2]))), 1, 2)
              }
            }
            
            # print("------------MS1-----------------")
            # print(paste("d: ", d, "subsettedFrag:", subsettedFrag))
            if(nrow(subsettedFrag)==0){
              ms1Intensity <- append(ms1Intensity, 0)
            }else{# if there's 1 fragment
              ms1Intensity <- append(ms1Intensity, subsettedFrag[,2])
            }
            ms1ScanNum <- append(ms1ScanNum, as.numeric(AIFms1SubsettedRTList[[p]][d,1])) #gets scan number
          }
          for(d in seq_len(length(AIFms2SubsettedRTList[[p]][,1]))){#store ms2 intensity
            subsettedFrag <- vector(length=0) 
            
            #Take the theoretical LibParentHits fragment m/z and look within a ppm window through the scan's ms2 fragments
            AIFms2fragConditional <- as.numeric(as.character(LibInfoFromDataComb_df[p, c])) - abs(as.numeric(as.character(LibInfoFromDataComb_df[p, c])) - as.numeric(as.character(LibInfoFromDataComb_df[p, c]))*PPM_CONST) <= as.numeric(AIFms2SubsettedRTList[[p]][,3][[d]][,1]) & as.numeric(AIFms2SubsettedRTList[[p]][,3][[d]][,1]) <= as.numeric(as.character(LibInfoFromDataComb_df[p, c])) + abs(as.numeric(as.character(LibInfoFromDataComb_df[p, c])) - as.numeric(as.character(LibInfoFromDataComb_df[p, c]))*PPM_CONST)
            subsettedFrag <- subset(AIFms2SubsettedRTList[[p]][,3][[d]], AIFms2fragConditional)
            if(nrow(subsettedFrag)>1){
              LibMass<-as.numeric(as.character(LibInfoFromDataComb_df[p, c]))
              fragMass<-as.numeric(subsettedFrag[,1])
              closestMass<-which(abs(LibMass-fragMass)==min(abs(LibMass-fragMass)))
              subsettedFrag<-subsettedFrag[closestMass,]
              subsettedFrag<-matrix(subsettedFrag,1,2)
              if(length(closestMass)>1){#edge case: if the two fragment masses are equidistant from the lib mass... then avg their intensities
                #average the intensities (and masses, but I don't use the mass later, so it doesn't matter)
                subsettedFrag<-matrix(c(mean(as.numeric(subsettedFrag[,1])),mean(as.numeric(subsettedFrag[,2]))), 1, 2)
              }
            }
            
            # print("------------MS2-----------------")
            # print(paste("d: ", d, "subsettedFrag:", subsettedFrag))
            if(nrow(subsettedFrag)==0){  
              ms2Intensity <- append(ms2Intensity, 0)  
            }else{
              ms2Intensity <- append(ms2Intensity, subsettedFrag[,2])
            }
            ms2ScanNum <- append(ms2ScanNum, as.numeric(AIFms2SubsettedRTList[[p]][d,1])) #gets scan number
          }
          ms1ScanNum<-as.numeric(ms1ScanNum)
          ms2ScanNum<-as.numeric(ms2ScanNum)
          ms1Intensity <- as.numeric(ms1Intensity)
          ms2Intensity <- as.numeric(ms2Intensity)
          ms1IntensityAveraged<-vector()
          ms1ScanNumAveraged <- vector()
          
          #average each ms1's intensity that are next to eachother so we can correlate it against ms2
          for(a in seq_len(length(ms1Intensity)-1)){#-1 so I don't go out of bounds
            temp <- ms1Intensity[a+1]#next intensity
            averaged <- mean(c(ms1Intensity[a], temp))
            ms1IntensityAveraged <- append(ms1IntensityAveraged, averaged)
          }
          for(b in seq_len(length(ms1ScanNum)-1)){#-1 so I don't go out of bounds
            temp <- ms1ScanNum[b+1]#next intensity
            averaged <- mean(c(ms1ScanNum[b], temp))
            ms1ScanNumAveraged <- append(ms1ScanNumAveraged, averaged)
          }
          
          intensityCol1 <- ms1IntensityAveraged[ms1ScanNumAveraged %in% ms2ScanNum]
          intensityCol2 <- ms2Intensity[ms2ScanNum %in% ms1ScanNumAveraged]
          
          #test for mininum number of couples (found fragments in both ms1 and ms2)
          intensitiesMultiplied <- intensityCol1*intensityCol2
          numOfCouples <- length(subset(intensitiesMultiplied, intensitiesMultiplied != 0)) 
          
          columnToFill <- match(c, colsToCorr) + 1   # +1 because the ID column in AdjR2_df is column 1.
          #are there min number of user specified couples (Definition: Couples (noun) := fragment found in both ms1 and ms2 at the same scan time)
          if(numOfCouples < minNumberOfAIFScans){#bad, you get no adj R2
            # AdjR2_df[p,columnToFill] <- paste("Not enough ms1 averaged scans & ms2 scans for a linear model. We found: ", numOfCouples, " ms1 and ms2 scan couples. You specified: ", minNumberOfAIFScans, " number of coupled scans.",sep="")
            # Slope_df[p,columnToFill] <- paste("Not enough ms1 averaged scans & ms2 scans for a linear model. We found: ", numOfCouples, " ms1 and ms2 scan couples. You specified: ", minNumberOfAIFScans, " number of coupled scans.",sep="")
            AdjR2_df[p,columnToFill] <- NA
            Slope_df[p,columnToFill] <- NA
          }else{
            fragCorr_df <- data.frame(ms1ScanNumAveraged, intensityCol1, intensityCol2)
            
            intensityFit<-lm(fragCorr_df[,3]~fragCorr_df[,2])
            intensitySummary <- summary(intensityFit)
            intensityAdjR2 <- intensitySummary$adj.r.squared
            intensitySlope <- intensityFit$coefficients[2]
            
            # if(intensityAdjR2 > corrMin){
            AdjR2_df[p,columnToFill] <- round(intensityAdjR2, 3)
            Slope_df[p,columnToFill] <- round(intensitySlope, 5)
            # }else{
            
            #   AdjR2_df[p,columnToFill] <- NA
            #   Slope_df[p,columnToFill] <- NA
            # }
            
            # print(fragCorr_df)
            ##We don't use this. But I'll keep it here. intensityCorr <- cor(fragCorr_df[,2],fragCorr_df[,3],use="complete.obs")##
          }
        }#end row loop
      }#end column loop
      # write the table out.
      
      #remove theoretical library from end of DataCombinedConfirmed_df
      DataCombinedConfirmed_df <- DataCombinedConfirmed_df[,-((ncolReduced-ncolLibParentHits):ncolReduced)] #CAREFUL NICK! THIS MESSES WITH THE HEADER!...but I kinda like the .1 .2 .3. .4 
      blankColumn <- rep("", nrowReduced)
      DataCombinedConfirmed_df <- cbind(DataCombinedConfirmed_df, blankColumn, AdjR2_df, blankColumn, Slope_df, blankColumn, LibInfoFromDataComb_df) 
      
      colnames(DataCombinedConfirmed_df)[ncolLibParentHits*5+6] <- paste("Adjusted R2, Precursor Intensity vs Fragment Intensity (AIF). AdjR2 > ", corrMin, sep="")
      colnames(DataCombinedConfirmed_df)[ncolLibParentHits*5+6 + numColsForAIF + 1] <- "Slope, Precursor Intensity vs Fragment Intensity (AIF)"
      colnames(DataCombinedConfirmed_df)[ncolLibParentHits*5+6 + numColsForAIF*2 + 2] <- "Theoretical, Library Parent Hits"
      
      ConfirmedReduced_dir <- paste(OutputDirectory,"Additional_Files//",ExtraFileNameInfo,"_AdjR2Info.csv",sep="")
      write.table(DataCombinedConfirmed_df, ConfirmedReduced_dir, sep=",",col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
      
      #Reduce matches based on ConfirmAndCol
      if(length(ConfirmANDcol) > 0){
        columnToFill<-vector()
        # AdjR2_df[] <- lapply(AdjR2_df, as.numeric)
        for(i in ConfirmANDcol){
          columnToFill <- append(columnToFill, match(i, ConfirmANDcol) + 1)
        }
        #get R2 rows with text in them, and remove them.
        if(!is.numeric(AdjR2_df[,columnToFill])){#if the adjR2 column is already numeric, (don't change to as.numeric...this gives an error)
          AdjR2_df[,columnToFill] <- lapply(AdjR2_df[,columnToFill], as.numeric)
        }
        if(!is.numeric(Slope_df[,columnToFill])){#if the adjR2 column is already numeric, (don't change to as.numeric...this gives an error)
          Slope_df[,columnToFill] <- lapply(Slope_df[,columnToFill], as.numeric)
        }
        #We want AdjR2 to be > corrMin and slopes to be positive!
        #set non positive slopes to NA
        Slope_df[,columnToFill] <- replace(Slope_df[,columnToFill],Slope_df[,columnToFill]<=0,NA)
        #remove negative slopes
        AdjR2_df <- AdjR2_df[apply(cbind(AdjR2_df[,columnToFill],Slope_df[,columnToFill]), 1, function(x) all(!is.na(x))),]
        AdjR2_df <- AdjR2_df[apply(AdjR2_df[,columnToFill, drop=F], 1, function(x) all(x > corrMin)),]
        rowsToKeep <- as.numeric(row.names(AdjR2_df)) #used to reduce final output table based on the reduced AdjR2_df rows from above ConfirmAND and ConfirmOR columns
        Slope_df <- Slope_df[rowsToKeep,]
        # AdjR2_df <- AdjR2_df[apply(AdjR2_df[columnToFill], 1, function(x) all(x > corrMin & !is.na(x))),]
        
        # AdjR2_df <- AdjR2_df[apply(AdjR2_df[c(2,3)], 1, function(x) all((x > corrMin) & is.numeric(x))),]
      }
      #difference here is in the apply function.... "all" vs "any"
      if(length(ConfirmORcol) > 0){
        columnToFill<-vector()
        # AdjR2_df[] <- lapply(AdjR2_df, as.numeric)
        for(i in ConfirmORcol){
          columnToFill <- append(columnToFill, match(i, ConfirmORcol) + 1 + length(ConfirmANDcol))
        }
        if(nrow(AdjR2_df[,columnToFill]) !=0){#The AND could've removed all rows, so now we have nothing to work with.
          if(!is.numeric(AdjR2_df[,columnToFill])){#if the adjR2 column is already numeric, (don't change to as.numeric...this gives an error)
            AdjR2_df[,columnToFill] <- lapply(AdjR2_df[,columnToFill], as.numeric)
          }
          if(!is.numeric(Slope_df[,columnToFill])){#if the adjR2 column is already numeric, (don't change to as.numeric...this gives an error)
            Slope_df[,columnToFill] <- lapply(Slope_df[,columnToFill], as.numeric)
          }
          
          #We want AdjR2 to be > corrMin and slopes to be positive!
          #set non positive slopes to NA
          Slope_df[,columnToFill] <- replace(Slope_df[,columnToFill],Slope_df[,columnToFill]<=0,NA)
          #remove negative slopes
          AdjR2_df <- AdjR2_df[apply(cbind(AdjR2_df[,columnToFill],Slope_df[,columnToFill]), 1, function(x) all(!is.na(x))),]
          AdjR2_df <- AdjR2_df[apply(AdjR2_df[,columnToFill, drop=F], 1, function(x) any(x > corrMin)),]
        }
      }
      
      rowsToKeep <- as.numeric(row.names(AdjR2_df)) #used to reduce final output table based on the reduced AdjR2_df rows from above ConfirmAND and ConfirmOR columns
      DataCombinedConfirmed_df <- DataCombinedConfirmed_df[rowsToKeep,]
      
      ConfirmedReduced_dir <- paste(OutputDirectory,"Confirmed_Lipids//",ExtraFileNameInfo,"_IDed.csv",sep="")
      if(nrow(DataCombinedConfirmed_df)>0){#don't output if there's nothing in the reduced file
        write.table(DataCombinedConfirmed_df, ConfirmedReduced_dir, sep=",",col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
      }
      
      
    }#end else (if there are matches after reduction step)
    
    tEnd<-Sys.time()
    # Capture parameters used for this run and save them
    RunInfo<-matrix("",24,2)
    RunInfo[,1]<-c("Parameters Used","","Run Time","Computation Started:","Computation Ended:","Elapsed Time (largest time unit)","","Files","MS2 file name and Directory:","MS1 file name and Directory","Feature Table name and directory:","Library name and directory:","","MS/MS confirmation criteria","Library columns for confirmation (AND)","Library columns for confirmation (OR)","m/z ppm window:","RT window (min):","minimum scans:","minimum intensity:", "","AIF confirmation criteria","Minimum adjusted R2 value","Minimum scans:")
    RunInfo[,2]<-c("Values","","",as.character(tStart),as.character(tEnd),tEnd-tStart,"","",OutputInfo[1],OutputInfo[4],OutputInfo[3],LibraryLipid_self,"","",paste(ConfirmANDcol,collapse=","),paste(ConfirmORcol,collapse=","),ppm_Window,RT_Window,minNumberOfAIFScans,intensityCutOff,"","",corrMin,minNumberOfAIFScans)
    Info_dir<-paste(OutputDirectory,"Additional_Files//",ExtraFileNameInfo,"_Parameters.csv",sep="")
    write.table(RunInfo, Info_dir, sep=",",col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
  }
}#end RunAIF

#Function for MS/MS ID
RunTargeted <- function(ms2_df, FeatureList, LibraryLipid_self, ParentMZcol, OutputDirectory, ExtraFileNameInfo, ConfirmORcol, ConfirmANDcol, OutputInfo){
  if(!dir.exists(paste(OutputDirectory,"//Confirmed_Lipids",sep=""))){
    dir.create(paste(OutputDirectory,"//Confirmed_Lipids",sep=""))
  }
  if(!dir.exists(paste(OutputDirectory,"//Additional_Files",sep=""))){
    dir.create(paste(OutputDirectory,"//Additional_Files",sep=""))
  }
  #store starting time
  tStart<-Sys.time()
  
  LibraryLipid<-read.csv(LibraryLipid_self, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
  libraryLipidMZColumn <- ParentMZcol
  FeatureListMZColumn <- 1
  
  ## Create a Library of Lipids and fragments for those masses in Feature List
  FeatureListHeader <- FeatureList[1,]
  LibraryLipidHeader <- LibraryLipid[1,]
  NewLibraryLipidHeader <- cbind(LibraryLipidHeader, FeatureListHeader)
  
  sqlMerged <- sqldf(paste("select * from LibraryLipid lib inner join FeatureList iList on lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "-", PrecursorMassAccuracy, "<= iList.", colnames(FeatureList)[FeatureListMZColumn], " and iList.", colnames(FeatureList)[FeatureListMZColumn], " <= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "+", PrecursorMassAccuracy, sep = ""))
  # sqlMerged <- sqldf(paste("select * from LibraryLipid lib inner join FeatureList iList on iList.", colnames(FeatureList)[FeatureListMZColumn], " >= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], " -.005 and iList.", colnames(FeatureList)[FeatureListMZColumn], " <= lib.", colnames(LibraryLipid)[libraryLipidMZColumn], "+ .005", sep = ""))
  NewLibraryLipid <- as.matrix(sqlMerged)
  NewLibraryLipid <- rbind(NewLibraryLipidHeader,NewLibraryLipid)
  NewLibraryLipid <- NewLibraryLipid[,-(ncol(LibraryLipid)+1)] #removes mz from Feature list
  
  #Removes 1 rt col and creates RT_min and RT_max cols based on RT_Window
  RT_Col <- NewLibraryLipid[2:nrow(NewLibraryLipid),ncol(NewLibraryLipid)-1]
  NewLibraryLipid <- NewLibraryLipid[,-(ncol(NewLibraryLipid)-1)]
  RT_Window_Vector <- rep(RT_Window/2, each=nrow(NewLibraryLipid)-1)
  RT_min <- as.numeric(levels(RT_Col))[RT_Col] - as.numeric(RT_Window_Vector)
  RT_max <- as.numeric(levels(RT_Col))[RT_Col] + as.numeric(RT_Window_Vector)
  RT_min <- c("RT_min",RT_min)
  RT_max <- c("RT_max",RT_max)
  Comment <- NewLibraryLipid[,ncol(NewLibraryLipid)]
  NewLibraryLipid <- cbind(NewLibraryLipid[,1:(ncol(NewLibraryLipid)-1)], RT_min, RT_max, Comment)
  colnames(NewLibraryLipid) <- as.character(as.matrix(NewLibraryLipid[1,]))
  NewLibraryLipid <- NewLibraryLipid[-1,]
  NewLibraryLipid <- NewLibraryLipid[!is.na(NewLibraryLipid[,1]),!is.na(NewLibraryLipid[1,])]
  
  startRTCol <- ncol(NewLibraryLipid)-2
  endRTCol <- startRTCol + 1
  LibParentHits_df <- NewLibraryLipid #name change
  nrowLibParentHits <- nrow(LibParentHits_df)
  ncolLibParentHits <- ncol(LibParentHits_df)
  NoMatches_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_NoIDs.csv",sep="")
  if(nrowLibParentHits == 0){#No hits between Feature List and Library
    write.table("No Matches Found between Library and Feature List", NoMatches_dir, col.names=FALSE, row.names=FALSE, quote=FALSE)
  }else{#If there was at least one match between Feature List and the Library
    #Output dataframes. Used to build the All Confirmed
    ConfirmedFragments_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    RTMaxIntensity_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    MaxIntensity_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    NumOfScans_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    AverageMZ_df <- data.frame(matrix(0, ncol = ncolLibParentHits, nrow = nrowLibParentHits))
    
    colnames(ConfirmedFragments_df) <- colnames(LibParentHits_df) #get header from LibParentHits_df
    colnames(RTMaxIntensity_df) <- colnames(LibParentHits_df) #get header from LibParentHits_df
    colnames(MaxIntensity_df) <- colnames(LibParentHits_df) #get header from LibParentHits_df
    colnames(NumOfScans_df) <- colnames(LibParentHits_df) #get header from LibParentHits_df
    colnames(AverageMZ_df) <- colnames(LibParentHits_df) #get header from LibParentHits_df
    
    ConfirmedFragments_df[,c(1,startRTCol:ncolLibParentHits)] <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    RTMaxIntensity_df[,c(1,startRTCol:ncolLibParentHits)] <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    MaxIntensity_df[,c(1,startRTCol:ncolLibParentHits)] <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    NumOfScans_df[,c(1,startRTCol:ncolLibParentHits)] <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    AverageMZ_df[,c(1,startRTCol:ncolLibParentHits)] <- LibParentHits_df[,c(1,startRTCol:ncolLibParentHits)] #gets data from LibParentHits_df
    
    ########################################################################################
    # loop through lib parent hits M+H column, see                                         #
    #   1) if the precursor(from ms2) falls within this targetAcccuracy mz window          #
    #     2) if so... does this ms2 RT fall within the start and end RT from LibParentHits?#
    #     3) if mz frag from ms2 falls within ppm of LibParentHits                         #
    #   Then fragment is confirmed.                                                        #  
    #                                                                                      #
    #   ms2_df[[1, 3]][1, 1]                                                               #  
    #         ^matrix ^[row, column] from matrix                                           #
    ########################################################################################
    
    #Create a list (1 element for each LibParentHits row) of subsetted ms2_df based on the 
    # mz conditional: LibPrecursor - targetedAccuracy < ms2Precursor & ms2Precursor < LibPrecursor + targetedAccuracy
    # RT conditional: LibRTStart < ms2RT & ms2RT < LibRTEnd
    ms2SubsettedMZList <- list()
    ms2SubsettedRTList <- list()
    simplifiedAndSubsettedList <- list()
    for(p in 1:nrowLibParentHits){
      # MZConditional <- (((as.numeric(LibParentHits_df[p,2]) - abs(as.numeric(LibParentHits_df[p,2]) - as.numeric(LibParentHits_df[p,2])*PPM_CONST)) < as.numeric(ms2_df[,1]))) & (as.numeric(ms2_df[,1]) < (as.numeric(LibParentHits_df[p,2]) + abs(as.numeric(LibParentHits_df[p,2]) - as.numeric(LibParentHits_df[p,2])*PPM_CONST)))
      MZConditional <- (as.numeric(as.character(LibParentHits_df[p,ParentMZcol])) - SelectionAccuracy/2) <= as.numeric(ms2_df[,1]) & as.numeric(ms2_df[,1]) <= (as.numeric(as.character(LibParentHits_df[p,ParentMZcol])) + SelectionAccuracy/2)
      ms2SubsettedMZList[[p]] <- subset(ms2_df, MZConditional)
      RTConditional <- as.numeric(as.character(LibParentHits_df[p,startRTCol])) <= as.numeric(ms2_df[,2]) & as.numeric(ms2_df[,2]) <= as.numeric(as.character(LibParentHits_df[p,endRTCol]))
      ms2SubsettedRTList[[p]] <- subset(ms2_df, RTConditional)
      
      #Combines MZ and RT ms2Subsetted lists
      uniqueMZ <- unique(ms2SubsettedMZList[[p]][,1])
      simplifiedAndSubsettedList[[p]] <- subset(ms2SubsettedRTList[[p]], ms2SubsettedRTList[[p]][,1] %in% uniqueMZ)
    }
    
    ##############################################################
    #Building All_Confirmed data frame for output                #
    # Creates a list of dataframes for each column.              #
    #   Why? To organize our data.                               #  
    # Each list's element corresponds to a row from LibParentHits#
    # Each list's element holds a fragment found in the scans    #  
    ##############################################################
    for(c in ParentMZcol:((startRTCol - ParentMZcol)+1)){ #loop all fragment columns
      subsettedFragList <- list() #has nrow(LibParentHits_df) elements
      for(p in 1:nrowLibParentHits){ #loop through all simplifiedAndSubsettedList elements
        if(nrow(simplifiedAndSubsettedList[[p]]) != 0){ #don't loop if the list is empty! (for loops run once when the size is 0... :(...)
          subsettedFrag_df <- data.frame(scans=numeric(), rt=numeric(), frags=numeric(), intensity=numeric())
          scans <- vector()
          rt <- vector()
          frags <- vector()
          intensity <- vector()
          for(d in 1:nrow(simplifiedAndSubsettedList[[p]])){ #loop through each element in the list
            subsettedFrag <- vector(length=0) 
            #Take the theoretical LibParentHits fragment m/z and look within a ppm window through the scan's ms2 fragments
            fragConditional <- as.numeric(as.character(LibParentHits_df[p, c])) - abs(as.numeric(as.character(LibParentHits_df[p, c])) - as.numeric(as.character(LibParentHits_df[p, c]))*PPM_CONST) <= as.numeric(simplifiedAndSubsettedList[[p]][,3][[d]][,1]) & as.numeric(simplifiedAndSubsettedList[[p]][,3][[d]][,1]) <= as.numeric(as.character(LibParentHits_df[p, c])) + abs(as.numeric(as.character(LibParentHits_df[p, c])) - as.numeric(as.character(LibParentHits_df[p, c]))*PPM_CONST)
            subsettedFrag <- subset(simplifiedAndSubsettedList[[p]][,3][[d]], fragConditional)
            if(nrow(subsettedFrag)>1){#more than one matching frag
              for(s in 1:nrow(subsettedFrag)){
                scans <- append(scans, as.numeric(row.names(simplifiedAndSubsettedList[[p]][d,]))) #gets scan number
                rt <- append(rt, as.numeric(simplifiedAndSubsettedList[[p]][d,2])) #get rt
              }
            }else if(nrow(subsettedFrag)==1){#only 1 mataching frag
              scans <- append(scans, as.numeric(row.names(simplifiedAndSubsettedList[[p]][d,]))) #gets scan number
              rt <- append(rt, as.numeric(simplifiedAndSubsettedList[[p]][d,2])) #get rt
            }#else, length is 0. no fragments found. do nothing.
            frags <- append(frags, as.numeric(subsettedFrag[,1])) #gets fragment from ms2
            intensity <- append(intensity, as.numeric(subsettedFrag[,2])) #gets intensity associated with that fragment
          }
          subsettedFrag_df <- data.frame(scans, rt, frags, intensity)
          subsettedFragList[[p]] <- subsettedFrag_df
          
          #Stores information into respective dataframes for output as All_Confirmed
          if(nrow(subsettedFragList[[p]])>0){#loop over list elements that have values
            #Max Intensity
            maxIntensity <- max(subsettedFragList[[p]][,4])
            MaxIntensity_df[p, c] <- maxIntensity
            
            #Number of Scans
            numScans <- nrow(subsettedFragList[[p]])
            NumOfScans_df[p, c] <- numScans
            
            #Confirming fragments
            if(sum(subsettedFragList[[p]][,4] > intensityCutOff) > 0 & (numScans >= ScanCutOff)){ #at least one fragment's intensity is above user inputed threshold and number of scans is greater than or equal to user inputed, ScansCutOff
              ConfirmedFragments_df[p, c] <- 1 #1 for yes, 0 for no. (ConfirmedFragments_df was initialized with all 0s)
            }
            
            #RT at max intensity
            maxIntIndex <- which.max(subsettedFragList[[p]][,4])
            RTMaxInt <- subsettedFragList[[p]][maxIntIndex,2]
            RTMaxIntensity_df[p, c] <- RTMaxInt
            
            
            #Average m/z for confirmed fragment
            avgMZ <- mean(subsettedFragList[[p]][,3])
            AverageMZ_df[p, c] <- avgMZ
          }
        }
      }
    }
    
    #Creates df for output "AllConfirmed"
    AllConfirmed_df <- data.frame(matrix("", ncol = ncolLibParentHits*5, nrow = nrowLibParentHits))
    
    blankColumn <- rep("", nrowLibParentHits)
    
    AllConfirmed_df <- cbind(blankColumn, ConfirmedFragments_df, blankColumn, RTMaxIntensity_df, blankColumn, NumOfScans_df, blankColumn, MaxIntensity_df, blankColumn, AverageMZ_df, blankColumn, LibParentHits_df)
    colnames(AllConfirmed_df)[1] <- paste("Confirmed fragments if intensity is above", intensityCutOff,"and number of scans is greater than or equal to", ScanCutOff)
    colnames(AllConfirmed_df)[ncolLibParentHits+2] <- "RT at Max Intensity"
    colnames(AllConfirmed_df)[ncolLibParentHits*2+3] <- "Number of Scans"
    colnames(AllConfirmed_df)[ncolLibParentHits*3+4] <- "Max Intensity"
    colnames(AllConfirmed_df)[ncolLibParentHits*4+5] <- "Average m/z"
    colnames(AllConfirmed_df)[ncolLibParentHits*5+6] <- "Theoretical, Library Parent Hits"
    
    ConfirmedAll_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_All.csv",sep="")
    write.table(AllConfirmed_df, ConfirmedAll_dir, sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
    
    ## Code to reduce the dataset to lipids containing necessary fragments 
    ## Start with both true, if no inputs then all rows are retained
    AllConfirmed_matrix<-as.matrix(AllConfirmed_df)
    OR_BinTruth<-1
    AND_BinTruth<-1
    ## For any given confirmed fragment/precursor 
    ## add an extra row to avoid error in an apply function encase of a one row matrix
    DataCombined<-rbind(AllConfirmed_matrix,((1:ncol(AllConfirmed_matrix))*0))
    if ((length(ConfirmORcol)>0)&&(is.matrix(DataCombined[1:nrow(DataCombined),ConfirmORcol+1]))) {
      ## Sum all the elements from the binary confirmation table, if any fragment is 1 (above threshold & minimum # of scans) the element is TRUE 
      OR_BinTruth<-as.numeric(c((apply(apply(DataCombined[1:nrow(DataCombined),ConfirmORcol+1],2,as.numeric),1,sum)>0)))
    }
    if ((length(ConfirmANDcol)>0)&&(is.matrix(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))) {
      ## Sum all the elements from the binary confirmation table, if ALL fragments are 1 (above threshold & minimum # of scans) the element is TRUE
      AND_BinTruth<-as.numeric(c((apply(apply(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1],2,as.numeric),1,sum)>(length(ConfirmANDcol)-1))))
    }
    if ((length(ConfirmANDcol)>0)&&(is.vector(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))) {
      AND_BinTruth<-as.numeric(c(DataCombined[1:nrow(DataCombined),ConfirmANDcol+1]))
    }
    ## Reduce the data set to those values which are TRUE for both the OR and AND fragments calculated in the two IF statements above
    DataCombinedConfirmed<-DataCombined[(OR_BinTruth+AND_BinTruth)>1,,drop=FALSE]
    
    NoMatches_dir<-paste(OutputDirectory,"Additional_Files//", ExtraFileNameInfo,"_NoID.csv",sep="")
    if(nrow(DataCombinedConfirmed)==0){
      write.table("No Confirmed Lipids Found",NoMatches_dir, col.names=FALSE, row.names=FALSE, quote=FALSE)
    }else{
      ConfirmedReduced_dir <- paste(OutputDirectory,"Confirmed_Lipids//",ExtraFileNameInfo,"_IDed.csv",sep="")
      write.table(DataCombinedConfirmed, ConfirmedReduced_dir, sep=",",col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
    }
  }
  tEnd<-Sys.time()
  # Capture parameters used for this run and save them
  RunInfo<-matrix("",19,2)
  RunInfo[,1]<-c("Parameters Used","","Run Time","Computation Started:","Computation Ended:","Elapsed Time (largest time unit)","","Files","MS2 file name and Directory:","Feature Table name and directory:","Library name and directory:","","MS/MS confirmation criteria","Library columns for confirmation (AND)","Library columns for confirmation (OR)","m/z ppm window:","RT window (min):","minimum scans:","minimum intensity:")
  RunInfo[,2]<-c("Values","","",as.character(tStart),as.character(tEnd),tEnd-tStart,"","",OutputInfo[1],OutputInfo[3],LibraryLipid_self,"","",paste(ConfirmANDcol,collapse=","),paste(ConfirmORcol,collapse=","),ppm_Window,RT_Window,ScanCutOff,intensityCutOff)
  Info_dir<-paste(OutputDirectory,"Additional_Files//",ExtraFileNameInfo,"_Parameters.csv",sep="")
  write.table(RunInfo, Info_dir, sep=",",col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
}#end of function

ReadFeatureTable <- function(FeatureTable_dir){
  #Converts Feature Table to an "Feature List" which has only MZ, RT, and Comment columns from the Feature table.
  FeatureTable_df<- read.csv(FeatureTable_dir, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE, stringsAsFactors=FALSE)
  
  #Conversion: Feature Table -> Feature List
  IncMZCol <- as.numeric(FeatureTable_df[RowStartForFeatureTableData:nrow(FeatureTable_df),MZColumn])
  IncRTCol <- as.numeric(FeatureTable_df[RowStartForFeatureTableData:nrow(FeatureTable_df),RTColumn])
  IncCommentCol <- as.numeric(FeatureTable_df[RowStartForFeatureTableData:nrow(FeatureTable_df),CommentColumn])
  FeatureList <- matrix(data=c(IncMZCol, IncRTCol, IncCommentCol), nrow=length(IncMZCol), ncol=3)
  FeatureList <- rbind(c("M/Z", "RT", "Comment"), FeatureList)
  FeatureList <- as.data.frame(FeatureList)
  FeatureList<-FeatureList[c(TRUE,(!is.na(as.numeric(FeatureList[2:nrow(FeatureList),1])))),]
  return(FeatureList)
}

#BUILD ms1
createMS1dataFrame <- function (ms1_dir){
  ###############################
  #Store ms1 data into dataframe#
  ###############################
  ms1 <- scan(file=ms1_dir, what='character') #read in .ms1 as list of characters
  s_index <- match("S", ms1)
  ms1 <- ms1[s_index:length(ms1)] #cut off head useless info from .ms1
  s_indicies <- which(ms1=="S")
  ms1_df <- data.frame(scanNum=numeric(), rt=numeric(), mz_intensity=I(list()))
  
  #interpret .ms1 file into more reasonable format
  #loop over all "S" blocks to get ms1 data
  for(s in 1:length(s_indicies)){
    mz <- vector()
    intensity <- vector()
    nextSBlock <- s_indicies[s+1]
    if(s == length(s_indicies)) { #last iteration
      for (m in seq(15, length(ms1)-s_indicies[s], 2)){
        mz <- append(mz, ms1[s_indicies[s]+m])
      } 
      for (i in seq(16, length(ms1)-s_indicies[s], 2)){
        intensity <- append(intensity, ms1[s_indicies[s]+i])
      }
    }else{ #every iteration except last
      for (m in seq(15, nextSBlock-s_indicies[s]-2, 2)){
        mz <- append(mz, ms1[s_indicies[s]+m])
      } 
      for (i in seq(16, nextSBlock-s_indicies[s]-1, 2)){
        intensity <- append(intensity, ms1[s_indicies[s]+i])
      } 
    }
    rt <- ms1[s_indicies[s]+5]
    scanNum <- ms1[s_indicies[s]+1]#scan Number (1 unit to the right of each S block)
    ms1_df[s, 1] <- as.numeric(scanNum)
    ms1_df[s, 2] <- as.numeric(rt) #store RT in second col
    ms1_df[[s, 3]] <- matrix(cbind(mz, intensity), length(mz), 2) #store mz fragments and their intensity corresponding to each respective precursor
  }
  return(ms1_df)
}

createddMS2dataFrame <- function (ms2_dir){
  ###############################
  #Store ms2 data into dataframe#
  ###############################
  ms2 <- scan(file=ms2_dir, what='character') #read in .ms2 as list of characters
  s_index <- match("S", ms2)
  ms2 <- ms2[s_index:length(ms2)] #cut off head useless info from .ms2
  s_indicies <- which(ms2=="S")
  ms2_df <- data.frame(precursor=numeric(), rt=numeric(), mz_intensity=I(list()))
  
  #interpret .ms2 file into more reasonable format
  #loop over all "S" blocks to get ms2 data
  for(s in 1:length(s_indicies)){
    mz <- vector()
    intensity <- vector()
    nextSBlock <- s_indicies[s+1]
    if(s == length(s_indicies)) { #last iteration
      if(ms2[s_indicies[s]+16]=="Z"){ 
        for (m in seq(19, length(ms2)-s_indicies[s], 2)){
          mz <- append(mz, ms2[s_indicies[s]+m])
        } 
        for (i in seq(20, length(ms2)-s_indicies[s], 2)){
          intensity <- append(intensity, ms2[s_indicies[s]+i])
        } 
      }else{
        for (m in seq(16, length(ms2)-s_indicies[s], 2)){
          mz <- append(mz, ms2[s_indicies[s]+m])
        } 
        for (i in seq(17, length(ms2)-s_indicies[s], 2)){
          intensity <- append(intensity, ms2[s_indicies[s]+i])
        }
      }
    }else if(ms2[s_indicies[s]+16]=="Z"){ #Need this conditional because some "S blocks" in the ms2 file have an extra line of information, specifically, "Z" "1" "#####"
      for (m in seq(19, nextSBlock-s_indicies[s]-2, 2)){
        mz <- append(mz, ms2[s_indicies[s]+m])
      } 
      for (i in seq(20, nextSBlock-s_indicies[s]-1, 2)){
        intensity <- append(intensity, ms2[s_indicies[s]+i])
      } 
    }else{ #every iteration except last
      for (m in seq(16, nextSBlock-s_indicies[s]-2, 2)){
        mz <- append(mz, ms2[s_indicies[s]+m])
      } 
      for (i in seq(17, nextSBlock-s_indicies[s]-1, 2)){
        intensity <- append(intensity, ms2[s_indicies[s]+i])
      } 
    }
    precursor <- ms2[s_indicies[s]+3]
    rt <- ms2[s_indicies[s]+6]
    ms2_df[s, 1] <- as.numeric(precursor) #store precursor mz in first col
    ms2_df[s, 2] <- as.numeric(rt) #store RT in second col
    ms2_df[[s, 3]] <- matrix(cbind(mz, intensity), length(mz), 2) #store mz fragments and their intensity corresponding to each respective precursor
  }
  return(ms2_df)
}

createAIFMS2dataFrame <- function (ms2_dir){
  ###############################
  #Store ms2 data into dataframe#
  ###############################
  ms2 <- scan(file=ms2_dir, what='character') #read in .ms2 as list of characters
  s_index <- match("S", ms2)
  ms2 <- ms2[s_index:length(ms2)] #cut off head useless info from .ms2
  s_indicies <- which(ms2=="S")
  ms2_df<- data.frame(scanNum=numeric(), rt=numeric(), mz_intensity=I(list()))
  
  #interpret .ms2 file into more reasonable format
  #loop over all "S" blocks to get ms2 data
  for(s in 1:length(s_indicies)){
    mz <- vector()
    intensity <- vector()
    nextSBlock <- s_indicies[s+1]
    if(s == length(s_indicies)) { #last iteration
      if(ms2[s_indicies[s]+16]=="Z"){ 
        for (m in seq(19, length(ms2)-s_indicies[s], 2)){
          mz <- append(mz, ms2[s_indicies[s]+m])
        } 
        for (i in seq(20, length(ms2)-s_indicies[s], 2)){
          intensity <- append(intensity, ms2[s_indicies[s]+i])
        } 
      }else{
        for (m in seq(16, length(ms2)-s_indicies[s], 2)){
          mz <- append(mz, ms2[s_indicies[s]+m])
        } 
        for (i in seq(17, length(ms2)-s_indicies[s], 2)){
          intensity <- append(intensity, ms2[s_indicies[s]+i])
        }
      }
    }else if(ms2[s_indicies[s]+16]=="Z"){ #Need this conditional because some "S blocks" in the ms2 file have an extra line of information, specifically, "Z" "1" "#####"
      for (m in seq(19, nextSBlock-s_indicies[s]-2, 2)){
        mz <- append(mz, ms2[s_indicies[s]+m])
      } 
      for (i in seq(20, nextSBlock-s_indicies[s]-1, 2)){
        intensity <- append(intensity, ms2[s_indicies[s]+i])
      } 
    }else{ #every iteration except last
      for (m in seq(16, nextSBlock-s_indicies[s]-2, 2)){
        mz <- append(mz, ms2[s_indicies[s]+m])
      } 
      for (i in seq(17, nextSBlock-s_indicies[s]-1, 2)){
        intensity <- append(intensity, ms2[s_indicies[s]+i])
      } 
    }
    scanNum <- ms2[s_indicies[s]+1] #gets Scan Number (right of each "S" block in ms2)
    rt <- ms2[s_indicies[s]+6]
    ms2_df[s, 1] <- as.numeric(scanNum) #store scan number in first col
    ms2_df[s, 2] <- as.numeric(rt) #store RT in second col
    ms2_df[[s, 3]] <- matrix(cbind(mz, intensity), length(mz), 2) #store mz fragments and their intensity corresponding to each respective precursor
  }
  return(ms2_df)
}

CreateIDs <- function(PeakTableDirectory, ddMS2directory, Classdirectory, AIFdirectory, ImportLib, OutputDirectory, ddMS2, ddMS2Class, AIF, mode){
  # Generates IDs by matching Comment column from FeatureTable.csv and All_Confirmed.csv files
  # Feature Table Columns:
  # ID
  # Intensity
  # Class
  # Adduct
  # Notation if only 1 class was observed
  
  PeakTable<-read.csv(PeakTableDirectory,sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
  LastCol<-ncol(PeakTable)
  IDcolumn<-LastCol+1
  ddMS2_Files<-list.files(ddMS2directory,pattern="*IDed.csv")
  class_Files<-list.files(Classdirectory,pattern="*IDed.csv")
  AIF_Files<-list.files(AIFdirectory,pattern="*IDed.csv")
  
  ##Debug
  # Directory<-ddMS2directory
  # Files<-Files_ddMS2Directory
  # Code<-ddMS2_Code
  
  getIDCommentAndIntensities <- function (RunTF,Directory,Files,Code,isAIF){
    Compiled<-data.frame(Lipid=character(), Feature=numeric(), SumOfAllFragmentsIntensities=numeric(), class=character())
    if(RunTF==TRUE & length(Files) > 0){
      for(i in seq_len(length(Files))){
        #Read in reduced_confirmed file
        TempFile <- read.csv(paste(Directory,Files[i],sep=""),sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
        if(nrow(TempFile)>1){ #safety net to check if there are more rows than just the header
          CommentCol <- which(TempFile[1,]=="Comment")[1]#receive first comment column
          #get Max Intensity dataframe portion from All_Confirmed (AIF all_confirmed files have specific header)
          if(isAIF){
            MaxIntensityCols <- (which(TempFile[1,]=="Max Intensity")+2):(which(TempFile[1,]=="RT_min.3")-1)
          }else{
            MaxIntensityCols <- (which(TempFile[1,]=="Max Intensity")+2):(which(TempFile[1,]=="RT_min")[4]-1)
          }
          maxIntensity_df <- TempFile[2:nrow(TempFile),MaxIntensityCols]
          
          summedIntensity<-c()
          for(i in seq_len(nrow(maxIntensity_df))){
            summedIntensity <- append(summedIntensity, sum(unique(as.numeric(as.matrix(maxIntensity_df[i,])))))
          }
          
          class<-rep(as.character(TempFile[1,2]), times=(nrow(TempFile)-1))#Gets class and repeats it
          adduct<-rep(as.character(TempFile[1,3]), times=(nrow(TempFile)-1))#Gets adduct and repeats it
          TempFile <- TempFile[2:nrow(TempFile),c(2, CommentCol)] #remove header and keep ID & Comment column
          TempFile <- cbind(TempFile, summedIntensity, class, adduct) #append summed intensity to TempFile
          colnames(TempFile)[1]<-"Lipid"
          colnames(TempFile)[2]<-"Feature"
          colnames(TempFile)[3]<-"SumOfAllFragmentsIntensities"
          colnames(TempFile)[4]<-"Class"
          colnames(TempFile)[5]<-"Adduct"
          
          Compiled <- rbind(Compiled, TempFile)
        }
      }
      Compiled[,1]<-paste(Code,"_",Compiled[,1],sep="")
    }
    return(Compiled)
  }
  compAIF_df <- getIDCommentAndIntensities(AIF,AIFdirectory,AIF_Files,AIF_Code, TRUE)
  compddMS2_df <- getIDCommentAndIntensities(ddMS2,ddMS2directory,ddMS2_Files,ddMS2_Code, FALSE)
  compddMS2class_df <- getIDCommentAndIntensities(ddMS2Class,Classdirectory,class_Files,Class_Code, FALSE)
  
  compiled<-rbind(compddMS2_df, compAIF_df, compddMS2class_df) #join all compiled dataframes together (AIF, class, ddms2)
  write.table(compiled, paste(OutputDirectory,mode,"_OnlyIDs.csv",sep=""), sep=",", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
  
  #Create dataframe to append on the end of your feature table  
  NOID<-paste(NoID_Code,"_NoID",sep="")
  appendedIDCol<-matrix(NOID, nrow(PeakTable),1)
  if(RowStartForFeatureTableData > 2){
    appendedIDCol[2:(RowStartForFeatureTableData-1)]<-"" #Start "_NoID" where your data/comments/features starts...therefore, don't put "_NoID" if there is no data there.
  }
  appendedIntensityCol<-matrix("", nrow(PeakTable),1)
  appendedClass<-matrix("", nrow(PeakTable),1)
  appendedAdduct<-matrix("", nrow(PeakTable),1)
  appendedOnlyOneClass<-matrix("", nrow(PeakTable),1)
  NewPeakTable<-cbind(PeakTable,appendedIDCol, appendedIntensityCol, appendedClass, appendedAdduct, appendedOnlyOneClass)
  
  #Handy function to convert factor to character so you can manipulate the data in cells
  factorToCharacter <- function(df){
    for(i in which(sapply(df, class) == "factor")) df[[i]] = as.character(df[[i]])
    return(df)
  }
  NewPeakTable[,IDcolumn:(IDcolumn+4)] <- factorToCharacter(as.data.frame(NewPeakTable[,IDcolumn:(IDcolumn+4)]))#factor to character on the last 5 columns
  NewPeakTable[1,IDcolumn:(IDcolumn+4)] <- c("ID_Ranked","Intensity_Ranked","Class_At_Max_Intensity","Adduct_At_Max_Intensity","Only_One_Class")#create header for the last 5 columns
  
  for (i in RowStartForFeatureTableData:nrow(NewPeakTable)) {
    TempCompiled <- compiled[(as.numeric(as.character(compiled[,2]))==as.numeric(as.character(NewPeakTable[i,CommentColumn]))),]
    if(nrow(TempCompiled)==1) {
      NewPeakTable[i,IDcolumn]<-TempCompiled[,1] 
      NewPeakTable[i,IDcolumn+1] <- paste0(TempCompiled[,3], collapse=" | ") #intensity
      NewPeakTable[i,IDcolumn+2] <- paste0(TempCompiled[1,4], collapse=" | ") #get's class at max intensity
      NewPeakTable[i,IDcolumn+3] <- paste0(TempCompiled[1,5]) #get's adduct at max intensity
      NewPeakTable[i,IDcolumn+4] <- "Yes" #only one class. yes.
    }else if(nrow(TempCompiled)>1) {
      TempCompiled <- TempCompiled[with(TempCompiled, order(-SumOfAllFragmentsIntensities)), ]#sort intensities, max first
      
      rowsToKeep <- match(unique(TempCompiled[,1]),TempCompiled[,1]) #removes duplicates and keeps highest intensity 
      TempCompiled <- TempCompiled[rowsToKeep,] #removes duplicates 
      
      NewPeakTable[i,IDcolumn]   <- paste0(TempCompiled[,1], collapse=" | ")
      NewPeakTable[i,IDcolumn+1] <- paste0(TempCompiled[,3], collapse=" | ") #intensity
      NewPeakTable[i,IDcolumn+2] <- paste0(TempCompiled[1,4], collapse=" | ") #get's class at max intensity
      NewPeakTable[i,IDcolumn+3] <- paste0(TempCompiled[1,5]) #get's adduct at max intensity
      NewPeakTable[i,IDcolumn+4] <- ifelse(length(unique(TempCompiled[,4]))==1, "Yes", "No") #Only one class?: yes, else? no.
    }
  }
  
  Lib <- read.csv(ImportLib, sep=",", na.strings="NA", dec=".", strip.white=TRUE,header=FALSE)
  Lib <- as.matrix(Lib)
  #Exact mass matching for Precursor library to feature table m/zs
  NOID <- paste(NoID_Code,"_NoID",sep="")
  NumLibMZ <- as.numeric(Lib[,ParentMZcol_in])
  NumLibMZ[1] <- 0
  for (i in RowStartForFeatureTableData:nrow(NewPeakTable)) {
    NumData <- as.numeric(as.character(NewPeakTable[i,MZColumn]))
    TempID <- Lib[(NumData - PrecursorMassAccuracy < NumLibMZ) & (NumLibMZ < NumData + PrecursorMassAccuracy), LibColInfo]
    # TempID <- Lib[(NumData-abs(NumData-NumData*PPM_CONST) < NumLibMZ) & (NumLibMZ < NumData + (abs(NumData-NumData*PPM_CONST))), LibColInfo]
    if ((length(TempID)==1)&&(NewPeakTable[i,IDcolumn]==NOID)){
      NewPeakTable[i,IDcolumn]<-paste0(ExactMass_Code,"_",TempID,sep="")
    }else if ((length(TempID)>1)&&(NewPeakTable[i,IDcolumn]==NOID)){
      NewPeakTable[i,IDcolumn]<- paste0(paste(ExactMass_Code,"_",TempID, sep=""),collapse=" | ")
    }
  }
  
  #Sorting based on confirmation numbers 
  dataRange <- RowStartForFeatureTableData:nrow(NewPeakTable) #defining a constant
  TempSorted <- NewPeakTable[dataRange,]
  ConfirmationNum <- as.numeric(substring(NewPeakTable[dataRange,IDcolumn], 1, 1)) #get's confirmation numbers (first element in string of IDs)
  IDSubstr <- substring(NewPeakTable[dataRange,IDcolumn], 3, 10)
  TempSorted <- cbind(TempSorted,ConfirmationNum)  #add IDs to end of NewPeakTable
  TempSorted <- TempSorted[order(ConfirmationNum, IDSubstr),]  #sort NewPeakTable based on Confirmation numbers
  TempSorted <- TempSorted[,-ncol(TempSorted)]
  NewPeakTable[dataRange,] <- TempSorted
  
  write.table(NewPeakTable, paste(OutputDirectory,mode,"IDed.csv",sep=""), sep=",",col.names=FALSE, row.names=FALSE, quote=TRUE, na="NA")
  
}

####End functions####




####Read in files, create folder structure, and error handle####
if(length(foldersToRun)==0){
  lengthFoldersToRun <- 1 #if there are no subfolders, that means you have the faeture table and ms2s in that current directory, therefore, run analysis on those files.
}else{
  lengthFoldersToRun <- length(foldersToRun)#run analysis on all subfolders
}

for(i in seq_len(lengthFoldersToRun)){
  if(length(foldersToRun)==0){#we're in current (and only) folder that contains feature table and ms2 
    fpath <- InputDirectory
  }else if(foldersToRun[i] == "Output"){
    fpath <- InputDirectory
    print(paste("Warning: Remove your 'Output' folder from the current Input Directory:", InputDirectory))
  }else{
    fpath <- paste(InputDirectory, foldersToRun[i], "\\", sep="")
  }
  fileName <- basename(fpath)
  
  ddMS2NEG_in <- list.files(path=fpath, pattern="[nNgG]\\.ms2", ignore.case=FALSE)
  AIFMS1NEG_in <- list.files(path=fpath, pattern="[nNgG]\\.ms1", ignore.case=FALSE)
  AIFMS2NEG_in <- list.files(path=fpath, pattern="[nNgG]\\.ms2", ignore.case=FALSE)
  ddMS2POS_in <- list.files(path=fpath, pattern="[pPsS]\\.ms2", ignore.case=FALSE)
  AIFMS1POS_in <- list.files(path=fpath, pattern="[pPsS]\\.ms1", ignore.case=FALSE)
  AIFMS2POS_in <- list.files(path=fpath, pattern="[pPsS]\\.ms2", ignore.case=FALSE)
  
  #separate ddMS and AIF
  ddMS2NEG_in <- ddMS2NEG_in[grep("[dD][dD]", ddMS2NEG_in)]
  AIFMS1NEG_in <- AIFMS1NEG_in[grep("[Aa][Ii][Ff]", AIFMS1NEG_in)] #Yang 20180315. Orig: AIFMS1NEG_in <- AIFMS1NEG_in[grep("[AIFaif][AIFaif][AIFaif]", AIFMS1NEG_in)]
  AIFMS2NEG_in <- AIFMS2NEG_in[grep("[Aa][Ii][Ff]", AIFMS2NEG_in)] #Yang 20180315. Orig: AIFMS2NEG_in <- AIFMS2NEG_in[grep("[AIFaif][AIFaif][AIFaif]", AIFMS2NEG_in)]
  ddMS2POS_in <- ddMS2POS_in[grep("[dD][dD]", ddMS2POS_in)]
  AIFMS1POS_in <- AIFMS1POS_in[grep("[Aa][Ii][Ff]", AIFMS1POS_in)] #Yang 20180315. Orig: AIFMS1POS_in <- AIFMS1POS_in[grep("[AIFaif][AIFaif][AIFaif]", AIFMS1POS_in)]
  AIFMS2POS_in <- AIFMS2POS_in[grep("[Aa][Ii][Ff]", AIFMS2POS_in)] #Yang 20180315. Orig: AIFMS2POS_in <- AIFMS2POS_in[grep("[AIFaif][AIFaif][AIFaif]", AIFMS2POS_in)]
  
  #user info outputted for error handling
  if(length(ddMS2POS_in) == 0){
    print(paste("CAUTION: We detected", length(ddMS2POS_in),"positive ddMS .ms2 files in the folder: ", fileName," ...If this incorrect, check that you have 'p', 'P', 'pos', or 'POS' at the end of the file name and you must have a 'dd' within the name. OR Remove the folder: ", fileName))
  }
  if(length(ddMS2NEG_in) == 0){
    print(paste("CAUTION: We detected", length(ddMS2NEG_in),"negative ddMS .ms2 files in the folder: ", fileName," ...If this incorrect, check that you have 'n', 'N', 'neg', or 'NEG' at the end of the file name and you must have a 'dd' within the name. OR Remove the folder: ", fileName))
  }
  if(length(AIFMS1POS_in) == 0){
    print(paste("CAUTION: We detected", length(AIFMS1POS_in),"positive AIF .ms1 files in the folder: ", fileName," ...If this incorrect, check that you have ('AIF') and ('p', 'P', 'pos', or 'POS') at the end of the file name. OR Remove the folder: ", fileName))
  }
  if(length(AIFMS1NEG_in) == 0){
    print(paste("CAUTION: We detected", length(AIFMS1NEG_in),"negative AIF .ms1 files in the folder: ", fileName," ...If this incorrect, check that you have ('AIF') and ('n', 'N', 'neg', or 'NEG') at the end of the file name. OR Remove the folder: ", fileName))
  }
  if(length(AIFMS2POS_in) == 0){
    print(paste("CAUTION: We detected", length(AIFMS2POS_in),"positive AIF .ms2 files in the folder: ", fileName," ...If this incorrect, check that you have ('AIF') and ('p', 'P', 'pos', or 'POS') at the end of the file name. OR Remove the folder: ", fileName))
  }
  if(length(AIFMS2NEG_in) == 0){
    print(paste("CAUTION: We detected", length(AIFMS2NEG_in),"negative AIF .ms2 files in the folder: ", fileName," ...If this incorrect, check that you have ('AIF') and ('n', 'N', 'neg', or 'NEG') at the end of the file name. OR Remove the folder: ", fileName))
  }
  
  FeatureTable_NEG <- list.files(path=fpath, pattern="[nNgG]\\.csv", ignore.case=FALSE)
  if(length(FeatureTable_NEG) > 1){
    stop(paste("ERROR: You should only have 1 Negative mode Feature Table... we detected", length(FeatureTable_NEG)," Feature Tables in the folder:", fileName))
  }else if(length(FeatureTable_NEG) == 0){
    print(paste("CAUTION: Could not find any negative mode Feature Tables... we detected", length(FeatureTable_NEG)," Feature Tables in the folder: ", fileName," ...If this incorrect, check that you have an 'n', 'N', 'neg', or 'NEG' at the end of the file name. OR Remove the folder: ", fileName))
  }
  
  FeatureTable_POS <- list.files(path=fpath, pattern="[PpSs]\\.csv", ignore.case=FALSE)
  if(length(FeatureTable_POS) > 1){
    stop(paste("ERROR: You should only have 1 Positive mode Feature Table... we detected", length(FeatureTable_POS)," Feature Tables in the folder:", fileName))
  }else if(length(FeatureTable_POS) == 0){
    print(paste("CAUTION: Could not find any Positive mode Feature Tables... we detected", length(FeatureTable_POS)," Feature Tables in the folder: ", fileName," ...If this incorrect, check that you have an 'p', 'P', 'pos', or 'POS' at the end of the file name. OR Remove the folder: ", fileName))
  }
  
  #Negative/Positive mode sample names (took .ms2 files and dropped the ".ms2")
  ExtraSampleNameddMSNEG_in <- vector()
  ExtraSampleNameddMSPOS_in <- vector()
  ExtraSampleNameAIFNEG_in <- vector()
  ExtraSampleNameAIFPOS_in <- vector()
  for(j in seq_len(length(ddMS2NEG_in))){   ExtraSampleNameddMSNEG_in[j] <- sub("\\.\\w+", "", ddMS2NEG_in[j])    }
  for(j in seq_len(length(ddMS2POS_in))){   ExtraSampleNameddMSPOS_in[j] <- sub("\\.\\w+", "", ddMS2POS_in[j])    }
  for(j in seq_len(length(AIFMS2NEG_in))){   ExtraSampleNameAIFNEG_in[j] <- sub("\\.\\w+", "", AIFMS2NEG_in[j])    }
  for(j in seq_len(length(AIFMS2POS_in))){   ExtraSampleNameAIFPOS_in[j] <- sub("\\.\\w+", "", AIFMS2POS_in[j])    }
  
  runPosddMS <- FALSE
  runNegddMS <- FALSE
  runPosAIF <- FALSE
  runNegAIF <- FALSE
  #Run Negative mode analysis if there are negative .ms2 and .csv files
  if(length(ddMS2NEG_in) != 0 && length(FeatureTable_NEG) == 1){  runNegddMS <- TRUE  }
  #Run Positive mode analysis if there are positive .ms2 and .csv files
  if(length(ddMS2POS_in) != 0 && length(FeatureTable_POS) == 1) {
    runPosddMS <- TRUE
  }
  if(length(AIFMS2NEG_in) != 0 && length(AIFMS1NEG_in) != 0 && length(FeatureTable_NEG) == 1){  runNegAIF <- TRUE   }
  if(length(AIFMS2POS_in) != 0 && length(AIFMS1POS_in) != 0 && length(FeatureTable_POS) == 1){  runPosAIF <- TRUE   }
  
  #Create output file structure
  #Shrimp
  #--AIF
  #----Neg
  #------Additional_Files
  #------Confirmed_Lipids
  #----Pos
  #------Additional_Files
  #------Confirmed_Lipids
  #--ddMS
  #----Neg
  #------Additional_Files
  #------Confirmed_Lipids
  #----Pos
  #------Additional_Files
  #------Confirmed_Lipids
  #----PosByClass
  #------Additional_Files
  #------Confirmed_Lipids
  
  if(length(foldersToRun)==0){#1 root folder
    OutputDirectory<-paste(InputDirectory, "Output\\", sep="")
    if(!dir.exists(OutputDirectory)){ dir.create(OutputDirectory) }
    if(runPosAIF || runNegAIF){#AIF
      OutputDirectoryAIF <- paste(InputDirectory, "Output\\AIF\\", sep="")    
      if(!dir.exists(OutputDirectoryAIF)){  dir.create(OutputDirectoryAIF)  }
      if(runPosAIF){#pos AIF
        OutputDirectoryAIFPos_in <- paste(OutputDirectoryAIF,"Pos\\",sep="")
        if(!dir.exists(OutputDirectoryAIFPos_in)){ dir.create(OutputDirectoryAIFPos_in) }
      }
      if(runNegAIF){#neg AIF
        OutputDirectoryAIFNeg_in <- paste(OutputDirectoryAIF,"Neg\\",sep="")
        if(!dir.exists(OutputDirectoryAIFNeg_in)){ dir.create(OutputDirectoryAIFNeg_in) }
      }
    }
    if(runPosddMS || runNegddMS){#ddMS
      OutputDirectoryddMS <- paste(InputDirectory, "Output\\ddMS\\", sep="")    
      if(!dir.exists(OutputDirectoryddMS)){ dir.create(OutputDirectoryddMS) }
      if(runPosddMS){#pos ddMS
        OutputDirectoryddMSPos_in <- paste(OutputDirectoryddMS,"Pos\\",sep="")
        if(!dir.exists(OutputDirectoryddMSPos_in)){ dir.create(OutputDirectoryddMSPos_in) }
      }
      if(runPosddMS){#posByClass ddMS
        OutputDirectoryddMSPosByClass_in <- paste(OutputDirectoryddMS,"PosByClass\\",sep="")
        if(!dir.exists(OutputDirectoryddMSPosByClass_in)){ dir.create(OutputDirectoryddMSPosByClass_in) }
      }
      if(runNegddMS){#negByClass ddMS
        OutputDirectoryddMSNegByClass_in <- paste(OutputDirectoryddMS,"NegByClass\\",sep="")
        if(!dir.exists(OutputDirectoryddMSNegByClass_in)){ dir.create(OutputDirectoryddMSNegByClass_in) }
      }
      if(runNegddMS){#neg ddMS
        OutputDirectoryddMSNeg_in <- paste(OutputDirectoryddMS,"Neg\\",sep="")
        if(!dir.exists(OutputDirectoryddMSNeg_in)){ dir.create(OutputDirectoryddMSNeg_in) }
      }
    }
  }else{#more than 1 root folder  
    OutputDirectory <- paste(InputDirectory, foldersToRun[i], "\\Output\\", sep="")
    if(!dir.exists(OutputDirectory)){ dir.create(OutputDirectory) }
    if(runPosAIF || runNegAIF){#AIF
      OutputDirectoryAIF <- paste(OutputDirectory, "AIF\\", sep="")    
      if(!dir.exists(OutputDirectoryAIF)){  dir.create(OutputDirectoryAIF)  }
      if(runPosAIF){#pos AIF
        OutputDirectoryAIFPos_in <- paste(OutputDirectoryAIF, "Pos\\",sep="")
        if(!dir.exists(OutputDirectoryAIFPos_in)){ dir.create(OutputDirectoryAIFPos_in) }
      }
      if(runNegAIF){#neg AIF
        OutputDirectoryAIFNeg_in <- paste(OutputDirectoryAIF, "Neg\\",sep="")
        if(!dir.exists(OutputDirectoryAIFNeg_in)){ dir.create(OutputDirectoryAIFNeg_in) }
      }
    }
    if(runPosddMS || runNegddMS){#ddMS
      OutputDirectoryddMS <- paste(OutputDirectory, "ddMS\\", sep="")    
      if(!dir.exists(OutputDirectoryddMS)){ dir.create(OutputDirectoryddMS) }
      if(runPosddMS){#pos ddMS
        OutputDirectoryddMSPos_in <- paste(OutputDirectoryddMS, "Pos\\",sep="")
        if(!dir.exists(OutputDirectoryddMSPos_in)){ dir.create(OutputDirectoryddMSPos_in) }
      }
      if(runPosddMS){#posByClass ddMS
        OutputDirectoryddMSPosByClass_in <- paste(OutputDirectoryddMS, "PosByClass\\",sep="")
        if(!dir.exists(OutputDirectoryddMSPosByClass_in)){ dir.create(OutputDirectoryddMSPosByClass_in) }
      }
      if(runNegddMS){#negByClass ddMS
        OutputDirectoryddMSNegByClass_in <- paste(OutputDirectoryddMS,"NegByClass\\",sep="")
        if(!dir.exists(OutputDirectoryddMSNegByClass_in)){ dir.create(OutputDirectoryddMSNegByClass_in) }
      }
      if(runNegddMS){#neg ddMS
        OutputDirectoryddMSNeg_in <- paste(OutputDirectoryddMS, "Neg\\",sep="")
        if(!dir.exists(OutputDirectoryddMSNeg_in)){ dir.create(OutputDirectoryddMSNeg_in) }
      }
    }
  }#end else
  
  
  #### Run the libraries and input data ####
  NegClassDDLib <- FALSE
  PosDDLib <- FALSE
  NegDDLib <- FALSE
  PosClassDDLib <- FALSE
  NegAIFLib <- FALSE
  PosAIFLib <- FALSE
  
  #NEG
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "NEG" & toupper(LibraryCriteria[,6]) == "FALSE",] #subset LibraryCriteria to find negative libraries
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,4]) == "TRUE",] #subset LibraryCriteria to find ddMS libraries to run
  if(runNegddMS && nrow(LibraryCriteria)>0){
    NegDDLib <- TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_NEG, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_NEG,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    for (c in 1:length(ddMS2NEG_in)){
      MS2_dir_in <- paste(fpath, ddMS2NEG_in[c], sep="")
      cat(paste0("Reading in file:\t", ddMS2NEG_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      ExtraSample<-ExtraSampleNameddMSNEG_in[c]
      MS2_df_in <- createddMS2dataFrame(MS2_dir_in)
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in)
      
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";"))) 
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0){ConfirmORCol<-NULL}
        RunTargeted(MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryddMSNeg_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Negative ddMS analysis")
  }
  
  #POS ddMS
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "POS" & toupper(LibraryCriteria[,6]) == "FALSE",] #subset LibraryCriteria to find positive libraries(not pos-by-class)
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,4]) == "TRUE",] #subset LibraryCriteria to find ddMS libraries to run
  if(runPosddMS && nrow(LibraryCriteria)>0){
    PosDDLib <- TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_POS, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_POS,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    for (c in 1:length(ddMS2POS_in)){
      MS2_dir_in <- paste(fpath, ddMS2POS_in[c],sep="")
      cat(paste0("Reading in file:\t", ddMS2POS_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      ExtraSample <- ExtraSampleNameddMSPOS_in[c]
      MS2_df_in <- createddMS2dataFrame(MS2_dir_in)
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in)
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";"))) 
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0 || is.na(ConfirmANDCol)){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0 || is.na(ConfirmORCol)){ConfirmORCol<-NULL}
        RunTargeted(MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryddMSPos_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Positive ddMS analysis")
  }
  
  #NEG BY CLASS  
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "NEG" & toupper(LibraryCriteria[,6]) == "TRUE",] #subset LibraryCriteria to find negative libraries
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,4]) == "TRUE",] #subset LibraryCriteria to find ddMS libraries to run
  if(runNegddMS && nrow(LibraryCriteria)>0){
    NegClassDDLib <- TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_NEG, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_NEG,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    for (c in 1:length(ddMS2NEG_in)){
      MS2_dir_in <- paste(fpath, ddMS2NEG_in[c], sep="")
      cat(paste0("Reading in file:\t", ddMS2NEG_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      ExtraSample<-ExtraSampleNameddMSNEG_in[c]
      MS2_df_in <- createddMS2dataFrame(MS2_dir_in)
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in)
      
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";"))) 
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0 || is.na(ConfirmANDCol)){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0 || is.na(ConfirmORCol)){ConfirmORCol<-NULL}
        RunTargeted(MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryddMSNegByClass_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Negative by class ddMS analysis")
  }
  
  
  #POS BY CLASS  
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "POS" & toupper(LibraryCriteria[,6]) == "TRUE",] #subset LibraryCriteria to find positive class libraries
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,4]) == "TRUE",] #subset LibraryCriteria to find ddMS libraries to run
  if(runPosddMS && nrow(LibraryCriteria)>0){
    PosClassDDLib<-TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_POS, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_POS,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    for (c in 1:length(ddMS2POS_in)){
      MS2_dir_in<-paste(fpath, ddMS2POS_in[c],sep="")
      cat(paste0("Reading in file:\t", ddMS2POS_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      ExtraSample<-ExtraSampleNameddMSPOS_in[c]
      MS2_df_in <- createddMS2dataFrame(MS2_dir_in)
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in)
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";"))) 
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0 || is.na(ConfirmANDCol)){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0 || is.na(ConfirmORCol)){ConfirmORCol<-NULL}
        RunTargeted(MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryddMSPosByClass_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Positive by class ddMS analysis")
  }
  
  ####AIF####
  #Neg AIF
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "NEG",] #subset LibraryCriteria to find negative class libraries
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,5]) == "TRUE",] #subset LibraryCriteria to find AIF libraries to run
  if(runNegAIF && nrow(LibraryCriteria)>0){
    NegAIFLib <- TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_NEG, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_NEG,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    #sort AIF files
    AIFMS1NEG_in <- AIFMS1NEG_in[order(AIFMS1NEG_in)]
    AIFMS2NEG_in <- AIFMS2NEG_in[order(AIFMS2NEG_in)]
    for (c in 1:length(AIFMS1NEG_in)){
      MS1_dir_in <- paste(fpath, AIFMS1NEG_in[c], sep="")
      cat(paste0("Reading in file:\t", AIFMS1NEG_in[c],"\nFrom Directory:\t\t", MS1_dir_in,"\n"))
      MS1_df_in <- createMS1dataFrame(MS1_dir_in)
      
      MS2_dir_in <- paste(fpath, AIFMS2NEG_in[c], sep="")
      cat(paste0("Reading in file:\t", AIFMS2NEG_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      MS2_df_in <- createAIFMS2dataFrame(MS2_dir_in)
      
      ExtraSample<-ExtraSampleNameAIFNEG_in[c]
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in, MS1_dir_in)
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";")))
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0 || is.na(ConfirmANDCol)){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0 || is.na(ConfirmORCol)){ConfirmORCol<-NULL}
        RunAIF(MS1_df_in, MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryAIFNeg_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Negative AIF analysis")
  }
  
  #Pos AIF
  LibraryCriteria <- read.csv(LibCriteria) #Read-in Library ID criteria (csv) located in the LibrariesReducedAdducts folder
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,7]) == "POS",] #subset LibraryCriteria to find positive class libraries
  LibraryCriteria <- LibraryCriteria[toupper(LibraryCriteria[,5]) == "TRUE",] #subset LibraryCriteria to find AIF libraries to run
  if(runPosAIF && nrow(LibraryCriteria)>0){
    PosAIFLib <- TRUE
    FeatureTable_dir_in<-paste(fpath, FeatureTable_POS, sep="")
    cat(paste0("Reading in file:\t", FeatureTable_POS,"\nFrom Directory:\t\t", FeatureTable_dir_in,"\n"))
    FeatureList_in <- ReadFeatureTable(FeatureTable_dir_in)
    AIFMS1POS_in <- AIFMS1POS_in[order(AIFMS1POS_in)]
    AIFMS2POS_in <- AIFMS2POS_in[order(AIFMS2POS_in)]
    for (c in 1:length(AIFMS1POS_in)){
      MS1_dir_in <- paste(fpath, AIFMS1POS_in[c], sep="")
      cat(paste0("Reading in file:\t", AIFMS1POS_in[c],"\nFrom Directory:\t\t", MS1_dir_in,"\n"))
      MS1_df_in <- createMS1dataFrame(MS1_dir_in)
      
      MS2_dir_in <- paste(fpath, AIFMS2POS_in[c], sep="")
      cat(paste0("Reading in file:\t", AIFMS2POS_in[c],"\nFrom Directory:\t\t", MS2_dir_in,"\n"))
      MS2_df_in <- createAIFMS2dataFrame(MS2_dir_in)
      
      ExtraSample<-ExtraSampleNameAIFPOS_in[c]
      OutputInfo <- c(MS2_dir_in, ExtraSample, FeatureTable_dir_in, MS1_dir_in)
      for(i in seq_len(nrow(LibraryCriteria))){
        LibraryFile <- paste(InputLibrary, LibraryCriteria[i,1], sep="") #create directory/file of each library
        OutputName <- paste(ExtraSample,"_",gsub('.{4}$', '', LibraryCriteria[i,1]), sep="") #get ms2 name and library name
        ConfirmANDCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,2]), ";")))
        ConfirmORCol <- as.numeric(unlist(strsplit(as.character(LibraryCriteria[i,3]), ";")))
        if(length(ConfirmANDCol)==0){ConfirmANDCol<-NULL}
        if(length(ConfirmORCol)==0){ConfirmORCol<-NULL}
        RunAIF(MS1_df_in, MS2_df_in, FeatureList_in, LibraryFile, ParentMZcol_in, OutputDirectoryAIFPos_in, OutputName, ConfirmORCol, ConfirmANDCol, OutputInfo)
      }
    }
    print("Finished Positive AIF analysis")
  }
  
  #Compilation/ID code for reduced confirmed files
  if(runPosAIF || runPosddMS){
    print("Creating Identifications for Positive Mode")
  }
  if(runPosddMS & !runPosAIF){
    ddMS2directory<-paste(OutputDirectoryddMSPos_in,"Confirmed_Lipids\\", sep="")
    Classdirectory<-paste(OutputDirectoryddMSPosByClass_in,"Confirmed_Lipids\\", sep="")
    AIFdirectory<-"Nothing"
    CreateIDs(paste(fpath,FeatureTable_POS,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibPOS, OutputDirectory, PosDDLib, PosClassDDLib, PosAIFLib, "Pos")
  }
  
  if(runPosAIF & runPosddMS){
    ddMS2directory<-paste(OutputDirectoryddMSPos_in,"Confirmed_Lipids\\", sep="")
    Classdirectory<-paste(OutputDirectoryddMSPosByClass_in,"Confirmed_Lipids\\", sep="")
    AIFdirectory<-paste(OutputDirectoryAIFPos_in,"Confirmed_Lipids\\", sep="")
    CreateIDs(paste(fpath,FeatureTable_POS,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibPOS, OutputDirectory, PosDDLib, PosClassDDLib, PosAIFLib, "Pos")
  }
  
  if(runPosAIF & !runPosddMS){
    ddMS2directory<-"Nothing"
    Classdirectory<-"Nothing"
    AIFdirectory<-paste(OutputDirectoryAIFPos_in,"Confirmed_Lipids\\", sep="")
    CreateIDs(paste(fpath,FeatureTable_POS,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibPOS, OutputDirectory, PosDDLib, PosClassDDLib, PosAIFLib, "Pos")
  }
  
  if(runNegAIF || runNegddMS){
    print("Creating Identifications for Negative Mode")  
  }
  
  if(runNegAIF & !runNegddMS){
    ddMS2directory <- "Nothing"
    Classdirectory <- "Nothing"
    AIFdirectory <- paste(OutputDirectoryAIFNeg_in,"Confirmed_Lipids\\", sep="")
    CreateIDs(paste(fpath,FeatureTable_NEG,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibNEG, OutputDirectory, NegDDLib, NegClassDDLib, NegAIFLib, "Neg")
  }
  
  if(runNegddMS & !runNegAIF){
    ddMS2directory <- paste(OutputDirectoryddMSNeg_in,"Confirmed_Lipids\\", sep="")
    Classdirectory <- paste(OutputDirectoryddMSNegByClass_in,"Confirmed_Lipids\\", sep="")
    AIFdirectory <- "Nothing"
    CreateIDs(paste(fpath,FeatureTable_NEG,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibNEG, OutputDirectory, NegDDLib, NegClassDDLib, NegAIFLib, "Neg")
  }
  
  if(runNegddMS & runNegAIF){
    ddMS2directory <- paste(OutputDirectoryddMSNeg_in,"Confirmed_Lipids\\", sep="")
    Classdirectory <- paste(OutputDirectoryddMSNegByClass_in,"Confirmed_Lipids\\", sep="")
    AIFdirectory <- paste(OutputDirectoryAIFNeg_in,"Confirmed_Lipids\\", sep="")
    CreateIDs(paste(fpath,FeatureTable_NEG,sep=""), ddMS2directory, Classdirectory, AIFdirectory, ImportLibNEG, OutputDirectory, NegDDLib, NegClassDDLib, NegAIFLib, "Neg")
  }
  
}#end folder loop

options(warn=0)#suppress warning off

Rversion<-(paste("ERROR:R version must be equal to, or between, 2.0.3 and 3.3.3. Please download 3.3.3. You are using version: ", paste(version$major,version$minor,sep=".")))
OutputRemoval<-paste("ERROR: Remove your 'Output' folder from the current Input Directory: ", InputDirectory)


#DEBUG CreateIDs
# ddMS2directory<-"Nothing"
# Classdirectory<-"Nothing"
# AIFdirectory<-paste(OutputDirectoryAIFPos_in,"Confirmed_Lipids\\", sep="")
# PeakTableDirectory <- paste(fpath,FeatureTable_POS,sep="")
# ddMS2directory <- ddMS2directory
# Classdirectory <- Classdirectory
# AIFdirectory <- AIFdirectory
# ImportLib <- ImportLibPOS
# OutputDirectory <- OutputDirectory
# ddMS2 <- PosDDLib
# ddMS2Class <- PosClassDDLib
# AIF <- PosAIFLib
# mode <- "Pos"

#CreateIDs(paste(fpath,FeatureTable_POS,sep=""), ddMS2directory, Classdirectory, 
#AIFdirectory, ImportLibPOS, OutputDirectory, PosDDLib, PosClassDDLib, PosAIFLib, "Pos")

#Debug AIF
# ms1_df<-MS1_df_in
# ms2_df<-MS2_df_in
# FeatureList<-FeatureList_in
# LibraryLipid_self<-LibraryFile
# ParentMZcol<-ParentMZcol_in
# OutputDirectory<-OutputDirectoryAIFNeg_in
# ExtraFileNameInfo<-OutputName
# ConfirmORcol<-ConfirmORCol
# ConfirmANDcol<-ConfirmANDCol

if(!((as.numeric(paste(version$major,version$minor,sep=""))>=20.3) && (as.numeric(paste(version$major,version$minor,sep=""))<=33.3))) {
  Rversion
}

if(ErrorOutput==1){
  OutputRemoval
}
