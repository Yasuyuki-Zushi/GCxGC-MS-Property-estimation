###Input the folder location of "GCxGC-MS-Property-estimation_v1.1.0", including itself.
Folder_location <-"C:/....../GCxGC-MS-Property-estimation_v1.1.0"


###Name the input measurement file (cdf)
Input_measuremet_data <- "GCxGC_measurement_testdata.cdf"


###Skip the following installation code, if the following packages have been installed.
#install.packages("plotrix")
#install.packages("pracma")
#install.packages("RNetCDF")


###Not required to change.
inputDrive <- paste(Folder_location, "/Input",sep="")
outputDrive <- paste(Folder_location, "/Output",sep="")
codeDrive <- paste(Folder_location, "/Code",sep="")
TempDrive <- paste(Folder_location, "/Output/Output_temp",sep="")


###Parameter calibration?: "Yes" or "No". 
###If "No", "default.alpha.csv" and ramda in "default.prop_coef.csv" is used.
###If "Yes", parameter calibration is performed based on RTs in "Chemical_list_for_LFER_Caliration.csv".
parameter_calibration <- "Yes"


###Choose column type used for the measurement from the file "Column_System_Parameter.csv" in input folder.
first.column <- "SE-30"    #polydimethyl-siloxane = 100
second.column <- "BPX50"   #polydimethyl-siloxane:diphenyl-siloxane = 50:50


###Parameters of measurement data
Modulation_Period  <- 20
Sampling_Rate <- 13.549952
Phase_Shift <- 9
Initial_RT <- 7.338950


###Property domain to map on GCxGC-MS chromatogram. See below for available items.
#"ATBP":    Aquatic and Terrestrial Bioaccumulation Potential
#"LRTP":    Long Range Transport Potential
#"HBAP":    Human bioaccumulation potential
#"TBP":     Terrestrial Bioaccumulation Potential
#"ACP":     Arctic Contamination Potential
#"ABP":     Aquatic Bioaccumulation Potential
#"BAP":     Bioaccumulation Potential
#"ABP&ACP": Combination of ABP and ACP. Other combination of the above list is available.

domain.name <- "ABP"


###############################################################################################
###############################################################################################
###Run the property estimation code start###

setwd(codeDrive)
source("GCxGC_propety_estimation_main.r")

###Run the property estimation code end###
###############################################################################################
###############################################################################################
