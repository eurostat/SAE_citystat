#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) for city statistics and other functional areas
# part II  
#
# R-Code:
# Coordinate execution of AROPE SAE example scripts
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, Simon Lenau, Julia Manecke,
# Hariolf Merkle, Jan Weymeirsch
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Initial Environment checks -------------------------------------------------
rm(list=ls())
cat("\n\nChecking if scripts are present...\n")
# list of expected script names
script_names <- c("01_Population_Module.R", "02_Sampling_Module.R",
                  "03_SAE_DataPreparation.R", "04_SAE_SampleEstimation.R",
                  "05_SAE_EstimationFunctions.R")
# set working directory where this file (00_MAIN.R) is located
if (!grepl("r_code$",getwd())) setwd("r_code")
# list of files in current working directory
wd_files <- list.files("./")
# vector of missing scripts
script_nexist <- !(script_names %in% wd_files)
if (any(script_nexist)){
  msg <- paste0("\n\nERROR!\n",
                "Following scripts are missing from the current directory:",
                "\n\n",
                "- Working Directory:\n",
                getwd(), "\n\n",
                "- Missing scripts:\n",
                paste(script_names[script_nexist], collapse = "\n"), "\n\n",
                "Please make sure, that all scripts are present in the\n",
                "current working directory or set your working directory\n",
                "to an appropriate path using setwd() and re-run this\n",
                "script.\n"
  )
  cat(msg)
} else {
  cat("All relevant scripts are present. Please continue.\n")
}

# Warning before Running rest of the script ------------------------------------
cat(paste0("ATTENTION:\nPlease run this script line by line, or else ",
           "interactivity is broken.\n")); Sys.sleep(10)

# Setting versioning parameters ------------------------------------------------

# AMELIA version
aver <- "v0.2.3"

# Execute Scripts --------------------------------------------------------------

# 01 Population Module ------------------------------------
cat(paste0("*** Running ", getwd(), "/01_Population_Module.R\n"))
source("./01_Population_Module.R")

s1 <- script_01_Population_Module()

for (i in names(s1)) if (!is.na(i)) assign(i, s1[[i]]); rm("s1")
cat("\nEOF: 02_Population_Module.R\n\n")

# 02 Sampling Module --------------------------------------
cat(paste0("*** Running ", getwd(), "/02_Sampling_Module.R\n"))
source("./02_Sampling_Module.R")

s2 <- script_02_Sampling_Module()

for (i in names(s2)) if (!is.na(i)) assign(i, s2[[i]]); rm("s2")
cat("\nEOF: 02_Sampling_Module.R\n\n")

# 03 SAE Data Preparation ---------------------------------
cat(paste0("*** Running ", getwd(), "/03_SAE_DataPreparation.R\n"))
source("./03_SAE_DataPreparation.R")

s3 <- script_03_SAE_DataPreparation()

for (i in names(s3)) if (!is.na(i)) assign(i, s3[[i]]); rm("s3")
cat("\nEOF: 03_SAE_DataPreparation.R\n\n")

# 04 SAE Sample Estimation --------------------------------
cat(paste0("*** Running ", getwd(), "/04_SAE_SampleEstimation.R\n",
           "    this will also execute", getwd(),
           "05/_SAE_EstimationFunctions.R\n"))
source("./04_SAE_SampleEstimation.R")

s4 <- script_04_SAE_SampleEstimation()

for (i in names(s4)) if (!is.na(i)) assign(i, s4[[i]]); rm("s4")
cat("\nEOF: 04_SAE_SampleEstimation.R\n\n")

# EOF 00_MAIN.R
