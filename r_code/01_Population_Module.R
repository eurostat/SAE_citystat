#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) 
# for city statistics 
# and other functional areas part II  
#
# R-Code:
# Generation of population data
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, 
# Simon Lenau, Julia Manecke, Hariolf Merkle
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Code explanation: Population_Module ------------------------------------------
#   
#   The following R-script is an example code. 
#   It ilustrates the generation of the population data set,
#   consisting of the AMELIA variables
#   
#   PROV    (Regional identifier: NUTS2)
#   DIS     (Regional identifier: District)
#   CIT     (Regional identifier: City/Community)
#   DOU     (Degree of urbanisation of CIT)
#   HID     (Household ID)
#   PID     (Personal ID)
#   AGE     (Age)
#   EDI     (Equalized disposable (household) income)
#   
#   And derived variables
#   
#   inc_c   (edi split at deciles)
#   age_c   (age split at 11 equidistant points)
#   
#   The population dataset is stored for faster
#   initialization of subsequent simulation steps
#   
#-------------------------------------------------------------------------------

script_01_Population_Module <- function(){

# Paths ------------------------------------------------------------------------
#
# set paths
path             <- getwd()
data.path        <- paste0(path,"/AMELIA_Data/")
output.path      <- paste0(path,"/AMELIA_samples/", "City_Statistics_II/",
                           "Population_Data/")
data.output.file <- paste0(output.path, "Simulation_Universe.RData")

# set version if not set already
if (!try(length(aver), silent = TRUE) < 1 && TRUE) aver <- "v0.2.3"
# afolder <- c(paste0("AMELIA_HH_level_", aver),
#              paste0("AMELIA_P_level_", aver))


# create directories
dirs <- list.dirs(path = getwd(), full.names = TRUE)
#for(lev in afolder){
  for (p in c(data.path, output.path)){
    if (!(p %in% dirs)){
      P <- paste0(p)
      for (pp in P){
        cat(paste("Creating directory:\n", pp, "\n"))
        dir.create(pp, showWarnings = FALSE, recursive = TRUE)
      }
    }
  }
#}


# Packages ---------------------------------------------------------------------

if (!require(data.table)){ install.packages("data.table"); library(data.table) }

# Data -------------------------------------------------------------------------

cat("\n\nChecking if data is present...\n")
# list variables that will be loaded
variables <- c("PROV", "DIS", "CIT", "DOU", "HID", "PID", "AGE", "EDI")
# list of files in current working directory
wd_files <- list.files(paste0(data.path, "AMELIA_P_level_", aver),
                       pattern = ".RData")
# check which variables are present
load.files <- lapply(X = variables, FUN = function(x){
                         grep(x = wd_files, pattern = x, ignore.case = TRUE,
                              value = TRUE)
                    })
miss_files <- sapply(X = load.files, FUN = length) < 1

if (any(miss_files)){
    msg <- paste0("\n\nERROR!\n",
                  "Following data sets are missing from the current directory:",
                  "\n\n",
                  "- Working Directory:\n",
                  getwd(), "\n\n",
                  "- Missing data sets with variables:\n",
                  paste(variables[miss_files], collapse = "\n"), "\n\n",
                  "Please make sure, that all data files are present in the\n",
                  "current working directory or set your working directory\n",
                  "to an appropriate path using setwd() and re-run this\n",
                  "script.\n"
                 )
    cat(msg, "\n\n")
} else {
    cat("Data is present!\n")
}
# Offer to download AMELIA
if(any(miss_files)) {
    user_q <- paste0("Do you want to download a complete version of AMELIA?",
                     "[Y/n] \n")
    if (!interactive()) {
        cat(user_q)
        user_input <- readLines("stdin", n = 1) 
    } else {
        user_input <- readline(user_q)
    }
  
    if (user_input %in% c("y", "Y", "yes", "YES", "Yes")){
        for (lvl in c("P", "HH")){
            amelia.file <- paste0(data.path, "AMELIA_", lvl,
                                  "_level_", aver, ".zip")
            cat(paste("Downloading", lvl, "data...\n"))
            download.file(url = paste0("http://amelia.uni-trier.de/",
                                       "wp-content/", "uploads/", "2019/",
                                       "12/", "AMELIA_", lvl,
                                       "_level_", aver, ".zip"),
                          destfile = amelia.file)
            cat(paste("Extracting", lvl, "data...\n"))
            unzip(amelia.file, exdir = paste0(data.path, "AMELIA_", lvl,
                                                                "_level_", aver),
                  junkpaths = TRUE)
        }
    } else {
        stop("Aborting, as AMELIA input files are missing!\n")
    }
  
    amelia.files <- list.files(data.path, recursive = TRUE, pattern = ".RData")
    load.files <- lapply(X = variables, FUN = function(x){
                             grep(x = amelia.files, pattern = x,
                                  ignore.case = TRUE, value = TRUE)
                        })
    miss.files <- sapply(X = load.files, FUN = length) < 1
  
    if(any(miss.files)){
        stop(paste0("[ERR] Something went wrong with downloading the data.\n",
                    "Please download it manually from:\n",
                    "http://amelia.uni-trier.de/", "wp-content/", "uploads/",
                    "2019/", "12/", "AMELIA_P_level_", aver, "zip","\n",
                    "to the directory:\n", data.path, "\n\n"))  
    }
} 

cat("Loading data...\n")
data <- lapply(X = grep(x = unlist(load.files), pattern = "PAML", value = TRUE),
               FUN = function(x){
                   get(load(paste0(data.path, #"AMELIA_P_level_", aver, "/", 
                                   x)))
              })
setDT(data)
setnames(data, names(data), tolower(variables))

# Restructuring ----------------------------------------------------------------

inc_q <- quantile(data$edi, p = (0:10) / 10)

data[, inc_c:= cut(edi, breaks = inc_q, include.lowest = TRUE,
                    labels = FALSE)]
data[, age_c:= cut(age, breaks = 10, include.lowest = TRUE, labels = FALSE)]
#data[, c("age","edi"):= NULL]

setcolorder(x = data, neworder = tolower(variables[1:5]))
setkeyv(x = data, cols = tolower(variables[1:5]))

data <- data[order(data$pid),]

output_set <- try(mode(data.output.file), silent = TRUE)
if (is(output_set, "try-error")){
  data.output.file <- paste0(getwd(), "population.RData")
}

cat(paste("Writing to", data.output.file, "!\n"))
save(data, file = data.output.file)

# return *everything*
curobj <- ls()
ret <- lapply(X = curobj, FUN = function(x) eval(parse(text = x)))
names(ret) <- curobj
return(ret)
}

# EOF 01_Population_Module.R
