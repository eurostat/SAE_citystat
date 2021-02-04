#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) 
# for city statistics 
# and other functional areas part II  
#
# R-Code:
# Examples for sampling according to the used designs
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, 
# Simon Lenau, Julia Manecke, Hariolf Merkle
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Code explanation -------------------------------------------------------------
#   
#   The following R-script is an example code. 
#   It ilustrates the generation of the samples from the various designs
#   used throughout the simulation study.
#   
#   The actual sampling for the simulation was carried out
#   on a large-scale server infrastructure with a special queueing system,
#   which, in general, is not available to standard users.
#   
#   The sampling designs cover a range of realistic scenarios 
#   described in  Chapter 3 ('Sampling designs') of the deliverable.
#   They were derived from an overview (mainly of) the designs used in
#   the EU-SILC and LFS.
#   
#   The variations of the STSI_H, TS_H and TS_P designs just require 
#   modifications of the variables 
#   'strata.variables', 'psu.variable' and 'ssu.variable'
#   in the following code
#
#   -----------------------------------------------------------------------
#   Name     Stage 1             |          Stage 2 
#             PSU   Strata       | fraction   SSU     fraction
#   -----------------------------------------------------------------------
#   SRS_H     HID   –            | 0.16%     –       –
#   SRS_P     PID   –            | 0.16%     –       –
#   STSI_H1   HID   PROV         | 0.16%     –       –
#   STSI_H2   HID   DOU          | 0.16%     –       –
#   STSI_H3   HID   PROV x INC_C | 0.16%     –       –
#   STSI_H4   HID   DIST x DOU   | 0.16%     –       –
#   STSI_P    PID   AGE_C        | 0.16%     –       –
#   TS_H1     CIT   PROV         |16.00%     HID     1%
#   TS_H2     CIT   PROV x DOU   |16.00%     HID     1%
#   TS_H3     CITG  PROV         |16.00%     HID     1%
#   TS_P1     CIT   PROV x DOU   |16.00%     PID     1%
#   TS_P2     DIST  PROV         |16.00%     PID     1%
#   -----------------------------------------------------------------------

#-------------------------------------------------------------------------------

script_02_Sampling_Module <- function(){

# Load required packages -------------------------------------------------------

if (!require(data.table)){ install.packages("data.table"); library(data.table) }

# Define Functions -------------------------------------------------------------
status.bar <- function(i, I, c80 = TRUE, perc = TRUE, stepsize = 1, pch = "."){

    # set modulo according to whether percentages shall be shown or not
    if (perc) mod <- 75 else mod <- 80

    # calculate modulo for column-80-rule
    if (c80) m80 <- i %% (mod * stepsize) else m80 <- -1

    # if new line starts, print percentage
    if (m80 == 1){
        ## calculate current progress in percent
        ip <- round((i / I) * 100)
        ## fill up chars until status is 3 characters long
        while (nchar(ip) < 3) ip <- paste0(" ", ip)
        ## finally, print percentage
        cat(paste0(ip, "% "))
    }

    # for every step, print a dot
    if (i %% stepsize == 0) cat(pch)

    # if a column is full, do a line-break (column-80-rule)
    if (m80 == 0) cat("\n")

}

# Number of samples for simulation ---------------------------------------------
# Define the number of samples to be drawn in the simulation study

cat(paste0("ATTENTION:\n", "Precision of estimators and thus of the simulation",
           " highly depend on the number of iterations and thus on the number",
           " of samples drawn. An increased number of iterations also",
           " increases the computation time SUBSTANTIALLY!\n\n",
           "In order to derive precise estimators,",
           " a minimum of 1,000 iterations is recommended.",
           " However, computation may take hours, if not days,",
           " depending on the underlying hardware.\n\n",
           "For the sake of reproducability, only a small number of iterations",
           " is therefore recommended in this case.\n"))

R <- readline("How many iterations do you want to carry out? [20] \n")

R <- as.integer(R)
if (is.na(R) | try(R < 1, silent = TRUE)){
    R <- 20
    cat("Assuming 20 samples to be drawn...\n")
} else {
    cat(paste("Drawing", R, "samples...\n"))
}

# Sampling fractions -----------------------------------------------------------
# Define sampling fractions for first / second / single stage
fr_1 <- (16 / 100)
fr_2 <- (1 / 100)
fr <- (fr_1 * fr_2)

# Paths & Files ----------------------------------------------------------------
data.path <- paste0(getwd(), "/AMELIA_samples/", "City_Statistics_II/",
                    "Population_Data/")
function.path <- paste0(getwd(), "/Functions/", "Sampling/")



cat("\n\nChecking if sampling functions are present...\n")
# list of expected script names
funct_names <- c("SRS_Function.R", "STSI_Function.R", "TS_Function.R")
# list of files in current working directory
miss_funct <- list.files(function.path)
# vector of missing scripts
funct_nexist <- !(funct_names %in% miss_funct)
if (any(funct_nexist)){
  msg <- paste0("\n\nERROR!\n",
                "Following scripts are missing from the following subdirectory:",
                "\n\n",
                "- Subdirectory:\n",
                function.path, "\n\n",
                "- Missing scripts:\n",
                paste(funct_names[funct_nexist], collapse = "\n"), "\n\n",
                "Please make sure that the subdirectory exists and\n",
                "that it contains the listed scripts. Then, re-run this\n",
                "script.\n"
  )
  cat(msg)
} else {
  cat("All relevant scripts are present. Please continue.\n")
}

source(paste0(function.path, "SRS_Function.R"))
source(paste0(function.path, "STSI_Function.R"))
source(paste0(function.path, "TS_Function.R"))

# Define sampling designs ------------------------------------------------------
designs <- data.table(
  design.id    = c("SRS_H", "SRS_P", "STSI_H1", "STSI_H2", "STSI_H3", "STSI_H4",
                   "STSI_P", "TS_H1", "TS_H2", "TS_H3", "TS_P1", "TS_P2"),
  strata       = tolower(c(NA, NA, "c('PROV')", "c('DOU')", "c('PROV','INC_C')",
                           "c('DIS','DOU')", "c('AGE_C')", "c('PROV')",
                           "c('PROV','DOU')", "c('PROV')", "c('PROV','DOU')",
                           "c('PROV')")),
  psu          = tolower(c("HID", "PID", "HID", "HID", "HID", "HID", "PID",
                           "CIT", "CIT", "CITG", "CIT", "DIS")),
  ssu          = tolower(c(NA, NA, NA, NA, NA, NA, NA, "HID", "HID", "HID",
                           "PID", "PID")),
  fraction.psu = c(rep(x = (fr_1 / 100), times = 7),
                   rep(x = fr_1, times = 5)),
  fraction.ssu = c(rep(x = NA, times = 7),
                   rep(x = (1 / 100), times = 5))
)

designs[ , sampling_function:=tolower(paste0(strsplit(design.id, "_")[[1]][1],
                                               "_function")), by = "design.id"]
designs[ , out.path:=paste0(dirname(data.path), "/Samples/" ,design.id, "/")]

designs <- designs[-10, ] # The CITG variable is not yet publicly available


invisible(designs[ , dir.create(out.path, showWarnings = FALSE,
                                recursive = TRUE), by = "design.id"])

# Load population dataset ------------------------------------------------------

load(paste0(data.path, "Simulation_Universe.RData"))

status_count <- 0
for (r in 1:R) {


    sample_no <- formatC(as.numeric(r), width = nchar(R), format = "d",
                         flag = "0")
  
    for (design_no in 1:nrow(designs)){

        # status message
        status_count <- status_count + 1
        status.bar(i = status_count, I = R * nrow(designs))

        sampling.function <- get(designs[design_no, ]$sampling_function)
    
        smp <- sampling.function(population.data =  data,
                                 strata.variables = eval(
                                    parse(text = designs[design_no, ]$strata)),
                                 psu.variable  = designs[design_no, ]$psu,
                                 ssu.variable  = designs[design_no, ]$ssu,
                                 sampling.fraction1  = designs[design_no,
                                                               ]$fraction.psu,
                                 sampling.fraction2  = designs[design_no,
                                                               ]$fraction.ssu
                                )
    
    save(smp, file = paste0(designs[design_no, ]$out.path, "Sample_", sample_no,
                            ".RData")
        )
    
    #cat("Iteration ", sample_no, " of ", R, " design: ",
    #    designs[design_no, ]$design.id, "\n")
    
  }
}

# return *everything*
curobj <- ls()
ret <- lapply(X = curobj, FUN = function(x) eval(parse(text = x)))
names(ret) <- curobj
return(ret)
}

# EOF 02_Sampling_Module.R
