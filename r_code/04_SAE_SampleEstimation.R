#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) 
# for city statistics 
# and other functional areas part II  
#
# R-Code:
# Sample Preparation and Estimation
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, 
# Simon Lenau, Julia Manecke, Hariolf Merkle
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Code explanation -------------------------------------------------------------
#
# This code consists of a sample preparation stage and an estimation stage. 
#
# Within the sample preparation stage, the extraction of a single sample from 
# the simulation data as well as the relevant information on the respective 
# sampling design (variables for strata/clusters, sampling fractions, 
# allocation) are demonstrated using one of the generated sampling designs. 
# It constructs the survey-design-object (according to the R-Package "survey") 
# and estimates the AROPE-threshold based on the sample. Based on the 
# AROPE-threshold, the variable of interest is derived for the sampling units.
#
# Within the estimation stage, the parameters of interest at municipality- and 
# city-level are estimated using the small area estimation strategies described 
# in Deliverable 3. The functions applied within this are contained in the file 
# SAE_EstimationFunctions.R.
#
#-------------------------------------------------------------------------------

script_04_SAE_SampleEstimation <- function(){

# Loading necessary R-packages -------------------------------------------------

if (!require(data.table)){ install.packages("data.table"); library(data.table) }
if (!require(survey)){ install.packages("survey"); library(survey) }
if (!require(vardpoor)){ install.packages("vardpoor"); library(vardpoor) }
if (!require(sae)){ install.packages("sae"); library(sae) }
if (!require(convey)){ install.packages("convey"); library(convey) }
if (!require(arm)){ install.packages("arm"); library(arm) }
options("survey.lonely.psu" = "remove")

# Paths ------------------------------------------------------------------------

path             <- getwd()
data.path        <- paste0(path,"/AMELIA_Data/")
output.path      <- paste0(path,"/AMELIA_samples/", "City_Statistics_II/",
                           "Population_Data/")

# set version if not set already
if (!try(length(aver), silent = TRUE) < 1 && TRUE) aver <- "v0.2.3"

# Load Data if necessary -------------------------------------------------------

cat("Testing if data is present...\n")
data_present <- try(mode(c(data_P, data_H, Clus_data)), silent = TRUE)
if (is(object = data_present, class2 = "try-error")){
    cat(paste0("Some or all of the needed objects have not been loaded yet:\n",
               "- data_P\n- data_H\n\nTrying to load them from:\n",
               getwd(), "/data_prepared.RData\n\n",
               "Please make sure, this file is present!\n"
               )
       )
    readline("Press [ENTER] to continue...")
    load("./data_prepared.RData")
} else {
    cat("Data is present!\n")
}


# Generation of Array with Estimates
# Dimension: R (number of iterations) x 
#            no_CITL (number of areas 'CITL') x 
#            no_Est (number of estimation approaches) x 
#            no_Des (number of sampling designs)
no_CITL <- nrow(data_CITL)
no_Est <- 8
no_Des <- nrow(designs)

EstCITL <- array(NA, dim=c(R, no_CITL, no_Est, no_Des),
                 dimnames = list(1:R,
                                 row.names(data_CITL),
                                 c("HT_N", "HT01", "HT_w", "CL", "GREG","BHF",
                                   "FH", "YL"),
                                 designs$design.id
                 )
)

source("./05_SAE_EstimationFunctions.R")


designs$SampUn <- ifelse(is.element(designs$psu, "hid") | 
                            is.element(designs$ssu, "hid"),
                          "H", "P")

for(DesNo in 1:nrow(designs)){
  
  cat(" Design: ",
      designs[DesNo, ]$design.id, "\n")
  
  
  DesID <- designs$design.id[DesNo]
  SampUn <- designs[designs$design.id == DesID,]$SampUn

  if (SampUn == "H"){
    data <- data_H
    Nh_Np_CITL <- table(data$CITL) /
      tapply(X = data$HHS, INDEX = data$CITL, FUN = sum)
  } else {
    data <- data_P
    Var <- c("AGE", "BAS", "COB")
    for (v in 1:length(Var)){
      load(paste0(data.path, "AMELIA_P_level_", aver, "/",
                  "PAML.", Var[v], "_", aver, ".RData"))
    }
    
    data <- cbind.data.frame(data, AGE, BAS, COB)
    data$BAS1  <- as.numeric(data$BAS == 1)
    data$BAS3  <- as.numeric(data$BAS == 3)
    data$AGE65 <- as.numeric(data$AGE >= 65)
    data$COB23 <- as.numeric(data$COB != 1)
  }
  
  # WORKAROUND: sometimes variables can be duplicated. Remove them!
  dupl <- which(duplicated(names(data)))
  if (length(dupl) > 0){
    data <- as.data.table(as.data.frame(data)[, -dupl])
  }
  
  # Assignment of units to clusters previously defined according to age and
  # gender structures
  data$Clus <- factor(Clus_data$cl[data$CITL])
  data$cit <- factor(data$cit)
  data$id <- ifelse(test = rep(SampUn == "H", length = nrow(data)),
                    yes = data$hid, no = data$pid)
  
  Clus_CITL <- Clus_data$cl
  
  # 1) Sample Preparation -----------------------------------
  
  #cat("Sample Preparations...\n")
  
  # Loading R different samples according to Design "DesID"
  #files <- paste0("Sample_", c(paste0(0, 1:9), 10), ".RData")
  files <- list.files(designs[designs$design.id == DesID, ]$out.path)
  #files <- list.files(path = paste0(dirname(data.path), "/Samples/"),
  #                    recursive = TRUE, full.names = TRUE, pattern = ".RData")
  
  for (i in 1:length(files)){
    load(paste0(designs[designs$design.id == DesID, ]$out.path, files[i]))
    # combine sample data sets to 1 data.frame()
    smp$smp_no <- i
    if (i == 1){
      samples <- smp
    } else {
      samples <- rbind(samples, smp)
    }
  }
  # 'samples' now contains 'R' samples stored below each other
  
  # Definition of necessary sampling information (variables for strata/clusters, 
  # sampling fractions, allocation) of the respective design
  design <- attr(smp, "design")
  
  if (sum(colnames(samples) == "cit") != 0) {
    samples$cit <- factor(samples$cit, levels = levels(data$cit))
  }
  
  # Merging sample data and simulationd dataset by design variables
  sample_data <- data.table(merge(x = samples, y = data,
                                #by = as.character(design$variable),
                                  key = FALSE, all.x = TRUE
  )
  )
  
  
  
  # Computation of overall design weights
  sample_data[ , weight := 1]
  for (dv in design$variable) {
    sample_data[ , weight := (weight / get(paste0("pi_", dv)))]
  }
  
  # Computation of person-specific weights
  # Household designs: household weight * household size
  sample_data$weight_Pers <- #
    ifelse(test = rep(SampUn == "H", length = nrow(sample_data)),
           yes = (sample_data$weight * sample_data$HHS),
           no = sample_data$weight)
  
  p_fun <- function(...){ paste(..., sep = "_") }
  
  # Extraction of strata and cluster variables
  design <- data.table(design)
  strata <- design[sampling.fraction == 1, ]$variable
  cluster <- design[sampling.fraction < 1, ]$variable
  
  
  for(s in 1:R){
    cat(" - Iteration ", s, " of ", R, " (Design: ",
        designs[DesNo, ]$design.id, ")", "\n")
    
    
    # As 'samples' contains in total 'R' samples and these samples shall be 
    # analysed individually, 'samples' is now reduced to each of the 'R' samples
    # subsequently
    
    # Generation of a TRUE/FALSE-Vector indicating which of the elements in 
    # 'samples' belongs to the sample drawn in iteration 's'
    smp_no <-samples$smp_no == unique(samples$smp_no)[s]
    
    # Generation of sample-dataset drawn in iteration 's'
    Sample <- sample_data[sample_data$smp_no == unique(samples$smp_no)[s], ]
    Sample <- Sample[order(id)]
    
    # Formatting sample design information for survey-design-object
    
    if(length(strata) == 0){
      Sample[ , strata_combination := 1]
    } else {
      Sample[ , strata_combination := Reduce(p_fun,.SD),
              .SDcols = as.character(strata)]
    }
    
    cluster.formula <- if (length(cluster) > 0){
      as.formula(paste0("~", paste0(cluster, collapse = "+")))
    } else {
      NULL
    }
    
    fpc.formula <- as.formula(paste0("~", paste0("pi_", cluster,
                                                 collapse = "+")))
    
    # Generation of survey-design-object with person-level weights 
    # (needed for computation of the ARP-Threshold)
    
    design <- svydesign(ids = cluster.formula,
                        strata = ~ Sample$strata_combination, 
                        weights = ~ weight_Pers, 
                        fpc = fpc.formula, 
                        data = Sample,
                        nest = TRUE
    )
    
    try(design <- convey_prep(design))
    
    # Estimation of the ARP-threshold
    arpt <- svyarpt(~ edi, design = design)
    
    
    # AROPE indicator variable at level of final sampling units
    # Attention: ESTIMATED ARP-threshold is used
    Sample$AROPE_Est <- ifelse(test = rowSums(cbind(as.numeric(Sample$edi <= arpt),
                                                    Sample$LWI, Sample$MD)) == 0,
                               yes = 0, no = 1)
    
    # Number of estimated persons affected by AROPE at level of final sampling
    # units If households are final sampling units: estimated AROPE indicator *
    # household size
    Sample$y <- ifelse(test = rep(SampUn == "H", length = nrow(Sample)),
                       yes = (Sample$AROPE_Est * Sample$HHS), no = Sample$AROPE_Est)
    
    #Sample$dou <- Sample$dou.x
    #Sample$age_c <- Sample$age_c.x
  
    # 2) Estimation -------------------------------------------
    # a) HT -------------------------------
    
    EstCITL[s,,"HT_N",DesNo] <- HT_N.fkt(Sample,"CITL", SampUn, data)
    EstCITL[s,,"HT01",DesNo] <- ifelse(test = EstCITL[s,,"HT_N",DesNo] > 1, yes = 1,
                               no = EstCITL[s,,"HT_N",DesNo])
    EstCITL[s,,"HT_w",DesNo] <- HT_w.fkt(Sample, "CITL")
    
    # b) Clustering -----------------------
    
    EstCITL[s,,"CL",DesNo] <- Clus.fkt(Sample, "Clus", Clus_CITL)
    
    
    # c) GREG -----------------------------
    
    if (SampUn == "H"){
      EstCITL[s,,"GREG",DesNo] <- GREG.fkt(y ~ BAS1_Abs + BAS3_Abs + AGE65_Abs + 
                                       factor(REG) + factor(dou),
                                     Sample, data, SampUn, "CITL") * Nh_Np_CITL
    } else {
      EstCITL[s,,"GREG",DesNo] <- GREG.fkt(y ~ factor(BAS) + factor(age) +
                                       factor(REG) + factor(dou), Sample, data,
                                     SampUn, "CITL", "probit")
    }
    
    
    # d) BHF ------------------------------
    
    if(SampUn == "H"){
      EstCITL[s,,"BHF",DesNo] <- BHF.fkt(y ~ BAS1_Abs + BAS3_Abs + AGE65_Abs +
                                     COB23_H_Abs, Sample, data, SampUn, "CITL") *
        Nh_Np_CITL
    } else {
      EstCITL[s,,"BHF",DesNo] <- BHF.fkt(y ~ BAS1 + BAS3 + AGE65 + COB23,
                                         Sample, data, SampUn, "CITL") 
    }
    
    # e) FH -------------------------------
    
    # FH --> Estimation of variance of HT estimator necessary
    AROPE_pest <- rep(NA, length(sort(unique(data$CITL))))
    AROPE_vest <- rep(NA, length(sort(unique(data$CITL))))
    names(AROPE_pest) <- levels(data$CITL)
    names(AROPE_vest) <- levels(data$CITL)
    
    DOI_KEEP <- levels(design$variables$CITL)[table(design$variables$CITL) > 1]
    
    for (h in DOI_KEEP){
      design_sub <- design[design$variables$CITL == h, ]
      Res <- try(svyaroper(inc.formula = ~ edi, 
                           lwi.formula = ~ LWI, 
                           dep.formula = ~ MD, 
                           design = design_sub,
                           gamod = FALSE),
                 silent = TRUE)
      
      if (sum(class(Res) == "try-error") == 1){
        AROPE_pest[h] <- NA
        AROPE_vest[h] <- NA
      } else {
        AROPE_pest[h] <- as.vector(Res)
        AROPE_vest[h] <- as.vector(attr(Res, "var"))
      }
    }
    
    data_CITL$AROPE_pest <- AROPE_pest
    data_CITL$AROPE_vest <- AROPE_vest
    
    EstCITL[s,,"FH",DesNo] <- FH.fkt(AROPE_pest ~ ISCED56 + UER + COB_LOC + Rent
                                     + SUP + u20 +  as.factor(REG), data_CITL,
                                     which(!is.na(data_CITL$AROPE_pest))
    )
    
    # f) YL -------------------------------
    
    # Generation of a predifined variance-covariance matrix of auxiliary variables
    VCM <- cov(data_CITL[ , c("ISCED56", "UER", "COB_LOC")])
    S2X <- lapply(X = 1:1580, FUN = function(x){
      mat <- matrix(0,4,4)
      mat[2:4,2:4] <- VCM
      diag(mat) <- unlist(c(0, (data_CITL[x ,c("ISCED56", "UER", 
                                               "COB_LOC")]*0.1)^2
      )
      )
      mat[2:4,2:4] <- as.matrix(nearPD(mat[2:4, 2:4])$mat)
      return(mat)
    })
    
    # Sampling X_hat (i.e. "estimated" auxiliary variables) according to true
    # X-values and predifined variance-covariance matrix S2X
    
    X_Hat <- t(sapply(X = 1:1580, FUN = function(x){
      mvrnorm(n = 1,
              mu = unlist(
                as.vector(data_CITL[x, c("ISCED56","UER",
                                         "COB_LOC")]
                )),
              Sigma =  matrix(unlist(S2X[x]),
                              ncol = 4)[2:4, 2:4]
      )
    }))
    
    EstCITL[s,,"YL",DesNo] <- YL_REML(which(!is.na(data_CITL$AROPE_pest)),
                                      data_CITL,
                                      S2X,
                                      X_Hat)
    
  }

}


# Output / Export --------------------------------------------------------------


#cat(" - Iteration ", s, " of ", R, " (Design: ",
#    designs[DesNo, ]$design.id, ")", "\n")


cat(" The desired number of", R, "iterations has been completed.\n")
cat(" The results of the estimation approaches are stored in the array",
    " 'EstCITL' of dimension:\n", R, "(number of iterations)", 
    "x 1580 (number of CITS)", 
    "x 8 (number of estimation approaches)",
    "x", no_Des, "(number of sampling designs).\n")



# True value as comparative measure for the evaluation of the estimates
TVAL <- tapply(data_P$AROPE, data_P$CITL, mean)
cat(" The true CIT-specific AROPE rates as a comparative measure for the evaluation",
    " of the estimates are stored in the vector",
    " 'TVAL' of length:\n",
    " 1580 (number of CITS).\n")



do_safe <- readline(paste("Do you want to save 'EstCITL' and 'TVAL' to the current",
                          "working-directory? [y/N]\n"))
if (do_safe %in% c("yes", "YES", "Yes", "y", "Y")){
  cat(paste0("Writing simlation results to: ", getwd(), "/results.RData\n"))
  save(list = c("EstCITL", "TVAL"),
       file = "./results.RData.")
}


# return *everything*
curobj <- ls()
ret <- lapply(X = curobj, FUN = function(x) eval(parse(text = x)))
names(ret) <- curobj
return(ret)
}

# EOF 04_SAE_SampleEstimation.R
