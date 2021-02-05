#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) 
# for city statistics 
# and other functional areas part II  
#
# R-Code:
# Preparation of the AMELIA-Data for the Sampling and Estimation Process
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, 
# Simon Lenau, Julia Manecke, Hariolf Merkle
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Code explanation -------------------------------------------------------------
#
# This code prepares the AMELIA-data for the small area sampling and estimation
# process within the simulation study. At first, it compiles aggregated
# area-level auxiliary information at the level of municipalities and cities,
# which is needed for the Fay-Herriot-Estimator and the Ybarra-Lohr-Estimator.
# The code additionally complements the simulation data set by further variables
# relevant for the sampling and estimation stage. Thus, using the variables
# generated according to Create_arope_example.R, it derives the subcomponents of
# the AROPE-variable and the AROPE-variable itself. The code enables a
# generation of the simulation dataset at both household- and person-level
# depending on the final sampling units of the sampling design to be
# investigated. 
#
#-------------------------------------------------------------------------------

script_03_SAE_DataPreparation <- function(){

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

# 1 ) Compilation of aggregated AMELIA area-level auxiliary information --------

# - Generation of new Variable 'CITL' (combines municipalities and cities) -----
# Municipalities (CIT) not part of cities receive their municipality identifier,
# while those municipalities being part of cities are assigned the city
# identifier (L01 - L10)

# CITL variable at household-level ------------------------
cat("Load CIT at household level...\n")
citl.hh.path <- paste0(data.path, 
                       "HAML.CIT_", aver, ".RData")
load(citl.hh.path)

CITL_HH <- sprintf("%04d", CIT)
CITL_HH[is.element(CIT, c(322,323,326))] <- "L01"
CITL_HH[is.element(CIT, c(311,305,306,309,310,312))] <- "L02"
CITL_HH[is.element(CIT, c(292))] <- "L03"
CITL_HH[is.element(CIT, c(1372,1369))] <- "L04"
CITL_HH[is.element(CIT, c(1088))] <- "L05"
CITL_HH[is.element(CIT, c(1250,1255))] <- "L06"
CITL_HH[is.element(CIT, c(400))] <- "L07"
CITL_HH[is.element(CIT, c(189))] <- "L08"
CITL_HH[is.element(CIT, c(1532,1523,1530,1546))] <- "L09"
CITL_HH[is.element(CIT, c(278))] <- "L10"
CITL_HH <- factor(CITL_HH, levels = c("L01","L02","L03","L04","L05",
                                "L06","L07","L08","L09","L10",
                                levels(factor(sprintf("%04d", CIT)))))
CITL_HH <- factor(CITL_HH)

# CITL variable at person-level ---------------------------
cat("Load CIT at person level...\n")
citl.pl.path <- paste0(data.path, 
                       "PAML.CIT_", aver, ".RData")
load(citl.pl.path)

CITL <- sprintf("%04d", CIT)
CITL[is.element(CIT, c(322,323,326))] <- "L01"
CITL[is.element(CIT, c(311,305,306,309,310,312))] <- "L02"
CITL[is.element(CIT, c(292))] <- "L03"
CITL[is.element(CIT, c(1372,1369))] <- "L04"
CITL[is.element(CIT, c(1088))] <- "L05"
CITL[is.element(CIT, c(1250,1255))] <- "L06"
CITL[is.element(CIT, c(400))] <- "L07"
CITL[is.element(CIT, c(189))] <- "L08"
CITL[is.element(CIT, c(1532,1523,1530,1546))] <- "L09"
CITL[is.element(CIT, c(278))] <- "L10"
CITL <- factor(CITL, levels = c("L01","L02","L03","L04","L05",
                                "L06","L07","L08","L09","L10",
                                levels(factor(sprintf("%04d", CIT)))))
CITL <- factor(CITL)


# - Compilation of aggregated AMELIA-Auxiliary Information at CITL-level -------
# These are needed for area-level estimators, i.e. the Fay-Herriot-Estimator or
# the Measurement Error Estimator 

# Loading required variables ------------------------------
cat("Load multiple person level variables...\n")
Var <- c("HHS", "REG", "DOU", "AGE","COB", "ISCED", "EDU", "BAS", "SEM", "SUP", 
         "UEP", "SEX")
for(v in 1:length(Var)){
    v.path <- paste0(data.path,  
                     "PAML.", Var[v], "_", aver, ".RData")
    load(v.path)
}

hy30.path <- paste0(data.path, 
                    "HAML.HY030_", aver, ".RData")
load(hy30.path)

# Generation of AGE-Groups --------------------------------
AGE_G <- cut(AGE, breaks = c(-1,20,30,40,50,60,70,80))

# Aggregation of variables at CITL-level ------------------

# Number of households
NH_CITL <- unname(table(CITL))
# Number of persons
NP_CITL <- unname(tapply(HHS, CITL, sum))
# Region each CITL belongs to
REG_CITL <- apply(table(CITL, REG), MARGIN = 1, function(i) which(i != 0))
# Degree of urbanisation
DOU_CITL <- apply(table(CITL, DOU), MARGIN = 1, function(i) which(i !=0 ))
# Proportion of age groups
AGE_CITL <- prop.table(table(CITL, AGE_G), margin = 1)
# Proportion of contry of birth
COB_CITL <- prop.table(table(CITL, COB), margin = 1)
# Proportion of ISCED-levels
ISCED_CITL <- prop.table(table(CITL, ISCED), margin = 1)
# NEET rate
neet_cond1 <- ((AGE > 15) & (AGE < 25))
neet_cond2 <- ((EDU != 1) & (!is.na(EDU)) & (BAS != 1) & (BAS != 4))
NEET_CITL <- table(CITL[neet_cond1 & neet_cond2]) / table(CITL[neet_cond1])
# Proportion of households paying rent
Rent_CITL <-  table(CITL_HH[HY030 != 0]) / table(CITL_HH)
# Proportion of self-employed persons between the age of 15 and 66 years
age1566_cond0 <- ((AGE > 15) & (AGE < 66))
SEM_CITL <- prop.table(table(CITL[age1566_cond0], SEM[age1566_cond0]),
                       margin = 1)[,1]
# Proportion of persons with a managerial position between the age of 15 and 66
# years
SUP_CITL <- prop.table(table(CITL[age1566_cond0], SUP[age1566_cond0]),
                       margin = 1)[,1]
# Unemployment profile
UEP_CITL <- apply(table(CITL, UEP), MARGIN = 1, function(i) which(i != 0))
UEP_CITL <- sapply(UEP_CITL, function(i) mean(i))
# Unemployment rate
uer_tab <- table(CITL[age1566_cond0 & is.element(BAS, c(1, 2))], 
                  BAS[age1566_cond0  & is.element(BAS, c(1, 2))])
uer_sum <- rowSums(uer_tab)
UER_CITL <- uer_tab[,2] / uer_sum

# Merging CITL-level auxiliary variables in a dataframe
data_CITL <- cbind.data.frame(NH = as.vector(NH_CITL),
                              NP = NP_CITL,
                              REG = REG_CITL,
                              DOU = DOU_CITL, 
                              round(as.data.frame.matrix(AGE_CITL), 3), 
                              round(as.data.frame.matrix(COB_CITL), 3),  
                              round(as.data.frame.matrix(ISCED_CITL), 3),
                              NEET = round(as.vector(NEET_CITL), 3),
                              Rent = round(as.vector(Rent_CITL), 3),
                              SEM = round(as.vector(SEM_CITL), 3),
                              SUP = round(as.vector(SUP_CITL), 3),
                              UEP = as.vector(UEP_CITL),
                              UER = round(as.vector(UER_CITL), 3))
colnames(data_CITL)[c(5,12:20)] <- c("u20", "COB_LOC", "COB_EU", "COB_OTH",
                                     "ISCED0", "ISCED1", "ISCED2", "ISCED3",
                                     "ISCED4", "ISCED56")

# - Clustering of Cities according to age (AGE) and gender (SEX) structures ----
# This approach is needed for the Synthetic Estimation based on cluster analysis
# As age and sex structures tend to be known across smaller areas, clusteres are
# formed based on the mean age and the percentage of women/men in each area.

StdMean_AGE_SEX <- cbind(rescale(tapply(X = AGE, INDEX = CITL, FUN = mean)),
                         rescale(tapply(X = SEX, INDEX = CITL, FUN = mean))
                        )

Clus_data <- kmeans(StdMean_AGE_SEX, 10)
#Clus_data$cl[CITL]


# 2 ) Generation of the simulation dataset -------------------------------------


# - AROPE-Variables ---------------------------------------

# The AROPE Household Variables that have been generated according to code
# 'Create_arope_example.R' are now processed to derive the AROPE-subindicators 
# (Material Deprivation, Low Work Intensity, At-Risk-Of-Poverty) and the 
# AROPE-variable itself
# The variable-vector is generated for both person- and household-level

# Material Deprivation ----------------
cat("Load AROPE variables...\n")
for(v in c(1:8, 10:11)){
    haml.path <- paste0(data.path,  "HAML.HS", 
                        sprintf("%02d", v), "0","_", aver, ".RData")
    load(haml.path)
}
haml.hh050.path <- paste0(data.path, "HAML.HH050_",
                          aver,".RData")
load(haml.hh050.path)

L1 <- ifelse(test = (as.numeric(HS010) + as.numeric(HS020) + as.numeric(HS030))
                    == 6,
             yes = 0, no = 1)
vars <- c("HH050",
          paste0("HS", c("060", "050", "040", "080", "100", "110", "070"))
         )
for (i in 1:8){
    assign(paste0("L", i + 1), ifelse(test = eval(parse(text = vars[i])) == 2,
                                      yes = 1, no = 0)
        )
}

MD <- ifelse(test = rowSums(cbind(L1, L2, L3, L4, L5, L6, L7, L8, L9)) >= 3,
             yes = 1, no = 0)

hid.path <- paste0(data.path, 
                   "HAML.HID_", aver, ".RData")
load(hid.path)

tabMD <- tapply(X = MD, INDEX = HID, FUN = sum)

# Low work intensity ------------------

cat("AROPE low work intensity...\n")
for(v in c(70, 72, 80, 85, 87, 90)){
    paml.path <- paste0(data.path,  "PAML.PL", 
                       sprintf("%03d", v), "_", aver, ".RData")
    
    load(paml.path)
}
paml.hid.path <- paste0(data.path, 
                        "PAML.HID_", aver, ".RData")
load(paml.hid.path)

PLSum <- rowSums(cbind(PL070, PL072, PL080, PL085, PL087, PL090))

Ne1 <- PL070 + 0.5 * PL072
month_rat <- Ne1 / 12
wi <- tapply(X = month_rat, INDEX = HID, FUN = sum, na.rm = TRUE)
size <- tapply(X = !is.na(month_rat), INDEX = HID, FUN = sum)
WORK_INT <- wi / size
LWI <- as.vector(ifelse(test = WORK_INT <= 0.2, yes = 1, no = 0))

haml.hid.path <- paste0(data.path, 
                        "HAML.HID_", aver, ".RData")
load(haml.hid.path)

tabLWI <- tapply(X = LWI, INDEX = HID, FUN = sum)

# At-risk-of-poverty ------------------

cat("AROPE at risk of poverty...\n")
paml.edi.path <- paste0(data.path, 
                        "PAML.EDI_", aver, ".RData")
paml.reg.path <- paste0(data.path, 
                        "PAML.REG_", aver, ".RData")
load(paml.edi.path); load(paml.reg.path)

medEDIReg <- tapply(X = EDI, INDEX = REG, FUN = median)[REG]
ARP_P <- as.numeric(EDI < (0.6 * medEDIReg))

# Transformation to person-level
cat("Transform data from HH to P level...\n")
paml.hid.path <- paste0(data.path, 
                        "PAML.HID_", aver, ".RData")
load(paml.hid.path)

MD_P <- as.vector(tabMD[HID])
LWI_P <- as.vector(tabLWI[HID])

AROPE_P <- ifelse(test = rowSums(cbind(ARP_P, LWI_P, MD_P)) == 0, yes = 0,
                  no = 1)

# - Person-level simulation dataset -----------------------
cat("Load person level simulation set...\n")

# For the generation of the R-File 'Simulation_Universe.RData' it is referred to 
# the R-Code 'Population_Module.R'
simuni.path <- paste0(getwd(), "/AMELIA_samples/", "City_Statistics_II/",
                      "Population_Data/", "Simulation_Universe.RData")
load(simuni.path) # The Simulation Universe is denoted "data"

# Ordering the simulation dataset according to person-ID and adding generated
# variables
data_P <-  data[order(data$pid), ]
data_P <- cbind.data.frame(data_P, AGE, BAS, COB, CITL, REG)
data_P <- within(data = data_P, expr = {
                     #dou   <- dou;
                     #inc_c <- inc_c;
                     AROPE <- AROPE_P;
                     ARP   <- ARP_P;
                     LWI   <- LWI_P;
                     MD    <- MD_P;
                     #age_c <- age_c;
                     BAS1  <- as.numeric(BAS == 1);
                     BAS3  <- as.numeric(BAS == 3);
                     AGE65 <- as.numeric(AGE >= 65);
                     COB23 <- as.numeric(COB != 1);
                })

hid_diff <- vector(length = nrow(data_P))
hid_diff[2:length(hid_diff)] <- data_P[1:(length(hid_diff) - 1), ]$hid

# - Household-level simulation dataset -----------------------------------------
cat("Load Household level simulation set...\n")

# "Reduction" of the dataset to household-level.

data_H <- data_P[which(data_P$hid != hid_diff), c(1:5,8,14:15,20:23)]

haml.hhs.path <- paste0(data.path, 
                        "HAML.HHS_", aver, ".RData")
load(haml.hhs.path)

# Amendment with further auxiliary variables
cat("Merge with auxiliary information...\n")
data_H <- cbind.data.frame(data_H, 
                           BAS1_Abs = as.vector(tapply(BAS == 1, HID, sum)),
                           BAS3_Abs = as.vector(tapply(BAS == 3, HID, sum)),
                           AGE65_Abs = as.vector(tapply(AGE >= 65, HID, sum)),
                           COB23_H_Abs = as.vector(tapply(COB != 1, HID, sum)),
                           HHS = HHS,
                           CITL = CITL_HH
                          )

# Output / Export --------------------------------------------------------------

cat("Data Sets are fully prepared!\n")
do_safe <- readline(paste("Do you want to save them to the current",
                          "working-directory? [y/N]\n"))
if (do_safe %in% c("yes", "YES", "Yes", "y", "Y")){
    cat(paste0("Writing data sets to: ", getwd(), "/data_prepared.RData\n"))
    save(list = c("data_P", "data_H", "Clus_data", "data_CITL"),
         file = "./data_prepared.RData.")
}

# return *everything*
curobj <- ls()
ret <- lapply(X = curobj, FUN = function(x) eval(parse(text = x)))
names(ret) <- curobj
return(ret)
}

# EOF 03_SAE_DataPreparation.R
