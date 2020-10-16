########### ARDL COINTEGRATION BPCGM ESTIMATION #############
relpath <- "/home/other/Desktop/ARDL.sept2020/"
source("/home/other/Desktop/ARDL.sept2020/API/rdbenomicsL.FULL.R")
options(warn = -1)
library("systemfit")
library("ARDL")
library("dLagM")

# DATA CONTAINERS
coeff_x <- data.frame(matrix(NA, 10, 11))
coeff_m <- data.frame(matrix(NA, 18, 11))
p_test_mf <- data.frame(matrix(NA, 11, 3))
p_test_xf <- data.frame(matrix(NA, 11, 3))
ardl_fx <- array()
ardl_fm <- array()
ardl_fx1 <- array()
ardl_fm1 <- array()
rows_x <- c("intercept", "finco", "rpri", "ect", "tsat", "tstat", "tstat", "tsat", "FboundX", "FpvalueX")
rows_m <- c("intercept", "inco", "rpri", "dum", "dum2", "dum3", "dum4", "ect", "tstat", "tsat", "tstat", "tstat", "tstat", "tstat", "tstat", "tstat", "FboundM", "FpvalueM")

# MODELLIZATION PARAMETERS
break_m <- c(0, 0, 15, 0, 0, 15, 0, 15, 8, 0, 15)
break_m1 <- c(0, 0, 0, 0, 0, 20, 0, 20, 0, 0, 20)
max_p <- 2
max_q <- 2
ordering_x <- 4
ordering_m <- 4
ordering <- c(1, 1, 1)
criterion <- "BIC"
ardl_order <- c(1,2,1,1)
new_order <- c(rep(4, 4))
case_x <- 2
case_m <- 4
if (case_m == 1) {
  deter_m <- deter_d <- " - 1"
}
if (case_x == 1) {
  deter_x <- " - 1"
}
if (case_m == 2) {
  deter_d <- deter_m <- ""
}
if (case_x == 2) {
  deter_x <- ""
}
if (case_m == 4) {
  deter_m <- deter_d <- "  + trend(impo)"
}
if (case_x == 4) {
  deter_x <- "  + trend(expo)"
}
formula_x <- as.formula(paste0("expo ~ finco + rpri", deter_x))
formula_m <- as.formula(paste0("impo ~ inco + rpri ", deter_m))
formula_m_dum <- as.formula(paste0("impo ~ inco + rpri ", deter_d, " | dum"))

# CONDITIONAL MODELS BASED ON THE PRESENCE OF DUMMYS OF STRUC. CHANGE
for (i in seq_along(Countries)) {
  if (i == 1 | i == 2 | i == 4 | i == 5 | i == 7 | i == 10) {
    source(paste0(relpath, "/MODELS/FORMULA/NODUMnew.R"))
    # source(paste0(relpath, "/MODELS/FORMULA/NODUM.R"))
    #source(paste0(relpath, "/MODELS/FORMULA/NODUMnorpi.R"))
  }
  if (i == 6 | i == 8 | i == 11 | i == 9 | i == 3) {
    source(paste0(relpath, "/MODELS/FORMULA/DUMnew.R"))
    # source(paste0(relpath, "/MODELS/FORMULA/DUM.R")
    # source(paste0(relpath, "/MODELS/FORMULA/NODUMnorpi.R"))
  }
}
# PRESENTATION OF JOINT RESULTS
#ardl_ftest <- cbind(ardl_fx, ardl_fx1, p_test_xf, ardl_fm, ardl_fm1, p_test_mf)
ardl_ftest <- cbind(ardl_fx,  p_test_xf, ardl_fm,  p_test_mf)
View(ardl_ftest)