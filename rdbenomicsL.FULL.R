# LIVE API FOR AMECO
library(readr)
library(readxl)
library(plyr)
library(cointReg)
library(dplyr)
library(rdbnomics)
Countries <- list("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain")
ccode <- c("aut", "bel", "fin", "fra", "deu", "grc", "irl", "ita", "nld", "prt", "esp")
ccode1 <- c("aut", "bel", "fin", "fra", "deu", "grc", "irl", "ita", "nld", "prt", "esp")
# nominal gdp
rpri <- rdb("AMECO", "XUNRQ-1", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
gdp <- rdb("AMECO", "OVGD", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # REAL PRICES
gdp1 <- rdb("AMECO", "UVGD", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # NOMINAL PRICES
# eurozone aggregate growth
eaz <- rdb("AMECO", "OVGD", dimensions = list(geo = "ea12", unit = "mrd-ecu-eur-weighted-mean-of-t-t-1-national-growth-rates-weights-t-1-current-prices-in-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
eaz1 <- rdb("AMECO", "OVGD", dimensions = list(geo = "usa", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # or OVGD [WB/WDI/NY.GDP.MKTP.KD-1W]
eaz2 <- rdb("AMECO", "OVGD", dimensions = list(geo = "jpn", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # or OVGD
eazW <- rdb("WB", "WDI", mask = "NY.GDP.MKTP.KD-1W") %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # or OVGD
china <- rdb("WB", "GEM", mask = "NYGDPMKTPSAKD-CHN") %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019) # or OVGD
inf <- rdb("AMECO", "PXGS", dimensions = list(geo = ccode, unit = "ecu-eur-2015-100", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
# foreign prices
finf <- rdb("AMECO", "PMGS", dimensions = list(geo = ccode, unit = "ecu-eur-2015-100", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
# exports
exp <- rdb("AMECO", "OXGS", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
# imports
imp <- rdb("AMECO", "OMGS", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
# exchange rate
exr <- rdb("AMECO", "XNE", dimensions = list(geo = ccode, unit = "annual-average-1-ecu-eur-units-of-national-currency", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
# privte consumption
gdc <- rdb("AMECO", "OCNT", dimensions = list(geo = ccode, freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
acc <- rdb("AMECO", "UBCA", dimensions = list(geo = ccode, unit = "percentage-of-gross-domestic-product-at-current-prices", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
acc1 <- rdb("AMECO", "UBCA", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
pub_debt <- rdb("AMECO", "UDGGL", dimensions = list(geo = ccode, unit = "percentage-of-gdp-at-current-prices-excessive-deficit-procedure", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1991 & original_period < 2019)
pub_debt1 <- rdb("AMECO", "UDGGL", dimensions = list(geo = ccode, unit = "mrd-ecu-eur", freq = "a")) %>%
    filter(!is.na(value) & original_period > 1990 & original_period < 2019)
eudata2 <- cbind(pub_debt1$original_period, pub_debt1$Country, pub_debt1$value)
colnames(eudata2) <- c("period", "target", "debt")
for (i in Countries) {
  eudata3 <- as.data.frame(eudata2) %>% filter(target == i)
  eudata4 <- diff(as.numeric(eudata3$debt))
  eudata5 <- as.numeric(eudata3$debt)
  assign(paste0(rep("deb."), i), diff(eudata4))
  assign(paste0(rep("def."), i), diff(eudata5))
}


################################################
eudata1 <- cbind(gdp$original_period, gdp$Country, gdp$value, inf$value, finf$value, exp$value, imp$value, exr$value, rpri$value, gdc$value, acc$value, pub_debt$value, gdp1$value, acc1$value)
colnames(eudata1) <- c("period", "target", "income", "prices", "fprices", "exports", "imports", "xrate", "rprices", "consump", "cacc", "debt.pub", "nom.income", "cacc1")
for (i in Countries) {
  eudata <- as.data.frame(eudata1) %>% filter(target == i)
  share <- as.numeric(eudata$income) / as.numeric(eaz$value)
  income <- log(as.numeric(as.character(eudata$income)))
  nom.income <- as.numeric(eudata$nom.income)
  fincome2a <- as.numeric(eaz1$value)
  fincome2j <- as.numeric(eaz2$value)
  fincome2c <- as.numeric(china$value)
  fincome1 <- as.numeric(eaz$value) / (1 - share) -
    as.numeric(as.character(eudata$income)) * share
  fincome <- log((fincome1 + fincome2a + fincome2j) / 3)
  rprices <- log(as.numeric(as.character(eudata$rprices)))
  exports <- log(as.numeric(as.character(eudata$exports)))
  imports <- log(as.numeric(as.character(eudata$imports)))
  caccount <- as.numeric(as.character(eudata$cacc))
  nom.caccount <- as.numeric(eudata$cacc1)
  consump <- log(as.numeric(as.character(eudata$consump)))
  eudata <- cbind(income, fincome, rprices, exports, imports, consump, caccount, nom.caccount, nom.income)
  assign(paste0(rep("d."), i), eudata)
}
###### final log-dif data.frame of values 1991-2019
nom.income1 <- cbind(d.Austria[, "nom.income"], d.Belgium[, "nom.income"], d.Finland[, "nom.income"], d.France[, "nom.income"], d.Germany[, "nom.income"], d.Greece[, "nom.income"], d.Ireland[, "nom.income"], d.Italy[, "nom.income"], d.Netherlands[, "nom.income"], d.Portugal[, "nom.income"], d.Spain[, "nom.income"])
LincomeDL <- cbind(d.Austria[, "income"], d.Belgium[, "income"], d.Finland[, "income"], d.France[, "income"], d.Germany[, "income"], d.Greece[, "income"], d.Ireland[, "income"], d.Italy[, "income"], d.Netherlands[, "income"], d.Portugal[, "income"], d.Spain[, "income"])
LfincomeDL <- cbind(d.Austria[, "fincome"], d.Belgium[, "fincome"], d.Finland[, "fincome"], d.France[, "fincome"], d.Germany[, "fincome"], d.Greece[, "fincome"], d.Ireland[, "fincome"], d.Italy[, "fincome"], d.Netherlands[, "fincome"], d.Portugal[, "fincome"], d.Spain[, "fincome"])
LrpricesDL <- cbind(d.Austria[, "rprices"], d.Belgium[, "rprices"], d.Finland[, "rprices"], d.France[, "rprices"], d.Germany[, "rprices"], d.Greece[, "rprices"], d.Ireland[, "rprices"], d.Italy[, "rprices"], d.Netherlands[, "rprices"], d.Portugal[, "rprices"], d.Spain[, "rprices"])
LexportsDL <- cbind(d.Austria[, "exports"], d.Belgium[, "exports"], d.Finland[, "exports"], d.France[, "exports"], d.Germany[, "exports"], d.Greece[, "exports"], d.Ireland[, "exports"], d.Italy[, "exports"], d.Netherlands[, "exports"], d.Portugal[, "exports"], d.Spain[, "exports"])
LimportsDL <- cbind(d.Austria[, "imports"], d.Belgium[, "imports"], d.Finland[, "imports"], d.France[, "imports"], d.Germany[, "imports"], d.Greece[, "imports"], d.Ireland[, "imports"], d.Italy[, "imports"], d.Netherlands[, "imports"], d.Portugal[, "imports"], d.Spain[, "imports"])
LconsumpDL <- cbind(d.Austria[, "consump"], d.Belgium[, "consump"], d.Finland[, "consump"], d.France[, "consump"], d.Germany[, "consump"], d.Greece[, "consump"], d.Ireland[, "consump"], d.Italy[, "consump"], d.Netherlands[, "consump"], d.Portugal[, "consump"], d.Spain[, "consump"])

# SECTORAL BALANCES
nom.caccountDL <- cbind(d.Austria[, "nom.caccount"], d.Belgium[, "nom.caccount"], d.Finland[, "nom.caccount"], d.France[, "nom.caccount"], d.Germany[, "nom.caccount"], d.Greece[, "nom.caccount"], d.Ireland[, "nom.caccount"], d.Italy[, "nom.caccount"], d.Netherlands[, "nom.caccount"], d.Portugal[, "nom.caccount"], d.Spain[, "nom.caccount"])
caccountDL <- cbind(d.Austria[, "caccount"], d.Belgium[, "caccount"], d.Finland[, "caccount"], d.France[, "caccount"], d.Germany[, "caccount"], d.Greece[, "caccount"], d.Ireland[, "caccount"], d.Italy[, "caccount"], d.Netherlands[, "caccount"], d.Portugal[, "caccount"], d.Spain[, "caccount"])
debtDL <- cbind(deb.Austria, deb.Belgium, deb.Finland, deb.France, deb.Germany, deb.Greece, deb.Ireland, deb.Italy, deb.Netherlands, deb.Portugal, deb.Spain)
defpubDL <- -cbind(def.Austria, def.Belgium, def.Finland, def.France, def.Germany, def.Greece, def.Ireland, def.Italy, def.Netherlands, def.Portugal, def.Spain)
defprvDL <- defpubDL + nom.caccountDL
defpubDL1 <- defpubDL / nom.income1
defprvDL1 <- defprvDL / nom.income1
cacc_share <- caccountDL / nom.income1


inco_north <- rowsum(d.Greece[, "nom.income"], d.Italy[, "nom.income"], d.Portugal[, "nom.income"], d.Spain[, "nom.income"])
inco_south <- rowsum(d.Austria[, "nom.income"], d.Finland[, "nom.income"], d.Germany[, "nom.income"], d.Netherlands[, "nom.income"])
cacc_north <- rowsum(d.Greece[, "nom.caccount"], d.Italy[, "nom.caccount"], d.Portugal[, "nom.caccount"], d.Spain[, "nom.caccount"])
cacc_south <- rowsum(d.Austria[, "nom.caccount"], d.Finland[, "nom.caccount"], d.Germany[, "nom.caccount"], d.Netherlands[, "nom.caccount"])
share_north <- cacc_north / inco_north
share_south <- cacc_south / inco_south
cacc_mean <- array()
for (i in 1:11) {
  cacc_mean[i] <- mean(cacc_share[, i])
}