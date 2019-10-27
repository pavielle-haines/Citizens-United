
# READ IN DATA AND CALL LIBRARIES -----------------------------------------
library(knitr)
library(foreign)
library(Synth)
library(lfe)
library(standardize)
library(gsynth)
library(panelView)

setwd("/Users/Shawn/Dropbox/State policy/R scripts/External Election Data/Election Data Files/Final Files for Analysis")

################################################################
cu0 <- read.csv("Final Outcome Analysis Data.csv")
head(cu0)

gdp <- read.csv("NewStateGDP2.csv")
head(gdp)

dwnom <- read.csv("State DW Nominate.csv")
head(dwnom)

union <- read.csv("Union Membership.csv")
head(union)

deficit <- read.csv("State Deficits.csv")
head(deficit)


# ORGANIZE THE DATA -------------------------------------------------------

gdp <- subset(gdp, legyear >= 2003)
cu0a <- subset(cu0, legyear >= 2003)
dwnom <- subset(dwnom, legyear >= 2003)
tax09 <- subset(cu0, legyear == 2009, select = c("statenum", "standtax"))
names(tax09) <- c("statenum", "tax09")
tax03 <- subset(cu0, legyear == 2003, select = c("statenum", "standtax"))
names(tax03) <- c("statenum", "tax03")
tax00 <- subset(cu0, legyear == 2000, select = c("statenum", "standtax"))
names(tax00) <- c("statenum", "tax00")


cu1 <- merge(cu0a, gdp, by = c("state", "legyear"))
cu2 <- merge(cu1, dwnom, by = c("state", "legyear"))
cu3 <- merge(cu2, union, by = c("state", "legyear"))
cu4 <- merge(cu3, deficit, by = c("state", "legyear"))
cu5 <- merge(cu4, tax09, by = c("statenum"))
cu6 <- merge(cu5, tax03, by = c("statenum"))
cu <- merge(cu6, tax00, by = c("statenum"))

head(cu)


normalize <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
cu$indtaxrev <- cu$indtax*-1
cu$standintax <- normalize(cu$indtaxrev, na.rm = TRUE)
##################################################################
# because my windows machine had trouble with the above csv files
# Pavielle made a merged file that is based on the above commands
#################################################################

cu <- read.csv("CU Analysis Data.csv")
head(cu)





# TEST THE DATA -----------------------------------------------------------

#test <- subset(cu, banstate = 0)

#m1 <- felm(standtax ~ standintax + gdplag1 + G(state) + G(legyear), data = test, cluster = test$state)
#summary(m1)



# SYNTHESIZE TAX DATA BAN/NOBAN --------------------------------------------------------

#drop unwanted variables
cusynth <- subset(cu, select = c(banstate, postcit, legyear, banstate, statenum, standtax, indtax, Rperc, tax09, tax03, tax00, gdplag1, unemploylag1, deficitlag1, firmslag, unionmem))
cusynthfinal <- na.omit(cusynth)
head(cusynthfinal)

control <- subset(cusynthfinal, banstate == 0)
head(control)
control1 <- aggregate(control, by = list(control$legyear), FUN = mean)
control1 <- subset(control1, select = -c(Group.1))
head(control1)

treatment <- subset(cusynthfinal, banstate == 1, select = -c(statenum))
head(treatment)
treatment1 <- aggregate(treatment, by = list(treatment$legyear), FUN = mean)
treatment1$statenum <- 28
head(treatment1)
treatment <- subset(treatment1, select = -c(Group.1))
head(treatment)

cusynthfinal <- rbind(control, treatment)
head(cusynthfinal, n = 25)

dataprep.out <- 
  dataprep(
  foo = cusynthfinal,
  predictors = c("indtax", "Rperc", "standtax", "gdplag1", "unemploylag1", "deficitlag1", "firmslag", "unionmem"),
  dependent = "standtax",
  unit.variable = "statenum",
  time.variable = "legyear",
  treatment.identifier = c(28),
  controls.identifier = c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,18, 19,21, 22, 23, 24, 25, 26, 27), 
  time.predictors.prior = c(2003:2009),
  time.optimize.ssr = c(2003:2015)
  )

synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(dataprep.res = dataprep.out, synth.res = synth.out)

print(synth.tables)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("Pro-Corporate Tax Rates"),
          Xlab = c("Year"),
          Ylim = c(.25,.55),
          Legend = NA)
legend("topleft", bty = "n", c("Ban States", "Synthetic Ban States"), lty = c(1, 2))
abline(v = 2010, col = "red")
lines(control1$legyear, control1$standtax, col = "blue")
title("Comparing No Ban States to Ban States (Taxes Full)")




# SYNTHESIZE TAX DATA CORPBAN/NOBAN --------------------------------------------------------

cusynth <- subset(cu, select = c(bancorp, postcit, legyear, banstate, statenum, standtax, indtax, Rperc, tax09, tax00, tax03, gdplag1, unemploylag1, deficitlag1, firmslag, unionmem))
cusynthfinal <- na.omit(cusynth)
head(cusynthfinal)

control <- subset(cusynthfinal, banstate == 0)
head(control)
control1 <- aggregate(control, by = list(control$legyear), FUN = mean)
control1 <- subset(control1, select = -c(Group.1))
head(control1)

treatment <- subset(cusynthfinal, bancorp == 1, select = -c(statenum))
head(treatment)
treatment1 <- aggregate(treatment, by = list(treatment$legyear), FUN = mean)
treatment1$statenum <- 28
head(treatment1)
treatment <- subset(treatment1, select = -c(Group.1))
head(treatment)

cusynthfinal <- rbind(control, treatment)
head(cusynthfinal, n = 5)

dataprep.out30 <- 
  dataprep(
    foo = cusynthfinal,
    predictors = c("indtax", "Rperc", "tax09", "tax03", "gdplag1", "unemploylag1", "deficitlag1", "firmslag", "unionmem"),
    dependent = "standtax",
    unit.variable = "statenum",
    time.variable = "legyear",
    treatment.identifier = c(28),
    controls.identifier = c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,18, 19,21, 22, 23, 24, 25, 26, 27), 
    time.predictors.prior = c(2003:2009),
    time.optimize.ssr = c(2003:2015)
  )

synth.out <- synth(dataprep.out30)

synth.tables <- synth.tab(dataprep.res = dataprep.out30, synth.res = synth.out)

print(synth.tables)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out30,
          Ylab = c("Pro-Corporate Tax Rates"),
          Xlab = c("Year"),
          Ylim = c(.25,.55),
          Legend = NA)
legend("topleft", bty = "n", c("Ban States", "Synthetic Ban States"), lty = c(1, 2))
abline(v = 2010, col = "red")
lines(control1$legyear, control1$standtax, col = "blue")
title("Comparing No Ban States to Corporate Ban States (Full Taxes)")




# SYNTHESIZE TAX DATA UNIONCORPBAN/NOBAN --------------------------------------------------------

cusynth <- subset(cu, select = c(bancorpunion, postcit, legyear, banstate, statenum, standtax, indtax, Rperc, tax09, tax03, tax00, gdplag1, unemploylag1, deficitlag1, firmslag, unionmem))
cusynthfinal <- na.omit(cusynth)
head(cusynthfinal)

control <- subset(cusynthfinal, banstate == 0)
head(control)
control1 <- aggregate(control, by = list(control$legyear), FUN = mean)
control1 <- subset(control1, select = -c(Group.1))
head(control1)

treatment <- subset(cusynthfinal, bancorpunion == 1, select = -c(statenum))
head(treatment)
treatment1 <- aggregate(treatment, by = list(treatment$legyear), FUN = mean)
treatment1$statenum <- 28
head(treatment1)
treatment <- subset(treatment1, select = -c(Group.1))
head(treatment)

cusynthfinal <- rbind(control, treatment)
head(cusynthfinal, n = 25)

dataprep.out31 <- 
  dataprep(
    foo = cusynthfinal,
    predictors = c("indtax", "Rperc", "tax09", "tax03", "gdplag1", "unemploylag1", "deficitlag1", "firmslag", "unionmem"),
    dependent = "standtax",
    unit.variable = "statenum",
    time.variable = "legyear",
    treatment.identifier = c(28),
    controls.identifier = c(2,3,4,5,6,7,8,9,10,11,13,14,15,16,18, 19,21, 22, 23, 24, 25, 26, 27), 
    time.predictors.prior = c(2003:2009),
    time.optimize.ssr = c(2003:2015)
  )

synth.out <- synth(dataprep.out31)

synth.tables <- synth.tab(dataprep.res = dataprep.out31, synth.res = synth.out)

print(synth.tables)

path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out31,
          Ylab = c("Pro-Corporate Tax Rates"),
          Xlab = c("Year"),
          Ylim = c(.25,.55),
          Legend = NA)
legend("topleft", bty = "n", c("Ban States", "Synthetic Ban States"), lty = c(1, 2))
abline(v = 2010, col = "red")
lines(control1$legyear, control1$standtax, col = "blue")
title("Comparing No Ban States Corporate\nand Union Ban States (Full Taxes)")


panelView(standtax ~ treat1, data = cu,  index = c("state","legyear")) 

plot(out, main = "Estimated ATT (MC)", xaxt='n')




