# Plik z SAD - zajęcia 1 - 2025-12-10

# czyszczenie środowiska
rm(list = ls())

#import danych
data <- read.csv("data/data_ecotones.csv")

# przegląd i czyszczenie danych
colnames(data)
rownames(data)
names(data) 

data$X
data$X == as.integer(rownames(data))
identical(data$X, as.integer(rownames(data)))

data <- data[, -which(names(data) == "X")]

View(data[, c("Species_fn", "Species")])
summary(as.factor(data$Species))
summary(as.factor(data$Species_fn))
data <- data[, -which(names(data) == "Species_fn")]
View(data[, c("Place", "Site")])
  
data$V_blood_BDeq1
sum(data$V_blood_BDeq1) == length(data$V_blood_BDeq1) 
data$V_blood_BDeq1 <- as.logical(data$V_blood_BDeq1) 

data$V_feather_BDeq1
sum(data$V_feather_BDeq1) == length(data$V_feather_BDeq1)
data$V_feather_BDeq1 <- as.logical(data$V_feather_BDeq1) 

# ustalenie ostatecznego porządku zmiennych
data <- data[, c(1:12, 14:16, 18:27, 13, 28:40, 17, 41:74)]

# zmiana charakteru zmiennych 
data$Species <- as.factor(data$Species)
data$Year <- as.factor(data$Year)




data$Place <- as.factor(data$Place)
data$Site <- as.factor(data$Site)
data$Sex <- as.factor(data$Sex)



# Plik z SAD - zajęcia 2 - SAD 2025-12-18

# Czyszczenie środowiska
rm(list = ls())

# Przegląd zbioru danych
names(data)
summary(data)
unique(data$ID)
length(unique(data$ID)) 
as.factor(data$ID)

## Ekstrakcja wybranych zmiennych
var_blood <- grep("blood", names(data), value = TRUE)
var_blood_bdeq1 <- grep("_BDeq1", var_blood, value = TRUE)

var_blood[!var_blood %in% var_blood_bdeq1] 
'%out%' <- Negate('%in%') 
var_blood[var_blood %out% var_blood_bdeq1] 
setdiff(var_blood, var_blood_bdeq1) 
elements_blood <- var_blood[!var_blood %in% c(var_blood_bdeq1, "d13C_blood", "d15N_blood", "d34S_blood")]
elements <- gsub("_blood", "", elements_blood)

var_feathers <- grep("feather", names(data), value = TRUE)
var_feathers_bdeq1 <- grep("_BDeq1", var_feathers, value = TRUE)
elements_feather <- var_feathers[!var_feathers %in% c(var_feathers_bdeq1, "d13C_feather", "d15N_feather", "d34S_feather")]

factors <- names(data)[names(data) %in% c("Species", "Year", "Place", "Site", "Sex")] # ekstrakcja czynników

# Graficzny przegląd danych
boxplot(data$Hg_blood~data$Species, range = 0)
stripchart(data$Hg_blood~data$Species, vertical = TRUE, method="jitter", pch=1, add = TRUE)

# Zamknijmy to w funkcję
boxplotraw <- function(y, x) {
  boxplot(y~x, range = 0, varwidth = TRUE)
  stripchart(y~x, vertical = TRUE, method="jitter", pch=1, add = TRUE)
}
boxplotraw(data$Hg_blood, data$Species)

# Pętla (wyjście pdostawowe)
for (var in elements_blood) { # przejście pętli po kolejnych składowych elements_blood
  boxplot(data[[var]]~data$Species, range = 0, ylab = var)
  stripchart(data[[var]]~data$Species, vertical = TRUE, method="jitter", pch=1, add = TRUE)
  rm(var) # zawarte w pętli i wykonuje to po każej iteracji pętli
}

# Pętla (wyjscie podstawowe, podejście alternatywne)
for (i in seq_along(elements_blood)) { # numeracja iteracji na liczbach
  var = elements_blood[i]
  boxplot(data[[var]]~data$Species, range = 0, ylab = var)
  stripchart(data[[var]]~data$Species, vertical = TRUE, method="jitter", pch=1, add = TRUE)
  rm(i, var) 
}

# Pętla (wyjście z funkcją)
for (var in elements_blood) { # przejście pętli po kolejnych składowych elements_blood
  boxplotraw(data[[var]], data$Species)
  rm(var) 
}

# Skoro pętla gotowa to zastosujmy ją dla różnych czynników
for (factor in factors) { # przejście pętli po kolejnych składowych foctors
  for (var in elements_blood) { # przejście pętli po kolejnych składowych elements_blood
    boxplot(data[[var]]~data[[factor]], range = 0, ylab = var, xlab = factor)
    stripchart(data[[var]]~data[[factor]], vertical = TRUE, method="jitter", pch=1, add = TRUE)
  }
  rm(factor, var) 
}

for (factor in factors) { 
  for (var in elements_blood) { 
    print(c(factor, var))
    print(aggregate(data[[var]]~data[[factor]], FUN = mean))
  }
  rm(factor, var) 
}



# Plik z SAD - zajęcia 3 - SAD 2026-01-07

# Czyszczenie środowiska
rm(list = ls())

# Wybierzemy jedną zmienną i dokonamy szczegółowego przeglądu i dalszej analizy.
names(data)
var_blood
var_blood[1:17]

# Przegląd szczegółowy Hg
boxplotraw(data$Hg_blood, data$Species) 
boxplotraw(data$Hg_blood, data$Year) 
boxplotraw(data$Hg_blood, data$Place) 
data[data$Place == "Antifer", 1:5] 
boxplotraw(data$Hg_blood, data$Site) 
nrow(data[data$Site == "Chausey islands",]) 
nrow(data[data$Site == "Seine estuary",]) 
boxplotraw(data$Hg_blood, data$Sex) 

# Identyfikacja rozkładu
hist(data$Hg_blood) 
hist(data$Hg_blood[data$Species == "GBBG"]) 

library(fitdistrplus)
descdist(data$Hg_blood, boot = 100)
fit_norm <- fitdist(data$Hg_blood, "norm")
fit_lnorm <- fitdist(data$Hg_blood, "lnorm")
fit_gamma <- fitdist(data$Hg_blood, "gamma")
summary(fit_norm) 
gofstat(fit_norm) 
gofstat(fit_lnorm)
gofstat(fit_gamma)

cdfcomp(fit_norm)
cdfcomp(list(fit_norm, fit_lnorm, fit_gamma))

# Sprawdzenie współliniowości
cor(data$d13C_blood, data$d15N_blood)
cor(data$d13C_blood, data$d34S_blood)
cor(data$d15N_blood, data$d34S_blood)
cor(data$Hg_blood, data$Hg_feather)
cor(data$Se_blood, data$d15N_blood)

mod1 <- glm(Hg_blood ~ Site + Sex + SMI + d15N_blood, data = data[data$Species == "GBBG",])
summary(mod1)

mod0 <- glm(log(Hg_blood) ~ 1, data = data[data$Species == "GBBG",]) 
mod1 <- glm(log(Hg_blood) ~ Site, data = data[data$Species == "GBBG",])
mod2 <- glm(log(Hg_blood) ~ Sex, data = data[data$Species == "GBBG",])
mod3 <- glm(log(Hg_blood) ~ SMI, data = data[data$Species == "GBBG",])
mod4 <- glm(log(Hg_blood) ~ d15N_blood, data = data[data$Species == "GBBG",])
mod5 <- glm(log(Hg_blood) ~ d13C_blood, data = data[data$Species == "GBBG",])
mod6 <- glm(log(Hg_blood) ~ d34S_blood, data = data[data$Species == "GBBG",])
mod7 <- glm(log(Hg_blood) ~ Site + Sex + SMI + d15N_blood, data = data[data$Species == "GBBG",])
mod8 <- glm(log(Hg_blood) ~ Site * Sex * SMI * d15N_blood, data = data[data$Species == "GBBG",])
mod9 <- glm(log(Hg_blood) ~ Site + Sex + SMI + d13C_blood, data = data[data$Species == "GBBG",])
mod10 <- glm(log(Hg_blood) ~ Site * Sex * SMI * d13C_blood, data = data[data$Species == "GBBG",])
mod11 <- glm(log(Hg_blood) ~ Site + Sex + SMI + d34S_blood, data = data[data$Species == "GBBG",])
mod12 <- glm(log(Hg_blood) ~ Site * Sex * SMI * d34S_blood, data = data[data$Species == "GBBG",])


library(AICcmodavg) # pakiet, który służy do wyliczania AICc, czyli AIC dedykowanego niewielki zbiorom danych.
AICc(mod0)
AICc(mod1)
AICc(mod2)
AICc(mod3)
AICc(mod4)
AICc(mod5)
AICc(mod6)
AICc(mod7)
AICc(mod8)
AICc(mod9)
AICc(mod10)
AICc(mod11)
AICc(mod12)
summary(mod7)
exp(0.246) 

AICc(glm(Hg_blood ~ Site + Sex + SMI + d15N_blood, family = gaussian(link = "identity"), data = data[data$Species == "GBBG",]))
AICc(glm(Hg_blood ~ Site + Sex + SMI + d15N_blood, family = Gamma(link = "inverse"), data = data[data$Species == "GBBG",]))
