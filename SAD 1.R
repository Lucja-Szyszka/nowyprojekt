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

summary(as.factor(data$Species))
summary(as.factor(data$Species_fn))
data <- data[, -which(names(data) == "Species_fn")]
  
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

