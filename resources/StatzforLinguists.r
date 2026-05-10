#######################################################
###          DATEN IMPORTIEREN
#######################################################
# raute entfernen, um den code zu aktivieren
#mydata <- read.table(choose.files(), header = T, sep = "\t", quote = "", comment.char = "")

#install.packages("xlsx")
#library(xlsx)
#mydataxlsx <- read.xlsx("C:\\03-MyProjects\\03RorLinguists/testdata1.xlsx", 1)
#mydataxlsx <- read.xlsx(choose.files(), 1)
#######################################################
###          DATEN EXPORTIEREN
#######################################################
#write.table(mydata, "C:\\MyProjects\\StatzForLinguists/testdata.txt", sep = "\t",
#  row.names = F, col.names = TRUE)

#install.packages("xlsx")
#library(xlsx)
#write.xlsx(mydata, "C:\\MyProjects\\StatzForLinguists/testdata.xlsx")
#######################################################
###          MASSE ZENTRALER TENDENZ
#######################################################
# erstellen eines vektors mit zahlen
zahlen <- c(3, 40, 15, 87)
# berechnen des arithmetische mittels
mean(zahlen)

# median
# erstellen eines vektors mit raengen
rangdaten <- c(rep(1, 9), rep(2, 160), rep(3, 70), rep(4, 15), rep(5, 9), rep(6, 57))
# berechnen des medians
median(rangdaten)

# grafik
mydata <- read.table("http://martinschweinberger.de/docs/data/BiodataIceIreland.txt",
  header = T, sep = "\t", quote = "", comment.char = "", skipNul=T)
colnames(mydata) <- gsub("X.", "", colnames(mydata), fixed = T)
colnames(mydata) <- gsub(".$", "", colnames(mydata))
mydata$text.id <- gsub("-.*", "", mydata$text.id)
mydata <- mydata[mydata$text.id == "\"S1A",]
age <- table(mydata$age)
barplot(age, ylim=c(0,200), ylab = "Absolute Häufigkeit (Sprecher)", xlab = "Altersgruppen")
text(seq(0.7, 6.7, 1.2), age+10, cex = 0.85, labels = age)
box()
grid()
# extract median
age <- rep(names(age), age)
age <- gsub("\"", "", age, fixed = T)
m <- ceiling(length(age)/2)
age[m]

# modus/modalwert
# erstellen eines faktors mit kategorien
kategorien <- c(rep("Belfast", 98), rep("Down", 20), rep("Dublin (city)", 110), rep("Limerick", 13), rep("Tipperary", 19))
# berechnen des modalwerts
names(which.max(table(kategorien)))

# grafik
mydata <- read.table("http://martinschweinberger.de/docs/data/BiodataIceIreland.txt", header = T, sep = "\t", quote = "", comment.char = "")
mydata$X.text.id. <- gsub("-.*", "", mydata$X.text.id.)
mydata <- mydata[mydata$X.text.id. == "\"S1A",]
res <- table(mydata$X.reside.)
res <- res[which(res> 10)]
barplot(res, ylim=c(0, 200), ylab = "Absolute Häufigkeit (Sprecher)", xlab = "Current Residence")
text(seq(0.7, 6.7, 1.2), res+10, cex = 0.85, labels = res)
box()
grid()

# anmerkungen
k1 <- c(5.2, 11.4, 27.1, 13.7, 9.6)
k2 <- c(0.2, 0.0, 1.1, 93.7, 0.4)
mean(k1)

mean(k1)

# tabelle der werte erstellen
k <- rbind(k1, k2)
colnames(k) <- c("A", "B", "C", "D", "E")
rownames(k) <- c("Korpus 1", "Korpus 2")
test <- barplot(k, ylab = "Relative Häufigkeit (pro 1,000 Wörter)", xlab = "Sprecher", 
  col = c("lightgreen", "lightblue"), beside=TRUE, ylim = c(0, 100), legend = rownames(k))
p <- c(1.5, 2.5, 4.5, 5.5, 7.5, 8.5, 10.5, 11.5, 13.5, 14.5)      
w <- c(5.2, 0.2, 11.4, 0.0, 27.1, 1.1, 13.7, 93.7, 9.6, 0.4)
text(p, w+5, cex = 0.85, labels = w)
box()
grid()

medianzent1 <- k1[order(k1)]
medianzent2 <- k2[order(k2)]

#######################################################
###          STREUUNGSMAßE
#######################################################
# erstellen eines vektors mit zahlen
StadtA <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1)
# berechnen der varianz
sd(StadtA)^2

# grafik
s1 <- c(-5, -12, 5, 12, 15, 18, 22, 23, 20, 16, 8, 1)
s2 <- c(7, 7, 8, 9, 10, 13, 15, 15, 13, 11, 8, 7)
mean(s1)

mean(s2)

plot(s1, axes = F, type = "l", ylab = "Temperatur in °Celsius", xlab = "Monate", lty = 1, col = "red")
lines(s2, lty = 2, col = "blue")
lx <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", 
  "September", "Oktober", "November", "Dezember")
ly <- c(-10, -5, 0, 5, 10, 15, 20)
lyt <- c("-10", "-5", "0", "5", "10", "15", "20")
axis(1, at=1:12, labels=lx)
axis(2, at = ly, labels = lyt)
legend("topleft", inset=.05, c("Stadt A","Stadt B"), lty = c(1, 2), 
  col = c("red", "blue"), horiz=TRUE)
box()
grid()

# Aufgaben
A <- c(1, 3, 6, 2, 1, 1, 6, 8, 4, 2, 3, 5, 0, 0, 2, 1, 2, 1, 0)
B <- c(3, 2, 5, 1, 1, 4, 0, 0, 2, 3, 0, 3, 0, 5, 4, 5, 3, 3, 4)
#Mittelwert
mean(A)

mean(B)

#Median
median(A)

median(B)

#Modalwert
names(which.max(table(A)))

names(which.max(table(B)))

#Standardabweichung
sd(A)

sd(B)

#######################################################
###          WAHRSCHEINLICHKEITEN
#######################################################
# wahrscheinlichkeiten: 0, 1, 2 und 3 mal kopf
# bei drei wuerfen
dbinom(0:3, 3, 0.5)
# wahrscheinlichkeiten: 2 oder 3 mal kopf
# bei drei wuerfen
sum(dbinom(2:3, 3, 0.5))
# wahrscheinlichkeiten: 100 mal kopf
# bei 100 wuerfen
dbinom(100, 100, 0.5)
# wahrscheinlichkeit: 58 mal oder mehr kopf
# bei 100 wuerfen
sum(dbinom(58:100, 100, 0.5))
# wahrscheinlichkeit: 59 mal oder mehr kopf
# bei 100 wuerfen
sum(dbinom(59:100, 100, 0.5))
# wo sinkt die wahrscheinlichkeit unter 5 prozent
# bei 100 wuerfen kopf zu erhalten
qbinom(0.05, 100, 0.5, lower.tail=FALSE)

qnorm(0.05, lower.tail=TRUE)

#######################################################
###          SIGNIFIKANZ
#######################################################
# load packages
library(grDevices)
# set up data
p100 <- dbinom(0:100, 100, 0.5)
w100 <- c(0:100)
wtb <- rbind(w100, p100)
colnames(wtb) <- c(0:100)
rownames(wtb) <- c("anzahl (kopf)", "prob")
xseq <- seq(0.7, 120.7, 1.2)
xseq <- xseq[c(1, 20, 40, 60, 80, 100)]
# start plotting
par(mfrow = c(3,1))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, col = "lightgrey")
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
# summ probs
sp <- cumsum(wtb[2,])
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.025, TRUE,
    ifelse(x >= 0.975, TRUE, FALSE))
    })))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, , col = ifelse(ttsp == T, "red", "lightgrey"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
text(75, 0.06, expression(paste(mu^1 !=  mu^2, sep = "")))
###
ttsp <- as.vector(unlist(sapply(sp, function(x){
  x <- ifelse(x <= 0.95, TRUE, FALSE)
    })))
barplot(wtb[2,], ylim = c(0, 0.08), ylab = "Probability", xlab = "Anzahl (Kopf)",
  axes = F, axisnames = F, col = ifelse(ttsp == T, "lightgrey", "red"))
axis(1, at= xseq, labels= c("0", "20", "40", "60", "80", "100"))
axis(2, at= c(0.0, 0.02, 0.04, 0.06, 0.08), labels= c("0.00", "", "0.04", "", "0.08"))
box()
grid()
text(75, 0.06, expression(paste(mu^1 >=  mu^2, sep = "")))
par(mfrow = c(1,1))
#############################################
# Normal Distribution
#############################################
par(mfrow = c(3,1))
### normal distribution: Mu=0, Sigma=1: Standard normal
library(graphics)
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(0, 0), c(-1, 0.5), col = "lightgrey")
lines(c(-1:1), c(rep(0.3, 3)), col="red", lty=2)
text(0, 0.35, "68%")
lines(c(-2:2), c(rep(0.2, 5)), col="blue", lty=2)
text(0, 0.25, "95%")
lines(c(-3:3), c(rep(0.1, 7)), col="green", lty=2)
text(0, 0.15, "99.7%")
lines(c(-1, -1), c(-1, 0.5), col = "red")
lines(c(1, 1), c(-1, 0.5), col = "red")
lines(c(-2, -2), c(-1, 0.5), col = "blue")
lines(c(2, 2), c(-1, 0.5), col = "blue")
lines(c(-3, -3), c(-1, 0.5), col = "green")
lines(c(3, 3), c(-1, 0.5), col = "green")
###
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.96, 1.96), c(0, 0.5), col="lightgrey", lty=2)
text(2.25, 0.15, "2.5%")
text(2.25, 0.085, "(sd=1.96)")
lines(c(-1.96, -1.96), c(0, 0.5), col="lightgrey", lty=2)
text(-2.25, 0.15, "2.5%")
text(-2.25, 0.085, "(sd=-1.96)")
###
plot(dnorm, -4, 4, axes = F, xlab = "Standard Deviations", ylab = "Probability")
axis(1, at = seq(-4, 4, 1), labels = seq(-4, 4, 1))
axis(2, at= c(0.0, 0.2, 0.4), labels= c("0.0", "0.2", "0.4"))
text(3, 0.2, expression(paste(mu, "=0, ", sigma^2, "=1", sep = "")))
box()
lines(c(1.64, 1.64), c(0, 0.5), col="lightgrey", lty=2)
text(2.2, 0.15, "5% (sd=1.64)")

par(mfrow = c(1,1))

#plot(p, axes = F, xlab = "Standard Deviations", ylab = "Probability")
#######################################################
###              AUFGABEN
#######################################################
# bei sieben wuerfen genau drei mal kopf
dbinom(3, 7, 0.5)

# wahrscheinlichkeiten: 2 oder 5 mal kopf bei sieben wuerfen
sum(dbinom(c(2, 5), 7, 0.5))

# wahrscheinlichkeiten: 5 oder mehr mal kopf bei sieben wuerfen
sum(dbinom(5:7, 7, 0.5))

# wahrscheinlichkeiten: zwischen drei und sechs mal kopf bei sieben wuerfen
sum(dbinom(3:6, 7, 0.5))

#######################################################
###          DER CHI-QUADRAT TEST IN R
#######################################################
# einlesen der daten
mydata <- read.table("http://martinschweinberger.de/docs/data/dt001.txt", header = F, sep = "\t", quote = "", comment.char = "")
# betiteln
colnames(mydata) <- c("BrE", "AmE")
rownames(mydata) <- c("kindof", "sortof")
# daten betrachten
mydata

# grafisceh darstellung
# daten visualisieren
par(mfrow=c(1, 2)) # zwei plots in zwei spalten in einem fenster
assocplot(as.matrix(mydata))
mosaicplot(mydata, shade = TRUE, type = "pearson", main = "")
par(mfrow=c(1, 1)) # herstellen der original parameter ein plot pro fenster

# testen
chisq.results <- chisq.test(mydata, corr = F)
# ergebnis ebtrachten
chisq.results

# effektstaerke berechnen
phi.coefficient = sqrt(chisq.results$statistic / sum(mydata) * (min(dim(mydata))-1))
# ergebnis ebtrachten
phi.coefficient

#######################################################
###                AUFGABEN
#######################################################
chi <- matrix(c(61, 43, 42, 36), ncol = 2, byrow = T)
colnames(chi) <- c("1SGPN", "PNohne1SG")
rownames(chi) <- c("Jung", "Alt")
chi

# grafisceh darstellung
# daten visualisieren
par(mfrow=c(1, 2)) # zwei plots in zwei spalten in einem fenster
assocplot(as.matrix(chi))
mosaicplot(chi, shade = TRUE, type = "pearson", main = "")
par(mfrow=c(1, 1)) # herstellen der original parameter ein plot pro fenster

# testen
chisq.results <- chisq.test(chi, corr = F)
# ergebnis ebtrachten
chisq.results

# effektstaerke berechnen
phi.coefficient = sqrt(chisq.results$statistic / sum(chi) * (min(dim(chi))-1))
# ergebnis ebtrachten
phi.coefficient

#######################################################
###          ALTERNATIVE CHI2 TESTS
#######################################################
source("http://martinschweinberger.de/docs/scripts/x2.2k.r")
# daten generieren
chitb2 <- matrix(c(21, 14, 18, 13, 24, 12, 13, 30), byrow = T, nrow = 4)
colnames(chitb2) <- c("erreicht", "nichterreicht")
rownames(chitb2) <- c("rweich", "rhart", "beta", "licht")
# teiltabelle extrahieren
chitb3 <- matrix(c(21, 14, 18, 13), byrow = T, nrow = 2)
colnames(chitb3) <- c("erreicht", "nichterreicht")
rownames(chitb3) <- c("rweich", "rhart")
# einfacher x2-test
chisq.test(chitb3, corr = F)

x2.2k(chitb2, 1, 2)

# SUBTABLES
x <- matrix(c(8, 31, 44, 36, 5, 14, 25, 38, 4, 22, 17, 12, 8, 11, 16, 24), ncol=4)
attr(x, "dimnames")<-list(Register=c("acad", "spoken", "fiction", "new"),
Metaphor = c("Heated fluid", "Light", "NatForce", "Other"))

subtable <- matrix(c(14, 25, 22, 17), ncol=2)
chisq.results <- chisq.test(subtable, correct=FALSE) # WRONG!
phi.coefficient = sqrt(chisq.results$statistic / sum(subtable) * (min(dim(subtable))-1))
chisq.results
phi.coefficient

source("http://martinschweinberger.de/docs/scripts/sub.table.r") # funktion zur analyse von untertabellen
results <- sub.table(x, 2:3, 2:3, out="short")
results

#######################################################
###          EINFACHE LINEARE REGRESSION
#######################################################
# Scatterplot with lines from teh regression line to the dots
x <- c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187)
y <- c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92) # plot scatterplot and the regression line
mod1 <- lm(y ~ x)
# drei plots in drei spalten in einem fenster
par(mfrow=c(1, 3))
# ein plot pro fenster
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10))
abline(mod1, lwd=2)
# calculate residuals and predicted values
res <- signif(residuals(mod1), 5)
res <- round(res, 1)
pre <- predict(mod1) # plot distances between points and the regression line
segments(x, y, x, pre, col="red")
# add labels (res values) to points
#install.packages("calibrate")
library(calibrate)
textxy(x, y, res, cex=1)
# herstellen der original parameter
par(mfrow=c(1, 1))

##########################################################
# entfernen aller objekte aus dem aktuellen workspace
rm(list=ls(all=T))
# installieren der notwendigen pakete (falls nicht schon geschehen)
# (um die befehle zu aktivieren # entfernen)
#install.packages("QuantPsyc")
#install.packages("car")
# pakete initialisieren
library(QuantPsyc)
library(car)
library(ggplot2)
source("http://martinschweinberger.de/docs/scripts/multiplot_ggplot2.r") # funktion um mehrere grafiken in einem fenster anzuzeigen
source("http://martinschweinberger.de/docs/scripts/slr.summary.tb.r") # funktion zum erstellen von summary tabellen

# laden der daten
slr.data <- read.delim("http://martinschweinberger.de/docs/data/slr.data.txt", header = TRUE)
# wir attachen den datensatz, sodass wir nicht immer
# den vollen namen nageben muessen
attach(slr.data)
# unnoetige spalten entfernen
slr.data <- as.data.frame(cbind(slr.data$datems, slr.data$P.ptw))
# spaltennamen hinzufuegen
colnames(slr.data) <- c("year", "prep.ptw")
# entfernen unvollstaendiger datenpunkte
slr.data <- slr.data[!is.na(slr.data$year) == T, ]
# erste zeilen des datensatzes betrachten
head(slr.data)

# struktur des datensatzes betrachten
str(slr.data)

# eigenschaften des datensatzes betrachten
summary(slr.data)

# visualisieren der daten
p2 <- ggplot(slr.data, aes(year, prep.ptw)) +
 geom_point() +
 labs(x = "Year") +
 labs(y = "Prepositions per 1,000 words") +
 geom_smooth()

p3 <- ggplot(slr.data, aes(year, prep.ptw)) +
 geom_point() +
 labs(x = "Year") +
 labs(y = "Prepositions per 1,000 words") +
 geom_smooth(method = "lm") # with linear model smoothing!

multiplot(p2, p3, cols = 2)

# skalieren der jahreszahlen
slr.data$prep.ptw <- slr.data$prep.ptw - mean(slr.data$prep.ptw)

# Simples Lineares Regressionsmodel erstellen
prep.lm <- lm(prep.ptw ~ year, data = slr.data)
# ergebnisse betrachten
summary(prep.lm)

# graphik parameter: 3 plots in einer reihe in einem fenster
par(mfrow = c(1, 3))
plot(resid(prep.lm))
plot(rstandard(prep.lm))
plot(rstudent(prep.lm))
par(mfrow = c(1, 1)) # wiederherstellen der originalparameter

# generiere eine 2x2 matrize diagnostischer grafiken
par(mfrow = c(2, 2))
plot(prep.lm)
par(mfrow = c(1, 1))

# ergebnisse tabellieren
slr.summary(prep.lm)

##########################################################
# BEISPIEL 2
# entfernen aller objekte aus dem aktuellen workspace
rm(list=ls(all=T))
# installieren der notwendigen pakete
# (falls nicht schon geschehen)
# (um die befehle zu aktivieren # entfernen)
#install.packages("QuantPsyc")
#install.packages("car")
# pakete initialisieren
library(QuantPsyc)
library(car)
library(ggplot2)
source("http://martinschweinberger.de/docs/scripts/multiplot_ggplot2.r") # mehrere ggplots in einem fenster
source("http://martinschweinberger.de/docs/scripts/slr.summary.tb.r") # funktion zum erstellen von summary tabellen
# einladen der daten
g1 <- c(15, 12, 11, 18, 15, 15, 9, 19, 14, 13, 11, 12, 18, 15, 16, 14, 16, 17, 15, 17, 13, 14, 13, 15, 17, 19, 17, 18, 16, 14)
g2 <- c(11, 16, 14, 18, 6, 8, 9, 14, 12, 12, 10, 15, 12, 9, 13, 16, 17, 12, 8, 7, 15, 5, 14, 13, 13, 12, 11, 13, 11, 7)
g <- c(rep("A", length(g1)), rep("B", length(g2)))
sprtestdata <- data.frame(g, c(g1, g2))
# spaltennamen hinzufuegen
colnames(sprtestdata) <- c("gruppe", "punkte")
# erste zeilen des datensatzes betrachten
head(sprtestdata)

# struktur des datensatzes betrachten
str(sprtestdata)

# eigenschaften des datensatzes betrachten
summary(sprtestdata)

# erstelle boxplot
boxplot(punkte ~ gruppe,
  data = sprtestdata, # the data we want to display
  main = "", # you could specify a title here
  ylab = "Punkte", # titel der y-achse
  ylim = c(0, 20), #
  xlab = c("Gruppen"), # titel der x-achse
  notch = T, # notches einf{\"u}gen
  col = c("lightgreen", "lightblue")) # boxplots unterschiedlich einf{\"a}rben

text(1:2,
  c(4.0, 4.0),
  cex = 0.85,
  labels = paste("mean\n",
  c(round(as.vector(by(sprtestdata$punkte, sprtestdata$gruppe, mean))[1], 2),
    round(as.vector(by(sprtestdata$punkte, sprtestdata$gruppe, mean))[2], 2),
    sep = "")))
rug(jitter(sprtestdata$punkte),
  side=4)
grid()
box()

# Simples Lineares Regressionsmodel erstellen
sprtest.lm <- lm(punkte ~ gruppe, data = sprtestdata)
# ergebnisse betrachten
summary(sprtest.lm)

# graphik parameter setzen: 3 plots in einer reihe
par(mfrow = c(1, 3))
plot(resid(sprtest.lm))
plot(rstandard(sprtest.lm))
plot(rstudent(sprtest.lm))
par(mfrow = c(1, 1)) # wiederherstellen der originalparameter

# generiere eine 2x2 matrize diagnostischer grafiken
par(mfrow = c(2, 2))
plot(sprtest.lm)
par(mfrow = c(1, 1))

# ergebnisse tabellieren
slr.summary(sprtest.lm)

#######################################################
###          MULTIPLE LINEAR REGRESSION
#######################################################
# entfernen aller objekte aus dem gegenw{\"a}rtigen workspace
rm(list=ls(all=T))
# installieren der notwendigen pakete
# (falls nicht schon geschehen)
# (um die befehle zu aktivieren # entfernen)
#install.packages("rms")
#install.packages("glmulti")
#install.packages("lmtest")
#install.packages("MASS")
#install.packages("QuantPsyc")
#install.packages("car")
#install.packages("ggplot2")
# pakete initialisieren
#library(rms)
#library(glmulti)
#library(lmtest)
#library(MASS)
library(car)
library(QuantPsyc)
library(boot)
library(ggplot2)
source("http://martinschweinberger.de/docs/scripts/multiplot_ggplot2.r")
source("http://martinschweinberger.de/docs/scripts/mlinr.summary.r")
source("http://martinschweinberger.de/docs/scripts/SampleSizeMLR.r")
source("http://martinschweinberger.de/docs/scripts/ExpR.r")
# optionen festlegen
options("scipen" = 100, "digits" = 4)

# daten laden
mlrdata <- read.delim("http://martinschweinberger.de/docs/data/mlrdata.txt", header = TRUE)
# ersten zeilen der daten betrachten
head(mlrdata)

# struktur der daten betrachten
str(mlrdata)

# zusammenfassung der daten betrachten
summary(mlrdata)

p1 <- ggplot(mlrdata, aes(status, money)) + 
 geom_boxplot(notch = T, aes(fill = factor(status))) +
 scale_fill_brewer() +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE) +
 ggtitle("Status") 
p2 <- ggplot(mlrdata, aes(attraction, money)) + 
 geom_boxplot(notch = T, aes(fill = factor(attraction))) +
 scale_fill_brewer() +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE) +
 ggtitle("Attraction")
p3 <- ggplot(mlrdata, aes(x = money)) + 
 geom_histogram(aes(y=..density..),
 binwidth = 10,
 colour = "black", fill = "white") +
 geom_density(alpha=.2, fill = "#FF6666") # Overlay with transparent density plot
p4 <- ggplot(mlrdata, aes(status, money)) + 
 geom_boxplot(notch = F, aes(fill = factor(status))) +
 scale_fill_brewer(palette="Paired") +
 facet_wrap(~ attraction, nrow = 1) +
 theme_bw() + # backgroud white(inactive to default grey)
 labs(x = "") +
 labs(y = "Money spent on present (Euro)") +
 coord_cartesian(ylim = c(0, 250)) +
 guides(fill = FALSE) 
# Plot the plots
multiplot(p1, p3, p2, p4, cols = 2)

# generieren der minimalen baselinemodelle, die nur den 
# intercept (mittelwert) als unabh. variable beinhalten
m0.mlr = lm(money ~ 1, data = mlrdata) # baseline model
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata) 
# ergebnisse betrachten
summary(m0.mlr)

# ergebnisse betrachten
summary(m0.glm)

#############################
# generieren der saturated models, die alle 
# unabh. variablen und interaktionen beinhalten
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m1.mlr)

# ergebnisse betrachten
summary(m1.glm)

# automatisches modelfitting
# kriterium: AIC (um so kleiner umso besser)
step(m1.mlr, direction = "both")

# minimales adequates modell generieren
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ (status + attraction)^2, family = gaussian, data = mlrdata)

# zusammenfassugn der modellergebnisse betrachten
summary(m2.mlr)

# konfidenzintervalle der koeffizineten
confint(m2.mlr)

# vergleich zwiscehn dem baseline-modell und dem minimal adequate model
anova(m0.mlr, m2.mlr)

Anova(m0.mlr, m2.mlr, type = "III")

# suche nach problematischen datenpunkten
# erzeugen diagnostischer grafiken
par(mfrow = c(3, 2))
plot(m2.mlr)
qqPlot(m2.mlr, main="QQ Plot")
# Cooks D plot
# D-werte > 4/(n-k-1) sind problematisch
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
plot(m2.mlr, which=4, cook.levels = cutoff)
par(mfrow = c(1, 1))

# entfernen zu einflussreicher datenpunkte
# um dies zu tun extrahieren wir diagnostische 
# werte zu allen datenpunkten und addieren die
# spalten mit diesen werten zu unserem
# datensatz hinzu
infl <- influence.measures(m2.mlr)

# addieren der einflussstatistiken zu dme datensatz
mydata <- data.frame(mlrdata, infl[[1]], infl[[2]])
head(mydata)

# zu einflussreiche datenpunkte erkennen
remove <- apply(infl$is.inf, 1, function(x) {
 ifelse(x == TRUE, return("remove"), return("keep")) } )

# informationen zu den zu einflussreichen datenpunkten 
# zum datensatz hinzuaddieren  
mlrdata <- data.frame(mlrdata, remove)

# zeilenzahl des alten datensatzes anzeigen
nrow(mydata)

mlrdata <- mlrdata[mlrdata$remove == "keep", ] 

# zeilenzahl des neuen datensatzes anzeigen
nrow(mlrdata)

# generieren der minimalen baselinemodelle, die nur den 
# intercept (mittelwert) als unabh. variable beinhalten
m0.mlr = lm(money ~ 1, data = mlrdata) # baseline model
m0.glm = glm(money ~ 1, family = gaussian, data = mlrdata) 
# ergebnisse betrachten
summary(m0.mlr)

# ergebnisse betrachten
summary(m0.glm)

#############################
# generieren der saturated models, die alle 
# unabh. variablen und interaktionen beinhalten
m1.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m1.glm = glm(money ~ status * attraction, family = gaussian, data = mlrdata)
# ergebnisse betrachten
summary(m1.mlr)

# ergebnisse betrachten
summary(m1.glm)

#############################
# automatisches modelfitting
# kriterium: AIC (um so kleiner umso besser)
step(m1.mlr, direction = "both")

# minimales adequates modell generieren
m2.mlr = lm(money ~ (status + attraction)^2, data = mlrdata)
m2.glm = glm(money ~ (status + attraction)^2, family = gaussian, data = mlrdata)

# zusammenfassung der modellergebnisse betrachten
summary(m2.mlr)

# konfidenzintervalle der koeffizineten
confint(m2.mlr)

# vergleich zwiscehn dem baseline-modell und dem minimal adequate model
anova(m0.mlr, m2.mlr)

Anova(m0.mlr, m2.mlr, type = "III")

# suche nach problematischen datenpunkten
# erzeugen diagnostischer grafiken
par(mfrow = c(3, 2))
plot(m2.mlr)
qqPlot(m2.mlr, main="QQ Plot")
# Cooks D plot
# D-werte > 4/(n-k-1) sind problematisch
cutoff <- 4/((nrow(mlrdata)-length(m2.mlr$coefficients)-2))
plot(m2.mlr, which=4, cook.levels = cutoff)
par(mfrow = c(1, 1))

# addieren von modelldiagnostiken zum datasatz
mlrdata$residuals <- resid(m2.mlr)
mlrdata$standardized.residuals <- rstandard(m2.mlr)
mlrdata$studentized.residuals <- rstudent(m2.mlr)
mlrdata$cooks.distance <- cooks.distance(m2.mlr)
mlrdata$dffit <- dffits(m2.mlr)
mlrdata$leverage <- hatvalues(m2.mlr)
mlrdata$covariance.ratios <- covratio(m2.mlr)
mlrdata$fitted <- m2.mlr$fitted.values
# erstellen diagnostischer grafiken 
# (drei grafiken in einem fenster)
p1 <- histogram<-ggplot(mlrdata, aes(studentized.residuals)) +
 theme(legend.position = "none") +
 geom_histogram(aes(y=..density..),
 binwidth = 1,
 colour="black",
 fill="white") +
 labs(x = "Studentized Residual", y = "Density")
p2 <- histogram + stat_function(fun = dnorm, args = list(mean = mean(mlrdata$studentized.residuals, na.rm = TRUE), sd = sd(mlrdata$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
p3 <- scatter <- ggplot(mlrdata, aes(fitted, studentized.residuals))
p4 <- scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Fitted Values", y = "Studentized Residual")
p5 <- qqplot.resid <- qplot(sample = mlrdata$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values")
p6 <- qqplot.resid
multiplot(p2, p4, p6, cols=3)
# 1: optimal = 0 (all cases listed must be removed)
which(mlrdata$standardized.residuals > 3.29)

# 2: optimal = 1
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100

# 3: optimal = 5
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100

# 4: optimal = 0 (all cases listed must be removed)
which(mlrdata$cooks.distance > 1)

# 5: optimal = 0 (all cases listed must be
# removed IF cook's distance close to 1)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))

# 6: checking autocorrelation:
# perform Durbin-Watson test (optimal: large p-value)
dwt(m2.mlr)

# 7: checking multicolliniarity: 
vif(m2.mlr)

# 8: checking multicolliniarity: tolerance is 1/VIF 
# values smaller than .01 should/must be excluded

1/vif(m2.mlr)

# 9: mean VIF: should not be greater than 1
mean(vif(m2.mlr))

# ist die stichprobe ausreichend gross
smplesz(m2.mlr)

# gefahr von beta-fehlern
expR(m2.mlr)

# ergebnisse der mlr betrachten
mlr.summary(m2.mlr, m2.glm, ia = T) 

################################################
# Mixed-Effcets Modelle
################################################
# random intercepts and random slops
x <- 0:10
y = 0:10
png("C:\\01-University\\03-Lehre\\00-Workshops\\StatzforLinguistiswithR\\images/mem02.png", width = 800, height = 400) # save plot
# start plot
par(mfrow = c(1, 4))
# intercepts
plot(x, y, type = "n", xlim = c(0, 10), ylim = c(-5, 10))
abline(0, 1, lty = 1, col ="black")
box()
grid()
# random intercepts
plot(x, y, type = "n", xlim = c(0, 10), ylim = c(-5, 10))
abline(4, 1, lty = 2, col ="black")
abline(2, 1, lty = 2, col ="black")
abline(2, 1, lty = 2, col ="black")
abline(0, 1, lty = 2, col ="black")
abline(-1, 1, lty = 2, col ="black")
abline(-2, 1, lty = 2, col ="black")
abline(-4, 1, lty = 2, col ="black")
box()
grid()
# random slopes
plot(x, y, type = "n", xlim = c(0, 10), ylim = c(-5, 10))
abline(0, 1.75, lty = 2, col ="black")
abline(0, 1.5, lty = 2, col ="black")
abline(0, 1.25, lty = 2, col ="black")
abline(0, 0, lty = 2, col ="black")
abline(0, -.25, lty = 2, col ="black")
abline(0, -.5, lty = 2, col ="black")
abline(0, -.75, lty = 2, col ="black")
box()
grid()
# random slopesund random intercepts
plot(x, y, type = "n", xlim = c(0, 10), ylim = c(-5, 10))
abline(2, 1.75, lty = 2, col ="black")
abline(-1, 1.5, lty = 2, col ="black")
abline(1, 1.25, lty = 2, col ="black")
abline(4, 0, lty = 2, col ="black")
abline(-4, -.25, lty = 2, col ="black")
abline(0, -.5, lty = 2, col ="black")
abline(-1, -.75, lty = 2, col ="black")
box()
grid()
# restore original graphic's parameters
par(mfrow = c(1, 1))
dev.off()
################################################
x <- c(169, 176, 164, 160, 158, 173, 166, 161, 180, 187, 170, 177, 163, 161, 157)
y <- c(68, 72, 65, 62, 60, 80, 75, 70, 85, 92, 88, 92, 85, 82, 80) # plot scatterplot and the regression line
z <- c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "c", "c", "c", "c", "c")
tb <- data.frame(x,y, z)
a <- tb[z == "a", ]
a <- a[, 1:2]
b <- tb[z == "b", ]
b <- b[, 1:2]
c <- tb[z == "c", ]
c <- c[, 1:2]
d <- tb[, 1:2]
# plot
png("C:\\01-University\\03-Lehre\\00-Workshops\\StatzforLinguistiswithR\\images/mem01.png", width = 680, height = 400) # save plot
par(mfrow = c(1, 3))
# plot 1
plot(a, xlim = c(150, 200), ylim = c(50, 100))
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
grid()
# plot 2
plot(a, xlim = c(150, 200), ylim = c(50, 100))
mod0 <- lm(d$y ~ d$x, data = d)
abline(mod0, lty=1, col = "black")
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
grid()
# plot 3
plot(a, xlim = c(150, 200), ylim = c(50, 100))
grid()
mod1 <- lm(a$y ~ a$x, data = a)
abline(mod0, lty=1, col = "black")
abline(mod0[[1]][[1]]+10, mod0[[1]][[2]], lty = 2, col = "red")
abline(mod0[[1]][[1]]-10, mod0[[1]][[2]], lty = 3, col = "blue")
abline(mod0[[1]][[1]]-1, mod0[[1]][[2]], lty = 4, col = "green")
text(b[,1], b[,2], "+")
text(c[,1], c[,2], "*")
grid()
par(mfrow = c(1, 1))
dev.off()
################################################
