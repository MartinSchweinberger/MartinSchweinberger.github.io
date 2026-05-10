mydata <- read.table(choose.files(), header = T, sep = "\t", quote = "", comment.char = "")

install.package("xlsx")
library(xlsx)
#mydataxlsx <- read.xlsx("C:\\03-MyProjects\\03RorLinguists/testdata1.xlsx", 1)
mydataxlsx <- read.xlsx(choose.files(), 1)

write.table(mydata, "C:\\03-MyProjects\\03RorLinguists/testdata2.txt", sep = "\t", row.names = F, col.names = TRUE)

library(xlsx)
write.xlsx(mydata, "C:\\03-MyProjects\\03RorLinguists/testdata2.xlsx")

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

# einlesen der daten
mydata <- read.table("http://martinschweinberger.de/docs/data/dt001.txt", header = F, sep = "\t", quote = "", comment.char = "")
# betiteln
colnames(mydata) <- c("BrE", "AmE")
rownames(mydata) <- c("kindof", "sortof")
# daten betrachten
mydata

# daten visualisieren
par(mfrow=c(1, 2)) # zwei plots in zwei spalten in einem fenster
assocplot(as.matrix(mydata))
mosaicplot(mydata, shade = TRUE, type = "pearson", main = "")
par(mfrow=c(1, 1)) # herstellen der original parameter
# ein plot pro fenster

# testen
chisq.results  <- chisq.test(mydata, corr = F)
# ergebnis ebtrachten
chisq.results

# effektst{\"a}rke berechnen
phi.coefficient = sqrt(chisq.results$statistic / sum(mydata) * (min(dim(mydata))-1))
# ergebnis ebtrachten
phi.coefficient

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

# entfernen aller objekte aus dem gegenw{\"a}rtigen workspace
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
# den vollen namen nageben m{\"u}ssen
attach(slr.data)
# unn{\"o}tige splaten entfernen
slr.data <- as.data.frame(cbind(slr.data$datems, slr.data$P.ptw))
# spaltennamen hinzuf{\"u}gen
colnames(slr.data) <- c("year", "prep.ptw")
# entfernen unvollst{\"a}ndiger datenpunkte
slr.data <- slr.data[!is.na(slr.data$year) == T, ]
# erste zeilen des datensatzes betrachten
head(slr.data)

# struktur des datensatzes betrachten
str(slr.data)

# eigenschaften des datensatzes
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

# entfernen aller objekte aus dem gegenw{\"a}rtigen workspace
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
source("http://martinschweinberger.de/docs/scripts/multiplot_ggplot2.r") # funktion f{\"u}r mehrere plots in einem fenster
source("http://martinschweinberger.de/docs/scripts/slr.summary.tb.r") # funktion zum erstellen sch{\"o}ner summary tabellen

# einladen der daten
g1 <- c(15, 12, 11, 18, 15, 15, 9, 19, 14, 13, 11, 12, 18, 15, 16, 14, 16, 17, 15, 17, 13, 14, 13, 15, 17, 19, 17, 18, 16, 14)
g2 <- c(11, 16, 14, 18, 6, 8, 9, 14, 12, 12, 10, 15, 12, 9, 13, 16, 17, 12, 8, 7, 15, 5, 14, 13, 13, 12, 11, 13, 11, 7)
g <- c(rep("A", length(g1)), rep("B", length(g2)))
sprtestdata <- data.frame(g, c(g1, g2))
# spaltennamen hinzuf{\"u}gen
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
  ylim = c(0, 20), # grenzen der y-achse festlegen
  xlab = c("Gruppen"), # titel der x-achse
  notch = T, # notches einf{\"u}gen
  col = c("lightgreen", "lightblue")) # box einf{\"a}rben
# text darstellen
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

# 1: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$standardized.residuals > 3.29)

# 2: optimal = 1
# (aufgelistete datenpunkte sollten entfernt werden)
stdres_258 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 2.58, 1, 0) } ))
(sum(stdres_258) / length(stdres_258)) * 100

# 3: optimal = 5
# (aufgelistete datenpunkte sollten entfernt werden)
stdres_196 <- as.vector(sapply(mlrdata$standardized.residuals, function(x) {
 ifelse(sqrt((x^2)) > 1.96, 1, 0) } ))
(sum(stdres_196) / length(stdres_196)) * 100

# 4: optimal = 0
# (aufgelistete datenpunkte sollten entfernt werden)
which(mlrdata$cooks.distance > 1)

# 5: optimal = 0
# (datenpunkte sollten entfernt werden, wenn cooks distanz nahe 1 ist)
which(mlrdata$leverage >= (3*mean(mlrdata$leverage)))

# 6: checking autocorrelation:
# Durbin-Watson test (optimal: grosser p-wert)
dwt(m2.mlr)

# 7: multicolliniaritaet testen 1
vif(m2.mlr)

# 8: multicolliniaritaet testen 2
1/vif(m2.mlr)

# 9: mittlerer vif wert sollte nicht groesser als 1 sein
mean(vif(m2.mlr))

# ist die stichprobe ausreichend gross
smplesz(m2.mlr)

# gefahr von beta-fehlern
expR(m2.mlr)

# ergebnisse der mlr betrachten
mlr.summary(m2.mlr, m2.glm, ia = T)

test1 <- read.delim("http://martinschweinberger.de/docs/data/testdata1.txt", header = T, sep = "\t")
bioire <- read.delim("http://martinschweinberger.de/docs/data/BiodataIceIreland.txt", header = T, sep = "\t")

mydata <- test1[order(test1$Variable2),]
par(mfrow = c(2,2)) # um vier grafiken pro fenster anzuzeigen
plot(Variable1 ~ Variable2, type = "p", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'p' (points)")
plot(Variable1 ~ Variable2, type = "l", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'l' (lines)")
plot(Variable1 ~ Variable2, type = "b", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'b' (both points and lines)")
plot(Variable1 ~ Variable2, type = "h", data = mydata, ylab = "Variable1", xlab = "Variable2", main = "plot type 'h' (histogram)")
par(mfrow = c(1,1)) # um eine grafik pro fenster anzuzeigen

plot(Variable1 ~ Variable2, type = "b", data = mydata, ylab = "x-Achse", xlab = "y-Achse", main = "New title", xlim = c(35, 55), ylim = c(30, 120), pch = 3, col = "red", lty = 3, lwd = 2)

plot(Variable1 ~ Variable2, data = mydata, ylab = "length", xlab = "Frequency")
abline(lm(Variable1 ~ Variable2, data = mydata), col = "blue", lty=2)

plot(Variable1 ~ Variable2, data = mydata, ylab = "length", xlab = "Frequency", pch = 16)
abline(lm(Variable1 ~ Variable2, data = mydata), col = "red", lty=3)

newdata <- data.frame(c(rep("female", 50), rep("male", 50)), c(rnorm(n = 50, mean = 20, sd = 10), rnorm(n = 50, mean = 50, sd = 20)))
colnames(newdata) <- c("Gender", "Frequency")

boxplot(Frequency ~ Gender, data = newdata, ylab = "Frequency", xlab = "Gender")

boxplot(Frequency ~ Gender, data = newdata, ylab = "Frequency", xlab = "Gender", notch=TRUE,
 col=(c("lightgreen","lightgrey")))
 
barplot(tapply(newdata$Frequency, newdata$Gender, mean), ylim = c(0,80), ylab = "Frequency", xlab = "Gender", col=(c("lightgreen","lightgrey")))
text(c(0.7,1.9), tapply(newdata$Frequency, newdata$Gender, mean)+5, paste("mean = ", round(tapply(newdata$Frequency, newdata$Gender, mean), 3), sep = ""))
grid()
box()

# independent 2-group t-test
t.test(y~x) # where y is numeric and x is a binary factor

# independent 2-group t-test
t.test(y1,y2) # where y1 and y2 are numeric

# paired t-test
t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric

# independent 2-group Mann-Whitney U Test
wilcox.test(y~x)
# where y is numeric and x is x binary factor

# independent 2-group Mann-Whitney U Test
wilcox.test(y1,y2) # where y1 and y2 are numeric

# dependent 2-group Wilcoxon Signed Rank Test
wilcox.test(y1,y2,paired=TRUE) # where y1 and y2 are numeric

# Kruskal Wallis Test One Way Anova by Ranks
kruskal.test(y~x) # where y1 is numeric and x is a factor

# Randomized Block Design - Friedman Test
friedman.test(y~x|z)
# where y are the data values, x is a grouping factor
# and z is a blocking factor

#############################
###        THE END
#############################
