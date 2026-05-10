##################################################################
### --- R script "Intensifiers in New Zealand English"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- CONTACT
### --- martin.schweinberger.hh@gmail.com
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 20156. "Intensifiers in New Zealand English", unpublished R script, Hamburg University.
###############################################################
###                   START
###############################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages
#install.packages("stringr")
library(tm)
library(stringr)
library(plyr)
library(car)
library(QuantPsyc)
library(plyr)
library(rms)
library(gsubfn)
library(reshape)
library(zoo)
#library(ggplot2)
library(cfa)
library(Hmisc)
library(lme4)
library(languageR)
library(syuzhet)
source("C:\\R/POStagObject_02.R") # for pos-tagging objects
source("C:\\R/multiplot_ggplot2.R") # for multiple ggplot2 plots in one window
source("C:\\R/ConcR_2.4_LoadedFiles.R") # for concordancing
source("C:\\R/PseudoR2lmerBinomial.R")
source("C:\\R/mlr.summary.R")
source("C:\\R/blr.summary.R")
source("C:\\R/meblr.summary.R")
source("C:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSD.R") # for Mixed Effects Model fitting (step-wise step-down): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSULogReg.R") # for Fixed Effects Model fitting: Binary Logistic Models
###############################################################
# Setting options
options(stringsAsFactors = F)
options(scipen = 999)
# define image directors
imageDirectory<-"C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images"
# Specify pathnames of the corpra
corpus.ire <- "C:\\Corpora\\original\\ICE New Zealand\\Spoken"
bio.path <- "C:\\Corpora\\original\\00-Metadata/BiodataIceNewZealand.txt"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.ire, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- sapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  x <- paste(x, sep = " ", collapse = " ")
  x <- strsplit(gsub("(<I>)", "~\\1", x), "~" )
  x <- unlist(x)
  x <- x[2:length(x)]
  x <- gsub("<I> ", "", x)
  x <- as.vector(unlist(x))
  } )
sf <- as.vector(unlist(sapply(corpus.tmp, function(x){
  x <- length(x)
  x <- str_replace_all(x, "2","1 2")
  x <- str_replace_all(x, "3","1 2 3")
  x <- str_replace_all(x, "4","1 2 3 4")
  x <- strsplit(x, " ")
  })))
corpus.tmp01 <- unlist(corpus.tmp)
fl <- as.vector(unlist(sapply(corpus.tmp01, function(x){
  x <- gsub("\\#.*", "", x)
  x <- gsub(".*:", "", x)
  })))
suspk <- lapply(corpus.tmp01, function(x){
  x <- strsplit(gsub("(<ICE-NZ:[A-Z][0-9][A-Z])", "~\\1", x), "~" )
  x <- unlist(x)
  x <- x[2:length(x)]
  } )
l <- as.vector(unlist(sapply(suspk, function(x){
  x <- length(x)
  })))
fln <- rep(fl, l)
sfn <- rep(sf, l)
suspkn <- as.vector(unlist(suspk))
spk <- as.vector(unlist(sapply(suspkn, function(x){
  x <- gsub(">.*", "", x)
  x <- gsub(".*\\:", "", x)
  })))
# create a df out of the vectors
df <- data.frame(fln, sfn, spk, suspkn)
# create a full-id vector
flid <- as.vector(unlist(apply(df, 1, FUN=function(x){
  x <- paste("<", x[1], ":", x[2], "$", x[3], ">", sep = "", collapse = "")
  } )))
suspknn <- as.vector(unlist(sapply(suspkn, function(x){
  x <- sub(" ", " <#>", x)
  x <- gsub("<#><#>", "<#>", x)
  })))
su <- sapply(suspknn, function(x){
  x <- strsplit(gsub("(<#>)", "~\\1", x), "~" )
  })
sun <- sapply(su, function(x){
  x <- x[2:length(x)]
  })
sunn <- as.vector(unlist(sun))
# extract the number of speech units per turn
n <- as.vector(unlist(sapply(sun, function(x){
  x <- length(x)
  })))
# create a clean vector of the speech units
sucl <- as.vector(unlist(sapply(sunn, function(x){
  x <- gsub("<#>", " ", x)
  x <- gsub("</{0,1}\\?>", " ", x)
  x <- gsub("<&> {0,1}.* {0,1}</&>", " ", x)
  x <- gsub("<O> {0,1}.* {0,1}</O>", " ", x)
  x <- gsub("<unclear> {0,1}.* {0,1}</unclear>", " ", x)
  x <- gsub("<.{0,1}/{0,1}.{0,1}[A-Z]{0,1}[a-z]{1,}>", " ", x)
  x <- gsub("<,{1,3}>", " ", x)
  x <- gsub("<\\.>[a-z]{1,}</\\.>", " ", x)
  x <- gsub("</{0,1}\\{[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}[0-9]{0,1}\\[{0,1}\\{{0,1}[0-9]{0,2}>", " ", x)
  x <- gsub("<\\[/[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\[[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\}[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\][0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\.[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}[A-Z]{0,2}[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}[a-z]{1,}=[A-Z]{0,1}[a-z]{1,}>", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  })))
# test if non-words are still present
#t1 <- as.vector(unlist(sapply(sunn, function(x) {x <- grep("<indig", x) } )))
#t1 <- sucl[grep("<", sucl)]
# additional cleaning
#sucl <- as.vector(unlist(sapply(sucl, function(x){
#  x <- gsub("<&> laughter <&>", " ", x)
#  x <- gsub("<", " ", x)
#  x <- gsub("&", " ", x)
#  x <- gsub(">", " ", x)
#  x <- gsub("/", "", x)
#  x <- gsub("[", " ", x, fixed = T)
#  x <- gsub("{", " ", x, fixed = T)
#  x <- gsub("?", "", x, fixed = T)
#  x <- gsub(" {2,}", " ", x)
#  x <- str_trim(x, side = "both")
#  })))
# rep vectors number of times of the speech units per turn
flnn <- rep(fln, n)
sfnn <- rep(sfn, n)
spkn <- rep(spk, n)
flidn <- rep(flid, n)
df2 <- data.frame(flidn, flnn, sfnn, spkn, sunn, sucl)
#t1 <- df2$sucl[grep("\\[", df2$sucl)] #test
# save data before pos tagging
write.table(df2, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/IntNZE-ISLE-data.txt", sep = "\t", row.names = F, col.names = T)
# read data for pos tagging
df2 <- read.table("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/IntNZE-ISLE-data.txt", sep = "\t", header = T)
# remove empty speech units
df2 <- df2[df2$sucl != "",]
# split data into smaller chunks
pos01 <- df2$sucl[1:5000]
pos02 <- df2$sucl[5001:10000]
pos03 <- df2$sucl[10001:15000]
pos04 <- df2$sucl[15001:20000]
pos05 <- df2$sucl[20001:25000]
pos06 <- df2$sucl[25001:30000]
pos07 <- df2$sucl[30001:35000]
pos08 <- df2$sucl[35001:40000]
pos09 <- df2$sucl[40001:45000]
pos10 <- df2$sucl[45001:50000]
pos11 <- df2$sucl[50001:55000]
pos12 <- df2$sucl[55001:60000]
pos13 <- df2$sucl[60001:65000]
pos14 <- df2$sucl[65001:nrow(df2)]
# pos tagging data
#irepos01 <- POStag(object = pos01)
#irepos01 <- as.vector(unlist(irepos01))
#writeLines(irepos01, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos01.txt", sep = "\n", useBytes = FALSE)
# chunk 2
#irepos02 <- POStag(object = pos02)
#irepos02 <- as.vector(unlist(irepos02))
#writeLines(irepos02, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos02.txt", sep = "\n", useBytes = FALSE)
# chunk 03
#irepos03 <- POStag(object = pos03)
#irepos03 <- as.vector(unlist(irepos03))
#writeLines(irepos03, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos03.txt", sep = "\n", useBytes = FALSE)
# chunk 04
#irepos04 <- POStag(object = pos04)
#irepos04 <- as.vector(unlist(irepos04))
#writeLines(irepos04, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos04.txt", sep = "\n", useBytes = FALSE)
# chunk 05
#irepos05 <- POStag(object = pos05)
#irepos05 <- as.vector(unlist(irepos05))
#writeLines(irepos05, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos05.txt", sep = "\n", useBytes = FALSE)
# chunk 06
#irepos06 <- POStag(object = pos06)
#irepos06 <- as.vector(unlist(irepos06))
#writeLines(irepos06, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos06.txt", sep = "\n", useBytes = FALSE)
# chunk 07
#irepos07 <- POStag(object = pos07)
#irepos07 <- as.vector(unlist(irepos07))
#writeLines(irepos07, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos07.txt", sep = "\n", useBytes = FALSE)
# chunk 08
#irepos08 <- POStag(object = pos08)
#irepos08 <- as.vector(unlist(irepos08))
#writeLines(irepos08, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos08.txt", sep = "\n", useBytes = FALSE)
# chunk 09
#irepos09 <- POStag(object = pos09)
#irepos09 <- as.vector(unlist(irepos09))
#writeLines(irepos09, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos09.txt", sep = "\n", useBytes = FALSE)
# chunk 10
#irepos10 <- POStag(object = pos10)
#irepos10 <- as.vector(unlist(irepos10))
#writeLines(irepos10, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos10.txt", sep = "\n", useBytes = FALSE)
# chunk 11
#irepos11 <- POStag(object = pos11)
#irepos11 <- as.vector(unlist(irepos11))
#writeLines(irepos11, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos11.txt", sep = "\n", useBytes = FALSE)
# chunk 12
#irepos12 <- POStag(object = pos12)
#irepos12 <- as.vector(unlist(irepos12))
#writeLines(irepos11, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos12.txt", sep = "\n", useBytes = FALSE)
# chunk 13
#irepos13 <- POStag(object = pos13)
#irepos13 <- as.vector(unlist(irepos13))
#writeLines(irepos13, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos13.txt", sep = "\n", useBytes = FALSE)
# chunk 14
#irepos14 <- POStag(object = pos14)
#irepos14 <- as.vector(unlist(irepos14))
#writeLines(irepos14, con = "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos14.txt", sep = "\n", useBytes = FALSE)
# list pos tagged elements
postag.files = c("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos01.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos02.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos03.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos04.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos05.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos06.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos07.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos08.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos09.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos10.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos11.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos12.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos13.txt",
  "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/irepos14.txt")
# load pos tagged elements
irepos <- sapply(postag.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  })
# unlist pos tagged elements
df2$irepos <- unlist(irepos)
###############################################################
# extract adjs and element preceeding and following it
pstggd <- df2$irepos
lpstggd <- strsplit(pstggd, " ")
nlpstggd <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("JJ", x)]
  x <- length(x) } )))
rp <- nlpstggd
# extract all adjectives (concordance)
###############################################################
pattern <- "[A-Z]{0,1}[a-z]{1,}/JJ[A-Z]{0,1}"
corpus.files <- pstggd
context <- 65
all.pre <- FALSE
jjcnc <- ConcR(corpus.files, pattern, context, all.pre = FALSE)
###############################################################
#jjcnc$pos <- gsub(".*/", "", jjcnc$token)
#jjcnc <- jjcnc[jjcnc$pos == "JJ",]
jjcnc$pre <- str_trim(jjcnc$pre)
jjcnc$post <- str_trim(jjcnc$post)
strng <- as.vector(unlist(apply(jjcnc, 1, function(x){
  paste(x[1], " << ", x[2], " >> ", x[3], collapse = " ") } )))
strng <- str_trim(strng)
strng  <- gsub(" {2,}", " ", strng)
adjs <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("JJ", x)] } )))
pre1 <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- ifelse(grep("JJ", x)-1 >= 1, x[grep("JJ", x)-1], NA)} )))
post1 <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("JJ", x)+1] } )))
post2 <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("JJ", x)+2] } )))
post3 <- as.vector(unlist(sapply(lpstggd, function(x){
  x <- x[grep("JJ", x)+3] } )))
dftmp <- cbind(pre1, adjs, post1, post2, post3)
###
# create new vectors so that jjs are repeated as often as reps2 defines
flid <- as.vector(unlist(rep(df2$flidn, rp)))
fl <- as.vector(unlist(rep(df2$flnn, rp)))
sf <- as.vector(unlist(rep(df2$sfnn, rp)))
spk <- as.vector(unlist(rep(df2$spkn, rp)))
su <- as.vector(unlist(rep(df2$sunn, rp)))
pos <- as.vector(unlist(rep(df2$irepos, rp)))
# create new df with data
df3 <- data.frame(1:length(flid), flid, fl, sf, spk, su, pos, pre1, adjs, post1, strng)
colnames(df3)[c(1,8)] <- c("id", "pint")
###############################################################
# clean pinttags
df3$pinttag <- as.vector(unlist(sapply(df3$pint, function(x){
  x <- ifelse(is.na(x) == T, 0, x)
  x <- gsub(".* ", 0, x)
  x <- gsub(".*VB$", 0, x)
  x <- gsub(".*WRB$", 0, x)
  x <- gsub(".*VB$", 0, x)
  x <- gsub(".*D$", 0, x)
  x <- gsub(".*P$", 0, x)
  x <- gsub(".*C$", 0, x)
#  x <- gsub(".*J$", 0, x)
#  x <- gsub(".*R$", 0, x)
  x <- gsub(".*S$", 0, x)
  x <- gsub(".*O$", 0, x)
  x <- gsub(".*H$", 0, x)
  x <- gsub(".*G$", 0, x)
  x <- gsub(".*N$", 0, x)
  x <- gsub(".*Z$", 0, x)
  x <- gsub(".*T$", 0, x)
  x <- gsub(".*W$", 0, x)
  x <- gsub(".*M$", 0, x)
  x <- gsub(".*X$", 0, x)
  x <- gsub(".*\\$$", 0, x)
  x <- gsub(".*\\.$", 0, x)
  })))
# fill empty pinttags
df3$pinttag <- as.vector(unlist(sapply(df3$pinttag, function(x){
  x <- ifelse(x == "", 0, x) } )))
df3$pint <- as.vector(unlist(sapply(df3$pinttag, function(x){
  x <- gsub("/.*", "", x) } )))
# clean adjs
df3$adj <- gsub("/.*", "", df3$adjs)
# create vector with intensifiers in data
intensifiers <- c("absolutely", "actually", "aggressively", "amazingly",
  "appallingly", "awful", "awfully", "badly", "bloody", "certainly", "clearly",
  "complete", "completely", "considerably", "crazy", "decidedly", "definitely",
  "distinctly", "dreadfully", "enormously", "entirely", "especially", "exactly",
  "exceedingly", "exceptionally", "excruciatingly", "extraordinarily", "extremely",
  "fiercely", "firmly", "frightfully", "fucking", "fully", "genuinely", "greatly",
  "grossly", "heavily", "highly", "hopelessly", "horrendously", "hugely",
  "immediately", "immensely", "incredibly", "infinitely", "intensely", "irrevocably",
  "mad", "mighty", "obviously", "openly", "overwhelmingly", "particularly",
  "perfectly", "plenty", "positively", "precisely", "pretty", "profoundly",
  "purely", "real", "really", "remarkably", "seriously", "shocking",
  "significant", "significantly", "so", "specially", "specifically", "strikingly",
  "strongly", "super", "surely", "terribly", "terrifically", #"too",
  "total",
  "totally", "traditionally", "true", "truly", "ultra", "utterly", "very",
  "viciously", "well", "wholly", "wicked", "wildly")
df3$pint <- as.vector(unlist(sapply(df3$pint, function(x){
  ifelse(x %in% intensifiers == T, x, 0) } )))
# remove all non adj slots from data
df4 <- df3[complete.cases(df3),]
df4$
preshort <- as.vector(unlist(sapply(df4$strng, function(x){
  x <- gsub("<<.*", "", x)
  x <- substr(x, nchar(x)-30, nchar(x))
  } )))
df4$remove <- as.vector(unlist(sapply(df4$preshort, function(x){
  x <- gsub(".*not/.*", "remove", x)
  x <- gsub(".*n't/.*", "remove", x)
  x <- gsub(".*no/.*", "remove", x)
  } )))
df4 <- df4[df4$remove != "remove",]
# create vector with false adjectives
rmvadj <- c("much", "many", "'o", "'otanga", "only", "other", "aaqib", "abby", "er", "egmont", "eric")
df4$remove <- as.vector(unlist(sapply(df4$adj, function(x){
  ifelse(x %in% rmvadj == T, "remove", x)
  } )))
df4 <- df4[df4$remove != "remove",]
# remove unnecessary columns
#df4 <- df4[, 1:12]
# convert pinttag RB to 1
df4$int <- as.vector(unlist(sapply(df4$pint, function(x){
  x <- ifelse(x == 0, 0, 1) } )))
# create a txttyp column
df4$txtyp <- as.vector(unlist(sapply(df4$fl, function(x){
  x <- gsub("-.*", "", x)
  x <- tolower(x)
  x <- ifelse(x == "s1a", "PrivateDialogue",
    ifelse(x == "s1b", "PublicDialogue",
    ifelse(x == "s2a", "UnscriptedMonologue",
    ifelse(x == "s2b", "ScriptedMonologue", x))))
  } )))
# clean fl
df4$fl <- gsub("-", "", df4$fl)
df4$sf <- as.factor(df4$sf)
# rename data set
inttb <- df4
# inspect data
head(inttb)

###############################################################
###############################################################
###############################################################
# add biodata
###############################################################
###############################################################
###############################################################
# read in data
bio <- read.table(bio.path, sep = "\t", header=TRUE)
# homogenize column names
colnames(bio)[c(4, 5, 6)] <- c("fl", "sf", "spk")
bio$sf <- as.factor(bio$sf)
# join data
intdata <- join(inttb, bio, by = c("fl", "sf", "spk"), type = "left")
# remove speakers below the age of 18
intdata <- intdata[intdata$age != "0-18", ]
# restructure data frame
int <- data.frame(1:nrow(intdata), intdata$flid, intdata$sf, intdata$spk, intdata$sex,
  intdata$age, intdata$occupation, intdata$ethnicity, intdata$txtyp, intdata$word.count,
  intdata$pint, intdata$adjs, intdata$post1, intdata$strng, intdata$pos, intdata$su, intdata$int)
# clean colnames
colnames(int) <- gsub("intdata.", "", colnames(int))
colnames(int)[c(1)] <- c("id")
# recode ethnicity
Maori <- c("Cook Island Maori",  "Maori",  "Maori/French",  "Maori/Nuiean/Samoan",  "Maori/Samoan",  "Samoan",  "Tokelauan")
NAN <- c("Chinese",  "European Asian",  "Lebanese",  "Maori/Negro/Pakeha",
  "Maori/Pakeha",  "Other",  "Pakeha/Maori",  "Pakeha/Samoan",  "Samoan/Dutch",
  "Samoan/Pakeha",  "Scots/Maori",  "Semitic")
Pakeha <- c("Cook Island/Pakeha",  "Dutch",  "Greek",  "Irish",  "Jewish",
  "NZ Greek",  "Pakeha",  "Pakeha/Arabic",  "Pakeha/Asian",  "Pakeha/Danish",
  "Pakeha/Dutch",  "Pakeha/German",  "Spanish/German")
int$ethnicity <- as.vector(unlist(sapply(int$ethnicity, function(x) {
  ifelse(x %in% Maori, "Maori",
  ifelse(x %in% NAN, NA,
  ifelse(x %in% Pakeha, "Pakeha", x))) } )))
################################################################
# recode occupation 1
int$occupation <- sapply(int$occupation, function(x) {
  x <- gsub(".*[T|t]eache.*", "acmp", x)     # teachers
  x <- gsub(".*[D|d]irect.*", "acmp", x)     # directors
  x <- gsub(".*[L|l]awye.*", "acmp", x)      # lawyers
  x <- gsub(".*[B|b]arrist.*", "acmp", x)    # barristers
  x <- gsub(".*[M|m]anage.*", "acmp", x)     # managers
  x <- gsub(".*[C|c]ler[i]{0,1}[c|k].*", "acmp", x)  # clerks
  x <- gsub(".*[A|a]ccounta.*", "acmp", x)   # accountants
  x <- gsub(".*[A|a]naly.*", "acmp", x)      # analysts
  x <- gsub(".*[M|m]iniste.*", "acmp", x)    # ministers
  x <- gsub(".*[A|a]dministr.*", "acmp", x)  # administartors
  x <- gsub(".*[J|j]udge.*", "acmp", x)      # judges
  x <- gsub(".*[A|a]pprent.*", "sml", x)     # apprentices
  x <- gsub(".*[J|j]ournal.*", "acmp", x)    # journalists
  x <- gsub(".*[W|w]rite.*", "acmp", x)      # writers
  x <- gsub(".*[S|s]olicit.*", "acmp", x)    # solicitors
  x <- gsub(".*[P|p]arliam.*", "acmp", x)    # pariamentary speakers/members of parliament
  x <- gsub(".*[P|p]rincip.*", "acmp", x)    # principals
  x <- gsub(".*[R|r]esearch.*", "acmp", x)   # researchers/research assistants
  x <- gsub(".*[C|c]omment.*", "acmp", x)    # political commentators
  x <- gsub(".*MP.*", "acmp", x)             # members of parliament
  x <- gsub(".*[L|l]ectur.*", "acmp", x)     # lecturers
  x <- gsub(".*[P|p]rogramm.*", "acmp", x)   # programmers & programm producers/directors
  x <- gsub(".*[E|e]dit.*", "acmp", x)       # editors
  x <- gsub(".*[B|b]ank.*", "acmp", x)       # bankers
  x <- gsub(".*[P|p]rof.*", "acmp", x)       # professors
      }  )
acmp <- c("Actor", "Anglican Youth Worker", "Auctioneer", "Book Shop Assistant",
  "Broadcaster", "Choreographer", "Company Chairman", "Composer/Music Education Advisor",
  "Constable", "Consultant", "Cultural Consultant", "Dbase Admin/Checkout Operator",
  "Delivery Contractor", "Documentary Producer", "Economist", "Education Officer",
  "Entertainer", "ESL Tutor", "Executive Radio Producer", "Faculty", "Knowledge Engineer",
  "Leader of the Opposition", "Librarian", "Library Assistant", "Marketing Rep",
  "Musician", "Occupational Therapist", "P/T Counsellor", "P/T Japanese Tutor",
  "P/T Telemarketing Trainer/Student", "P/T Tutor", "Police Constable", "PR Office Assistant",
  "Priest", "Privacy Commissioner", "Processing Officer", "Radio Broadcaster",
  "Reservations Consultant", "Restauranteur", "Self-Employed Consultant", "Self Employed Musician",
  "Senior Policy Advisor", "Shop Owner", "Student", "Student Support Person",
  "Student/Checkout Operator", "Student/Tutor", "Talkback Host", "Teaching Assistant",
  "Tutor", "Tutor/Fulltime Student", "Tutor/Student", "TV Quiz Show Host",
  "Uni Student", "Advisor to Race Relations", "Board Secretary", "Broadcaster/Financial Consultant",
  "Cartage Contractor", "Catholic Priest", "Company Executive", "Computer Consultant",
  "Dean of Commerce", "Deputy Academic Registrar", "Deputy Secretary to Treasury",
  "Distributor/Student", "Education Offiver/Tupperware", "Environmental Geochemist",
  "Freelance Broadcast", "FREELANCE Broadcaster", "Governor General of NZ",
  "Home Economist", "Launchmaster", "Liaison Officer", "Media", "Musician/Broadcaster",
  "News Reader", "Newscaster", "Newsreader", "NZ High Commission London",
  "NZRFU Resource Coach", "Oral Historian", "P/T Cinema Attendant/Student",
  "Patent Attorney", "Post Doctoral Fellow", "Publisher", "Radio Announcer",
  "Radio Announcer/News Reader", "Radio News Reader", "Radio Newsreader",
  "Radio Producer/News Reader", "Radio Producer/Newsreader", "Radio/Television Presenter",
  "Radio/TV Presenter", "Receptionist/Telephonist", "Regional Councillor", "Reporter",
  "Retired Public Servant/Academic", "Scientist", "Scientist Enviroment Officer",
  "Secretary", "Seismologist", "Sports Broadcaster", "Sports Broadcaster RNZ",
  "Sports Education Consultant", "Student/Teaching Fellow", "Television News Presenter",
  "Television Newscaster", "Television Presenter", "Television Presenter/Radio Announcer",
  "Television Presenter/Reporter", "TV/Film Producer", "University Reader")
NAN <- c("Baker/Student", "Barmaid/Student", "Barman/Student", "Building Supervisor",
  "Chef/Student", "Cleaner/Student", "Clerk/Cleaner/Student", "Detective NZ Police",
  "ECE Worker/Student", "Factory Labourer/Student", "Gymnastics Coach/Student",
  "Jewellery Salesperson", "Kitchenhand/Student", "Machinist in Proof Centre/Student",
  "P/T Barman/Student", "P/T Cleaner/Student", "P/T Pool Attendant/Student",
  "P/T Tutor/KFC/Student", "P/T Tutor/Retail Assistant", "P/T Vet's Nurse/Student",
  "P/T Waitress/Student", "Police Officer", "Registered Nurse/Student",
  "Retail/Student", "Self-Employed", "Self Employed", "Shop Assistant/Student",
  "Steward/Student", "Student/P/T Waitress", "Telephonist/Student", "Temp",
  "Tutor/Shop Assistant", "Retired", "Unemployed", "Waiter/PR Officer", "Waitress/Student",
  "(no BI)")
sml <- c("Ambulance Officer", "Baker/Patissier", "Builder", "Car Salesman", "Caregiver",
  "Catering", "Cleaner", "Cook", "Courier", "Doing Catering Course", "Firefighter",
  "Florist", "Food Delivery", "Joiner", "Nanny", "P/T Cafe Worker", "P/T Cleaner",
  "P/T Film Technician", "P/T House Cleaner", "P/T Shop Assistant", "Primary Health Social Worker",
  "Property Developer", "Quantity Surv & Carpentry", "Receptionist",
  "Receptionist/Secretary", "Service Station Attendant", "Shop Assistant",
  "Sports Shop Assistant", "Video Hire", "Waitress/Bartender", "Farm Tour Guide",
  "Fisheries Technician", "Former Airline Serviceman", "Kentucky Fried Chicken",
  "Technician")
# recode occupation 2
int$occupation <- sapply(int$occupation, function(x) {
  ifelse(x %in% acmp == T, "acmp",
  ifelse(x %in% NAN == T, NA,
  ifelse(x %in% sml == T, "sml", x))) } )
# add functional distinction (predicative vs attributive)
int$fun <- as.vector(unlist(sapply(int$post1, function(x){
  x <- gsub(".*/", "", x)
  x <- gsub(".*NN.*", "attributive", x)
  ifelse(x == "attributive", x, "predicative")  } )))
# create columns with intensifier freqs
int$very <- as.vector(unlist(sapply(int$pint, function(x){
  x <- ifelse(x == "very", 1, 0) } )))
int$really <- as.vector(unlist(sapply(int$pint, function(x){
  x <- ifelse(x == "really", 1, 0) } )))
int$so <- as.vector(unlist(sapply(int$pint, function(x){
  x <- ifelse(x == "so", 1, 0) } )))
# rename data
data <- int
# clean data
data <- data[complete.cases(data), ]
# inspect results
head(data)

###############################################################
# extract frequencies of ints
pints <- table(data$pint)
pints <- pints[order(pints, decreasing = T)]
pints

###############################################################
# recode age
###############################################################
data$ageorig <- data$age
data$age <- as.vector(unlist(sapply(data$age, function(x){
  ifelse(x == "16-19", "16-24",
  ifelse(x == "20-24", "16-24",
  ifelse(x == "25-29", "25-39",
  ifelse(x == "30-34", "25-39",
  ifelse(x == "35-39", "25-39",
  ifelse(x == "40-44", "40-49",
  ifelse(x == "45-49", "40-49",
  ifelse(x == "50-54", "50+",
  ifelse(x == "55-59", "50+",
  ifelse(x == "60-64", "50+",
  ifelse(x == "65-69", "50+",
  ifelse(x == "70-74", "50+", x)))))))))))) } )))
###############################################################
# recode text type
data$txtyp <- factor(data$txtyp, levels = c("PrivateDialogue", "PublicDialogue",
  "UnscriptedMonologue", "ScriptedMonologue"))
# code priming
prim1 <- c(rep(0, 1), data$int[1:length(data$int)-1])
prim2 <- c(rep(0, 2), data$int[1:(length(data$int)-2)])
prim3 <- c(rep(0, 3), data$int[1:(length(data$int)-3)])
primtb <- cbind(prim1, prim2, prim3)
priming <- rowSums(primtb)
data$priming <- as.vector(unlist(sapply(priming, function(x){
  x <- ifelse(x == 0, 0, 1) } )))
# clean adjs
data$adjs <- gsub("/.*", "", data$adjs)
# remove all incomplete cases from data set
data <- data[complete.cases(data),]
# add semantic analysis
# create list of adjectives
#names(table(data$adj))
#head(data)
# add gradability
nograd <- c("abject", "able", "abrasive", "abstract", "absurd", "abundant", "abusive",
  "accurate", "acrimonious", "active", "advanced", "adverse", "affectionate", "afraid",
  "aged", "aggressive", "agile", "agitated", "aimless", "airy", "alert", "alleged",
  "allusive", "amazing", "ambitious", "amused", "amusing", "ancient", "angry",
  "annoying", "anxious", "appalling", "apparent", "appealing", "applicable", "applied",
  "appreciative", "apprehensive", "approachable", "appropriate", "approving",
  "arduous", "arrogant", "ashamed", "associated", "astute", "athletic", "atrocious",
  "attitudinal", "attractive", "authentic", "authoritarian   ", "authoritative",
  "available", "aware", "awesome", "awful", "awkward", "awry", "bad", "bare", "base",
  "battered", "beautiful", "beloved", "benevolent", "benign", "besetting", "bitter",
  "bizarre", "bleak", "bleary", "bloody", "blotchy", "bold", "boppy", "bored",
  "boring", "bossy", "brave", "brief", "bright", "brilliant", "broad", "browsing",
  "brutal", "bubbly", "burly", "buzzy", "callous", "calm", "campy", "candid",
  "capable", "careful", "careless", "casual", "cautious", "ceremonial", "challenging",
  "changed", "charismatic", "charming", "cheap", "circumspect", "civic", "civil",
  "civilised", "classy", "clever", "cocky", "cold", "collective", "colossal", "colourful",
  "comfortable", "commandeered", "committed", "compatible", "compelling", "competent",
  "competitive", "complex", "complicated", "conceivable", "concentrated", "concerned",
  "confident", "confidential", "confused", "confusing", "considerable", "constructive",
  "consultative", "contrived", "controversial", "convenient", "conventional", "converted",
  "convinced", "cool", "corrupt", "cosy", "coy", "cramped", "crass", "crazy", "creative",
  "criminal", "crippling", "critical", "cross", "crowded", "crucial", "cruel", "cumbersome",
  "curious", "cushy", "cute", "cynical", "damaged", "damaging", "damp", "dangerous",
  "daring", "darkened", "darn", "daunting", "dear", "debatable", "decent", "dedicated",
  "deep", "defective", "defensive", "delicate", "delicious", "delighted", "delightful",
  "dense", "dependent", "depressed", "desirable", "despairing", "desperate", "despicable",
  "despondent", "destructive", "detailed", "detrimental", "devilish", "difficult",
  "dirty", "disabled", "disadvantaged", "disappointed", "disappointing", "disastrous",
  "disenchanted", "disgraceful", "disgusting", "dishonest", "disparaging", "distant",
  "distinguished   ", "distorted", "distressed", "disturbed", "disturbing", "dizzy",
  "dodgy", "dominant", "dotty", "double", "doubtful", "downhill", "dramatic", "dreadful",
  "driving", "drunk", "drunken", "ductile", "dull", "dumb", "dusty", "dylan", "dynamic",
  "dynamical", "eager", "early", "earnest", "earthy", "easterly", "eastern", "easy",
  "eccentric", "economic", "edible", "effective", "efficient", "elderly", "elegant",
  "eligible", "elitist", "elusive", "embarrassed", "embarrassing", "emergent", "eminent",
  "emotional", "emotive", "encouraging", "energetic", "enlightening", "enormous",
  "entertaining", "enthusiastic", "epic", "erudite", "estimated", "estranged", "everyday",
  "evil", "exact", "exceptional", "excessive", "excited", "exciting", "expensive",
  "experienced", "expert", "explicit", "express", "expressive", "extended", "extensive",
  "extraordinary", "extravagant", "extroverted", "fabulous", "facile", "factual", "faint",
  "familiar", "famous", "fanatic", "fancy", "fantastic", "fascinating", "fast", "fastidious",
  "fat", "favourable", "favoured", "fearful", "feisty", "fergal", "ferocious", "fertile",
  "fierce", "fiery", "filthy", "fine", "finished", "finite", "firm", "fitting", "fizzy",
  "flexible", "fluffy", "fluttering", "foggy", "foolish", "forceful", "formalised",
  "formidable", "fortunate", "frank", "frantic", "fraudulent", "fraught", "frenzied",
  "frequent", "friendly", "frightening", "frightful", "frustrated", "frustrating",
  "fulsome", "fun", "funny", "furious", "generous", "gentle", "giant", "gifted",
  "gigantic", "glad", "glib", "glorious", "glossy", "good", "goodhearted", "gorgeous",
  "gracious", "gradual", "grand", "grandiose", "grateful", "grave", "greasy", "great",
  "grim", "groggy", "groovy", "gross", "grubby", "guilty", "gutless", "habitual",
  "handsome", "handy", "hapless", "happy", "hard", "hardy", "harmful", "harmless",
  "harmonic", "harsh", "hazardous", "hazy", "heavy", "hectic", "helpful", "hideous",
  "high", "hilarious", "holy", "honest", "honorable", "honorary", "honourable",
  "hooked", "hopeful", "hopeless", "horrendous", "horrible", "horrific", "hostile",
  "hot", "huge", "humble", "humorous", "hungry", "hurt", "hysterical", "idealistic",
  "igneous", "ignorant", "imaginative", "immature", "immediate", "immense", "imperative",
  "important", "impotent", "impressive", "inane", "incompetent", "inconsistent",
  "incorporate", "incorporated", "increased", "incredible", "incredulous", "indecent",
  "independent", "individual", "individualistic ", "ineffective", "ineffectual",
  "inept", "inevitable", "inexorable", "inexpensive", "inexperienced", "infamous",
  "infertile", "informal", "infuriating", "injured", "innovative", "insatiable",
  "insecure", "insidious", "inspirational   ", "inspired", "instructive", "insuperable",
  "integrated", "intellectual", "intelligent", "intense", "intensive", "intimate",
  "intolerant", "invaluable", "inventive", "ironic", "irresponsible", "irritable",
  "irritating", "itchy", "jealous", "joyful", "justified", "justifying", "keen",
  "labour", "ladylike", "lame", "large", "late", "layered", "lazy", "lean", "legitimate",
  "leisurely", "less", "liberal", "liberating", "light", "likely", "limp", "little",
  "loath", "locating", "lone", "lonely", "long", "loony", "loud", "lousy", "lovely",
  "low", "loyal", "lucky", "lumbering", "luminous", "lumpy", "lunatic", "lush",
  "mad", "magic", "magnificent", "major", "mandatory", "manipulated", "marginal",
  "marvellous", "massive", "matrimonial", "mean", "meaningful", "measurable", "medical",
  "medicinal", "mediocre", "mere", "mighty", "mild", "minatory", "minded", "minor",
  "minted", "miraculous", "miscellaneous", "misleading", "mixed", "mock", "modal",
  "modern", "modest", "modesty", "momentous", "monetary", "monstrous", "moral", "motivating",
  "muddy", "muggy", "multiple", "mutual", "mystical", "mythical", "naive", "narrow", "nasty",
  "naughty", "near", "nearby", "neat", "necessary", "neglected", "negligent", "nervous",
  "net", "new", "nice", "noble", "noisy", "normal", "northern", "nostalgic", "notable",
  "noted", "noteworthy", "noxious", "numerous", "objective", "obnoxious", "obscure",
  "observant", "odd", "off", "oily", "okay", "old", "oldfashioned", "operatic", "optimistic",
  "orderly", "ordinary", "orientated", "oriented", "other", "outdated", "outrageous",
  "outstanding", "over", "overhanging", "overwhelming", "painful", "parky", "parlous",
  "passionate", "pathetic", "patronising", "patterned", "peaked", "peculiar", "perforated",
  "perishable", "pernicious", "perplexed", "perplexing", "persistent", "personal",
  "persuasive", "perverted", "pessimistic", "petite", "petty", "phenomenal", "picturesque",
  "pinkish", "plain", "pleasant", "pleased", "pleasing", "pleasurable", "plenty",
  "poetic", "polite", "poor", "popular", "possessive", "potent", "potential", "powerful",
  "practical", "pragmatic", "preachy", "precarious", "precious", "precise", "predatory",
  "predictable", "prepared", "prescriptive", "pressing", "prestigious", "presumptuous",
  "pretentious", "pretty", "prevalent", "primitive", "privileged", "prodigious", "productive",
  "professional", "profitable", "profligate", "progressive", "prominent", "promotional",
  "prone", "proper", "proportionate", "prospective", "prosperous", "protective", "proud",
  "provocative", "prudential", "psycho", "psychotic", "public", "puerile", "purposeful",
  "quaint", "qualitative", "queer", "quick", "quiet", "racist", "radical", "rainy",
  "rampant", "rank", "rapid", "rapt", "rare", "rational", "rattled", "raw", "reactionary",
  "reactive", "ready", "realistic", "reasonable", "recognisable", "recognised",
  "recreational", "reddish", "reduced", "refreshing", "regretful", "regular", "relaxed",
  "relaxing", "relentless", "relevant", "reliable", "reluctant", "remote", "required",
  "resourceful", "respected", "responsible", "restless", "revealing", "rich", "ridiculous",
  "risky", "robust", "rocky", "romantic", "rotten", "rough", "rowdy", "rude", "rumbling",
  "rusty", "sacred", "sad", "safe", "sandy", "sarcastic", "satisfied", "satisfying",
  "savage", "scarce", "scared", "sceptical", "scientific", "scrappy", "scratchy",
  "scruffy", "scurrilous", "secret", "secular", "secure", "sedate", "seduced", "seedy",
  "seismic", "selfconfessed", "selfish", "selfreliant", "senile", "sensational",
  "sensible", "sensitive", "sentimental", "serious", "severe", "sexist", "sexual",
  "sexy", "shadowy", "shaky", "shaped", "sharp", "shiny", "shitty", "shocking",
  "short", "sick", "sickly", "silly", "simple", "sizeable", "skilful", "skilled",
  "sleepy", "slight", "slim", "slippery", "sloppy", "slow", "small", "smart", "snoopy",
  "snotty", "sociable", "social", "soft", "soggy", "solid", "sophisticated", "sore",
  "sorry", "sour", "south", "spare", "sparkling", "spectacular", "spectral", "spiritual",
  "spiteful", "splendid", "sporting", "starkly", "startling", "staunch", "steady",
  "steamy", "steep", "stellar", "sticky", "stiff", "stimulating", "stoical", "stormy",
  "strange", "strategic", "stressful", "stretched", "strict", "striking", "strong",
  "structured", "stubborn", "stunning", "stupid", "subject", "subtle", "successful",
  "suffering", "suitable", "sunny", "super", "superficial", "superior", "supernatural",
  "supportive", "suppressed", "sure", "surplus", "surprised", "surprising", "susceptible",
  "suspicious", "sustainable", "sweaty", "sweet", "swift", "sympathetic", "tacky",
  "tactic", "talented", "tall", "tantalising", "tasteful", "tedious", "teensy", "temperate",
  "tempting", "tended", "tense", "tentative", "terrible", "terrific", "theatrical",
  "theoretical", "thermal", "thick", "thickened", "thin", "thirsty", "thoughtful",
  "threatening", "thriving", "tight", "tiny", "tired", "titanic", "tony", "top", "topical",
  "torrential", "tortious", "tortured", "torturous", "tough", "touring", "tragic",
  "transcendental", "transferable", "traumatic", "treacherous", "tremendous", "trendy",
  "tricky", "trim", "triumphal", "trivial", "troubled", "twee", "twisted", "typical",
  "ugly", "ulterior", "unable", "unattractive", "unaware", "unbeknown", "unbelievable",
  "uncaring", "uncertain", "unclear", "unctuous", "undecided", "undeniable", "undifferentiated",
  "undignified", "uneven", "unexpected", "unfair", "unfamiliar", "unfavourable",
  "unfit", "unflattering", "unforced", "unfortunate", "ungrateful", "unhappy", "unholy",
  "unified", "unknown", "unlikely", "unlucky", "unorthodox", "unpleasant", "unreal",
  "unseemly", "unsmiling", "unsocial", "unsound", "unstable", "unusual", "upset",
  "uptight", "urban", "urbanised", "urgent", "useful", "vague", "vain", "valiant",
  "valuable", "variable", "varied", "vast", "venerated", "vengeful", "versatile",
  "vested", "veteran", "viable", "vigorous", "vile", "violent", "virtual", "visionary",
  "visual", "vital", "vivid", "vocal", "volatile", "vulnerable", "wakeful", "warm",
  "wayward", "weak", "weakly", "wealthy", "weary", "wee", "weird", "wet", "wicked",
  "wide", "widespread", "wild", "willing", "wise", "wishful", "witty", "wobbly",
  "wonderful", "wondrous", "worried", "worthwhile", "worthy", "wounded", "young",
  "yukky", "yummy")
grad <- c("delusive", "abdominal", "aboriginal", "absent", "absolute", "academic",
  "accented", "acceptable", "accessible", "accomplished", "accountable", "acoustical",
  "acrylic", "actual", "additional", "adequate", "adjacent", "administrative",
  "adolescent", "advantageous", "aerial", "affected", "affirmative", "affordable",
  "african", "aggregate", "agricultural", "albanian", "alive", "allergic", "alternative",
  "ambiguous", "american", "analogous", "analytical", "ancestral", "anecdotal",
  "angled", "anglican", "announced", "annual", "anonymous", "antarctic", "apocryphal",
  "aqueous", "arbitrary", "archaeological", "archaic", "arctic", "armed", "armoured",
  "artificial", "artistic", "asian", "asthmatic", "atmospheric", "atomic", "aussie",
  "australian", "austrian", "authorised", "automatic", "autonomous", "average", "awake",
  "back", "backward", "baked", "balanced", "bald", "bankrupt", "basic", "bearded",
  "beneficial", "best", "biblical", "bibliographic", "binding", "biodegradeable",
  "biographical", "biological", "black", "blank", "blatant", "blind", "blonde", "blue",
  "bodily", "booed", "botanical", "bottom", "british", "broke", "broken", "brown",
  "bucketful", "budgetary", "bureaucratic", "burnt", "businesslike", "busy", "californian",
  "canonical", "capitalistic", "captive", "cardiac", "catholic", "cellular", "central",
  "centralised", "centred", "certain", "characteristic  ", "chartered", "cheated",
  "chemical", "chilean", "chinese", "chivalrous", "christian", "chromatic", "chronological",
  "churchy", "classic", "classical", "clean", "clear", "close", "closed", "coarse",
  "coated", "coherent", "cohesive", "coincidental", "colloquial", "coloured", "coming",
  "commercial", "common", "compact", "comparable", "complete", "compound", "comprehensive",
  "compulsory", "computerised", "conceptual", "concrete", "confessional", "confirmed",
  "conscious", "conservative", "consistent", "constant", "constituent", "contemporary",
  "contestable", "continual", "contraceptive", "contrary", "cooked", "cooking",
  "corporate", "correct", "cracked", "crushed", "cubic", "cultural", "curly", "current",
  "customary", "cut", "daily", "dark", "dead", "deadly", "deaf", "decisive", "definite",
  "definitive", "deliberate", "democratic", "demographic", "determined", "diagnostic",
  "diagonal", "dietetic", "different", "digestive", "digital", "diplomatic", "direct",
  "discursive", "displaced", "disqualified", "distinct", "distinctive", "diverse", "divine",
  "domestic", "down", "downward", "dry", "dual", "dubious", "dummy", "dutch", "east",
  "educational", "effluent", "egalitarian", "electable", "electric", "electrical", "electronic",
  "elemental", "empty", "endemic", "endless", "english", "enough", "enrolled", "entailed",
  "entire", "equal", "equatorial", "equestrian", "equitable", "equivalent", "eritrean",
  "essential", "estonian", "ethic", "ethiopian", "ethnic", "european", "ewen", "exalted",
  "excellent", "executive", "exiguous", "existent", "existing", "exotic", "expected",
  "experimental", "explosive", "exponential", "external", "extinct", "extra", "extreme",
  "fair", "fake", "false", "far", "fatal", "favourite", "federal", "federated",
  "fellow", "female", "feminist", "feudal", "few", "fictional", "final", "financial",
  "first", "fixed", "flagged", "flannelled", "flat", "fleet", "flowing", "fluent",
  "fluid", "focused", "folded", "folding", "following", "foreign", "foremost", "formal",
  "forthcoming", "forward", "fossil", "foster", "founding", "fragile", "free", "french",
  "fresh", "frisian", "front", "frontal", "frosted", "fucking", "full", "fundamental",
  "funded", "further", "future", "gaelic", "gay", "general", "generational", "generic",
  "genuine", "geographical", "geological", "geotechnical", "german", "germanic", "glandular",
  "global", "gold", "golden", "governmental", "granulitic", "graphical", "gray", "greek",
  "green", "grey", "guaranteed", "half", "halved", "halving", "healthy", "hereditary",
  "heterogeneous   ", "heterogenious", "hidden", "historic", "historical", "holistic",
  "homosexual", "hooped", "horizontal", "hourly", "human", "humanitarian", "humiliating",
  "hungary", "hydroplaning", "hypocritical", "hypothetical", "iambic", "ideal", "identical",
  "ideological", "idle", "ill", "illegal", "imaginable", "immune", "imperial", "implicit",
  "implied", "impossible", "improved", "inaccessible", "inaccurate", "inadequate", "inclusive",
  "incoming", "incorrect", "incumbent", "indian", "indifferent", "indigenous", "indispensable",
  "indisputable", "industrial", "inefficient", "inescapable", "inexplicable", "infallible",
  "inflatable", "inflated", "informed", "infrequent", "inherent", "initial", "innate",
  "inner", "innocent", "innumerable", "inorganic", "inside", "insignificant", "instant",
  "instrumental", "insufficient", "intact", "integral", "intentional", "interactive",
  "intercultural", "interested", "interesting", "internal", "international", "interrupted",
  "intervening", "intriguing", "intrinsic", "inverted", "iraq", "irish", "irrelevant",
  "irrespective", "islamic", "italian", "japanese", "jewish", "joint", "journalistic",
  "judicial", "judicious", "junior", "just", "last", "latter", "leading", "learned",
  "learnt", "left", "lefthand", "legal", "legged", "legislative", "lesbian", "liable",
  "lime", "limited", "linear", "linguistic", "liquid", "literary", "liturgical", "live",
  "loaded", "local", "logarithmic", "logical", "logistic", "lost", "macrocyclic",
  "magnetic", "main", "male", "marine", "marked", "married", "masqueraded", "masterly",
  "materialistic   ", "maternal", "mathematical", "mature", "maximum", "mechanistic",
  "medieval", "mega", "melodic", "mental", "messy", "metamorphic", "meterological",
  "metrical", "metropolitan", "mexican", "micro", "microeconomic", "mid", "middle",
  "militaristic", "military", "milky", "minimal", "minimalist", "minimum", "ministerial",
  "missionary", "mobile", "moderate", "molecular", "molten", "monotonous", "mundane",
  "muscovite", "musical", "mutant", "naked", "narrative", "nasal", "natal", "national",
  "nationwide", "native", "natural", "nautical", "naval", "nazi", "needy", "negative",
  "neurotic", "next", "nitric", "north", "noticeable", "now", "nuclear", "obligatory",
  "obvious", "occasional", "octave", "official", "olympic", "ongoing", "only", "onward",
  "open", "operational", "opposed", "opposite", "optical", "optimum", "optional", "oral",
  "orange", "orchestral", "orchestrated", "organic", "original", "outside", "overlapping",
  "pacific", "painless", "pakistani", "parallel", "paramount", "parental", "parliamentary",
  "partial", "particular", "partisan", "passive", "past", "pastoral", "paternal",
  "paternalistic", "patriarchal", "patriotic", "perfect", "peripheral", "permanent",
  "permissive", "pertinent", "peruvian", "philosophical", "phonetic", "physical", "pink",
  "plastic", "pluralistic", "polar", "political", "politicised", "polynesian", "pornographic",
  "portable", "positive", "possible", "practicable", "preconceived", "preferential",
  "preferred", "pregnant", "preliminary", "presbyterian", "present", "presidential",
  "previous", "prewarned", "priceless", "primary", "prime", "principal", "prior", "pristine",
  "private", "privatised", "probable", "procedural", "programmed", "prolonged", "pronged",
  "proportional", "provincial", "psychiatric", "psychic", "pure", "purple", "quantifiable",
  "quantitative", "racial", "radioactive", "random", "readable", "real", "rear", "recent",
  "recycled", "red", "redundant", "reformed", "regional", "registered", "regulated",
  "regulatory", "reissued", "related", "relational", "relative", "remarkable", "remedial",
  "reportable", "reported", "residential", "respective", "resulting", "retrospective",
  "reusable", "reverse", "revolutionary", "ridged", "right", "rightful", "righthand",
  "rigid", "rigorous", "romanian", "rotary", "round", "royal", "ruined", "rural",
  "russian", "same", "samoan", "sane", "saturated", "scandinavian", "scholastic",
  "scottish", "scriptural", "seasonal", "secondary", "securing", "selected", "selective",
  "selfstyled", "senior", "senseless", "separate", "separated", "serial", "sheer",
  "siberian", "significant", "silver", "similar", "simultaneous", "sincere", "singaporean",
  "single", "skinned", "sleeveless", "sliced", "smokefree", "smooth", "sober", "socialist",
  "sociodemographic", "socioeconomic", "sole", "solitary", "soluble", "southern",
  "southwest", "sovereign", "soviet", "spanish", "special", "specialised", "specific",
  "spinal", "spontaneous", "spurious", "square", "stable", "stagnant", "standard",
  "stated", "stationary", "statistical", "statutory", "steely", "stereo", "stolen",
  "straight", "stratospheric", "striped", "structural", "subconscious", "subordinate",
  "subset", "substantial", "substantive", "suburban", "sudden", "sufficient", "suggestive",
  "sundry", "superheated", "supplementary", "supreme", "surgical", "sustained", "swedish",
  "swiss", "swollen", "symbolic", "synthetic", "technical", "technological", "temporary",
  "terminal", "territorial", "textual", "textural", "thematic", "thorough", "thoroughgoing",
  "timely", "total", "totalitarian", "toxic", "traditional", "transmitted", "traversable",
  "true", "twin", "ultimate", "unacceptable", "unaffected", "unallocated", "unannounced",
  "unanswered", "unbeaten", "unbiased", "unblemished", "unchanged", "uncoordinated",
  "under", "undisclosed", "undone", "unemployed", "unequal", "unexpired", "unfilled",
  "unfurnished", "unique", "universal", "unlimited", "unnatural", "unnecessary", "unoccupied",
  "unofficial", "unplayable", "unpopular", "unprecedented", "unprejudiced", "unpretentious",
  "unpromising", "unreceptive", "unregulated", "unrelated", "unresolved", "unrhymed",
  "unseeded", "unseen", "unselective", "unselfish", "unspecified", "unspoilt", "unstressed",
  "unsubsidised", "untold", "untrue", "unvarnished", "unwanted", "unwarranted", "unwilling",
  "unwrinkled", "upward", "usable", "useless", "usual", "utter", "vacant", "valid",
  "various", "veiled", "venetian", "verbal", "verbatim", "verifiable", "vertical",
  "volcanic", "voluntary", "weekly", "west", "western", "white", "whole", "wilful",
  "wooden", "woollen", "written", "wrong", "yellow", "youthful", "religious")
rmv <- c("above", "absorbing", "abused", "accessory", "acoustic", "acting", "adrian",
  "adriatic", "aerosol", "africa", "afro", "akoranga", "aladdin", "alain", "alan",
  "alas", "algal", "allan", "amachi", "amateur", "amending", "amnesty", "amphibolite",
  "amy", "andy", "ann", "annie", "ant", "anti", "antiinflammatory", "antony", "anzus",
  "apothecary", "april", "archery", "arnold", "ascii", "associate", "aston", "asymptotic",
  "athlete", "audio", "augsburg", "australia", "australian", "avanti", "awa", "awkwardly", "ayrton",
  "backhand", "bailey", "baird", "banny", "barry", "becky", "beetroot", "bendon",
  "benizir", "berl", "better", "bhutan", "bible", "bibliographical",  "big", "bigger",
  "biggest", "bilingual", "billy", "blackbirding", "blah", "bledisloe", "blindside",
  "blooded", "brad", "brent", "brian", "brighter", "brisbane", "broader", "bronx",
  "bryan", "bubonic", "bulk", "busier", "busiest", "byrd", "calculus", "calvin",
  "cannot", "cardinal", "carnival", "castletown", "caucus", "chas", "cheaper",
  "cheapest", "chiastolite", "chief", "clandestine", "cleaner", "clearer", "clerical",
  "clinozoisite", "clint", "clive", "clorosell", "closer", "closest", "clown",
  "coastal", "complementary   ", "constable", "constitutional", "content",
  "continuous", "cooler", "cooper", "cooperative", "coppery", "corequisite",
  "cornered", "cornish", "coromandel", "coronation", "coronet", "corpus", "craig",
  "crocodile", "crowe", "cuba", "cullen", "curfew", "curling", "cycling", "d",
  "daggy", "dam", "dame", "damn", "damprid", "danaran", "dane", "daniel", "danny",
  "darcey", "darren", "daryl", "dave", "david", "de", "debbie", "december", "deeper",
  "deepest", "deer", "deli", "delphic", "departmental", "deputy", "dhaka", "diatomaceous",
  "differentiate", "dipak", "disincentive", "disparate", "distributive", "diversionary",
  "diving", "divisional", "doctoral", "donald", "dorothy", "dorp", "doublebreasted",
  "due", "dumber", "duncan", "dunedin", "dyrite", "earlier", "earliest", "easier",
  "easiest", "eastbourne", "easter", "ed", "eeriest", "eh", "eighteenth", "eighth",
  "eighty", "eisenhower", "electoral", "electorate", "electricorp", "eleventh",
  "ellipse", "else", "embassy", "emperor", "english", "ensemble", "entrepreneurial",  "environmental",
  "enzac", "epuni", "er", "evidentiary", "fairer", "fairy", "fale", "falsehood", "fanta",
  "farce", "farr", "faster", "fastest", "february", "femme", "fernyhough", "ferric",
  "ferris", "feverfew", "fewer", "fifteenth", "fifth", "fiftieth", "fiji", "finer",
  "finest", "finlay", "fitzpatrick", "fitzy", "flannagan", "flatmate", "flatting",
  "fledgling", "flown", "ford", "format", "former", "fourteenth", "fourth", "fourwheel",
  "fragmented", "frailty", "fran", "fred", "freeman", "fremantle", "freyberg", "friday",
  "fright", "frilly", "froth", "fuck", "fullback", "fuller", "fulltime", "fundraising",
  "funniest", "futunan", "g", "gale", "gall", "gary", "gee", "george", "germany", "gerold",
  "gibbs", "gideon", "gilbert", "gisborne", "glen", "gloomy", "gneissic", "go", "gong",
  "goretex", "grad", "graeme", "granddad", "grandstand", "granted", "gravy", "greater",
  "greatest", "greg", "gregory", "griffin", "grimmer", "grimmest", "grippy", "grossular",
  "gulden", "gully", "h", "hadlee", "haile", "haired", "halfhearted", "halfway", "hamilton",
  "hamish", "hanky", "hannah", "happier", "happiest", "harder", "hardest", "harp", "harris",
  "harvesting", "hatchery", "have", "healthier", "healy", "heaviest", "hegan", "helen",
  "henare", "henry", "highbury", "higher", "highest", "hindsight", "hoani", "homing",
  "hong", "horan", "horological", "hosted", "hottest", "hugely", "hugh", "hummocky",
  "hutt", "huwert", "hypoimonic", "i", "ian", "illfated", "inasmuch", "infectious",
  "inferior", "innermost", "innovate", "institutional   ", "interim", "interlinked",
  "intermediate", "interminable", "interrupt", "interventionist",  "interviewing",
  "jaap", "jacqui", "jane", "javanese", "javed", "jean", "jed", "jenny", "joanne",
  "johnny", "judy", "jukken", "julian", "jus", "kecky", "kelly", "ken", "kenny", "key",
  "keynsian", "kharan", "kindhearted", "kong", "korere", "kuan", "l", "lankan", "larger",
  "largest", "largish", "larry", "later", "latest", "lava", "leanne", "leasamaivao",
  "least", "leaves", "lee", "leech", "leftist", "leigh", "leilani", "len", "leopard",
  "leopold", "lesser", "level", "levin", "lexic", "liaison", "lietchy", "like", "litmus",
  "liverpool", "liverpudlian", "lolani", "london", "longer", "longest", "longitudinal",
  "longstanding", "longterm", "longtime", "lonnegan", "loose", "lord", "lowe", "lower",
  "lowest", "lowish", "lynagh", "lynda", "m", "mac", "maccarthy", "madam", "madame",
  "madras", "mai", "maiden", "makeshift", "mal", "malivai", "mandy", "mangare", "mango",
  "manifest", "manuka", "maori", "marianne", "mary", "matt", "matu", "maud", "maui",
  "maul", "mauri", "mccarthy", "mcgann", "melanesian", "melbourne", "metre", "michael",
  "michelle", "michigan", "mickey", "midtwentieth", "midweek", "mike", "millard", "mine",
  "minh", "mini", "minute", "mislead", "missus", "mister", "mitzy", "mobil", "moby",
  "monday", "moore", "more", "morphous", "morven", "most", "mouthy", "mozart", "mozartian",
  "muldoonist", "multi", "mum", "munich", "muriwai", "nah", "nanny", "narrower", "nat", "national",
  "nearest", "neil", "nesian", "networked", "neville", "newish", "newsworthy", "newtown",
  "ngati", "nil", "nineteenth", "ninth", "noel", "non", "nonbrown", "noncommercial",
  "nonconducive", "nonphysical", "nonracial", "nonrefundable", "nonreligious", "nonsense",
  "nonstop", "nontransferrable", "nordenskjold", "norseman", "nosed", "novel", "nuisance",
  "nuts", "observational   ", "observer", "october", "offshore", "okayed", "olden", "older",
  "oldest", "oldfashionedy   ", "olivier", "olly", "olo", "oof", "oprah", "organisational",
  "organised", "otago", "other", "ours", "outer", "overactive", "overall", "overseas", "overshadowed",
  "overthrow", "overview", "owen", "own", "p", "pakeha", "pakehas", "pakistan", "pal",
  "par", "payday", "pent", "pentecostal", "personed", "perth", "pervaded", "pest",
  "pete", "phalsphatic", "pharmaceutical  ", "photocopied", "photocopying", "pinky",
  "plough", "plus", "poorer", "pornography", "posit", "pre", "premier", "prench",
  "prep", "prerequisite", "pressured", "primetime", "primus", "pro", "proliberal",
  "promising", "proposed", "prostatic", "prue", "psychological   ", "puni", "puzzled",
  "pyrothrastic", "quandary", "quantum", "quartz", "queensgate", "queensland", "quicker",
  "r", "rachel", "radiata", "radisich", "raggy", "ranfurly", "rarotonga", "rascal",
  "rashid", "re", "rebuttal", "reckless", "referee", "referral", "rehearsal", "reich",
  "remotest", "representational", "representative", "retail", "rhiolithic", "rhyme",
  "riccarton", "richard", "richardson", "richmond", "rid", "riddiford", "righty",
  "rip", "ripene", "robert", "robyn", "rouge", "roughy", "rounder", "routine", "rubbish",
  "rudey", "rugby", "russell", "ruth", "rutherford", "ryan", "safer", "salim", "sam",
  "sammy", "samuel", "san", "sarah", "satisfactional",   "satisfactory", "saturday",
  "scan", "scotty", "scroty", "scrummiest", "sean", "sec", "second", "secondhand",
  "select", "selwyn", "semifinal", "seminar", "sesame", "seventeenth", "seventh",
  "seventy", "several", "seville", "sharper", "shat", "shearer", "shit", "shorter",
  "shortland", "shortterm", "sic", "sideways", "sikh", "sillimanite", "simon", "sind",
  "singapore", "siross", "sixteenth", "sixth", "ski", "skiting", "slightest", "slower",
  "slowest", "smaller", "smallest", "smithsonian", "socio", "soonest", "sound",
  "southerly", "southland", "specifying", "spleen", "stanislavsky", "statistic",
  "stepping", "stern", "steve", "steven", "stewart", "still", "stink", "stralian",
  "strasbourg", "strata", "stratosphere", "stronger", "strongest", "successive",
  "such", "suckful", "suffrage", "sunday", "sunnyside", "superannuitant",   "superb",
  "superimposed", "superthrifty", "suzy", "sven", "sweden", "swirly", "syllable",
  "symphony", "t", "tackled", "tail", "talky", "taller", "tallish", "tansy", "taranaki",
  "tarawera", "tarseal", "taumarunui", "taupo", "tauranga", "tautoko", "taylor", "te",
  "tee", "tenth", "tertiary", "theirs", "then", "thinner", "third", "thirtieth", "tighter",
  "tigrean", "tim", "tony", "toughest", "tramping", "transmitter", "traversal", "treble", "tribal",
  "triple", "tronic", "troublesome", "tuesday", "turntable", "turoa", "tutorial", "twelfth",
  "twentieth", "ugliest", "uh", "ultra", "um", "unbelieveable", "uncomfortable",
  "underestimate   ", "undergraduate", "underground", "understandable", "uni",
  "uniform", "unmuddied", "unneedy", "unpredictable", "unquote", "unrepresentative",
  "unsubstantiated",  "unsustainable", "untampered", "upper", "upskill", "urbanly",
  "urologist", "usurpt", "vaega", "vaunted", "very", "vic", "vicky", "video",
  "vincent", "vinny", "vivian", "w", "walter", "wanganui", "waqar", "warped", "warwick",
  "waterbed", "waugh", "wavyish", "wayne", "weakest", "wednesday", "weensy", "welcome",
  "wellbeing", "wellinformed", "wellington", "wellingtonian   ", "wellknown", "westpac",
  "western", "whakarewarewa", "whitbread", "whitianga", "wholehearted", "wholesale", "wholesome",
  "whoof", "wickliffe", "wider", "widish", "willy", "wilton", "wimpish", "winfield",
  "winning", "wong", "woolshed", "working", "worrying", "worse", "worst", "worth",
  "wosaki", "wow", "wreathian", "year", "yearly", "younger", "youngest", "yours",
  "yous", "zed")
# add gradability coding
data$grad <- sapply(data$adjs, function(x) {
  ifelse(x %in% grad == T, "grad",
  ifelse(x %in% nograd == T, "nograd", "nograd")) } )
# remove non-adjectives
data$remove <- as.vector(unlist(sapply(data$adjs, function(x){
  ifelse(x %in% rmv == T, "remove", "keep")
  } )))
data <- data[data$remove != "remove",]
# add semantic types
semage <- c("contemporary", "aged", "ancient", "elderly", "immature", "late",
  "modern", "old", "oldfashioned", "outdated", "puerile", "senile", "topical",
  "veteran", "young", "actual", "adolescent", "ancestral", "annual", "archaeological",
  "archaic", "biographical", "foster", "generational", "historic", "historical",
  "junior", "mature", "medieval", "past", "preliminary", "present", "primary",
  "prime", "prior", "recent", "seasonal", "senior", "temporary", "youthful")
semcol <- c("colourful", "darkened", "pinkish", "reddish", "black", "blue", "brown",
  "coloured", "dark", "gold", "golden", "gray", "green", "grey", "lime", "marine",
  "orange", "pink", "purple", "red", "silver", "white", "yellow")
semdif <- c("complicated", "difficult", "easy", "elusive", "facile", "precarious",
  "risky", "simple", "stressful", "tricky", "twisted", "unpromising")
semdim <- c("warped", "brief", "bright", "broad", "deep", "distant", "early", "easterly",
  "eastern", "giant", "gigantic", "grand", "high", "huge", "large", "little",
  "locating", "long", "low", "massive", "minor", "misleading", "narrow", "near",
  "nearby", "northern", "off", "orientated", "over", "overhanging", "petite",
  "public", "remote", "short", "sizeable", "slight", "small", "south", "steep",
  "super", "tall", "teensy", "thick", "thickened", "thin", "tight", "tiny",
  "titanic", "top", "torrential", "touring", "tremendous", "urban", "urbanised",
  "vast", "wee", "wide", "widespread", "adjacent", "angled", "arctic", "back",
  "backward", "bottom", "central", "centralised", "centred", "close", "compact",
  "diagonal", "direct", "down", "downward", "east", "endemic", "endless", "equatorial",
  "european", "ewen", "far", "few", "first", "flat", "foreign", "foremost", "forthcoming",
  "forward", "free", "front", "frontal", "further", "geographical", "global", "half",
  "halved", "halving", "horizontal", "inner", "inside", "internal", "international",
  "last", "latter", "left", "linear", "local", "micro", "mid", "middle", "minimal",
  "minimalist", "minimum", "national", "nationwide", "native", "next", "north", "onward",
  "outside", "overlapping", "pacific", "parallel", "paramount", "peripheral", "polar",
  "proportional", "provincial", "rear", "regional", "reverse", "round", "rural",
  "separate", "separated", "southern", "southwest", "spinal", "square", "stratospheric",
  "suburban", "terminal", "territorial", "under", "universal", "unseeded", "upward",
  "vertical", "west", "western")
semhup <- c("amusing", "puzzled", "abrasive", "abusive", "adverse", "aggressive",
  "angry", "besetting", "bossy", "brutal", "callous", "cocky", "compelling", "cross",
  "cruel", "cynical", "depressed", "despairing", "desperate", "despondent", "disappointed",
  "dodgy", "evil", "ferocious", "fierce", "forceful", "frustrated", "furious", "grim",
  "gutless", "hostile", "hysterical", "ignorant", "imperative", "inexorable", "insidious",
  "jealous", "loath", "lone", "lonely", "lunatic", "mad", "mean", "nasty", "patronising",
  "pessimistic", "psycho", "regretful", "relentless", "rowdy", "rude", "sad", "sarcastic",
  "selfish", "snotty", "spiteful", "strict", "suffering", "treacherous", "uncaring",
  "unsmiling", "unsocial", "upset", "vengeful", "vile", "wicked", "cheated", "intriguing",
  "able", "advanced", "astute", "capable", "challenging", "clever", "competent",
  "competitive", "consultative", "dotty", "dull", "dumb", "erudite", "excited",
  "foolish", "gifted", "incompetent", "inexperienced", "intellectual", "intelligent",
  "inventive", "minded", "preachy", "primitive", "professional", "prudential",
  "rational", "resourceful", "silly", "skilful", "skilled", "smart", "sophisticated",
  "strategic", "stupid", "tactic", "talented", "unable", "unaware", "wise", "academic",
  "accomplished", "analytical", "infallible", "informed", "learned", "learnt", "procedural",
  "sane", "selective", "technical", "brave", "candid", "eager", "fanatic", "fraudulent",
  "insatiable", "motivating", "prepared", "presumptuous", "promotional", "stubborn",
  "superior", "willing", "sovereign", "valid", "amused", "anxious", "appreciative",
  "ashamed", "benevolent", "bold", "charismatic", "concerned", "confident", "convinced",
  "delighted", "encouraging", "entertaining", "enthusiastic", "fortunate", "friendly",
  "fun", "generous", "glad", "goodhearted", "gracious", "grateful", "hapless", "happy",
  "hopeful", "hopeless", "joyful", "lucky", "nervous", "optimistic", "passionate",
  "pleased", "polite", "rapt", "respected", "romantic", "satisfied", "satisfying",
  "scared", "sensitive", "sentimental", "sociable", "sorry", "supportive", "troubled",
  "ungrateful", "unhappy", "valiant", "witty", "worried", "afraid", "aimless", "apprehensive",
  "aware", "fearful", "sceptical", "staunch", "dubious", "unanswered")
# add semantic type classification
data$sem <- sapply(data$adjs, function(x) {
  ifelse(x %in% semage == T, "age",
  ifelse(x %in% semcol == T, "col",
  ifelse(x %in% semdif == T, "dif",
  ifelse(x %in% semdim == T, "dim",
  ifelse(x %in% semhup == T, "hup", "notype"))))) } )
# classify emotion
class_emo <- get_nrc_sentiment(data$adjs)
# process sentiment
emo <- as.vector(unlist(apply(class_emo, 1, function(x){
  x <- ifelse(x[9] == 1, "emotional",
    ifelse(x[10] == 1, "emotional", "nonemotional")) } )))
# add gender, age, ed.lev, and occupation
data <- data.frame(data, emo)
colnames(data) <- gsub("", "", colnames(data))
# revert order of factor emo
data$emo <- factor(data$emo, levels = c("nonemotional", "emotional" ))
# remove all incomplete cases from data set
#data <- data[complete.cases(data),]
# extract examples
s1a <- data[data$txtyp == "PrivateDialogue", ]
expls <- s1a[s1a$int == 1, ]
head(expls)

###############################################################
data$pos <- data$strng
data$pos <- gsub(" [A-Z]{0,1}[a-z]{0,}/", "", data$pos)
data$strng  <- gsub("/[A-Z]{2,3}", "", data$strng)
data$strng  <- gsub("$", "", data$strng, fixed = T)
data$test <- as.vector(unlist(sapply(data$strng, function(x){
  x <- gsub("<<.*", "", x)
  x <- gsub(".* is.*", "1", x)
  x <- gsub(".* was.*", "1", x)
  x <- gsub(".* are.*", "1", x)
  x <- gsub(".* were.*", "1", x)
  x <- gsub(".* has.*", "1", x)
  x <- gsub(".* have.*", "1", x)
  x <- gsub(".*which.*", "1", x)
  x <- gsub(".*who.*", "1", x)
  x <- gsub(".*what.*", "1", x)
  x <- gsub(".*where.*", "1", x)
  x <- gsub(".*how.*", "1", x)
  x <- gsub(".* do.*", "1", x)
  x <- gsub(".* did.*", "1", x)
  x <- ifelse(x == "1", "1", "0") } )))
# inspect data
head(data)

write.table(data, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/IntNZE-ISLE-data2.txt", row.names = F, sep="\t")
###############################################################
###############################################################
###############################################################
###             TABULARIZATION
###############################################################
###############################################################
###############################################################
# pint by tt
mean.pinttt <- by(data$pint, list(data$txtyp), table)
pri <- mean.pinttt$PrivateDialogue[order(mean.pinttt$PrivateDialogue, decreasing = T)]
pub <- mean.pinttt$PublicDialogue[order(mean.pinttt$PublicDialogue, decreasing = T)]
uns <- mean.pinttt$UnscriptedMonologue[order(mean.pinttt$UnscriptedMonologue, decreasing = T)]
scr <- mean.pinttt$ScriptedMonologue[order(mean.pinttt$ScriptedMonologue, decreasing = T)]
pri <- as.data.frame(cbind(names(pri), pri))
pub <- as.data.frame(cbind(names(pub), pub))
uns <- as.data.frame(cbind(names(uns), uns))
scr <- as.data.frame(cbind(names(scr), scr))
pinttb <- join(pri, pub, by = c("V1"), type = "full")
pinttb <- join(pinttb, uns, by = c("V1"), type = "full")
pinttb <- join(pinttb, scr, by = c("V1"), type = "full")
pinttb <- as.data.frame(apply(pinttb, 2, function(x){
  x <- ifelse(is.na(x) == T, 0, x) } ))
pinttb[,2] <- as.numeric(pinttb[,2])
pinttb[,3] <- as.numeric(pinttb[,3])
pinttb[,4] <- as.numeric(pinttb[,4])
pinttb[,5] <- as.numeric(pinttb[,5])
pinttb$total <- rowSums(pinttb[,2:5])
colnames(pinttb) <- c("intensifier", "s1a", "s1b", "s2a", "s2b", "total")
pinttb <- pinttb[order(pinttb$total, decreasing = T),]
pinttb <- rbind(pinttb, c("Total", sum(pinttb[, 2]), sum(pinttb[, 3]), sum(pinttb[, 4]), sum(pinttb[, 5]), sum(pinttb[, 6])))
pinttb

write.table(pinttb, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/pinttb.txt", row.names = F, sep="\t")
pinttb <- table(data$pint)
pinttb <- pinttb[order(table(data$pint), decreasing = T)]
pinttb <- data.frame(names(pinttb), pinttb, round(pinttb/sum(pinttb)*100, 2),
  c("", round(pinttb[2:nrow(pinttb)]/sum(pinttb[2:nrow(pinttb)])*100, 2)))
colnames(pinttb) <- c("Intensifier", "TokenFrequency", "PercentageSlots", "PercentageIntensifiers")
pinttb <- rbind(pinttb, c("Total", sum(pinttb$TokenFrequency), "", ""))
rownames(pinttb) <- NULL
head(pinttb)

# save data to disc
write.table(pinttb, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/pinttb2.txt", sep = "\t", row.names = F)
###############################################################
s1a <- data[data$txtyp == "PrivateDialogue", ]
pinttbs1a <- table(s1a$pint)
pinttbs1a <- pinttbs1a[order(table(s1a$pint), decreasing = T)]
pinttbs1a <- data.frame(names(pinttbs1a), pinttbs1a, round(pinttbs1a/sum(pinttbs1a)*100, 2),
  c("", round(pinttbs1a[2:nrow(pinttbs1a)]/sum(pinttbs1a[2:nrow(pinttbs1a)])*100, 2)))
colnames(pinttbs1a) <- c("Intensifier", "TokenFrequency", "PercentageSlots", "PercentageIntensifiers")
pinttbs1a <- rbind(pinttbs1a, c("Total", sum(pinttbs1a$TokenFrequency), "", ""))
rownames(pinttbs1a) <- NULL
head(pinttbs1a)

# save data to disc
write.table(pinttbs1a, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/pinttb_s1a.txt", sep = "\t", row.names = F)
###############################################################
# number of speakers by age and gender
ftbl <- ftable(data$txtyp, data$age, data$sex, data$int)
nspktb <- data.frame(data$txtyp, data$age, data$sex, data$flid)
nspk <- unique(nspktb)
nspk <- nspk[1:ncol(nspk)-1]
nspk <- ftable(nspk[,1], nspk[,2], nspk[,3])
nspk <- as.vector(unlist(t(nspk)))
nslot <- rowSums(ftbl)
nint <- ftbl[,2]
pcnt <- round(ftbl[,2]/nslot*100, 1)
txtyp <- c(rep(attr(ftbl, "row.vars")[[1]][[1]], 8),
  rep(attr(ftbl, "row.vars")[[1]][[2]], 8),
  rep(attr(ftbl, "row.vars")[[1]][[3]], 8),
  rep(attr(ftbl, "row.vars")[[1]][[4]], 8))
age <- rep(c(rep(attr(ftbl, "row.vars")[[2]][[1]], 2),
  rep(attr(ftbl, "row.vars")[[2]][[2]], 2),
  rep(attr(ftbl, "row.vars")[[2]][[3]], 2),
  rep(attr(ftbl, "row.vars")[[2]][[4]], 2)), 4)
sex <- rep(attr(ftbl, "row.vars")[[3]], 16)
inttb01 <- data.frame(txtyp, age, sex, nspk, nslot, nint, pcnt)
#inspect table
inttb01

# only s1a
inttb02 <- inttb01[inttb01$txtyp == "PrivateDialogue", ]
inttb02 <- rbind(inttb02, c("", "", "", sum(inttb02$nspk), sum(inttb02$nslot),
  sum(inttb02$nint), ""))
#inspect table
inttb02

# save data to disc
write.table("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/inttb01.txt", sep = "\t", row.names = F)
###############################################################
# pint by tt
mean.pinttt <- by(data$pint, list(data$txtyp), table)
pri <- mean.pinttt$PrivateDialogue[order(mean.pinttt$PrivateDialogue, decreasing = T)]
pub <- mean.pinttt$PublicDialogue[order(mean.pinttt$PublicDialogue, decreasing = T)]
uns <- mean.pinttt$UnscriptedMonologue[order(mean.pinttt$UnscriptedMonologue, decreasing = T)]
scr <- mean.pinttt$ScriptedMonologue[order(mean.pinttt$ScriptedMonologue, decreasing = T)]
pri <- as.data.frame(cbind(names(pri), pri))
pub <- as.data.frame(cbind(names(pub), pub))
uns <- as.data.frame(cbind(names(uns), uns))
scr <- as.data.frame(cbind(names(scr), scr))
pinttb <- join(pri, pub, by = c("V1"), type = "full")
pinttb <- join(pinttb, uns, by = c("V1"), type = "full")
pinttb <- join(pinttb, scr, by = c("V1"), type = "full")
pinttb <- as.data.frame(apply(pinttb, 2, function(x){
  x <- ifelse(is.na(x) == T, 0, x) } ))
pinttb[,2] <- as.numeric(pinttb[,2])
pinttb[,3] <- as.numeric(pinttb[,3])
pinttb[,4] <- as.numeric(pinttb[,4])
pinttb[,5] <- as.numeric(pinttb[,5])
pinttb$total <- rowSums(pinttb[,2:5])
colnames(pinttb) <- c("intensifier", "s1a", "s1b", "s2a", "s2b", "total")
pinttb <- pinttb[order(pinttb$total, decreasing = T),]
pinttb <- rbind(pinttb, c("Total", sum(pinttb[, 2]), sum(pinttb[, 3]), sum(pinttb[, 4]), sum(pinttb[, 5]), sum(pinttb[, 6])))
head(pinttb)

write.table(pinttb, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/pinttb.txt", row.names = F, sep="\t")
###############################################################
###############################################################
###############################################################
###             VISUALIZATION
###############################################################
###############################################################
###############################################################
# plot priming
int.prim.tb <- as.vector(unlist(by(data$int, list(data$priming), table)))
prim0 <- int.prim.tb[2]/(int.prim.tb[1]+int.prim.tb[2])*100
prim1 <- int.prim.tb[4]/(int.prim.tb[3]+int.prim.tb[4])*100
sd0 <- as.vector(unlist(by(data$int, list(data$priming), sd)))[1]
sd1 <- as.vector(unlist(by(data$int, list(data$priming), sd)))[2]
lprim0 <- length(which(data$priming == 0))
lprim1 <- length(which(data$priming == 1))
#computation of the standard errors
sem0<-sd0/sqrt(lprim0)*100
sem1<-sd1/sqrt(lprim1)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntPrim.png", width = 480, height = 480) # save plot
plot(c(prim0, prim1), xlim = c(0.5, 2.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
axis(1, c(1,2), c("No Priming", "Priming"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
arrows(1, prim0, 1, prim0+sem0, angle = 90)
arrows(1, prim0, 1, prim0-sem0, angle = 90)
arrows(2, prim1, 2, prim1+sem1, angle = 90)
arrows(2, prim1, 2, prim1-sem1, angle = 90)
#grid()
box()
# add statz
x2tb <- data.frame(c(int.prim.tb[4], int.prim.tb[3]), c(int.prim.tb[2], int.prim.tb[1]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("1","0")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.sex.tb)), 2)
text(2, 19, expression(paste(chi^{2}, " = ")))
text(2.13, 19, chi)
text(2.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(2.05, 17, "p < .001")
text(2, 16, expression(paste(phi, " = ")))
text(2.13, 16, phi)# end plot
dev.off()
###############################################################
# plot sex
int.sex.tb <- as.vector(unlist(by(data$int, list(data$sex), table)))
sexf <- int.sex.tb[2]/(int.sex.tb[1]+int.sex.tb[2])*100
sexm <- int.sex.tb[4]/(int.sex.tb[3]+int.sex.tb[4])*100
sd0 <- as.vector(unlist(by(data$int, list(data$sex), sd)))[1]
sd1 <- as.vector(unlist(by(data$int, list(data$sex), sd)))[2]
lsexf <- length(which(data$sex == "female"))
lsexm <- length(which(data$sex == "male"))
#computation of the standard errors
sem0<-sd0/sqrt(lsexf)*100
sem1<-sd1/sqrt(lsexm)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntSex.png", width = 480, height = 480) # save plot
plot(c(sexf, sexm), xlim = c(0.5, 2.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
axis(1, c(1,2), c("Female", "Male"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
arrows(1, sexf, 1, sexf+sem0, angle = 90)
arrows(1, sexf, 1, sexf-sem0, angle = 90)
arrows(2, sexm, 2, sexm+sem1, angle = 90)
arrows(2, sexm, 2, sexm-sem1, angle = 90)
#grid()
box()
# add statz
x2tb <- data.frame(c(int.sex.tb[4], int.sex.tb[3]), c(int.sex.tb[2], int.sex.tb[1]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("1","0")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.sex.tb)), 2)
text(2, 19, expression(paste(chi^{2}, " = ")))
text(2.13, 19, chi)
text(2.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(2.05, 17, "p < .001")
text(2, 16, expression(paste(phi, " = ")))
text(2.13, 16, phi)
# end plot
dev.off()
###############################################################
# plot function
int.fun.tb <- as.vector(unlist(by(data$int, list(data$fun), table)))
funa <- int.fun.tb[2]/(int.fun.tb[1]+int.fun.tb[2])*100
funp <- int.fun.tb[4]/(int.fun.tb[3]+int.fun.tb[4])*100
sd0 <- as.vector(unlist(by(data$int, list(data$fun), sd)))[1]
sd1 <- as.vector(unlist(by(data$int, list(data$fun), sd)))[2]
lfuna<- length(which(data$fun == "attributive"))
lfunp <- length(which(data$fun == "predicative"))
#computation of the standard errors
sem0<-sd0/sqrt(lfuna)*100
sem1<-sd1/sqrt(lfunp)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntFun.png", width = 480, height = 480) # save plot
plot(c(funa, funp), xlim = c(0.5, 2.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
axis(1, c(1,2), c("Attributive", "Predicative"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
arrows(1, funa, 1, funa+sem0, angle = 90)
arrows(1, funa, 1, funa-sem0, angle = 90)
arrows(2, funp, 2, funp+sem1, angle = 90)
arrows(2, funp, 2, funp-sem1, angle = 90)
#grid()
box()
# add statz
x2tb <- data.frame(c(int.fun.tb[4], int.fun.tb[3]), c(int.fun.tb[2], int.fun.tb[1]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("1","0")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.fun.tb)), 2)
text(1, 19, expression(paste(chi^{2}, " = ")))
text(1.15, 19, chi)
text(1.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(1.05, 17, "p < .001")
text(1, 16, expression(paste(phi, " = ")))
text(1.13, 16, phi)
# end plot
dev.off()
###############################################################
# plot txtyp
# extract frequencies
int.txtyp.tb <- as.vector(unlist(by(data$int, list(data$txtyp), table)))
txtyps1a <- int.txtyp.tb[2]/(int.txtyp.tb[1]+int.txtyp.tb[2])*100
txtyps1b <- int.txtyp.tb[4]/(int.txtyp.tb[3]+int.txtyp.tb[4])*100
txtyps2a <- int.txtyp.tb[6]/(int.txtyp.tb[5]+int.txtyp.tb[6])*100
txtyps2b <- int.txtyp.tb[8]/(int.txtyp.tb[7]+int.txtyp.tb[8])*100
# claculate standard errors
sd1 <- as.vector(unlist(by(data$int, list(data$txtyp), sd)))[1]
sd2 <- as.vector(unlist(by(data$int, list(data$txtyp), sd)))[2]
sd3 <- as.vector(unlist(by(data$int, list(data$txtyp), sd)))[3]
sd4 <- as.vector(unlist(by(data$int, list(data$txtyp), sd)))[4]
# extract lengths
ltxtyps1a <- length(which(data$txtyp == "PrivateDialogue"))
ltxtyps1b <- length(which(data$txtyp == "PublicDialogue"))
ltxtyps2a <- length(which(data$txtyp == "UnscriptedMonologue"))
ltxtyps2b <- length(which(data$txtyp == "ScriptedMonologue"))
#computation of the standard errors
sem1<-sd1/sqrt(ltxtyps1a)*100
sem2<-sd2/sqrt(ltxtyps1b)*100
sem3<-sd3/sqrt(ltxtyps2a)*100
sem4<-sd4/sqrt(ltxtyps2b)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntTxtyp.png", width = 580, height = 480) # save plot
plot(c(txtyps1a, txtyps1b, txtyps2a, txtyps2b), xlim = c(0.5, 4.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
# add axes
axis(1, seq(1,4,1), c("Private Dialogue", "Public Dialogue", "Unscripted Monologue",
  "ScriptedMonologue"), cex.axis = 0.8)
axis(2, seq(0, 20, 5), seq(0, 20, 5))
# add arrows s1a
arrows(1, txtyps1a, 1, txtyps1a+sem0, angle = 90)
arrows(1, txtyps1a, 1, txtyps1a-sem0, angle = 90)
# add arrows s1b
arrows(2, txtyps1b, 2, txtyps1b+sem1, angle = 90)
arrows(2, txtyps1b, 2, txtyps1b-sem1, angle = 90)
# add arrows s2a
arrows(3, txtyps2a, 3, txtyps2a+sem1, angle = 90)
arrows(3, txtyps2a, 3, txtyps2a-sem1, angle = 90)
# add arrows s1b
arrows(4, txtyps2b, 4, txtyps2b+sem1, angle = 90)
arrows(4, txtyps2b, 4, txtyps2b-sem1, angle = 90)
# add grid and box
#grid()
box()
# add statz
x2tb <- data.frame(c(int.txtyp.tb[2], int.txtyp.tb[1]), c(int.txtyp.tb[4], int.txtyp.tb[3]),
  c(int.txtyp.tb[6], int.txtyp.tb[5]), c(int.txtyp.tb[8], int.txtyp.tb[7]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("PrivateDialogue", "PublicDialogue", "UnscriptedMonologue", "ScriptedMonologue")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.txtyp.tb)), 2)
text(1, 19, expression(paste(chi^{2}, " = ")))
text(1.2, 19, chi)
text(1.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(1.1, 17, "p < .001")
text(1, 16, expression(paste(phi, " = ")))
text(1.15, 16, phi)
# end plot
dev.off()
###############################################################
# plot age
# extract frequencies
int.age.tb <- as.vector(unlist(by(data$int, list(data$age), table)))
int1624 <- int.age.tb[2]/(int.age.tb[1]+int.age.tb[2])*100
int2539 <- int.age.tb[4]/(int.age.tb[3]+int.age.tb[4])*100
int4049 <- int.age.tb[6]/(int.age.tb[5]+int.age.tb[6])*100
int50 <- int.age.tb[8]/(int.age.tb[7]+int.age.tb[8])*100
# claculate standard errors
sd1 <- as.vector(unlist(by(data$int, list(data$age), sd)))[1]
sd2 <- as.vector(unlist(by(data$int, list(data$age), sd)))[2]
sd3 <- as.vector(unlist(by(data$int, list(data$age), sd)))[3]
sd4 <- as.vector(unlist(by(data$int, list(data$age), sd)))[4]
# extract lengths
l1624 <- length(which(data$age == "16-24"))
l2539 <- length(which(data$age == "25-39"))
l4049 <- length(which(data$age == "40-49"))
l50 <- length(which(data$age == "50+"))
#computation of the standard errors
sem1<-sd1/sqrt(l1624)*100
sem2<-sd2/sqrt(l2539)*100
sem3<-sd3/sqrt(l4049)*100
sem4<-sd4/sqrt(l50)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntAge.png", width = 480, height = 480) # save plot
plot(c(int1624, int2539, int4049, int50), xlim = c(0.5, 4.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
# add axes
axis(1, seq(1,4,1), c("16-24", "25-39", "40-49", "50+"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
# add arrows 16-24
arrows(1, int1624, 1, int1624+sem0, angle = 90)
arrows(1, int1624, 1, int1624-sem0, angle = 90)
# add arrows 25-39
arrows(2, int2539, 2, int2539+sem1, angle = 90)
arrows(2, int2539, 2, int2539-sem1, angle = 90)
# add arrows 40-49
arrows(3, int4049, 3, int4049+sem1, angle = 90)
arrows(3, int4049, 3, int4049-sem1, angle = 90)
# add arrows 50+
arrows(4, int50, 4, int50+sem1, angle = 90)
arrows(4, int50, 4, int50-sem1, angle = 90)
# add grid and box
#grid()
box()
# add statz
x2tb <- data.frame(c(int.age.tb[2], int.age.tb[1]), c(int.age.tb[4], int.age.tb[3]),
  c(int.age.tb[6], int.age.tb[5]), c(int.age.tb[8], int.age.tb[7]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("16-24", "25-39", "40-49", "50+")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.txtyp.tb)), 2)
text(1, 19, expression(paste(chi^{2}, " = ")))
text(1.3, 19, chi)
text(1.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(1.1, 17, "p < .001")
text(1, 16, expression(paste(phi, " = ")))
text(1.3, 16, phi)
# end plot
dev.off()
###############################################################
# plot age : s1a
s1a <- data[data$txtyp == "PrivateDialogue", ]

# extract frequencies
int.age.tb <- as.vector(unlist(by(s1a$int, list(s1a$age), table)))
int1624 <- int.age.tb[2]/(int.age.tb[1]+int.age.tb[2])*100
int2539 <- int.age.tb[4]/(int.age.tb[3]+int.age.tb[4])*100
int4049 <- int.age.tb[6]/(int.age.tb[5]+int.age.tb[6])*100
int50 <- int.age.tb[8]/(int.age.tb[7]+int.age.tb[8])*100
# claculate standard errors
sd1 <- as.vector(unlist(by(data$int, list(data$age), sd)))[1]
sd2 <- as.vector(unlist(by(data$int, list(data$age), sd)))[2]
sd3 <- as.vector(unlist(by(data$int, list(data$age), sd)))[3]
sd4 <- as.vector(unlist(by(data$int, list(data$age), sd)))[4]
# extract lengths
l1624 <- length(which(data$age == "16-24"))
l2539 <- length(which(data$age == "25-39"))
l4049 <- length(which(data$age == "40-49"))
l50 <- length(which(data$age == "50+"))
#computation of the standard errors
sem1<-sd1/sqrt(l1624)*100
sem2<-sd2/sqrt(l2539)*100
sem3<-sd3/sqrt(l4049)*100
sem4<-sd4/sqrt(l50)*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/IntAgeS1A.png", width = 480, height = 480) # save plot
plot(c(int1624, int2539, int4049, int50), xlim = c(0.5, 4.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
# add axes
axis(1, seq(1,4,1), c("16-24", "25-39", "40-49", "50+"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
# add arrows 16-24
arrows(1, int1624, 1, int1624+sem0, angle = 90)
arrows(1, int1624, 1, int1624-sem0, angle = 90)
# add arrows 25-39
arrows(2, int2539, 2, int2539+sem1, angle = 90)
arrows(2, int2539, 2, int2539-sem1, angle = 90)
# add arrows 40-49
arrows(3, int4049, 3, int4049+sem1, angle = 90)
arrows(3, int4049, 3, int4049-sem1, angle = 90)
# add arrows 50+
arrows(4, int50, 4, int50+sem1, angle = 90)
arrows(4, int50, 4, int50-sem1, angle = 90)
# add grid and box
grid()
box()
# add statz
x2tb <- data.frame(c(int.age.tb[2], int.age.tb[1]), c(int.age.tb[4], int.age.tb[3]),
  c(int.age.tb[6], int.age.tb[5]), c(int.age.tb[8], int.age.tb[7]))
rownames(x2tb) <- c("1","0")
colnames(x2tb) <- c("16-24", "25-39", "40-49", "50+")
x2 <- chisq.test(x2tb)
chi <- round(as.vector(unlist(x2[1])), 2)
phi <- round(sqrt(chi/sum(int.txtyp.tb)), 2)
text(1, 19, expression(paste(chi^{2}, " = ")))
text(1.3, 19, chi)
text(1.02, 18, paste("df = ", as.vector(x2[2]), sep = ""))
text(1.1, 17, "p < .001")
text(1, 16, expression(paste(phi, " = ")))
text(1.3, 16, phi)
# end plot
dev.off()

###############################################################
# plot very : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract frequencies
very.age.tb <- (tapply(s1a$very, list(s1a$age), mean))*100
very1624 <- very.age.tb[1]
very2539 <- very.age.tb[2]
very4049 <- very.age.tb[3]
very50 <- very.age.tb[4]
# claculate standard deviations
sd1 <- (tapply(s1a$very, list(s1a$age), sd))[1]
sd2 <- (tapply(s1a$very, list(s1a$age), sd))[2]
sd3 <- (tapply(s1a$very, list(s1a$age), sd))[3]
sd4 <- (tapply(s1a$very, list(s1a$age), sd))[4]
# extract lengths
lage1624 <- length(which(s1a$age == "16-24"))
lages2539 <- length(which(s1a$age == "25-39"))
lages4049 <- length(which(s1a$age == "40-49"))
lages50 <- length(which(s1a$age == "50+"))
#computation of the standard errors
sem1<-sd1/sqrt(lage1624)*100
sem2<-sd2/sqrt(lages2539)*100
sem3<-sd3/sqrt(lages4049)*100
sem4<-sd4/sqrt(lages50)*100
# calculate sex differences
very.age.sex.tb <- (tapply(s1a$very, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/VeryAgeS1aOnlyInts.png", width = 480, height = 480) # save plot
plot(c(very1624, very2539, very4049, very50), xlim = c(0.5, 4.5), axes = F,
  ylim = c(0, 70), xlab = "", ylab = "", pch = 20, ann = F)
points(very.age.sex.tb[,1], col = "red", pch = "+")
points(very.age.sex.tb[,2], col = "blue", pch = "+")
lines(very.age.sex.tb[,1], col = "red", lty = 2, lwd = 2)
lines(very.age.sex.tb[,2], col = "blue", lty = 2, lwd = 2)
# add axes
axis(1, seq(1,4,1), rownames(very.age.sex.tb), cex.axis = 1.5)
axis(2, seq(0, 70, 10), seq(0, 70, 10), las = 2, cex.axis = 1.5)
mtext("Percent of VERY of all Intensifiers\nin Private Dialogue", 3,1,cex = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
# add arrows s1a
arrows(1, very1624, 1, very1624+sem0, angle = 90)
arrows(1, very1624, 1, very1624-sem0, angle = 90)
# add arrows s1b
arrows(2, very2539, 2, very2539+sem1, angle = 90)
arrows(2, very2539, 2, very2539-sem1, angle = 90)
# add arrows s2a
arrows(3, very4049, 3, very4049+sem1, angle = 90)
arrows(3, very4049, 3, very4049-sem1, angle = 90)
# add arrows s1b
arrows(4, very50, 4, very50+sem1, angle = 90)
arrows(4, very50, 4, very50-sem1, angle = 90)
# add legend
legend("topleft", legend = c("female", "male"), col = c("red", "blue"),
       border = "black", lty= c(2, 2))
# add grid and box
#grid()
#box()
# end plot
dev.off()
###############################################################
# plot really : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract frequencies
really.age.tb <- (tapply(s1a$really, list(s1a$age), mean))*100
really1624 <- really.age.tb[1]
really2539 <- really.age.tb[2]
really4049 <- really.age.tb[3]
really50 <- really.age.tb[4]
# claculate standard deviations
sd1 <- (tapply(s1a$really, list(s1a$age), sd))[1]
sd2 <- (tapply(s1a$really, list(s1a$age), sd))[2]
sd3 <- (tapply(s1a$really, list(s1a$age), sd))[3]
sd4 <- (tapply(s1a$really, list(s1a$age), sd))[4]
# extract lengths
lage1624 <- length(which(s1a$age == "16-24"))
lages2539 <- length(which(s1a$age == "25-39"))
lages4049 <- length(which(s1a$age == "40-49"))
lages50 <- length(which(s1a$age == "50+"))
#computation of the standard errors
sem1<-sd1/sqrt(lage1624)*100
sem2<-sd2/sqrt(lages2539)*100
sem3<-sd3/sqrt(lages4049)*100
sem4<-sd4/sqrt(lages50)*100
# calculate sex differences
really.age.sex.tb <- (tapply(s1a$really, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/reallyAgeS1aOnlyInts.png",
  width = 480, height = 480) # save plot
plot(c(really1624, really2539, really4049, really50), xlim = c(0.5, 4.5), axes = F,
  ylim = c(0, 70), main = "Percent of REALLY of all Intensifiers\nin Private Dialogue",
  xlab = "", ylab = "", pch = 20, ann = F)
points(really.age.sex.tb[,1], col = "red", pch = "+")
points(really.age.sex.tb[,2], col = "blue", pch = "+")
lines(really.age.sex.tb[,1], col = "red", lty = 2, lwd = 2)
lines(really.age.sex.tb[,2], col = "blue", lty = 2, lwd = 2)
# add axes
axis(1, seq(1,4,1), rownames(really.age.sex.tb), cex.axis = 1.5)
axis(2, seq(0, 70, 10), seq(0, 70, 10), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
mtext("Percent of REALLY of all Intensifiers\nin Private Dialogue", 3,1,cex = 1.5)

# add arrows s1a
arrows(1, really1624, 1, really1624+sem0, angle = 90)
arrows(1, really1624, 1, really1624-sem0, angle = 90)
# add arrows s1b
arrows(2, really2539, 2, really2539+sem1, angle = 90)
arrows(2, really2539, 2, really2539-sem1, angle = 90)
# add arrows s2a
arrows(3, really4049, 3, really4049+sem1, angle = 90)
arrows(3, really4049, 3, really4049-sem1, angle = 90)
# add arrows s1b
arrows(4, really50, 4, really50+sem1, angle = 90)
arrows(4, really50, 4, really50-sem1, angle = 90)
# add legend
legend("topright", legend = c("female", "male"), col = c("red", "blue"),
       border = "black", lty= c(2, 2))
# add grid and box
#grid()
#box()
# end plot
dev.off()
###############################################################
# plot so : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract frequencies
so.age.tb <- (tapply(s1a$so, list(s1a$age), mean))*100
so1624 <- so.age.tb[1]
so2539 <- so.age.tb[2]
so4049 <- so.age.tb[3]
so50 <- so.age.tb[4]
# claculate standard deviations
sd1 <- (tapply(s1a$so, list(s1a$age), sd))[1]
sd2 <- (tapply(s1a$so, list(s1a$age), sd))[2]
sd3 <- (tapply(s1a$so, list(s1a$age), sd))[3]
sd4 <- (tapply(s1a$so, list(s1a$age), sd))[4]
# extract lengths
lage1624 <- length(which(s1a$age == "16-24"))
lages2539 <- length(which(s1a$age == "25-39"))
lages4049 <- length(which(s1a$age == "40-49"))
lages50 <- length(which(s1a$age == "50+"))
#computation of the standard errors
sem1<-sd1/sqrt(lage1624)*100
sem2<-sd2/sqrt(lages2539)*100
sem3<-sd3/sqrt(lages4049)*100
sem4<-sd4/sqrt(lages50)*100
# calculate sex differences
so.age.sex.tb <- (tapply(s1a$so, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/soAgeS1aOnlyInts.png",
  width = 480, height = 480) # save plot
plot(c(so1624, so2539, so4049, so50), xlim = c(0.5, 4.5), axes = F, ylim = c(0, 70),
  main = "Percent of SO of all Intensifiers\nin Private Dialogue",
  xlab = "Age", ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
points(so.age.sex.tb[,1], col = "salmon", pch = "+")
points(so.age.sex.tb[,2], col = "lightblue", pch = "+")
lines(so.age.sex.tb[,1], col = "salmon", lty = 2)
lines(so.age.sex.tb[,2], col = "lightblue", lty = 2)
# add axes
axis(1, seq(1,4,1), c("16-24", "25-39", "40-49", "50+"))
axis(2, seq(0, 70, 10), seq(0, 70, 10))
# add arrows s1a
arrows(1, so1624, 1, so1624+sem0, angle = 90)
arrows(1, so1624, 1, so1624-sem0, angle = 90)
# add arrows s1b
arrows(2, so2539, 2, so2539+sem1, angle = 90)
arrows(2, so2539, 2, so2539-sem1, angle = 90)
# add arrows s2a
arrows(3, so4049, 3, so4049+sem1, angle = 90)
arrows(3, so4049, 3, so4049-sem1, angle = 90)
# add arrows s1b
arrows(4, so50, 4, so50+sem1, angle = 90)
arrows(4, so50, 4, so50-sem1, angle = 90)
# add legend
legend("topleft", legend = c("female", "male"), col = c("salmon", "lightblue"),
       border = "black", lty= c(2, 2))
# add grid and box
#grid()
box()
# end plot
dev.off()
###############################################################
# plot ttr : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract ttrs by age
ttr.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
ttr1624 <- length(ttr.age.tb[[1]])/sum(ttr.age.tb[[1]])
ttr2539 <- length(ttr.age.tb[[2]])/sum(ttr.age.tb[[2]])
ttr4049 <- length(ttr.age.tb[[3]])/sum(ttr.age.tb[[3]])
ttr50 <- length(ttr.age.tb[[4]])/sum(ttr.age.tb[[4]])
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/TTRAgeS1aOnlyInts.png", width = 480, height = 480) # save plot
plot(c(ttr1624, ttr2539, ttr4049, ttr50), xlim = c(0.5, 4.5), axes = F, ylim = c(0, .25),
  main = "Type-Token-Ratio of Intensifies\nin Private Dialogue by Age",
  xlab = "Age", ylab = "Type-Token-Ratio of Intensifies", pch = 20)
# add axes
axis(1, seq(1,4,1), c("16-24", "25-39", "40-49", "50+"))
axis(2, seq(0, .25, .05), seq(0, .25, .05))
# add regression line
ttrtb <- data.frame(c(1:4), c(ttr1624, ttr2539, ttr4049, ttr50))
colnames(ttrtb) <- c("age", "ttr")
abline(lm(ttrtb[,2]~ttrtb[,1]), col="red", lty = 3)
# add grid and box
#grid()
box()
# end plot
dev.off()
###############################################################
# plot ttr : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract ttrs by age
t.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
t1624 <- length(t.age.tb[[1]])
t2539 <- length(t.age.tb[[2]])
t4049 <- length(t.age.tb[[3]])
t50 <- length(t.age.tb[[4]])
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/TAgeS1aOnlyInts.png", width = 480, height = 480) # save plot
x <- barplot(c(t1624, t2539, t4049, t50), beside = T, xlim = c(0, 5), axes = F, ylim = c(0, 20),
  main = "Types of Intensifies\nin Private Dialogue by Age",
  xlab = "Age", ylab = "Absolute frequency (Types of Intensifies)", pch = 20)
# add axes
axis(1, seq(0.7,4.3,1.2), c("16-24", "25-39", "40-49", "50+"))
axis(2, seq(0, 20, 5), seq(0, 20, 5))
# add text
text(seq(0.7,4.3,1.2), c(t1624, t2539, t4049, t50)+2, c(t1624, t2539, t4049, t50))
# add regression line
ttb <- data.frame(seq(0.7,4.3,1.2), c(t1624, t2539, t4049, t50))
colnames(ttb) <- c("age", "ttr")
abline(lm(ttb[,2]~ttb[,1]), col="red", lty = 3)
# add grid and box
#grid()
box()
# end plot
dev.off()
###############################################################
# plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract ttrs by age
tt.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
tt1624 <- tt.age.tb[[1]]
tt1624 <- as.data.frame(tt1624)
colnames(tt1624) <- c("Intensifier", "16-24")
###
tt2539 <- tt.age.tb[[2]]
tt2539 <- as.data.frame(tt2539)
colnames(tt2539) <- c("Intensifier", "25-39")
###
tt4049 <- tt.age.tb[[3]]
tt4049 <- as.data.frame(tt4049)
colnames(tt4049) <- c("Intensifier", "40-49")
###
tt50 <- tt.age.tb[[4]]
tt50 <- as.data.frame(tt50)
colnames(tt50) <- c("Intensifier", "50+")
###
tt1639 <- join(tt1624, tt2539, by = "Intensifier", type = "full")
tt1649 <- join(tt1639, tt4049, by = "Intensifier", type = "full")
tt <- join(tt1649, tt50, by = "Intensifier", type = "full")
tt <- as.data.frame(t(apply(tt, 1, function(x){  x <- ifelse(is.na(x) == T, 0, x) } )))
tt[,2] <- as.numeric(tt[,2])
tt[,3] <- as.numeric(tt[,3])
tt[,4] <- as.numeric(tt[,4])
tt[,5] <- as.numeric(tt[,5])
tt$cs <- rowSums(tt[, c(2:5)])
#tt <- tt[tt$cs >= 5,]
tt <- tt[order(tt[,6], decreasing = T),]
rownames(tt) <- tt$Intensifier
tt <- tt[, 2:5]
tt <- as.matrix(tt)
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/FreqAgeS1aOnlyInts.png", width = 1000, height = 480) # save plot
barplot(tt, beside = T, ylim = c(0, 120),
  main = "Token frequency of Intensifies\nin Private Dialogue by Age",
  xlab = "Age", ylab = "Types and token frequency of Intensifies",
  col = c("orange", "lightgreen", "lightblue", rep("lightgrey", 17)))
# add legend
legend("topright", legend = rownames(tt), fill = c("orange", "lightgreen", "lightblue", rep("lightgrey", 17)),
       border = "black")
# add grid and box
#grid()
box()
# end plot
dev.off()
###############################################################
# plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract ttrs by age
tt.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
tt1624 <- tt.age.tb[[1]]
tt1624 <- as.data.frame(tt1624)
colnames(tt1624) <- c("Intensifier", "16-24")
###
tt2539 <- tt.age.tb[[2]]
tt2539 <- as.data.frame(tt2539)
colnames(tt2539) <- c("Intensifier", "25-39")
###
tt4049 <- tt.age.tb[[3]]
tt4049 <- as.data.frame(tt4049)
colnames(tt4049) <- c("Intensifier", "40-49")
###
tt50 <- tt.age.tb[[4]]
tt50 <- as.data.frame(tt50)
colnames(tt50) <- c("Intensifier", "50+")
###
tt1639 <- join(tt1624, tt2539, by = "Intensifier", type = "full")
tt1649 <- join(tt1639, tt4049, by = "Intensifier", type = "full")
tt <- join(tt1649, tt50, by = "Intensifier", type = "full")
tt <- as.data.frame(t(apply(tt, 1, function(x){  x <- ifelse(is.na(x) == T, 0, x) } )))
tt[,2] <- as.numeric(tt[,2])
tt[,3] <- as.numeric(tt[,3])
tt[,4] <- as.numeric(tt[,4])
tt[,5] <- as.numeric(tt[,5])
tt$cs <- rowSums(tt[, c(2:5)])
#tt <- tt[tt$cs >= 5,]
tt <- tt[order(tt[,6], decreasing = T),]
rownames(tt) <- tt$Intensifier
tt <- tt[, 2:5]
tt <- as.matrix(tt)
tt[,1] <- round(tt[,1]/sum(tt[,1])*100, 1)
tt[,2] <- round(tt[,2]/sum(tt[,2])*100, 1)
tt[,3] <- round(tt[,3]/sum(tt[,3])*100, 1)
tt[,4] <- round(tt[,4]/sum(tt[,4])*100, 1)
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/PercentAgeS1aOnlyInts.png", width = 1000, height = 480) # save plot
barplot(tt, beside = T, ylim = c(0, 80),
  main = "Percent of Intensifier Tokens\nin Private Dialogue by Age",
  xlab = "Age", ylab = "Types and token frequency of Intensifies",
  col = c("orange", "lightgreen", "lightblue", rep("lightgrey", 17)))
# add legend
legend("topright", legend = rownames(tt), fill = c("orange", "lightgreen", "lightblue", rep("lightgrey", 17)),
       border = "black")
# add grid and box
#grid()
box()
# end plot
dev.off()
##############################################################
#                 AGE plus SEX
###############################################################
# line plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
#s1a <- s1a[s1a$fun == "predicative", ]
# extract ttrs by age
tt.age.tb <- ftable(s1a$pint, s1a$age)
cols <- as.vector(unlist(attr(tt.age.tb, "col.vars")[1]))
rows <- as.vector(unlist(attr(tt.age.tb, "row.vars")[1]))
rows <- as.vector(unlist(rows[order(rowSums(tt.age.tb), decreasing = T)]))
tt.age.tb <- tt.age.tb[order(rowSums(tt.age.tb), decreasing = T),]
tt.age.tb.pc <- data.frame(tt.age.tb[, 1]/colSums(tt.age.tb)[1]*100,
  tt.age.tb[, 2]/colSums(tt.age.tb)[2]*100,
  tt.age.tb[, 3]/colSums(tt.age.tb)[3]*100,
  tt.age.tb[, 4]/colSums(tt.age.tb)[4]*100)
colnames(tt.age.tb.pc) <- cols
rownames(tt.age.tb.pc) <- rows
tt.age.tb.pc <- tt.age.tb.pc[which(rowSums(tt.age.tb.pc) > 2),]
tt.age.tb.pc <- tt.age.tb.pc[2:nrow(tt.age.tb.pc), ]
ttsel <- as.matrix(tt.age.tb.pc)
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/PercentAgeS1aOnlyIntsLine.png",  width = 480, height = 480) # save plot
plot(ttsel[1,], type = "o", ylim = c(0, 8), col = "green",
  xlab = "", ylab = "", xlim = c(0,5), axes = F, lwd = 2, lty = 1, pch = 1, cex.axis = 1.5)
lines(ttsel[2,], type = "o", lwd = 2,  lty = 2, pch = 2, col = "orange")
lines(ttsel[3,], type = "o", lwd = 2,  lty = 3, pch = 3, col = "gray")
lines(ttsel[4,], type = "o", lwd = 2,  lty = 4, pch = 4, col = "gray")
lines(ttsel[5,], type = "o", lwd = 2,  lty = 5, pch = 5, col = "gray")
# add axes
axis(1, 1:4, colnames(ttsel), cex.axis = 1.5)
axis(2, seq(0, 8, 1), seq(0, 8, 1), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
# add legend
legend("topleft", inset = .05, rownames(ttsel),
  horiz = F,  pch = 1:5, lty = 1:5, col = c("green", "orange", rep("grey", 3)))
# end plot
dev.off()

###############################################################
# line plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
#s1a <- s1a[s1a$fun == "predicative", ]
# extract ttrs by age
tt.age.tb <- ftable(s1a$pint, s1a$ageorig)
cols <- as.vector(unlist(attr(tt.age.tb, "col.vars")[1]))
rows <- as.vector(unlist(attr(tt.age.tb, "row.vars")[1]))
rows <- as.vector(unlist(rows[order(rowSums(tt.age.tb), decreasing = T)]))
tt.age.tb <- tt.age.tb[order(rowSums(tt.age.tb), decreasing = T),]
tt.age.tb.pc <- data.frame(tt.age.tb[, 1]/colSums(tt.age.tb)[1]*100,
  tt.age.tb[, 2]/colSums(tt.age.tb)[2]*100,
  tt.age.tb[, 3]/colSums(tt.age.tb)[3]*100,
  tt.age.tb[, 4]/colSums(tt.age.tb)[4]*100,
  tt.age.tb[, 5]/colSums(tt.age.tb)[5]*100,
  tt.age.tb[, 6]/colSums(tt.age.tb)[6]*100,
  tt.age.tb[, 7]/colSums(tt.age.tb)[7]*100,
  tt.age.tb[, 8]/colSums(tt.age.tb)[8]*100,
  tt.age.tb[, 9]/colSums(tt.age.tb)[9]*100,
  tt.age.tb[, 10]/colSums(tt.age.tb)[10]*100)
colnames(tt.age.tb.pc) <- cols
rownames(tt.age.tb.pc) <- rows
tt.age.tb.pc <- tt.age.tb.pc[which(rowSums(tt.age.tb.pc) > 5),]
tt.age.tb.pc <- tt.age.tb.pc[2:nrow(tt.age.tb.pc), ]
ttsel <- as.matrix(tt.age.tb.pc)
# plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/PercentAgeOrigS1aOnlyIntsLine.png",  width = 900, height = 480) # save plot
plot(ttsel[1,], type = "o", ylim = c(0, 8), col = "blue",
  xlab = "", ylab = "", xlim = c(0,11), axes = F, lwd = 2, lty = 1, pch = 1, cex.axis = 1.5)
lines(ttsel[2,], type = "o", lwd = 2,  lty = 2, pch = 2, col = "red")
lines(ttsel[3,], type = "o", lwd = 2,  lty = 3, pch = 3, col = "gray")
lines(ttsel[4,], type = "o", lwd = 2,  lty = 4, pch = 4, col = "gray")
lines(ttsel[5,], type = "o", lwd = 2,  lty = 5, pch = 5, col = "gray")
# add axes
axis(1, 1:10, colnames(ttsel), cex.axis = 1.5)
axis(2, seq(0, 8, 2), seq(0, 8, 2), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
# add legend
legend("topleft", inset = .05, rownames(ttsel),
  horiz = F,  pch = 1:5, lty = 1:5, col = c("blue", "red", rep("grey", 3)))
# end plot
dev.off()
###############################################################
# extract lexical diversity of intensifiers
tbpint <- table(data$pint)
tbpint2 <- tbpint[order(table(data$pint), decreasing = T)]
intyps <- names(which(tbpint2 > 10))
pintadj <- data.frame(data$pint, data$adjs)
colnames(pintadj) <- gsub("data.", "", colnames(pintadj))

lexdivm <- as.vector(unlist(sapply(intyps, function(x){
  x <- pintadj[pintadj$pint == x,]
  xtf <- length(table(x$adjs))/sum(table(x$adjs))
  return(xtf)
  } )))
  # plot
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/TTRIntsAdj.png", width = 750, height = 480) # save plot
plot(lexdivm[2:length(lexdivm)], axes = F, ylim = c(0, 3), xlab = "Intensifier type",
  ylab = "Lexical diversity measure", pch = 20)
axis(1, 1:length(lexdivm)-1, intyps, cex.axis = .8)
axis(2, seq(0,3,.5), seq(0,3,.5))
lines(lexdivm[2:length(lexdivm)], lty = 2, lwd = 1, col = "red")
# add grid and box
grid()
box()
# end plot
dev.off()
###############################################################



###############################################################
# find collocates
# create table
t1 <- tapply(data$int, list(data$adjs, data$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
collex <- function(data = data, cv1 = cv1){
  # set up rslttb
  rslttb <- matrix(c("int", "adj", "or", "p"), ncol = 4)
  colnames(rslttb) <- c("Intensifier", "Adjective", "OddsRatio", "p-Value")
  rvs <- 1:nrow(t3)
  # define column values
  cv0 <- 1
  # set up table
  sapply(rvs, function(x){
    # extract values
    b2 <- t3[x,cv1] # freq adj with int
    b3 <- sum(t3[x,])-b2 # freq adj without int
    c2 <- sum(t3[,cv1])-b2   # freq int general
    c3 <- sum(t3[,cv0])-t3[x,cv0] # freq adj without int general
    # set up table
    collextb <- matrix(c(b2, b3, c2, c3), ncol = 2, byrow = F)
    # add row names
    rownames(collextb) <- c("Int", "NoInt")
    # add column names
    colnames(collextb) <- c("Adj", "AdjGen")
    # perform fisher's exact test
    rslt <- fisher.test(collextb)
    # set up table with results
    rslttb <- list(c(colnames(data)[cv1], rownames(data)[x],
      as.vector(unlist(rslt[3])), as.vector(unlist(rslt[1]))))
    # return results
    return(rslttb)
    } )
    }
# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
absolutely <- collex(data = t3, cv1 = 2)
actually  <- collex(data = t3, cv1 = 3)
completely  <- collex(data = t3, cv1 = 11)
extremely  <- collex(data = t3, cv1 = 21)
highly  <- collex(data = t3, cv1 = 25)
incredibly  <- collex(data = t3, cv1 = 28)
particularly  <- collex(data = t3, cv1 = 33)
pretty  <- collex(data = t3, cv1 = 36)
real  <- collex(data = t3, cv1 = 39)
really  <- collex(data = t3, cv1 = 40)
so  <- collex(data = t3, cv1 = 45)
totally  <- collex(data = t3, cv1 = 52)
very  <- collex(data = t3, cv1 = 55)
well  <- collex(data = t3, cv1 = 57)
# extract informaltion
absolutely <- matrix(unlist(absolutely),ncol=4,byrow=TRUE)
actually <- matrix(unlist(actually),ncol=4,byrow=TRUE)
completely <- matrix(unlist(completely),ncol=4,byrow=TRUE)
extremely <- matrix(unlist(extremely),ncol=4,byrow=TRUE)
highly <- matrix(unlist(highly),ncol=4,byrow=TRUE)
incredibly <- matrix(unlist(incredibly),ncol=4,byrow=TRUE)
particularly <- matrix(unlist(particularly),ncol=4,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
totally <- matrix(unlist(totally),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
well <- matrix(unlist(well),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(absolutely, actually, completely, extremely, highly, incredibly,
  particularly, pretty, real, really, so, totally, very, well)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[collexdf$p <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
head(sigcollexdf)

corrsigcollexdf

###############################################################
# collocation analysis age 16-24
dfyoung <- data[data$age == "16-24",]
t1 <- tapply(dfyoung$int, list(dfyoung$adjs, dfyoung$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
pretty  <- collex(data = t3, cv1 = 15)
real  <- collex(data = t3, cv1 = 16)
really  <- collex(data = t3, cv1 = 17)
so  <- collex(data = t3, cv1 = 19)
very  <- collex(data = t3, cv1 = 23)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdfyoung <- as.data.frame(collextab)
# add colnames
colnames(collexdfyoung) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdfyoung)
collexdfyoung$corr05 <- rep(corr05, nrow(collexdfyoung))
corr01 <- 0.01/nrow(collexdf)
collexdfyoung$corr01 <- rep(corr01, nrow(collexdfyoung))
corr001 <- 0.001/nrow(collexdf)
collexdfyoung$corr001 <- rep(corr001, nrow(collexdfyoung))
# calculate corrected significance status
collexdfyoung$sig <- as.vector(unlist(sapply(collexdfyoung$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdfyoung <- collexdfyoung[collexdfyoung$p <= .05, ]
corrsigcollexdfyoung <- collexdfyoung[collexdfyoung$sig != "n.s.", ]
# inspect results
head(sigcollexdfyoung)

corrsigcollexdfyoung

###############################################################
# collocation analysis age 25-39
dfmedy <- data[data$age == "25-39",]
t1 <- tapply(dfmedy$int, list(dfmedy$adjs, dfmedy$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
extremely  <- collex(data = t3, cv1 = 9)
pretty  <- collex(data = t3, cv1 = 17)
real  <- collex(data = t3, cv1 = 19)
really  <- collex(data = t3, cv1 = 20)
so  <- collex(data = t3, cv1 = 21)
very  <- collex(data = t3, cv1 = 25)
# extract informaltion
extremely <- matrix(unlist(extremely),ncol=4,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(extremely, pretty, real, really, so, very)
# convert into data frame
collexdfmedy <- as.data.frame(collextab)
# add colnames
colnames(collexdfmedy) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdfmedy)
collexdfmedy$corr05 <- rep(corr05, nrow(collexdfmedy))
corr01 <- 0.01/nrow(collexdf)
collexdfmedy$corr01 <- rep(corr01, nrow(collexdfmedy))
corr001 <- 0.001/nrow(collexdf)
collexdfmedy$corr001 <- rep(corr001, nrow(collexdfmedy))
# calculate corrected significance status
collexdfmedy$sig <- as.vector(unlist(sapply(collexdfmedy$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdfmedy <- collexdfmedy[collexdfmedy$p <= .05, ]
corrsigcollexdfmedy <- collexdfmedy[collexdfmedy$sig != "n.s.", ]
# inspect results
head(sigcollexdfmedy)

corrsigcollexdfmedy

###############################################################
# collocation analysis age 40-49
dfmedo <- data[data$age == "40-49",]
t1 <- tapply(dfmedo$int, list(dfmedo$adjs, dfmedo$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
absolutely  <- collex(data = t3, cv1 = 2)
completely  <- collex(data = t3, cv1 = 4)
pretty  <- collex(data = t3, cv1 = 13)
real  <- collex(data = t3, cv1 = 16)
really  <- collex(data = t3, cv1 = 17)
so  <- collex(data = t3, cv1 = 21)
very  <- collex(data = t3, cv1 = 29)
well  <- collex(data = t3, cv1 = 30)
# extract informaltion
absolutely <- matrix(unlist(absolutely),ncol=4,byrow=TRUE)
completely <- matrix(unlist(completely),ncol=4,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
well <- matrix(unlist(well),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(extremely, pretty, real, really, so, very)
# convert into data frame
collexdfmedo <- as.data.frame(collextab)
# add colnames
colnames(collexdfmedo) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdfmedo)
collexdfmedo$corr05 <- rep(corr05, nrow(collexdfmedo))
corr01 <- 0.01/nrow(collexdf)
collexdfmedo$corr01 <- rep(corr01, nrow(collexdfmedo))
corr001 <- 0.001/nrow(collexdf)
collexdfmedo$corr001 <- rep(corr001, nrow(collexdfmedo))
# calculate corrected significance status
collexdfmedo$sig <- as.vector(unlist(sapply(collexdfmedo$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdfmedo <- collexdfmedo[collexdfmedo$p <= .05, ]
corrsigcollexdfmedo <- collexdfmedo[collexdfmedo$sig != "n.s.", ]
# inspect results
head(sigcollexdfmedo)

corrsigcollexdfmedo


###############################################################
# collocation analysis age 50+
dfold <- data[data$age == "50+",]
t1 <- tapply(dfold$int, list(dfold$adjs, dfold$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
# apply collex function (ints >= 5: colnames(t3)[which(colSums(t3) >= 5)]
highly  <- collex(data = t3, cv1 = 13)
pretty  <- collex(data = t3, cv1 =  20)
really  <- collex(data = t3, cv1 = 22)
so  <- collex(data = t3, cv1 = 26)
totally  <- collex(data = t3, cv1 = 29)
very  <- collex(data = t3, cv1 = 30)
well  <- collex(data = t3, cv1 = 31)
# extract informaltion
highly <- matrix(unlist(highly),ncol=4,byrow=TRUE)
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
totally <- matrix(unlist(totally),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
well <- matrix(unlist(well),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(highly, pretty, really, so, totally, very, well)
# convert into data frame
collexdfold <- as.data.frame(collextab)
# add colnames
colnames(collexdfold) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdfold)
collexdfold$corr05 <- rep(corr05, nrow(collexdfold))
corr01 <- 0.01/nrow(collexdf)
collexdfold$corr01 <- rep(corr01, nrow(collexdfold))
corr001 <- 0.001/nrow(collexdf)
collexdfold$corr001 <- rep(corr001, nrow(collexdfold))
# calculate corrected significance status
collexdfold$sig <- as.vector(unlist(sapply(collexdfold$p, function(x){
  x <- ifelse(x <= corr001, "p<.001",
    ifelse(x <= corr01, "p<.01",
    ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdfold <- collexdfold[collexdfold$p <= .05, ]
corrsigcollexdfold <- collexdfold[collexdfold$sig != "n.s.", ]
# inspect results
head(sigcollexdfold)

corrsigcollexdfold

###############################################################
chdf <- data[data$pint != "0",]
tbadj <- table(chdf$adjs)
tbadj <- tbadj[order(tbadj, decreasing = T)]
tbadj <- tbadj[which(tbadj >= 17)]
freqadj <- names(tbadj)
dffadj <- chdf[chdf$adjs %in% freqadj,]
dffadj <- dffadj[, c(6, 11, 12)]
tst5 <- table(data$pint)
infreqint <- names(tst5[which(tst5 <160)])
dffadj$pint <- as.vector(unlist(sapply(dffadj$pint, function(x){
  x <- ifelse(x %in% infreqint, "other", x) } )))
dffadj <- dffadj[dffadj$pint != "0",]
# tabulate by age
chdf <- ftable(dffadj$adjs, dffadj$pint, dffadj$age)
chdf

###############################################################
# test number of types used with int types
pintadjtb01 <- table(data$pint, data$adjs)
pintadjtb02 <- t(apply(pintadjtb01, 1, function(x){
  x <- ifelse(x > 0, 1, 0) } ))
adjtypes <- rowSums(pintadjtb02)
pinttokens <- table(data$pint)
adjpinttokensdf <- data.frame(names(adjtypes), as.numeric(adjtypes), as.numeric(pinttokens), as.numeric(adjtypes/pinttokens))
colnames(adjpinttokensdf) <- c("Intensifier", "AdjecticeTypes", "IntensifierTokens", "Ratio")
adjpinttokensdf <- adjpinttokensdf[order(adjpinttokensdf$AdjecticeTypes, decreasing = T), ]
adjpinttokensdf02 <- adjpinttokensdf[which(adjpinttokensdf$IntensifierTokens >=5), ]
adjpinttokensdf02$Ratio2 <- 1-adjpinttokensdf02$Ratio
adjpinttokensdf02 <- adjpinttokensdf02[2:nrow(adjpinttokensdf02),]
# plot ratio of intensifiertokens against intensified adjective types
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/RIntsAdjRev.png", width = 1520, height = 580) # save plot
plot(adjpinttokensdf02$Ratio2, type = "n", axes = F, ylim = c(0, 1), xlab = "",
  ylab = "Ratio (Adjective Types against Intensifier Tokens)", lwd = 2, pch = 20,
  cex.axis = 1.5, cex = 1.5, cex.lab=1.5)
axis(1, 1:nrow(adjpinttokensdf02), adjpinttokensdf02$Intensifier, cex.axis = 1.5, labels = F)
text(1:nrow(adjpinttokensdf02), par("usr")[3] - 0.075, labels = adjpinttokensdf02$Intensifier,
  cex = 1.5, srt = 45, pos = 1, xpd = TRUE)
axis(2, seq(0,1,.2), seq(0,1,.2), cex.axis=1.5, las = 2)
axis(3, c(2,18), c("high frequency", "low frequency"), cex.axis = 1.5 )
lines(lowess(adjpinttokensdf02$Ratio2), col = "blue", lwd = 2, lty= 2)
# add grid and box
grid()
box()
# end plot
dev.off()
###############################################################
# test number of types used with int types
pintadjtb01 <- table(data$pint, data$adjs)
pintadjtb02 <- t(apply(pintadjtb01, 1, function(x){
  x <- ifelse(x > 0, 1, 0) } ))
adjtypes <- rowSums(pintadjtb02)
pinttokens <- table(data$pint)
adjpinttokensdf <- data.frame(names(adjtypes), as.numeric(adjtypes), as.numeric(pinttokens), as.numeric(adjtypes/pinttokens))
colnames(adjpinttokensdf) <- c("Intensifier", "AdjecticeTypes", "IntensifierTokens", "Ratio")
adjpinttokensdf <- adjpinttokensdf[order(adjpinttokensdf$AdjecticeTypes, decreasing = T), ]
adjpinttokensdf02 <- adjpinttokensdf[which(adjpinttokensdf$IntensifierTokens >=5), ]
adjpinttokensdf02$Ratio2 <- 1-adjpinttokensdf02$Ratio
adjpinttokensdf02 <- adjpinttokensdf02[2:nrow(adjpinttokensdf02),]
# plot ratio of intensifiertokens against intensified adjective types
png("C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE\\images/RIntsAdj.png", width = 1520, height = 580) # save plot
plot(adjpinttokensdf02$Ratio, axes = F, ylim = c(0, 1), xlab = "",
  ylab = "Ratio (Adjective Types against Intensifier Tokens)", lwd = 2, pch = 20,
  cex.axis = 1.5, cex = 1.5, cex.lab=1.5)
axis(1, 1:nrow(adjpinttokensdf02), adjpinttokensdf02$Intensifier, cex.axis = 1.5, labels = F)
text(1:nrow(adjpinttokensdf02), par("usr")[3] - 0.075, labels = adjpinttokensdf02$Intensifier,
  cex = 1.5, srt = 45, pos = 1, xpd = TRUE)
axis(2, seq(0,1,.2), seq(0,1,.2), cex.axis=1.5, las = 2)
axis(3, c(2,18), c("high frequency", "low frequency"), cex.axis = 1.5 )
lines(adjpinttokensdf02$Ratio[1:length(adjpinttokensdf02$Ratio)], lty = 2, lwd = 2, col = "red")
lines(lowess(adjpinttokensdf02$Ratio), col = "grey", lwd = 2, lty= 2)
lines(lowess(adjpinttokensdf02$Ratio2), col = "blue", lwd = 2, lty= 2)
# add grid and box
grid()
box()
# end plot
dev.off()
###############################################################
# extract collocations of good
#s1a <- data[data$txtyp == "PrivateDialogue", ]
int <- data[data$int == 1, ]
adjtb <- table(int$adjs)
adjtb <- adjtb[order(adjtb, decreasing = T)]
adjtb

good <- int[int$adjs == "good",]
nice <- int[int$adjs == "nice",]
hard <- int[int$adjs == "hard",]

goodtb <- ftable(good$pint, good$age)
nicetb <- ftable(nice$pint, nice$age)
hardtb <- ftable(hard$pint, hard$age)

goodtb

nicetb

hardtb


###############################################################
###############################################################
###############################################################
### --- statz
###############################################################
###############################################################
###############################################################
### ---             Model Building
################################################################
s1a <- data[data$txtyp == "PrivateDialogue", ]
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
#options(contrasts  =c("contr.sum", "contr.poly"))

data.dist <- datadist(s1a)
options(datadist = "data.dist")
# generate initial minimal regression model
m0.glm = glm(really ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(really ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# create model with a random intercept for flid
m0.glmer = glmer(really ~ 1 + (1|flid), data = s1a, family = binomial)
m0.lmer <- lmer(really ~ 1 + (1|flid), data = s1a, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm # the aic of the glmer object is smaller: random intercepts are justified
# manual model fitting
m0.glmer = glmer(really ~ 1 + (1|flid), data = s1a, family = binomial,
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
m1.glmer = update(m0.glmer, .~. + age)
anova(m0.glmer, m1.glmer, test = "Chi")# SIG!

m2.glmer = update(m1.glmer, .~. + fun)
anova(m1.glmer, m2.glmer, test = "Chi")# SIG!

m3.glmer = update(m2.glmer, .~. + grad)
anova(m2.glmer, m3.glmer, test = "Chi")# SIG!

m4.glmer = update(m3.glmer, .~. + emo)
anova(m3.glmer, m4.glmer, test = "Chi")# SIG!

m5.glmer = update(m4.glmer, .~. + sex)
anova(m4.glmer, m5.glmer, test = "Chi")# SIG!

# create final minimal adequate model and rename it
mlr.glmer <- glmer(really ~ age + fun + grad + emo + sex + (1 | flid), family = binomial, data = s1a, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_really <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, s1a$really) #
# save results to disc
write.table(meblrm_really, "C:\\03-MyProjects\\07IntensifiersNZE\\Beamer/meblrm_really.txt", sep="\t")


################################################################
# VIFs unacceptable in models below!

m6.glmer = update(m5.glmer, .~. + ethnicity)
anova(m5.glmer, m6.glmer, test = "Chi")# not sig.

m7.glmer = update(m5.glmer, .~. + sem)
anova(m5.glmer, m7.glmer, test = "Chi")# not sig.

m8.glmer = update(m5.glmer, .~. + occupation)
anova(m5.glmer, m8.glmer, test = "Chi")# not sig.

m9.glmer = update(m5.glmer, .~. + age:fun)
anova(m5.glmer, m9.glmer, test = "Chi")# not sig.

m10.glmer = update(m5.glmer, .~. + age:grad)# large eigenvalue ratio
m11.glmer = update(m5.glmer, .~. + age:emo)
anova(m5.glmer, m11.glmer, test = "Chi")# not sig.

m12.glmer = update(m5.glmer, .~. + age:sex)
anova(m5.glmer, m12.glmer, test = "Chi")# not sig.

m13a.glmer = update(m5.glmer, .~. + ethnicity)
m13.glmer = update(m13a.glmer, .~. + age:ethnicity)
anova(m5.glmer, m13.glmer, test = "Chi")# not sig.

m14a.glmer = update(m5.glmer, .~. + sem)
m14.glmer = update(m14a.glmer, .~. + age:sem)# failure to coverge
m15a.glmer = update(m5.glmer, .~. + occupation)
m15.glmer = update(m15a.glmer, .~. + age:occupation)# failure to coverge
m16.glmer = update(m5.glmer, .~. + fun:grad)
anova(m5.glmer, m16.glmer, test = "Chi")# SIG!

m17.glmer = update(m16.glmer, .~. + fun:emo)
anova(m16.glmer, m17.glmer, test = "Chi")# SIG!

m18.glmer = update(m17.glmer, .~. + fun:sex)
anova(m17.glmer, m18.glmer, test = "Chi")# not sig.

m19a.glmer = update(m17.glmer, .~. + ethnicity)
m19.glmer = update(m19a.glmer, .~. + fun:ethnicity)
anova(m17.glmer, m19.glmer, test = "Chi")# not sig.

m20a.glmer = update(m17.glmer, .~. + sem)
m20.glmer = update(m20a.glmer, .~. + fun:sem)# large eigenvalue ratio
m21a.glmer = update(m17.glmer, .~. + occupation)
m21.glmer = update(m21a.glmer, .~. + fun:occupation)
anova(m17.glmer, m21.glmer, test = "Chi")# not sig.

m22.glmer = update(m17.glmer, .~. + grad:emo)
anova(m17.glmer, m22.glmer, test = "Chi")# not sig.

m23.glmer = update(m17.glmer, .~. + grad:sex)
anova(m17.glmer, m23.glmer, test = "Chi")# not sig.

m24a.glmer = update(m17.glmer, .~. + ethnicity)
m24.glmer = update(m24a.glmer, .~. + grad:ethnicity)
anova(m17.glmer, m24.glmer, test = "Chi")# SIG!

m25a.glmer = update(m24.glmer, .~. + sem)
m25.glmer = update(m25a.glmer, .~. + grad:sem)# failure to coverge
m26a.glmer = update(m24.glmer, .~. + occupation)
m26.glmer = update(m26a.glmer, .~. + grad:occupation)
anova(m24.glmer, m26.glmer, test = "Chi")# not sig.

m27.glmer = update(m24.glmer, .~. + emo:sex)
anova(m24.glmer, m27.glmer, test = "Chi")# not sig.

m28a.glmer = update(m24.glmer, .~. + ethnicity)
m28.glmer = update(m28a.glmer, .~. + emo:ethnicity)
anova(m24.glmer, m28.glmer, test = "Chi")# not sig.

m29a.glmer = update(m24.glmer, .~. + sem)
m29.glmer = update(m29a.glmer, .~. + emo:sem)# failure to coverge
m30a.glmer = update(m24.glmer, .~. + occupation)
m30.glmer = update(m30a.glmer, .~. + emo:occupation)
anova(m24.glmer, m30.glmer, test = "Chi")# not sig.

# m24.glmer is the final minimal adequate model!
# formula: really ~ age + fun + grad + emo + sex + ethnicity + fun:grad + fun:emo + grad:ethnicity + (1 | flid)

# compare models
mdlcmp <- anova(m0.glmer, m1.glmer, m2.glmer, m3.glmer, m4.glmer, m5.glmer, m6.glmer,
  m7.glmer, m8.glmer, m9.glmer, m10.glmer, m11.glmer, m12.glmer, m13.glmer,
  m14.glmer, m15.glmer, m16.glmer, m17.glmer, m18.glmer, m19.glmer,
  m20.glmer, m21.glmer, m22.glmer, m23.glmer, m24.glmer, m25.glmer,
  m26.glmer, m27.glmer, m28.glmer, m29.glmer, m30.glmer, test = "Chi")
# create table with AIC values







###########################################################################
###                         REALLY
###########################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(really ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(really ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * occupation * ethnicity + priming, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(really ~ fun + age + sex + priming,  family = binomial, data = s1a)
lr.lrm <- lrm(really ~ fun + age + sex + priming,  data = s1a, x = T, y = T, linear.predictors = T)
# determine penalty
pentrace(lr.lrm, seq(0, 0.8, by = 0.05))

# the pentrace function proposes a penaty of .8 but the values are so similar
# that a penalty is unneccessary

# add noint column
s1a$noreally <- as.vector(unlist(sapply(s1a$really, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$really * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$noreally * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$really) + sum(s1a$noreally)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz <- function(x) {
  ifelse((length(x$fitted) < (104 + ncol(summary(x)$coefficients)-1)) == TRUE,
    return(
      paste("Sample too small: please increase your sample by ",
      104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
      " data points", collapse = "")),
    return("Sample size sufficient")) }
smplesz(lr.glm)

# create summary table
blrm.really<- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.really, "C:\\03-MyProjects\\07IntensifiersNZE\\IntNZEISLE/blrm_really.txt", sep="\t")



###############################################################
###############################################################
###############################################################
### ---                   THE END
###############################################################
###############################################################
###############################################################


