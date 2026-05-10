###########################################################
### --- SENIMENT ANALYSIS with R
### --- comparign sentiment scores of males and females in irish english
###########################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# load the necessary packages
# required pakacges
#library(sentiment)
library(syuzhet)
library(stringr)
library(plyr)
library(tm)
library(ggplot2)
library(mlogit)
library(rms)
library(effects)
library(Hmisc)
library(RLRsim)
library(lme4)
library(cfa)
library(lattice)
library(MASS)
#library(nlme)
source("C:\\R/PseudoR2lmerBinomial.R")
source("C:\\R/mlr.summary.R")
source("C:\\R/blr.summary.R")
source("C:\\R/meblr.summary.R")
source("C:\\R/ModelFittingSummarySWSU.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSD.R") # for Mixed Effects Model fitting (step-wise step-down): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSULogReg.R") # for Fixed Effects Model fitting: Binary Logistic Models
###########################################################
# load data
###########################################################
# Setting options
options(stringsAsFactors = F)
# Specify pathnames of the corpra
corpus.ire <- "C:\\Corpora\\original\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
# Define input pathname of raw biodata
bio.ire.1 <- "C:\\Corpora\\original\\ICE\\02-SpeakerinformationFromCompilers\\ICE Ireland/Census spoken N complete HANDBOOK.txt" # (southern data)
bio.ire.2 <- "C:\\Corpora\\original\\ICE\\02-SpeakerinformationFromCompilers\\ICE Ireland/Census spoken S complete HANDBOOK.txt" # (northern data)
# Define image directory
imageDirectory<-"C:\\03-MyProjects\\04SentimentIrE\\Poster\\images"
###########################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.ire, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
#corpus.files <- corpus.files[1:5]
#x <- corpus.files[1]
###########################################################
# load and process corpus
corpus.tmp1 <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "\t", quiet = T, quote = "", skipNul = T)
  x <- paste(x, collapse = " ")
  x <- enc2utf8(x)
  x <- gsub(" {2,}", " ", x)
  x <- str_replace_all(x, fixed("\n"), " ")
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  splitpattern1 = "</I>"
  x <- strsplit(x, splitpattern1)
  splitpattern2 = "<I> "
  x <- sapply(x, function(y){
    y <- strsplit(y, splitpattern2)
    y <- unlist(y)
    })
  x <- as.vector(unlist(x))
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  corpus <- x[2:length(x)]
  corpus <- corpus[corpus != ""]
  corpus <- as.vector(unlist(corpus))
  utts <- as.vector(unlist(sapply(corpus, function(y){
    y <- strsplit(gsub("(<[A-Z]{1,1}[0-9]{1,1}[A-Z]{1,1}-)", "~\\1", y), "~" )
    })))
  file <- as.vector(unlist(sapply(utts, function(x){
    x <- sub(".*([A-Z]{1,1}[0-9]{1,1}[A-Z]{1,1}-[0-9]{3,3}).*", "\\1", x) } )))
  txt <- as.vector(unlist(sapply(utts, function(x){
    x <- sub(".*>", "", x)
    x <- gsub(" {2,}", " ", x)
    x <- str_trim(x, side = "both") } )))
  spk <- as.vector(unlist(sapply(utts, function(x){
    x <- gsub(".*\\$(.{0,3})>.*", "\\1", x) } )))
  uttcl <- as.vector(unlist(sapply(txt, function(x){
    x <- str_replace_all(x, "(<X>.*</X>)", "")
    x <- str_replace_all(x, "(<&> [a-z]{1,} {0,1}[a-z]{0,} {0,1}</&>)", "")
    x <- str_replace_all(x, "(<.{1,3}>)", "")
    x <- gsub("[-|\\$|&|,|<|>|/]", "", x)
    x <- gsub("<.{6,15}>", " ", x)
    x <- tolower(x)
    x <- gsub(" {2,}", " ", x)
    x <- str_trim(x, side = "both") } )))
  df <- data.frame(file, spk, txt, uttcl)
  colnames(df) <- c("file", "spk", "su", "sucl")
  df <- df[df$file != "", ]
  df <- df[df$sucl != "", ]
  return(df)
  })
# collapse data frames
corpus <- do.call("rbind", corpus.tmp1)
# clean spk column
corpus$spkcl <- as.vector(unlist(sapply(corpus$spk, function(x){
  x <- gsub(".*([A-Z]).*", "\\1", x) } )))
###########################################################
### --- STEP
###########################################################
# calculate length of sucl
lsu <- as.vector(unlist(sapply(corpus$sucl, function(x){
  l <- length(unlist(strsplit(x, " "))) } )))
# extract words
words <- as.vector(unlist(sapply(corpus$sucl, function(x){
  x <- unlist(strsplit(x, " ")) } )))
# create new table
file <- rep(corpus$file, lsu)
spk <- rep(corpus$spk, lsu)
# set up data frame
corpw <- data.frame(file, spk, words)
colnames(corpw) <- c("text.id", "spk.ref", "words")
###########################################################
# WARNING only S1A files
corpw$txtyp <- gsub("-.*", "", corpw$text.id)
#corpw <- corpw[corpw$texttyp == "S1A",]
# Inspect the resulting file
#str(corpw)

#head(corpw)

###########################################################
### --- STEP
###########################################################
# load biodata
biodata.tmp1 <- read.delim(bio.ire.1, header = T, sep = "\t")
# Inspect data
#head(biodata.tmp1)
# Load data
biodata.tmp2 <- read.delim(bio.ire.2, header = T, sep = "\t")
# Inspect data
#head(biodata.tmp2)
# Parallelize column names
colnames(biodata.tmp1)[14] <- "1st.lg"
colnames(biodata.tmp2) <- colnames(biodata.tmp1)
# Combine data sets
biodata.ice.tmp1 <- rbind(biodata.tmp1, biodata.tmp2)
column.names <- colnames(biodata.ice.tmp1)
biodata.ice.tmp1 <- apply(biodata.ice.tmp1, 1, str_trim)
biodata.ice.tmp1 <- matrix(biodata.ice.tmp1, ncol = 15, byrow = T)
colnames(biodata.ice.tmp1) <- column.names
colnames(biodata.ice.tmp1)[1] <- "zone"
# Inspect data
#head(biodata.ice.tmp1)
# Delete superfluous lines
biodata.ice.tmp2 <- rbind(biodata.ice.tmp1[c(1:656, 658:length(biodata.ice.tmp1[ , 1])), ])
biodata.ice.tmp3 <- rbind(biodata.ice.tmp2[c(1:(length(biodata.ice.tmp2[ , 1])-1)), ])
biodata.ice.tmp4 <- biodata.ice.tmp3
biodata.ice.tmp4 <- as.matrix(t(apply(biodata.ice.tmp3, 1, FUN = function(x) {
  ifelse(x == "nag", "NA", x)  }  )), ncol = length(biodata.ice.tmp3[1, ]))
# Repair inaccurate cells
# repair 1 (to check: which(biodata.ice.tmp4[, 8] == "LS"))
biodata.ice.tmp4[1133, 8] <- "NA"
# repair 2 (to check: which(biodata.ice.tmp4[, 11] == "D"))
biodata.ice.tmp4[942, 11] <- "NA"
# Inspect data
#head(biodata.ice.tmp4)

###########################################################
# Replace abbreviations with actual values
# convert table into a data frame
biodata.ice.tmp4<- as.data.frame(biodata.ice.tmp4)
# zone
biodata.ice.tmp4$zone <- as.vector(unlist(sapply(biodata.ice.tmp4$zone, function(x) {
  ifelse(x == "N", "northern ireland",
  ifelse(x == "S", "republic of ireland",
  ifelse(x == "M", "mixed between ni and roi",
  ifelse(x == "X", "non-corpus speaker",
  ifelse(x == "NA", NA, x
  )))))  } )))
# date of recording
biodata.ice.tmp4$date <- as.vector(unlist(sapply(biodata.ice.tmp4$date, function(x) {
  ifelse(x == "A", "1990-1994",
  ifelse(x == "B", "1995-2001",
  ifelse(x == "C", "2002-2005",
  ifelse(x == "NA", NA, x
  ))))  } )))
# gender
biodata.ice.tmp4$sex <- as.vector(unlist(sapply(biodata.ice.tmp4$sex, function(x) {
  ifelse(x == "F", "female",
  ifelse(x == "M", "male",
  ifelse(x == "NA", NA,
  ifelse(x == "nag", NA, x
  ))))  } )))
# age
biodata.ice.tmp4$age <- as.vector(unlist(sapply(biodata.ice.tmp4$age, function(x) {
  ifelse(x == "0", "0-18",
  ifelse(x == "1", "19-25",
  ifelse(x == "2", "26-33",
  ifelse(x == "3", "34-41",
  ifelse(x == "4", "42-49",
  ifelse(x == "5", "50+",
  ifelse(x == "", NA,
  ifelse(x == "NA", NA, x
  ))))))))   }  )))
# prov
biodata.ice.tmp4$prov <- as.vector(unlist(sapply(biodata.ice.tmp4$prov, function(x) {
  ifelse(x == "AN", "antrim",
  ifelse(x == "AR", "armagh",
  ifelse(x == "B", "belfast",
  ifelse(x == "C", "cork (city)",
  ifelse(x == "CE", "clare",
  ifelse(x == "D", "dublin (city)",
  ifelse(x == "DL", "donegal",
  ifelse(x == "DW", "down",
  ifelse(x == "DY", "(london)derry",
  ifelse(x == "EN", "england",
  ifelse(x == "FG", "fermanagh",
  ifelse(x == "G", "galway (city)",
  ifelse(x == "GA", "galway (county)",
  ifelse(x == "KY", "kerry",
  ifelse(x == "LK", "limerick",
  ifelse(x == "LS", "laois",
  ifelse(x == "MH", "meath",
  ifelse(x == "MO", "mayo",
  ifelse(x == "RN", "roscommon",
  ifelse(x == "TP", "tipperary",
  ifelse(x == "TY", "tyrone",
  ifelse(x == "WX", "wexford",
  ifelse(x == "NA", NA, NA
  )))))))))))))))))))))))   } )))
# reside
biodata.ice.tmp4$reside <- as.vector(unlist(sapply(biodata.ice.tmp4$reside, function(x) {
  ifelse(x == "AN", "antrim",
  ifelse(x == "AR", "armagh",
  ifelse(x == "B", "belfast",
  ifelse(x == "C", "cork (city)",
  ifelse(x == "CC", "cork (county)",
  ifelse(x == "D", "dublin (city)",
  ifelse(x == "DU", "dublin (county)",
  ifelse(x == "DW", "down",
  ifelse(x == "DY", "(london)derry",
  ifelse(x == "EN", "england",
  ifelse(x == "FG", "fermanagh",
  ifelse(x == "G", "galway (city)",
  ifelse(x == "KE", "kildare",
  ifelse(x == "KY", "kerry",
  ifelse(x == "LK", "limerick",
  ifelse(x == "MH", "meath",
  ifelse(x == "MO", "mayo",
  ifelse(x == "SO", "sligo",
  ifelse(x == "TP", "tipperary",
  ifelse(x == "TY", "tyrone",
  ifelse(x == "WW", "wicklow",
  ifelse(x == "NA", NA, NA
  ))))))))))))))))))))))   } )))
# education level
college <- c("FID", "PGQ", "PHD", "STE",  "TEQ")
nocollege <- c("PRI", "SES", "SSE")
biodata.ice.tmp4$ed.lev <- as.vector(unlist(sapply(biodata.ice.tmp4$ed.lev, function(x) {
  ifelse(x %in% college, "college",
    ifelse(x %in% nocollege, "nocollege", NA))   } )))
# religion
biodata.ice.tmp4$relig <- as.vector(unlist(sapply(biodata.ice.tmp4$relig, function(x) {
  ifelse(x == "C", "catholic",
  ifelse(x == "P", "protestant", "other"))   }  )))
# mother tongue
colnames(biodata.ice.tmp4)[14] <- "first.lg"
biodata.ice.tmp4$first.lg <- as.vector(unlist(sapply(biodata.ice.tmp4$first.lg, function(x) {
  ifelse(x == "ENG", "english",
  ifelse(x == "GLE", "irish gaelic", "other"))   }  )))
# other.languages
biodata.ice.tmp4$other.lgs <- as.vector(unlist(sapply(biodata.ice.tmp4$other.lgs, function(x) {
  ifelse(x == "FRA", "french",
  ifelse(x == "FRA GLE", "french & irish gaelic",
  ifelse(x == "FRA ITA", "french & italian",
  ifelse(x == "FRA SPA", "french & spanish",
  ifelse(x == "GLE", "irish gaelic",
  ifelse(x == "GLE FRA", "french & irish gaelic",
  ifelse(x == "GLE FRA DEU", "french & irish gaelic & german",
  ifelse(x == "GLE FRA SPA", "french & irish gaelic & spanish",
  ifelse(x == "GLE LAT", "french & latin",
  ifelse(x == "LAT ELL", "french & greek",
  ifelse(x == "none", NA,
  ifelse(x == "SPA", "spanish",
  ifelse(x == "THA FRA SPA", "thai & french & spanish",
  ifelse(x == "", NA,
  ifelse(x == "NA", NA, NA
  )))))))))))))))   }  )))
# Convert to lower case
biodata.ice.tmp4$occupation <- tolower(biodata.ice.tmp4$occupation)
biodata.ice.tmp4$title <- tolower(biodata.ice.tmp4$title)
# Add id to enable restoring original order
biodata.ice.tmp5 <- data.frame(1:nrow(biodata.ice.tmp4), biodata.ice.tmp4)
colnames(biodata.ice.tmp5) <- c("orig.id", colnames(biodata.ice.tmp4))
# order data
biodata.ice.tmp6 <- biodata.ice.tmp5[order(biodata.ice.tmp5[, 3],biodata.ice.tmp5[, 7]), ]
# Add id to ease merging the data sets
biodata.ice.tmp7 <- cbind(1: length(biodata.ice.tmp6[, 1]), biodata.ice.tmp6)
# Add column names
colnames(biodata.ice.tmp7) <- c("id", colnames(biodata.ice.tmp6))
# convert data set to a data frame
biodata <- as.data.frame(biodata.ice.tmp7)
# Inspect data
#head(biodata)

###########################################################
### --- STEP
###########################################################
# merge the two data sets
iceire <- join(corpw, biodata, by = c("text.id", "spk.ref"), type = "left")
# inspect data
head(iceire)

###########################################################
###########################################################
###########################################################
# clean text
iceire$words <- removeWords(iceire$words, stopwords('english'))
# remove empty rows
iceire$remove <- sapply(iceire$words, function(x){
  x <- ifelse(x == "", "remove", x) })
nrow(iceire)
iceire <- iceire[!iceire$remove == "remove", ]
nrow(iceire)
###########################################################
# Perform Sentiment Analysis
# classify emotion
class_emo <- get_nrc_sentiment(iceire$words)
# add gender, age, ed.lev, and occupation
emoire <- data.frame(iceire, class_emo)

# recode occupation
# Academic, clerical, managerial professions
acmp <- c("accountant", "administrator", "apprentice barrister", "artist typist",
  "author", "author journalist", "bank clerk", "bank manager", "bank official",
  "bank official lecturer", "banker", "barrister", "bbc production assistant",
  "bifhe tutor", "biochemist", "bishop", "broadcast journalist", "broadcast reporter",
  "broadcaster", "broadcaster cooking school owner", "broadcaster director",
  "broadcaster journalist", "broadcaster journalist writer", "broadcaster producer",
  "building society executive", "businesswoman", "catholic priest", "ceo",
  "chief auditor of ehsesb", "chief executive", "civil servant", "classroom assistant",
  "clerical officer", "college administrator", "college president",
  "commission chairman", "company director", "company director politician",
  "company director sports com", "company representative", "computer technician",
  "consultant", "court clerk", "credit union official", "curate",
  "department store manager", "deputy editor", "designer", "diplomat politician",
  "doctor", "domestic economist", "economist politician", "employment coordinator",
  "engineer", "environmental health officer", "estate agent",
  "executive director unicef irl", "financial advisor", "financier author",
  "football manager", "former gaa hurler student", "former town clerk",
  "fun park owner", "general practitioner", "graphic designer",
  "health employers official", "high court judge", "hotelier sports commentator",
  "investment analyst", "it analyst", "journalist", "journalist  broadcaster",
  "journalist author", "journalist broadcaster", "journalist community worker",
  "journalist producer", "journalist writer", "judge", "lecturer", "lecturer musician",
  "lecturer security analyst", "legal secretary", "letting agent",
  "lord mayor politician", "management consultant", "manager", "manager social worker",
  "marketing", "marketing assistant", "marketing consultant", "marketing manager",
  "medical consultant", "medical doctor", "medical journal editor", "methodist minister",
  "minister of religion", "musician", "musician singer", "news reporter", "newscaster",
  "newsreader", "newsreporter", "pastor community leader", "pg student", "physicist",
  "physiotherapist student", "police superintendent", "politician",
  "politician academic", "politician accountant", "politician architect",
  "politician barrister", "politician barrister accountant",
  "politician business executive", "politician company director",
  "politician economist", "politician executive", "politician historian",
  "politician lecturer consultant", "politician minister of religion",
  "politician psychiatrist", "politician publican", "politician secretary",
  "politician solicitor", "politician teacher", "politician trade union official",
  "practice manager", "presbyterian minister", "presbyterian moderator", "priest",
  "priest journalist", "priest lecturer", "priest teacher", "professor",
  "professor composer", "programme manager trocaire", "property developer politician",
  "public relations officer", "publican", "radio presenter",
  "radio producer writer teacher", "registrar", "rep for chemical company",
  "reporter", "research assistant", "researcher", "retired accountant",
  "retired bishop", "retired broadcaster writer", "retired civil servant",
  "retired general secretary gra", "retired presbyterian minister",
  "retired social worker", "sales manager", "secretary student",
  "senior counsel lecturer", "senior legal assistant", "senior systems analyst",
  "singer", "solicitor", "solicitor politician", "speech & language therapist",
  "sports commentator", "sports commentator teacher", "sports official",
  "sports reporter", "student", "student hockey player", "student union officer",
  "surgeon", "systems analyst", "teacher", "teacher politician",
  "television journalist", "television painting expert", "television presenter",
  "television producer ski instructor", "tourist board employee",
  "trade union official", "trade union representative", "trainee solicitor", "travel advisor",
  "vp of primary school", "weather reporter", "writer", "writer broadcaster")
# Skilled Manual Labour
sml <- c("athlete", "staff nurse", "waitress", "ward assistant", "social worker",
  "socialworker", "building company employee", "bus driver", "captain of fishing boat",
  "retail manageress", "shop employee", "retired social worker", "care assistant",
  "community worker", "racing commentator", "receiver", "receptionist", "farm manager",
  "farmer", "farmer politician", "politician stonemason", "fire officer", "fisherman",
  "fishwife", "fitness instructor", "footballer", "former gaa footballer",
  "furniture store employee", "former road sweeper", "horticulturist", "nanny",
  "nurse", "nurse student", "peat bog owner", "police officer")
# unclassifiable
NAN <- c("retired", "employed", "housewife", "na")
# reclassify
emoire$occupation <- as.vector(unlist(sapply(emoire$occupation, function(x) {
  x <- ifelse(x %in% acmp, "acmp",
    ifelse(x %in% sml, "sml", NA)) } )))
# recode age
emoire <- emoire[emoire$age != "0-18", ]
emoire$age <- as.vector(unlist(sapply(emoire$age, function(x){
  x <- ifelse(x == "34-41", "34-49",
    ifelse(x == "42-49", "34-49", x)) } )))
# inspect results
head(emoire)

# add text.spk column
emoire$txtspk <- as.vector(unlist(apply(emoire, 1, function(x){
  x <- paste(x[1], "$", x[2], sep = "") } )))
########################################################
### Step
########################################################
# add info on samesex vs diffsex files
sstb <- table(emoire$txtspk, emoire$sex)
fl <- gsub("\\$.*", "", rownames(sstb))
spk <- gsub(".*\\$([A-Z]{0,1}\\?{0,1}).*", "\\1", rownames(sstb))
fml <- as.vector(unlist(sapply(sstb[,1], function(x){
  x <- ifelse(x >0, 1, 0) } )))
ml <- as.vector(unlist(sapply(sstb[,2], function(x){
  x <- ifelse(x >0, 1, 0) } )))
# set up data frame
ssdf <- data.frame(rownames(sstb), fl, spk, fml, ml)
colnames(ssdf) <- c("full", "file", "spk", "female", "male")
# clean speakers
ssdf$spk <- gsub("?", "", ssdf$spk, fixed = T)
ssdf <- ssdf[ssdf$spk != "", ]
head(ssdf)

ssdf <- ssdf[rowSums(ssdf[, c(4:5)]) > 0, ]
ssdf$sex <- as.vector(unlist(sapply(ssdf[, 5], function(x){
  x <- ifelse(x == 1, "male", "female") } )))
head(ssdf)

tst <- table(ssdf$file, ssdf$sex)
intloc <- apply(tst, 1, function(x){
  x <- rowSums(tst) } )
intloc <- intloc[,1]
tst2 <- t(apply(tst, 1, function(x){
  x <- ifelse(x > 0, 1, 0) } ))
tst2 <- as.data.frame(tst2)
rwsm <- as.vector(unlist(rowSums(tst2)))
tst2$smsx <- as.vector(unlist(sapply(rwsm, function(x){
  x <- ifelse(x == 1, "samesex", "mixedsex") } )))
tst2$text.id <- rownames(tst2)
tst2$ints <- intloc
tst3 <- data.frame(tst2$smsx, tst2$text.id, tst2$ints)
colnames(tst3) <- gsub("tst2.", "", colnames(tst3))
# combine number of interlocutors and emoire data frame
emoire <- join(emoire, tst3, by = c("text.id"), type = "left")
# inspect data
head(emoire)

# add a column with cumulated sum of emotion scores
emo <- emoire[, c(21:28)]
emoire$emosum <- rowSums(emo)    
remove <- which(is.na(emoire$emosum) == F)
emoire <- emoire[remove, ]
# inspect final data set
head(emoire)

###########################################################
###########################################################
###########################################################
# Visualization of emotionality scores by age and sex
###########################################################
# multiply values by 100 to plot percentages
emoire$emosum <- emoire$emosum*100

emoire$anger <- emoire$anger*100
emoire$anticipation <- emoire$anticipation*100
emoire$disgust <- emoire$disgust*100
emoire$fear <- emoire$fear*100
emoire$joy <- emoire$joy*100
emoire$sadness <- emoire$sadness*100
emoire$surprise <- emoire$surprise*100
emoire$trust <- emoire$trust*100
# sum
###
p0a <- ggplot(emoire, aes(sex, emosum, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=90,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "Sex", y = "%") +
  scale_color_manual(values = c("red", "blue"), guide = FALSE)
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoSexAll.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p0a

###
p0b <- ggplot(emoire, aes(age, emosum, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= age)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=90,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=90,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "Sex", y = "%") +
  scale_color_manual(values = c("grey40", "grey40", "grey40", "grey40", "grey40"), guide = FALSE)
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeAll.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p0b

###
p1 <- ggplot(emoire, aes(age, emosum, colour = sex)) +
  ggtitle("All Emotives Combined") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 100)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexAll.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p1

# anger
p2 <- ggplot(emoire, aes(age, anger, colour = sex)) +
  ggtitle("Anger") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexAnger.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p2

# anticipation
p3 <- ggplot(emoire, aes(age, anticipation, colour = sex)) +
  ggtitle("Anticipation") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexAnticipation.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p3

# disgust
p4 <- ggplot(emoire, aes(age, disgust, colour = sex)) +
  ggtitle("Disgust") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexDisgust.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p4

# fear
p5 <- ggplot(emoire, aes(age, fear, colour = sex)) +
  ggtitle("Fear") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexFear.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p5

# joy
p6 <- ggplot(emoire, aes(age, joy, colour = sex)) +
  ggtitle("Joy") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexJoy.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p6

# sadness
p7 <- ggplot(emoire, aes(age, sadness, colour = sex)) +
  ggtitle("Sadness") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 5)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexSadness.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p7

# surprise
p8 <- ggplot(emoire, aes(age, surprise, colour = sex)) +
  ggtitle("Surprise") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 5)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexSurprise.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p8

# trust
p9 <- ggplot(emoire, aes(age, trust, colour = sex)) +
  ggtitle("Sadness") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain"),
        legend.position = c(0.5, 0.8)) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexTrust.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p9

# fear samesex
ssx <- emoire[emoire$smsx == "samesex", ]
p10 <- ggplot(ssx, aes(age, fear, colour = sex)) +
  ggtitle("Fear (same sex)") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexSameSexFear.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p10

# fear mixedsex
ssx <- emoire[emoire$smsx == "mixedsex", ]
p11 <- ggplot(ssx, aes(age, fear, colour = sex)) +
  ggtitle("Fear (mixed sex)") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexMixedSexFear.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p11

# fear samesex
ssx <- emoire[emoire$smsx == "samesex", ]
p12 <- ggplot(ssx, aes(age, anger, colour = sex)) +
  ggtitle("Anger (same sex)") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexSameSexAnger.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p12

# fear mixedsex
ssx <- emoire[emoire$smsx == "mixedsex", ]
p13 <- ggplot(ssx, aes(age, anger, colour = sex)) +
  ggtitle("Anger (mixed sex)") +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_set(theme_bw(base_size = 20)) +
    theme_set(theme_bw(base_size = 20)) +
    theme(axis.text.x = element_text(colour="grey20",size=18,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=18,angle=0,hjust=1,vjust=0,face="plain"),
        axis.title.x = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=18,angle=0,hjust=.5,vjust=.5,face="plain")) +
  labs(x = "", y = "%", colour = "sex") +
  scale_color_manual(values = c("red", "blue"), name  ="", breaks=c("female", "male"), labels=c("Female", "Male"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EmoAgeSexMixedSexAnger.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p13


###########################################################
###########################################################
###########################################################
emoire$emosum  <- emoire$emosum0/100
emoire$anger  <- emoire$anger/100
emoire$anticipation  <- emoire$anticipation/100
emoire$disgust  <- emoire$disgust/100
emoire$fear  <- emoire$fear/100
emoire$joy  <- emoire$joy/100
emoire$sadness  <- emoire$sadness/100
emoire$surprise  <- emoire$surprise/100
emoire$trust  <- emoire$trust/100

###########################################################
###########################################################
###########################################################
###                 STATZ
###########################################################
###########################################################
###########################################################
### ---             DATA PREPARATION
###########################################################
# rename and clean data
nrow(emoire)
mydata <- emoire[is.na(emoire$smsx) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$date) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$ints) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$age) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$sex) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$occupation) == F, ]
nrow(mydata)
mydata <- mydata[is.na(mydata$ed.lev) == F, ]
nrow(mydata)
###########################################################
### --- Model Building: PARAMETER SETTING
###########################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mydata.dist <- datadist(mydata)
options(datadist = "mydata.dist")
###########################################################
### --- Model Building: ANGER
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(anger ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(anger ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(anger ~ sex, family = binomial, data = mydata)
lr.lrm <- lrm(anger ~ sex, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$noanger <- as.vector(unlist(sapply(mydata$anger, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$anger * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$noanger * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$anger) + sum(mydata$noanger)
predict.acc <- (correct/tot)*100
# create summary table
blrm.anger <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.anger, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-anger.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(anger ~ (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(anger ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(anger ~ sex + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_anger <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$anger) #
# save results to disc
write.table(meblrm_anger, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_anger.txt", sep="\t")
###########################################################
### --- Model Building: ANTICIPATION
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(anticipation ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(anticipation ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(anticipation ~ 1, family = binomial, data = mydata)
lr.lrm <- lrm(anticipation ~ 1, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$noanticipation <- as.vector(unlist(sapply(mydata$anticipation, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$anticipation * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$noanticipation * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$anticipation) + sum(mydata$noanticipation)
predict.acc <- (correct/tot)*100
# create summary table
blrm.anticipation <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.anticipation, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-anticipation.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(anticipation ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(anticipation ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(anticipation ~ 1 + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_anticipation <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$anticipation) #
# save results to disc
write.table(meblrm_anticipation, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_anticipation.txt", sep="\t")
###########################################################
### --- Model Building: DISGUST
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(disgust ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(disgust ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(disgust ~ age + sex, family = binomial, data = mydata)
lr.lrm <- lrm(disgust ~ age + sex, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$nodisgust <- as.vector(unlist(sapply(mydata$disgust, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$disgust * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$nodisgust * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$disgust) + sum(mydata$nodisgust)
predict.acc <- (correct/tot)*100
# create summary table
blrm.disgust <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.disgust, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-disgust.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(disgust ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(disgust ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(disgust ~ age + sex + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_disgust <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$disgust) #
# save results to disc
write.table(meblrm_disgust, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_disgust.txt", sep="\t")
###########################################################
### --- Model Building: FEAR
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(fear ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(fear ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(fear ~ txtyp + sex, family = binomial, data = mydata)
lr.lrm <- lrm(fear ~ txtyp + sex, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$nofear <- as.vector(unlist(sapply(mydata$fear, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$fear * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$nofear * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$fear) + sum(mydata$nofear)
predict.acc <- (correct/tot)*100
# create summary table
blrm.fear <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.fear, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-fear.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(fear ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(fear ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(fear ~ txtyp + sex + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_fear <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$fear) #
# save results to disc
write.table(meblrm_fear, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_fear.txt", sep="\t")
###########################################################
### --- Model Building: JOY
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(joy ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(joy ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(joy ~ sex + ints + txtyp + ints:txtyp + sex:txtyp, family = binomial, data = mydata)
lr.lrm <- lrm(joy ~ sex + ints + txtyp + ints:txtyp + sex:ints, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$nojoy <- as.vector(unlist(sapply(mydata$joy, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$joy * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$nojoy * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$joy) + sum(mydata$nojoy)
predict.acc <- (correct/tot)*100
# create summary table
blrm.joy <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.joy, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-joy.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(joy ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(joy ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(joy ~ sex + ints + txtyp + ints:txtyp + sex:ints + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_joy <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$joy) #
# save results to disc
write.table(meblrm_joy, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_joy.txt", sep="\t")
###########################################################
### --- Model Building: SADNESS
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(sadness ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(sadness ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(sadness ~ sex, family = binomial, data = mydata)
lr.lrm <- lrm(sadness ~ sex, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$nosadness <- as.vector(unlist(sapply(mydata$sadness, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$sadness * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$nosadness * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$sadness) + sum(mydata$nosadness)
predict.acc <- (correct/tot)*100
# create summary table
blrm.sadness <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.sadness, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-sadness.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(sadness ~ (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(sadness ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(sadness ~ sex + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_sadness <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$sadness) #
# save results to disc
write.table(meblrm_sadness, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_sadness.txt", sep="\t")
###########################################################
### --- Model Building: SURPRISE
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(surprise ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(surprise ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(surprise ~ 1, family = binomial, data = mydata)
lr.lrm <- lrm(surprise ~ 1, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$nosurprise <- as.vector(unlist(sapply(mydata$surprise, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$surprise * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$nosurprise * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$surprise) + sum(mydata$nosurprise)
predict.acc <- (correct/tot)*100
# create summary table
blrm.surprise <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.surprise, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-surprise.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(surprise ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(surprise ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(surprise ~ 1 + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_surprise <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$surprise) #
# save results to disc
write.table(meblrm_surprise, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_surprise.txt", sep="\t")
###########################################################
### --- Model Building: TRUST
###########################################################
#                    GLM
# generate initial minimal regression model including only the intercept
m0.glm = glm(trust ~ 1, family = binomial, data = mydata) # baseline model glm
m0.lrm = lrm(trust ~ 1, data = mydata, x = T, y = T) # baseline model lrm
# model fitting
stepAIC(m0.glm, scope = list(upper = ~age * sex * txtyp * smsx * ints, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(trust ~ txtyp + smsx + age, family = binomial, data = mydata)
lr.lrm <- lrm(trust ~ txtyp + smsx + age, data = mydata, x = T, y = T, linear.predictors = T)
# add noint column
mydata$notrust <- as.vector(unlist(sapply(mydata$trust, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(mydata$trust * (predict(lr.glm, type = "response") >= 0.5)) + sum(mydata$notrust * (predict(lr.glm, type="response") < 0.5))
tot <- sum(mydata$trust) + sum(mydata$notrust)
predict.acc <- (correct/tot)*100
# create summary table
blrm.trust <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.trust, "C:\\03-MyProjects\\04SentimentIrE\\Poster/blrm-trust.txt", sep="\t")
###########################################################
#                    GLMEM
# create model with a random intercept for txtspk
m0.glmer = glmer(trust ~ 1 + (1|txtspk), data = mydata, family = binomial)
m0.lmer <- lmer(trust ~ 1 + (1|txtspk), data = mydata, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(trust ~ txtyp + smsx + age + (1|txtspk), family = binomial, data = mydata, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_trust <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydata$trust) #
# save results to disc
write.table(meblrm_trust, "C:\\03-MyProjects\\04SentimentIrE\\Poster/meblrm_trust.txt", sep="\t")
###########################################################
###                     THE END
###########################################################
