##################################################################
### --- R script "Intensifiers in Irish English"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- CONTACT
### --- martin.schweinberger.hh@gmail.com
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2015. "Intensifiers in Irish English",
### --- unpublished R script, Hamburg University.
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
library(cfa)
library(Hmisc)
library(lme4)
library(languageR)
library(ggplot2)
library(syuzhet)
source("C:\\R/ConcR_2.3.R")
source("C:\\R/POStagObject.R") # for multiple ggplot2 plots in one window
source("C:\\R/multiplot_ggplot2.R") # for multiple ggplot2 plots in one window
source("C:\\R/CurlyBraces.R") # for braces in simple plots
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
imageDirectory<-"C:\\03-MyProjects\\07IntensifiersIrE\\Article\\images"
# Specify pathnames of the corpra
corpus.ire <- "C:\\Corpora\\original\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
bio.path <- "C:\\Corpora\\original\\00-Metadata/BiodataIceIreland.txt"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.ire, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  x <- scan(x, what = "char", sep = "", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  x <- paste(x, sep = " ", collapse = " ")
  x <- strsplit(gsub("(<I>)", "~\\1", x), "~" )
  x <- unlist(x)
  x <- x[2:length(x)]
  x <- gsub("<I> ", "", x)
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
  x <- gsub("\\$.*", "", x)
  x <- gsub(".*<", "", x)
  })))
suspk <- lapply(corpus.tmp01, function(x){
  x <- strsplit(gsub("(<[A-Z][0-9][A-Z])", "~\\1", x), "~" )
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
  x <- gsub(".*\\$", "", x)
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
  x <- gsub("<&> .* </&>", " ", x)
  x <- gsub("<unclear> .* </unclear>", " ", x)
  x <- gsub("<.{0,1}/{0,1}.{0,1}[A-Z]{0,1}[a-z]{1,}>", " ", x)
  x <- gsub("<,{1,3}>", " ", x)
  x <- gsub("</{0,1}\\{[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}[0-9]{0,1}\\[{0,1}\\{{0,1}[0-9]{0,2}>", " ", x)
  x <- gsub("<\\[/[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\[[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\}[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\][0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}\\.[0-9]{0,2}>", " ", x)
  x <- gsub("</{0,1}[A-Z]{0,2}[0-9]{0,2}>", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  })))
# additional cleaning
sucl <- as.vector(unlist(sapply(sucl, function(x){
  x <- gsub("<&> laughter <&>", " ", x)
  x <- gsub("<", " ", x)
  x <- gsub("&", " ", x)
  x <- gsub(">", " ", x)
  x <- gsub("/", "", x)
  x <- gsub("[", " ", x, fixed = T)
  x <- gsub("{", " ", x, fixed = T)
#  x <- gsub("-", "", x, fixed = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  })))
# rep vectors number of times of the speech units per turn
flnn <- rep(fln, n)
sfnn <- rep(sfn, n)
spkn <- rep(spk, n)
flidn <- rep(flid, n)
df2 <- data.frame(flidn, flnn, sfnn, spkn, sunn, sucl)
#t1 <- df2$sucl[grep("\\[", df2$sucl)] #test
# save data before pos tagging
write.table(df2, "C:\\03-MyProjects\\07IntensifiersIrE\\Article/datadf.txt", sep = "\t", row.names = F, col.names = T)
# read data for pos tagging
df2 <- read.table("C:\\03-MyProjects\\07IntensifiersIrE\\Article/datadf.txt", sep = "\t", header = T)
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
pos11 <- df2$sucl[50001:nrow(df2)]
# pos tagging data
#irepos01 <- POStag(object = pos01)
#irepos01 <- as.vector(unlist(irepos01))
#writeLines(irepos01, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos01.txt", sep = "\n", useBytes = FALSE)
# chunk 2
#irepos02 <- POStag(object = pos02)
#irepos02 <- as.vector(unlist(irepos02))
#writeLines(irepos02, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos02.txt", sep = "\n", useBytes = FALSE)
# chunk 2
#irepos02 <- POStag(object = pos02)
#irepos02 <- as.vector(unlist(irepos02))
#writeLines(irepos02, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos02.txt", sep = "\n", useBytes = FALSE)
# chunk 03
#irepos03 <- POStag(object = pos03)
#irepos03 <- as.vector(unlist(irepos03))
#writeLines(irepos03, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos03.txt", sep = "\n", useBytes = FALSE)
# chunk 04
#irepos04 <- POStag(object = pos04)
#irepos04 <- as.vector(unlist(irepos04))
#writeLines(irepos04, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos04.txt", sep = "\n", useBytes = FALSE)
# chunk 05
#irepos05 <- POStag(object = pos05)
#irepos05 <- as.vector(unlist(irepos05))
#writeLines(irepos05, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos05.txt", sep = "\n", useBytes = FALSE)
# chunk 06
#irepos06 <- POStag(object = pos06)
#irepos06 <- as.vector(unlist(irepos06))
#writeLines(irepos06, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos06.txt", sep = "\n", useBytes = FALSE)
# chunk 07
#irepos07 <- POStag(object = pos07)
#irepos07 <- as.vector(unlist(irepos07))
#writeLines(irepos07, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos07.txt", sep = "\n", useBytes = FALSE)
# chunk 08
#irepos08 <- POStag(object = pos08)
#irepos08 <- as.vector(unlist(irepos08))
#writeLines(irepos08, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos08.txt", sep = "\n", useBytes = FALSE)
# chunk 09
#irepos09 <- POStag(object = pos09)
#irepos09 <- as.vector(unlist(irepos09))
#writeLines(irepos09, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos09.txt", sep = "\n", useBytes = FALSE)
# chunk 10
#irepos10 <- POStag(object = pos10)
#irepos10 <- as.vector(unlist(irepos10))
#writeLines(irepos10, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos10.txt", sep = "\n", useBytes = FALSE)
# chunk 11
#irepos11 <- POStag(object = pos11)
#irepos11 <- as.vector(unlist(irepos11))
#writeLines(irepos11, con = "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos11.txt", sep = "\n", useBytes = FALSE)
# list pos tagged elements
postag.files = c("C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos01.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos02.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos03.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos04.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos05.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos06.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos07.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos08.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos09.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos10.txt",
  "C:\\03-MyProjects\\07IntensifiersIrE\\Article/irepos11.txt")
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
strng <- as.vector(unlist(apply(dftmp, 1, function(x){
  x <- paste(x[1], x[2], x[3], x[4], x[5], collapse = " ") } )))
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
###############################################################
classpint <- data.frame(df3$pint, df3$adj)
colnames(classpint) <- gsub("df3.", "", colnames(classpint))
classpint <- classpint[order(classpint$pint),]
classpint <- classpint[which(classpint$pint != 0),]
#classpint$pint <- tolower(classpint$pint)
head(classpint)

###############################################################
# create vector with intensifiers in data
intensifiers <- c("absolutely", "Absolutely", "actually", "Actually", "apparently",
  "Apparently", "awful", "awfully", "beautiful", "Beautiful", "beautifully",
  "bloody", "certainly", "clearly", "completely", "dead", "deadly", "deeply",
  "definite", "definitely", "easily", "enormously", "entirely", "especially",
  "exact", "exactly", "exceptionally", "extra", "extraordinarily", "extremely",
  "fairly", "Fairly", "fierce", "fiercely", "fiery", "fine", "fucker", "fucking",
  "full", "fully", "fundamentally", "great", "heavily", "highly", "hugely",
  "immediately", "immensely", "impressively", "incredibly", "indeed", "instantly",
  "insufferably", "intensely", "keen", "literally", "majorly", "manifestly",
  "markedly", "marvellously", "massively", "mega", "nastily", "nasty", "obviously",
  "particularly", "perfectly", "plain", "plainly", "pretty", "Pretty", "profoundly",
  "proper", "pure", "quite", "Quite", "real", "Real", "really", "Really", "remarkably",
  "right", "Right", "seriously", "severely", "shockingly", "so", "So", "strictly",
  "strongly", "surprisingly", "terribly", "Terribly", "too", "Too", "totally",
  "Totally", "tough", "tremendously", "true", "truly", "unbelievably", "unusually",
  "utterly", "vastly", "very", "Very", "well", "Well", "wholly", "widely", "wonderfully")
rmvpint <- c("few", "many", "much", "more", "never", "not", "n't", "no")
# set non intensifiers to 0
df3$pint <- as.vector(unlist(sapply(df3$pint, function(x){
  ifelse(x %in% intensifiers == T, x, 0) } )))
# clean adjs
df3$adjpos <- gsub(".*/", "", df3$adjs)
df3 <- df3[df3$adjpos == "JJ", ]
# remove all non adj slots from data
df3$remove <- as.vector(unlist(sapply(df3$pint, function(x){
  ifelse(x %in% rmvpint == T, "remove", x) } )))
df3 <- df3[df3$remove != "remove",]
df4 <- df3[complete.cases(df3),]
df4 <- df4[, c(1:13)]

###############################################################
# clean data
###############################################################
df4$upcs <- as.vector(unlist(sapply(df4$adj, function(x){
  x <- gsub("/.*", "", x)
  x <- gsub(".*[A-Z].*", 1, x) } )))
# save data to disc for manual coding
write.table(df4, "C:\\03-MyProjects\\07IntensifiersIrE/df4.txt", row.names= F, sep = "\t")
# create vector with false adjectives
rmvadj <- c("much", "many", "Absolute", "African", "Aftershave", "Aggressive", "A-level",
  "Algerian", "all-Ireland", "all-Irish", "American", "Anglo-American",
  "Anglo-Irish", "Anti-smoking", "Arab", "Arabic", "Aramaic", "Ard", "Argentine",
  "Arsenal", "Asian", "Asiatic", "Attempted", "Austen", "Australian",
  "Austrian", "Average", "Bad", "Beautiful", "Belarusian", "Belfast", "better",
  "Belgian", "better", "Big", "Bit", "Bless", "Blind", "Bosnian", "Boston-based",
  "Botanic", "Boxing", "British", "Byzantine", "Ca", "Callan", "Canadian",
  "Canary", "Can't", "Catholic", "Certify", "Chinese", "Christian", "Conservative",
  "Contentious", "Converted", "Corporate", "CO-two", "C-plus-plus", "Critic",
  "Cromwellian", "CSPE", "CV", "Dail", "Damien", "Damn", "Dangerous", "Darby",
  "Darling", "Deep", "Democratic", "DENI", "Deputy", "DG", "Diocese", "Disagree",
  "Distant", "Double", "Dublinish", "DUCAC", "Due", "Duffy", "Dungiven", "DUP",
  "Dutch", "East", "Eastern", "Easy", "Economic", "EEC", "Egyptian", "Eighty-fifth",
  "English", "English-born", "English-speaking", "Enniscrone", "ESERP", "Ethiopian",
  "European", "Evangelical", "Extreme", "Exupery", "Fast", "Fenian", "Few", "fewer", "Fierce",
  "Fifth", "Final", "Fine", "First", "Flow", "Foolish", "Foreign", "Former", "Four-day",
  "Fourth", "Free", "French", "Freudian", "Front", "Funeral", "Further", "Gaelic",
  "Genoese", "Ger", "Geraldine", "German", "Glentoran", "Glue", "Goal", "God-given",
  "Gough", "Greek-speaking", "Grozny", "Hate", "higher", "Hot", "HP", "Hybernian", "Identical",
  "Importing", "Independent", "Indian", "Initial", "Iranian", "Iraqi", "Irish",
  "Irish-American", "Irish-language", "Irish-speaking", "Irredentist", "Irritant",
  "Islamic", "Israeli", "Italian", "Japanese", "Jealous", "Jewish", "Labour", "Laid",
  "Last", "Late", "Latin", "Leas-Cheann", "Lebanese", "less", "Liberal", "Library", "little", "Live",
  "Liverpool", "London-based", "Long-term", "Loyalist", "Lucky", "M-and-M", "Many",
  "Massive", "Matrix-style", "McBeal", "Mexican", "Military", "more", "Mum", "Mummy", "Muslim",
  "Muslim-led", "Mutual", "Nationalist", "Natural", "Nazist", "Needless", "Neolithic",
  "Newry", "Next", "Nicaraguan", "non-Belfast", "non-Labour", "non-Rent", "Northern",
  "Northern-based", "Norwegian", "Nuits-St", "Objective-C", "Official", "Old", "Olive",
  "Orient", "Oriental", "Orpheo", "OS-X", "Other", "Oul", "Out-marked", "Outside", "Overall",
  "Overdrawn", "Own", "Painful", "Pakistani", "Palestinian", "Part-time", "Pensionable",
  "Peripheral", "Permanent", "Peruvian", "Phatic", "Photocopied", "Physical",
  "Plucked", "Portuguese", "post-Darwinian", "post-Leaving", "post-Stormont",
  "Primary", "Progressive", "Protestant", "Provisional", "PRSI", "Put", "Q-Bar",
  "Relax", "Republican", "Roman", "Romantic", "Roscommon", "Rubenesque",
  "Russian", "Rwandan", "Scottish", "Scripture", "Second", "Secret", "Semi-Final",
  "Senior", "Serbian", "Serial", "Seventy-five", "Several", "Sevlakovs", "Similar",
  "Single-party", "Sixty-eight", "Social", "Sole", "Somali", "Sound", "South",
  "Southern", "Soviet", "Spanish", "Special", "Special-purpose", "Spot-on", "Stand-up",
  "State-owned", "Stile", "Strategy", "sub-Saharan", "Successive", "Such", "Suckled",
  "Swiss", "Talent", "Tallaght", "Third", "three-D", "Top", "Torrential", "Total",
  "trans-Atlantic", "Tremendous", "Trumpy", "Tyrone", "UDR", "Ugh", "Uhm", "Ukrainian",
  "Unable", "Undaunted", "Underground", "Union", "Unionist", "Unionist-controlled",
  "Uruguayan", "Used", "Utopian", "U-turned", "UVF", "Variable", "Various", "Varying",
  "VE", "Venetian", "Very", "Victim", "Victorian", "Well-Known", "Welsh", "Western",
  "Wet", "Whole", "Whose", "WordPerfect", "X-rayed")
df4$remove <- as.vector(unlist(sapply(df4$adj, function(x){
  ifelse(x %in% rmvadj == T, "remove", x)
  } )))
df4 <- df4[df4$remove != "remove",]
# create vector with false adjectives
rmvadj <- c("much", "many", "'o", "'otanga", "aaqib", "abby", "er", "egmont", "eric")
df4$remove <- as.vector(unlist(sapply(df4$adj, function(x){
  ifelse(x %in% rmvadj == T, "remove", x)
  } )))
df4 <- df4[df4$remove != "remove",]
# remove unnecessary columns
df4 <- df4[, 1:13]
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
colnames(bio)[c(4, 5, 7)] <- c("fl", "sf", "spk")
# clean data for joining
bio$fl <- gsub("-", "", bio$fl)
# join data
intdata <- join(inttb, bio, by = c("fl", "sf", "spk"), type = "left")
# remove speakers below the age of 18
intdata <- intdata[intdata$age != "0-18", ]
# restructure data frame
int <- data.frame(1:nrow(intdata), intdata$flid, intdata$sf, intdata$spk,
  intdata$sex, intdata$ed.lev, intdata$age, intdata$occupation, intdata$reside,
  intdata$prov, intdata$date, intdata$txtyp, intdata$word.count,
  intdata$pint, intdata$adj, intdata$pos, intdata$adjs,
  intdata$post1, intdata$strng, intdata$int)
# clean colnames
colnames(int) <- gsub("intdata.", "", colnames(int))
colnames(int)[c(1, 6)] <- c("id", "edlev")
# recode age
int$age <- as.vector(unlist(sapply(int$age, function(x){
  ifelse(x == "34-41", "34-49",
  ifelse(x == "42-49", "34-49", x)) } )))
# clean ed.lev
int$edlev <- as.vector(unlist(sapply(int$edlev, function(x){
  x <- gsub("doctoral degree", "college", x)
  x <- gsub("first degree", "college", x)
  x <- gsub("non-degree tertiary qualification", "college", x)
  x <- gsub("postgraduate qualification", "college", x)
  x <- gsub("some tertiary education", "college", x)
  x <- gsub("primary education", "nocollege", x)
  x <- gsub("secondary school qualification", "nocollege", x)
  x <- gsub("some secondary education", "nocollege", x)
  } )))
# recode resid
int$reside <- as.vector(unlist(sapply(int$reside, function(x){
  x <- ifelse(x =="(london)derry", "north",
    ifelse(x =="antrim", "north",
    ifelse(x =="armagh", "north",
    ifelse(x =="belfast", "north",
    ifelse(x =="cork (city)", "south",
    ifelse(x =="cork (county)", "south",
    ifelse(x =="down", "south",
    ifelse(x =="dublin (city)", "south",
    ifelse(x =="dublin (county)", "south",
    ifelse(x =="england", "NA",
    ifelse(x =="fermanagh", "NA",
    ifelse(x =="galway (city)", "south",
    ifelse(x =="kerry", "south",
    ifelse(x =="kildare", "south",
    ifelse(x =="limerick", "south",
    ifelse(x =="mayo", "south",
    ifelse(x =="meath", "south",
    ifelse(x =="sligo", "south",
    ifelse(x =="tipperary", "south",
    ifelse(x =="tyrone", "south",
    ifelse(x =="wicklow", "south", x)))))))))))))))))))))
  } )))
# recode prov
int$prov <- as.vector(unlist(sapply(int$prov, function(x){
  x <- ifelse(x =="(london)derry", "north",
    ifelse(x =="antrim", "north",
    ifelse(x =="armagh", "north",
    ifelse(x =="belfast", "north",
    ifelse(x =="claire", "south",
    ifelse(x =="cork (city)", "south",
    ifelse(x =="cork (county)", "south",
    ifelse(x =="donegal", "north",
    ifelse(x =="down", "south",
    ifelse(x =="dublin (city)", "south",
    ifelse(x =="dublin (county)", "south",
    ifelse(x =="england", "NA",
    ifelse(x =="fermanagh", "NA",
    ifelse(x =="galway (city)", "south",
    ifelse(x =="galway (county)", "south",
    ifelse(x =="kerry", "south",
    ifelse(x =="kildare", "south",
    ifelse(x =="limerick", "south",
    ifelse(x =="laois", "south",
    ifelse(x =="mayo", "south",
    ifelse(x =="meath", "south",
    ifelse(x =="sligo", "south",
    ifelse(x == "roscommon", "south",
    ifelse(x =="tipperary", "south",
    ifelse(x =="tyrone", "south",
    ifelse(x =="wexford", "south",
    ifelse(x =="wicklow", "south", x)))))))))))))))))))))))))))
  } )))
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
int$occupation <- as.vector(unlist(sapply(int$occupation, function(x) {
  x <- ifelse(x %in% acmp, "acmp",
    ifelse(x %in% sml, "sml", NA)) } )))
# add functional distinction (predicative vs attributive)
int$fun <- as.vector(unlist(sapply(int$post1, function(x){
  x <- gsub(".*/", "", x)
  x <- gsub(".*NN.*", "attributive", x)
  ifelse(x == "attributive", x, "predicative")  } )))
# rename data
data <- int
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
# clean data
data <- data[complete.cases(data), ]
# convert txtyp into an ordered factor
data$txtyp <- factor(data$txtyp, levels = c("PrivateDialogue", "PublicDialogue", "UnscriptedMonologue","ScriptedMonologue" ))
# save data to disc for manual coding
write.table(data, "C:\\03-MyProjects\\07IntensifiersIrE/dataIrE.txt", row.names=F, sep="\t")
# create list of adjectives
#names(table(data$adj))
# inspect results
head(data)
##################################################################
### --- R script "Intensifiers in Irish English"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- CONTACT
### --- martin.schweinberger.hh@gmail.com
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2015. "Intensifiers in Irish English",
### --- unpublished R script, Hamburg University.
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
library(cfa)
library(Hmisc)
library(lme4)
library(languageR)
library(ggplot2)
library(syuzhet)
source("C:\\R/ConcR_2.3.R")
source("C:\\R/POStagObject.R") # for multiple ggplot2 plots in one window
source("C:\\R/multiplot_ggplot2.R") # for multiple ggplot2 plots in one window
source("C:\\R/CurlyBraces.R") # for braces in simple plots
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
###############################################################
###############################################################
###############################################################
###                    START FROM HERE
###############################################################
###############################################################
###############################################################
# load manual coding
data <- read.table("C:\\03-MyProjects\\07IntensifiersIrE/dataIrEManualCoding.txt",
  sep = "\t", comment.char = "", quote = "", header = T)
# convert pint and adj to lower case
data$pint <- tolower(data$pint)
data$adj <- tolower(data$adj)
data$adjs <- tolower(data$adjs)
# create columns with dataensifier freqs
data$very <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "very", 1, 0) } )))
data$really <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "really", 1, 0) } )))
data$quite <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "quite", 1, 0) } )))
data$so <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "so", 1, 0) } )))
data$too <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "too", 1, 0) } )))
data$absolutely <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "absolutely", 1, 0) } )))
data$extremely <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "extremely", 1, 0) } )))
data$completely <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "completely", 1, 0) } )))
data$totally <- as.vector(unlist(sapply(data$pint, function(x){
  x <- ifelse(x == "totally", 1, 0) } )))
# add sentiment
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
# inspect results
head(data)

###############################################################
###############################################################
###############################################################
###             TABULARIZATION
###############################################################
###############################################################
###############################################################
pinttb <- table(data$pint)
pinttb <- pinttb[order(table(data$pint), decreasing = T)]
pinttb <- data.frame(names(pinttb), pinttb, round(pinttb/sum(pinttb)*100, 2),
  c("", round(pinttb[2:nrow(pinttb)]/sum(pinttb[2:nrow(pinttb)])*100, 2)))
colnames(pinttb) <- c("Intensifier", "TokenFrequency", "PercentageSlots", "PercentageIntensifiers")
pinttb <- rbind(pinttb, c("Total", sum(pinttb$TokenFrequency), "", ""))
rownames(pinttb) <- NULL
head(pinttb)

# save data to disc
write.table("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/pinttb.txt", sep = "\t", row.names = F)
###############################################################
# number of speakers by age and gender
txtypagesextb1 <- by(data$int, list(data$sex, data$age, data$txtyp), table)
# number of speakers by age and gender
agesextb1 <- data.frame(data$flid, data$txtyp, data$sex, data$age, data$int)
agesextb2 <- data.frame(data$flid, data$txtyp, data$sex, data$age)
agesextb3 <- unique(agesextb2)
colnames(agesextb3) <- gsub("data.", "", colnames(agesextb3))
# s1a
s1a <- agesextb3[agesextb3$txtyp == "PrivateDialogue", ]
s1a <- data.frame(s1a$sex, s1a$age)
colnames(s1a) <- gsub("s1a.", "", colnames(s1a))
# s1b
s1b <- agesextb3[agesextb3$txtyp == "PublicDialogue", ]
s1b <- data.frame(s1b$sex, s1b$age)
colnames(s1b) <- gsub("s1b.", "", colnames(s1b))
# s2a
s2a <- agesextb3[agesextb3$txtyp == "UnscriptedMonologue", ]
s2a <- data.frame(s2a$sex, s2a$age)
colnames(s2a) <- gsub("s2a.", "", colnames(s2a))
# s2b
s2b <- agesextb3[agesextb3$txtyp == "ScriptedMonologue", ]
s2b <- data.frame(s2b$sex, s2b$age)
colnames(s2b) <- gsub("s2b.", "", colnames(s2b))
# tabulate
agesexs1a <- table(s1a$age, s1a$sex)
agesexs1b <- table(s1b$age, s1b$sex)
agesexs2a <- table(s2a$age, s2a$sex)
agesexs2b <- table(s2b$age, s2b$sex)
# create vectors
txtypagesexvec <- as.vector(unlist(txtypagesextb1))
txtyp <- c(rep(attr(txtypagesextb1, "dimnames")[[3]][[1]], 8),
  rep(attr(txtypagesextb1, "dimnames")[[3]][[2]], 8),
  rep(attr(txtypagesextb1, "dimnames")[[3]][[3]], 8),
  rep(attr(txtypagesextb1, "dimnames")[[3]][[4]], 8))
age <- rep(c(rep(attr(txtypagesextb1, "dimnames")[[2]][[1]], 2),
  rep(attr(txtypagesextb1, "dimnames")[[2]][[2]], 2),
  rep(attr(txtypagesextb1, "dimnames")[[2]][[3]], 2),
  rep(attr(txtypagesextb1, "dimnames")[[2]][[4]], 2)), 4)
sex <- c(rep(attr(txtypagesextb1, "dimnames")[[1]], 16))
nspk <- as.vector(c(agesexs1a[1,], agesexs1a[2,], agesexs1a[3,], agesexs1a[4,],
  agesexs1b[1,], agesexs1b[2,], agesexs1b[3,], agesexs1b[4,],
  0, 0, agesexs2a[1,], agesexs2a[2,], agesexs2a[3,],
  agesexs2b[1,], agesexs2b[2,], agesexs2b[3,], agesexs2b[4,]))
int <- as.vector(unlist(txtypagesextb1))
int <- c(976, 96, 174, 8, 701, 89, 43, 5, 117, 28, 169, 18, 220, 18, 126, 7, 85,
  2, 46, 3, 16, 2, 17, 1, 270, 34, 667, 46, 138, 13, 846, 88, 0, 0, 2, 0, 9, 0,
  363, 10, 106, 1, 1035, 32, 153, 9, 901, 36, 0, 0, 0, 0, 0, 0, 37, 0, 468, 43,
  488, 35, 169, 21, 590, 56)
nint <- int[seq(2, 64, 2)]
nnoint <- int[seq(1, 63, 2)]
nslot <- nint + nnoint
pcnt <- round((nint/(nslot+nint))*100, 2)
# set up table
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
write.table("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/inttb01.txt", sep = "\t", row.names = F)
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

write.table(pinttb, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/pinttb.txt", row.names = F, sep="\t")
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
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/IntPrim.png", width = 480, height = 480) # save plot
plot(c(prim0, prim1), xlim = c(0.5, 2.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "Percent of Intensified Pre-ADJ Slots", pch = 20)
axis(1, c(1,2), c("No Priming", "Priming"))
axis(2, seq(0, 20, 5), seq(0, 20, 5), las = 2)
arrows(1, prim0, 1, prim0+sem0, angle = 90)
arrows(1, prim0, 1, prim0-sem0, angle = 90)
arrows(2, prim1, 2, prim1+sem1, angle = 90)
arrows(2, prim1, 2, prim1-sem1, angle = 90)
#grid()
box()
dev.off()
###############################################################
#                   TEXT TYPE
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
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/IntTxtyp.png", width = 900, height = 480) # save plot
plot(c(txtyps1a, txtyps1b, txtyps2a, txtyps2b), xlim = c(0.5, 4.5), axes = F, ylim = c(0, 20), xlab = "",
  ylab = "", pch = 20)
# add axes
axis(1, seq(1,4,1), c("Private Dialogue", "Public Dialogue", "Unscripted Monologue",
  "ScriptedMonologue"), cex.axis = 1.5)
axis(2, seq(0, 20, 5), seq(0, 20, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 1.5, las = 2)
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
# end plot
dev.off()
###############################################################
# plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$int == 1, ]
# extract ttrs by age
tt.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
tt1925 <- tt.age.tb[[1]]
tt1925 <- as.data.frame(tt1925)
colnames(tt1925) <- c("Intensifier", "19-25")
###
tt2633 <- tt.age.tb[[2]]
tt2633 <- as.data.frame(tt2633)
colnames(tt2633) <- c("Intensifier", "26-33")
###
tt3449 <- tt.age.tb[[3]]
tt3449 <- as.data.frame(tt3449)
colnames(tt3449) <- c("Intensifier", "34-49")
###
tt50 <- tt.age.tb[[4]]
tt50 <- as.data.frame(tt50)
colnames(tt50) <- c("Intensifier", "50+")
###
tt1639 <- join(tt1925, tt2633, by = "Intensifier", type = "full")
tt1649 <- join(tt1639, tt3449, by = "Intensifier", type = "full")
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
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/FreqAgeS1aOnlyInts.png", width = 1000, height = 580) # save plot
barplot(tt, beside = T, ylim = c(0, 60), xlab = "", ylab = "", yaxt='n', xaxt='n', cex.axis = 1.5,
  col = c("orange", "lightgreen", "lightblue", rep("lightgrey", 21)))
axis(1, seq(12.5, 87.5, 25), names(table(s1a$age)), cex.axis = 1.5)
axis(2, seq(0, 60, 10), seq(0, 60, 10), las = 2, cex.axis = 1.5)
mtext("Abs. Frequency", 2, 2.5, cex = 1.5, las = 0)
# add legend
legend("topright", legend = rownames(tt), fill = c("orange", "lightgreen", "lightblue",
  rep("lightgrey", 21)))
# add grid and box
#grid()
box()
# end plot
dev.off()
###############################################################
#                     SYNTACTIC FUNCTION
###############################################################
# plot very : age : fun (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
#s1a <- s1a[s1a$very == 1, ]
funagevery <- ftable(s1a$very, s1a$fun, s1a$age)
sumfunagevery <- colSums(funagevery)
pctattfunagevery <- funagevery[3,]/sumfunagevery*100
pctpredfunagevery <- funagevery[4,]/sumfunagevery*100
funagevery <- rbind(pctattfunagevery, pctpredfunagevery)
colnames(funagevery) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentSynFunVery.png", width = 480, height = 480) # save plot
barplot(funagevery, beside = T, ylim = c(0, 30),   xlab = "", ylab = "", cex.axis = 1.5, xaxt='n', yaxt='n', ann=FALSE)
axis(1, seq(2, 11, 3), names(table(s1a$age)), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
legend("topright", legend = c("attributive", "predicative"), fill = c("grey50", "grey95"), horiz=TRUE)
# end plot
dev.off()
###############################################################
# plot really : age : fun (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
#s1a <- s1a[s1a$really == 1, ]
funagereally <- ftable(s1a$really, s1a$fun, s1a$age)
sumfunagereally <- colSums(funagereally)
pctattfunagereally <- funagereally[3,]/sumfunagereally*100
pctpredfunagereally <- funagereally[4,]/sumfunagereally*100
funagereally <- rbind(pctattfunagereally, pctpredfunagereally)
colnames(funagereally) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentSynFunReally.png", width = 480, height = 480) # save plot
barplot(funagereally, beside = T, ylim = c(0, 30),   xlab = "", ylab = "", cex.axis = 1.5, xaxt='n', yaxt='n', ann=FALSE)
axis(1, seq(2, 11, 3), names(table(s1a$age)), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
legend("topright", legend = c("attributive", "predicative"), fill = c("grey50", "grey95"), horiz=TRUE)
# end plot
dev.off()
###############################################################
# plot so : age : fun (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
#s1a <- s1a[s1a$so == 1, ]
funageso <- ftable(s1a$so, s1a$fun, s1a$age)
sumfunageso <- colSums(funageso)
pctattfunageso <- funageso[3,]/sumfunageso*100
pctpredfunageso <- funageso[4,]/sumfunageso*100
funageso <- rbind(pctattfunageso, pctpredfunageso)
colnames(funageso) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentSynFunSo.png", width = 480, height = 480) # save plot
barplot(funageso, beside = T, ylim = c(0, 30), xlab = "", ylab = "", cex.axis = 1.5, xaxt='n', yaxt='n', ann=FALSE)
axis(1, seq(2, 11, 3), names(table(s1a$age)), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
legend("topright", legend = c("attributive", "predicative"), fill = c("grey50", "grey95"), horiz=TRUE)
# end plot
dev.off()
###############################################################
#                 AGE plus SEX
###############################################################
# line plot int : age (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
# extract ttrs by age
tt.age.tb <- (tapply(s1a$pint, list(s1a$age), table))
# extract
tt1925 <- tt.age.tb[[1]]
tt1925 <- as.data.frame(tt1925)
colnames(tt1925) <- c("Intensifier", "19-25")
###
tt2633 <- tt.age.tb[[2]]
tt2633 <- as.data.frame(tt2633)
colnames(tt2633) <- c("Intensifier", "26-33")
###
tt3449 <- tt.age.tb[[3]]
tt3449 <- as.data.frame(tt3449)
colnames(tt3449) <- c("Intensifier", "34-49")
###
tt50 <- tt.age.tb[[4]]
tt50 <- as.data.frame(tt50)
colnames(tt50) <- c("Intensifier", "50+")
###
tt1639 <- join(tt1925, tt2633, by = "Intensifier", type = "full")
tt1649 <- join(tt1639, tt3449, by = "Intensifier", type = "full")
tt <- join(tt1649, tt50, by = "Intensifier", type = "full")
tt <- as.data.frame(t(apply(tt, 1, function(x){  x <- ifelse(is.na(x) == T, 0, x) } )))
tt[,2] <- as.numeric(tt[,2])
tt[,3] <- as.numeric(tt[,3])
tt[,4] <- as.numeric(tt[,4])
tt[,5] <- as.numeric(tt[,5])
tt$cs <- rowSums(tt[, c(2:5)])
#tt <- tt[tt$cs >= 10,]
tt <- tt[order(tt[,6], decreasing = T),]
rownames(tt) <- tt$Intensifier
tt <- tt[, 2:5]
tt <- as.matrix(tt)
tt[,1] <- round(tt[,1]/sum(tt[,1])*100, 1)
tt[,2] <- round(tt[,2]/sum(tt[,2])*100, 1)
tt[,3] <- round(tt[,3]/sum(tt[,3])*100, 1)
tt[,4] <- round(tt[,4]/sum(tt[,4])*100, 1)
ttsel <- tt[which(rowSums(tt) >= 5), ]
ttsel <- ttsel[2:nrow(ttsel), ]
# plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentAgeS1aOnlyIntsLine.png",  width = 480, height = 480) # save plot
plot(ttsel[1,], type = "o", ylim = c(0, 25),
  xlab = "", ylab = "", axes = F, lwd = 2, lty = 1, pch = 1, cex.axis = 1.5)
lines(ttsel[2,], type = "o", lwd = 2,  lty = 2, pch = 2)
lines(ttsel[3,], type = "o", lwd = 2,  lty = 3, pch = 3)
lines(ttsel[4,], type = "o", lwd = 2,  lty = 4, pch = 4, col = "gray")
lines(ttsel[5,], type = "o", lwd = 2,  lty = 5, pch = 5, col = "gray")
# add axes
axis(1, 1:4, colnames(tt), cex.axis = 1.5)
axis(2, seq(0, 25, 5), seq(0, 25, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2, cex = 2, las = 2)
# add legend
legend("topleft", inset = .05, rownames(ttsel),
  horiz = F,  pch = 1:5, lty = 1:5, col = c(rep("black", 3), rep("grey", 2)))
# end plot
dev.off()
###############################################################
# plot age : s1a
s1a <- data[data$txtyp == "PrivateDialogue", ]
# extract frequencies
int.age.tb <- as.vector(unlist(by(s1a$int, list(s1a$age), table)))
int1925 <- int.age.tb[2]/(int.age.tb[1]+int.age.tb[2])*100
int2633 <- int.age.tb[4]/(int.age.tb[3]+int.age.tb[4])*100
int3449 <- int.age.tb[6]/(int.age.tb[5]+int.age.tb[6])*100
int50 <- int.age.tb[8]/(int.age.tb[7]+int.age.tb[8])*100
# plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/IntAgeS1ALine.png", width = 480, height = 480) # save plot
plot(c(int1925, int2633, int3449, int50), type = "o", xlim = c(0.5, 4.5),
  axes = F, ylim = c(0, 100), xlab = "", ylab = "", pch = 8)
lines(c(int1925, int2633, int3449, int50), lwd = 2, lty = 1)
# add axes
axis(1, seq(1,4,1), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 100, 10), seq(0, 100, 10), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2)
# end plot
dev.off()
###############################################################
# plot very : age (s1a) line
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
# calculate sex differences
very.age.sex.tb <- (tapply(s1a$very, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/VeryAgeS1aOnlyIntsLine.png", width = 480, height = 480) # save plot
plot(very.age.sex.tb[,1], type = "o", xlim = c(0.5, 4.5), axes = F, ylim = c(0, 30),
  xlab = "", ylab = "", pch = 8, lwd = 2)
points(very.age.sex.tb[,2], pch = 8)
lines(very.age.sex.tb[,2], lty = 3, lwd = 2)
# add axes
axis(1, seq(1,4,1), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2)
# add legend
legend("topleft", legend = c("female", "male"), lty= c(1, 3), horiz=TRUE)
# end plot
dev.off()
###############################################################
# plot really : age (s1a) line
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
# calculate sex differences
really.age.sex.tb <- (tapply(s1a$really, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/reallyAgeS1aOnlyIntsLine.png", width = 480, height = 480) # save plot
plot(really.age.sex.tb[,1], type = "o", xlim = c(0.5, 4.5), axes = F, ylim = c(0, 30),
  xlab = "", ylab = "", pch = 8, lwd = 2)
points(really.age.sex.tb[,2], pch = 8)
lines(really.age.sex.tb[,2], lty = 3, lwd = 2)
# add axes
axis(1, seq(1,4,1), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2)
# add legend
legend("topleft", legend = c("female", "male"), lty= c(1, 3), horiz=TRUE)
# end plot
dev.off()
###############################################################
# plot so : age (s1a) line
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
# calculate sex differences
so.age.sex.tb <- (tapply(s1a$so, list(s1a$age, s1a$sex), mean))*100
# plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/SoAgeS1aOnlyIntsLine.png", width = 480, height = 480) # save plot
plot(so.age.sex.tb[,1], type = "o", xlim = c(0.5, 4.5), axes = F, ylim = c(0, 30),
  xlab = "", ylab = "", pch = 8, lwd = 2)
points(so.age.sex.tb[,2], pch = 8)
lines(so.age.sex.tb[,2], lty = 3, lwd = 2)
# add axes
axis(1, seq(1,4,1), c("19-25", "26-33", "34-49", "50+"))
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2, cex.axis = 1.5)
# add legend
legend("topleft", legend = c("female", "male"), lty= c(1, 3), horiz=TRUE)
# end plot
dev.off()

###############################################################
#                     EMOTIONAL VALUE
###############################################################
# plot very : age : emo (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
emoagevery <- ftable(s1a$very, s1a$emo, s1a$age)
sumemoagevery <- colSums(emoagevery)
pctattemoagevery <- emoagevery[3,]/sumemoagevery*100
pctpredemoagevery <- emoagevery[4,]/sumemoagevery*100
emoagevery <- rbind(pctattemoagevery, pctpredemoagevery)
colnames(emoagevery) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentEmoVery.png", width = 480, height = 480) # save plot
barplot(emoagevery, beside = T, ylim = c(0, 30),   xlab = "", ylab = "", ann = F, axes = F, xaxt='n', yaxt='n')
legend("topright", legend = c("nonemotional", "emotional"), fill = c("grey60", "grey95"), horiz=TRUE)
# add axes
axis(1, seq(2,11,3), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2, cex.axis = 1.5)
# end plot
dev.off()
###############################################################
# plot really : age : emo (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
#s1a <- s1a[s1a$really == 1, ]
emoagereally <- ftable(s1a$really, s1a$emo, s1a$age)
sumemoagereally <- colSums(emoagereally)
pctattemoagereally <- emoagereally[3,]/sumemoagereally*100
pctpredemoagereally <- emoagereally[4,]/sumemoagereally*100
emoagereally <- rbind(pctattemoagereally, pctpredemoagereally)
colnames(emoagereally) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentEmoReally.png", width = 480, height = 480) # save plot
barplot(emoagereally, beside = T, ylim = c(0, 30),   xlab = "", ylab = "", ann = F, axes = F, xaxt='n', yaxt='n')
legend("topright", legend = c("nonemotional", "emotional"), fill = c("grey60", "grey95"), horiz=TRUE)
# add axes
axis(1, seq(2,11,3), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2, cex.axis = 1.5)
# end plot
dev.off()
###############################################################
# plot so : age : emo (s1a)
s1a <- data[data$txtyp == "PrivateDialogue", ]
s1a <- s1a[s1a$fun == "predicative", ]
#s1a <- s1a[s1a$so == 1, ]
emoageso <- ftable(s1a$so, s1a$emo, s1a$age)
sumemoageso <- colSums(emoageso)
pctattemoageso <- emoageso[3,]/sumemoageso*100
pctpredemoageso <- emoageso[4,]/sumemoageso*100
emoageso <- rbind(pctattemoageso, pctpredemoageso)
colnames(emoageso) <- names(table(data$age))
# save plot
png("C:\\03-MyProjects\\07IntensifiersIrE\\Beamer\\images/PercentEmoSo.png", width = 480, height = 480) # save plot
barplot(emoageso, beside = T, ylim = c(0, 30),   xlab = "", ylab = "", ann = F, axes = F, xaxt='n', yaxt='n')
legend("topright", legend = c("nonemotional", "emotional"), fill = c("grey60", "grey95"), horiz=TRUE)
# add axes
axis(1, seq(2,11,3), c("19-25", "26-33", "34-49", "50+"), cex.axis = 1.5)
axis(2, seq(0, 30, 5), seq(0, 30, 5), las = 2, cex.axis = 1.5)
mtext("%", 2, 2.5, cex = 2, las = 2, cex.axis = 1.5)
# end plot
dev.off()
###############################################################
###############################################################
###############################################################
### --- statz
###############################################################
###############################################################
###############################################################
### ---        Model Building - GLM (glmer too unstable!)
################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
s1a <- data[data$txtyp == "PrivateDialogue", ]
# collase age groups for statz
s1a$age <- as.vector(unlist(sapply(s1a$age, function(x){
  x <- ifelse(x == "26-33", "26-49",
    ifelse(x == "34-49", "26-49", x)) } )))
data.dist <- datadist(s1a)
options(datadist = "data.dist")
################################################################
# check data
ftable(s1a$int, s1a$fun, s1a$sex, s1a$age, s1a$emo, s1a$priming)

ftable(s1a$very, s1a$fun, s1a$sex, s1a$age, s1a$emo, s1a$priming)

ftable(s1a$really, s1a$fun, s1a$sex, s1a$age, s1a$emo, s1a$priming)

ftable(s1a$so, s1a$fun, s1a$sex, s1a$age, s1a$emo, s1a$priming)

################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(int ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(int ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
###########################################################################
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo + priming, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(int ~ fun + emo + age + sex + priming,
  family = binomial, data = s1a)
lr.lrm <- lrm(int ~ fun + emo + age + sex + priming,
  family = binomial,  data = s1a, x = T, y = T)
# add noint column
data$noint <- as.vector(unlist(sapply(data$int, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(data$int * (predict(lr.glm, type = "response") >= 0.5)) + sum(data$noint * (predict(lr.glm, type="response") < 0.5))
tot <- sum(data$int) + sum(data$noint)
predict.acc <- (correct/tot)*100
##########################################################################
# continue diagnostics
# checking sample size (Green 1991)
# if you are interested in the overall model: 50 + 8k (k = number of predictors)
# if you are interested in individual predictors: 104 + k
# if you are interesetd in both: take the higher value!
smplesz <- function(x) {
  ifelse((length(x$fitted) < (104 + ncol(summary(x)$coefficients)-1)) == TRUE,
    return(
      paste("Sample too small: please increase your sample by ",
      104 + ncol(summary(x)$coefficients)-1 - length(x$fitted),
      " data points", collapse = "")),
    return("Sample size sufficient")) }
smplesz(lr.glm)

# create summary table
blrm.int <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.int, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_int.txt", sep="\t")
###########################################################################
###                         VERY
###########################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(very ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(very ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(very ~ fun,  family = binomial, data = s1a)
lr.lrm <- lrm(very ~ fun,  data = s1a, x = T, y = T, linear.predictors = T)
# add noint column
s1a$novery <- as.vector(unlist(sapply(s1a$very, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$very * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$novery * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$very) + sum(s1a$novery)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz(lr.glm)

# create summary table
blrm.very <- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.very, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_very.txt", sep="\t")
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
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(really ~ emo + age + sex + fun,  family = binomial, data = s1a)
lr.lrm <- lrm(really ~ emo + age + sex + fun,  data = s1a, x = T, y = T, linear.predictors = T)
# add noint column
s1a$noreally <- as.vector(unlist(sapply(s1a$really, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$really * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$noreally * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$really) + sum(s1a$noreally)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz(lr.glm)

# create summary table
blrm.really<- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.really, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_really.txt", sep="\t")
###########################################################################
###                         SO
###########################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(so ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(so ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(so ~ fun + emo + sex,  family = binomial, data = s1a)
lr.lrm <- lrm(so ~ fun + emo + sex,  data = s1a, x = T, y = T, linear.predictors = T)
# add noint column
s1a$noso <- as.vector(unlist(sapply(s1a$so, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$so * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$noso * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$so) + sum(s1a$noso)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz(lr.glm)

# create summary table
blrm.so<- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.so, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_so.txt", sep="\t")
###########################################################################
###                         TOO
###########################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(too ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(too ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(too ~ fun,  family = binomial, data = s1a)
lr.lrm <- lrm(too ~ fun,  data = s1a, x = T, y = T, linear.predictors = T)
# add noint column
s1a$notoo <- as.vector(unlist(sapply(s1a$too, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$too * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$notoo * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$too) + sum(s1a$notoo)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz(lr.glm)

# create summary table
blrm.too<- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.too, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_too.txt", sep="\t")
###########################################################################
###                         QUITE
###########################################################################
# generate initial minimal regression model including only the intercept as predictor
m0.glm = glm(quite ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(quite ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use both a step-wise step up and step-wise step-down model fitting based on the AIC
stepAIC(m0.glm, scope = list(upper = ~fun * sex * age * emo, lower = ~1),
  scale = 0, direction = c("both"))
# final inimal adequate model
lr.glm <- glm(quite ~ fun + age,  family = binomial, data = s1a)
lr.lrm <- lrm(quite ~ fun + age,  data = s1a, x = T, y = T, linear.predictors = T)
# add noint column
s1a$noquite <- as.vector(unlist(sapply(s1a$quite, function(x){
  x <- ifelse(x == 0, 1, 0) } )))
# check accuracy of the model
correct <- sum(s1a$quite * (predict(lr.glm, type = "response") >= 0.5)) + sum(s1a$noquite * (predict(lr.glm, type="response") < 0.5))
tot <- sum(s1a$quite) + sum(s1a$noquite)
predict.acc <- (correct/tot)*100
# checking sample size (Green 1991)
smplesz(lr.glm)

# create summary table
blrm.quite<- blrm.summary(lr.glm, lr.lrm, predict.acc)
# save results tabel to disc
write.table(blrm.quite, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/blrm_quite.txt", sep="\t")
###########################################################################
#                             GLMER
###########################################################################
#                             VERY
###########################################################################
# generate initial minimal regression model
m0.glm = glm(very ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(very ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# create model with a random intercept for flid
m0.glmer = glmer(very ~ (1|flid), data = s1a, family = binomial)
m0.lmer <- lmer(very ~ 1 + (1|flid), data = s1a, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm # the aic of the glmer object is smaller: random intercepts are justified
# model fitting
# write function to extract aic
getAic <- function(models){
  aic <- sapply(models, function(x){
    x <- AIC(logLik(x))})
  return(aic)}
# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(very ~ fun + (1|flid), family = binomial, data = s1a, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_very <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, s1a$very) #
# save results to disc
write.table(meblrm_very, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/meblrm_very.txt", sep="\t")
###########################################################################
#                            REALLY
###########################################################################
# generate initial minimal regression model
m0.glm = glm(really ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(really ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# create model with a random intercept for flid
m0.glmer = glmer(really ~ (1|flid), data = s1a, family = binomial)
m0.lmer <- lmer(really ~ 1 + (1|flid), data = s1a, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm # the aic of the glmer object is smaller: random intercepts are justified
# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(really ~ emo + age + sex + fun + (1|flid), family = binomial, data = s1a, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_really <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, s1a$really) #
# save results to disc
write.table(meblrm_really, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/meblrm_really.txt", sep="\t")
###########################################################################
#                            SO
###########################################################################
# generate initial minimal regression model
m0.glm = glm(so ~ 1, family = binomial, data = s1a) # baseline model glm
m0.lrm = lrm(so ~ 1, data = s1a, x = T, y = T) # baseline model lrm
# create model with a random intercept for flid
m0.glmer = glmer(so ~ (1|flid), data = s1a, family = binomial)
m0.lmer <- lmer(so ~ 1 + (1|flid), data = s1a, family = binomial)
# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm # the aic of the glmer object is smaller: random intercepts are justified
# create glmer object with best aic (cf. glm object)
mlr.glmer <- glmer(so ~ fun + emo + sex + (1|flid), family = binomial, data = s1a, control = glmerControl(optimizer = "bobyqa"))
# set up summary table
meblrm_so <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, s1a$so) #
# save results to disc
write.table(meblrm_so, "C:\\03-MyProjects\\07IntensifiersIrE\\Beamer/meblrm_so.txt", sep="\t")
###############################################################
#                      THE END
###############################################################


