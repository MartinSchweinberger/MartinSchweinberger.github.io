##################################################################
### --- R script "Acquisition of Intensifiers. Part 1"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- NOTE
### --- CONTACT
### --- martin.schweinberger.hh@gmail.com
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2015. "The acquisition of intensifiers",
### --- unpublished R script, Hamburg University.
###############################################################
###                   START
###############################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages
library(tm)
library(stringr)
library(plyr)
library(car)
library(RLRsim)
library(car)
library(QuantPsyc)
library(boot)
library(plyr)
library(rms)
library(tm)
library(plyr)
library(reshape)
library(zoo)
library(Hmisc)
library(gsubfn)
library(reshape)
source("C:\\R/ConcR_2.3.r")
source("C:\\R/POStagObject_02.R") # for pos-tagging objects
###############################################################
# Setting options
options(stringsAsFactors = F)
# define image directors
imageDirectory<-"C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images"
# Specify pathnames of the corpra
corpus <- "C:\\Corpora\\original\\CHILDESunzipped\\Eng-NA-MOR\\HSLLD"
bio.path <- "C:\\03-MyProjects\\07IntensifierAcquisition/biowc.txt"
#int.path <- "C:\\03-MyProjects\\07IntensifierAcquisition/int.txt"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files
cha = list.files(path = corpus, pattern = ".cha$", all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# extract biodata
#cha <- cha[1:5]
bio <- sapply(cha, function(x) {
  file <- gsub("C:\\Corpora\\original\\CHILDESunzipped\\Eng-NA-MOR\\HSLLD/", "", x, fixed = T)
  file <- gsub(".cha", "", file, fixed = T)
  x <- scan(x, what = "char", sep = "\t", quiet = T, quote = "", skipNul = T)
  x <- gsub("\n", " ", x, fixed = T)
  x <- gsub("\t", " ", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)
  x <- paste(x, collapse = " ", sep = " ")
  x <- str_replace_all(x, "\\*.*", "")
  rps <- str_count(x, "@ID")
  # name
  nam <- str_replace_all(file, ".*/", "")
  # id
  pid <- str_replace_all(x, ".*@PID", "@PID")
  # pid
  pid <- str_replace_all(x, ".*@PID: ", "")
  pid <- str_replace_all(pid, "@.*", "")
  pid <- str_trim(pid, side = "both")
  pid <- gsub(" {2,}", " ", pid)
  # languages
  lan <- str_replace_all(x, ".*@Languages: ", "")
  lan <- str_replace_all(lan, "@.*", "")
  lan <- str_trim(lan, side = "both")
  lan <- gsub(" {2,}", " ", lan)
  # participants
  par <- str_replace_all(x, ".*@Participants: ", "")
  par <- str_replace_all(par, "@.*", "")
  par <- str_trim(par, side = "both")
  par <- gsub(" {2,}", " ", par)
  # birth of child
  bir <- str_replace_all(x, ".*@Birth of CHI: ", "")
  bir <- str_replace_all(bir, "@.*", "")
  bir <- str_trim(bir, side = "both")
  bir <- gsub(" {2,}", " ", bir)
  # comment
  com <- str_replace_all(x, ".*@Comment: ", "")
  com <- str_replace_all(com, "@.*", "")
  com <- str_trim(com, side = "both")
  com <- gsub(" {2,}", " ", com)
  # date
  dat <- str_replace_all(x, ".*@Date: ", "")
  dat <- str_replace_all(dat, "@.*", "")
  dat <- str_trim(dat, side = "both")
  dat <- gsub(" {2,}", " ", dat)
  # location
  loc <- str_replace_all(x, ".*@Location: ", "")
  loc <- str_replace_all(loc, "@.*", "")
  loc <- str_trim(loc, side = "both")
  loc <- gsub(" {2,}", " ", loc)
  # situation
  sit <- str_replace_all(x, ".*@Situation: ", "")
  sit <- str_replace_all(sit, "@.*", "")
  sit <- str_trim(sit, side = "both")
  sit <- gsub(" {2,}", " ", sit)
  # activities
  act <- str_replace_all(x, ".*@Activities: ", "")
  act <- str_trim(act, side = "both")
  act <- gsub(" {2,}", " ", act)
  # ids
  ids <- as.vector(unlist(strsplit(gsub("(@ID)", "~~~\\1", x), "~~~")))
  ids <- as.vector(unlist(ids[2:length(ids)]))
  ids <- str_replace_all(ids, "@ID: ", "")
  ids <- str_replace_all(ids, "@.*", "")
  ids <- str_replace_all(ids, "\\|{2,}", "|")
  ids <- str_replace_all(ids, "eng\\|HSLLD\\|", "")
  ids <- gsub("\\|$", "", ids, perl = T)
  # agechild
  age <- str_replace_all(ids, "CHI\\|", "")
  age <- str_replace_all(age, "\\|.*", "")
  age <- str_replace_all(age, "[A-Z]{2,}[0-9]{0,2}", "NA")
  # genderchild
  sex <- str_replace_all(ids, ".*female", "female")
  sex <- str_replace_all(sex, ".*\\|male", "male")
  sex <- str_replace_all(sex, "male.*", "male")
  sex<- as.vector(unlist(sapply(sex, function(x){
    x <- ifelse(nchar(x) > 8, "NA", x)})))
  # ids 2
  ids <- str_replace_all(ids, "[0-9]{1,2};[0-9]{1,2}.{0,1}[0-9]{0,2}\\|{0,1}[a-z]{0,6}\\|", "")
  ids <- str_trim(ids, side = "both")
  ids <- str_replace_all(ids, "\\|$", "")
  # spk
  spk <- str_replace_all(ids, "\\|.*", "")
  # create a table of results
  vec <- c(file, pid, nam, lan, par, bir, com, dat, loc, sit, act, x)
  cntnt <- c(rep(vec, rps))
  df <- matrix(cntnt, ncol= 12, byrow = T)
  mt <- cbind(ids, spk, age, sex, df)
  colnames(mt) <- c("id", "spk", "age", "gender", "file", "pid", "name", "language", "participants", "birth", "comment", "date", "location", "situation", "actvities", "all")
  return(mt)
  })
# collapse all tables into 1 table
biotb <- do.call("rbind", bio)
biotb <- as.data.frame(biotb)
# inspect data
head(biotb)

###############################################################
# count words for each speaker
wc <- sapply(cha, function(x) {
  file <- gsub("C:\\Corpora\\original\\CHILDESunzipped\\Eng-NA-MOR\\HSLLD/", "", x, fixed = T)
  file <- gsub(".cha", "", file, fixed = T)
  x <- scan(x, what = "char", sep = "\t", quiet = T, quote = "", skipNul = T)
  x <- gsub("\n", " ", x, fixed = T)
  x <- gsub("\t", " ", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)
  orig <- paste(x, collapse = " ", sep = " ")
  full <- strsplit(gsub("([*]{1,1}[A-Z]{2,4}[0-9]{0,1}:)", "~~~\\1", orig), "~~~")
  full2 <- unlist(full)
  meta <- full2[1]
  full2 <- full2[2:length(full2)]
  x <- strsplit(gsub("([%|*][a-z|A-Z]{2,4}[0-9]{0,1}:)", "~~~\\1", orig), "~~~")
  txt <- sapply(x, function(y){
    y <- str_replace_all(y, "%.*", "")
    y <- str_replace_all(y, "@.*", "")
    y <- str_replace_all(y, "\\.|\\?|;|,|\\[|\\]|!|\\+|\\(|\\)|<|>|[0]", "")
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
    y <- y[y != ""] })
  x <- str_split(txt, ":")
  spk <- sapply(x, function(y){
    y <- y[1] })
  file <- as.vector(unlist(rep(file, length(spk))))
  wrds <- sapply(x, function(y){
    y <- y[2]
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
    })
  cnt <- sapply(x, function(y){
    y <- y[2]
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
    y <- str_split(y, " ")
    y <- sapply(y, function(z){ z <- ifelse(z == "", 0, length(z)) })
    y <- unique(y)
    })
  spk <- gsub("*", "", spk, fixed = T)
  df <- matrix(data = c(file, full2, txt, spk, wrds, cnt, rep(meta, length(txt))), ncol = 7, byrow = F)
  })
# collapse all tables into 1 table
wc <- do.call("rbind", wc)
wc <- as.data.frame(wc)
colnames(wc) <- c("file", "full", "spkutt", "spk", "utt", "wc", "meta")
# inspect data
head(wc)

###############################################################
# save data to disc
write.table(wc, file = "C:\\03-MyProjects\\07IntensifierAcquisition/wc.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
###############################################################
# read data from disc
wc <- read.table("C:\\03-MyProjects\\07IntensifierAcquisition/wc.txt", sep = "\t", header = T, quote = "", comment.char = "")
# clean read data
wc$utt <- gsub("\"", "", wc$utt, fixed = T)
# clean colnames
colnames(wc) <- gsub("X.", "", colnames(wc), fixed = T)
colnames(wc) <- gsub(".", "", colnames(wc), fixed = T)
# clean data
wc$utt <- as.vector(unlist(sapply(wc$utt, function(x){
  x <- gsub("/", "", x, fixed = T)
  x <- gsub("=", "", x, fixed = T)
  x <- gsub("&", "", x, fixed = T)
  x <- gsub("*", "", x, fixed = T)
  x <- gsub("^", "", x, fixed = T)
  x <- gsub("_", "", x, fixed = T)
  x <- gsub("€", "", x, fixed = T)
  x <- gsub("†", "", x, fixed = T)
  x <- gsub("â", "", x, fixed = T)
  x <- gsub("ž", "", x, fixed = T)
  x <- gsub("‘", "'", x, fixed = T) } )))
# inspect data
head(wc)

###############################################################
# split data into smaller chunks
pos01 <- wc$utt[1:50000]
pos02 <- wc$utt[50001:100000]
pos03 <- wc$utt[100001:150000]
pos04 <- wc$utt[150001:200000]
pos05 <- wc$utt[200001:250000]
pos06 <- wc$utt[250001:300000]
pos07 <- wc$utt[300001:nrow(wc)]
# pos tagging data
hslldpos01 <- POStag(object = pos01)
hslldpos01 <- as.vector(unlist(hslldpos01))
writeLines(hslldpos01, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos01.txt", sep = "\n", useBytes = FALSE)
###
hslldpos02 <- POStag(object = pos02)
hslldpos02 <- as.vector(unlist(hslldpos02))
writeLines(hslldpos02, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos02.txt", sep = "\n", useBytes = FALSE)
###
hslldpos03 <- POStag(object = pos03)
hslldpos03 <- as.vector(unlist(hslldpos03))
writeLines(hslldpos03, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos03.txt", sep = "\n", useBytes = FALSE)
###
hslldpos04 <- POStag(object = pos04)
hslldpos04 <- as.vector(unlist(hslldpos04))
writeLines(hslldpos04, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos04.txt", sep = "\n", useBytes = FALSE)
###
hslldpos05 <- POStag(object = pos05)
hslldpos05 <- as.vector(unlist(hslldpos05))
writeLines(hslldpos05, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos05.txt", sep = "\n", useBytes = FALSE)
###
hslldpos06 <- POStag(object = pos06)
hslldpos06 <- as.vector(unlist(hslldpos06))
writeLines(hslldpos06, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos06.txt", sep = "\n", useBytes = FALSE)
###
hslldpos07 <- POStag(object = pos07)
hslldpos07 <- as.vector(unlist(hslldpos07))
writeLines(hslldpos07, con = "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos07.txt", sep = "\n", useBytes = FALSE)
###############################################################
# list pos tagged elements
postag.files = c("C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos01.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos02.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos03.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos04.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos05.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos06.txt",
  "C:\\03-MyProjects\\07IntensifierAcquisition/hslldpos07.txt")
# load pos tagged elements
hslldpos <- as.vector(unlist(sapply(postag.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  } )))
# unlist pos tagged elements and add to data frame
wc$hslldpos <- hslldpos
# clean data frame
wc4 <- apply(wc, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(wc4)
###############################################################
# save data before pos tagging
write.table(df5, "C:\\03-MyProjects\\07IntensifierAcquisition/hslld_postagged.txt", sep = "\t", row.names = F, col.names = T)
# read data for pos tagging
df5 <- read.table("C:\\03-MyProjects\\07IntensifierAcquisition/hslld_postagged.txt", sep = "\t", header = T, quote = "", comment.char = "")
# clean data frame
df5 <- apply(df5, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(df5)
# add id column
df5$id <- 1:nrow(df5)
# clean colnames
colnames(df5) <- c("fl", "cntnt", "spkutt", "spk", "utt", "wc", "meta", "pos", "id")
# clean postagged utts
df5$pos <- as.vector(unlist(sapply(df5$pos, function(x){
  x <- gsub("/''", "", x, fixed = T)
  x <- gsub("/``", "", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)} )))
# save data before pos tagging
write.table(df5, "C:\\03-MyProjects\\07IntensifierAcquisition/hslld_postagged_2.txt", sep = "\t", row.names = F, col.names = T)
# read data for pos tagging
df5 <- read.table("C:\\03-MyProjects\\07IntensifierAcquisition/hslld_postagged_2.txt", sep = "\t", header = T, quote = "", comment.char = "")
# clean data frame
df5 <- apply(df5, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(df5)
# clean colnames
colnames(df5) <- c("fl", "cntnt", "spkutt", "spk", "utt", "wc", "meta", "pos", "id")
# attach file subfile speaker id to pos-tagged string
df5$spksupos <- as.vector(unlist(apply(df5, 1, function(x){
  x <- paste(x[4], ": ", x[8], sep = "", collapse = "")
  } )))
# repair file names
df5$fl <- as.vector(unlist(sapply(df5$fl, function(x){
  x <- gsub("/", "-", x, fixed = T) } )))
# split file subfile speaker pos-tagged su by file subfile
hslld.tmp1 <- by(df5$spksupos, df5$fl, paste)
# rename data
hslld <- hslld.tmp1
# save result to disc
lapply(seq_along(hslld), function(i)writeLines(text = unlist(hslld[i]),
    con = paste("C:\\Corpora\\edited\\HSLLD-PosTagged/", names(hslld)[i],".txt", sep = "")))
###############################################################
# tabulate wc
wctb <- data.frame(wc$file, wc$spk, wc$wc)
# clean colnames
colnames(wctb) <- str_replace_all(colnames(wctb), "wc.", "")
# clean content
wctb$file <- gsub("\"", "", wctb$file, fixed = T)
wctb$spk <- gsub("\"", "", wctb$spk, fixed = T)
wctb$wc <- gsub("\"", "", wctb$wc, fixed = T)
# combine file and speaker
wctb$filespk <- as.vector(unlist(apply(wctb, 1, function(x){
  x <- paste(x[1], "$", x[2], collapse = "")
  x <- str_replace_all(x, " ", "") })))
wctb$wc <- gsub("\"", "", wctb$wc, fixed = T)
wctb$wc <- as.numeric(wctb$wc)
wctb2 <- ddply(wctb, "wctb$filespk", transform, wc=sum(wc))
wctb2 <- subset(wctb2, !duplicated(filespk))
# combine bio and wc
biowc <- join(biotb, wctb2, by = c("file", "spk"), type = "left")
# inspect data
head(biowc)

# update gender column
biowc$gender <- with(biowc, ifelse(biowc$spk =="MOT", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS1", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS2", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS3", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI1", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI2", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI3", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="WOM", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="ANT", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="AUN", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GMA", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="FAT", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="UNC", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO1", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO2", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR1", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR2", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR3", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR3", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BAB", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GFA", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GPA", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAN", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAL", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAL2", "male", biowc$gender))
# create a column holding the socio economic status (ses)
biowc$ses <- biowc$comment
biowc$ses <- str_replace_all(biowc$ses, ".*SES", "")
biowc$ses <- str_replace_all(biowc$ses, ".*lower.*", "lower")
biowc$ses <- str_replace_all(biowc$ses, ".*middle.*", "middle")
biowc$ses <- as.vector(unlist(sapply(biowc$ses, function(x){
  x <- ifelse(x == "lower", "lower",
    ifelse(x == "middle", "middle", NA)) })))
# save results to disc
write.table(biowc, file = "C:\\03-MyProjects\\07IntensifierAcquisition/biowc.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
###############################################################
###                    SEARCH
###############################################################
# set parameters
pathname <- "C:\\Corpora\\edited\\HSLLD-PosTagged"
context <- 80
all.pre = T
# define search strings
search.pattern <-  c("[A-Z]{0,1}[a-z]{0,}-{0,1}[a-z]{2,}\\/JJ")
##########################################################
# start searches
int <- ConcR(pathname, search.pattern, context, all.pre = T)
# extract speaker
int$all.pre <- gsub(".*([A-Z]{2,}_{0,1}[0-9]{0,1}:)", "\\1", int$all.pre)
int$all.pre <- str_replace_all(int$all.pre, ":.*", "")
# code status
int$status <- as.vector(unlist(sapply(int$post, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(" .*", "", x)
  x <- gsub(".*\\/", "", x)
  x <- gsub("NNS", "NN", x)
  x <- ifelse(x == "NN", "attr", "pred")
  } )))
# code previous element (pos)
int$prepos <- as.vector(unlist(sapply(int$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".*\\/", "", x)
  x <- gsub(" .*", "", x) } )))
# code previous element (lexeme)
int$prelex <- as.vector(unlist(sapply(int$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".* ", "", x)
  x <- gsub("\\/.*", "", x) } )))
# clean file
int$file <- as.vector(unlist(sapply(int$file, function(x){
  x <- gsub(".txt", "", x, fixed = T)
  x <- gsub("-", "/", x, fixed = T) } )))
# extract all attributive adjectives
int <- int[int$status == "attr",]
# code intensifiers
int$absint <- as.vector(unlist(sapply(int$prelex, function(x){
  x <- ifelse(x == "awful", 1,
    ifelse(x == "awfully", 1,
    ifelse(x == "completely", 1,
    ifelse(x == "pretty", 1,
    ifelse(x == "radically", 1,
    ifelse(x == "real", 1,
    ifelse(x == "really", 1,
    ifelse(x == "so", 1,
    ifelse(x == "too", 1,
    ifelse(x == "rtotally", 1,
    ifelse(x == "very", 1,
    ifelse(x == "wonderfully", 1, 0)))))))))))) } )))
# inspect all pos tags preceeding attr adjectives
#table(int$prepos)
# reorganize data frames
# colnames(int)
int <- data.frame(1:nrow(int), int$file, int$file, int$file, int$file, int$all.pre, int$pre, int$token, int$post,
  int$status, int$prepos, int$prelex, int$absint, int$token)
# adapt column names
colnames(int) <- c("id", "file", "visit", "styp", "chi", "spk", "pre", "token", "post", "status", "prepos",
  "prelex", "absint", "tokenlex")
# clean visit
int$visit <- tolower(gsub("\\/.*", "", int$visit))
# clean styp
int$styp <- tolower(gsub("HV[0-9]{1,1}\\/", "", int$styp))
int$styp <- gsub("\\/.*", "", int$styp)
# clean chi
int$chi <- tolower(gsub(".*\\/", "", int$chi))
int$chi <- gsub("[a-z]{2,2}[0-9]{1,1}", "", int$chi)
# clean tokenlex
int$tokenlex <- gsub("\\/.*", "", int$tokenlex)
int$tokenlex <- str_trim(int$tokenlex, side = "both")
# inspect data
head(int)

###############################################################
# save table to disc
write.table(int, file = "C:\\03-MyProjects\\07IntensifierAcquisition/int.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
###############################################################
###                   END of PART 1
###############################################################
##################################################################
### --- R script "Acquisition of Intensifiers"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- NOTE
### --- CONTACT
### --- martin.schweinberger.hh@gmail.com
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2015. "The acquisition of intensifiers",
### --- unpublished R script, Hamburg University.
###############################################################
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
library(ggplot2)
library(RLRsim)
library(car)
library(QuantPsyc)
library(boot)
library(nlme)
#detach(package:nlme)
library(lme4)
library(ez)
library(sjPlot)
library(visreg)
library(mlogit)
library(plyr)
library(rms)
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
library(reshape)
library(zoo)
library(cfa)
library(Hmisc)
library(RLRsim)
library(sjPlot)
library(mlogit)
library(effects)
library(lme4)
library(languageR)
library(grDevices)
source("C:\\R/ConcR_2.3.r")
source("C:\\R/mlr.summary.R")
source("C:\\R/blr.summary.R")
source("C:\\R/meblr.summary.R")
source("C:\\R/ModelFittingSummarySWSDGLM.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSUGLM.R") # for Mixed Effects Model fitting (step-wise step-down): Binary Logistic Mixed Effects Models
source("C:\\R/ModelFittingSummarySWSULogReg.R") # for Fixed Effects Model fitting: Binary Logistic Models
###############################################################
# Setting options
options(stringsAsFactors = F)
# define image directors
imageDirectory<-"C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images"
# Specify pathnames of the corpra
corpus <- "C:\\Corpora\\original\\CHILDESunzipped\\Eng-NA-MOR\\HSLLD"
bio.path <- "C:\\03-MyProjects\\07IntensifierAcquisition/biowc.txt"
int.path <- "C:\\03-MyProjects\\07IntensifierAcquisition/int.txt"
###############################################################
### ---               PART TWO
###############################################################
###                   START
###############################################################
# read in data
bio <- read.table(bio.path, sep = "\t", header=TRUE)
int <- read.table(int.path, sep = "\t", header=TRUE)
# extract examples
exp <- int[int$absint == 1, ]
# join data
int.ac <- join(int, bio, by = c("file", "spk"), type = "left")
# inspect data
#head(int.ac)

###############################################################
# restructure data frame
int <- data.frame(int.ac$file, int.ac$visit, int.ac$styp, int.ac$spk,
  int.ac$age, int.ac$gender, int.ac$wc, int.ac$status, int.ac$prepos, int.ac$prelex,
  int.ac$tokenlex, int.ac$absint)
# clean colnames
colnames(int) <- gsub("int.ac.", "", colnames(int))
# clean age
int$age <- gsub(".", "qwertz", int$age, fixed = T)
int$age <- gsub("qwertz.*", "", int$age)
# remove speakers with less than 20 words
int.tmp5 <- int#[int$wc >=20, ]
# recode age
agem <- gsub(".*;", "", int.tmp5$age)
agem <- as.numeric(agem)
agem <- as.vector(unlist(sapply(agem, function(x){
  x <- x/12
  x <- round(x, 3)} )))
agey <- gsub(";.*", "", int.tmp5$age)
agey <- as.numeric(agey)
int.tmp5$ageedit <- agey + agem
# remove all speakers who are neither child or mother
int.tmp5$rmv <- as.vector(unlist(sapply(int.tmp5$spk, function(x){
  x <- ifelse(x == "CHI", "keep",
    ifelse(x == "MOT", "keep", "rmv")) } )))
int.tmp6 <- int.tmp5[int.tmp5$rmv == "keep", ]
# recode age
int.tmp6$agecat <- gsub("\\..*", "", int.tmp6$ageedit)
int.tmp6$agecat <- factor(int.tmp6$agecat, levels = c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
# rename data set
data <- int.tmp6
# recode styp
#data <- data[data$styp == "mt",]
#data$styp <- as.vector(unlist(sapply(data$styp, function(x){
#  x <- ifelse(x == "br", "formal",
#    ifelse(x == "tp", "informal",
#    ifelse(x == "mt", "informal",
#    ifelse(x == "er", "formal",
#    ifelse(x == "et", "formal",
#    ifelse(x == "lw", "formal",
#    ifelse(x == "re", "formal",
#    ifelse(x == "md", "formal", x)))))))) } )))
# remove styp MD, sytp er and styp re from data (too low frequency, er=14411, md = 1436, re = 4469)
data <- data[data$styp != "er",]
data <- data[data$styp != "md",]
data <- data[data$styp != "re",]
# remove data points above age 10 (too few instances)
data$remove <- as.vector(unlist(sapply(data$agecat, function(x){
  x <- ifelse(is.na(x) == T, "keep",
  ifelse(x == "11", "rmv",
  ifelse(x == "12", "rmv", "keep"))) } )))
data <- data[which(data$remove == "keep"), ]
data$agecat <- factor(data$agecat, levels = c("3", "4", "5", "6", "7", "8", "9", "10"))
# selcet mt data
datamt <- data[data$styp == "mt", ]
databr <- data[data$styp == "br", ]
datalw <- data[data$styp == "lw", ]
# remove test columns
data <- data[,c(1:13, 15)]
# inspect data
head(data)

# create data of children only
chint <- data[data$spk == "CHI",]
chintbr <- databr[databr$spk == "CHI",]
chintmt <- datamt[datamt$spk == "CHI",]
chintlw <- datalw[datalw$spk == "CHI",]
# create data of mothers only
motint <- data[data$spk == "MOT",]
###############################################################
###############################################################
###############################################################
### ---               VIZUALIZATION
###############################################################
###############################################################
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepct.png") # save plot
# tabulate data
inttb01 <- table(chint$absint, chint$agecat)
nchi <- ftable(chint$spk, chint$agecat)
nchi <- colSums(nchi)
# calculate percentages of intensified slots
pcnt <- round(inttb01[2,]/(inttb01[1,]+inttb01[2,])*100, 2)
# plot data
barplot(pcnt, col = "grey80", ylim = c(-2, 10), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers against age")
text(seq(0.7, 9.1, 1.2), pcnt + 1, cex = .8, labels = pcnt)
text(seq(0.7, 9.1, 1.2), -1, labels = c(paste("N(slots):\n", nchi[1], sep = ""),
  nchi[2: length(nchi)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                      MT
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepctmt02.png") # save plot
# tabulate data
inttb02 <- table(chintmt$absint, chintmt$agecat)
nchi02 <- ftable(chintmt$spk, chintmt$agecat)
nchi02 <- colSums(nchi02)
# calculate percentages of intensified slots
pcnt02 <- round(inttb02[2,]/(inttb02[1,]+inttb02[2,])*100, 2)
# plot data
barplot(pcnt02, col = "grey80", ylim = c(-1, 10), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers\nagainst age in meal time settings")
text(seq(0.7, 9.1, 1.2), pcnt02 + 1, cex = .8, labels = pcnt02)
text(seq(0.7, 9.1, 1.2), -.5, labels = c(paste("N(slots):\n", nchi02[1], sep = ""),
  nchi02[2: length(nchi02)]), cex = .8)
grid()
box()
dev.off()
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepctmt03.png") # save plot
# tabulate data
inttb03 <- table(chintmt$absint, chintmt$agecat)
inttb03 <- inttb03[, colSums(inttb03) > 100]
nchi03 <- ftable(chintmt$spk, chintmt$agecat)
nchi03 <- colSums(nchi03)
# calculate percentages of intensified slots
pcnt03 <- round(inttb03[2,]/(inttb03[1,]+inttb03[2,])*100, 2)
# plot data
barplot(pcnt03, col = "grey80", ylim = c(-1, 10), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers\nagainst age in meal time settings")
text(seq(0.7, 9.1, 1.2), pcnt03 + 1, cex = .8, labels = pcnt03)
text(seq(0.7, 9.1, 1.2), -.5, labels = c(paste("N(slots):\n", nchi03[1], sep = ""),
  nchi03[2: length(nchi03)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                      BR
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepctbr02.png") # save plot
# tabulate data
inttb04 <- table(chintbr$absint, chintbr$agecat)
nchi04 <- ftable(chintbr$spk, chintbr$agecat)
nchi04 <- colSums(nchi04)
# calculate percentages of intensified slots
pcnt04 <- round(inttb04[2,]/(inttb04[1,]+inttb04[2,])*100, 2)
pcnt04 <- pcnt04[is.na(pcnt04) == F]
# plot data
barplot(pcnt04, col = "grey80", ylim = c(-1, 10), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers\nagainst age in book reading settings")
text(seq(0.7, 9.1, 1.2), pcnt04 + 1, cex = .8, labels = pcnt04)
text(seq(0.7, 9.1, 1.2), -.5, labels = c(paste("N(slots):\n", nchi04[1], sep = ""),
  nchi04[2: length(nchi04)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                      LW
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepctlw02.png") # save plot
# tabulate data
inttb05 <- table(chintlw$absint, chintlw$agecat)
nchi05 <- ftable(chintlw$spk, chintlw$agecat)
nchi05 <- colSums(nchi05)
# calculate percentages of intensified slots
pcnt05 <- round(inttb05[2,]/(inttb05[1,]+inttb05[2,])*100, 2)
pcnt05 <- pcnt05[is.na(pcnt05) == F]
# plot data
barplot(pcnt05, col = "grey80", ylim = c(-1, 10), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers\nagainst age in letter writing settings")
text(seq(0.7, 9.1, 1.2), pcnt05 + 1, cex = .8, labels = pcnt05)
text(seq(0.7, 9.1, 1.2), -.5, labels = c(paste("N(slots):\n", nchi05[4], sep = ""),
  nchi05[5:length(nchi05)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                    AGE STYP
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagestyppct.png",
  width = 800, height = 480) # save plot
# tabulate data
inttb06 <- ftable(chint$styp, chint$absint, chint$agecat)
nchi06 <- ftable(chint$styp, chint$spk, chint$agecat)
nchi06 <- colSums(nchi06)
# calculate percentages of intensified slots
pcnt06br <- round(inttb06[2,]/(inttb06[1,]+inttb06[2,])*100, 1)
pcnt06et <- round(inttb06[4,]/(inttb06[3,]+inttb06[4,])*100, 1)
pcnt06lw <- round(inttb06[6,]/(inttb06[5,]+inttb06[6,])*100, 1)
pcnt06mt <- round(inttb06[8,]/(inttb06[7,]+inttb06[8,])*100, 1)
pcnt06tp <- round(inttb06[10,]/(inttb06[9,]+inttb06[10,])*100, 1)
pcnt06 <- rbind(pcnt06br, pcnt06et, pcnt06lw, pcnt06mt, pcnt06tp)
colnames(pcnt06) <- 3:10
# plot data
barplot(pcnt06, col = c("red", "grey 40", "blue", "green", "grey80"),
  ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of child", axes = T, beside = T,
  main = "Children's use of intensifiers by situation type")
text(seq(1.5, 43.5, 6), pcnt06[1,] + 1, cex = .8, labels = pcnt06[1,])
text(seq(2.5, 44.5, 6), pcnt06[2,] + 1, cex = .8, labels = pcnt06[2,])
text(seq(3.5, 45.5, 6), pcnt06[3,] + 1, cex = .8, labels = pcnt06[3,])
text(seq(4.5, 46.5, 6), pcnt06[4,] + 1, cex = .8, labels = pcnt06[4,])
text(seq(5.5, 47.5, 6), pcnt06[5,] + 1, cex = .8, labels = pcnt06[5,])
grid()
legend("topleft", inset = .05, c("book reading (br)", "experimental task (et)",
  "letter writing (lw)", "meal time (mt)", "toy play (tp)"),
  horiz = F,  fill = c("red", "grey 40", "blue", "green", "grey80"))
box()
dev.off()
###############################################################
###                    MOT STYP
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intmotstyppct.png",
  width = 480, height = 480) # save plot
# tabulate data
inttb07 <- ftable(motint$styp, motint$absint)
nmot07 <- ftable(motint$styp, motint$spk)
nmot07 <- colSums(nmot07)
# calculate percentages of intensified slots
pcnt07 <- cbind(inttb07, round(inttb07[,2]/(inttb07[,1]+inttb07[,2])*100, 1))
# plot data
barplot(pcnt07[,3], col = c("red", "grey 40", "blue", "green", "grey80"),  ylim = c(0, 12), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of Child", axes = T, beside = T,
  main = "Children's use of intensifiers by situation type")
text(seq(0.7, 9.1, 1.2), pcnt07[,3] + 1, cex = .8, labels = pcnt07[,3])
grid()
legend("topright", inset = .05, c("book reading (br)", "experimental task (et)",
  "letter writing (lw)", "meal time (mt)", "toy play (tp)"),
  horiz = F,  fill = c("red", "grey 40", "blue", "green", "grey80"))
box()
dev.off()
###############################################################
###                     TTR: CHI INT
###############################################################
# plot type-token-ratio of intensifiers against age
chintint <- chint[chint$absint == 1, ]
inttb08 <- table(chintint$prelex, chintint$agecat)
# calculate token freq
tokf08 <- colSums(inttb08)
# calculate type freq
typf08 <- t(apply(inttb08, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf08 <- colSums(typf08)
# calculate type-token-ratio
chittr08 <- data.frame(3:10, round(typf08/tokf08, 2), typf08, tokf08)
rownames(chittr08) <- 3:10
colnames(chittr08) <- c("Age", "TTR", "Types", "Tokens")
chittr08 <- chittr08[chittr08$Tokens > 1,]
# plot data
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/ttrchiageint.png") # save plot
plot(chittr08[,1:2], xlab = "Age", ylab = "Type-Token-Ratio (TTR): Intensifiers",
  ylim = c(0,1), pch = 19,
  main = "Children's TTRs of intensifiers by age")
abline(lm(chittr08[,2]~chittr08[,1]), col="red") # regression line (y~x)
lws08 <- unlist(lowess(chittr08[,2],chittr08[,1]))
lines(lws08[1:length(lws08)/2], col="blue")
grid()
dev.off()
###############################################################
###                     TTR: CHI ADJ
###############################################################
# plot type-token-ratio of intensifiers against age
chintint <- chint[chint$absint == 1, ]
inttb09 <- table(chintint$tokenlex, chintint$agecat)
# calculate token freq
tokf09 <- colSums(inttb09)
# calculate type freq
typf09 <- t(apply(inttb09, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf09 <- colSums(typf09)
# calculate type-token-ratio
chittr09 <- data.frame(3:10, round(typf09/tokf09, 2), typf09, tokf09)
rownames(chittr09) <- 3:10
colnames(chittr09) <- c("Age", "TTR", "Types", "Tokens")
chittr09 <- chittr09[chittr09$Tokens > 1,]
# plot data
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/ttrchiageadj.png") # save plot
plot(chittr09[,1:2], xlab = "Age", ylab = "Type-Token-Ratio (TTR): Adjectives",
  ylim = c(0,1), pch = 19,
  main = "Children's TTRs of intensified adjectives by age")
abline(lm(chittr09[,2]~chittr09[,1]), col="red") # regression line (y~x)
lws09 <- unlist(lowess(chittr09[,2],chittr09[,1]))
lines(lws09[1:length(lws09)/2], col="blue")
grid()
dev.off()
###############################################################
###                     MOT INT FREQ
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagepctmot.png") # save plot
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint10 <- join(motint, ag, by = "file", type = "left", match = "all")
motint10 <- motint10[, c(1:13, 15)]
motint10 <- motint10
# tabulate data
inttb10 <- table(motint10$absint, motint10$agecat)
nmot10 <- ftable(motint10$spk, motint10$agecat)
nmot10 <- colSums(nmot10)
# calculate percentages of intensified slots
pcnt10 <- round(inttb10[2,]/(inttb10[1,]+inttb10[2,])*100, 2)
# plot data
barplot(pcnt10, col = "grey80", ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of Child", axes = T, beside = T,
  main = "Mothers' use of intensifiers\nagainst the age of her child")
text(seq(0.7, 9.1, 1.2), pcnt10 + 1, cex = .8, labels = pcnt10)
text(seq(0.7, 9.1, 1.2), -1, labels = c(paste("N(slots):\n", nmot10[1], sep = ""),
  nmot10[2: length(nmot10)]), cex = .8)
tbmotabsint <- table(motint10$absint)
m10 <- round(tbmotabsint[2]/(tbmotabsint[1]+tbmotabsint[2])*100, 2)
lines(0:10, rep(m10, length(0:10)), col="red", lty = 2, lwd = 1)
text(8.2, m10 + 1, cex = .8, paste("mean = ", m10, collapse = ""), col = "red")
grid()
box()
dev.off()
###############################################################
###                     MOT INT MT FREQ
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagemtpctmot.png") # save plot
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint11 <- join(motint, ag, by = "file", type = "left", match = "all")
motint11 <- motint11[, c(1:13, 15)]
motint11 <- motint11[motint11$styp == "mt", ]
# tabulate data
inttb11 <- table(motint11$absint, motint11$agecat)
nmot11 <- ftable(motint11$spk, motint11$agecat)
nmot11 <- colSums(nmot11)
# calculate percentages of intensified slots
pcnt11 <- round(inttb11[2,]/(inttb11[1,]+inttb11[2,])*100, 2)
# plot data
barplot(pcnt11, col = "grey80", ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of Child", axes = T, beside = T,
  main = "Mother's Use of intensifiers during mean time")
text(seq(0.7, 9.1, 1.2), pcnt11 + 1, cex = .8, labels = pcnt11)
text(seq(0.7, 9.1, 1.2), -1, labels = c(paste("N(slots):\n", nmot11[1], sep = ""),
  nmot11[2: length(nmot11)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                     MOT INT BR FREQ
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagebrpctmot.png") # save plot
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint12 <- join(motint, ag, by = "file", type = "left", match = "all")
motint12 <- motint12[, c(1:13, 15)]
motint12 <- motint12[motint12$styp == "br", ]
# tabulate data
inttb12 <- table(motint12$absint, motint12$agecat)
nmot12 <- ftable(motint12$spk, motint12$agecat)
nmot12 <- colSums(nmot12)
# calculate percentages of intensified slots
pcnt12 <- round(inttb12[2,]/(inttb12[1,]+inttb12[2,])*100, 2)
pcnt12 <- pcnt12[is.na(pcnt12) == F]
# plot data
x <- barplot(pcnt12, col = "grey80", ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of Child", axes = T, beside = T,
  main = "Mother's Use of intensifiers during book reading")
m12 <- round(sum(inttb12[2,])/(sum(inttb12[1,]) + sum(inttb12[2,]))*100, 2)
lines(0:10, rep(m12, length(0:10)), lty = 2, col = "red", lwd = 1)
text(4.3, m12+0.5, paste("mean = ", m12, collapse = ""), cex = .8, col = "red")
text(seq(0.7, 9.1, 1.2), pcnt12 + 1, cex = .8, labels = pcnt12)
text(seq(0.7, 9.1, 1.2), -1, labels = c(paste("N(slots):\n", nmot12[1], sep = ""),
  nmot12[2: length(nmot12)]), cex = .8)
grid()
box()
dev.off()
###############################################################
###                     MOT INT LW FREQ
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagelwpctmot.png") # save plot
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint13 <- join(motint, ag, by = "file", type = "left", match = "all")
motint13 <- motint13[, c(1:13, 15)]
motint13 <- motint13[motint13$styp == "lw", ]
# tabulate data
inttb13 <- table(motint13$absint, motint13$agecat)
nmot13 <- ftable(motint13$spk, motint13$agecat)
nmot13 <- colSums(nmot13)
# calculate percentages of intensified slots
pcnt13 <- round(inttb13[2,]/(inttb13[1,]+inttb13[2,])*100, 2)
pcnt13 <- pcnt13[is.na(pcnt13) == F]
# plot data
barplot(pcnt13, col = "grey80", ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of Child", axes = T, beside = T,
  main = "Mother's Use of intensifiers during letter writing")
text(seq(0.7, 11.5, 1.2), pcnt13 + 1, cex = .8, labels = pcnt13)
text(seq(0.7, 11.5, 1.2), -1, labels = c(paste("N(slots):\n", nmot13[4], sep = ""),
  nmot13[5:length(nmot13)]), cex = .8)
m13 <- round(sum(inttb13[2,])/(sum(inttb13[1,]) + sum(inttb13[2,]))*100, 2)
lines(0:12, rep(m13, length(0:12)), lty = 2, col = "red", lwd = 1)
text(4.3, m13+0.5, paste("mean = ", m13, collapse = ""), cex = .8, col = "red")
grid()
box()
dev.off()
###############################################################
###
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagestyppctmot.png",
  width = 900, height = 480)
# tabulate data
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint14 <- join(motint, ag, by = "file", type = "left", match = "all")
motint14 <- motint14[, c(1:13, 15)]
inttb14 <- ftable(motint14$styp, motint14$absint, motint14$agecat)
nmot14 <- ftable(motint14$styp, motint14$spk, motint14$agecat)
nmot14 <- colSums(nmot14)
# calculate percentages of intensified slots
pcnt14br <- round(inttb14[2,]/(inttb14[1,]+inttb14[2,])*100, 1)
pcnt14et <- round(inttb14[4,]/(inttb14[3,]+inttb14[4,])*100, 1)
pcnt14lw <- round(inttb14[6,]/(inttb14[5,]+inttb14[6,])*100, 1)
pcnt14mt <- round(inttb14[8,]/(inttb14[7,]+inttb14[8,])*100, 1)
pcnt14tp <- round(inttb14[10,]/(inttb14[9,]+inttb14[10,])*100, 1)
pcnt14 <- rbind(pcnt14br, pcnt14et, pcnt14lw, pcnt14mt, pcnt14tp)
colnames(pcnt14) <- 3:10
# plot data
x <- barplot(pcnt14, col = c("red", "grey 40", "blue", "green", "grey80"), ylim = c(-2, 30),
  ylab = "Percent (intensified slots of all slots)",
  xlab = "Age of motld", axes = T, beside = T,
  main = "Mother's Use of intensifiers by situation type")
text(seq(1.5, 43.5, 6), pcnt14[1,] + 1, cex = .8, labels = pcnt14[1,])
text(seq(2.5, 44.5, 6), pcnt14[2,] + 1, cex = .8, labels = pcnt14[2,])
text(seq(3.5, 45.5, 6), pcnt14[3,] + 1, cex = .8, labels = pcnt14[3,])
text(seq(4.5, 46.5, 6), pcnt14[4,] + 1, cex = .8, labels = pcnt14[4,])
text(seq(5.5, 47.5, 6), pcnt14[5,] + 1, cex = .8, labels = pcnt14[5,])
legend("topleft", inset = .05, c("book reading (br)", "experimental task (et)",
  "letter writing (lw)", "meal time (mt)", "toy play (tp)"), horiz = F,
  fill = c("red", "grey 40", "blue", "green", "grey80"))
grid()
box()
dev.off()
###############################################################
###                       MOT TTR INT
###############################################################
# plot type-token-ratio of intensifiers against age
motint15 <- motint[motint$absint == 1, ]
# tabulate data
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motint15 <- join(motint15, ag, by = "file", type = "left", match = "all")
motint15 <- motint15[, c(1:13, 15)]
inttb15 <- table(motint15$prelex, motint15$agecat)
# calculate token freq
tokf15 <- colSums(inttb15)
# calculate type freq
typf15 <- t(apply(inttb15, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf15 <- colSums(typf15)
# calculate type-token-ratio
motttr15 <- data.frame(3:10, round(typf15/tokf15, 2), typf15, tokf15)
motttr15 <- na.omit(motttr15)
colnames(motttr15) <- c("Age", "TTR", "Types", "Tokens")
# plot data
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/ttrmotageint.png") # save plot
plot(motttr15[, 1:2], xlab = "Age of Child", ylab = "Type-Token-Ratio (TTR): Intensifiers",
  ylim = c(0,1), pch = 19,
  main = "Mother's TTRs of intensifiers\nby age of child")
abline(lm(motttr15[,2]~motttr15[,1]), col="red") # regression line (y~x)
lws15 <- unlist(lowess(motttr15[,2],motttr15[,1]))
lines(lws15[1:length(lws15)/2], col="blue")
grid()
dev.off()
###############################################################
###                       MOT TTR ADJ
###############################################################
motadj16 <- motint[motint$absint == 1, ]
# tabulate data
ag <- chint[,c(1, 14)]
ag <- unique(ag)
motadj16 <- join(motadj16, ag, by = "file", type = "left", match = "all")
motadj16 <- motadj16[, c(1:13, 15)]
adjtb16 <- table(motadj16$tokenlex, motadj16$agecat)
# calculate token freq
tokf16 <- colSums(adjtb16)
# calculate type freq
typf16 <- t(apply(adjtb16, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf16 <- colSums(typf16)
# calculate type-token-ratio
motttr16 <- data.frame(3:10, round(typf16/tokf16, 2), typf16, tokf16)
motttr16 <- na.omit(motttr16)
colnames(motttr16) <- c("Age", "TTR", "Types", "Tokens")
# plot data
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/ttrmotageadj.png") # save plot
plot(motttr16[, 1:2], xlab = "Age of Child", ylab = "Type-Token-Ratio (TTR): Intensifiers",
  ylim = c(0,1),pch = 19,
  main = "Mother's Type-Token-Ratios\nof intensified adjectives against the age of her child")
abline(lm(motttr16[,2]~motttr16[,1]), col="red") # regression line (y~x)
lws16 <- unlist(lowess(motttr16[,2],motttr16[,1]))
lines(lws16[1:length(lws16)/2], col="blue")
grid()
dev.off()
###############################################################
###############################################################
###############################################################
###                         SBC
###############################################################
###############################################################
###############################################################
# read in data
biosbc <- read.table("C:\\MeineHomepage\\docs\\data/BiodataSbcae.txt", sep = "\t", header=TRUE)
# set parameters
context <- 80
all.pre = T
# define search strings
search.pattern <-  c("[A-Z]{0,1}[a-z]{0,}-{0,1}[a-z]{2,}\\/JJ")
##########################################################
# start searches
sbcint <- ConcR("C:\\Corpora\\edited\\SBCPosTagged", search.pattern, context, all.pre = T)
# retrieve speaker
sbcint$all.pre <- gsub("([A-Z]{2,20}[0-9{0,1}]:)", "qwertz\\1", sbcint$all.pre)
sbcint$all.pre <- gsub(".*qwertz", "", sbcint$all.pre)
sbcint$all.pre <- gsub(":.*", "", sbcint$all.pre)
sbcint$all.pre <- tolower(sbcint$all.pre)
# clean column names
colnames(sbcint)[5] <- "spk"
colnames(biosbc)[3] <-  "file"
colnames(biosbc)[4] <-  "spk"
# clean file name
sbcint$file <- gsub(".txt", "", sbcint$file, fixed = T)
sbcint$file <- tolower(sbcint$file)
# join data sets
sbc <- join(sbcint, biosbc, by = c("file", "spk"), type = "left")
# select only female speakers
sbc <- sbc[sbc$gender == "female",]
# code status
sbc$status <- as.vector(unlist(sapply(sbc$post, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(" .*", "", x)
  x <- gsub(".*\\/", "", x)
  x <- gsub("NNS", "NN", x)
  x <- ifelse(x == "NN", "attr", "pred") } )))
# code previous element (pos)
sbc$prepos <- as.vector(unlist(sapply(sbc$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".*\\/", "", x)
  x <- gsub(" .*", "", x) } )))
# code previous element (lexeme)
sbc$prelex <- as.vector(unlist(sapply(sbc$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".* ", "", x)
  x <- gsub("\\/.*", "", x) } )))
# extract all attributive adjectives
sbc <- sbc[sbc$status == "attr",]
# code intensifiers
sbc$prelex <- tolower(sbc$prelex)
sbc$absint <- as.vector(unlist(sapply(sbc$prelex, function(x){
  x <- ifelse(x == "actually", 1,
    ifelse(x == "crass", 1,
    ifelse(x == "damn", 1,
    ifelse(x == "evidently", 1,
    ifelse(x == "particularly", 1,
    ifelse(x == "pretty", 1,
    ifelse(x == "real", 1,
    ifelse(x == "really", 1,
    ifelse(x == "right", 1,
    ifelse(x == "so", 1,
    ifelse(x == "too", 1,
    ifelse(x == "very", 1, 0)))))))))))) } )))
# select only women between 20 and 45
sbc <- sbc[sbc$age.exact >= 20, ]
sbc <- sbc[sbc$age.exact <= 45, ]
# inspect data
head(sbc)

##########################################################
###                     FREQ INT ALL
##########################################################
# plot frequency of intensification by child, mothers, and female sbc speakers
# tabulate data
cmp <- cbind(table(chint$absint), table(motint$absint), table(sbc$absint))
cmp <- rbind(cmp, round(cmp[2,]/(cmp[1,]+cmp[2,])*100,2))
colnames(cmp) <- c("Child", "Mother (CDS)", "Female (SBC)")
# save plot
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/cmp-all.png") # save plot
# plot data
barplot(cmp[3,], col = c("grey 40", "grey 60", "grey80"), ylim = c(0, 8),
  ylab = "Percent (intensified slots of all slots)",
  xlab = "", axes = T, beside = T,
  main = "Intensifier use across corpora\nin informal settings")
text(seq(0.7, 3.1, 1.2), cmp[3,] + .5, cex = .8, labels = cmp[3,])
legend("topleft", inset = .05, c("Children", "Mothers (CDS)", "Female SBC speakers"),
  horiz = F,  fill = c("grey 40", "grey 60", "grey80"))
grid()
box()
dev.off()
##########################################################
###                     FREQ INT MT
##########################################################
# plot frequency of intensification by child, mothers, and female sbc speakers
# tabulate data
chintmt <- chint[chint$styp == "mt",]
motintmt <- motint[motint$styp == "mt", ]
cmp <- cbind(table(chintmt$absint), table(motintmt$absint), table(sbc$absint))
cmp <- rbind(cmp, round(cmp[2,]/(cmp[1,]+cmp[2,])*100,2))
colnames(cmp) <- c("Child", "Mother (CDS)", "Female (SBC)")
# save plot
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/cmp-mt.png") # save plot
# plot data
barplot(cmp[3,], col = c("grey 40", "grey 60", "grey80"), ylim = c(0, 8),
  ylab = "Percent (intensified slots of all slots)",
  xlab = "", axes = T, beside = T,
  main = "Intensifier use across corpora\nin informal settings")
text(seq(0.7, 3.1, 1.2), cmp[3,] + .5, cex = .8, labels = cmp[3,])
legend("topleft", inset = .05, c("Children", "Mothers (CDS)", "Female SBC speakers"),
  horiz = F,  fill = c("grey 40", "grey 60", "grey80"))
grid()
box()
dev.off()
###############################################################
###############################################################
###############################################################
### --- tabulate data
###############################################################
###############################################################
###############################################################
# create data for tabularizaion
wcmot <- by(motint$wc, list(motint$file), unique)
motwc <- as.vector(unlist(wcmot))
motfile <- as.vector(unlist(names(wcmot)))
mdat <- data.frame(1:length(motfile), motfile, motwc)
motadj <- as.vector(unlist(table(motint$file)))
motabsint <- as.vector(unlist(by(motint$absint, list(motint$file), sum)))
motrat <- round(motabsint/motadj, 2)
###
motint$awful <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "awful", 1, 0) } )))
motint$awfully <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "awfully", 1, 0) } )))
motint$pretty <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "pretty", 1, 0) } )))
motint$real <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "real", 1, 0) } )))
motint$really <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "really", 1, 0) } )))
motint$so <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "so", 1, 0) } )))
motint$too <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "too", 1, 0) } )))
motint$very <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "very", 1, 0) } )))
motint$wonderfully <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "wonderfully", 1, 0) } )))
###
motawful <- as.vector(unlist(by(motint$awful, list(motint$file), sum)))
motawfully <- as.vector(unlist(by(motint$awfully, list(motint$file), sum)))
motpretty <- as.vector(unlist(by(motint$pretty, list(motint$file), sum)))
motreal <- as.vector(unlist(by(motint$real, list(motint$file), sum)))
motreally <- as.vector(unlist(by(motint$really, list(motint$file), sum)))
motso <- as.vector(unlist(by(motint$so, list(motint$file), sum)))
mottoo <- as.vector(unlist(by(motint$too, list(motint$file), sum)))
motvery <- as.vector(unlist(by(motint$very, list(motint$file), sum)))
motwonderfully <- as.vector(unlist(by(motint$wonderfully, list(motint$file), sum)))
# creat data frame
mdat <- data.frame(1:length(motfile), motfile, motwc, motabsint, motadj, motrat, motawful,
  motawfully, motpretty, motreal, motreally, motso, mottoo, motvery, motwonderfully)
# clean column names
colnames(mdat)[1:2] <- c("id", "file")
# inspect data
head(mdat)

###############################################################
# join child and mother data
data2 <- join(chint, mdat, by = "file", type = "left", match = "all")
# add child column
data2$chi <- gsub(".*\\/", "", data2$file)
data2$chi <- gsub("[0-9]", "", data2$chi)
data2$chi <- gsub("[a-z]{2,2}$", "", data2$chi)
# inspect data
head(data2)

###############################################################
# extract file based data
filechiwcagesex <- data.frame(data2$file, data2$wc, data2$agecat, data2$gender)
chiwcagesex <- unique(filechiwcagesex)
colnames(chiwcagesex) <- c("file", "chiwc", "age", "sex")
###
filemotwc <- data.frame(data2$file, data2$motwc, data2$motabsint)
motwc <- unique(filemotwc)
colnames(motwc) <- c("file", "motwc", "motabsint")
###
absint <- as.vector(unlist(by(data2$absint, list(data2$file), sum)))
fileabsint <- names(by(data2$absint, list(data2$file), sum))
fileabsint <- data.frame(fileabsint, absint)
colnames(fileabsint) <- c("file", "absint")
# combine information
data3 <- join(chiwcagesex, motwc, by = "file", type = "left", match = "all")
data4 <- join(data3, fileabsint, by = "file", type = "left", match = "all")
# add child column
data4$chi <- gsub(".*\\/", "", data4$file)
data4$chi <- gsub("[0-9]", "", data4$chi)
data4$chi <- gsub("[a-z]{2,2}$", "", data4$chi)
# add styp column
data4$styp <- gsub("HV[0-9]\\/", "", data4$file)
data4$styp <- gsub("\\/.*", "", data4$styp)
data4$styp <- tolower(data4$styp)
# remove rows with incomplete information
data4 <- na.omit(data4)
# inspect data
head(data4)

write.table(data4, "C:\\03-MyProjects\\07IntensifierAcquisition/data4.txt", row.names = F, sep="\t")
###############################################################
# create agecat based data set
info <- by(data4$chi, list(data4$styp, data4$sex, data4$age), length)
nspk <-  as.vector(unlist(by(data4$chi, list(data4$styp, data4$sex, data4$age), length)))
chiwc <- as.vector(unlist(by(data4$chiwc, list(data4$styp, data4$sex, data4$age), sum)))
absint <- as.vector(unlist(by(data4$absint, list(data4$styp, data4$sex, data4$age), sum)))
motabsint <- as.vector(unlist(by(data4$motabsint, list(data4$styp, data4$sex, data4$age), sum)))
motwc <-  as.vector(unlist(by(data4$motwc, list(data4$styp, data4$sex, data4$age), sum)))
chiptw <- round(absint/chiwc*1000, 2)
motptw <- round(motabsint/motwc*1000, 2)
###
styp <- rep(c("br", "et", "lw", "mt", "tp"), 16)
sex <- rep(c(rep("female", 5), rep("male", 5)), 8)
age <- c(rep(3, 10), rep(4, 10), rep(5, 10),rep(6, 10),rep(7, 10),rep(8, 10),rep(9, 10), rep(10, 10))
#combine data to table
df1 <- data.frame(age, sex, styp, nspk, absint, chiwc, chiptw, motabsint, motwc, motptw)
# remove incolplete rows
df1 <- na.omit(df1)
# inspect results
head(df1)

write.table(df1, "C:\\03-MyProjects\\07IntensifierAcquisition/df1.txt", row.names = F, sep="\t")
###############################################################
# create agecat based data set
info2 <- by(data4$chi, list(data4$sex, data4$age), length)
nspk2 <-  by(data4$chi, list(data4$sex, data4$age), table)
nspk2 <- sapply(nspk2, function(x){ x <- length(x) } )
chiwc2 <- as.vector(unlist(by(data4$chiwc, list(data4$sex, data4$age), sum)))
absint2 <- as.vector(unlist(by(data4$absint, list(data4$sex, data4$age), sum)))
motabsint2 <- as.vector(unlist(by(data4$motabsint, list(data4$sex, data4$age), sum)))
motwc2 <-  as.vector(unlist(by(data4$motwc, list(data4$sex, data4$age), sum)))
chiptw2 <- round(absint2/chiwc2*1000, 2)
motptw2 <- round(motabsint2/motwc2*1000, 2)
###
sex2 <- rep(c("female", "male"), 8)
age2 <- c(rep(3, 2), rep(4, 2), rep(5, 2),rep(6, 2),rep(7, 2),rep(8, 2),rep(9, 2), rep(10, 2))
#combine data to table
df2 <- data.frame(age2, sex2, nspk2, absint2, chiwc2, chiptw2, motabsint2, motwc2, motptw2)
# remove incolplete rows
sumspk2 <- as.vector(unlist(length(table(data4$chi))))
sumabsint2 <- as.vector(unlist(sum(data4$absint)))
sumchiwc2 <- sum(chiwc2)
sumchiptw2 <- round(sumabsint2/sumchiwc2*1000, 2)
summotabsint2 <- sum(motabsint2)
summotwc2 <- sum(motwc2)
summotptw2 <- round(summotabsint2/summotwc2*1000, 2)
total <- c("", "", sumspk2, sumabsint2, sumchiwc2, sumchiptw2, summotabsint2, summotwc2, summotptw2)
# add to table
df2 <- rbind(df2, total)
# inspect results
head(df2)

write.table(df2, "C:\\03-MyProjects\\07IntensifierAcquisition/df2.txt", row.names = F, sep="\t")
###############################################################
###############################################################
###############################################################
# create agecat based data set
nspk0 <-  length(names(table(data4$chi)))
chiwc0 <- as.vector(unlist(sum(data4$chiwc)))
absint0 <- as.vector(unlist(sum(data4$absint)))
motabsint0 <- as.vector(unlist(sum(data4$motabsint)))
motwc0 <-  as.vector(unlist(sum(data4$motwc)))
chiptw0 <- round(absint0/chiwc0*1000, 2)
motptw0 <- round(motabsint0/motwc0*1000, 2)
#combine data to table
df0 <- cbind(nspk0, absint0, chiwc0, chiptw0)
df0 <- rbind(df0, c("", motabsint0, motwc0, motptw0))
# add row and column names
colnames(df0) <- c("Speakers (N)", "Intensifiers (N)", "Words (N)", "Rel. Freq. (ptw)")
rownames(df0) <- c("Children", "Mothers")
# inspect results
head(df0)

write.table(df0, "C:\\03-MyProjects\\07IntensifierAcquisition/df0.txt", row.names = F, sep="\t")
###############################################################
# add ptw to data4
data4$intptw <- round(data4$absint/data4$chiwc*1000, 2)
# plot genders (scatterplot)
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagegenderptw.png") # save plot
scatterplot(intptw ~ as.numeric(age)|sex, data=data4, reg.line = FALSE, xlab = "Age",
  ylab = "Rel. Frequency", axes = F)
axis(1, at= 1:8, labels = 3:10)
axis(2, at = seq(0, 25, 5), labels = seq(0, 25, 5))
grid()
dev.off()
# plot styp (scatterplot)
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intagestypptw.png") # save plot
scatterplot(intptw ~ as.numeric(age)|styp, data=data4, reg.line = FALSE, xlab = "Age",
  ylab = "Rel. Frequency", axes = F)
axis(1, at= 1:8, labels = 3:10)
axis(2, at = seq(0, 25, 5), labels = seq(0, 25, 5))
grid()
dev.off()
###############################################################
###############################################################
###############################################################
# analogous plot in ggplot2
agemeans <- as.vector(by(data4$intptw, data4$age, mean))
agemeans <- agemeans[complete.cases(agemeans)]
p0 <- ggplot(data4, aes(age, intptw, color = age)) +
  scale_fill_brewer() +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 20)) +
  coord_cartesian(ylim = c(0, 5)) +
  labs(x = "Age", y = "Rel. Freq. (per 1,000 words)", colour = "age", cex = .75) +
  scale_color_manual(values = c("grey20", "grey25", "grey30", "grey35", "grey40", "grey45", "grey50", "grey55", "grey60")) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[1], 2), sep = ""), x = 1, y = agemeans[1] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[2], 2), sep = ""), x = 2, y = agemeans[2] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[3], 2), sep = ""), x = 3, y = agemeans[3] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[4], 2), sep = ""), x = 4, y = agemeans[4] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[5], 2), sep = ""), x = 5, y = agemeans[5] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[6], 2), sep = ""), x = 6, y = agemeans[6] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[7], 2), sep = ""), x = 7, y = agemeans[7] + 2, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[8], 2), sep = ""), x = 8, y = agemeans[8] + 2, colour = "grey20", size = 5) +
#  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[9], 2), sep = ""), x = 9, y = agemeans[9] + 2, colour = "grey20", size = 5) +
  theme(legend.position="none")
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intageggplot.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p0

###
p1 <- ggplot(data4, aes(age, intptw, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = sex)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 7.5)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: per 1,000 words)", colour = "sex") +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "lightblue"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intAgeSexggplot.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p1

###
p2 <- ggplot(data4, aes(age, intptw, colour = styp)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = styp)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ styp, nrow = 1) +
  coord_cartesian(ylim = c(0, 7.5)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: per 1,000 words)", colour = "styp") +
  theme(legend.position="none") +
  scale_color_manual(values = c("grey30", "grey30", "grey30", "grey30", "grey30", "grey30", "grey30"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intAgeStypggplot.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p2

###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intAgeGenderStyp.png") # save plot
par(mfrow=c(3,2))
brp3 <- data[data$styp == "br", ]
etp3 <- data[data$styp == "et", ]
lwp3 <- data[data$styp == "lw", ]
mtp3 <- data[data$styp == "mt", ]
tpp3 <- data[data$styp == "tp", ]
# brp3
absintsexagebr <- by(brp3$absint, list(brp3$agecat, brp3$gender), mean)
female <- round(as.vector(absintsexagebr)[1:8], 2)
male <- round(as.vector(absintsexagebr)[9:16], 2)
plot(female, type = "o", lwd = 2, lty = 1, pch = 19, ylim = c(0, .2), main = "book reading",
  ylab = "Relative Frequency", xlab = "Age", axes = F, cex = 1, col = "red")
lines(male, type = "o", lwd = 2,  lty = 2, pch = 0,  cex = 1, col = "blue")
# add x-axes with specified labels at specified intervals
axis(1, at = 1:8, lab = 3:10, cex = 2)
axis(2, at = seq(0, .2, .05), las = 1, lab = seq(0, .2, .05))
linetype <- c(1, 2)
plotchar <- c(19, 0)
legend("topleft", inset = .05, c("female", "male"),
  horiz = F,  pch = plotchar, lty = linetype, col = c("red", "blue"))
box()
grid()
# etp3
absintsexageet <- by(etp3$absint, list(etp3$agecat, etp3$gender), mean)
female <- round(as.vector(absintsexageet)[1:8], 2)
male <- round(as.vector(absintsexageet)[9:16], 2)
plot(female, type = "o", lwd = 2, lty = 1, pch = 19, ylim = c(0, .2), main = "experimental task",
  ylab = "", xlab = "Age", axes = F, cex = 1, col = "red")
lines(male, type = "o", lwd = 2,  lty = 2, pch = 0,  cex = 1, col = "blue")
# add x-axes with specified labels at specified intervals
axis(1, at = 1:8, lab = 3:10)
axis(2, at = seq(0, .2, .05), las = 1, lab = seq(0, .2, .05))
linetype <- c(1, 2)
plotchar <- c(19, 0)
legend("topleft", inset = .05, c("female", "male"),
  horiz = F,  pch = plotchar, lty = linetype, col = c("red", "blue"))
box()
grid()
# lwp3
absintagelw <- by(lwp3$absint, list(lwp3$agecat, lwp3$gender), mean)
female <- round(as.vector(absintagelw)[1:8], 2)
male <- round(as.vector(absintagelw)[9:16], 2)
plot(female, type = "o", lwd = 2, lty = 1, pch = 19, ylim = c(0, .2), main = "letter writing",
  ylab = "Relative Frequency", xlab = "Age", axes = F, cex = 1, col = "red")
lines(male, type = "o", lwd = 2,  lty = 2, pch = 0,  cex = 1, col = "blue")
# add x-axes with specified labels at specified intervals
axis(1, at = 1:8, lab = 3:10)
axis(2, at = seq(0, .2, .05), las = 1, lab = seq(0, .2, .05))
linetype <- c(1, 2)
plotchar <- c(19, 0)
legend("topleft", inset = .05, c("female", "male"),
  horiz = F,  pch = plotchar, lty = linetype, col = c("red", "blue"))
box()
grid()
# mtp3
absintsexragemt <- by(mtp3$absint, list(mtp3$agecat, mtp3$gender), mean)
female <- round(as.vector(absintsexragemt)[1:8], 2)
male <- round(as.vector(absintsexragemt)[9:16], 2)
plot(female, type = "o", lwd = 2, lty = 1, pch = 19, ylim = c(0, .2), main = "meal time",
  ylab = "Relative Frequency", xlab = "Age", axes = F, cex = 1, col = "red")
lines(male, type = "o", lwd = 2,  lty = 2, pch = 0,  cex = 1, col = "blue")
# add x-axes with specified labels at specified intervals
axis(1, at = 1:8, lab = 3:10)
axis(2, at = seq(0, .2, .05), las = 1, lab = seq(0, .2, .05))
linetype <- c(1, 2)
plotchar <- c(19, 0)
legend("topleft", inset = .05, c("female", "male"),
  horiz = F,  pch = plotchar, lty = linetype, col = c("red", "blue"))
box()
grid()
# tpp3
absintsexragetp <- by(tpp3$absint, list(tpp3$agecat, tpp3$gender), mean)
female <- round(as.vector(absintsexragetp)[1:8], 2)
male <- round(as.vector(absintsexragetp)[9:16], 2)
plot(female, type = "o", lwd = 2, lty = 1, pch = 19, ylim = c(0, .2), main = "toy play",
  ylab = "", xlab = "Age", axes = F, cex = 1, col = "red")
lines(male, type = "o", lwd = 2,  lty = 2, pch = 0,  cex = 1, col = "blue")
# add x-axes with specified labels at specified intervals
axis(1, at = 1:8, lab = 3:10)
axis(2, at = seq(0, .2, .05), las = 1, lab = seq(0, .2, .05))
linetype <- c(1, 2)
plotchar <- c(19, 0)
legend("topleft", inset = .05, c("female", "male"),
  horiz = F,  pch = plotchar, lty = linetype, col = c("red", "blue"))
box()
grid()
par(mfrow=c(1,1))
dev.off()
###############################################################
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intGenderStyp.png") # save plot
absintagestyp <- by(data$absint, list(data$styp, data$gender), mean)
st <- c("br", "et", "lw", "mt", "tp")
female <- absintagestyp[1:5]
male <- absintagestyp[6:10]
absintagestyptb <- rbind(female, male)
colnames(absintagestyptb) <- st
rownames(absintagestyptb) <- c("female", "male")
x <- barplot(absintagestyptb, main="", ylim = c(0, 0.2), ylab = "Probabilty of Intensification",
  xlab="Situation Type", col=c("lightgrey", "darkgrey"), beside=TRUE)
legend("topright", inset = .05, c("female", "male"), #title="Gender of child",
  horiz = F,  fill = c("lightgrey", "darkgrey"))
legend("topleft", inset=.05, title="Situation Types",
  c("br: book reading", "et: experimental task ", "lw: letter writing", "mt: meal time", "tp: toy play"),
  horiz=F)
text(c(1.5, 4.5, 7.5, 10.5, 13.5, 2.5, 5.5, 8.5, 11.5, 14.5), absintagestyp + .005, cex = 1, labels = round(absintagestyp, 2))
grid()
box()
dev.off()
###############################################################
p4 <- ggplot(data, aes(styp, absint, colour = gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  coord_cartesian(ylim = c(0, .2)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Age", y = "Frequency (Mean: Ratio of Intensified Slots)", colour = "styp") +
  theme(legend.position="none") +
  scale_color_manual(values = c("red", "blue"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intGenderStyp.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p4

###############################################################
wcstyp <- aggregate(absint ~ styp, data=data, FUN=mean)
wcstyp <- wcstyp[order(wcstyp$absint, decreasing = T),]
wcstyp[,1] <- c("br", "et", "lw", "mt", "tpbr")
pct <- round(wcstyp[, 2]/ sum(wcstyp[, 2])*100, 2)
png("C:\\03-MyProjects\\07IntensifierAcquisition\\Article\\images/intptwstyprecoded.png") # save plot
#dev.new(width=9, height=9)
barplot(wcstyp[, 2], ylim = c(0, .2), ylab = "Ratio of Intensified Slots", xlab = "Situation Type",
  col = c("grey50", "grey65", "grey80", "grey95"))
axis(1, at=seq(0.7, 5.5, 1.2), labels=wcstyp[, 1])
legend("topright", inset=.05, title="Situation Types",
   c("book reading (br)", "experimental task (et)", "letter writing (lw)", "meal time (mt)", "toy play (tp)"),
   fill=c("grey50", "grey65", "grey80", "grey95"), horiz=F)
box()
grid()
text(seq(0.7, 5.5, 1.2), wcstyp[, 2]+.005, round(wcstyp[, 2], 2))
dev.off()
###############################################################
###############################################################
###############################################################
### --- statz
###############################################################
###############################################################
###############################################################
# remove all incomplete cases from data set
data2 <- data2[complete.cases(data2),]
################################################################
### ---        Model Building - GLMEBR
################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
data.dist <- datadist(data2)
options(datadist = "data.dist")
data2$agecat <- as.numeric(data2$agecat)
data2$agecat <- as.vector(unlist(sapply(data2$agecat, function(x){
  x <- ifelse(x == 8, 10,
    ifelse(x == 7, 9,
    ifelse(x == 6, 8,
    ifelse(x == 5, 7,
    ifelse(x == 4, 6,
    ifelse(x == 3, 5,
    ifelse(x == 2, 4,
    ifelse(x == 1, 3, x)))))))) } )))
# a few words on glm vs lrm: Baayen (2008:196-197) states that lrm should be
# the function of choice in cases where each row contains
# exactly 1 success OR failure (1 or 0) while glm is preferrable if there are two
# columns holding the number of successes and the number of failures
# respectively. i have tried it both ways and both functions work fine if
# each row contains exactly 1 success OR failure but only glm can handle the
# latter case.
# generate initial saturated regression model including
# all variables and their interactions
m0.glm = glm(absint ~ 1, family = binomial, data = data2) # baseline model glm
m0.lrm = lrm(absint ~ 1, data = data2, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# create model with a random intercept for childid
m0.lmer <- lmer(absint ~ (1|chi), data = data2, family = binomial) # do not use: deprecated!
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(absint ~ (1|chi), data = data2, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glm object is smaller which shows that including the random
# intercepts is NOT justified
###
# testing whether random intercepts are justified using a intlihood ratio test
lrt <- function (obj1, obj2) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = L01, df = df,
  "p-value" = pchisq(L01, df, lower.tail = FALSE))
  }
lrt(obj1=m0.glm, obj2=m0.glmer)

# the test confirms that including random intercepts is NOT justified
# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up, i.e. forward, procedure although with step-wise step-up
# the intlyhood of type II errors is much higher than with step-wise
# step down, i.e. backward elimination(cf. Field, Miles & Field 2012:265)
#	automated model fitting
m1.glm <- glm(absint ~ agecat, family = binomial, data = data2)
m2.glm <- glm(absint ~ agecat + gender, family = binomial, data = data2)
m3.glm <- glm(absint ~ agecat + gender + motrat, family = binomial, data = data2)
m4.glm <- glm(absint ~ agecat + gender + motrat + styp, family = binomial, data = data2)
m5.glm <- glm(absint ~ agecat + gender + motrat + styp + agecat : gender, family = binomial, data = data2)
m6.glm <- glm(absint ~ agecat + gender + motrat + styp + agecat : gender + agecat : motrat, family = binomial, data = data2)
m7.glm <- glm(absint ~ agecat + gender + motrat + styp + agecat : gender + agecat : motrat + agecat : styp, family = binomial, data = data2)
# we compare all models because this way, we get an overview of model paramerets
# and can check which model has the lowerst AIC, BIC, and the highest X^2 value
anova(m0.glm, m1.glm, m2.glm, m3.glm, m4.glm, m5.glm, m6.glm, m7.glm, test = "Chi")

# use customized model comparison function
# create comparisons
m1.m0 <- anova(m1.glm, m0.glm, test = "Chi")
m2.m1 <- anova(m2.glm, m1.glm, test = "Chi")
m3.m2 <- anova(m3.glm, m2.glm, test = "Chi")
m4.m3 <- anova(m4.glm, m3.glm, test = "Chi")
m5.m4 <- anova(m5.glm, m4.glm, test = "Chi")
m6.m5 <- anova(m6.glm, m5.glm, test = "Chi")
m7.m6 <- anova(m7.glm, m6.glm, test = "Chi")
# model 5: m4.glm is our final model
# create a list of the model comparisons
mdlcmp <- list(m1.m0, m2.m1, m3.m2, m4.m3, m5.m4, m6.m5, m7.m6)
# apply function
mdl.cmp.glm.swsu <- mdl.fttng.glm.swsd(mdlcmp)
# inspect output
mdl.cmp.glm.swsu

write.table(mdl.cmp.glm.swsu, "C:\\03-MyProjects\\07IntensifierAcquisition\\Article/mdl_cmp_glm_swsd.txt", sep="\t")
###########################################################################
mfin.glm <- glm(absint ~ agecat + motrat + styp, family = binomial, data = data2)
# rename final minimal adeqaute model
mlr.glm <- mfin.glm

# test if the final minimal adequate model performs better than the base-line model
anova(mlr.glm, m0.glm, test = "Chi")

# inspect results of the final minimal adequate model
print(mlr.glm, corr = F)

# alternative result display (anova)
anova(mlr.glm)

# alternative result display (anova)
anovasummary <- Anova(mlr.glm, type = "III", test = "Wald")

# extract the parameters of the fixed effects for the report
# to do that, we compare the model with only the random effect to a model with
# the random effect and the fixed effect for age
age.effect.glm <- anova(m0.glm, m1.glm, test = "Chi") # effect of age

# we test the effect of motrat, by adding stype and compare a
# model with motrat to a model without motrat
motrat.effect.glm <- anova(m2.glm, m3.glm, test = "Chi") # effect of motrat

# we test the effect of styp, by adding stype and compare a
# model with styp to a model without styp
styp.effect.glm <- anova(m4.glm, m3.glm, test = "Chi") # effect of styp

###########################################################################
### --- extracting and calculating model fit parameters
# we now create a lmr object equivalent to the final minimal adequate model
# but without the random effect
mlr.lrm <- lrm(absint ~ agecat + motrat + styp, data = data2, x = T, y = T)
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(absint ~ agecat + motrat + styp + (1|chi), data = data2, family = binomial)

# now we check if the fixed effects of the lrm and the lmer model correlate (cf Baayen 2008:281)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))

# the fixed effects correlate very strongly - this is good as it suggests that
# the coefficient estimates are very stable
# calculate prediction accuracy
pred <- as.vector(unlist(sapply(fitted(mlr.glm), function(x){
  x <- ifelse(x > .5, 1, 0) } )))
accuracy <- table(pred, data2$absint)
accuracy <- sum(diag(accuracy))/sum(accuracy)
accuracy <- round(accuracy*100, 2)
# extract summary of final model
blrm.summary <- blrm.summary(mlr.glm, mlr.lrm, accuracy)
blrm.summary

write.table(blrm.summary, "C:\\03-MyProjects\\07IntensifierAcquisition/blrmsummary.txt", sep="\t")
###########################################################################
# we activate the package Hmisc (if not already active)
library(Hmisc)
# we now extract model fit parameters (cf Baayen 2008:281)
probs = 1/(1+exp(-fitted(mlr.glm)))
probs = binomial()$linkinv(fitted(mlr.glm))
somers2(probs, as.numeric(data2$absint))

# the model fit values indicate a very good fit:
# C
# "The measure named C is an index of concordance between the predicted
# probability and the observed response. [...] When C takes the value 0.5, the
# predictions are random, when it is 1, prediction is perfect. A value above
# 0.8 indicates that the model may have some real predictive capacity."
# (Baayen 2008:204)
# "Somers’ Dxy,
# a rank correlation between predicted probabilities and observed responses, [...]
# ranges between 0 (randomness) and 1 (perfect prediction)." (Baayen 2008:204)

# extract Pseudo R^2 values for binomial logistic mixed effects model
my.lmer.nagelkerke(c("absint ~ agecat + motrat + styp", "(1 | childid)"), data)

###########################################################################
# model diagnostics: plot fitted against residuals
plot(mlr.glm)


###############################################################
#inspect the results meblrm.summary(glm0, glm1, glmer0, glmer1, dpvar)
meblrmsc <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, data$intcmsm) #
write.table(meblrmsc, "C:\\03-MyProjects\\07IntensifierAcquisition\\Article/meblrmsc.txt", sep="\t")

###########################################################################
###########################################################################
###########################################################################
### --- EXPLANATION of MODEL FIT PARAMETERS
###########################################################################
###########################################################################
###########################################################################
# R2 (Hosmer & Lemeshow)
# "Rt is the proportional reduction in the absolute value of the log-intlihood
# measure and as such it is a measure of how much the badness of fit improves
# as a result of the inclusionof the predictor variables. It can vary between 0
#(indicating that the predictors are useless at predicting the outcome variable)
# and 1 (indicating that the model predicts the outcome variable perfectly)"
# (Field, Miles & Field 2012:317).
###########################################################################
# R2 (Cox & Snell)
# "Cox and Snell's R~s (1989) is based on the deviance of the model (-2LL(new»)
# and the deviance of the baseline model (-2LL(baseline), and the sample size,
# n [...]. However, this statistic never reaches its theoretical maximum of 1.
###########################################################################
# R2 (Nagelkerke)
# Since R2 (Cox & Snell) never reaches its theoretical maximum of 1,
# Nagelkerke (1991) suggested Nagelkerke's R^2. (Field, Miles & Field 2012:317-318).
###########################################################################
# Somers’ Dxy
# Somers’ Dxy is a rank correlation between predicted probabilities and observed
# responses ranges between 0 (randomness) and 1 (perfect prediction). (Baayen 2008:204)
###########################################################################
# C
# C is an index of concordance between the predicted probability and the
# observed response. When C takes the value 0.5, the predictions are random,
# when it is 1, prediction is perfect. A value above 0.8 indicates that the
# model may have some real predictive capacity. (Baayen 2008:204)
###########################################################################
# AIC
# Akaike information criteria (AlC = -2LL + 2k): "contains more predictor variables.
# You can think of this as the price you pay for something: you get a better
# value of R2, but you pay a higher price, and was that higher price worth it?
# These information criteria help you to decide.model. The BIC is the same as
# the AIC but adjusts the penalty included in the AlC (i.e., 2k) by the number
# of cases: BlC = -2LL + 2k x log(n) in which n is the number of cases in the
# model." (Field, Miles & Field 2012:318).
################################################################
################################################################
################################################################
################################################################
### --- VERY IMPORTANT OBJECTS
################################################################
################################################################
################################################################
# inspect very important objects
head(data)

int.df1

# glmer
mdl.cmp.glmersc.swsu

mdl.cmp.glmersc.swsu.gries

age.effect.glmersc

styp.effect.glmersc

stypegender.effect.glmersc

meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, data$intcmsm)

anovasummary

###############################################################
###############################################################
###############################################################
### ---                   THE END
###############################################################
###############################################################
###############################################################






