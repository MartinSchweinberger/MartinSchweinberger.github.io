##################################################################
### --- R script "Speech-Unit-Final EH in New Zealand English"
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- NOTE
### --- Speakers who do not occur in the corpus but are included
### --- in bioinfo provided by the ICE team are left out of the final
### --- bioinfo spreadsheet!!!
### --- Words uttered by extra corpus speakers (annotation:
### --- start = <X>; end = </X>) are not considered in the word counts.
### --- Abbreviated forms ('ve, 're, 's etc.) are not considered full
### --- words but are regarded as part of the words to which they
### --- are attached, e.g. that's = 1 token/word; I've = 1 token/word)
### --- Each conversation is treated individually, i.e. for a file which
### --- contains several conversations among distinct or even
### --- the same speakers, the word counts for each conversation
### --- will be extracted separately from the other conversations
### --- in that file.
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- CITATION
###############################################################
###############################################################
###                   START
###############################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Install packages
#install.packages("tm")
#install.packages("stringr")
#install.packages("gsubfn")
#install.packages("plyr")
#install.packages("reshape")
#install.packages("zoo")
#install.packages("rms")
#install.packages("glmulti")
#install.packages("lmtest")
#install.packages("MASS")
#install.packages("QuantPsyc")
#install.packages("car")
#install.packages("mlogit")
#install.packages("ggplot2")
#install.packages("cfa")
# Initiate the packages
#library(glmulti)
#library(lmtest)
#library(MASS)
#library(car)
#library(QuantPsyc)
#library(boot)
#library(car)
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
library(ggplot2)
library(cfa)
library(Hmisc)
library(RLRsim)
library(sjPlot)
library(mlogit)
library(ggplot2)
library(effects)
library(lme4)
library(languageR)
#library(nlme)
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
# define image directors
imageDirectory<-"C:\\03-MyProjects\\EHinNZE\\Article\\images"
# Specify pathnames of the corpra
corpus.nz <- "C:\\03-MyProjects\\EHinNZE\\ICE New Zealand\\Spoken"
# Define input pathname of raw biodata
bio.nz <- "C:\\03-MyProjects\\EHinNZE\\ICE New Zealand\\/NZGUIDE.txt"
# Define outputpath of final biodata
#out.nz <- "C:\\PhD\\skripts n data/biodata ice new zealand.txt"
###############################################################
###                   ICE New Zealand
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.nz, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T) }  )
corpus.tmp <- unlist(corpus.tmp)
# Paste all elements of the corpus together
corpus.tmp1 <- paste(corpus.tmp, collapse = " ")
# Clean corpus
corpus.tmp2 <- enc2utf8(corpus.tmp1)
corpus.tmp2 <- gsub(" {2,}", " ", corpus.tmp2)
corpus.tmp2 <- str_replace_all(corpus.tmp2, fixed("\n"), " ")
corpus.tmp2 <- str_trim(corpus.tmp2, side = "both")
###############################################################
# Specify searchpattern
splitpattern1 = "</I>"
# Split corpus
corpus.tmp4 <- strsplit(as.character(corpus.tmp2), splitpattern1)
# Specify search pattern
splitpattern2 = "<I> "
# Splits corpus into parts
corpus.tmp5 <- lapply(corpus.tmp4, function(x) {
  strsplit(as.character(x), splitpattern2) }  )
# Extract file.ids
file.ids.tmp1 <- lapply(corpus.tmp5, function(x) {
  sapply(x, "[[", 2)  }  )
# Clean file ids
file.ids.tmp2 <- lapply(file.ids.tmp1, function(x) {
  x <- str_trim(x)
  x <- gsub("#.*", "", x)
  x <- gsub(".*<", "", x)
  }  )
file.ids <- as.vector(unlist(file.ids.tmp2))
# Extract subfile.ids
subfile.ids.tmp1 <- unlist(file.ids.tmp2)
subfile.ids.tmp2 <- sapply(as.vector(table(subfile.ids.tmp1)), function(x) {
  x <- str_replace_all(x, "2","1 2")
  x <- str_replace_all(x, "3","1 2 3")
  x <- str_replace_all(x, "4","1 2 3 4")
  x <- paste(x, collapse = " ")
  x <- strsplit(as.character(x), " ")
  x <- unlist(x)
  x <- sapply(x, "[", 1)  }  )
subfile.ids <- as.vector(unlist(subfile.ids.tmp2))
# Transform corpus.tmp6 into a data frame
corpus.tmp7 <- as.data.frame(corpus.tmp5)
# Store results in vector
corpus.tmp8 <- corpus.tmp7[2, c(1:length(corpus.tmp7))]
# Convert into character strings
corpus.tmp9 <- as.character(corpus.tmp8)
# Add names to corpus.tmp9
names(corpus.tmp9) <- file.ids
###############################################################
# create a table out of the results
corpus.tmp10 <- as.data.frame(corpus.tmp9)
corpus.tmp11 <-t(corpus.tmp10)
corpus.tmp12 <- as.table(corpus.tmp11)
corpus.table1 <- cbind(file.ids[1:length(file.ids)], corpus.tmp12[,1:length(file.ids)])
# Add id as a column
id <- 1:length(corpus.table1[, 1])
corpus.table2 <- cbind(id, corpus.table1)
corpus.table2 <- cbind(corpus.table2[, 1], corpus.table2[, 2], subfile.ids, corpus.table2[, 3])
# Add column labels
colnames(corpus.table2) <- c("id", "file", "subfile", "corpusfile")
# Add row labels
rownames(corpus.table2) <- c(1:length(corpus.table2[,1]))
corpus.table2 <- as.table(corpus.table2)
###############################################################
### --- STEP
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Clean corpus file
all.files <- lapply(all.files, function(x) {
  x <- sub(".*?<ICE-NZ:", "<ICE-NZ:", x)  }  )
# Split corpus files so that each turn is one element
all.files.unclean <- sapply(all.files, function(x) {
  corpfile2 <- strsplit( gsub("(<ICE-NZ:[A-Z][0-9][A-Z])", "~\\1", x), "~" ) }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)] , 1 , paste , collapse = " " )
names(all.files.unclean) <- file.subfile.ids
###############################################################
# Separate the speakers from the turns
speakers.and.turns <- lapply(all.files.unclean, function(x) {
  x <- str_split(x, " ", n = 2)
  x <- x[2:length(x)]  }  )
# Store speakers in extra vector
speakers <- lapply(speakers.and.turns, function(x) {
  x <- sapply(x, "[[", 1)
  x <- gsub("<ICE-NZ:", "<", x)
  x <- gsub("#[0-9]{1,4}:", "#", x) }  )
# Store speakers in extra vector
speaker.ids <- lapply(speakers.and.turns, function(x) {
  x <- sapply(x, "[[", 1)
  x <- gsub(".*:", "", x)
  x <- gsub(">", "", x) }  )
# Extract the speech unit count for eac h speaker
turn.count <- lapply(speakers, function(x) {
  x <- gsub("<.*>", "1", x)  }  )
# Store turns in extra vector
turns <- lapply(speakers.and.turns, function(x) {
  sapply(x, function(x) x[2])  }  )
###############################################################
# Create a list with all turns but cleaned, i.e. without metas
turns.clean <- lapply(turns, function(x) {
  x <- str_replace_all(x, "(<q.*?/q>)","")
#  x <- gsub("<\\?>.*?</\\?>", " ", x)
  x <- str_replace_all(x, "(<&.*/&.*>)","")
  x <- str_replace_all(x, "(<un.*?clear>)","")
  x <- str_replace_all(x, "(<O>.*?</O>)","")
  x <- str_replace_all(x, "(<[a-z]{4,}.*</[a-z]{4,}>)","")
#  x <- str_replace_all(x, "(<\\..*?/.>)","")
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*</X>)","")
  x <- str_replace_all(x, "(<X>)","")
  x <- str_replace_all(x, "(</X>)","")
# WARNING: THEORETICAL ISSUE
  x <- gsub(" {2,}", " ", x)
  x <- gsub(" re |'re ", "'re ", x)
  x <- gsub(" ll |'ll ", "'ll ", x)
  x <- gsub(" ve |'ve ", "'ve ", x)
  x <- gsub(" s ", "'s ", x)
  x <- gsub(" d ", "'d ", x)
  x <- gsub(" {0,1}I m ", " I'm ", x)
  x <- gsub("ouldnt ", "ouldn't ", x)
  x <- gsub(" cant ", " can't ", x)
  x <- gsub("Cant ", "Can't ", x)
  x <- gsub("Dont ", "Don't ", x)
  x <- gsub(" dont ", " don't ", x)
  x <- gsub("Didnt ", "Didn't ", x)
  x <- gsub(" didnt ", " didn't ", x)
  x <- gsub("Isnt ", "Isn't ", x)
  x <- gsub(" isnt ", " isn't ", x)
  x <- gsub("Arent ", "Aren't ", x)
  x <- gsub(" arent ", " aren't ", x)
  x <- gsub("Wasnt ", "Wasn't ", x)
  x <- gsub(" wasnt ", " wasn't ", x)
  x <- gsub("(<.*?>)", " ", x)
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
###############################################################
### --- Create a list which holds the number of words per speech unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(turns.clean, function(x){
  tokenized <- strsplit(x, " ")  }  )
# Now, we count the words elements(words) in each turn (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))  } )
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, turn, turn.clean,
# turn.count, word.count)
###############################################################
speaker.and.unclean.turns <- mapply(cbind, speakers[], turns[], SIMPLIFY = F)
speaker.both.turns <- mapply(cbind, speaker.and.unclean.turns[], turns.clean[], SIMPLIFY = F)
names(speaker.both.turns) <- file.subfile.ids
# Add file.subfile.ids to speaker.both.turns
speaker.both.turns.subfile <- mapply(cbind, speaker.both.turns[],names(speaker.both.turns[]), SIMPLIFY = F)
# Add turn.counts
speaker.both.turns.subfile.and.turn.count <- mapply(cbind, speaker.both.turns.subfile [], turn.count[], SIMPLIFY = F)
speakerinfo1 <- mapply(cbind, speaker.both.turns.subfile.and.turn.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# We now need to convert the elements of the fourth and fifth column into numeric elements
speakerinfo2 <-lapply(speakerinfo1, function(x) {
  X <- as.data.frame(x[])
  X[, 5] <- as.numeric(X[, 5])
  X[, 6] <- as.numeric(X[, 6])
  x <- X }  )
###############################################################
# Rename data for later kwic seraches
kwic.tb.ice.nz <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum))) } )
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# Extract the turn counts for speakers in one file
turn.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum))) } )
# Simplify the results
overview.turn.count.results <- sapply(turn.count.result, "[[", 1)
###############################################################
# We now want to extract the full speaker ids in vector format
speaker.id.list <- lapply(speakerinfo2, function(x) {
  x <- x[, 1]
  x <- names(table(x))   }  )
speakers.full.ids <- as.vector(unlist(speaker.id.list))
###############################################################
# We now want to extract the speaker ids in vector format so
# that we can easily create a table out of the results
speaker.id.list <- lapply(speakerinfo2, function(x) {
  x <- x[, 1]
  x <- names(table(x))
  x <- gsub(".*:", "", x)
  x <- gsub(">", "", x)  }  )
speaker.ids <- as.vector(unlist(speaker.id.list))
###############################################################
# We now want to extract the subfile ids in vector format so
# that we can easily create a table out of the results
# First we determine how many speakers are in a file
subfile.list <- lapply(speakerinfo2, function(x) {
  x <- x[, 1]
  x <- names(table(x))
  x <- gsub(".*#", "", x)
  x <- gsub(":.*", "", x) }  )
subfiles <- as.vector(unlist(subfile.list))
##############################################################
# From the speaker.ids vector, we also extract the file names
file.list <- lapply(speakerinfo2, function(x) {
  x <- x[, 1]
  x <- names(table(x))
  x <- gsub("#.*", "", x)
  x <- gsub("<", "", x)  }  )
files <- as.vector(unlist(file.list))
# Now, let’s extract the speaker.ids
speakers <- speaker.ids
###############################################################
# We now want to extract the turn counts for each speaker
# in vector format so that we can easily create a table out of
# the results
turn.count.list <- lapply(X = overview.turn.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) }  )  }  )
# Convert the list into a vector
turn.counts <- as.vector(unlist(turn.count.list))
###############################################################
# We now want to extract the word counts for each speaker in vector format so that we can easily create a table out of the results
word.count.list <- lapply(X = overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) } ) }  )
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
###############################################################
# We now want to create a table with speaker id, turn count and word count
# First, we create an index
id <- c(1:length(speaker.ids))
# Now, we set up the data frame
speakerinfo.ice <- cbind(id, speakers.full.ids, files, subfiles, speakers,
  turn.counts, word.counts)
speakerinfo.ice <- as.data.frame(speakerinfo.ice)
colnames(speakerinfo.ice ) <- c("id", "file.speaker.id", "text.id",
  "subfile.id", "spk.ref", "speech.unit.count", "word.count")
# View results (without empty rows)
speakerinfo.ice.1 <- speakerinfo.ice[!speakerinfo.ice[, 2] == "", ]
speakerinfo.ice.1[, 6] <- as.numeric(speakerinfo.ice.1[, 6])
speakerinfo.ice.1[, 7] <- as.numeric(speakerinfo.ice.1[, 7])
speakerinfo.ice.1[, 1] <- 1:length(speakerinfo.ice.1[, 1])
rownames(speakerinfo.ice.1) <- speakerinfo.ice.1[, 1]
# Modify text.ids to match the text.ids from the biodata
speakerinfo.ice.1[, 3] <- gsub("(-)", "", speakerinfo.ice.1[, 3])
################################################################
################################################################
################################################################
### --- STEP
################################################################
################################################################
################################################################
# Load biodata
meta.tmp1 <- scan(file = bio.nz, what = "char", sep = " ", quiet = T)
meta.tmp2 <- unlist(meta.tmp1)
##################################################################
# Paste all elements of the meta data together
meta.tmp3 <- paste(meta.tmp2, collapse = " ")
# Clean corpus
meta.tmp3 <- enc2utf8(meta.tmp3)
meta.tmp3 <- gsub(" {2,}", " ", meta.tmp3)
meta.tmp3 <- str_replace_all(meta.tmp3, fixed("\n"), " ")
meta.tmp3 <- str_replace_all(meta.tmp3, fixed("\t"), " ")
meta.tmp3 <- str_trim(meta.tmp3, side = "both")
meta.tmp3 <- str_replace_all(meta.tmp3, ".*Direct Conversations", "8.1 Direct Conversations ")
###############################################################
# Split corpus
meta.tmp4 <- strsplit( gsub("([A-Z][0-9][A-Z])", "~\\1", meta.tmp3), "~" )
# Remove all superfluous meta
meta.tmp5 <- meta.tmp4[[1]][2:379]
# Split file meta from speaker meta
meta.tmp6 <- strsplit( gsub("(min|mins))", "\\1)~", meta.tmp5), "~" )
# Delete superfluous spaces
meta.tmp7 <- lapply(meta.tmp6, function(x) {
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")   }  )
###############################################################
#REPAIR BROKEN ELEMENTS:
# meta.tmp7 [[159]], meta.tmp7 [[161]], meta.tmp7 [[202]]
# meta.tmp7 [[159]]
meta.tmp7[[159]][2] <- meta.tmp7[[159]][1]
meta.tmp7[[159]][1] <- gsub("S is a Pakeha male aged.*", "", meta.tmp7[[159]][1])
meta.tmp7[[159]][2] <- gsub(".*Prisons, 18 mins DGU011 ", "", meta.tmp7[[159]][2])
# meta.tmp7 [[161]]
meta.tmp7[[161]][2] <- meta.tmp7[[161]][1]
meta.tmp7[[161]][1] <- gsub("S is a Pakeha male.*", "", meta.tmp7[[161]][1])
meta.tmp7[[161]][2] <- gsub(".*Pollution Bill,5 mins DGU013 ", "", meta.tmp7[[161]][2])
# meta.tmp7 [[202]]
meta.tmp7[[202]][2] <- meta.tmp7[[202]][1]
meta.tmp7[[202]][1] <- gsub("A is a Pakeha male 55-59.*", "", meta.tmp7[[202]][1])
meta.tmp7[[202]][2] <- gsub(".*Cup, 5 mins 36 mins ", "", meta.tmp7[[202]][2])
meta.tmp7 <- lapply(meta.tmp7, function(x) {
  x <- gsub("([0-9]\\.[0-9]{1,2} [A-Z][a-z]{1,} .*)", "", x)
  unlist(x)  } )
# meta.tmp7 [[378]][2]
meta.tmp7[[378]][2] <- gsub("9 Spoken Participants In this section.*", "", meta.tmp7[[378]][2])
###############################################################
# Extract the file meta
file.meta.tmp1 <- lapply(meta.tmp7, function(x) {
  x <- x[1]
  unlist(x)  }  )
file.meta.tmp2 <- unlist(file.meta.tmp1)
# Exctract the speaker meta
meta.tmp8 <- lapply(meta.tmp7, function(x) {  x <- x[2]  }  )
# Add names to meta data
names(meta.tmp8) <- file.meta.tmp2
# Extract speakers
spk.tmp1 <- sapply(meta.tmp8, function(x) {
  x <- strsplit( gsub("([A-Z]{1,2} is [a-z]{0,2}|[A-Z],{0,1} [A-Z]{0,1},{0,1} {0,1}[A-Z]{0,1},{0,1} {0,1}[A-Z]{0,1} {0,1}[a-z]{0,3} [A-Z] are [a-z]{0,2})", "~\\1", x), "~" )  }  )
# Exctract the speaker meta
spk.tmp2 <- lapply(spk.tmp1, function(x) { x <- x[2:length(x)]  }  )
#REPAIR BROKEN ELEMENTS
spk.tmp2[[103]][7] <- "X is a female"
spk.tmp2[[103]][8] <- "Y is a female"
spk.tmp2[[105]][9] <- "X is a female"
spk.tmp2[[105]][10] <- "Y is a female"
spk.tmp2[[106]][9] <- "X is a female"
spk.tmp2[[106]][10] <- "Y is a female"
spk.tmp2[[107]][8] <- "W is a female"
spk.tmp2[[107]][9] <- "X is a female"
spk.tmp2[[107]][10] <- "Y is a female"
spk.tmp2[[109]][7] <- "X is a female"
spk.tmp2[[109]][8] <- "Y is a female"
spk.tmp2[[111]][7] <- "X is a male"
spk.tmp2[[111]][8] <- "Y is a male"
spk.tmp2[[112]][11] <- "X is a male"
spk.tmp2[[112]][12] <- "Y is a male"
spk.tmp2[[117]][12] <- "X is a male"
spk.tmp2[[117]][13] <- "Y is a male"
spk.tmp2[[119]][7] <- "X is a female"
spk.tmp2[[119]][8] <- "Y is a female"
spk.tmp2[[180]][2] <- gsub("X refers to an unknown.*", "", spk.tmp2[[180]][2])
spk.tmp2[[180]][3] <- "X is a NA"
spk.tmp2[[184]][6] <- "X is a female"
spk.tmp2[[184]][7] <- "Y is a female"
spk.tmp2[[186]][5] <- "W is a female"
spk.tmp2[[186]][6] <- "V is a female"
spk.tmp2[[186]][7] <- "X is a female"
spk.tmp2[[186]][8] <- "Y is a female"
spk.tmp2[[191]][5] <- "W is a female"
spk.tmp2[[191]][6] <- "X is a male"
spk.tmp2[[191]][7] <- "Y is a male"
spk.tmp2[[203]][2] <- "V is a female"
spk.tmp2[[203]][3] <- "W is a female"
spk.tmp2[[203]][4] <- "X is a female"
spk.tmp2[[203]][5] <- "Y is a female"
spk.tmp2[[213]][2] <- "U is a female"
spk.tmp2[[213]][3] <- "V is a female"
spk.tmp2[[213]][4] <- "W is a female"
spk.tmp2[[213]][5] <- "X is a female"
spk.tmp2[[213]][6] <- "Y is a female"
spk.tmp2[[240]][2] <- "T is a male"
spk.tmp2[[240]][3] <- "U is a male"
spk.tmp2[[240]][4] <- "V is a female"
spk.tmp2[[240]][5] <- "W is a male"
spk.tmp2[[240]][6] <- "X is a female"
spk.tmp2[[240]][7] <- "Y is a female"
spk.tmp2[[265]][2] <- "X is a male"
spk.tmp2[[265]][3] <- "Y is a male"
spk.tmp2[[363]][3] <- "S is a male"
spk.tmp2[[363]][4] <- "U is a male"
spk.tmp2[[363]][5] <- "V is a male"
spk.tmp2[[363]][6] <- "W is a male"
##################################################################
# Setting up table
# Extracting speakers
speakers <- sapply(spk.tmp2, function(x) { x <- x[] }  )
speakers <- as.vector(unlist(speakers))
# Counting number of speakers per file
nspeakers <- sapply(spk.tmp2, function(x) { length(x) }  )
nspeakers <- as.vector(unlist(nspeakers))
# Extracting file ids
file.ids <- names(spk.tmp2)
# Setting up table
biodata.tb.tmp1 <- cbind(rep(file.ids, nspeakers), speakers)
# Add spk.ref
spk.ref <- gsub(" .*", "", biodata.tb.tmp1[, 2])
# Setting up table
biodata.tb.tmp2 <- cbind(biodata.tb.tmp1, spk.ref)
# Add age
age.tmp1 <- gsub("([0-9][0-9]-[0-9][0-9])", "#\\1~", biodata.tb.tmp1[, 2])
age.tmp2 <- gsub("~.*", "", age.tmp1)
age.tmp3 <- gsub(".*#", "", age.tmp2)
age.index1 <- order(age.tmp3)
age.tmp4 <- age.tmp3[order(age.tmp3)]
age.tmp4[841:length(age.tmp4)] <- "NA"
age <- age.tmp4[order(age.index1)]
# Setting up table
biodata.tb.tmp3 <- cbind(biodata.tb.tmp2, age)
# Add sex
sex.tmp1 <- gsub("(female)", "#\\1~", biodata.tb.tmp1[, 2])
sex.tmp1 <- gsub("( male)", "#\\1~", sex.tmp1)
sex.tmp2 <- gsub("~.*", "", sex.tmp1)
sex.tmp3 <- gsub(".*#", "", sex.tmp2)
sex.index1 <- order(sex.tmp3)
sex.tmp4 <- sex.tmp3[order(sex.tmp3)]
sex.tmp4[841:length(sex.tmp4)] <- "NA"
sex <- sex.tmp4[order(sex.index1)]
sex <- str_trim(sex, side = "both")
# Setting up table
biodata.tb.tmp4 <- cbind(biodata.tb.tmp3, sex)
# Add date
date.tmp1 <- gsub("([0-9][0-9]{0,1}/[0-9][0-9]{0,1}/[0-9][0-9])", "#\\1~", biodata.tb.tmp1[, 1])
date.tmp2 <- gsub("~.*", "", date.tmp1)
date.tmp3 <- gsub(".*#", "", date.tmp2)
date.index1 <- order(date.tmp3)
date.tmp4 <- date.tmp3[order(date.tmp3)]
date.tmp4[911:length(date.tmp4)] <- "NA"
date <- date.tmp4[order(date.index1)]
# Setting up table
biodata.tb.tmp5 <- cbind(biodata.tb.tmp4, date)
# Add file
file.tmp1 <- gsub("([A-Z][0-9][A-Z][0-9][0-9][0-9])", "#\\1~", biodata.tb.tmp1[, 1])
file.tmp2 <- gsub("~.*", "", file.tmp1)
file <- gsub(".*#", "", file.tmp2)
# Setting up table
biodata.tb.tmp6 <- cbind(biodata.tb.tmp5, file)
# Add subsubfile
subfile.tmp1 <- gsub("([A-Z][0-9][A-Z][0-9][0-9][0-9][a-z]{0,1})", "#\\1~", biodata.tb.tmp1[, 1])
subfile.tmp2 <- gsub("~.*", "", subfile.tmp1)
subfile.tmp3 <- gsub(".*#", "", subfile.tmp2)
subfile.index1 <- order(subfile.tmp3)
subfile.tmp4 <- subfile.tmp3[order(subfile.tmp3)]
subfile.tmp5 <- gsub("([A-Z][0-9][A-Z][0-9][0-9][0-9])", "", subfile.tmp4)
subfile <- as.vector(unlist(sapply(subfile.tmp5, function(x) {
  ifelse(x == "a", 1,
  ifelse(x == "b", 2,
  ifelse(x == "c", 3, 1))) } )))
# Setting up table
biodata.tb.tmp7 <- cbind(biodata.tb.tmp6, subfile)
# Add ethnicity
ethnicity.tmp1 <- gsub("(.* is [a-z]{0,2} )", "\\1~", biodata.tb.tmp1[, 2])
ethnicity.tmp2 <- gsub(".*~", "", ethnicity.tmp1)
ethnicity.tmp3 <- gsub("female.*", "", ethnicity.tmp2)
ethnicity.tmp3 <- gsub("male.*", "", ethnicity.tmp3)
ethnicity.index1 <- order(ethnicity.tmp3)
ethnicity.tmp4 <- ethnicity.tmp3[order(ethnicity.tmp3)]
ethnicity.tmp4[1:123] <- NA
ethnicity.tmp4[238] <- NA
ethnicity.tmp4[958] <- NA
ethnicity.tmp4[976:1002] <- NA
ethnicity.tmp5 <- ethnicity.tmp4[order(ethnicity.index1)]
ethnicity.tmp5 <- str_trim(ethnicity.tmp5, side = "both")
# Rename vector
ethnicity <- ethnicity.tmp5
# Setting up table
biodata.tb.tmp8 <- cbind(biodata.tb.tmp7, ethnicity)
# Adding an id to the table
biodata.tb.tmp9 <- cbind(1:length(biodata.tb.tmp8[, 1]), biodata.tb.tmp8)
# Extract occupation of speaker
occ.tmp1 <- biodata.tb.tmp9[, 3]
occ.tmp2 <- gsub("(.*[0-9][0-9]\\-[0-9][0-9])", "", occ.tmp1)
occ.tmp3 <- gsub("(\\(#.*)", "", occ.tmp2)
occ.tmp4 <- gsub("(, )", "", occ.tmp3)
occ.tmp5 <- gsub("(.* male.*)", "", occ.tmp4)
occ.tmp6 <- gsub("(.* female.*)", "", occ.tmp5)
occ.tmp7 <- gsub("(.* is the .*)", "", occ.tmp6)
occ.tmp8 <- str_trim(occ.tmp7, side = "both")
occupation <- occ.tmp8
# Adding occupation to the table
biodata.tb.tmp10 <- cbind(biodata.tb.tmp9, occupation)
# Finalize biodata table
biodata.tb.tmp11 <- cbind(biodata.tb.tmp10[, c(1, 8, 9, 4, 6, 5, 10, 11, 7)])
# Add column names
colnames(biodata.tb.tmp11) <- c("id", "text.id", "subfile.id", "spk.ref", "sex", "age", "ethnicity", "occupation", "date")
# Final clean up
biodata.tb.tmp11[, 5] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 5], function(x) {
  x <- gsub("NA", NA, x) } )))
biodata.tb.tmp11[, 6] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 6], function(x) {
  x <- gsub("NA", NA, x) } )))
biodata.tb.tmp11[, 7] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 7], function(x) {
  x <- gsub("NA", NA, x) } )))
biodata.tb.tmp11[, 8] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 8], function(x) {
  ifelse(x == "", NA, x)} )))
biodata.tb.tmp11[, 8] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 8], function(x) {
  x <- gsub("NA", NA, x) } )))
biodata.tb.tmp11[, 9] <- as.vector(unlist(sapply(biodata.tb.tmp11[, 9], function(x) {
  x <- gsub("NA", NA, x)  } )))
# Rename data
biodata.tmp1 <- biodata.tb.tmp11
biodata.ice.nz <- biodata.tb.tmp11
################################################################
### --- STEP
################################################################
# We will now join the two data sets
# Transform data sets into data frames
speakerinfo.ice.1 <- as.data.frame(speakerinfo.ice.1)
biodata.tmp1 <- as.data.frame(biodata.tmp1)
# Join data sets (without speakers that do not occur in the corpus but do occur in the biodata spreadsheet provided by the corpus compilers) RECOMMENDED
biodata.ice.nz.tmp1 <- join(speakerinfo.ice.1, biodata.tmp1, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# Reorganize data set
biodata.ice.nz.tmp2 <- cbind(1:length(biodata.ice.nz.tmp1[, 1]), biodata.ice.nz.tmp1[, c(1:5, 9:12, 6:7)])# not date because it only ranges from 1991 to 1994
# Reorganize data set
colnames(biodata.ice.nz.tmp2) <- c("id", "id.orig", colnames(biodata.ice.nz.tmp2)[3:12])
# Rename data
biodata.ice.nz <- biodata.ice.nz.tmp2
# Inspect data
#head(biodata.ice.nz)
###############################################################
###############################################################
###############################################################
# cols = file_subfile_speaker, orig_content, clean_content, subfile, su_count, word_count
# extract file_subfile_speaker
file_subfile_speaker <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 1]  } )))
# extract orig_content
orig_content <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 2] } )))
# extract clean_content
clean_content <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 3] } )))
# extract subfile
subfile <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 4] } )))
# extract su_count
su_count <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 5] } )))
# extract w_count
w_count <- as.vector(unlist(sapply(kwic.tb.ice.nz, function(x) {
  x <- x[, 6] } )))
# combine the vectors into a data frame
ice.nz.df <- data.frame(file_subfile_speaker, orig_content, clean_content, subfile, su_count, w_count)
# inspect results
head(ice.nz.df)

###############################################################
### --- STEP
###############################################################
# combine biodata
# extract speaker reference
spk.ref <- gsub(".*:", "", ice.nz.df$file_subfile_speaker)
spk.ref <- gsub(">", "", spk.ref)
# extract file id
text.id <- gsub("#.*", "", ice.nz.df$file_subfile_speaker)
text.id <- gsub("<", "", text.id)
text.id <- gsub("-", "", text.id)
# extract subfile id
subfile.id <- gsub(".*#", "", ice.nz.df$file_subfile_speaker)
subfile.id <- gsub(":.*", "", subfile.id)
# add information to ice.nz.df
ice.nz.df.tmp1 <- data.frame(ice.nz.df, text.id, subfile.id, spk.ref)
# join biodata to corpus content
ice.nz.tmp1 <- join(ice.nz.df.tmp1, biodata.ice.nz, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# inspect results
suf.eh <- as.vector(unlist(sapply(ice.nz.tmp1$clean_content, function(x) {
  ifelse(grepl(" eh$", x, perl = T) == F, 0, 1) } )))
# add suf.eh to ice.nz.tmp1
ice.nz.tmp2 <- data.frame(ice.nz.tmp1, suf.eh)
# remove data points that are not text type S1A
ice.nz.tmp2$texttype <- gsub("S1A.*", "S1A", ice.nz.tmp2$text.id)
ice.nz.tmp2$texttype <- gsub("S1B.*", "S1B", ice.nz.tmp2$texttype)
ice.nz.tmp2$texttype <- gsub("S2A.*", "S2A", ice.nz.tmp2$texttype)
ice.nz.tmp2$texttype <- gsub("S2B.*", "S2B", ice.nz.tmp2$texttype)
ice.nz.tmp3 <- ice.nz.tmp2[ice.nz.tmp2$texttype == "S1A",]
# rename data set
eh.by.su <- ice.nz.tmp3
# inspect results
head(eh.by.su)

################################################################
eh.by.su.clust <- eh.by.su[is.na(eh.by.su$age) == F & is.na(eh.by.su$suf.eh) == F, ]
age.means <- by(eh.by.su.clust$suf.eh, eh.by.su.clust$age, mean)
age.means <- data.frame(names(age.means[1:length(age.means)]), age.means[1:length(age.means)])
colnames(age.means) <- c("age", "ptwmean")
d <- dist(age.means)
hc <- hclust(d, method = "complete")
# activate (remove #) to save
#png("C:\\03-MyProjects\\EHinNZE\\Article\\images/Classification of age groups based on their mean frequenecy of EH.png.png") # save plot
plot(hc)
rect.hclust(hc, 2)
# activate (remove #) to save
#dev.off()
# based on the analysis, we collapse all speakers above the age of 40 in a group called "old
# and below the age of 39 into a group called "young"
################################################################
# recode age(!) based on cluster results
age.new <- sapply(eh.by.su$age, function(x) {
  ifelse(x == "70-74", "old", 
  ifelse(x == "65-69", "old",
  ifelse(x == "60-64", "old",
  ifelse(x == "55-59", "old",
  ifelse(x == "40-44", "old",
  ifelse(x == "45-49", "old",
  ifelse(x == "50-54", "old",
  ifelse(x == "30-34", "young",
  ifelse(x == "35-39", "young",
  ifelse(x == "25-29", "young", 
  ifelse(x == "20-24", "young",
  ifelse(x == "16-19", "young",)))))))))))) } )
eh.by.su <- eh.by.su
eh.by.su$age <- age.new
eh.by.su$age <- factor(eh.by.su$age, levels = c("old", "young"))
table(eh.by.su$age, eh.by.su$sex)

################################################################
# recode ethnicity
eh.by.su$ethnicity <- sapply(eh.by.su$ethnicity, function(x) {
  ifelse(x == "Cook Island Maori", "Maori",
  ifelse(x == "Cook Island/Pakeha", "Pakeha",
  ifelse(x == "Maori", "Maori",
  ifelse(x == "Maori/Nuiean/Samoan", "Maori",
  ifelse(x == "Maori/Samoan", "Maori", 
  ifelse(x == "Pakeha", "Pakeha",   
  ifelse(x == "Cook Island/Pakeha", "Pakeha",   
  ifelse(x == "Pakeha/Danish", "Pakeha", NA)))))))) } )  
table(eh.by.su$ethnicity)                             

################################################################
# recode occupation 1
eh.by.su$occ.type <- sapply(eh.by.su$occupation, function(x) {
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
# recode occupation 2
eh.by.su$occ.type <- sapply(eh.by.su$occ.type, function(x) {
  ifelse(x == "Actor", "acmp",
  ifelse(x == "Ambulance Officer", "sml",
  ifelse(x == "Anglican Youth Worker", "acmp",
  ifelse(x == "Auctioneer", "acmp",
  ifelse(x == "Baker/Patissier", "sml", 
  ifelse(x == "Baker/Student", NA,   
  ifelse(x == "Barmaid/Student", NA,   
  ifelse(x == "Barman/Student", NA, 
  ifelse(x == "Book Shop Assistant", "acmp",
  ifelse(x == "Broadcaster", "acmp",
  ifelse(x == "Builder", "sml",
  ifelse(x == "Building Supervisor", NA,
  ifelse(x == "Car Salesman", "sml",
  ifelse(x == "Caregiver", "sml",
  ifelse(x == "Catering", "sml",
  ifelse(x == "Chef/Student", NA,
  ifelse(x == "Choreographer", "acmp",
  ifelse(x == "Cleaner", "sml",
  ifelse(x == "Cleaner/Student", NA,
  ifelse(x == "Company Chairman", "acmp",
  ifelse(x == "Composer/Music Education Advisor", "acmp",
  ifelse(x == "Constable", "acmp",
  ifelse(x == "Consultant", "acmp",
  ifelse(x == "Cook", "sml",  x)))))))))))))))))))))))) } )  
# recode occupation 3
eh.by.su$occ.type <- sapply(eh.by.su$occ.type, function(x) {
  ifelse(x == "Clerk/Cleaner/Student", NA,
  ifelse(x == "Courier", "sml",
  ifelse(x == "Cultural Consultant", "acmp",
  ifelse(x == "Dbase Admin/Checkout Operator", "acmp",
  ifelse(x == "Delivery Contractor", "acmp",
  ifelse(x == "Detective NZ Police", NA,
  ifelse(x == "Documentary Producer", "acmp",
  ifelse(x == "Doing Catering Course", "sml",
  ifelse(x == "ECE Worker/Student", NA,
  ifelse(x == "Economist", "acmp",
  ifelse(x == "Education Officer", "acmp",
  ifelse(x == "Entertainer", "acmp",
  ifelse(x == "ESL Tutor", "acmp",
  ifelse(x == "Executive Radio Producer", "acmp",
  ifelse(x == "Factory Labourer/Student", NA,
  ifelse(x == "Faculty", "acmp",
  ifelse(x == "Firefighter", "sml",
  ifelse(x == "Florist", "sml",
  ifelse(x == "Food Delivery", "sml",
  ifelse(x == "Gymnastics Coach/Student", NA,
  ifelse(x == "Jewellery Salesperson", NA,
  ifelse(x == "Joiner", "sml",
  ifelse(x == "Kitchenhand/Student", NA,
  ifelse(x == "Knowledge Engineer", "acmp",
  ifelse(x == "Leader of the Opposition", "acmp",  x))))))))))))))))))))))))) } )  
# recode occupation 4
eh.by.su$occ.type <- sapply(eh.by.su$occ.type, function(x) {
  ifelse(x == "Librarian", "acmp",
  ifelse(x == "Library Assistant", "acmp",
  ifelse(x == "Machinist in Proof Centre/Student", NA,
  ifelse(x == "Marketing Rep", "acmp",
  ifelse(x == "Musician", "acmp",
  ifelse(x == "Nanny", "sml",
  ifelse(x == "Occupational Therapist", "acmp", 
  ifelse(x == "P/T Barman/Student", NA, 
  ifelse(x == "P/T Cafe Worker", "sml", 
  ifelse(x == "P/T Cleaner", "sml",
  ifelse(x == "P/T Cleaner/Student", NA,
  ifelse(x == "P/T Counsellor", "acmp",
  ifelse(x == "P/T Film Technician", "sml",
  ifelse(x == "P/T House Cleaner", "sml",
  ifelse(x == "P/T Japanese Tutor", "acmp",
  ifelse(x == "P/T Pool Attendant/Student", NA,
  ifelse(x == "P/T Shop Assistant", "sml",
  ifelse(x == "P/T Telemarketing Trainer/Student", "acmp",
  ifelse(x == "P/T Tutor", "acmp",
  ifelse(x == "P/T Tutor/KFC/Student", NA,
  ifelse(x == "P/T Tutor/Retail Assistant", NA,
  ifelse(x == "P/T Vet's Nurse/Student", NA,
  ifelse(x == "P/T Waitress/Student", NA,
  ifelse(x == "Police Constable", "acmp",
  ifelse(x == "Police Officer", NA,
  ifelse(x == "PR Office Assistant", "acmp",
  ifelse(x == "Priest", "acmp", 
  ifelse(x == "Primary Health Social Worker", "sml",
  ifelse(x == "Privacy Commissioner", "acmp",
  ifelse(x == "Processing Officer", "acmp",
  ifelse(x == "Property Developer", "sml",
  ifelse(x == "Quantity Surv & Carpentry", "sml",
  ifelse(x == "Radio Broadcaster", "acmp", x))))))))))))))))))))))))))))))))) } )  
# recode occupation 5
eh.by.su$occ.type <- sapply(eh.by.su$occ.type, function(x) {
  ifelse(x == "Receptionist", "sml",
  ifelse(x == "Receptionist/Secretary", "sml",
  ifelse(x == "Registered Nurse/Student", NA,
  ifelse(x == "Reservations Consultant", "acmp",
  ifelse(x == "Restauranteur", "acmp",
  ifelse(x == "Retail/Student", NA,
  ifelse(x == "Self-Employed", NA,
  ifelse(x == "Self-Employed Consultant", "acmp",
  ifelse(x == "Self Employed", NA,
  ifelse(x == "Self Employed Musician", "acmp",
  ifelse(x == "Senior Policy Advisor", "acmp",
  ifelse(x == "Service Station Attendant", "sml",
  ifelse(x == "Shop Assistant", "sml",
  ifelse(x == "Shop Assistant/Student", NA,
  ifelse(x == "Shop Owner", "acmp",
  ifelse(x == "Sports Shop Assistant", "sml",
  ifelse(x == "Steward/Student", NA,
  ifelse(x == "Student", "acmp",
  ifelse(x == "Student Support Person", "acmp",
  ifelse(x == "Student/Checkout Operator", "acmp",
  ifelse(x == "Student/P/T Waitress", NA,
  ifelse(x == "Student/Tutor", "acmp",
  ifelse(x == "Talkback Host", "acmp",
  ifelse(x == "Teaching Assistant", "acmp",
  ifelse(x == "Telephonist/Student", NA,
  ifelse(x == "Temp", NA,
  ifelse(x == "Tutor", "acmp",
  ifelse(x == "Tutor/Fulltime Student", "acmp",  x)))))))))))))))))))))))))))) } )  
# recode occupation 6
eh.by.su$occ.type <- sapply(eh.by.su$occ.type, function(x) {
  ifelse(x == "Tutor/Shop Assistant", NA,
  ifelse(x == "Retired", NA,
  ifelse(x == "Tutor/Student", "acmp",
  ifelse(x == "TV Quiz Show Host", "acmp",
  ifelse(x == "Unemployed", NA,
  ifelse(x == "Uni Student", "acmp",
  ifelse(x == "Video Hire", "sml",
  ifelse(x == "Waiter/PR Officer", NA,
  ifelse(x == "Waitress/Bartender", "sml",
  ifelse(x == "Waitress/Student", NA,  x)))))))))) } )  
table(eh.by.su$occ.type) 

################################################################
################################################################
### --- How many words do we utter in 5 seconds?
################################################################
### --- set up dist column for priming
################################################################
# Setting options
options(stringsAsFactors = F)
# Specify pathnames of the corpra
corpus.sbc <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\corpusdata\\TRN"
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.sbc, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and store corpus (optimaler)
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", quiet = T, encoding = "UTF-8")
  })
###############################################################
# Extract the corpus file ids
# Write function to extract the file ids
file.ids <- lapply(corpus.files, function(x) {
  x <- gsub("(.* )", "", x, perl = T)
  x <- gsub("(.*S)", "S", x, perl = T)
  x <- gsub("\\.trn", "\\1", x, perl = T)  }  )
###############################################################
# Write function to collapse the file content
corpus.tmp1 <- lapply(corpus.tmp, function(x) {
  x <- paste(x, collapse = " ")  }  )
# Add names
names(corpus.tmp1) <- file.ids
###############################################################
# Merge the file.ids with the file content
corpus.tmp2 <- apply(cbind(file.ids, corpus.tmp1), 1, function(x) unname(x))
###############################################################
# Extract the file content
corpus.content.orig.tmp1 <- sapply(corpus.tmp2, "[[", 2)
names(corpus.content.orig.tmp1) <- file.ids
###############################################################
# Clean corpus (corpus.content.orig.tmp1)
#corpus.content.orig.tmp2 <- enc2utf8(corpus.content.orig.tmp1)
corpus.content.orig.tmp2 <- corpus.content.orig.tmp1
corpus.content.orig.tmp3 <- gsub(" {2,}", " ", corpus.content.orig.tmp2)
corpus.content.orig.tmp4 <- str_trim(corpus.content.orig.tmp3, side = "both")
###############################################################
# Create a table holding the file id and the file content
overview.corpus.tb2 <- (cbind(
  1:length(corpus.content.orig.tmp4),
  file.ids,
  corpus.content.orig.tmp4))
colnames(overview.corpus.tb2) <- c("id", "file", "content")
rownames(overview.corpus.tb2) <- c(1:length(corpus.content.orig.tmp4))
# Rename the table
corpus.table2 <- overview.corpus.tb2
###############################################################
### --- STEP 2
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 3]
# Split corpus files so that each utterance is one element
all.files.unclean <- sapply(all.files, function(X) {
  x <- strsplit(gsub("([0-9]{1,4}\\.[0-9]{1,4})","~#~\\1", X), "~#~" )  }  )
###############################################################
# extract the time stamps
crps <- sapply(all.files.unclean, function(x){
  x <- strsplit(x, " ")
  }  )
tmstmps <- sapply(crps, function(x){
  x <- sapply(x, "[", 1) } )
# extract content
cntnt <-  sapply(crps, function(x){
  sapply(x, function(y) {
    paste(y, collapse = " ")
    } )
  })
# clean content
clncntnt <- sapply(cntnt, function(x){
  x <- gsub("[0-9]", "", x)
  x <- gsub("[A-Z]{2,}:", "", x)
  x <- gsub("[A-Z]{2,}", "", x)
  x <- gsub(".", "", x, fixed = T)
  x <- gsub("[", "", x, fixed = T)
  x <- gsub("(", "", x, fixed = T)
  x <- gsub(")", "", x, fixed = T)
  x <- gsub("]", "", x, fixed = T)
  x <- gsub("?", "", x, fixed = T)
  x <- gsub("!", "", x, fixed = T)
  x <- gsub("-", "", x, fixed = T)
  x <- gsub("=", "", x, fixed = T)
  x <- gsub(",", "", x, fixed = T)
  x <- gsub(">", "", x, fixed = T)
  x <- gsub("<", "", x, fixed = T)
  x <- gsub("X", "", x, fixed = T)
  x <- gsub("(H)", "", x, fixed = T)
  x <- gsub("  ", " ", x)
  x <- str_trim(x, side = "both")
  })
# count number of words
cntclncntnt <- sapply(clncntnt, function(x){
  x <- strsplit(x, " ")
  })
cntclncntnt <- sapply(cntclncntnt, function(x){
  sapply(x, function(y){
    y <- length(y)
    })
  })
# combine timestanmps and couts
sbc001 <- cbind(tmstmps[[1]], cntclncntnt[[1]])
sbc001 <- as.data.frame(sbc001)
sbc001 <- apply(sbc001, 2, as.numeric)
sbc001 <- as.data.frame(sbc001)
head(sbc001)
# create groups of 5 seconds
grps <- sbc001[,1]
grps <- sapply(grps, function(x){
  x <- ifelse(round(x/5,0) == 1, "aa",
    ifelse(round(x/5,0) == 2, "ab",
    ifelse(round(x/5,0) == 3, "ac",
    ifelse(round(x/5,0) == 4, "ad",
    ifelse(round(x/5,0) == 5, "ae",
    ifelse(round(x/5,0) == 6, "af",
    ifelse(round(x/5,0) == 7, "ag",
    ifelse(round(x/5,0) == 8, "ah",
    ifelse(round(x/5,0) == 9, "ai",
    ifelse(round(x/5,0) == 10, "aj",
    ifelse(round(x/5,0) == 11, "ak",
    ifelse(round(x/5,0) == 12, "al",
    ifelse(round(x/5,0) == 13, "am",
    ifelse(round(x/5,0) == 14, "an",
    ifelse(round(x/5,0) == 15, "ao",
    ifelse(round(x/5,0) == 16, "ap",
    ifelse(round(x/5,0) == 17, "aq",
    ifelse(round(x/5,0) == 18, "ar",
    ifelse(round(x/5,0) == 19, "as",
    ifelse(round(x/5,0) == 20, "at",
    ifelse(round(x/5,0) == 21, "au",
    ifelse(round(x/5,0) == 22, "av",
    ifelse(round(x/5,0) == 23, "aw",
    ifelse(round(x/5,0) == 24, "ax",
    ifelse(round(x/5,0) == 25, "ay",
    ifelse(round(x/5,0) == 26, "az", x))))))))))))))))))))))))))
  })
sbc001 <- data.frame(sbc001, grps)
sbc001 <- sbc001[3:240,]
sbc001
# sum up nuber of words by grps
wrds <- by(data = sbc001[,2], INDICES = sbc001[,3], FUN=sum)
wrds <- as.numeric(as.vector(wrds))
mean(wrds)

#18.45 words per 5 seconds (~18 words for priming)
###############################################################
###############################################################
###############################################################
# create vector which indicates where adding up should start
post.eh <- c(eh.by.su$suf.eh[1], eh.by.su$suf.eh[1:length(eh.by.su$suf.eh)-1] )
# create identifiable sections
sec <- as.factor(cumsum(post.eh))
# perform adding up tp new hit
dist <- ave(eh.by.su$w_count, sec, FUN = cumsum)
# set up column priming as nominal
priming <- sapply(dist, function(x){
  x <- ifelse(x <= 18, "prime", "noprime")
  }  )
# because first lines are categorzied as prime, we overwrite them
priming[1:3] <- c("noprime", "noprime", "noprime")
# add sec, dist, and priming to data set
eh.by.su <- data.frame(eh.by.su, sec, dist, priming)
# convert priming into a factor
eh.by.su$priming <- factor(eh.by.su$priming, levels = c("noprime", "prime"))
eh.by.su$age <- factor(eh.by.su$age, levels = c("young", "old"))

# inspect new data set
head(eh.by.su)

################################################################
################################################################
################################################################
### ---  CLEAN DATA
################################################################
################################################################
################################################################
# check how many sepakers are in the final data set
length(table(eh.by.su$file.speaker.id))

#remove empty su
eh.by.su <- eh.by.su[eh.by.su$w_count >=1, ]
table(eh.by.su$w_count)

# inspect final data set
head(eh.by.su)

#eh.by.su without NA
eh.by.su <- eh.by.su[is.na(eh.by.su$age) == F, ]
eh.by.su <- eh.by.su[is.na(eh.by.su$sex) == F, ]
eh.by.su <- eh.by.su[is.na(eh.by.su$ethnicity) == F, ]
eh.by.su <- eh.by.su[is.na(eh.by.su$occ.type) == F, ]
mydata <- eh.by.su
head(mydata)

################################################################
################################################################
################################################################
###                       TABULATION
################################################################
################################################################
################################################################
# tabulate results
#tapply(eh.by.su$suf.eh, eh.by.su$sex, sum)
#tapply(eh.by.speaker$suf.eh, eh.by.speaker$age, sum)
################################################################
# summarize data processing
# original data
#head(ice.nz.tmp2)
# only s1a
#head(ice.nz.tmp3)
# final data set
#head(eh.by.su)
# extract speakers words su eh mean.eh sd.eh
spk.orig <- length(table(ice.nz.tmp2$file_subfile_speaker))
spk.s1a <- length(table(ice.nz.tmp3$file_subfile_speaker))
spk.fin <- length(table(eh.by.su$file_subfile_speaker))
words.orig <- sum(ice.nz.tmp2$w_count)
words.s1a <- sum(ice.nz.tmp3$w_count)
words.fin <- sum(eh.by.su$w_count)
su.orig <- sum(ice.nz.tmp2$su_count)
su.s1a <- sum(ice.nz.tmp3$su_count)
su.fin <- sum(eh.by.su$su_count)
eh.orig <- sum(ice.nz.tmp2$suf.eh)
eh.s1a <- sum(ice.nz.tmp3$suf.eh)
eh.fin <- sum(eh.by.su$suf.eh)
# set up table with info
eh.df0 <- data.frame(cbind(c(spk.orig, spk.s1a, spk.fin), 
  c(words.orig, words.s1a, words.fin),
  c(su.orig, su.s1a, su.fin),
  c(eh.orig, eh.s1a, eh.fin)))
colnames(eh.df0) <- c("Speakers (N)", "Words (N)", "Speech Units (N)", "EH (N)")
# inspect data
eh.df0

################################################################
# all variables except occ.type
mean.eh.agesexeth <- by(eh.by.su$suf.eh, list(eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), mean)
sum.eh.agesexeth <- by(eh.by.su$suf.eh, list(eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.words.agesexeth <- by(eh.by.su$w_count, list(eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.su.agesexeth <- by(eh.by.su$su_count, list(eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.spk.agesexeth <- by(eh.by.su$file.speaker.id, list(eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), unique)
sum.spk.agesexeth <- sapply(sum.spk.agesexeth, function(x) { length(x) } )
# inspect results
#mean.eh.agesexeth
#sum.eh.agesexeth
#sum.words.agesexeth
#sum.su.agesexeth
# create a table with the results
eth <- rep(c("Maori", "Pakeha"), 4)
sex <- rep(c("Female", "Female", "Male", "Male"), 2)
age <- c(rep("Young", 4),  rep("Old", 4))
spk <- sum.spk.agesexeth[1:length(mean.eh.agesexeth)]
eh <- sum.eh.agesexeth[1:length(mean.eh.agesexeth)]
su <- sum.su.agesexeth[1:length(mean.eh.agesexeth)]-eh
eh.mean <- round(mean.eh.agesexeth[1:length(mean.eh.agesexeth)], 3)
eh.df1 <- data.frame(age, sex, eth, spk, eh, su, eh.mean)
eh.df1 <- rbind(eh.df1, c("Total", "", "", sum(na.omit(spk)), sum(na.omit(eh)), sum(na.omit(su)), round(mean(eh.by.su$suf.eh), 3)))
colnames(eh.df1) <- c("Age", "Sex", "Ethnicity", "Speakers (N)", "EH", "Speech Units without EH", "Mean EH (per SU)")
# inspect results
eh.df1

################################################################
# all variables
mean.eh.agesexethstat <- by(eh.by.su$suf.eh, list(eh.by.su$occ.type, eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), mean)
sum.eh.agesexethstat <- by(eh.by.su$suf.eh, list(eh.by.su$occ.type, eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.words.agesexethstat <- by(eh.by.su$w_count, list(eh.by.su$occ.type, eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.su.agesexethstat <- by(eh.by.su$su_count, list(eh.by.su$occ.type, eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), sum)
sum.spk.agesexethstat <- by(eh.by.su$file.speaker.id, list(eh.by.su$occ.type, eh.by.su$ethnicity, eh.by.su$sex, eh.by.su$age), unique)
sum.spk.agesexethstat <- sapply(sum.spk.agesexethstat, function(x) { length(x) } )
# inspect results
#mean.eh.agesexethstat
#sum.eh.agesexethstat
#sum.words.agesexethstat
#sum.su.agesexethstat
# create a table with the results
stat <- rep(c("ACMP", "SML"), 8)
eth <- rep(c("Maori", "Maori", "Pakeha", "Pakeha"), 4)
sex <- rep(c(rep("Female", 4), rep("Male", 4)), 2)
age <- c(rep("Young", 8), rep("Old", 8))
spk <- sum.spk.agesexethstat[1:length(mean.eh.agesexethstat)]
eh <- sum.eh.agesexethstat[1:length(mean.eh.agesexethstat)]
su <- sum.su.agesexethstat[1:length(mean.eh.agesexethstat)]-eh
eh.mean <- round(mean.eh.agesexethstat[1:length(mean.eh.agesexethstat)], 3)
eh.df2 <- data.frame(age, sex, eth, stat, spk, eh, su, eh.mean)
eh.df2 <- rbind(eh.df2, c("Total", "", "", "", sum(na.omit(spk)), sum(na.omit(eh)), sum(na.omit(su)), round(mean(eh.by.su$suf.eh), 3)))
colnames(eh.df2) <- c("Age", "Sex", "Ethnicity", "Occupation", "Speakers (N)", "EH", "Speech Units without EH", "Mean EH (per SU)")
# inspect results
eh.df2

################################################################
# sex age eth priming
mean.eh.agesexethsocpriming <- by(mydata$suf.eh, list(mydata$priming, mydata$occ.type, mydata$ethnicity, mydata$sex, mydata$age), mean)
sum.eh.agesexethsocpriming <- by(mydata$suf.eh, list(mydata$priming, mydata$occ.type, mydata$ethnicity, mydata$sex, mydata$age), sum)
sum.su.agesexethsocpriming <- by(rep(1, nrow(mydata)), list(mydata$priming, mydata$occ.type, mydata$ethnicity, mydata$sex, mydata$age), sum)
sum.su.agesexethsocpriming <- by(rep(1, nrow(mydata)), list(mydata$priming, mydata$occ.type, mydata$ethnicity, mydata$sex, mydata$age), sum)

# inspect results
#mean.eh.agesexethpriming
#sum.eh.agesexethpriming
#sum.su.agesexethpriming
#sum.spk.agesexethpriming
# create a table with the results
priming <- rep(c("noprime", "prime"), 16)
soc <- rep(c("acmp", "acmp", "sml", "sml"), 8)
eth <- rep(c("Maori", "Maori", "Maori", "Maori", "Pakeha", "Pakeha", "Pakeha", "Pakeha"), 4)
sex <- rep(c(rep("Female", 8), rep("Male", 8)), 2)
age <- c(rep("Young", 16), rep("Old", 16))
eh <- sum.eh.agesexethsocpriming[1:length(mean.eh.agesexethsocpriming)]
su <- sum.su.agesexethsocpriming[1:length(mean.eh.agesexethsocpriming)]-eh
eh.mean <- round(mean.eh.agesexethsocpriming[1:length(mean.eh.agesexethsocpriming)], 3)
eh.df3 <- data.frame(age, sex, eth, soc, priming, eh, su, eh.mean)
eh.df3 <- rbind(eh.df3, c("Total", "", "", "", "", sum(na.omit(eh)), sum(na.omit(su)), round(mean(mydata$suf.eh), 3)))
colnames(eh.df3) <- c("Age", "Sex", "Ethnicity", "Occ. Type", "Priming", "EH", "Speech Units without EH", "Mean EH (ptw)")
# inspect results
eh.df3

table(mydata$priming, mydata$suf.eh)

# SAVE DATA
#write.table(eh.df0, "C:\\03-MyProjects\\EHinNZE\\Article/ehdf0.txt", sep="\t")
#write.table(eh.df1, "C:\\03-MyProjects\\EHinNZE\\Article/ehdf1.txt", sep="\t")
#write.table(eh.df2, "C:\\03-MyProjects\\EHinNZE\\Article/ehdf2.txt", sep="\t")
#write.table(eh.df3, "C:\\03-MyProjects\\EHinNZE\\Article/ehdf3.txt", sep="\t")
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
###                       STATISTICS
################################################################
################################################################
################################################################
### --- CFA
################################################################
################################################################
################################################################
# define configurations
cfg <-  eh.df1[, 1:3]
# define counts
cnts <-  as.numeric(eh.df1[, 5])
# perform cfa
cfa.rslt <- cfa(cfg, cnts)
# inspect results
cfa.rslt

################################################################
################################################################
################################################################
### --- VISUALIZATION
################################################################
################################################################
################################################################
# set options
options("scipen" = 100, "digits" = 4)
# read in existing s´data set mblrdata.txt
mydata <- eh.by.su
# convert age, sex, and ethnicity into factors
mydata$age <- as.factor(mydata$age)
mydata$sex <- as.factor(mydata$sex)
mydata$ethnicity <- as.factor(mydata$ethnicity)
# relevel factors age & ethnicity
mydata$age <- relevel(mydata$age, "young") 
mydata$ethnicity <- relevel(mydata$ethnicity, "Pakeha") 
# provide an overview of the data
head(mydata); str(mydata); summary(mydata)

###########################################################################
# prepare data
x <- 100:85
y <- 2:17
z <- x/y
z <- c(3, z)
# activate (remove #) to save
png("C:\\03-MyProjects\\EHinNZE\\Article\\images/Priming.png") # save plot
plot(z, type = "l", xlim = c(0.5, 19), ylim = c(-6, 55), axes = F, ylab  = "Neuronal Activity", xlab = "", lwd = 2)
lines(0:17, rep(20, length(0:17)), lty = 3, col = "red")
lines(0:17, rep(3, length(0:17)), lty = 3, col = "red")
text(x = 12, y = 4.5, "Base-line Activity (BA)")
text(x = 14, y = 22, "Activation Threshold (AT)")
text(x = 5, y = 50, bquote(""~T^{1}~": Activation Peak"))
text(x = 5, y = 11, bquote("Distance\n to AT at "~T^{1}~""))
text(x = 11, y = 15.5, bquote("Distance\n to AT at "~T^{2}~""))
text(x = 3, y = -1.5, bquote(""~T^{0}~": Activation Event"))
text(x = 11.5, y = -1.5, bquote(""~T^{2}~": Second Activation Event"))
arrows(3, -6, 12, -6, length = 0.15, angle = 30, code = 2)
text(x = 7, y = -4.5, "Time")
CurlyBraces(2, 2, 3, 20, pos = 1, direction = 1, depth = 1)
CurlyBraces(8, 8, z[8], 20, pos = 1, direction = 1, depth = .5 )
arrows(1, 0, 1, 2, length = 0.05, angle = 30, code = 2, col = "red")
arrows(8, 0, 8, 2, length = 0.05, angle = 30, code = 2, col = "red")
# activate (remove #) to save
dev.off()
###
# prepare data for visualization
p0.dat <- data.frame(ice.nz.tmp2$file_subfile_speaker, ice.nz.tmp2$w_count, ice.nz.tmp2$suf.eh)
colnames(p0.dat) <- c("speaker", "words", "eh")
words <- as.vector(by(p0.dat$words, p0.dat$speaker, sum))
eh <- as.vector(by(p0.dat$eh, p0.dat$speaker, sum))
ptw <- eh / words * 1000
texttype <- gsub("-[0-9]{3,3}.*", "", names(table(p0.dat$speaker)))
texttype <- gsub("<", "", texttype)
p0.dat <- data.frame(texttype, ptw)
colnames(p0.dat) <- c("texttype", "ptw")
p0.dat <- p0.dat[complete.cases(p0.dat),]
# create boxplot showing the use of clause-final EH across text types
# activate (remove #) to save
png("C:\\03-MyProjects\\EHinNZE\\Article\\images/EHText.png") # save plot
# make plot
boxplot(ptw ~ texttype,
  data = p0.dat, # the data we want to display
  main = "", # you could specify a title here
  ylab = "Rel. Freq. (per 1,000 words)", # label of the y-axis
  ylim = c(-2, 10), # label of the x-axis
  xlab = "Text Type", # label x-axis
  axes = F, # do not draw axes yet
  notch = T, # include notches
  col = c("lightgreen", "lightgrey", "lightblue", "lightcoral"),
  las = 1,
  cex = 1) # the font size should be 100% of the normal size) # create boxplots with different colors
axis(1, # set up the x-axis (1 = x, 2 = y)
  at = 1:4, # we specify the locations where we want the tickmarks
  labels = c("", "", "", ""), # you could specify the text here
  lty = 1, # we define the linetype (1 = straight line)
  col = "black", # the tickmarks should be black
  las = 1,
  cex = 1) # the font size should be 100% of the normal size
axis(2, # set up y-axis
  at = c(0, 2, 4, 6, 8, 10), # create tick marks at the specified locations
  labels= c("0", "2", "4", "6", "8", "10"), #create text at the specified locations
  lty = 1, # we define the linetype (1 = straight line)
  col = "black", # the tickmarks should be black
  las = 1,
  cex = 1) # the font size should be 100% of the normal size
mtext(c("S1A", "S1B", "S2A", "S2B"), # create specified text 
  side = 1, # put text along the x-axis
  line = 2, # place text at the 3rd line of the x-axis
  at = 1:4, # put text at location 1 to 4
  las = 1,
  cex = 1) # the font size should be 100% of the normal size
text(1:4, 
  c(as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[1], 
  as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[2],
  as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[3], 
  as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[4]),
    "+")
text(1:4, 
  c(-1.0, -1.0, -1.0, -1.0), 
  cex = 1, 
  labels = paste("mean\n",
  c(round(as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[1], 2), 
    round(as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[2], 2),
    round(as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[3], 2), 
    round(as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))[4], 2),
    sep = "")))
rug(jitter(p0.dat$ptw), 
  side=4)
grid()
box()
# activate (remove #) to save
dev.off()

# analogous plot in ggplot2
texttypemeans <- as.vector(by(p0.dat$ptw, p0.dat$texttype, mean))
p0 <- ggplot(p0.dat, aes(texttype, ptw, color = texttype)) +
  scale_fill_brewer() + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  theme_set(theme_bw(base_size = 20)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  labs(x = "Text Type", y = "Rel. Freq. (EH per 1,000 words)", colour = "texttype") +
  scale_color_manual(values = c("grey20", "grey35", "grey50", "grey65")) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[1], 2), sep = ""), x = 1, y = texttypemeans[1] -1, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[2], 2), sep = ""), x = 2, y = texttypemeans[2] + .3, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[3], 2), sep = ""), x = 3, y = texttypemeans[3] + .3, colour = "grey20", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[4], 2), sep = ""), x = 4, y = texttypemeans[4] + .3, colour = "grey20", size = 5) +
  theme(legend.position="none")
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHTextgg.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p0

# prepare data for visualization
mydatan <- data.frame(mydata$ethnicity, mydata$age, mydata$sex, mydata$occ.type, mydata$suf.eh)
colnames(mydatan) <- gsub("mydata.", "", colnames(mydatan))
mydatan <- na.omit(mydatan)
#prepare data p1
mydata1 <- data.frame(mydata$sex, mydata$suf.eh)
colnames(mydata1) <- gsub("mydata.", "", colnames(mydata1))
mydata1 <- na.omit(mydata1)
p1 <- ggplot(mydata1, aes(sex, suf.eh, color = sex)) +
  scale_fill_brewer() + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  theme_set(theme_bw(base_size = 20)) +
  coord_cartesian(ylim = c(0, 0.05)) +
  theme(legend.position="none") +
  labs(x = "Sex", y = "Frequency (Mean: EH per Speech Unit)") 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSex.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p1

# prepare data p2
mydata2 <- data.frame(mydata$sex, mydata$age, mydata$suf.eh)
colnames(mydata2) <- gsub("mydata.", "", colnames(mydata2))
mydata2 <- na.omit(mydata2)
p2 <- ggplot(mydata2, aes(age, suf.eh, color = age)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Age", y = "Frequency (Mean: EH per Speech Unit)") +
  scale_color_manual(values = c("darkblue", "lightblue")) 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHAge.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p2

# prepare data p3
mydata3 <- data.frame(mydata$age, mydata$ethnicity, mydata$suf.eh)
colnames(mydata3) <- gsub("mydata.", "", colnames(mydata3))
mydata3 <- na.omit(mydata3)
p3 <- ggplot(mydata3, aes(ethnicity, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Ethnicity", y = "Frequency (Mean: EH per Speech Unit)", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHEth.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p3

# prepare data p21
mydata21 <- data.frame(mydata$age, mydata$occ.type, mydata$suf.eh)
colnames(mydata21) <- gsub("mydata.", "", colnames(mydata21))
mydata21 <- na.omit(mydata21)
p21 <- ggplot(mydata21, aes(occ.type, suf.eh, colour = occ.type)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Occ. Type", y = "Frequency (Mean: EH per Speech Unit)", colour = "occ.type") +
  scale_color_manual(values = c("orange", "red"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSoc.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p21


# prepare data p4
mydata4 <- data.frame(mydata$ethnicity, mydata$sex, mydata$suf.eh)
colnames(mydata4) <- gsub("mydata.", "", colnames(mydata4))
mydata4 <- na.omit(mydata4)
p4 <- ggplot(mydata4, aes(ethnicity, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group= sex)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Ethnicity", y = "Frequency (Mean: EH per Speech Unit)", colour = "sex") 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSexEth.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p4

# prepare data p5
mydata5 <- data.frame(mydata$age, mydata$sex, mydata$suf.eh)
colnames(mydata5) <- gsub("mydata.", "", colnames(mydata5))
mydata5 <- na.omit(mydata5)
p5 <- ggplot(mydata5, aes(sex, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group= age)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Sex", y = "Frequency (Mean: EH per Speech Unit)", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue"))   
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSexAge.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p5

# prepare data p6
mydata6 <- data.frame(mydata$age, mydata$ethnicity, mydata$suf.eh)
colnames(mydata6) <- gsub("mydata.", "", colnames(mydata6))
mydata6 <- na.omit(mydata6)
p6 <- ggplot(mydata6, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group= ethnicity)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Age", y = "Frequency (Mean: EH per Speech Unit)", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHAgeEth.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p6

# prepare data p7
mydata7 <- data.frame(mydata$occ.type, mydata$sex, mydata$age, mydata$suf.eh)
colnames(mydata7) <- gsub("mydata.", "", colnames(mydata7))
mydata7 <- na.omit(mydata7)
p7 <- ggplot(mydata7, aes(sex, suf.eh, colour = occ.type)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group= occ.type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) 
  labs(x = "Sex", y = "Frequency (Mean: EH per Speech Unit)", colour = "occ.type") +
  scale_color_manual(values = c("orange", "red"))  
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSexSoc.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p7

# prepare data p8
mydata8 <- data.frame( mydata$ethnicity, mydata$occ.type, mydata$suf.eh)
colnames(mydata8) <- gsub("mydata.", "", colnames(mydata8))
mydata8 <- na.omit(mydata8)
p8 <- ggplot(mydata8, aes(ethnicity, suf.eh, colour = occ.type)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group= occ.type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  coord_cartesian(ylim = c(0, 0.05)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Ethnicity", y = "Frequency (Mean: EH per Speech Unit)", colour = "occ.type") +
  scale_color_manual(values = c("orange", "red"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSocEth.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p8

# Plot the plots
multiplot(p1, p3, p5, p7, p2, p4, p6, p8, cols = 2)

# plot effect of priming
eh.prm.tb <- table(mydata$priming, mydata$suf.eh)
# activate (remove #) to save
png("C:\\03-MyProjects\\EHinNZE\\Article\\images/EHPriming.png") # save plot
#par(mfrow=c(1, 2)) # plot two plots in two columns in one window
assocplot(eh.prm.tb, col = c("grey70", "grey95"))
#mosaicplot(eh.prm.tb, shade = TRUE, type = "pearson", main = "")
#par(mfrow=c(1, 1)) # restore original graphical parameters
# activate (remove #) to save
dev.off()

###########################################################################
# prepare data for visualization
mydatan <- data.frame(mydata$ethnicity, mydata$age, mydata$sex, mydata$occ.type, mydata$suf.eh)
colnames(mydatan) <- gsub("mydata.", "", colnames(mydatan))
mydatan <- na.omit(mydatan)

p9 <- ggplot(mydatan, aes(age, suf.eh, colour = sex)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  facet_wrap(~ ethnicity, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: EH per Speech Unit)", colour = "sex")
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHSexAge.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p9

p10 <- ggplot(mydatan, aes(age, suf.eh, colour = ethnicity)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group = ethnicity)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: EH per Speech Unit)", colour = "ethnicity") +
  scale_color_manual(values = c("darkgreen", "lightgreen")) 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHAgeEth.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p10

p11 <- ggplot(mydatan, aes(ethnicity, suf.eh, colour = age)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group = age)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: EH per Speech Unit)", colour = "age") +
  scale_color_manual(values = c("darkblue", "lightblue")) 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHEthAge.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p11

p12 <- ggplot(mydatan, aes(ethnicity, suf.eh, colour = occ.type)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group = occ.type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: EH per Speech Unit)", colour = "occ.type") +
  scale_color_manual(values = c("orange", "red"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHEthSoc.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p12

p13 <- ggplot(mydatan, aes(age, suf.eh, colour = occ.type)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "point", aes(group = occ.type)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  facet_wrap(~ sex, nrow = 1) +
  coord_cartesian(ylim = c(0, 0.075)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Frequency (Mean: EH per Speech Unit)", colour = "occ.type") +
  scale_color_manual(values = c("orange", "red"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"EHAgeSoc.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p13

################################################################
################################################################
################################################################
### ---         SOCIAL CLASS
################################################################
################################################################
################################################################
###             Mixed-Effects Binomial Logistic Regression
################################################################
################################################################
################################################################
# extract data
mydatasc <- mydata

# delete all NAs from data set
mydatasc <- data.frame(mydatasc$file.speaker.id, mydatasc$ethnicity, mydatasc$age, mydatasc$sex, mydatasc$occ.type, mydatasc$priming, mydatasc$suf.eh)
colnames(mydatasc) <- gsub("mydatasc.", "", colnames(mydatasc))
mydatasc$file.speaker.id <- as.factor(mydatasc$file.speaker.id)
mydatasc <- na.omit(mydatasc)
# change factor level for age
mydatasc$age <- factor(mydatasc$age, levels = c("old", "young"))
# inspect data
str(mydatasc)

# inspect data (check how many speakers are in the subgroups for sex:occ.type)
speakers.sex.sc <- by(mydatasc$file.speaker.id, list(mydatasc$sex, mydatasc$occ.type), ftable)
n.speakers.sex.sc <- sapply(speakers.sex.sc, function(x){
  x <- ifelse(x == 0, 0, 1)
  x <- sum(x) } )
# inspect the number of speakers in each subcohort
sum(n.speakers.sex.sc)

# inspect data (check how many speakers are in the subgroups for age:occ.type)
speakers.age.sc <- by(mydatasc$file.speaker.id, list(mydatasc$age, mydatasc$occ.type), ftable)
n.speakers.age.sc <- sapply(speakers.age.sc, function(x){
  x <- ifelse(x == 0, 0, 1)
  x <- sum(x) } )
# inspect the number of speakers in each subcohort
sum(n.speakers.age.sc)

################################################################
################################################################
################################################################
### ---        Model Building - GLMEBR
################################################################
################################################################
################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
mydatasc.dist <- datadist(mydatasc)
options(datadist = "mydatasc.dist")
# a few words on glm vs lrm: Baayen (2008:196-197) states that lrm should be
# the function of choice in cases where each row contains
# exactly 1 success OR failure (1 or 0) while glm is preferrable if there are two
# columns holding the number of successes and the number of failures
# respectively. i have tried it both ways and both functions work fine if
# each row contains exactly 1 success OR failure but only glm can handle the
# latter case.
# generate initial saturated regression model including
# all variables and their interactions
m0.glm = glm(suf.eh ~ 1, family = binomial, data = mydatasc) # baseline model glm
m0.lrm = lrm(suf.eh ~ 1, data = mydatasc, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# create model with a random intercept for file.speaker.id
#m1.lmer <- lmer(suf.eh ~ (1|file.speaker.id), data = mydatasc, family = binomial)
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(suf.eh ~ (1|file.speaker.id), data = mydatasc, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glmer object is smaller which shows that including the random
# intercepts is justified
###
# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up, i.e. forward, procedure although with step-wise step-up
# the likelyhood of type II errors is much higher than with step-wise
# step down, i.e. backward elimination(cf. Field, Miles & Field 2012:265)
# we need to add "control = glmerControl(optimizer = "bobyqa")" because otherwise R fails to converge
#	automated model fitting
m1.glmer <- glmer(suf.eh ~ age + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m2.glmer <- glmer(suf.eh ~ age + sex + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m3.glmer <- glmer(suf.eh ~ age + sex + ethnicity + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m4.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer="bobyqa"))
m5.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer="bobyqa"))
m6.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m7.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m8.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + sex : ethnicity + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa")) # BEST MODEL
m9.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + sex : ethnicity + age : occ.type + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m10.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + sex : ethnicity + age : occ.type + sex : occ.type + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m11.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + sex : ethnicity + age : occ.type + sex : occ.type +  ethnicity : occ.type  + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m12.glmer <- glmer(suf.eh ~ age + sex + ethnicity + priming + occ.type + age : sex + age : ethnicity + sex : ethnicity + age : occ.type + sex : occ.type +  ethnicity : occ.type  + age : sex : ethnicity + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
# test which models are the most adequate
# we compare all models because this way, we get an overview of model paramerets
# and can check which model has the lowerst AIC, BIC, and the highest X^2 value
anova(m0.glmer, m1.glmer, m2.glmer, m3.glmer, m4.glmer, m5.glmer, m6.glmer, m7.glmer, m8.glmer, m9.glmer, m10.glmer, m11.glmer, m12.glmer, test = "Chi")

# use customized model comparison function
# create comparisons
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi")
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi")
m3.m2 <- anova(m3.glmer, m2.glmer, test = "Chi")
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi")
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi")
m6.m5 <- anova(m6.glmer, m5.glmer, test = "Chi")
m7.m6 <- anova(m7.glmer, m6.glmer, test = "Chi")
m8.m7 <- anova(m8.glmer, m7.glmer, test = "Chi")
m9.m8 <- anova(m9.glmer, m8.glmer, test = "Chi")
m10.m9 <- anova(m10.glmer, m9.glmer, test = "Chi")
m11.m10 <- anova(m11.glmer, m10.glmer, test = "Chi")
m12.m11 <- anova(m12.glmer, m11.glmer, test = "Chi")
# create a list of the model comparisons
mdlcmp <- list(m1.m0, m2.m1, m3.m2, m4.m3, m5.m4, m6.m5, m7.m6, m8.m7, m9.m8, m10.m9, m11.m10, m12.m11)
# apply function
mdl.cmp.glmersc.swsu <- mdl.fttng.swsu(mdlcmp)
# inspect output
mdl.cmp.glmersc.swsu

write.table(mdl.cmp.glmersc.swsu, "C:\\03-MyProjects\\EHinNZE\\Article/mdl_cmp_glmersc_swsu.txt", sep="\t")

###########################################################################
# based on the overview, we expext m3.glmer to be the best model.
# we confirm this hypothesis by testing if deleting a variable significantly
# decreases model fit (if the models differ significantly then the deletion is
# not permitted and the effect has to remain in the model
###########################################################################
### --- STEP-WISE STEP-DOWN confimation
###########################################################################
# we begin by comparing the model with all main effects and all interactions to
# a model without the three-way-interaction and continue to delete insig. interactions and finally main effects
m12.m11 <- anova(m12.glmer, m11.glmer, test = "Chi") 
m11.m10 <- anova(m11.glmer, m10.glmer, test = "Chi")
m10.m9 <- anova(m10.glmer, m9.glmer, test = "Chi")
m9.m8 <- anova(m9.glmer, m8.glmer, test = "Chi") 
m8.m7 <- anova(m8.glmer, m7.glmer, test = "Chi") 
m7.m6 <- anova(m7.glmer, m6.glmer, test = "Chi") 
m6.m5 <- anova(m6.glmer, m5.glmer, test = "Chi")
m5.m4 <- anova(m5.glmer, m4.glmer, test = "Chi") 
m4.m3 <- anova(m4.glmer, m3.glmer, test = "Chi")
m3.m2 <- anova(m3.glmer, m2.glmer, test = "Chi") 
m2.m1 <- anova(m2.glmer, m1.glmer, test = "Chi")
m1.m0 <- anova(m1.glmer, m0.glmer, test = "Chi") 
# activate library
source("C:\\R/ModelFittingSummarySWSD.R") # for Mixed Effects Model fitting (step-wise step-down): Binary Logistic Mixed Effects Models
# create list of model comparisons
mdlcmp <- list(m12.m11, m11.m10, m10.m9, m9.m8, m8.m7, m7.m6, m6.m5, m5.m4, m4.m3, m3.m2, m2.m1, m1.m0)
# apply function
mdl.cmp.glmersc.swsd <- mdl.fttng.swsd(mdlcmp)
# inspect results
mdl.cmp.glmersc.swsd

write.table(mdl.cmp.glmersc.swsd, "C:\\03-MyProjects\\EHinNZE\\Article/mdl_cmp_glmersc_swsd.txt", sep="\t")

# m3.glmer is our final model!
###########################################################################
# test if including the marginally significant interaction is justified
m3.glmer <- glmer(suf.eh ~ age + sex + ethnicity + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m13.glmer <- glmer(suf.eh ~ age + sex + ethnicity + age : occ.type + (1|file.speaker.id), family = binomial, data = mydatasc, control = glmerControl(optimizer = "bobyqa"))
m13.m3 <- anova(m13.glmer, m3.glmer, test = "Chi") 
m13.m3 # no significant difference, we therefore opt for the smaller model

# rename final minimal adeqaute model
mlr.glmer <- m3.glmer

# test if the final minimal adequate model performs better than the base-line model
anova(mlr.glmer, m0.glmer, test = "Chi")

# inspect results of the final minimal adequate model
print(mlr.glmer, corr = F)

# alternative result display (anova)
anova(mlr.glmer)

# extract the parameters of the fixed effects for the report
# to do that, we compare the model with only the random effect to a model with
# the random effect and the fixed effect for age
age.effect.glmersc <- anova(m0.glmer, m1.glmer, test = "Chi") # effect of age

# we now test the effect of sex by adding sex as a fixed effect
sex.effect.glmersc <- anova(m1.glmer, m2.glmer, test = "Chi") # effect of sex

# finally, we test the effect of ethnicity, by adding ethnicity and compare a
# model with ethnicity to a model without ethnicity
ethnicity.effect.glmersc <- anova(m2.glmer, m3.glmer, test = "Chi") # effect of ethnicity

###########################################################################
### --- extracting and calculating model fit parameters
# we now create a lmr object equivalent to the final minimal adequate model
# but without the random effect
mlr.lrm <- lrm(suf.eh ~ age + sex + ethnicity + priming, data = mydatasc, x = T, y = T)
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(suf.eh ~ age + sex + ethnicity + priming + (1|file.speaker.id), data = mydatasc, family = binomial)

# now we check if the fixed effects of the lrm and the lmer model correlate (cf Baayen 2008:281)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))

# the fixed effects correlate very strongly - this is good as it suggests that
# the coefficient estimates are very stable

# we activate the package Hmisc (if not already active)
library(Hmisc)
# we now extract model fit parameters (cf Baayen 2008:281)
probs = 1/(1+exp(-fitted(mlr.lmer)))
probs = binomial()$linkinv(fitted(mlr.lmer))
somers2(probs, as.numeric(mydatasc$suf.eh))

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
my.lmer.nagelkerke(c("suf.eh ~ age + sex + ethnicity + priming", "(1 | file.speaker.id)"), mydatasc)

###########################################################################
# model diagnostics: plot fitted against residuals
plot(mlr.glmer)

# plot residuals against fitted
plot(mlr.glmer, form = resid(., type = "response") ~ fitted(.) | file.speaker.id, abline = 0, cex = .5,id = 0.05, adj = -0.3)

# diagnostic plot: examining residuals (Pinheiro & Bates 2000:175)
plot(mlr.glmer, file.speaker.id ~ resid(.), abline = 0 , cex = .5)

###############################################################
#inspect the results meblrm.summary(glm0, glm1, glmer0, glmer1, dpvar)
meblrmsc <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydatasc$suf.eh) #
write.table(meblrmsc, "C:\\03-MyProjects\\EHinNZE\\Article/meblrmsc.txt", sep="\t")

###########################################################################
###########################################################################
###########################################################################
### --- EXPLANATION of MODEL FIT PARAMETERS
###########################################################################
###########################################################################
###########################################################################
# R2 (Hosmer & Lemeshow)
# "Rt is the proportional reduction in the absolute value of the log-likelihood
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
head(eh.by.su)

eh.df0

eh.df1

eh.df2

eh.df3

# glmer
mdl.cmp.glmersc.swsu

mdl.cmp.glmersc.swsd

age.effect.glmersc 

sex.effect.glmersc 

ethnicity.effect.glmersc 

meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, mydatasc$suf.eh) 

################################################################
### --- THE END
################################################################
