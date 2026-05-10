##################################################################
### --- R-Skript "Combining biodata and word counts for speakers
### --- represented in the International Corpus of English (ICE) with R"
### --- Author: Martin Schweinberger (Dec 18th, 2013)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R retrieves number of words for each
### --- speaker in the ICE corpus and
### --- merges the word counts with the biodata of the speakers
### --- provided by the compilers of the components of the ICE.
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
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2013. " Combining biodata and word
### --- counts for speakers represented in the International Corpus of English
### --- (ICE) with R ", unpublished R-skript, Hamburg University.
### --- ACKNOWLEDGEMENTS
### --- I want to thank the compilers of the ICE components for providing me
### --- with the raw biodata of the speakers represented by the
### --- ICE data - without their generousity, this script would not exist.
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
# Load packages
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
library(reshape)
library(zoo)
###############################################################
# Setting options
options(stringsAsFactors = F)
# Specify pathnames of the corpra
corpus.can <- "C:\\PhD\\skripts n data\\corpora\\ICE CAN\\Corpus"
corpus.ind <- "C:\\PhD\\skripts n data\\corpora\\ICE India\\Corpus"
corpus.ire <- "C:\\PhD\\skripts n data\\corpora\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
corpus.gb <- "C:\\PhD\\skripts n data\\corpora\\ICE GB2\\ice-gb-2\\data"
corpus.jam <- "C:\\PhD\\skripts n data\\corpora\\ICE Jamaica"
corpus.nz <- "C:\\PhD\\skripts n data\\corpora\\ICE New Zealand\\Spoken"
corpus.phi <- "C:\\PhD\\skripts n data\\corpora\\ICE Philippines\\Corpus"
corpus.sbc <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\corpusdata\\TRN"
# Define input pathname of raw biodata
bio.can <- "C:\\PhD\\skripts n data\\corpora\\ICE CAN\\Headers/Spoken ICE-CAN metadata.txt"
bio.ind <- "C:\\PhD\\skripts n data\\corpora\\ICE India\\Headers"
bio.ire.1 <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Ireland biodata raw/Census spoken N complete HANDBOOK.txt" # (southern data)
bio.ire.2 <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Ireland biodata raw/Census spoken S complete HANDBOOK.txt" # (northern data)
bio.gb <- "C:\\PhD\\skripts n data\\corpora\\ICE GB2\\ice-gb-2\\text/sspeaker.txt"
bio.jam <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Jamaica biodata raw/biodata_original.txt"
bio.nz <- "C:\\PhD\\skripts n data\\corpora\\ICE New Zealand/NZGUIDE.txt"
bio.phi <- "C:\\PhD\\skripts n data\\corpora\\ICE Philippines\\Headers/ice philippines biodata spoken.txt"
bio.sbc.1 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata1.csv"
bio.sbc.2 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata2.csv"
bio.sbc.3 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata3.csv"
bio.sbc.4 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata4.csv"
# Define outputpath of final biodata
out.bio.phd <- "C:\\PhD\\skripts n data/biodata phd.txt"
out.can <- "C:\\PhD\\skripts n data/biodata ice canada.txt"
out.ind <- "C:\\PhD\\skripts n data/biodata ice india.txt"
out.ire <- "C:\\PhD\\skripts n data/biodata ice ireland.txt"
out.gb <- "C:\\PhD\\skripts n data/biodata ice gb r2.txt"
out.jam <- "C:\\PhD\\skripts n data/biodata ice jamaica.txt"
out.nz <- "C:\\PhD\\skripts n data/biodata ice new zealand.txt"
out.phi <- "C:\\PhD\\skripts n data/biodata ice philippines.txt"
out.sbc <- "C:\\PhD\\skripts n data/biodata sbcae.txt"
###############################################################
###############################################################
# Prepare for loading canadian corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.can,
  pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T,
  include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T)  }  )
corpus.tmp <- unlist(corpus.tmp)
# Paste all elements of the corpus together
corpus.tmp1 <- paste(corpus.tmp, collapse = " ")
# Clean corpus
corpus.tmp2 <- enc2utf8(corpus.tmp1)
corpus.tmp2 <- gsub(" {2,}", " ", corpus.tmp2)
corpus.tmp2 <- str_replace_all(corpus.tmp2, fixed("\n"), " ")
corpus.tmp2 <- str_trim(corpus.tmp2, side = "both")
###############################################################
# Specify search pattern
splitpattern = "</I>"
# Splits corpus into parts
corpus.tmp5 <- sapply(corpus.tmp2, function(x) {
  strsplit(as.character(x), splitpattern)  }  )
# Retrieve only spoken files
corpus.tmp5 <- corpus.tmp5[[1]][1:550]
# Inspect the resulting list
#str(corpus.tmp5)
#corpus.tmp5[[550]]
# Clean the corpus files
corpus.tmp6 <- sapply(corpus.tmp5, function(x)  {
  str_trim(x, side = "both")  }  )
# Inspect the resulting list
#str(corpus.tmp6)
#PROBLEM: Element [383] is broken!
#Delete element [383]
corpus.tmp6 <- c(corpus.tmp6 [1:382], corpus.tmp6 [384:length(corpus.tmp6)])
# Inspect the resulting list
#str(corpus.tmp6)
###############################################################
# Extract text.ids from the list elements
full.text.ids.tmp1 <- lapply(corpus.tmp6, function(x)  {
  x <- strsplit( gsub("(.*?<ICE-CAN:S[1|2][A-Z]-[0-9]{3,3}#[0-9]{0,3}:{0,1}[0-9]{0,3}:{0,1}[A-Z]{0,2}[0-9]{0,1}>)", "\\1~", x), "~" )  }  )
# Inspect the resulting list
#str(full.text.ids.tmp1)
# Retrieve the first item from each list element &
#  vectorize the resulting list
full.text.ids.tmp2 <- as.vector(unlist(sapply(full.text.ids.tmp1, function(x)  {
  sapply(x, "[", 1)  }  )))
# Inspect the resulting vector
#str(full.text.ids.tmp2)
# Clean the text.ids
full.text.ids.tmp3 <- as.vector(sapply(full.text.ids.tmp2, function(x)  {
  x <- gsub("(.*<ICE)","<ICE", x, perl = TRUE)  }  ))
# Inspect the resulting vector
#full.text.ids.tmp3
#length(full.text.ids.tmp3)
#PROBLEM: elements [83] is  broken!
# Repair element [83]
full.text.ids.tmp3[83] <- "<ICE-CAN:S1A-060#1:1:A>"
###############################################################
# Also repair the file content
corpus.tmp6[83] <- sapply(corpus.tmp6[83], function(x)  {
  x <- gsub("(<#)","<ICE-CAN:S1A-060#", x, perl = TRUE)  }  )
###############################################################
# Inspect the resulting vector
#full.text.ids.tmp3
full.text.ids.tmp4 <- as.vector(sapply(full.text.ids.tmp3, function(x)  {
  x <- gsub("(#[0-9]{1,3}:)","#", x, perl = TRUE)
  x <- gsub("(:[A-Z]>)","", x, perl = TRUE)
  x <- gsub("(<ICE-CAN:)","", x, perl = TRUE)
  x <- gsub("(>)","", x, perl = TRUE)  }  ))
# Inspect the resulting vector
#full.text.ids.tmp4
# Retrieve text.ids and subfile.ids from the data
# text.ids
text.ids <- as.vector(sapply(full.text.ids.tmp4, function(x)  {
  x <- gsub("(#.*)","", x, perl = TRUE)  }  ))
# Inspect the resulting vector
#text.ids
# subfile.ids
subfile.ids <- as.vector(sapply(full.text.ids.tmp4, function(x)  {
  x <- gsub("(.*#)","", x, perl = TRUE)  }  ))
# Inspect the resulting vector
#subfile.ids
###############################################################
# Create a matrix out of the data
corpus.tb1 <- as.data.frame(cbind(1:length(corpus.tmp6), text.ids, subfile.ids, corpus.tmp6))
# Add column labels
colnames(corpus.tb1) <- c("id","text.id","subfile.id","corpusfile")
# Add row labels
rownames(corpus.tb1) <- c(1:length(corpus.tmp6))
# Inspect the resulting table
#head(corpus.tb1)
# Rename table to match other ice skripts
corpus.table2 <- corpus.tb1
# View results
#head(corpus.table2)
###############################################################
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Split corpus files so that each turn is one element
all.files.unclean <- sapply(all.files, function(x) {
  corpfile2 <- strsplit(gsub("(<ICE-CAN:S[1|2][A-Z]-[0-9]{3,3}#[0-9]{0,3}:{0,1}[0-9]{0,3}:{0,1}[A-Z]{0,2}[0-9]{0,1}>)", "~\\1", x), "~" )  }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)] , 1 , paste , collapse = "#" )
names(all.files.unclean) <- file.subfile.ids
# Delete every first item from the elements
all.files.unclean <- sapply(all.files.unclean, function(x)  {
  x <- x[2:length(x)]  }  )
# PROBLEM: file S1B-019 (all.files.unclean[159]) contains a broken line!
# Replace broken string with repaired string
all.files.unclean[[159]][83] <- "<ICE-CAN:S1B-019#83:1:A> Yeah  <$A> "
# View results
#all.files.unclean
###############################################################
# Separate the speakers from the turns
speakers.and.turns <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2)  }  )
# View results
#speakers.and.turns
# Store speakers in extra vector and clan the elements
full.speakers <- lapply(speakers.and.turns, function(x) {
  sapply(x, "[[", 1)  }  )
# Extract the speakers
speakers.tmp1 <- lapply(full.speakers, function(x)  {
  str_replace_all(x, "(.*?:)","")  }  )
speakers <- lapply(speakers.tmp1, function(x) gsub(">","", x, fixed = TRUE))
# View results
#speakers
# Store turns in extra vector
turns <- lapply(speakers.and.turns, function(x) {
  sapply(x, function(x) x[2])  }  )
# View results
#turns
###############################################################
# Create a list with all turns but cleaned, i.e. without metas
turns.clean <- lapply(turns, function(x) {
  x <- str_replace_all(x, "(<O.*?/O>)","")
  x <- str_replace_all(x, "(<q.*?/q>)","")
  x <- str_replace_all(x, "(<&.*?/&.*>)","")
  x <- str_replace_all(x, "(<[a-z]{4,}.*?</[a-z]{4,}>)","")
  x <- str_replace_all(x, "(<,>)", "")
  x <- str_replace_all(x, "(<,,>)", "")
  x <- str_replace_all(x, "(\\[[0-9]{1,3}\\.[0-9]{1,3}\\])","")
  x <- str_replace_all(x, "(<\\..*/.>)","")
  x <- str_replace_all(x, "(<\\[[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\[[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<\\{[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\{[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<\\]>)","")
  x <- str_replace_all(x, "(</\\]>)","")
  x <- str_replace_all(x, "(<\\}>)","")
  x <- str_replace_all(x, "(</\\}>)","")
  x <- str_replace_all(x, "(<un.*?clear>)","")
  x <- str_replace_all(x, "(</[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<#>)","")
  x <- gsub("<\\$[A-Z]>","", x)
  x <- gsub("<?>","", x, fixed = TRUE)
  x <- gsub("</?>","", x, fixed = TRUE)
  x <- gsub("<->","", x, fixed = TRUE)
  x <- gsub("</->","", x, fixed = TRUE)
  x <- gsub("<+>","", x, fixed = TRUE)
  x <- gsub("</+>","", x, fixed = TRUE)
  x <- gsub("<=>","", x, fixed = TRUE)
  x <- gsub("</=>","", x, fixed = TRUE)
  x <- gsub("<@>","", x, fixed = TRUE)
  x <- gsub("</@>","", x, fixed = TRUE)
  x <- gsub("<w>","", x, fixed = TRUE)
  x <- gsub("</w>","", x, fixed = TRUE)
  x <- gsub("&eacute;","", x, fixed = TRUE)
  x <- str_replace_all(x, "(<I>)","")
  x <- str_replace_all(x, "(</I>)","")
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<X>.*?<X>)","")
  x <- str_replace_all(x, "(<X>)","")
  x <- str_replace_all(x, "(</X>)","")
# Final clean up
  x <- str_replace_all(x, "(\\[.*?\\])","")
  x <- gsub("<","", x, fixed = TRUE)
  x <- gsub("[","", x, fixed = TRUE)
  x <- gsub("/","", x, fixed = TRUE)
  x <- gsub("-","", x, fixed = TRUE)
  x <- gsub("?","", x, fixed = TRUE)
  x <- gsub(">","", x, fixed = TRUE)
# WARNING: THEORETICAL ISSUE
  x <- gsub("Ill ","I'll ", x, fixed = TRUE)
  x <- gsub("Theres ","There's ", x, fixed = TRUE)
  x <- gsub(" theres "," there's ", x, fixed = TRUE)
  x <- gsub("Im ","I'm ", x, fixed = TRUE)
  x <- gsub("Theyre ","They're ", x, fixed = TRUE)
  x <- gsub(" theyre "," they're ", x, fixed = TRUE)
  x <- gsub("Hes ","He's ", x, fixed = TRUE)
  x <- gsub("Its","It's ", x, fixed = TRUE)
  x <- gsub(" its"," it's ", x, fixed = TRUE)
  x <- gsub("Itll","It'll ", x, fixed = TRUE)
  x <- gsub(" itll"," it'll ", x, fixed = TRUE)
  x <- gsub("Youre ","You're ", x, fixed = TRUE)
  x <- gsub(" youre "," you're ", x, fixed = TRUE)
  x <- gsub("Thats ","That's ", x, fixed = TRUE)
  x <- gsub(" thats "," that's ", x, fixed = TRUE)
#  x <- gsub("<.*?>","", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
# View results
#turns.clean
#turns.clean[[1]]
###############################################################
### --- Create a list which holds the number of words per turn
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(turns.clean, function(x){
  tokenized <- strsplit(x, " ")  }  )
# View results
#tokenized
# Now, we count the words elements(words) in each turn (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))    } )
# View results
#word.count
###############################################################
# Create a list which holds the number of turns
#Extract the number of turns for each speaker of all spoken files
turn.count.tmp1 <- lapply(word.count, function(x) {
  sapply(x, function(y) gsub(".*",  "1", y))  }  )
#Add names (file.ids) to the number of utterances
#names(turn.count.tmp1) <- file.id.tmp2
# Rename list
turn.count <- turn.count.tmp1
# View results
#turn.count
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, turn, turn.clean,
# turn.count, word.count)
###############################################################
speaker.and.unclean.turns <- mapply(cbind, speakers[], turns[], SIMPLIFY = F)
# View results
#speaker.and.unclean.turns
speaker.both.turns <- mapply(cbind, speaker.and.unclean.turns[], turns.clean[], SIMPLIFY = F)
names(speaker.both.turns) <- file.subfile.ids
# View results
#str(speaker.both.turns)
#speaker.both.turns
# Add file.subfile.ids to speaker.both.turns
speaker.both.turns.subfile <- mapply(cbind, speaker.both.turns[],names(speaker.both.turns[]), SIMPLIFY = F)
# View results
#speaker.both.turns.subfile
# Add turn.counts
speaker.both.turns.subfile.and.turn.count <- mapply(cbind, speaker.both.turns.subfile [], turn.count[], SIMPLIFY = F)
# View results
#speaker.both.turns.subfile.and.turn.count
speakerinfo1 <- mapply(cbind, speaker.both.turns.subfile.and.turn.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# View results
#speakerinfo1
# We now need to convert the elements of the fourth and fifth
# column into numeric elements
speakerinfo2 <-lapply(speakerinfo1, function(x) {
  X <- as.data.frame(x[])
  X[, 5] <- as.numeric(X[, 5])
  X[, 6] <- as.numeric(X[, 6])
  x <- X  }  )
# View results
#speakerinfo2
###############################################################
# Rename data for later kwic seraches
kwic.tb.ice.can <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum)))  } )
# View results
#word.count.result
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# View results
#overview.word.count.results
# Extract the turn counts for speakers in one file
turn.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum)))  } )
# View results
#turn.count.result
# Simplify the results
overview.turn.count.results <- sapply(turn.count.result, "[[", 1)
# View results
#overview.turn.count.results
###############################################################
# We now want to extract the speaker ids in vector format so
# that we can easily create a table out of the results
speaker.ids.tmp1 <- lapply(speakerinfo2, function(x) {
  x <- table(x[, 1])  }  )
speakers <- as.vector(unlist(sapply(speaker.ids.tmp1, function(x)  {
  x <- names(x)    }  )))
###############################################################
# We now want to extract the subfile ids in vector format so
# that we can easily create a table out of the results
# First we determine how many speakers are in a file
n.speakers.file <- sapply(overview.word.count.results, function(x) {
  length(x)  }  )
# Then, we create a matrix with the names to be replicates in column 1
# and the number of times they are supposed to replicated in column 2
names.and.nspeakers.tb <- cbind(names(n.speakers.file), n.speakers.file)
# Create an index for repetition
index <- rep(1:nrow(names.and.nspeakers.tb), names.and.nspeakers.tb [ , 2])
# And save the result in a vector
subfile.ids.tmp1 <- names.and.nspeakers.tb[index, ]
subfiles.tmp <- as.vector(subfile.ids.tmp1[, 1])
subfiles <- gsub(".*#", "", subfiles.tmp)
# View results
#subfiles
###############################################################
# From the speaker.ids vector, we also extract the file names and
# the speaker.ids
# First, let's extract the file names
files <- gsub("#.*", "", subfiles.tmp)
# View results
#files
###############################################################
# We now want to extract the turn counts for each speaker
# in vector format so that we can easily create a table out of
# the results
turn.count.list <- lapply(X = overview.turn.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) } ) } )
# View results
#turn.count.list
# Convert the list into a vector
turn.counts <- as.vector(unlist(turn.count.list))
# View results
#turn.counts
###############################################################
# We now want to extract the word counts for each speaker
# in vector format so that we can easily create a table out of
# the results
word.count.list <- lapply(overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)  } )  }  )
# View results
#word.count.list
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
# View results
#word.counts
# Create an object holding file, subfile and speaker info
file.speaker.id.tmp1 <- paste("<", files, "#", subfiles, ":", speakers, ">", collapse = "")
file.speaker.id.tmp2 <- str_replace_all(file.speaker.id.tmp1, " ","")
file.speaker.id.tmp3 <- lapply(file.speaker.id.tmp2, function(x)  {
  x <- strsplit( gsub("(>)", "\\1~", x), "~" )  }  )
file.speaker.id <- as.vector(unlist(file.speaker.id.tmp3))
# View results
#file.speaker.id
###############################################################
# We now want to create a table with speaker id, turn count and word count
# First, we create an index
id <- c(1:length(speakers))
# Now, we set up the data frame
speakerinfo.ice.can <- cbind(id, file.speaker.id, files, subfiles, speakers,
  turn.counts, word.counts)
speakerinfo.ice.can <- as.data.frame(speakerinfo.ice.can)
colnames(speakerinfo.ice.can) <- c("id", "file.speaker.id", "text.id",
  "subfile.id", "speaker.id", "turn.count", "word.count")
# View results
#speakerinfo.ice.can
# View results (without empty rows)
speakerinfo.ice.can.1 <- speakerinfo.ice.can[!speakerinfo.ice.can[, 2] == "", ]
speakerinfo.ice.can.1[, 6] <- as.numeric(speakerinfo.ice.can.1[, 6])
speakerinfo.ice.can.1[, 7] <- as.numeric(speakerinfo.ice.can.1[, 7])
speakerinfo.ice.can.1[, 1] <- 1:length(speakerinfo.ice.can.1[, 1])
rownames(speakerinfo.ice.can.1) <- speakerinfo.ice.can.1[, 1]
# View results
#speakerinfo.ice.can.1
################################################################
################################################################
# Load biodata
biodata.tmp1 <- read.delim(bio.can, header = T, sep = "\t")
# Inspect data
#head(biodata.tmp1)
# Convert all column names to lower case
colnames(biodata.tmp1) <- tolower(colnames(biodata.tmp1))
# Repair broken cells
biodata.tmp1[841, 14] <- NA # previously "Roth"
# Replace NA with character "NA" so they are displayed in tables
biodata.tmp2 <- as.matrix(t(apply(biodata.tmp1, 1, FUN = function(x) {
  ifelse(is.na(x), "NA", x)  }  )), ncol = length(biodata.tmp1[1, ]))
###############################################################
# Replace abbreviations with actual values
# date of recording
# Create a dummy vector holding the dates
new.dates.tmp1 <- biodata.tmp2[, 6]
# Clean dates
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "11-Nov-1004", "1994")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "1991\\?", "1991")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "dates", "1990")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "91", "1991")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "94", "1994")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "95", "1995")
new.dates.tmp1 <- str_replace_all(new.dates.tmp1, "1919", "19")
new.dates.tmp1 <- sapply(new.dates.tmp1, function(x)  {
  x <- str_replace_all(x, ".* ", "")
  x <- str_replace_all(x, ".*\\.", "")  }  )
# Inspect the resulting dates
#table(new.dates.tmp1)
# Insert the cleaned dates into the biodata
biodata.tmp2[, 6] <- new.dates.tmp1
# gender
biodata.tmp2[, 14] <- str_replace_all(biodata.tmp2[, 14], "M", "male")
biodata.tmp2[, 14] <- str_replace_all(biodata.tmp2[, 14], "F", "female")
# Overview
#table(biodata.tmp2[, 14])
# Add an additional column (age)
biodata.tmp3 <- cbind(biodata.tmp2, biodata.tmp2[, 15], biodata.tmp2[, 15])
colnames(biodata.tmp3) <- c(colnames(biodata.tmp2), "age.exact", "age.orig")
# Add additional columns (other.languages, self.reported.ethnicity,
# occupation, educational.level)
biodata.tmp4 <- cbind(biodata.tmp3, biodata.tmp3[, 19], biodata.tmp3[, 20], biodata.tmp3[, 21], biodata.tmp3[, 22])
colnames(biodata.tmp4) <- c(colnames(biodata.tmp3), "additonal.languages", "ethnicity", "occupation.group", "education")
biodata.tmp3 <- biodata.tmp4
# age
biodata.tmp4[, 15] <- as.vector(unlist(sapply(biodata.tmp4[, 15], function(x) {
  ifelse(x == "12 or 13", "10-18",
  ifelse(x == "12", "10-18",
  ifelse(x == "5?", NA,
  ifelse(x == "28", "25-30",
  ifelse(x == "31", "31-40",
  ifelse(x == "32", "31-40",
  ifelse(x == "33", "31-40",
  ifelse(x == "44", "41-50",
  ifelse(x == "55", "51-60",
  ifelse(x == "56", "51-60",
  ifelse(x == "1", "10-18",
  ifelse(x == "2", "19-24",
  ifelse(x == "3", "25-30",
  ifelse(x == "4", "31-40",
  ifelse(x == "5", "41-50",
  ifelse(x == "6", "51-60",
  ifelse(x == "7", "61+",
  ifelse(x == "NA", NA, x <- x  ))))))))))))))))))  }  )))
# age.exact
biodata.tmp4[, 27] <- as.vector(unlist(sapply(biodata.tmp4[, 27], function(x) {
  ifelse(x == "12 or 13", NA,
  ifelse(x == "5?", NA,
  ifelse(x == "1", NA,
  ifelse(x == "2", NA,
  ifelse(x == "3", NA,
  ifelse(x == "4", NA,
  ifelse(x == "5", NA,
  ifelse(x == "6", NA,
  ifelse(x == "7", NA,
  ifelse(x == "NA", NA, x <- x  ))))))))))  }  )))
# Overview
#table(biodata.tmp3[, 15])
#table(biodata.tmp3[, 27])
# nationality
biodata.tmp4[, 17] <- as.vector(unlist(sapply(biodata.tmp4[, 17], function(x) {
  ifelse(x == "none given", NA,
  ifelse(x == "0", "Canadian",
  ifelse(x == "a", "Canadian",
  ifelse(x == "g", "British-Canadian",
  ifelse(x == "h", "British",
  ifelse(x == "i", "Canadian-American",
  ifelse(x == "j", "Israeli-Canadian",
  ifelse(x == "k", "Malaysian",
  ifelse(x == "NA", NA, x  )))))))))  }  )))
# Overview
#table(biodata.tmp4[, 17])
# mother tongue
biodata.tmp4[, 18] <- as.vector(unlist(sapply(biodata.tmp4[, 18], function(x)  {
  x <- str_trim(x, side = "both")
  ifelse(x == "A", "English",
  ifelse(x == "A+", "Balanced Bilingual (Canadian English/French)",
  ifelse(x == "B", "French (Canadian)",
  ifelse(x == "D", "German",
  ifelse(x == "E", "Italian",
  ifelse(x == "G", "Polish",
  ifelse(x == "I", "Spanish",
  ifelse(x == "K", "Gaelic (Scots)",
  ifelse(x == "L", "Hebrew",
  ifelse(x == "M", "Greek",
  ifelse(x == "N", "Hungarian",
  ifelse(x == "O", "Chinese",
  ifelse(x == "P", "Marathi",
  ifelse(x == "Q", "Low German/Plautdietsch",
  ifelse(x == "R", "Hindi",
  ifelse(x == "S", "Gujarati",
  ifelse(x == "U", "Kannada",
  ifelse(x == "V", "Cree",
  ifelse(x == "W", "Mohawk",
  ifelse(x == "X", "Aboriginal (unnamed language)",
  ifelse(x == "Y", "Unknown/Unnamed (when L2+ is given as English)",
  ifelse(x == "Z", "Portuguese",
  ifelse(x == "ZA", "Eastern European",
  ifelse(x == "NA", NA,
  ifelse(x == "I or A", NA,
  ifelse(x == "A, N", NA,
  ifelse(x == "D, Q", NA, x <- x)))))))))))))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 18])
# other.languages
biodata.tmp4[, 19] <- as.vector(unlist(sapply(biodata.tmp4[, 19], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "A", "English",
  ifelse(x == "A or I", "English, Spanish",
  ifelse(x == "A, B",  "English, French (Canadian)",
  ifelse(x == "A, B, J",  "English, French (Canadian), Russian",
  ifelse(x == "B",  "French (Canadian)",
  ifelse(x == "B, A",  "English, French (Canadian)",
  ifelse(x == "B, D",  "French (Canadian), German",
  ifelse(x == "B, F",  "French (Canadian), Romanian",
  ifelse(x == "B, I",  "French (Canadian), Spanish",
  ifelse(x == "B, L",  "French (Canadian), Hebrew",
  ifelse(x == "B, M",  "French (Canadian), Greek",
  ifelse(x == "B, R S, T, U",  "French (Canadian), Hindi, Gujarati, Nepali, Kannada",
  ifelse(x == "B, Z",  "French (Canadian), Portuguese",
  ifelse(x == "D, B",  "French (Canadian), German",
  ifelse(x == "H, B",  "French (Canadian), Swedish",
  ifelse(x == "I",  "Spanish",
  ifelse(x == "K",  "Gaelic (Scots)",
  ifelse(x == "K, B",  "French (Canadian), Gaelic (Scots)",
  ifelse(x == "L",  "Hebrew",
  ifelse(x == "NA",  "NA",
  ifelse(x == "O, B",  "French (Canadian), Chinese",
  ifelse(x == "V",  "Cree",
  ifelse(x == "V, B",  "French (Canadian), Cree",
  ifelse(x == "ZB?",  "French (Canadian), Portuguese", x <- x  ))))))))))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 19])
# Copy this column
biodata.tmp4[, 29] <- biodata.tmp4[, 19]
##################################################################
### --- Recoding
### --- ETHNICITY
##################################################################
# ETHNICITY 1
# self.reported ethnicity
biodata.tmp4[, 20] <- as.vector(unlist(sapply(biodata.tmp4[, 20], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "a", "Canadian",
  ifelse(x == "a-j+ze+r", "Canadian/British (English) and Spanish and German",
  ifelse(x == "a-l-k", "Canadian/Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Irish",
  ifelse(x == "b", "Native Canadian",
  ifelse(x == "b-lk-r-zi-ze", "Native Canadian/Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Irish/German/Welsh/Spanish",
  ifelse(x == "c", "English Canadian/Anglo(-phone)/Anglo-Canadian",
  ifelse(x == "c,g", "English Canadian/Anglo(-phone)/Anglo-Canadian/Dene",
  ifelse(x == "c+p", "English Canadian/Anglo(-phone)/Anglo-Canadian and Eastern-European",
  ifelse(x == "ca-d", "English-raised/French-Canadian/Francophone/Franco-Canadian",
  ifelse(x == "cb-k", "Angloid/Irish",
  ifelse(x == "cb-u", "Angloid/Italian",
  ifelse(x == "cb-x-a", "Angloid/Norwegian/Canadian",
  ifelse(x == "cb-zj", "Angloid/American",
  ifelse(x == "d", "French-Canadian/Francophone/Franco-Canadian",
  ifelse(x == "d-k", "French-Canadian/Francophone/Franco-Canadian/Irish",
  ifelse(x == "e", "Anglophone/Francophone or English-French Canadian",
  ifelse(x == "e-f", "Anglophone/Francophone or English-French Canadian/Acadian",
  ifelse(x == "f", "Acadian",
  ifelse(x == "g", "Dene",
  ifelse(x == "h", "Mennonite",
  ifelse(x == "i", "Anglo-Celtic/Anglo-Saxon-Celtic/Celtic",
  ifelse(x == "i-a", "Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Canadian",
  ifelse(x == "i-zf", "Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Texan",
  ifelse(x == "i+d", "Anglo-Celtic/Anglo-Saxon-Celtic/Celtic and French-Canadian/Francophone/Franco-Canadian",
  ifelse(x == "i=zf", "Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Texan",
  ifelse(x == "j", "British (English)",
  ifelse(x == "j-a", "British (English)/Canadian",
  ifelse(x == "j-d", "British (English)/French-Canadian/Francophone/Franco-Canadian",
  ifelse(x == "j-l-a", "British (English)/Scottish/Canadian",
  ifelse(x == "jk", "British/English-Irish",
  ifelse(x == "k-a", "Irish/Canadian",
  ifelse(x == "k-d", "Irish/French-Canadian/Francophone/Franco-Canadian",
  ifelse(x == "k+d+n", "Irish and rench-Canadian/Francophone/Franco-Canadian and Danish",
  ifelse(x == "l", "Scottish",
  ifelse(x == "l-a", "Scottish/Canadian",
  ifelse(x == "l-r-a", "Scottish/German/Canadian",
  ifelse(x == "l-zc", "Scottish/Slavic",
  ifelse(x == "lj", "Scottish/British (English)",
  ifelse(x == "lk", "Scottish-Irish",
  ifelse(x == "m-a", "Chinese/Canadian",
  ifelse(x == "o", "Dutch", x)))))))))))))))))))))))))))))))))))))))))  }  )))
biodata.tmp4[, 20] <- as.vector(unlist(sapply(biodata.tmp4[, 20], function(x) {
  ifelse(x == "q", "European",
  ifelse(x == "q-a", "European/Canadian",
  ifelse(x == "q-j-za", "European/British (English)/Romanian",
  ifelse(x == "q-w", "European/Jewish",
  ifelse(x == "r", "German",
  ifelse(x == "r-a", "German/Canadian",
  ifelse(x == "t-a", "Hungarian/Canadian",
  ifelse(x == "u", "Italian",
  ifelse(x == "u-a", "Italian/Canadian",
  ifelse(x == "u-i-f", "Italian/Anglo-Celtic/Anglo-Saxon-Celtic/Celtic/Acadian",
  ifelse(x == "v", "Japanese",
  ifelse(x == "v-a", "Japanese/Canadian",
  ifelse(x == "w", "Jewish",
  ifelse(x == "w-a", "Jewish/Canadian",
  ifelse(x == "x", "Norwegian",
  ifelse(x == "x-a", "Norwegian/Canadian",
  ifelse(x == "x-j", "Norwegian/British (English)",
  ifelse(x == "y", "Penan",
  ifelse(x == "z-s", "Polish/Greek",
  ifelse(x == "zb-a", "Scandinavian/Canadian",
  ifelse(x == "zd", "South Asian",
  ifelse(x == "zg-a", "Ukrainian/Canadian",
  ifelse(x == "zh", "WASP",
  ifelse(x == "zk-a", "Colombian/Canadian", x <- x  ))))))))))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 20])
##################################################################
### --- ETHNICITY 2
### --- based on:
# Bowsher, Kevin. "The code systems used within the Metropolitan
# Police Service (MPS) to formally record ethnicity". MPA briefing
# paper. Metropolitan Police Authority. Retrieved 14 December 2012.
##################################################################
biodata.tmp4[, 30] <- as.vector(unlist(sapply(biodata.tmp4[, 30], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "a", "White person, northern European type",
  ifelse(x == "a-j+ze+r", "White person, northern European type & Mediterranean European/Hispanic",
  ifelse(x == "a-l-k", "White person, northern European type",
  ifelse(x == "b", "Native Canadian",
  ifelse(x == "b-lk-r-zi-ze", "White person, northern European type & Native Canadian",
  ifelse(x == "c", "White person, northern European type",
  ifelse(x == "c,g", "White person, northern European type & Native Canadian",
  ifelse(x == "c+p", "White person, northern European type",
  ifelse(x == "ca-d", "White person, northern European type",
  ifelse(x == "cb-k", "White person, northern European type",
  ifelse(x == "cb-u", "White person, northern European type & Mediterranean European/Hispanic",
  ifelse(x == "cb-x-a", "White person, northern European type",
  ifelse(x == "cb-zj", "White person, northern European type",
  ifelse(x == "d", "White person, northern European type",
  ifelse(x == "d-k", "White person, northern European type",
  ifelse(x == "e", "White person, northern European type",
  ifelse(x == "e-f", "White person, northern European type & Native Canadian",
  ifelse(x == "f", "Native Canadian",
  ifelse(x == "g", "Native Canadian",
  ifelse(x == "h", "Native Canadian",
  ifelse(x == "i", "White person, northern European type",
  ifelse(x == "i-a", "White person, northern European type",
  ifelse(x == "i-zf", "White person, northern European type",
  ifelse(x == "i+d", "White person, northern European type",
  ifelse(x == "i=zf", "White person, northern European type",
  ifelse(x == "j", "White person, northern European type",
  ifelse(x == "j-a", "White person, northern European type",
  ifelse(x == "j-d", "White person, northern European type",
  ifelse(x == "j-l-a", "White person, northern European type",
  ifelse(x == "jk", "White person, northern European type",
  ifelse(x == "k-a", "White person, northern European type",
  ifelse(x == "k-d", "White person, northern European type",
  ifelse(x == "k+d+n", "White person, northern European type",
  ifelse(x == "l", "White person, northern European type",
  ifelse(x == "l-a", "White person, northern European type",
  ifelse(x == "l-r-a", "White person, northern European type",
  ifelse(x == "l-zc", "White person, northern European type",
  ifelse(x == "lj", "White person, northern European type",
  ifelse(x == "lk", "White person, northern European type",
  ifelse(x == "m-a", "White person, northern European type & Chinese, Japanese, or South-East Asian person",
  ifelse(x == "o", "White person, northern European type", x <- x
  )))))))))))))))))))))))))))))))))))))))))  }  )))
biodata.tmp4[, 30] <- as.vector(unlist(sapply(biodata.tmp4[, 30], function(x) {
  ifelse(x == "q", "White person, northern European type",
  ifelse(x == "q-a", "White person, northern European type",
  ifelse(x == "q-j-za", "White person, northern European type",
  ifelse(x == "q-w", "White person, northern European type",
  ifelse(x == "r", "White person, northern European type",
  ifelse(x == "r-a", "White person, northern European type",
  ifelse(x == "t-a", "White person, northern European type",
  ifelse(x == "u", "Mediterranean European/Hispanic",
  ifelse(x == "u-a", "White person, northern European type & Mediterranean European/Hispanic",
  ifelse(x == "u-i-f", "White person, northern European type & Mediterranean European/Hispanic & Native Canadian",
  ifelse(x == "v", "Chinese, Japanese, or South-East Asian person",
  ifelse(x == "v-a", "White person, northern European type & Chinese, Japanese, or South-East Asian person",
  ifelse(x == "w", NA,
  ifelse(x == "w-a", "White person, northern European type",
  ifelse(x == "x", "White person, northern European type",
  ifelse(x == "x-a", "White person, northern European type",
  ifelse(x == "x-j", "White person, northern European type",
  ifelse(x == "y", "Native Canadian",
  ifelse(x == "z-s", "White person, northern European type & Mediterranean European/Hispanic",
  ifelse(x == "zb-a", "White person, northern European type",
  ifelse(x == "zd", "Chinese, Japanese, or South-East Asian person",
  ifelse(x == "zg-a", "White person, northern European type",
  ifelse(x == "zh", "White person, northern European type",
  ifelse(x == "zk-a", "White person, northern European type & Mediterranean European/Hispanic",
  ifelse(x == "NA", NA, x
  )))))))))))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 30])
##################################################################
### --- Recoding
### --- EDUCATION
##################################################################
# EDUCATION 1
# educational.level
biodata.tmp4[, 22] <- as.vector(unlist(sapply(biodata.tmp4[, 22], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "A", "secondary schooling (not diploma)",
  ifelse(x == "A, C", "secondary schooling (not diploma), unspecifelseied post-secondary",
  ifelse(x == "A, EB", "secondary schooling (not diploma), unspecifelseied CEGEP",
  ifelse(x == "B", "high school diploma",
  ifelse(x == "CJSR", "NA",
  ifelse(x == "D", "nursing diploma",
  ifelse(x == "EA", "DEC - diplome d'etudes collegiale - CEGEP",
  ifelse(x == "EA (HEALTHSCI)", "DEC - diplome d'etudes collegiale - CEGEP (HEALTHSCI)", x
  ))))))))  }  )))
biodata.tmp4[, 22] <- as.vector(unlist(sapply(biodata.tmp4[, 22], function(x) {
  ifelse(x == "EA (LANG)", "DEC - diplome d'etudes collegiale - CEGEP (LANG)",
  ifelse(x == "EA (LANGLIT)", "DEC - diplome d'etudes collegiale - CEGEP (LANGLIT)",
  ifelse(x == "EA (SCI)", "DEC - diplome d'etudes collegiale - CEGEP (SCI)",
  ifelse(x == "EC (SOUND, DATA)", "unspecifelseied - college (SOUND, DATA)",
  ifelse(x == "G", "Bachelors (unspecifelseied) - university",
  ifelse(x == "G (MUS, TEACH)", "Bachelors (unspecifelseied) - university (MUS, TEACH)",
  ifelse(x == "GA (INSU)", "incomplete Bachelors (unspecifelseied) - university (INSU)",
  ifelse(x == "H (HEALTH)", "Bachelors in progress - university (HEALTH)",
  ifelse(x == "I", "BA - university",
  ifelse(x == "I-GE", "BA - university, NA",
  ifelse(x == "I-J (HIST, POLSC), U", "BA - university, Honours - university (HIST, POLSC), JD or LLB",
  ifelse(x == "I-J, Z", "BA - university, Honours - university, MIR",
  ifelse(x == "I (ANTH)", "BA - university (ANTH)",
  ifelse(x == "I (BUS)", "BA - university (BUS)",
  ifelse(x == "I (CHST, ED)", "BA - university (CHST, ED)",
  ifelse(x == "I (COMS)", "BA - university (COMS)",
  ifelse(x == "I (ED)", "BA - university (ED)",
  ifelse(x == "I (ENG)", "BA - university (ENG)",
  ifelse(x == "I (ENG), G (THEO), ZB (ENG)", "BA - university (ENG), Bachelors (unspecifelseied) - university (THEO), MA (ENG)", x <- x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 22] <- as.vector(unlist(sapply(biodata.tmp4[, 22], function(x) {
  ifelse(x == "I (ENG), LK", "BA - university (ENG), BEd TESL - university",
  ifelse(x == "I (ENG, FILM)", "BA - university (ENG, FILM)",
  ifelse(x == "I (FILM)", "BA - university (FILM)",
  ifelse(x == "I (FILM, ENG)", "BA - university (FILM, ENG)",
  ifelse(x == "I (FR, TESL), W (DENHYG)", "BA - university (FR, TESL), Post-Baccalaureate Diploma (DENHYG)",
  ifelse(x == "I (HIST)", "BA - university (HIST)",
  ifelse(x == "I (HIST, BIO), W (ED)", "BA - university (HIST, BIO), Post-Baccalaureate Diploma (ED)",
  ifelse(x == "I (SOC)", "BA - university (SOC)",
  ifelse(x == "I (SOC), LK", "BA - university (SOC), BEd TESL - university",
  ifelse(x == "I (WOMS)", "BA - university (WOMS)",
  ifelse(x == "I, J (LANG)", "BA - university, Honours - university (LANG)",
  ifelse(x == "I, K. (Geol)", "BA - university, BE - university (Geol)",
  ifelse(x == "I, L", "BA - university, BEd - university",
  ifelse(x == "I, L, ZC", "BA - university, BEd - university, MA in progress",
  ifelse(x == "I, U", "BA - university, JD or LLB",
  ifelse(x == "I, U, ZA", "BA - university, JD or LLB, LLM",
  ifelse(x == "I, ZB, U", "BA - university, JD or LLB, MA",
  ifelse(x == "I, ZB, ZH", "BA - university, MA, PhD",
  ifelse(x == "I, ZB, ZI (ENG)", "BA - university, JD or LLB, PhD in progress (ENG)",
  ifelse(x == "I,L", "BA - university, BEd - university",
  ifelse(x == "L", "BEd - university",
  ifelse(x == "L (CHST)", "BEd - university (CHST)", x <- x
  ))))))))))))))))))))))  }  )))
biodata.tmp4[, 22] <- as.vector(unlist(sapply(biodata.tmp4[, 22], function(x) {
  ifelse(x == "L (EARLYED)", "BEd - university (EARLYED)",
  ifelse(x == "LK", "BEd TESL - university",
  ifelse(x == "M", "BCompSci - university",
  ifelse(x == "N", "BSocial Work - university",
  ifelse(x == "NA", "NA",
  ifelse(x == "O", "BCom - university",
  ifelse(x == "P", "BAg/BSc(Agr)- university",
  ifelse(x == "Q, BUS AD", "BAS - university (BUS AD)",
  ifelse(x == "R", "BSc - university",
  ifelse(x == "R (BIO)", "BSc - university (BIO)",
  ifelse(x == "R (PSYC)", "BSc - university (PSYC)",
  ifelse(x == "R (PSYC), G (CHST)", "BSc - university (PSYC), Bachelors (unspecifelseied) - university (CHST)",
  ifelse(x == "R, I", "BSc - university, BA - university",
  ifelse(x == "S", "BSS - university",
  ifelse(x == "T", "BFA",
  ifelse(x == "T (FILM)", "BFA (FILM)",
  ifelse(x == "U", "JD or LLB",
  ifelse(x == "U, G", "JD or LLB, Bachelors (unspecifelseied) - university",
  ifelse(x == "U, L, G", "JD or LLB, BEd - university, Bachelors (unspecifelseied) - university",
  ifelse(x == "V", "Teacher's Certifelseicate",
  ifelse(x == "W (COMS)", "Post-Baccalaureate Diploma (COMS)",
  ifelse(x == "W (MUS)", "Post-Baccalaureate Diploma (MUS)",
  ifelse(x == "X (MUSPER)", "Graduate Diploma (MUSPER)",
  ifelse(x == "Y", "MBA",
  ifelse(x == "Y (ORGB)", "MBA (ORGB)",
  ifelse(x == "ZB", "MA",
  ifelse(x == "ZB (APPLING)", "MA (APPLING)",
  ifelse(x == "ZB (ART HIST)", "MA (ART HIST)",
  ifelse(x == "ZB (ARTHIST)", "MA",
  ifelse(x == "ZB (COMS)", "MA (COMS)", x <- x
  ))))))))))))))))))))))))))))))  }  )))
biodata.tmp4[, 22] <- as.vector(unlist(sapply(biodata.tmp4[, 22], function(x) {
  ifelse(x == "ZB (EARLYED)", "MA (EARLYED)",
  ifelse(x == "ZB (ED)", "MA (ED)",
  ifelse(x == "ZB (EdTech)", "MA (EDTECH)",
  ifelse(x == "ZB (EDTECH)", "MA (EDTECH)",
  ifelse(x == "ZB (ENG)", "MA (ENG)",
  ifelse(x == "ZB (FR)", "MA (FR)",
  ifelse(x == "ZB (LBSC)", "MA (LBSC)",
  ifelse(x == "ZB (PLANNING)", "MA (PLANNING)",
  ifelse(x == "ZB (POLSC)", "MA (POLSC)",
  ifelse(x == "ZB (Theo), ZG", "MA (Theo), STM (Master of Sacred Theology)",
  ifelse(x == "ZB (TRANS)", "MA (TRANS)",
  ifelse(x == "ZB, ZI", "MA, PhD in progress",
  ifelse(x == "ZB, ZI (PSYC)", "MA, PhD in progress (PSYC)",
  ifelse(x == "ZD", "MEd",
  ifelse(x == "ZD, I", "MEd, BA - university",
  ifelse(x == "ZE", "MSc",
  ifelse(x == "ZE (OCHLTH)", "MSc (OCHLTH)",
  ifelse(x == "ZH", "PhD",
  ifelse(x == "ZH (EDCOM)", "PhD (EDCOM)",
  ifelse(x == "ZH (FR)", "PhD (FR)",
  ifelse(x == "ZH (HIST)", "PhD (HIST)",
  ifelse(x == "ZH (MUSARTS)", "PhD (MUSARTS)",
  ifelse(x == "ZH (PHYS)", "PhD (PHYS)",
  ifelse(x == "ZH (PSYC)", "PhD (PSYC)",
  ifelse(x == "ZH (PSYLING)", "PhD (PSYLING)",
  ifelse(x == "ZI", "PhD in progress",
  ifelse(x == "ZI (HIST)", "PhD in progress (HIST)",
  ifelse(x == "ZJ", "EdD",
  ifelse(x == "ZK", "Master of social work", x <- x
  )))))))))))))))))))))))))))))  }  )))
##################################################################
# EDUCATION 2
biodata.tmp4[, 32] <- as.vector(unlist(sapply(biodata.tmp4[, 32], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "A", "Secondary education or lower",
  ifelse(x == "A, C", "Secondary education or lower",
  ifelse(x == "A, EB", "Secondary education or lower",
  ifelse(x == "B", "Secondary education or lower",
  ifelse(x == "CJSR", "NA",
  ifelse(x == "D", "Tertiary education - BA",
  ifelse(x == "EA", "Tertiary education - BA",
  ifelse(x == "EA (HEALTHSCI)", "Tertiary education - BA", x
  ))))))))  }  )))
biodata.tmp4[, 32] <- as.vector(unlist(sapply(biodata.tmp4[, 32], function(x) {
  ifelse(x == "EA (LANG)", "Tertiary education - BA",
  ifelse(x == "EA (LANGLIT)", "Tertiary education - BA",
  ifelse(x == "EA (SCI)", "Tertiary education - BA",
  ifelse(x == "EC (SOUND, DATA)", "Tertiary education - BA",
  ifelse(x == "G", "Tertiary education - BA",
  ifelse(x == "G (MUS, TEACH)", "Tertiary education - BA",
  ifelse(x == "GA (INSU)", "Tertiary education - BA",
  ifelse(x == "H (HEALTH)", "Tertiary education - BA",
  ifelse(x == "I", "Tertiary education - BA",
  ifelse(x == "I-GE", "Tertiary education - BA",
  ifelse(x == "I-J (HIST, POLSC), U", "Tertiary education - BA",
  ifelse(x == "I-J, Z", "Tertiary education - BA",
  ifelse(x == "I (ANTH)", "Tertiary education - BA",
  ifelse(x == "I (BUS)", "Tertiary education - BA",
  ifelse(x == "I (CHST, ED)", "Tertiary education - BA",
  ifelse(x == "I (COMS)", "Tertiary education - BA",
  ifelse(x == "I (ED)", "Tertiary education - BA",
  ifelse(x == "I (ENG)", "Tertiary education - BA",
  ifelse(x == "I (ENG), G (THEO), ZB (ENG)", "Tertiary education - MA or higher", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 32] <- as.vector(unlist(sapply(biodata.tmp4[, 32], function(x) {
  ifelse(x == "I (ENG), LK", "Tertiary education - BA",
  ifelse(x == "I (ENG, FILM)", "Tertiary education - BA",
  ifelse(x == "I (FILM)", "Tertiary education - BA",
  ifelse(x == "I (FILM, ENG)", "Tertiary education - BA",
  ifelse(x == "I (FR, TESL), W (DENHYG)", "Tertiary education - BA",
  ifelse(x == "I (HIST)", "Tertiary education - BA",
  ifelse(x == "I (HIST, BIO), W (ED)", "Tertiary education - BA",
  ifelse(x == "I (SOC)", "Tertiary education - BA",
  ifelse(x == "I (SOC), LK", "Tertiary education - BA",
  ifelse(x == "I (WOMS)", "Tertiary education - BA" ,
  ifelse(x == "I, J (LANG)", "Tertiary education - BA",
  ifelse(x == "I, K. (Geol)", "Tertiary education - BA",
  ifelse(x == "I, L", "Tertiary education - BA",
  ifelse(x == "I, L, ZC", "Tertiary education - MA or higher",
  ifelse(x == "I, U", "Tertiary education - BA",
  ifelse(x == "I, U, ZA", "Tertiary education - BA",
  ifelse(x == "I, ZB, U", "Tertiary education - MA or higher",
  ifelse(x == "I, ZB, ZH", "Tertiary education - MA or higher",
  ifelse(x == "I, ZB, ZI (ENG)", "Tertiary education - MA or higher",
  ifelse(x == "I,L", "Tertiary education - BA",
  ifelse(x == "L", "Tertiary education - BA",
  ifelse(x == "L (CHST)", "Tertiary education - BA", x
  ))))))))))))))))))))))  }  )))
biodata.tmp4[, 32] <- as.vector(unlist(sapply(biodata.tmp4[, 32], function(x) {
  ifelse(x == "L (EARLYED)", "Tertiary education - BA",
  ifelse(x == "LK", "Tertiary education - BA",
  ifelse(x == "M", "Tertiary education - BA",
  ifelse(x == "N", "Tertiary education - BA",
  ifelse(x == "NA", "NA",
  ifelse(x == "O", "Tertiary education - BA",
  ifelse(x == "P", "Tertiary education - BA",
  ifelse(x == "Q, BUS AD", "Tertiary education - BA",
  ifelse(x == "R", "Tertiary education - BA",
  ifelse(x == "R (BIO)", "Tertiary education - BA",
  ifelse(x == "R (PSYC)", "Tertiary education - BA",
  ifelse(x == "R (PSYC), G (CHST)", "Tertiary education - BA",
  ifelse(x == "R, I", "Tertiary education - BA",
  ifelse(x == "S", "Tertiary education - BA",
  ifelse(x == "T", "Tertiary education - BA",
  ifelse(x == "T (FILM)", "Tertiary education - BA",
  ifelse(x == "U", "Tertiary education - BA",
  ifelse(x == "U, G", "Tertiary education - BA",
  ifelse(x == "U, L, G", "Tertiary education - BA",
  ifelse(x == "V", "Tertiary education - BA",
  ifelse(x == "W (COMS)", "Tertiary education - BA",
  ifelse(x == "W (MUS)", "Tertiary education - BA",
  ifelse(x == "X (MUSPER)", "NA",
  ifelse(x == "Y", "Tertiary education - MA or higher",
  ifelse(x == "Y (ORGB)", "Tertiary education - MA or higher",
  ifelse(x == "ZB", "Tertiary education - MA or higher",
  ifelse(x == "ZB (APPLING)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (ART HIST)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (ARTHIST)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (COMS)", "Tertiary education - MA or higher", x
  ))))))))))))))))))))))))))))))  }  )))
biodata.tmp4[, 32] <- as.vector(unlist(sapply(biodata.tmp4[, 32], function(x) {
  ifelse(x == "ZB (EARLYED)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (ED)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (EdTech)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (EDTECH)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (ENG)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (FR)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (LBSC)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (PLANNING)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (POLSC)", "Tertiary education - MA or higher",
  ifelse(x == "ZB (Theo), ZG", "Tertiary education - MA or higher",
  ifelse(x == "ZB (TRANS)", "Tertiary education - MA or higher",
  ifelse(x == "ZB, ZI", "Tertiary education - MA or higher",
  ifelse(x == "ZB, ZI (PSYC)", "Tertiary education - MA or higher",
  ifelse(x == "ZD", "Tertiary education - MA or higher",
  ifelse(x == "ZD, I", "Tertiary education - MA or higher",
  ifelse(x == "ZE", "Tertiary education - MA or higher",
  ifelse(x == "ZE (OCHLTH)", "Tertiary education - MA or higher",
  ifelse(x == "ZH", "Tertiary education - MA or higher",
  ifelse(x == "ZH (EDCOM)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (FR)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (HIST)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (MUSARTS)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (PHYS)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (PSYC)", "Tertiary education - MA or higher",
  ifelse(x == "ZH (PSYLING)", "Tertiary education - MA or higher",
  ifelse(x == "ZI", "Tertiary education - MA or higher",
  ifelse(x == "ZI (HIST)", "Tertiary education - MA or higher",
  ifelse(x == "ZJ", NA,
  ifelse(x == "ZK", "Tertiary education - MA or higher",
  ifelse(x == "NA", NA, x
  ))))))))))))))))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 32])
##################################################################
### --- Recoding
### --- OCCUPATION
##################################################################
# OCCUPATION 1
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "", NA,
  ifelse(x == "a0", "(at-home-)mother",
  ifelse(x == "a1, a2", "critic, curator",
  ifelse(x == "a1, a47", "critic, politician",
  ifelse(x == "a10", "ecologist",
  ifelse(x == "a11", "editor",
  ifelse(x == "a12, e", "educator, administrator",
  ifelse(x == "a13", "environmental activist",
  ifelse(x == "a14", "environmental analyst",
  ifelse(x == "a15", "environmental specialist",
  ifelse(x == "a16", "ESL teacher",
  ifelse(x == "a17", "farmer",
  ifelse(x == "a17, j", "farmer, artist",
  ifelse(x == "a18", "fashion designer",
  ifelse(x == "a19", "film-maker",
  ifelse(x == "a19, q, a21", "film-maker, choreographer, florist",
  ifelse(x == "a20", "financial analyst",
  ifelse(x == "a22, e", " freelance, administrator",
  ifelse(x == "a23", "futures trader", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "", "NA",
  ifelse(x == "a0", "(at-home-)mother",
  ifelse(x == "a1, a2", "critic, curator",
  ifelse(x == "a1, a47", "critic, politician",
  ifelse(x == "a10", "ecologist",
  ifelse(x == "a11", "editor",
  ifelse(x == "a12, e", "educator, administrator",
  ifelse(x == "a13", "environmental activist",
  ifelse(x == "a14", "environmental analyst",
  ifelse(x == "a15", "environmental specialist",
  ifelse(x == "a16", "ESL teacher",
  ifelse(x == "a17", "farmer",
  ifelse(x == "a17, j", "farmer, artist",
  ifelse(x == "a18", "fashion designer",
  ifelse(x == "a19", "film-maker",
  ifelse(x == "a19, q, a21", "film-maker, choreographer, florist",
  ifelse(x == "a20", "financial analyst",
  ifelse(x == "a22, e", " freelance, administrator",
  ifelse(x == "a23", "futures trader", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "a24", "hobby teacher",
  ifelse(x == "a25, a70", "housewifelsee/homemaker, student (tertiary)",
  ifelse(x == "a26", "instructor",
  ifelse(x == "a28", "journalist",
  ifelse(x == "a28, L", "journalist, author",
  ifelse(x == "a29, a26", "lab technician, instructor",
  ifelse(x == "a3", "custodian",
  ifelse(x == "a30", "language teacher",
  ifelse(x == "a31", "lawyer",
  ifelse(x == "a31, a51", "lawyer, Professor (university, includes Associate and Assistant)",
  ifelse(x == "a32", "lecturer",
  ifelse(x == "a33", "manager",
  ifelse(x == "a34", "mayor",
  ifelse(x == "a34-a6", "mayor, deputy",
  ifelse(x == "a35", "military analyst",
  ifelse(x == "a36", "member of parliament",
  ifelse(x == "a37", "meteorologist",
  ifelse(x == "a38", "minister",
  ifelse(x == "a39", "musician", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "a4, a16", "dental hygienist, ESL teacher",
  ifelse(x == "a40", "nurse",
  ifelse(x == "a41", "outfitter",
  ifelse(x == "a42", "painter",
  ifelse(x == "a44", "party (vice) president",
  ifelse(x == "a45", "PhD candidate",
  ifelse(x == "a46", "police officer",
  ifelse(x == "a47", "politician",
  ifelse(x == "a48", "postal clerk",
  ifelse(x == "a49", "prime minister",
  ifelse(x == "a5, a51", "department head, Professor (university, includes Associate and Assistant)",
  ifelse(x == "a50", "professional athlete (former or current)",
  ifelse(x == "a51", "Professor (university, includes Associate and Assistant)",
  ifelse(x == "a51, a13, 15", "Professor (university, includes Associate and Assistant), environmental activist, environmental specialist",
  ifelse(x == "a51, a7", "Professor (university, includes Associate and Assistant), director",
  ifelse(x == "a51,L", " Professor (university, includes Associate and Assistant), author",
  ifelse(x == "a52", "psychiatrist",
  ifelse(x == "a53, L", "psychologist, author",
  ifelse(x == "a54", "radio announcer", x <- x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "a54, a55", "radio announcer, radio announcer - sport",
  ifelse(x == "a55", "radio announcer - sport",
  ifelse(x == "a57", "radio news reporter",
  ifelse(x == "a58", "radio producer",
  ifelse(x == "a59", "researcher / research assistant",
  ifelse(x == "a6-a67", "deputy - speaker of the house",
  ifelse(x == "a61", "retired",
  ifelse(x == "a62", "secretary",
  ifelse(x == "a62 to a38", "secretary to minister",
  ifelse(x == "a63", "secretary of state",
  ifelse(x == "a64, u", "singer, community development agent",
  ifelse(x == "a65", "social worker",
  ifelse(x == "a66", "software analyst and/or programmer",
  ifelse(x == "a68", "sportscaster (live commentary)",
  ifelse(x == "a68, a50", "sportscaster (live commentary), professional athlete (former or current)",
  ifelse(x == "a69", "state premier",
  ifelse(x == "a7", "director",
  ifelse(x == "a70", "student (tertiary)",
  ifelse(x == "a70, a16", "student (tertiary), ESL teacher", x <- x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "a70, a59", "student (tertiary), researcher / research assistant",
  ifelse(x == "a70, a80", " student (tertiary), writer",
  ifelse(x == "a71", "talk show host",
  ifelse(x == "a72", "teacher (school)",
  ifelse(x == "a72 / a26", "teacher (school)/ instructor",
  ifelse(x == "a72, a12", "teacher (school), educator",
  ifelse(x == "a72, a32, a19", "teacher (school), lecturer, film-maker",
  ifelse(x == "a72, a5", "teacher (school), department head",
  ifelse(x == "a73", "technical support",
  ifelse(x == "a74", "tour guide",
  ifelse(x == "a74, a59", "tour guide, researcher / research assistant",
  ifelse(x == "a75", "(tribal First Nations) Chief",
  ifelse(x == "a76", "truck driver",
  ifelse(x == "a77", "union president",
  ifelse(x == "a78", "volunteer coordinator",
  ifelse(x == "a79, a12", "waiter/waitress, educator",
  ifelse(x == "a8", "docent (non-university)",
  ifelse(x == "a80", "writer",
  ifelse(x == "a80, l", "writer, author", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "a83", "school student",
  ifelse(x == "a9", "doctor",
  ifelse(x == "b", "(community/school) committee member",
  ifelse(x == "b-o", "(community/school) committee member, CEO/organization founder/company president",
  ifelse(x == "b, a25", "(community/school) committee member, housewifelsee/homemaker",
  ifelse(x == "b38", "former minister",
  ifelse(x == "b50", "professional coach",
  ifelse(x == "b75", "tribal elder",
  ifelse(x == "c", "(micro)biologist",
  ifelse(x == "Chair", "Chair (Professor (university, includes Associate and Assistant))",
  ifelse(x == "d", "actor/voice over artist/ advertisement announcer",
  ifelse(x == "e", "administrator",
  ifelse(x == "e-a7", "administrator, director",
  ifelse(x == "e, w", "administrator, community worker",
  ifelse(x == "f", "advisor",
  ifelse(x == "g", "animator",
  ifelse(x == "h", "archivist/librarian",
  ifelse(x == "I", "army officer",
  ifelse(x == "j", "artist", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 21] <- as.vector(unlist(sapply(biodata.tmp4[, 21], function(x) {
  ifelse(x == "L, a10", "author, ecologist",
  ifelse(x == "m", "business owner",
  ifelse(x == "n", "businessman/businesswoman",
  ifelse(x == "NA", NA,
  ifelse(x == "none given", "NA",
  ifelse(x == "o", "CEO/organization founder/company president",
  ifelse(x == "p", "child guardian",
  ifelse(x == "r", "clam digger",
  ifelse(x == "s", "clergyman",
  ifelse(x == "sa", "bishop",
  ifelse(x == "t", "community activist",
  ifelse(x == "v", "(community or professional) spokesperson/PR",
  ifelse(x == "x", "consultant",
  ifelse(x == "x, l", "consultant, author",
  ifelse(x == "y", "councillor",
  ifelse(x == "z", "counsellor",
  ifelse(x == "NA", NA, x
  )))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 21])
##################################################################
# OCCUPATION 2
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  x <- str_trim(x, side = "both")
  ifelse(x == "", NA,
  ifelse(x == "a0", "(at-home-)mother",
  ifelse(x == "a1, a2", "critic, curator",
  ifelse(x == "a1, a47", "critic, politician",
  ifelse(x == "a10", "ecologist",
  ifelse(x == "a11", "editor",
  ifelse(x == "a12, e", "educator, administrator",
  ifelse(x == "a13", "environmental activist",
  ifelse(x == "a14", "environmental analyst",
  ifelse(x == "a15", "environmental specialist",
  ifelse(x == "a16", "ESL teacher",
  ifelse(x == "a17", "farmer",
  ifelse(x == "a17, j", "farmer, artist",
  ifelse(x == "a18", "fashion designer",
  ifelse(x == "a19", "film-maker",
  ifelse(x == "a19, q, a21", "film-maker, choreographer, florist",
  ifelse(x == "a20", "financial analyst",
  ifelse(x == "a22, e", " freelance, administrator",
  ifelse(x == "a23", "futures trader", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "a24", "hobby teacher",
  ifelse(x == "a25, a70", "housewifelsee/homemaker, student (tertiary)",
  ifelse(x == "a26", "instructor",
  ifelse(x == "a28", "journalist",
  ifelse(x == "a28, L", "journalist, author",
  ifelse(x == "a29, a26", "lab technician, instructor",
  ifelse(x == "a3", "custodian",
  ifelse(x == "a30", "language teacher",
  ifelse(x == "a31", "lawyer",
  ifelse(x == "a31, a51", "lawyer, Professor (university, includes Associate and Assistant)",
  ifelse(x == "a32", "lecturer",
  ifelse(x == "a33", "manager",
  ifelse(x == "a34", "mayor",
  ifelse(x == "a34-a6", "mayor, deputy",
  ifelse(x == "a35", "military analyst",
  ifelse(x == "a36", "member of parliament",
  ifelse(x == "a37", "meteorologist",
  ifelse(x == "a38", "minister",
  ifelse(x == "a39", "musician", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "a4, a16", "dental hygienist, ESL teacher",
  ifelse(x == "a40", "nurse",
  ifelse(x == "a41", "outfitter",
  ifelse(x == "a42", "painter",
  ifelse(x == "a44", "party (vice) president",
  ifelse(x == "a45", "PhD candidate",
  ifelse(x == "a46", "police officer",
  ifelse(x == "a47", "politician",
  ifelse(x == "a48", "postal clerk",
  ifelse(x == "a49", "prime minister",
  ifelse(x == "a5, a51", "department head, Professor (university, includes Associate and Assistant)",
  ifelse(x == "a50", "professional athlete (former or current)",
  ifelse(x == "a51", "Professor (university, includes Associate and Assistant)",
  ifelse(x == "a51, a13, 15", "Professor (university, includes Associate and Assistant), environmental activist, environmental specialist",
  ifelse(x == "a51, a7", "Professor (university, includes Associate and Assistant), director",
  ifelse(x == "a51,L", " Professor (university, includes Associate and Assistant), author",
  ifelse(x == "a52", "psychiatrist",
  ifelse(x == "a53, L", "psychologist, author",
  ifelse(x == "a54", "radio announcer", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "a54, a55", "radio announcer, radio announcer - sport",
  ifelse(x == "a55", "radio announcer - sport",
  ifelse(x == "a57", "radio news reporter",
  ifelse(x == "a58", "radio producer",
  ifelse(x == "a59", "researcher / research assistant",
  ifelse(x == "a6-a67", "deputy - speaker of the house",
  ifelse(x == "a61", "retired",
  ifelse(x == "a62", "secretary",
  ifelse(x == "a62 to a38", "secretary to minister",
  ifelse(x == "a63", "secretary of state",
  ifelse(x == "a64, u", "singer, community development agent",
  ifelse(x == "a65", "social worker",
  ifelse(x == "a66", "software analyst and/or programmer",
  ifelse(x == "a68", "sportscaster (live commentary)",
  ifelse(x == "a68, a50", "sportscaster (live commentary), professional athlete (former or current)",
  ifelse(x == "a69", "state premier",
  ifelse(x == "a7", "director",
  ifelse(x == "a70", "student (tertiary)",
  ifelse(x == "a70, a16", "student (tertiary), ESL teacher", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "a70, a59", "student (tertiary), researcher / research assistant",
  ifelse(x == "a70, a80", " student (tertiary), writer",
  ifelse(x == "a71", "talk show host",
  ifelse(x == "a72", "teacher (school)",
  ifelse(x == "a72 / a26", "teacher (school)/ instructor",
  ifelse(x == "a72, a12", "teacher (school), educator",
  ifelse(x == "a72, a32, a19", "teacher (school), lecturer, film-maker",
  ifelse(x == "a72, a5", "teacher (school), department head",
  ifelse(x == "a73", "technical support",
  ifelse(x == "a74", "tour guide",
  ifelse(x == "a74, a59", "tour guide, researcher / research assistant",
  ifelse(x == "a75", "(tribal First Nations) Chief",
  ifelse(x == "a76", "truck driver",
  ifelse(x == "a77", "union president",
  ifelse(x == "a78", "volunteer coordinator",
  ifelse(x == "a79, a12", "waiter/waitress, educator",
  ifelse(x == "a8", "docent (non-university)",
  ifelse(x == "a80", "writer",
  ifelse(x == "a80, l", "writer, author", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "a83", "school student",
  ifelse(x == "a9", "doctor",
  ifelse(x == "b", "(community/school) committee member",
  ifelse(x == "b-o", "(community/school) committee member, CEO/organization founder/company president",
  ifelse(x == "b, a25", "(community/school) committee member, housewifelsee/homemaker",
  ifelse(x == "b38", "former minister",
  ifelse(x == "b50", "professional coach",
  ifelse(x == "b75", "tribal elder",
  ifelse(x == "c", "(micro)biologist",
  ifelse(x == "Chair", "Chair (Professor (university, includes Associate and Assistant))",
  ifelse(x == "d", "actor/voice over artist/ advertisement announcer",
  ifelse(x == "e", "administrator",
  ifelse(x == "e-a7", "administrator, director",
  ifelse(x == "e, w", "administrator, community worker",
  ifelse(x == "f", "advisor",
  ifelse(x == "g", "animator",
  ifelse(x == "h", "archivist/librarian",
  ifelse(x == "I", "army officer",
  ifelse(x == "j", "artist", x
  )))))))))))))))))))  }  )))
biodata.tmp4[, 31] <- as.vector(unlist(sapply(biodata.tmp4[, 31], function(x) {
  ifelse(x == "L, a10", "author, ecologist",
  ifelse(x == "m", "business owner",
  ifelse(x == "n", "businessman/businesswoman",
  ifelse(x == "NA", NA,
  ifelse(x == "none given", NA,
  ifelse(x == "o", "CEO/organization founder/company president",
  ifelse(x == "p", "child guardian",
  ifelse(x == "r", "clam digger",
  ifelse(x == "s", "clergyman",
  ifelse(x == "sa", "bishop",
  ifelse(x == "t", "community activist",
  ifelse(x == "v", "(community or professional) spokesperson/PR",
  ifelse(x == "x", "consultant",
  ifelse(x == "x, l", "consultant, author",
  ifelse(x == "y", "councillor",
  ifelse(x == "z", "counsellor",
  ifelse(x == "", NA, x
  )))))))))))))))))  }  )))
# Overview
#table(biodata.tmp4[, 31])
# Clean up
rm.na <- function(x){ifelse(x == "NA", NA, x)}
biodata.tmp4 <- apply(biodata.tmp4, 2, FUN = rm.na)
###############################################################
#head(biodata.tmp4)
# Add id to enable restoring original order
biodata.ice.tmp5 <- cbind(1: length(biodata.tmp4[, 1]), biodata.tmp4)
colnames(biodata.ice.tmp5) <- c("orig.id", colnames(biodata.tmp4))
# order data
biodata.ice.tmp6 <- biodata.ice.tmp5[order(biodata.ice.tmp5[, 3], biodata.ice.tmp5[, 7]), ]
# Add id to ease merging the data sets
biodata.ice.tmp7 <- cbind(1: length(biodata.ice.tmp6[, 1]), biodata.ice.tmp6)
# Add column names
colnames(biodata.ice.tmp7) <- c("id", colnames(biodata.ice.tmp6))
colnames(biodata.ice.tmp7)[4] <- "text.id"
colnames(biodata.ice.tmp7)[5] <- "subfile.id"
colnames(biodata.ice.tmp7)[6] <- "spk.ref"
# Delete superfluous columns
biodata.ice.tmp7 <- cbind(biodata.ice.tmp7[, c(1:2, 4:length(biodata.ice.tmp7[1, ]))])
# Clean up
rm.na <- function(x){ifelse(x == "NA", NA, x)}
biodata.ice.tmp7 <- apply(biodata.ice.tmp7, 2, FUN = rm.na)
biodata.ice.tmp7[, 24] <- as.vector(unlist(sapply(biodata.ice.tmp7[, 24], function(x) {
  ifelse(x == "a31", NA, x) } )))
# Inspect data
#biodata.ice.tmp7
#head(biodata.ice.tmp7)
################################################################
################################################################
# We will now merge the two data sets
# Inspect data sets which will be merged
#head(speakerinfo.ice.can.1)
#head(biodata.ice.tmp7)
# Rename variables to allow merging
colnames(speakerinfo.ice.can.1)[5] <- "spk.ref"
# Transform data sets into data frames
speakerinfo.ice.can.1 <- as.data.frame(speakerinfo.ice.can.1)
biodata.ice.tmp7 <- as.data.frame(biodata.ice.tmp7)
# Join data sets (without speakers that do not occur in the corpus
# but do occur in the biodata spreadsheet provided by the
# corpus compilers) RECOMMENDED
biodata.ice.can.tmp2 <- join(speakerinfo.ice.can.1, biodata.ice.tmp7, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# Join data sets (with speakers that do not occur in the corpus
# but do occur in the biodata spreadsheet provided by the
# corpus compilers) NOT RECOMMENDED
#biodata.ice.can.tmp2 <- join(speakerinfo.ice.can.1, biodata.ice.tmp7,
#by = c("text.id", "spk.ref"), type = "full")
# Delete superfluous columns (info is already contained in another column)
biodata.ice.can.tmp3 <- cbind(1:length(biodata.ice.can.tmp2[, 1]), biodata.ice.can.tmp2[, 3:7], biodata.ice.can.tmp2[, 9:length(biodata.ice.can.tmp2[1, ])])
tmp.colnames <- colnames(biodata.ice.can.tmp3)
# Add column names
colnames(biodata.ice.can.tmp3) <- c("id", tmp.colnames[2:length(tmp.colnames)])
# Inspect data
#head(biodata.ice.can.tmp3)
# Reorder the columns so that the turn.count and word.count is
# in the last column
biodata.ice.can.tmp4 <- cbind(biodata.ice.can.tmp3[, 1:4], biodata.ice.can.tmp3[, 7:length(biodata.ice.can.tmp3[1, ])], biodata.ice.can.tmp3[, 5:6])
# Fill missing values with info if provided in other columns
# First we determine which rows contain missing values
# title column
# Which values are NA in the title column?
index.tmp1 <- is.na(biodata.ice.can.tmp4[, 7])
index.tmp2 <- which(index.tmp1 == "TRUE")
# Which values are NA?
index.tmp3 <- index.tmp2+1
# Replace the NA values with the values from below
biodata.ice.can.tmp4[, 7][index.tmp2] <- biodata.ice.can.tmp4[, 7][index.tmp3]
# date column
# Which values are NA in the title column?
index.tmp1 <- is.na(biodata.ice.can.tmp4[, 8])
index.tmp2 <- which(index.tmp1 == "TRUE")
# Which values are NA?
index.tmp3 <- index.tmp2+1
# Replace the NA values with the values from below
biodata.ice.can.tmp4[, 8][index.tmp2] <- biodata.ice.can.tmp4[, 8][index.tmp3]
# Inspect data
#head(biodata.ice.can.tmp4)
# Add column which specifies the corpus
biodata.ice.can.tmp5 <- cbind(biodata.ice.can.tmp4[, 1], c(rep("ice.canada", length(biodata.ice.can.tmp4 [, 1]))), biodata.ice.can.tmp4[, c(2:length(biodata.ice.can.tmp4[1, ]))])
# Add column names
colnames(biodata.ice.can.tmp5) <- c(colnames(biodata.ice.can.tmp4)[1], "corpus", colnames(biodata.ice.can.tmp4)[2:length(colnames(biodata.ice.can.tmp4))])
colnames(biodata.ice.can.tmp5) [35] <- "speech.unit.count"
# Delete superfluous columns
biodata.ice.can.tmp6 <- biodata.ice.can.tmp5[, -c(31, 33)]
# Reorder data set
biodata.ice.can.tmp7 <- biodata.ice.can.tmp6[, c(1, 6, 2, 7, 3:5, 17, 29, 30, 16, 8:15, 18:21, 31, 22, 23, 32, 24:28, 33:34)]
colnames(biodata.ice.can.tmp7) [18] <- "pseudoyn.first"
colnames(biodata.ice.can.tmp7) [19] <- "pseudoyn.second"
colnames(biodata.ice.can.tmp7) [24] <- "ethnicity"
colnames(biodata.ice.can.tmp7) [25] <- "ethnicity.orig"
colnames(biodata.ice.can.tmp7) [28] <- "education.orig"
# Inspect data
#head(biodata.ice.can.tmp7)
# Rename data
biodata.ice.canada <- biodata.ice.can.tmp7
# Inspect data
#head(biodata.ice.canada)
###############################################################
###############################################################
###                   ICE GBR2
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.gb, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# Load and store corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  corpus.tmp <- scan(x, what = "char", sep = "\t", quiet = T)  }  )
###############################################################
# Extract the file ids
file.ids <- lapply(corpus.files, function(x) {
  x <- gsub(".*([a-z][0-9][a-z]-[0-9][0-9][0-9]).*", "\\1", x, perl = T) }  )
###############################################################
# Collapse the file content
corpus.tmp1 <- lapply(corpus.tmp, function(x) {
  x <- paste(x, collapse = " ")  }  )
# Add names to the list elements
names(corpus.tmp1) <- file.ids
###############################################################
# Merge the file.ids with the file content
corpus.tmp2 <- apply(cbind(file.ids, corpus.tmp1), 1, function(x) unname(x))
###############################################################
# Specify searchpattern
splitpattern1 = "<I>"
# Split corpus
corpus.tmp3 <- lapply(corpus.tmp2,function(x) {
  strsplit(as.character(x), splitpattern1)  }  )
###############################################################
# Extract the number of elements in each file
subfile.count.tmp1 <- lapply(corpus.tmp3, function(x) {
  sapply(x, function(y) length(y))   }  )
# Extract the second element from subfile.count.tmp1
subfile.count.tmp2 <- sapply(subfile.count.tmp1, "[[", 2)
# Subtract 1 from each element in subfile.count.tmp2
subfile.count <- sapply(subfile.count.tmp2, function(x) {
  x <- x-1  }  )
###############################################################
# Create a table holding the file.ids and the number of
# subfiles in each file
corpus.files.tb.1 <- as.table(cbind(unlist(file.ids), subfile.count))
###############################################################
# Create a vector holding the file ids for each subfile
file.ids.tmp1 <- rep(file.ids, as.numeric(subfile.count))
#Extract all file.ids of spoken files
file.ids.spoken.tmp1 <- file.ids.tmp1[c(1:446)]
###############################################################
# Create a vector holding the subfile content of the corpus
subfile.contents.tmp1 <- sapply(corpus.tmp3, "[[", 2)
# Create a list of subfile.contents1 with the first element
# in each element deleted
subfile.contents.tmp2 <- sapply(subfile.contents.tmp1, "[", c(-1))
# Create a vector of subfile.contents2
subfile.contents.tmp3 <- unlist(subfile.contents.tmp2)
#Extract all spoken files
subfile.contents.tmp4 <- subfile.contents.tmp3[c(1:446)]
###############################################################
# EXCURSION
# We will now modify the names: the file and the subfile will be separated
# Extract the file.ids
file.id.tmp1 <- sapply(names(subfile.contents.tmp4), function(x) {
  strsplit(gsub("([a-z][0-9][a-z]-[0-9][0-9][0-9])", "\\1~", x), "~" )  }  )
file.id.tmp2 <- lapply(file.id.tmp1, "[[", 1)
# Extract the subfiles
subfile.id.tmp1 <- gsub("[a-z][0-9][a-z]-[0-9][0-9][0-9]", "", names(subfile.contents.tmp4))
# Replace empty elements with value 1
subfile.id.tmp2 <- lapply(subfile.id.tmp1,  function(x) {
  str_replace(x, "", "1")
  ifelse(x >= 1, x, "1") }  )
###############################################################
# Clean corpus (subfile.contents.tmp4)
subfile.corpus.tmp1 <- enc2utf8(subfile.contents.tmp4)
subfile.corpus.tmp2 <- gsub(" {2,}", " ", subfile.corpus.tmp1)
subfile.corpus.tmp3 <- str_replace_all(subfile.corpus.tmp2, fixed("\n"), " ")
subfile.corpus.tmp4 <- str_trim(subfile.corpus.tmp3, side = "both")
###############################################################
# Create a table holding the file id and the file content
overview.subfile.corpus.tb2 <- (cbind(
  1:446,
  file.id.tmp2,
  subfile.id.tmp2,
  subfile.corpus.tmp4))
colnames(overview.subfile.corpus.tb2) <- c("id", "file", "subfile", "content")
# Rename the table
corpus.table2 <- overview.subfile.corpus.tb2
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  corpfile2 <- strsplit( gsub("(<#[A-Z]{0,1}[0-9]{1,3}:[0-9]{1,3}:[A-Z]{0,1}\\?{0,1}>)","~\\1", x), "~" )  }  )
# Create a list of all.files.unclean with the first element
# in each element deleted
all.files.unclean <- sapply(all.files.unclean, "[", c(-1))
# Create a list of all.files.unclean with all spoken files
all.files.unclean <- all.files.unclean[c(1:446)]
###############################################################
# Separate the speakers from the speech.unit
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2)  }  )
###############################################################
# Store speakers in extra vector
speakers.full <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
speakers <- lapply(speakers.full, function(x) {
  x <- gsub("(<#[A-Z]{0,1}[0-9]{1,3}:[0-9]{1,3}:)","", x)
  x <- gsub("(>)","", x)}  )
###############################################################
#Extract the number of speech.units for each speaker of all spoken files
speech.unit.count.tmp1 <- lapply(speakers, function(x) {
  as.data.frame(table(x))  }  )
###############################################################
# Extract the number of speakers per file
speaker.count.tmp1 <- sapply(speech.unit.count.tmp1, function(x) {
  sapply(x, function(y) {
    length(y)  }  )  }  )
speaker.count.tmp2 <- speaker.count.tmp1[c(seq(2, 892, 2))]
###############################################################
# Create a vector with file.ids with equals the length of
# speakers in the corpus
file.ids.tmp1 <- rep(file.id.tmp2, speaker.count.tmp2)
###############################################################
# Create a vector with subfile.ids with equals the length of
# speakers in the corpus
subfile.ids.tmp1 <- rep(subfile.id.tmp2, speaker.count.tmp2)
###############################################################
# Create a vector with the speech.unit.count of all speakers in the corpus
speech.unit.count <- unlist(sapply(speakers, function(x) {table(x)}))
###############################################################
# Create a vector with the ids all speakers in the corpus
speaker.ids.tmp1 <- names(speech.unit.count)
speaker.ids.tmp2 <- lapply(speaker.ids.tmp1, function(x) {
  sapply(x, function(y) {
    gsub("([a-z][0-9][a-z]-[0-9]{3,5}.)","", y)  }  )  }  )
###############################################################
# Crate a table, holding the file, speaker.id and the speech.unit
# count for each speaker
overview.corpus.table.tb1 <- as.table(cbind(c(1:length(file.ids.tmp1)),
  file.ids.tmp1,
  subfile.ids.tmp1,
  as.character(speaker.ids.tmp2),
  speech.unit.count))
colnames(overview.corpus.table.tb1) <- c("id",
  "file.id",
  "subfile.id",
  "speaker.id",
  "speech.unit.count")
###############################################################
###############################################################
# Retrieve all speech.units
# Store speech.units in extra vector
corpus.content <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 2)  }  )
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
corpus.content.clean <- lapply(corpus.content, function (x){
# ICE GB specific replacement
  x <- str_replace_all(x, "(<sent>)","")
  x <- gsub(" {2,}", " ", x)
  x <- gsub("(\\}.*?\\{)", "}{", x, perl = TRUE)
  x <- sub("(.*?\\{)", "{", x, perl = TRUE)
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*</X>)","")
  x <- str_replace_all(x, "(<X>)","")
  x <- str_replace_all(x, "(</X>)","")
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
###############################################################
# Continue cleaning the cropus content
corpus.content.nomarkup.tmp1 <- sapply(corpus.content.clean, function(x) {
  x <- str_replace_all(x, "(\\}\\{)"," ")
# Deleting ICE GB Mark-up
  x <- str_replace_all(x, "(<O>.*?</O>)"," ")
  x <- str_replace_all(x, "(<O>.*?<O>)"," ")
  x <- str_replace_all(x, "(</O>.*?</O>)"," ")
  x <- str_replace_all(x, "(<quote>.*?</quote>)", "")
  x <- str_replace_all(x, "(<EXTM\\(begin\\)>.*?<EXTM\\(end\\)>)", "")
  x <- str_replace_all(x, "(<unclear-{0,1}[a-z]{4,5}>.*?/unclear-{0,1}[a-z]{4,5}>)", "")
  x <- str_replace_all(x, "(<unclear.*?/unclear.*?>)", "")
  x <- str_replace_all(x, "(<.*?>)", "")
  x <- str_replace_all(x, "(\\(.*?\\))", "")
  x <- str_replace_all(x, "(\\])", "")
  x <- str_replace_all(x, "(\\[)", "")
  x <- str_replace_all(x, "(0,0)", "")
  x <- str_replace_all(x, "([A-Z]{1,8},[A-Z]{1,8})", "")
  x <- str_replace_all(x, "(\\([a-z]{3,10},[a-z]{3,10}\\))", "")
  x <- str_replace_all(x, "([0-9]{1,3}>)", "")
  x <- str_replace_all(x, "(\\$[A-Z])", "")
  x <- str_replace_all(x, "(unclear INDET \\? unclear-word)","")
  x <- str_replace_all(x, "(#[0-9]{1,3}\\:[0-9]{1,3}\\:)", "")
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
  x <- gsub("([A-Z]{3,8})", "", x)
  x <- gsub("(\\{[0-9].*)", " ", x)
  x <- sub("(\\{)", " ", x)
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(corpus.content.nomarkup.tmp1, function(x) {
  tokenized <- strsplit(x, " ")  }  )
# Now, we count the words elements(words) in each speech.unit
word.count.tmp1 <- lapply(tokenized, function(x) {
  sapply(x, function(y) length(y))   } )
# We now convert the word counts into numeric values
word.count <- lapply(word.count.tmp1, function(x) {
  sapply(x, as.numeric)  }  )
###############################################################
# Determine the number of speech.units per speech.unit
# (which in ICE GB is always 1)
speech.unit.count <- lapply(word.count, function(x) {
  gsub("([0-9]{1,})", "1", x)  }  )
# Create a list with the ids for each file
id.ls <- lapply(speakers, function(x) {
  x <- c(1:length(x))  }  )
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# speech.unit.count, word.count)
###############################################################
# Create a list of tables representing the data
corpus.info.tmp1 <- mapply(cbind,
  id.ls,
  speakers.full,
  file.id.tmp2,
  subfile.id.tmp2,
  speakers,
  corpus.content,
  corpus.content.clean,
  corpus.content.nomarkup.tmp1,
  speech.unit.count,
  word.count,
  SIMPLIFY = F)
names(corpus.info.tmp1) <- file.id.tmp2
# Add row. and column names
corpus.info.tmp2 <- lapply(corpus.info.tmp1, function(x) {
  z <- as.data.frame(x[])
  colnames(z) <- c("id", "full.speaker.id", "text.id", "subfile.id", "spk.ref", "orig.content", "cleaned.content", "final.content", "speech.unit.count", "word.count")
  x <-z  }  )
###############################################################
# Now, we convert the matrixes in the list into data frames
corpus.info.tmp12 <-lapply(corpus.info.tmp2, function(x) {
  z <- as.data.frame(x)
  z[, 9] <- as.numeric(z[, 9])
  z[, 10] <- as.numeric(z[, 10])
  x <- z  }  )
###############################################################
# Rename data for later kwic seraches
kwic.tb.ice.gb <- corpus.info.tmp12
###############################################################
# Extract the words counts for speakers in one file
word.count.result.tmp1 <- lapply(corpus.info.tmp12, function(x) {
  x <- as.data.frame(tapply(x[[10]], x[[5]], sum)) } )
# Simplify the results
word.count.result.tmp2 <- sapply(word.count.result.tmp1, "[[", 1)
names(word.count.result.tmp2) <- file.ids.spoken.tmp1
# Unlist
word.count.result.tmp3 <- unlist(word.count.result.tmp2)
###############################################################
# Extract the speech.units counts for speakers in one file
speech.unit.count.result.tmp1 <- lapply(corpus.info.tmp12, function(x) {
  x <- as.data.frame(tapply(x[[9]], x[[5]], sum)) } )
# Simplify the results
speech.unit.count.result.tmp2 <- sapply(speech.unit.count.result.tmp1, "[[", 1)
names(speech.unit.count.result.tmp2) <- file.ids.spoken.tmp1
# Unlist
speech.unit.count.result.tmp3 <- unlist(speech.unit.count.result.tmp2)
###############################################################
# We now put all the information together in one table
overview.corpus.table.tb2 <- as.table(cbind(1:length(file.ids.tmp1),
  file.ids.tmp1, subfile.ids.tmp1, as.vector(unlist((speaker.ids.tmp2))),
  speech.unit.count.result.tmp3, word.count.result.tmp3))
# Rename the data
speakerinfo.ice.gbr2.tmp1 <- overview.corpus.table.tb2
rownames(speakerinfo.ice.gbr2.tmp1) <- 1:length(speakerinfo.ice.gbr2.tmp1[, 1])
speakerinfo.ice.gbr2.tmp2 <- matrix(as.vector(unlist(speakerinfo.ice.gbr2.tmp1)), ncol = 6)
speakerinfo.ice.gbr2 <- as.data.frame(speakerinfo.ice.gbr2.tmp2)
speakerinfo.ice.gbr2[, 5] <- as.numeric(speakerinfo.ice.gbr2[, 5])
speakerinfo.ice.gbr2[, 6] <- as.numeric(speakerinfo.ice.gbr2[, 6])
colnames(speakerinfo.ice.gbr2) <- c("id", "text.id", "subfile.id", "spk.ref", "speech.unit.count", "word.count")
################################################################
### --- STEP
################################################################
################################################################
# Load biodata
biodata.tmp1 <- read.delim(bio.gb, header = T, sep = "\t")
biodata.tmp1 <- as.matrix(biodata.tmp1)
# Delete spaces
biodata.tmp2 <- as.matrix(t(apply(biodata.tmp1, 1, FUN = function(x) {
  str_replace_all(x, " ","")
  }  )), ncol = length(biodata.tmp1[1, ]))
# Replace empty cells with NA
biodata.tmp3 <- as.matrix(t(apply(biodata.tmp2, 1, FUN = function(x) {
  ifelse(x == "", NA, x)
    ifelse(is.na(x) == T, NA, x)
    }  )), ncol = length(biodata.tmp2[1, ]))
# Convert to lower case
biodata.tmp4 <- tolower(biodata.tmp3)
colnames(biodata.tmp4) <- tolower(colnames(biodata.tmp3))
# Add id to data
biodata.tmp5 <- cbind(rep("ice.gb.r2", length(biodata.tmp4[, 1])), biodata.tmp4)
biodata.tmp6 <- cbind(1:length(biodata.tmp5[, 1]), biodata.tmp5)
colnames(biodata.tmp6) <- c("orig.id", "corpus", colnames(biodata.tmp4))
rownames(biodata.tmp6) <- biodata.tmp6[, 1]
# Align colnames and data
colnames(biodata.tmp6)[c(4:6)] <- c("text.id", "subfile.id", "spk.ref")
biodata.tmp6[, 6] <- toupper(biodata.tmp6[, 6])
# Convert to data.frame
biodata.tmp7 <- as.data.frame(biodata.tmp6)
################################################################
### --- STEP
################################################################
# Join data sets (without speakers that do not occur in the corpus
# but do occur in the biodata spreadsheet provided by the
# corpus compilers) RECOMMENDED
biodata.ice.gbr2.tmp1 <- join(speakerinfo.ice.gbr2, biodata.tmp7, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# Reorganize the data frame
biodata.ice.gbr2.tmp2 <- cbind(biodata.ice.gbr2.tmp1[, 1:4], biodata.ice.gbr2.tmp1[, 7:length(biodata.ice.gbr2.tmp1[1, ])], biodata.ice.gbr2.tmp1[, 5:6])
# Replace na cells with NA
biodata.ice.gbr2.tmp3 <- as.matrix(t(apply(biodata.ice.gbr2.tmp2, 1, FUN = function(x) {
  ifelse(x == "", NA,
  ifelse(x == "na", NA,
  ifelse(x == "NA", NA, x)))
  }  )), ncol = length(biodata.ice.gbr2.tmp2[1, ]))
# Align column names with other biodata spreadsheets
colnames(biodata.ice.gbr2.tmp3) <- c("id", "file.id", "subfile.id", "spk.ref", "orig.id", "corpus", "spk.id", "orig.spkr", "aware", "role", "last.name", "1st.name", "age", "sex", "nationality", "birthplace", "education", "ed.lev", "occupation", "affiliations", "other.lgs", "comments", "speech.unit.count", "word.count")
# Add row names
rownames(biodata.ice.gbr2.tmp3) <- c(1: length(biodata.ice.gbr2.tmp3[, 1]))
# Fill missing data
biodata.ice.gbr2.tmp3[, 6] <- as.vector(unlist(sapply(biodata.ice.gbr2.tmp3[, 6], function(x){
  ifelse(is.na(x) == T, "ice.gb.r2", x)  } )))
# Reorganize the data set
biodata.ice.gbr2.tmp4 <- cbind(biodata.ice.gbr2.tmp3[, c(1, 5, 6, 2:4, 7, 13:14, 8:12, 15:24)])
# Rename the data set
biodata.ice.gbr2 <- biodata.ice.gbr2.tmp4
###############################################################
###############################################################
###                   ICE India
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.ind, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and store corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  corpus.tmp <- scan(x, what = "char", sep = "\t", quiet = T)  }  )
corpus.tmp <- unlist(corpus.tmp)
# View results
#corpus.tmp
# Paste all elements of the corpus together
corpus.tmp1 <- paste(corpus.tmp, collapse = " ")
# Inspect the resulting file
#corpus.tmp1
# Clean corpus
corpus.tmp2 <- enc2utf8(corpus.tmp1)
corpus.tmp2 <- gsub(" {2,}", " ", corpus.tmp2)
corpus.tmp2 <- str_replace_all(corpus.tmp2, fixed("\n"), " ")
corpus.tmp2 <- str_trim(corpus.tmp2, side = "both")
# Inspect the resulting file
#corpus.tmp2
###############################################################
# Specify searchpattern
splitpattern2 = "<I> "
# Split the corpus
corpus.tmp4 <- sapply(corpus.tmp2, function(x) {
    strsplit(as.character(x), splitpattern2)  }  )
# Inspect the resulting object
#str(corpus.tmp4)
# Delete first element (broken)
corpus.tmp5 <- corpus.tmp4[[1]][2:length(corpus.tmp4[[1]])]
# Delete written files
corpus.tmp5 <- corpus.tmp5[1:335]
###############################################################
# Extract file ids
file.ids.tmp1 <- lapply(corpus.tmp5, function(x) {
    x <- str_replace_all(x, "(#[A-Z]{0,1}[0-9]{0,4}:[0-9]{0,4}:{0,1}[A-Z]{0,1}.*)", "")  }  )
file.ids.tmp2 <- sapply(file.ids.tmp1, function(x) {
  x <- str_replace_all(x, "(.*:)", "")  }  )
# View results
#file.ids.tmp2
# Extract subfile ids
subfile.ids.tmp1 <- sapply(as.vector(table(file.ids.tmp2)), function(x) {
  x <- str_replace_all(x, "2","1 2")
  x <- str_replace_all(x, "3","1 2 3")
  x <- str_replace_all(x, "4","1 2 3 4")
  x <- paste(x, collapse = " ")
  x <- strsplit(as.character(x), " ")
  x <- unlist(x)
  x <- sapply(x, "[", 1)  }  )
subfile.ids.tmp2 <- sapply(subfile.ids.tmp1, function(x) {
  x <- paste(x, collapse = " ")
  x <- strsplit(as.character(x), " ")  }  )
subfile.ids.tmp3 <- sapply(subfile.ids.tmp2, function(x) {
    x <- paste(x, collapse = " ")  }  )
subfile.ids.tmp4 <- paste(subfile.ids.tmp3, collapse = " ")
subfile.ids.tmp4 <- gsub(" {2,}", " ", subfile.ids.tmp4)
subfile.ids.tmp5 <- strsplit(as.character(subfile.ids.tmp4), " ")
subfile.ids.tmp5 <- strsplit(subfile.ids.tmp4, " ")
subfile.ids.tmp5 <- unlist(subfile.ids.tmp5)
# View results
#subfile.ids.tmp5
# Transform corpus.tmp6 into a data frame
corpus.tmp7 <- as.data.frame(corpus.tmp5)
# Create a table from the results
corpus.tb1 <- cbind(1:length(corpus.tmp7[, 1]), file.ids.tmp2, subfile.ids.tmp5, corpus.tmp7[, 1])
# Add column names
colnames(corpus.tb1) <- c("id", "file", "subfile", "corpusfile")
# Rename object
corpus.table2 <- corpus.tb1
# View results
#head(corpus.table2)
###############################################################
### --- STEP
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# View results
#str(all.files)
# Split corpus files so that each speech.unit is one element
all.files.unclean.tmp1 <- str_split(gsub("ICE-IND:", "\\1~<", all.files), "~")
all.files.unclean.tmp2 <- sapply(all.files.unclean.tmp1, function(x) {
  x <- x[2:length(x)]  }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)], 1 , paste , collapse = " " )
names(all.files.unclean.tmp2) <- file.subfile.ids
# View results
#str(all.files.unclean.tmp2)
###############################################################
# Separate the speakers from the speech.unit
speakers.and.utts <- lapply(all.files.unclean.tmp2, function(x) {
  str_split(x, " ", n = 2)  }  )
# View results
#speakers.and.utts
# Store speakers in extra vector
speakers.tmp1 <- lapply(speakers.and.utts, function(x) {
  sapply(x, "[[", 1)  }  )
# PROBLEM: speakers.tmp1[[8]][168] is broken!
# Repair speakers.tmp1[[8]][168]
speakers.tmp1[[8]][168] <- "<S1A-008#168:1:A>"
speakers <- lapply(speakers.tmp1, function(x) {
  x <- str_replace_all(x, "(<.*:)","")
  x <- str_replace_all(x, "(>)","")  }  )
# View results
#speakers
# Store speech.units in extra vector
speech.units <- lapply(speakers.and.utts, function(x) {
  sapply(x, function(x) x[2])  }  )
# View results
#speech.units
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
speech.units.clean <- lapply(speech.units, function (x){
  x <- str_replace_all(x, "(<q.*?/q>)","")
  x <- str_replace_all(x, "(<->.*?</->)","")
  x <- str_replace_all(x, "(<&.*?/&.*?>)","")
  x <- str_replace_all(x, "(<O.*?/O>)","")
  x <- str_replace_all(x, "(<\\?.*?/\\?>)","")
  x <- str_replace_all(x, "(<un.*?clear>)","")
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<X>)","")
  x <- str_replace_all(x, "(</X>)","")
#  x <- str_replace_all(x, "(<indig.*?/indig>)","")
  x <- str_replace_all(x, "(<.*?>)", "")
# WARNING: THEORETICAL ISSUE
  x <- gsub(" {2,}", " ", x)
  x <- gsub(" re |'re ", "'re ", x)
  x <- gsub(" ll |'ll ", "'ll ", x)
  x <- gsub(" {0,1}Ill ", " I'll ", x)
  x <- gsub(" ve |'ve ", "'ve ", x)
  x <- gsub(" s ", "'s ", x)
  x <- gsub(" d ", "'d ", x)
  x <- gsub(" {0,1}I m ", " I'm ", x)
  x <- gsub("Im ", " I'm ", x)
  x <- gsub("Its", " It's ", x)
  x <- gsub(" its", " it's ", x)
  x <- gsub("Hes", " He's ", x)
  x <- gsub(" hes", " he's ", x)
  x <- gsub("Ive", " I've ", x)
  x <- gsub(" {0,1}Thats ", " That's ", x)
  x <- gsub(" thats ", " that's ", x)
  x <- gsub(" {0,1}Theres ", " There's ", x)
  x <- gsub(" theres ", " there's ", x)
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
  x <- gsub(" havent ", " haven't ", x)
  x <- gsub("Havent ", "Haven't ", x)
  x <- gsub("Wasnt ", "Wasn't ", x)
  x <- gsub(" wasnt ", " wasn't ", x)
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
# View results
#str(speech.units.clean)
#speech.units.clean[[1]]
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(speech.units.clean, function(x){
  tokenized <- strsplit(x, " ")  }  )
# View results
#tokenized
# Now, we count the words elements(words) in each speech.unit (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y) length(y))   } )
# View results
#word.count
###############################################################
# Create a list which holds the number of speech.units per speech.unit
#Extract the number of speech.units for each speaker of all spoken files
utt.count.tmp1 <- lapply(word.count, function(x) {
  sapply(x, function(y) gsub(".*",  "1", y))   }  )
#Add names (file.ids) to the number of speech.units
#names(utt.count.tmp1) <- file.id.tmp2
# Rename list
utt.count <- utt.count.tmp1
# View results
#utt.count
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# utt.count, word.count)
###############################################################
speaker.and.unclean.utts <- mapply(cbind, speakers[], speech.units[], SIMPLIFY = F)
# View results
#speaker.and.unclean.utts
speaker.both.utts <- mapply(cbind, speaker.and.unclean.utts[], speech.units.clean[], SIMPLIFY = F)
names(speaker.both.utts) <- file.subfile.ids
# View results
#str(speaker.both.utts)
#speaker.both.utts
# Add file.subfile.ids to speaker.both.utts
speaker.both.utts.subfile <- mapply(cbind, speaker.both.utts[],names(speaker.both.utts[]), SIMPLIFY = F)
# View results
#speaker.both.utts.subfile
# Add utt.counts
speaker.both.utts.subfile.and.utt.count <- mapply(cbind, speaker.both.utts.subfile [], utt.count[], SIMPLIFY = F)
# View results
#speaker.both.utts.subfile.and.utt.count
speakerinfo1 <- mapply(cbind, speaker.both.utts.subfile.and.utt.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# View results
#speakerinfo1
# We now need to convert the elements of the fourth and fifth
# column into numeric elements
speakerinfo2 <-lapply(X = speakerinfo1, function (X) {
  x <- as.data.frame(X[])
  x[, 5] <- as.numeric(x[, 5])
  x[, 6] <- as.numeric(x[, 6])
  X <- x  }  )
# View results
#speakerinfo2
###############################################################
# Rename data for later kwic searches
kwic.tb.ice.ind <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum)))   } )
# View results
#word.count.result
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# View results
#overview.word.count.results
# Extract the speech.unit counts for speakers in one file
speech.unit.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum)))   } )
# View results
#speech.unit.count.result
# Simplify the results
overview.speech.unit.count.results <- sapply(speech.unit.count.result, "[[", 1)
# View results
#overview.speech.unit.count.results
###############################################################
# Extract the speakers in one file
speaker.id.list.tmp1 <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) {
    x <- x[[1]]   } )  }  )
# View results
#speaker.id.list.tmp1
# Extract all elements from the frist column (the speaker.refs)
speaker.id.list.tmp2 <- sapply(speaker.id.list.tmp1, function(x) {
  x <- x[, 1]  }  )
# View results
#speaker.id.list.tmp2[[1]]
speaker.ids.tmp1 <- sapply(speaker.id.list.tmp2, function(x) {
  table(x)  }  )
# View results
#speaker.ids.tmp1
speaker.ids.tmp2 <- sapply(speaker.ids.tmp1, function(x) {
  x <- names(x)  }  )
# View results
#speaker.ids.tmp2
# Extract the speakers and store them in a vector
speaker.ids.tmp3 <- as.vector(unlist(speaker.ids.tmp2))
# Rename vector
speaker.ids <- speaker.ids.tmp3
# View results
#speaker.ids
# Extract the number of speakers of each file
no.speakers.in.file.tmp1 <- sapply(speaker.ids.tmp2, function(x) {
  x <- length(x)  }  )
# View results
#no.speakers.in.file.tmp1
# Repeat each file name as many times as theer are speakers in that file
full.file.ids.tmp3 <- as.vector(rep(names(no.speakers.in.file.tmp1), as.vector(as.numeric(no.speakers.in.file.tmp1))))
# View results
#full.file.ids.tmp3
# Repeat each file name as many times as theer are speakers in that file
full.file.ids.tmp4 <- str_replace_all(full.file.ids.tmp3, " ", "#")
full.file.ids.tmp5 <- paste("<", full.file.ids.tmp4, ">")
full.file.ids.tmp6 <- str_replace_all(full.file.ids.tmp5, " ", "")
# View results
#full.file.ids.tmp6
# View results
#full.file.ids.tmp4
# Extract text.ids
text.ids <- str_replace_all(full.file.ids.tmp3, " .*", "")
# Extract subfile.ids
subfile.ids <- str_replace_all(full.file.ids.tmp3, ".* ", "")
###############################################################
# We now want to extract the speech.unit counts for each speaker
# in vector format so that we can easily create a table out of
# the results
speech.unit.count.list <- lapply(overview.speech.unit.count.results, function(x) { sapply(x, function(y) { sapply(y, "[[", 1)} )})
# View results
#speech.unit.count.list
# Convert the list into a vector
speech.unit.counts <- as.vector(unlist(speech.unit.count.list))
# View results
#speech.unit.counts
###
# We now want to extract the word counts for each speaker
# in vector format so that we can easily create a table out of
# the results
word.count.list <- lapply(overview.word.count.results, function(x) {
  sapply(x, function(y) { sapply(y, "[[", 1)} )  }  )
# View results
#word.count.list
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
# View results
#word.counts
###############################################################
# We now want to create a table with speaker id, speech.unit count
# and word count
# First, we create an index
id <- c(1:length(full.file.ids.tmp3))
# Now, we set up the data frame and combine file.ids and seakers in a table
speakerinfo.ice.india.tb.tmp1 <- as.data.frame(cbind(id, full.file.ids.tmp6, text.ids, subfile.ids, speaker.ids, speech.unit.counts, word.counts))
colnames(speakerinfo.ice.india.tb.tmp1) <- c("id", "file.speaker.id", "file", "subfile", "speakers", "speech.unit.count", "word.count")
# View results
#speakerinfo.ice.india.tb.tmp1
###############################################################
### --- STEP
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
header.files = list.files(path = bio.ind, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and store corpus (optimaler)
headers.tmp <- lapply(header.files, function(x) {
  header.tmp <- scan(x, what = "char", sep = "\t", quiet = T)  }  )
# Deelte broken first element
headers.tmp1 <- headers.tmp[2:length(headers.tmp)]
# Select all but only spoken files
headers.tmp2 <- headers.tmp1[1:300]
headers.tmp3 <- lapply(headers.tmp2, function(x)  {
  paste(x, collapse = "")  }  )
headers.tmp4 <- lapply(headers.tmp3, function(x)  {
  x <- gsub(" {2,}", " ", x)   }  )
headers.tmp4 <- as.character(headers.tmp4)
# View results
#headers.tmp4
# Clean corpus
headers.tmp5 <- enc2utf8(headers.tmp4)
headers.tmp5 <- gsub(" {2,}", " ", headers.tmp5)
headers.tmp5 <- str_replace_all(headers.tmp5, fixed("\n"), " ")
headers.tmp5 <- str_trim(headers.tmp5, side = "both")
# Inspect the resulting file
#headers.tmp5
###############################################################
# Split the headers
headers.tmp6 <- sapply(headers.tmp5, function(x) {
  str_split(gsub("<", "~\\1<", x, perl = T), "~")  }  )
headers.tmp6 <- sapply(headers.tmp6, function(x) {
  str_trim(x, side = "both")  }  )
# Inspect the resulting object
#headers.tmp6[1]
headers.speakers.tmp1 <- sapply(headers.tmp5, function(x)  {
  str_split(gsub("<speaker.id", "~\\1<speaker.id", x, perl = T), "~")  }  )
headers.speakers.tmp2 <- sapply(headers.speakers.tmp1, function(x)  {
  x <- x[2:length(x)]    }  )
headers.speakers.tmp3 <- sapply(headers.speakers.tmp1, function(x)  {
  x <- x[1]    }  )
# Extract text.ids to name elements in headers.speakers.tmp2
headers.speakers.tmp4 <- as.vector(sapply(headers.speakers.tmp3, function(x)  {
  x <- gsub(".*<biography=<it>ICE-IND-", "", x, perl = T)
  x <- gsub("</it>>.*", "", x, perl = T)
  x <- gsub("(1)", "", x, fixed = TRUE, perl = T)  }  ))
# Add names to elements in headers.speakers.tmp2
names(headers.speakers.tmp2) <- headers.speakers.tmp4
###############################################################
headers.speakers.tmp5 <- sapply(headers.tmp5, function(x)  {
  str_split(gsub("<subtext.source", "~\\1<subtext.source", x, perl = T), "~")  }  )
###
headers.speakers.tmp6 <- sapply(headers.speakers.tmp5, function(x)  {
  x <- x[2:length(x)]    }  )
headers.speakers.tmp7 <- unlist(headers.speakers.tmp6)
###
names(headers.speakers.tmp7) <- sapply(headers.speakers.tmp7, function(x)  {
  x <- gsub(">.*", "", x, perl = T)
  x <- gsub("<subtext.source=", "", x, perl = T)  }  )
###
headers.speakers.tmp8 <- sapply(headers.speakers.tmp7, function(x)  {
  str_split(gsub("<speaker.id", "~\\1<speaker.id", x, perl = T), "~")  }  )
###
headers.speakers.tmp9 <- sapply(headers.speakers.tmp8, function(x)  {
  x <- x[2:length(x)]    }  )
# Repair broken headers (no speaker.is in subfile)
headers.speakers.tmp9$`S2B-009(1)` <- headers.speakers.tmp9$`S2B-009(2)`
headers.speakers.tmp9$`S2B-010(1)` <- headers.speakers.tmp9$`S2B-010(2)`
headers.speakers.tmp9$`S2B-011(1)` <- headers.speakers.tmp9$`S2B-011(2)`
headers.speakers.tmp9$`S2B-012(1)` <- headers.speakers.tmp9$`S2B-012(2)`
headers.speakers.tmp9$`S2B-013(1)` <- headers.speakers.tmp9$`S2B-013(2)`
headers.speakers.tmp9$`S2B-014(1)` <- headers.speakers.tmp9$`S2B-014(2)`
headers.speakers.tmp9$`S2B-015(1)` <- headers.speakers.tmp9$`S2B-015(2)`
headers.speakers.tmp9$`S2B-016(1)` <- headers.speakers.tmp9$`S2B-016(2)`
headers.speakers.tmp9$`S2B-017(1)` <- headers.speakers.tmp9$`S2B-017(2)`
headers.speakers.tmp9$`S2B-018(1)` <- headers.speakers.tmp9$`S2B-018(2)`
###############################################################
headers.tmp7 <- sapply(headers.tmp6, function(x) {
  sapply(x, "[[", 1)  }  )
# Add names to list elements
names(headers.tmp7) <- names(table(file.ids.tmp2))
# Repeat the headers a often as theer are subfiles in the file
headers.tmp8 <- rep(headers.tmp7, table(file.ids.tmp2))
# Collapse th headers into a single string
headers.tmp9 <- lapply(headers.tmp8, function(x)  {
  paste(x, collapse = "")  }  )
###############################################################
# Extract the information from the headers
textcode <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<textcode>", "", x, perl = TRUE)
  x <- gsub("</textcode>.*", "", x, perl = TRUE)
  x <- gsub("ICE-IND-", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
date.of.recording <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<date>", "", x, perl = TRUE)
  x <- gsub("</date>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
place.of.recording <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<place>", "", x, perl = TRUE)
  x <- gsub("</place>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
text.category <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<category>", "", x, perl = TRUE)
  x <- gsub("</category>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
file.wordcount <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<wordcount>", "", x, perl = TRUE)
  x <- gsub("</wordcount>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
no.of.participants <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<no.of.participants>", "", x, perl = TRUE)
  x <- gsub("</no.of.participants>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
relationship.of.participants <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<relationship>", "", x, perl = TRUE)
  x <- gsub("</relationship>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
audience.size <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<audience.size>", "", x, perl = TRUE)
  x <- gsub("</audience.size>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
communicative.situation <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<communicative.situation>", "", x, perl = TRUE)
  x <- gsub("</communicative.situation>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
organising.body <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<organising.body>", "", x, perl = TRUE)
  x <- gsub("</organising.body>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
copyright.statement <- rep(as.vector(sapply(headers.tmp9, function(x)  {
  x <- gsub(".*<copyright.statement>", "", x, perl = TRUE)
  x <- gsub("</copyright.statement>.*", "", x, perl = TRUE)  }  )), no.speakers.in.file.tmp1)
speakerinfo.ice.india.tb.tmp2 <- as.data.frame(cbind(textcode, date.of.recording, place.of.recording, text.category, file.wordcount, no.of.participants, relationship.of.participants, audience.size, communicative.situation, organising.body, copyright.statement))
# Delete dublicated rows
speakerinfo.ice.india.tb.tmp2 <- speakerinfo.ice.india.tb.tmp2[!duplicated(speakerinfo.ice.india.tb.tmp2), ]
# Inspect resulting table
#head(speakerinfo.ice.india.tb.tmp2)
###############################################################
file.ids <- names(headers.speakers.tmp9)
no.speakers.in.file.ice.india <- sapply(headers.speakers.tmp9, function(x){
  length(x)  }  )
file.ids.tmp1 <- rep(file.ids, no.speakers.in.file.ice.india)
file.ids.tmp2 <- as.vector(sapply(file.ids.tmp1, function(x)  {
  x <- gsub("\\(.*", "", x, perl = TRUE)  }  ))
subfile.ids <- as.vector(sapply(file.ids.tmp1, function(x)  {
  x <- gsub(".*\\(", "", x, perl = TRUE)
  x <- gsub(")", "", x, perl = TRUE, fixed = TRUE)
  x <- gsub("S.*", "1", x, perl = TRUE)  }  ))
speaker.id <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<speaker.id>", "", x, perl = TRUE)
  x <- gsub("</speaker.id>.*", "", x, perl = TRUE)  }  )))
communicative.role <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<communicative.role>", "", x, perl = TRUE)
  x <- gsub("</communicative.role>.*", "", x, perl = TRUE)  }  )))
surname <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<surname>", "", x, perl = TRUE)
  x <- gsub("</surname>.*", "", x, perl = TRUE)  }  )))
forenames <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<forenames>", "", x, perl = TRUE)
  x <- gsub("</forenames>.*", "", x, perl = TRUE)  }  )))
age <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<age>", "", x, perl = TRUE)
  x <- gsub("</age>.*", "", x, perl = TRUE)  }  )))
gender <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<gender>", "", x, perl = TRUE)
  x <- gsub("</gender>.*", "", x, perl = TRUE)  }  )))
nationality <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<nationality>", "", x, perl = TRUE)
  x <- gsub("</nationality>.*", "", x, perl = TRUE)  }  )))
birthplace <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<birthplace>", "", x, perl = TRUE)
  x <- gsub("</birthplace>.*", "", x, perl = TRUE)  }  )))
education <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<education>", "", x, perl = TRUE)
  x <- gsub("</education>.*", "", x, perl = TRUE)  }  )))
occupation <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<occupation>", "", x, perl = TRUE)
  x <- gsub("</occupation>.*", "", x, perl = TRUE)  }  )))
affiliations <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<affiliations>", "", x, perl = TRUE)
  x <- gsub("</affiliations>.*", "", x, perl = TRUE)  }  )))
mother.tongue <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<mother.tongue>", "", x, perl = TRUE)
  x <- gsub("</mother.tongue>.*", "", x, perl = TRUE)  }  )))
other.languages <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<other.languages>", "", x, perl = TRUE)
  x <- gsub("</other.languages>.*", "", x, perl = TRUE)  }  )))
free.comments <- as.vector(unlist(sapply(headers.speakers.tmp9, function(x)  {
  x <- gsub(".*<free.comments>", "", x, perl = TRUE)
  x <- gsub("</free.comments>.*", "", x, perl = TRUE)  }  )))
speakerinfo.ice.india.tb.tmp3 <- as.data.frame(cbind(file.ids.tmp2, subfile.ids, speaker.id, communicative.role, surname, forenames, age, gender, nationality, birthplace, education, occupation, affiliations, mother.tongue, other.languages, free.comments))
rownames(speakerinfo.ice.india.tb.tmp3) <- 1:length(speakerinfo.ice.india.tb.tmp3[, 1])
# Inspect resulting table
#head(speakerinfo.ice.india.tb.tmp3)
###############################################################
### --- STEP
###############################################################
# Parallelize column names by which to join the data sets
colnames(speakerinfo.ice.india.tb.tmp1)[3] <- "text.id"
colnames(speakerinfo.ice.india.tb.tmp1)[4] <- "subfile.id"
colnames(speakerinfo.ice.india.tb.tmp1)[5] <- "spk.ref"
colnames(speakerinfo.ice.india.tb.tmp2)[1] <- "text.id"
# Join the data frames
biodata.ice.ind.tmp1 <- join(speakerinfo.ice.india.tb.tmp1, speakerinfo.ice.india.tb.tmp2, by = c("text.id"), type = "left")
# Inspect resulting table
#head(biodata.ice.ind.tmp1)
#length(biodata.ice.ind.tmp1[, 1])
# Parallelize column names by which to join the data sets
colnames(biodata.ice.ind.tmp1)[5] <- "spk.ref"
colnames(speakerinfo.ice.india.tb.tmp3) [1] <- "text.id"
colnames(speakerinfo.ice.india.tb.tmp3) [2] <- "subfile.id"
colnames(speakerinfo.ice.india.tb.tmp3) [3] <- "spk.ref"
# Join the data frames
biodata.ice.ind <- join(biodata.ice.ind.tmp1, speakerinfo.ice.india.tb.tmp3, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# Fill broken cells with correct values
biodata.ice.ind <- as.matrix(t(apply(biodata.ice.ind, 1, FUN = function(x) {
  x <- gsub("<text.info>.*", "", x, perl = TRUE)  }  )), ncol = length(biodata.ice.ind[1, ]))
# Fill broken cells with correct values
biodata.ice.ind <- as.matrix(t(apply(biodata.ice.ind, 1, FUN = function(x) {
  x <- gsub("Dialogue</communicative.stuation>.*", "Dialogue", x, perl = TRUE)  }  )), ncol = length(biodata.ice.ind[1, ]))
# Fill broken cells with correct values
biodata.ice.ind <- as.matrix(t(apply(biodata.ice.ind, 1, FUN = function(x) {
  x <- gsub("U\\.G\\.C\\. Refresher Course</organisinmg.body>.*", "U.G.C. Refresher Course", x, perl = TRUE)  }  )), ncol = length(biodata.ice.ind[1, ]))
# Fill empty cells
biodata.ice.ind <- as.matrix(t(apply(biodata.ice.ind, 1, function(x) {
  ifelse(x == "", NA,
  ifelse(x == "NA", NA,
  ifelse(is.na(x), NA, x)))  }  )), ncol = length(biodata.ice.ind [1, ]))
# Relable ids
biodata.ice.ind[, 1] <- 1: length(biodata.ice.ind[, 1])
# Reorder table
biodata.ice.ind <- cbind(biodata.ice.ind[, c(1:5, 8:length(biodata.ice.ind[1, ]), 6:7)])
# Reorder table
biodata.ice.ind[, c(6:length(biodata.ice.ind[1, ]))] <- tolower(biodata.ice.ind[, c(6:length(biodata.ice.ind[1, ]))])
# Inspect resulting table
#head(biodata.ice.ind)
# Recatergorize age
biodata.ice.ind[, 19] <- as.vector(unlist(sapply(biodata.ice.ind[, 19], function(x) {
  ifelse(x == "18", "18-25",
  ifelse(x == "19", "18-25",
  ifelse(x == "21", "18-25",
  ifelse(x == "22", "18-25",
  ifelse(x == "23", "18-25",
  ifelse(x == "18-26", "18-25",
  ifelse(x == "24-33", NA,
  ifelse(x == "24-41", NA,
  ifelse(x == "25", "18-25",
  ifelse(x == "25-33", "26-33",
  ifelse(x == "26-33", "26-33",
  ifelse(x == "30", "26-33",
  ifelse(x == "33", "26-33",
  ifelse(x == "33-41", NA,
  ifelse(x == "35", "34-41",
  ifelse(x == "34-51", NA,
  ifelse(x == "35-41", "34-41",
  ifelse(x == "36", "34-41",
  ifelse(x == "39", "34-41",
  ifelse(x == "40", "34-41",
  ifelse(x == "41", "34-41",
  ifelse(x == "40+", NA,
  ifelse(x == "45+", NA,
  ifelse(x == "41-49", NA,
  ifelse(x == "42", "42-49",
  ifelse(x == "42-49", "42-49", x <- x)))))))))))))))))))))))))) } )))
biodata.ice.ind[, 19] <- as.vector(unlist(sapply(biodata.ice.ind[, 19], function(x) {
  ifelse(x == "45", "42-49",
  ifelse(x == "47", "42-49",
  ifelse(x == "49", "42-49",
  ifelse(x == "50", "50+",
  ifelse(x == "52", "50+",
  ifelse(x == "52+", "50+",
  ifelse(x == "53", "50+",
  ifelse(x == "54", "50+",
  ifelse(x == "55", "50+",
  ifelse(x == "55+", "50+",
  ifelse(x == "56", "50+",
  ifelse(x == "57", "50+",
  ifelse(x == "58", "50+",
  ifelse(x == "60+", "50+",
  ifelse(x == "62", "50+",
  ifelse(x == "64", "50+",
  ifelse(x == "65", "50+",
  ifelse(x == "67", "50+",
  ifelse(x == "69", "50+",
  ifelse(x == "71", "50+",
  ifelse(x == "78", "50+",
  ifelse(x == "88", "50+",  x <- x)))))))))))))))))))))) } )))
# Recatergorize mother tongue
biodata.ice.ind[, 26] <- as.vector(unlist(sapply(biodata.ice.ind[, 26], function(x) {
  ifelse(x == "angami", NA,
  ifelse(x == "assamese", NA,
  ifelse(x == "bangla", NA,
  ifelse(x == "english", NA,
  ifelse(x == "bhojpuri", NA,
  ifelse(x == "gujrati", NA,
  ifelse(x == "hindi(bihari)", "hindi",
  ifelse(x == "kashmiri", NA,
  ifelse(x == "khasi", NA,
  ifelse(x == "manipuri", NA,
  ifelse(x == "marathi, kannada", "marathi",
  ifelse(x == "marwari", NA,
  ifelse(x == "naga", NA,
  ifelse(x == "nepali", NA,
  ifelse(x == "oriya", NA,
  ifelse(x == "sindhi", NA,
  ifelse(x == "tulu", NA,
 x <- x))))))))))))))))) } )))
# Recatergorize date of recordng
biodata.ice.ind[, 6] <- as.vector(unlist(sapply(biodata.ice.ind[, 6], function(x) {
  x <- gsub(".*-", "", x)
  x <- gsub("june 1991", "91", x)
  x <- gsub("9", "199", x)
  x <- gsub("199199", "1999", x) } )))
###############################################################
###                   ICE Ireland 1.2.2
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
  scan(x, what = "char", sep = "\t", quiet = T)  }  )
corpus.tmp <- unlist(corpus.tmp)
# View results
#corpus.tmp
# Paste all elements of the corpus together
corpus.tmp1 <- paste(corpus.tmp, collapse = " ")
# Inspect the resulting file
#corpus.tmp1
# Clean corpus
corpus.tmp2 <- enc2utf8(corpus.tmp1)
corpus.tmp2 <- gsub(" {2,}", " ", corpus.tmp2)
corpus.tmp2 <- str_replace_all(corpus.tmp2, fixed("\n"), " ")
corpus.tmp2 <- str_trim(corpus.tmp2, side = "both")
# Inspect the resulting file
#corpus.tmp2
###############################################################
# Specify searchpattern
splitpattern1 = "</I>"
# Split corpus
corpus.tmp4 <- strsplit(as.character(corpus.tmp2), splitpattern1)
# Inspect the resulting list
#corpus.tmp4
# Specify search pattern
splitpattern2 = "<I> "
# Splits corpus into parts
corpus.tmp5 <- lapply(corpus.tmp4, function(x) {
  strsplit(as.character(x), splitpattern2)  }  )
# Inspect the resulting list
#corpus.tmp5
# PROBLEM with corpus.tmp5[[1]][[249]]
# Repair corpus.tmp5[[1]][[249]]
corpus.tmp5[[1]] [[249]] [1] <- corpus.tmp5[[1]] [[248]] [1]
# Inspect the resulting list
#corpus.tmp5
# Inspect the resulting list
file.ids <- lapply(corpus.tmp5, function(x) {
  sapply(x, "[[", 1)  }  )
# Clean file ids
file.ids <- lapply(file.ids, function(x) {
  x <- str_trim(x)  }  )
file.ids <- unlist(file.ids)
file.ids <- lapply(file.ids, function(x) {
  ifelse(x == "", NA, x)  }  )
file.ids <- unlist(file.ids)
index.tmp1 <- sapply(file.ids, function(x) {
  is.na(x)  } )
index.tmp2 <- which(index.tmp1 == TRUE)
index.tmp3 <- sapply(index.tmp2, function(x) {
  x <- x-1   }  )
file.ids[index.tmp2] <- file.ids[index.tmp3]
file.ids[index.tmp2] <- file.ids[index.tmp3]
file.ids[index.tmp2] <- file.ids[index.tmp3]
file.ids[index.tmp2] <- file.ids[index.tmp3]
file.ids[index.tmp2] <- file.ids[index.tmp3]
file.ids[index.tmp2] <- file.ids[index.tmp3]
# View results
#file.ids
# Extract filename
file.names <- lapply(file.ids, function(x) {
  str_sub(x, 2, 8)  }  )
file.names <- sapply(file.names, function(x) {
  sapply(x, "[[", 1)  }  )
names(file.names) <- file.names
# View results
#file.names
# Extract subfile ids
subfile.ids.tmp1 <- sapply(as.vector(table(file.names)), function(x) {
  x <- str_replace_all(x, "2","1  2")
  x <- str_replace_all(x, "3","1 2 3")
  x <- str_replace_all(x, "4","1 2 3 4")
  x <- paste(x, collapse = " ")
  x <- strsplit(as.character(x), " ")
  x <- unlist(x)
  x <- sapply(x, "[", 1)  }  )
subfile.ids.tmp2 <- sapply(subfile.ids.tmp1, function(x) {
  x <- paste(x, collapse = " ")
  x <- strsplit(as.character(x), " ")  }  )
subfile.ids.tmp3 <- sapply(subfile.ids.tmp2, function(x) {
    x <- paste(x, collapse = " ")  }  )
subfile.ids.tmp4 <- paste(subfile.ids.tmp3, collapse = " ")
subfile.ids.tmp4 <- gsub(" {2,}", " ", subfile.ids.tmp4)
subfile.ids.tmp5 <- strsplit(as.character(subfile.ids.tmp4), " ")
subfile.ids.tmp5 <- strsplit(subfile.ids.tmp4, " ")
subfile.ids.tmp5 <- unlist(subfile.ids.tmp5)
subfile.ids <- subfile.ids.tmp5
# View results
#subfile.ids
# Transform corpus.tmp6 into a data frame
corpus.tmp7 <- as.data.frame(corpus.tmp5)
# Store results in vector
corpus.tmp8 <- corpus.tmp7[2, c(1:length(corpus.tmp7))]
# View results
#corpus.tmp8
# Convert into character strings
corpus.tmp9 <- as.character(corpus.tmp8)
# View results
#str(corpus.tmp9)
# Inspect broken files
#str(corpus.tmp9[144])
#str(corpus.tmp9[145])
# Repair broken files
corpus.tmp9 <- sapply(corpus.tmp9, function(x) {
  x <- str_replace_all(x, "(<&.*?/&> )","")   }  )
# View results
#str(corpus.tmp9)
# Add names to corpus.tmp9
names(corpus.tmp9) <- file.names
# View results
#str(corpus.tmp9)
###############################################################
# create a table out of the results
corpus.tmp10 <- as.data.frame(corpus.tmp9)
corpus.tmp11 <-t(corpus.tmp10)
corpus.tmp12 <- as.table(corpus.tmp11)
# View results
#corpus.tmp12
corpus.table1 <- cbind(file.names[1:length(file.names)], corpus.tmp12[,1:length(file.names)])
# View results
#corpus.table1
# Add id as a column
id <- 1:length(corpus.table1[, 1])
corpus.table2 <- cbind(id, corpus.table1)
corpus.table2 <- cbind(corpus.table2[, 1], corpus.table2[, 2], subfile.ids, corpus.table2[, 3])
# Add column labels
colnames(corpus.table2) <- c("id", "file", "subfile", "corpusfile")
# View results
#corpus.table2
# Add row labels
rownames(corpus.table2) <- c(1:length(corpus.table2[,1]))
corpus.table2 <- as.table(corpus.table2)
# View results
#corpus.table2
###############################################################
### --- STEP
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  corpfile2 <- strsplit( gsub("(<[A-Z][0-9][A-Z])", "~\\1", x), "~" )  }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)] , 1 , paste , collapse = " " )
names(all.files.unclean) <- file.subfile.ids
# View results
#all.files.unclean
###############################################################
# Separate the speakers from the speech.units
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2)  }  )
# View results
#speakers.and.speech.units
# Store speakers in extra vector
speakers <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
# View results
#speakers
# Store speech.units in extra vector
speech.units <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, function(x) x[2])  }  )
# View results
#speech.units
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
speech.units.clean <- lapply(speech.units, function(x) {
  x <- str_replace_all(x, "(<q.*?/q>)","")
  x <- str_replace_all(x, "(<&.*?/&.*>)","")
  x <- str_replace_all(x, "(<&.*?/&.*>)","")
  x <- str_replace_all(x, "(<[a-z]{4,}.*?</[a-z]{4,}>)","")
  x <- str_replace_all(x, "(<,>)", "")
  x <- str_replace_all(x, "(<,,>)", "")
  x <- str_replace_all(x, "(<\\..*?/.>)","")
  x <- str_replace_all(x, "(<\\[[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\[[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<\\{[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\{[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(</\\[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<\\]>)","")
  x <- str_replace_all(x, "(</\\]>)","")
  x <- str_replace_all(x, "(<\\}>)","")
  x <- str_replace_all(x, "(</\\}>)","")
  x <- str_replace_all(x, "(<un.*?clear>)","")
  x <- str_replace_all(x, "(</[0-9]{0,2}>)","")
  x <- str_replace_all(x, "(<#>)","")
  x <- str_replace_all(x, "(<\\.>)","")
  x <- str_replace_all(x, "(</\\.>)","")
  x <- str_replace_all(x, "(<I>)","")
  x <- str_replace_all(x, "(</I>)","")
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<X>.*?<X>)","")
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
# Final clean up
  x <- gsub("(<|\\{|(|/|\\.|\\[|>|\\}|)|\\])", "", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
# View results
#speech.units.clean
###############################################################
# Create a list which holds the number of speech.units
speech.unit.count <- lapply(speech.units, function(x){
  str_count(x, "<#>")  }  )
# View results
#speech.unit.count
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(speech.units.clean, function(x){
  tokenized <- strsplit(x, " ")  }  )
# View results
#tokenized
# Now, we count the words elements(words) in each speech.unit (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))   } )
# View results
#word.count
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# speech.unit.count, word.count)
###############################################################
speaker.and.unclean.speech.units <- mapply(cbind, speakers[], speech.units[], SIMPLIFY = F)
# View results
#speaker.and.unclean.speech.units
speaker.both.speech.units <- mapply(cbind, speaker.and.unclean.speech.units[], speech.units.clean[], SIMPLIFY = F)
names(speaker.both.speech.units) <- file.subfile.ids
# View results
#str(speaker.both.speech.units)
#speaker.both.speech.units
# Add file.subfile.ids to speaker.both.speech.units
speaker.both.speech.units.subfile <- mapply(cbind, speaker.both.speech.units[],names(speaker.both.speech.units[]), SIMPLIFY = F)
# View results
#speaker.both.speech.units.subfile
# Add speech.unit.counts
speaker.both.speech.units.subfile.and.speech.unit.count <- mapply(cbind, speaker.both.speech.units.subfile [], speech.unit.count[], SIMPLIFY = F)
# View results
#speaker.both.speech.units.subfile.and.speech.unit.count
speakerinfo1 <- mapply(cbind, speaker.both.speech.units.subfile.and.speech.unit.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# View results
#speakerinfo1
# We now need to convert the elements of the fourth and fifth
# column into numeric elements
speakerinfo2 <-lapply(speakerinfo1, function(x) {
  X <- as.data.frame(x[])
  X[, 5] <- as.numeric(X[, 5])
  X[, 6] <- as.numeric(X[, 6])
  x <- X  }  )
# View results
#speakerinfo2
###############################################################
# Rename data for later kwic seraches
kwic.tb.ice.ire <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum))) } )
# View results
#word.count.result
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# View results
#overview.word.count.results
# Extract the speech.unit counts for speakers in one file
speech.unit.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum))) } )
# View results
#speech.unit.count.result
# Simplify the results
overview.speech.unit.count.results <- sapply(speech.unit.count.result, "[[", 1)
# View results
#overview.speech.unit.count.results
###############################################################
# We now want to extract the speaker ids in vector format so
# that we can easily create a table out of the results
speaker.id.list <- lapply(overview.word.count.results, function(x) {
  sapply(names(x), function(y) {
    sapply(y, "[[", 1)   }  )  }  )
speaker.ids <- as.vector(unlist(speaker.id.list))
# View results
#speaker.ids
###############################################################
# We now want to extract the subfile ids in vector format so
# that we can easily create a table out of the results
# First we determine how many speakers are in a file
n.speakers.file <- sapply(overview.word.count.results, function(x) {
  length(x)  }  )
# Then, we create a matrix with the names to be replicates in column 1
# and the number of times they are supposed to replicated in column 2
names.and.nspeakers.tb <- cbind(names(n.speakers.file), n.speakers.file)
# Create an index for repetition
index <- rep(1:nrow(names.and.nspeakers.tb), names.and.nspeakers.tb [ , 2])
# And save the result in a vector
subfile.ids.tmp1 <- names.and.nspeakers.tb[index, ]
subfiles.tmp <- as.vector(subfile.ids.tmp1[, 1])
subfiles <- gsub(".* ", "", subfiles.tmp)
# View results
#subfiles
###############################################################
# From the speaker.ids vector, we also extract the file names and
# the speaker.ids
# First, let’s extract the file names
files <- gsub(" .*", "", subfiles.tmp)
# View results
#files
# Now, let’s extract the speaker.ids
speakers <- gsub("<", "", speaker.ids)
speakers <- gsub(">", "", speakers)
speakers <- gsub(".*\\$", "", speakers, perl = T)
# View results
#speakers
###############################################################
# We now want to extract the speech.unit counts for each speaker
# in vector format so that we can easily create a table out of
# the results
speech.unit.count.list <- lapply(X = overview.speech.unit.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)
    }  )  }  )
# View results
#speech.unit.count.list
# Convert the list into a vector
speech.unit.counts <- as.vector(unlist(speech.unit.count.list))
# View results
#speech.unit.counts
###############################################################
# We now want to extract the word counts for each speaker
# in vector format so that we can easily create a table out of
# the results
word.count.list <- lapply(X = overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)
    } )  }  )
# View results
#word.count.list
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
# View results
#word.counts
###############################################################
# We now want to create a table with speaker id, speech.unit count
# and word count
# First, we create an index
id <- c(1:length(speaker.ids))
# Now, we set up the data frame
speakerinfo.ice <- cbind(id, speaker.ids, files, subfiles, speakers,
  speech.unit.counts, word.counts)
speakerinfo.ice <- as.data.frame(speakerinfo.ice)
colnames(speakerinfo.ice ) <- c("id", "file.speaker.id", "file",
  "subfile", "speakers", "speech.unit.count", "word.count")
# View results
#speakerinfo.ice
# View results (without empty rows)
speakerinfo.ice.1 <- speakerinfo.ice[!speakerinfo.ice[, 2] == "", ]
speakerinfo.ice.1[, 6] <- as.numeric(speakerinfo.ice.1[, 6])
speakerinfo.ice.1[, 7] <- as.numeric(speakerinfo.ice.1[, 7])
speakerinfo.ice.1[, 1] <- 1:length(speakerinfo.ice.1[, 1])
rownames(speakerinfo.ice.1) <- speakerinfo.ice.1[, 1]
# View results
#speakerinfo.ice.1
################################################################
### --- STEP
################################################################
# Load biodata
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
#biodata.ice.tmp4
#head(biodata.ice.tmp4)
###############################################################
# Replace abbreviations with actual values
# zone
biodata.ice.tmp4[, 1] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 1], function(x) {
  ifelse(x == "N", "northern ireland",
  ifelse(x == "S", "republic of ireland",
  ifelse(x == "M", "mixed between ni and roi",
  ifelse(x == "X", "non-corpus speaker",
  ifelse(x == "NA", NA, x
  )))))  } )))
# date of recording
biodata.ice.tmp4[, 4] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 4], function(x) {
  ifelse(x == "A", "1990-1994",
  ifelse(x == "B", "1995-2001",
  ifelse(x == "C", "2002-2005",
  ifelse(x == "NA", NA, x
  ))))  } )))
# gender
biodata.ice.tmp4[, 7] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 7], function(x) {
  ifelse(x == "F", "female",
  ifelse(x == "M", "male",
  ifelse(x == "NA", NA,
  ifelse(x == "nag", NA, x
  ))))  } )))
# age
biodata.ice.tmp4[, 8] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 8], function(x) {
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
biodata.ice.tmp4[, 9] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 9], function(x) {
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
biodata.ice.tmp4[, 10] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 10], function(x) {
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
biodata.ice.tmp4[, 11] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 11], function(x) {
  ifelse(x == "FID", "first degree",
  ifelse(x == "PGQ", "postgraduate qualification",
  ifelse(x == "PHD", "doctoral degree",
  ifelse(x == "PRI", "primary education",
  ifelse(x == "SES", "secondary school qualification",
  ifelse(x == "SSE", "some secondary education",
  ifelse(x == "STE", "some tertiary education",
  ifelse(x == "TEQ", "non-degree tertiary qualification",
  ifelse(x == "NA", NA, x
  )))))))))   } )))
# religion
biodata.ice.tmp4[, 13] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 13], function(x) {
  ifelse(x == "C", "catholic",
  ifelse(x == "P", "protestant",
  ifelse(x == "J", "jewish",
  ifelse(x == "", NA,
  ifelse(x == "NA", NA, NA
  )))))   }  )))
# mother tongue
biodata.ice.tmp4[, 14] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 14], function(x) {
  ifelse(x == "ENG", "english",
  ifelse(x == "GLE", "irish gaelic",
  ifelse(x == "ENG GLE", "english & irish gaelic",
  ifelse(x == "GLE ENG", "english & irish gaelic",
  ifelse(x == "", NA,
  ifelse(x == "NA", NA, NA
  ))))))   }  )))
# other.languages
biodata.ice.tmp4[, 15] <- as.vector(unlist(sapply(biodata.ice.tmp4[, 15], function(x) {
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
biodata.ice.tmp4[, c(3, 12)] <- tolower(biodata.ice.tmp4[, c(3, 12)])
# Inspect data
#biodata.ice.tmp4
#head(biodata.ice.tmp4)
# Add id to enable restoring original order
biodata.ice.tmp5 <- cbind(1: length(biodata.ice.tmp4[, 1]), biodata.ice.tmp4)
colnames(biodata.ice.tmp5) <- c("orig.id", colnames(biodata.ice.tmp4))
# order data
biodata.ice.tmp6 <- biodata.ice.tmp5[order(biodata.ice.tmp5[, 3],biodata.ice.tmp5[, 7]), ]
# Add id to ease merging the data sets
biodata.ice.tmp7 <- cbind(1: length(biodata.ice.tmp6[, 1]), biodata.ice.tmp6)
# Add column names
colnames(biodata.ice.tmp7) <- c("id", colnames(biodata.ice.tmp6))
# Inspect data
#biodata.ice.tmp7
#head(biodata.ice.tmp7)
################################################################
### --- STEP
################################################################
# We will now merge the two data sets
# Inspect data sets which will be merged
#head(speakerinfo.ice.1)
#head(biodata.ice.tmp7)
# Rename variables to allow merging
colnames(speakerinfo.ice.1)[3] <- "text.id"
colnames(speakerinfo.ice.1)[5] <- "spk.ref"
# Transform data sets into data frames
speakerinfo.ice.1 <- as.data.frame(speakerinfo.ice.1)
biodata.ice.tmp7 <- as.data.frame(biodata.ice.tmp7)
# Join data sets (without speakers that do not occur in the corpus
# but do occur in the biodata spreadsheet provided by the
# corpus compilers) RECOMMENDED
biodata.ice.ire.tmp2 <- join(speakerinfo.ice.1, biodata.ice.tmp7, by = c("text.id", "spk.ref"), type = "left")
# Delete superfluous columns (info is already contained in another column)
biodata.ice.ire.tmp3 <- cbind(1:length(biodata.ice.ire.tmp2[, 1]), biodata.ice.ire.tmp2[, 2:7], biodata.ice.ire.tmp2[, 9:length(biodata.ice.ire.tmp2[1, ])])
colnames(biodata.ice.ire.tmp3)[1] <- "id"
# Inspect data
#head(biodata.ice.ire.tmp3)
# Reorder the columns so that the speech.unit.count and word.count is
# in the last column
biodata.ice.ire.tmp4 <- cbind(biodata.ice.ire.tmp3[, c(1, 8, 2:4, 12, 5, 9:11, 13:21, 6:7)])
# Inspect data
#head(biodata.ice.ire.tmp4)
# Rename data
biodata.ice.ire.1.2.2 <- biodata.ice.ire.tmp4
# Inspect data
#head(biodata.ice.ire.1.2.2)
###############################################################
###                   ICE Jamaica
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.jam, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T)   }  )
# Remove all written texts
corpus.tmp <- corpus.tmp[1:301]
# Paste all text elements in one file togehter
corpus.tmp1 <- lapply(corpus.tmp, function(x) {
  x <- paste(x, collapse = " ")  }  )
# View results
#str(corpus.tmp1)
# Create vector holding the text.ids
text.ids <- gsub(".*ICE Jamaica/", "", corpus.files)
text.ids <- gsub(".txt", "", text.ids)
text.ids <- str_trim(text.ids, side = "both")
text.ids <- text.ids[1:301]
# Name corpus elements
names(corpus.tmp1) <- text.ids
# Attach names to corpus files
corpus.tmp2 <- paste("<ICE-JAM:", names(corpus.tmp1), ">", corpus.tmp1, collapse = "")
# Clean resulting list
corpus.tmp3 <- lapply(corpus.tmp2, function(x) {
  x <- gsub("<ICE-JAM: ", "<ICE-JAM:", x)
  x <- gsub("( >)", ">", x)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)
  x <- gsub(" <I> ", "<I>", x)
  x <- gsub(" <I>", "<I>", x)
  x <- gsub("<I> ", "<I>", x) }  )
# View results
#str(corpus.tmp3)
corpus.tmp <- unlist(corpus.tmp3)
# View results
#corpus.tmp3
# Paste all elements of the corpus together
corpus.tmp4 <- paste(corpus.tmp3, collapse = " ")
# Inspect the resulting file
#corpus.tmp4
# Clean corpus
corpus.tmp5 <- enc2utf8(corpus.tmp4)
corpus.tmp5 <- gsub(" {2,}", " ", corpus.tmp5)
corpus.tmp5 <- str_replace_all(corpus.tmp5, fixed("\n"), " ")
corpus.tmp5 <- str_trim(corpus.tmp5, side = "both")
# Inspect the resulting file
#corpus.tmp5
###############################################################
# Specify searchpattern
splitpattern1 = "</I>"
# Split corpus
corpus.tmp6 <- strsplit(as.character(corpus.tmp5), splitpattern1)
# Inspect the resulting list
#corpus.tmp6
# Specify search pattern
splitpattern2 = "<I>"
# Splits corpus into parts
corpus.tmp7 <- lapply(corpus.tmp6, function(x) {
  strsplit(as.character(x), splitpattern2)   }  )
# Inspect the resulting list
#corpus.tmp7
# Repair broken elements
repair.tmp1 <- corpus.tmp7
repair.tmp1[[1]][[26]][2] <- repair.tmp1[[1]][[26]][1]
repair.tmp1[[1]][[26]][1] <- gsub("026>.*", "026>", repair.tmp1[[1]][[26]][1])
repair.tmp1[[1]][[212]][2] <- repair.tmp1[[1]][[212]][1]
repair.tmp1[[1]][[212]][1] <- gsub("005>.*", "005>", repair.tmp1[[1]][[212]][1])
repair.tmp1[[1]] <- repair.tmp1[[1]][c(1:295, 295:length(repair.tmp1[[1]]))]
repair.tmp1[[1]][[296]] <- repair.tmp1[[1]][[295]][3]
repair.tmp1[[1]][[295]] <- repair.tmp1[[1]][[295]][1:2]
repair.tmp1[[1]][[295]] <- unlist(repair.tmp1[[1]][[295]])
repair.tmp1[[1]][[296]] [2] <- repair.tmp1[[1]][[296]] [1]
repair.tmp1[[1]][[296]] [1] <- "<ICE-JAM:S2A-059>"
# Repair broken elements
corpus.tmp7 <- repair.tmp1
# Extract the text.ids
file.names <- sapply(corpus.tmp7, function(x) {
  sapply(x, "[[", 1)   }  )
file.names <- str_trim(file.names, side = "both")
# Inspect the resulting list
#file.names
# Repair broken text.ids
file.names <- gsub(">.*", ">", file.names)
file.names <- gsub(".*<", "<", file.names)
file.names <- as.vector(file.names)
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
index <- which(file.names == "")
file.names[index] <- file.names[index-1]
file.names[156] <- file.names[155]
file.names <- toupper(file.names)
file.names <- gsub("<ICE-JAM:", "", file.names)
file.names <- gsub(">", "", file.names)
# Extract subfile
subfiles.tmp1 <- table(file.names)
subfiles.tmp2 <- gsub("2", "1 2", subfiles.tmp1)
subfiles.tmp2 <- gsub("3", "1 2 3", subfiles.tmp2)
subfiles.tmp2 <- gsub("4", "1 2 3 4", subfiles.tmp2)
subfiles.tmp2 <- gsub("5", "1 2 3 4 5", subfiles.tmp2)
subfiles.tmp2 <- gsub("6", "1 2 3 4 5 6", subfiles.tmp2)
subfiles.tmp2 <- gsub("7", "1 2 3 4 5 6 7", subfiles.tmp2)
subfiles.tmp2 <- gsub("8", "1 2 3 4 5 6 7 8", subfiles.tmp2)
subfiles.tmp3 <- paste(subfiles.tmp2, collapse = " ")
subfiles.tmp4 <- strsplit(subfiles.tmp3, " ")
subfiles <- as.vector(unlist(subfiles.tmp4))
# Extract the corpus content
corpus.tmp8 <- as.vector(unlist(sapply(corpus.tmp7, function(x) {
  sapply(x, "[", 2) }  )))
corpus.tmp8 <- str_trim(corpus.tmp8, side = "both")
# Repair broken elements
corpus.tmp8[[7]] <- gsub(".*At the centre", "<ICE-JAM:$A><#>At the centre", corpus.tmp8[[7]])
corpus.tmp8[[26]] <- gsub("<ICE-JAM:S1A-026> ", "", corpus.tmp8[[26]])
corpus.tmp8[[63]] <- gsub("<&>TV-in-the-background-throughout-the-recording</&>", "", corpus.tmp8[[63]])
corpus.tmp8[[64]] <- gsub("<&>TV-in-the-background-throughout-recording</&>", "", corpus.tmp8[[64]])
corpus.tmp8[[86]] <- gsub("<&>throughout-whole-recording-talking-in-background</&>", "", corpus.tmp8[[86]])
corpus.tmp8[[91]] <- gsub("<&>throughout-the-recording-high-level-of-background-noise</&>", "", corpus.tmp8[[91]])
corpus.tmp8[[107]] <- gsub(".*Okay lets go back up the", "<ICE-JAM:$A><#>Okay lets go back up the", corpus.tmp8[[107]])
corpus.tmp8[[167]] <- gsub(".*Anyhow we are now pleased ", "<ICE-JAM:$A><#>Anyhow we are now pleased", corpus.tmp8[[167]])
corpus.tmp8[[212]] <- gsub("<ICE-JAM:S2A-005> ", "", corpus.tmp8[[212]])
corpus.tmp8[[260]] <- gsub("Madam Secretary<,> guest speaker ladies", "<#>Madam Secretary<,> guest speaker ladies", corpus.tmp8[[260]])
corpus.tmp8[[265]] <- gsub("Uhm enforces radio and the radio", "<#>Uhm enforces radio and the radio", corpus.tmp8[[265]])
corpus.tmp8[[271]] <- gsub("Good evening everybody<,> <#>Yeah dancehall", "<#>Good evening everybody<,> <#>Yeah dancehall", corpus.tmp8[[271]])
corpus.tmp8[[282]] <- gsub("Professor Errol Morrison<,> and so", "<#>Professor Errol Morrison<,> and so", corpus.tmp8[[282]])
corpus.tmp8[[329]] <- gsub(".*World Trade Organisation W T O collapsed", "<ICE-JAM:$A><#>World Trade Organisation W T O collapsed", corpus.tmp8[[329]])
corpus.tmp8[[331]] <- gsub(".*Good evening <#>", "<ICE-JAM:$A><#>Good evening <#>", corpus.tmp8[[331]])
corpus.tmp8 <- gsub("<", " <", corpus.tmp8)
corpus.tmp8 <- gsub(">", "> ", corpus.tmp8)
corpus.tmp8 <- gsub(" {2,}", " ", corpus.tmp8)
corpus.tmp8 <- str_trim(corpus.tmp8, side = "both")
# Create a table from the file names, subfiles and corpus content
ice.jam.tb1 <- cbind(1:length(corpus.tmp8), file.names, subfiles, corpus.tmp8)
colnames(ice.jam.tb1) <- c("id", "text.id", "subfile.id", "content")
ice.jam.tb1 <- as.data.frame(ice.jam.tb1)
###############################################################
### --- STEP
###############################################################
# Extract the corpus file
all.files <- ice.jam.tb1[1:nrow(ice.jam.tb1), 4]
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  strsplit( gsub("(<ICE)", "~~\\1", x), "~~" )  }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(ice.jam.tb1[ , c(2, 3)] , 1 , paste , collapse = " " )
names(all.files.unclean) <- file.subfile.ids
###############################################################
# Separate the speakers from the speech.units
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2)  }  )
# View results
#speakers.and.speech.units
# Store speakers in extra vector
speakers <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
# Store speech.units in extra vector
speech.units <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, function(x) x[2])   }  )
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
speech.units.clean <- lapply(speech.units, function(x) {
  x <- str_replace_all(x, "(<q.*?/q>)","")
  x <- str_replace_all(x, "(<&.*?/&.*>)","")
  x <- str_replace_all(x, "(<O.*?/O>)","")
  x <- str_replace_all(x, "(<un.*?/unclear>)","")
#  x <- str_replace_all(x, "(<[a-z]{4,}.*?</[a-z]{4,}>)","")
  x <- str_replace_all(x, "(<\\..*?/.>)","")
# WARNING: THEORETICAL ISSUE
#  x <- str_replace_all(x, "(<X>)","")
#  x <- str_replace_all(x, "(</X>)","")
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<X>.*)","")
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
# Repair broken elements
speech.units.clean[[6]][3] <- gsub("word(s)", "", speech.units.clean[[6]][3])
###############################################################
# Create a list which holds the number of speech.units
speech.unit.count <- lapply(speech.units, function(x){
  str_count(x, "<#>")  }  )
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(speech.units.clean, function(x){
  tokenized <- strsplit(x, " ") }  )
# Now, we count the words elements(words) in each speech.unit (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))  } )
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# speech.unit.count, word.count)
###############################################################
speaker.and.unclean.speech.units <- mapply(cbind, speakers[], speech.units[], SIMPLIFY = F)
speaker.both.speech.units <- mapply(cbind, speaker.and.unclean.speech.units[], speech.units.clean[], SIMPLIFY = F)
names(speaker.both.speech.units) <- file.subfile.ids
# Add file.subfile.ids to speaker.both.speech.units
speaker.both.speech.units.subfile <- mapply(cbind, speaker.both.speech.units[],names(speaker.both.speech.units[]), SIMPLIFY = F)
# Add speech.unit.counts
speaker.both.speech.units.subfile.and.speech.unit.count <- mapply(cbind, speaker.both.speech.units.subfile [], speech.unit.count[], SIMPLIFY = F)
# Combine results in a matrix
speakerinfo1 <- mapply(cbind, speaker.both.speech.units.subfile.and.speech.unit.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# View results
#speakerinfo1
# We now need to convert the elements of the fourth and fifth column into numeric elements
speakerinfo2 <-lapply(speakerinfo1, function(x) {
  X <- as.data.frame(x[])
  X[, 5] <- as.numeric(X[, 5])
  X[, 6] <- as.numeric(X[, 6])
  x <- X  }  )
# Repair broken element
speakerinfo2[[295]] <- speakerinfo2[[295]][1:95, ]
speakerinfo2[[58]][, 1] <- as.vector(unlist(sapply(speakerinfo2[[58]][, 1], function(x) {
  ifelse(x == "<ICE-JAM:$A", "<ICE-JAM:$A>", x <- x)  } )))
###############################################################
# Rename data for later kwic seraches
kwic.tb.ice.jam <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum))) } )
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
overview.word.count.results <- sapply(overview.word.count.results, function(x) {
  x <- x[2:length(x)]  }  )
# Extract the speech.unit counts for speakers in one file
speech.unit.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum))) } )
# Simplify the results
overview.speech.unit.count.results <- sapply(speech.unit.count.result, "[[", 1)
#Delete empty elements
overview.speech.unit.count.results <- sapply(overview.speech.unit.count.results, function(x) {
  x <- x[2:length(x)] }  )
###############################################################
# We now want to extract the speaker ids in vector format so
# that we can easily create a table out of the results
speaker.id.list <- lapply(overview.word.count.results, function(x) {
  sapply(names(x), function(y) {
    sapply(y, "[[", 1) }  )  }  )
speaker.ids <- as.vector(unlist(speaker.id.list))
###############################################################
# We now want to extract the subfile ids in vector format so
# that we can easily create a table out of the results
# First we determine how many speakers are in a file
n.speakers.file <- sapply(overview.word.count.results, function(x) {
  length(x)  }  )
# Then, we create a matrix with the names to be replicates in column 1
# and the number of times they are supposed to replicated in column 2
names.and.nspeakers.tb <- cbind(names(n.speakers.file), n.speakers.file)
# Create an index for repetition
index <- rep(1:nrow(names.and.nspeakers.tb), names.and.nspeakers.tb [ , 2])
# And save the result in a vector
subfile.ids.tmp1 <- names.and.nspeakers.tb[index, ]
subfiles.tmp <- as.vector(subfile.ids.tmp1[, 1])
subfiles <- gsub(".* ", "", subfiles.tmp)
###############################################################
# From the speaker.ids vector, we also extract the file names and the speaker.ids
# First, let’s extract the file names
files <- gsub(" .*", "", subfiles.tmp)
# Now, let’s extract the speaker.ids
speakers <- gsub("<", "", speaker.ids)
speakers <- gsub(">", "", speakers)
speakers <- gsub(".*\\$", "", speakers, perl = T)
###############################################################
# We now want to extract the speech.unit counts for each speaker
# in vector format so that we can easily create a table out of
# the results
speech.unit.count.list <- lapply(X = overview.speech.unit.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)  }  )  }  )
# Convert the list into a vector
speech.unit.counts <- as.vector(unlist(speech.unit.count.list))
###############################################################
# We now want to extract the word counts for each speaker
# in vector format so that we can easily create a table out of
# the results
word.count.list <- lapply(X = overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) } ) }  )
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
###############################################################
# We now want to create a table with speaker id, speech.unit count and word count
# First, we create an index
id <- c(1:length(speaker.ids))
# Now, we set up the data frame
speakerinfo.ice <- cbind(id, speaker.ids, files, subfiles, speakers,
  speech.unit.counts, word.counts)
speakerinfo.ice <- as.data.frame(speakerinfo.ice)
colnames(speakerinfo.ice ) <- c("id", "file.speaker.id", "text.id",
  "subfile.id", "spk.ref", "speech.unit.count", "word.count")
# Remove empty rows
speakerinfo.ice.1 <- speakerinfo.ice[!speakerinfo.ice[, 2] == "", ]
speakerinfo.ice.1[, 6] <- as.numeric(speakerinfo.ice.1[, 6])
speakerinfo.ice.1[, 7] <- as.numeric(speakerinfo.ice.1[, 7])
speakerinfo.ice.1[, 1] <- 1:length(speakerinfo.ice.1[, 1])
rownames(speakerinfo.ice.1) <- speakerinfo.ice.1[, 1]
# Rename data set
speakerinfo.ice.jam <- speakerinfo.ice.1
################################################################
### --- STEP
################################################################
# Load biodata
biodata.tmp1 <- read.delim(bio.jam, header = T, sep = "\t")
# Remove written data
biodata.tmp1 <- biodata.tmp1[1:971, ]
# Combine data sets
colnames(biodata.tmp1)[1:6] <- c("id.orig", "text.cat", "text.id", "subfile.id", "no.interlocs", "spk.ref")
colnames(biodata.tmp1)[12:15] <- c("age", "sex", "education", "education-level")
biodata.tmp1[, 3] <- gsub("", "", biodata.tmp1[, 3])
cat <- gsub("-.*", "", biodata.tmp1[, 3])
no <- gsub("[A-Z][0-9][A-Z]-", "", biodata.tmp1[, 3])
no <- gsub("-.*", "", no)
text.id.new <- paste(cat, no, collapse = "-")
text.id.new1 <- strsplit(text.id.new, "-")
biodata.tmp1[, 3] <- as.vector(unlist(sapply(text.id.new1, function(x) {
  x <- gsub(" ", "-", x) } )))
biodata.tmp1[, 4] <- as.vector(unlist(sapply(biodata.tmp1[, 4], function(x) {
  ifelse(x == "", "1", x <- x) } )))
# Transform data sets into data frames
speakerinfo.ice.jam <- as.data.frame(speakerinfo.ice.jam)
biodata.tmp1 <- as.data.frame(biodata.tmp1)
# Join data sets (without speakers that do not occur in the corpus but do occur in the biodata spreadsheet provided by the corpus compilers) RECOMMENDED
biodata.ice.jam <- join(speakerinfo.ice.jam, biodata.tmp1, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
# Delete superfluous columns
biodata.ice.jam <- biodata.ice.jam[, 1:31]
# Trim cell contents
biodata.ice.jam <- as.matrix(biodata.ice.jam)
strip.strs <- function(x) { str_trim(x, side = "both") }
biodata.ice.jam[] <- strip.strs(biodata.ice.jam)
# Fill empty cells
biodata.ice.jam <- as.matrix(biodata.ice.jam)
repl.w.na <- function(x) {ifelse(x == "", NA,
    ifelse(x == "NA", NA, x <- x))}
biodata.ice.jam[] <- repl.w.na(biodata.ice.jam)
# Clean data
biodata.ice.jam[390, 10] <- NA
biodata.ice.jam[, 11] <- as.vector(unlist(sapply(biodata.ice.jam[, 11], function(x) {
  x <- gsub(".*tra.*", "extra-corpus speaker", x) } )))
# Clean variable "first language"
biodata.ice.jam[, 15] <- as.vector(unlist(sapply(biodata.ice.jam[, 15], function(x) {
  ifelse(x == "N/A", NA,
  ifelse(x == "Patois and Standard English", "English/Patois",
  ifelse(x == "English,Patois", "English/Patois",
  ifelse(x == "Standard English", "English",
  ifelse(x == "Jamaican Standard English", "Jamaican English",
  ifelse(x == "English, Patois", "English/Patois",
  ifelse(x == "Creole", "Jamaican Creole",
  ifelse(x == "Creole, English", "Jamaican Creole/English",
  ifelse(x == "Jamaican English and Jamaican Creole", " Jamaican English/Jamaican Creole", x)))))))))  } )))
# Clean variable "age"
biodata.ice.jam[, 16] <- as.vector(unlist(sapply(biodata.ice.jam[, 16], function(x) {
  ifelse(x == "17-25", "18-25",
  ifelse(x == "22", "18-25",
  ifelse(x == "26-45?", "26-45",
  ifelse(x == "45+", "46+",
  ifelse(x == "45-65", "46+",
  ifelse(x == "40+", NA,
  ifelse(x == "45", "26-45",
  ifelse(x == "20-30", NA,
  ifelse(x == "46-65", "46+",
  ifelse(x == "66+", "46+", x))))))))))  } )))
# Clean variable "sex"
biodata.ice.jam[, 17] <- as.vector(unlist(sapply(biodata.ice.jam[, 17], function(x) {
  ifelse(x == "1", "male",
  ifelse(x == "2", "female", NA)
  )  } )))
# Clean variable "educational background"
biodata.ice.jam[, 18] <- as.vector(unlist(sapply(biodata.ice.jam[, 18], function(x) {
  ifelse(x == "Medical doctor", "phd",
  ifelse(x == "PhD", "phd",
  ifelse(x == "Dr.", "phd",
  ifelse(x == "Professor", "phd",
  ifelse(x == "Reverend", NA,
  ifelse(x == "University degree", NA,
  ifelse(x == "University degree, Reverend", NA,
  ifelse(x == "Law school", NA, x))))))))  } )))
biodata.ice.jam[, 18] <- as.vector(unlist(sapply(biodata.ice.jam[, 18], function(x) {
  x <- gsub(".*M.*", "master's degree", x)
  x <- gsub(".*B.*", "bachelor's degree", x)  }  )))
# Clean variable "education level"
biodata.ice.jam[, 19] <- as.vector(unlist(sapply(biodata.ice.jam[, 19], function(x) {
  ifelse(x == "1", "secondary education",
    ifelse(x == "2", "university education", NA) )  } )))
# Clean variable "time/date"
biodata.ice.jam[, 22] <- as.vector(unlist(sapply(biodata.ice.jam[, 22], function(x) {
  x <- gsub(".*, ", "", x)
  x <- gsub(".*,", "", x)
  x <- gsub(" \\(aired\\)", "", x)
  x <- gsub(".* ", "", x)
  x <- gsub("\\?", "", x)
  x <- gsub("1990", "1990-1994", x)
  x <- gsub("1991", "1990-1994", x)
  x <- gsub("1992", "1990-1994", x)
  x <- gsub("1993", "1990-1994", x)
  x <- gsub("1994", "1990-1994", x)
  x <- gsub("1995", "1995-1999", x)
  x <- gsub("1996", "1995-1999", x)
  x <- gsub("1997", "1995-1999", x)
  x <- gsub("1998", "1995-1999", x)
  x <- gsub("1999", "1995-1999", x)
  x <- gsub("1999", "1995-1999", x)
  x <- gsub("2000", "2000-2004", x)
  x <- gsub("2001", "2000-2004", x)
  x <- gsub("2002", "2000-2004", x)
  x <- gsub("2003", "2000-2004", x)
  x <- gsub("2004", "2000-2004", x)
  x <- gsub("2006", "2000-2004", x)
  x <- gsub("2005", "2005-2008", x)
  x <- gsub("2007", "2005-2008", x)
  x <- gsub("2008", "2005-2008", x)
  x <- gsub(".*1990", "1990", x)
  x <- gsub(".*1995", "1995", x)
  x <- gsub(".*2000", "2000", x)
  x <- gsub(".*2005", "2005", x)  } )))
# Clean variable "audience"
biodata.ice.jam[, 26] <- as.vector(unlist(sapply(biodata.ice.jam[, 26], function(x) {
 x <- tolower(x)
    ifelse(x == "students and faculty", "students and faculty",
    ifelse(x == "university students and staff", "students and faculty",
    ifelse(x == "university students", "students",
    ifelse(x == "students and staff", "students", x <- x)
    )))  } )))
# Delete superfluous columns
biodata.ice.jam <- biodata.ice.jam[, c(1:26, 29:31)]
# Convert characters to lower case
biodata.ice.jam[, 8:length(biodata.ice.jam[1, ])] <- tolower(biodata.ice.jam[, 8:length(biodata.ice.jam[1, ])])
# Reorder data
biodata.ice.jam <- cbind(biodata.ice.jam[, c(1:5, 16:19, 8:15, 20:29, 6:7)])
# Inspect data
#head(biodata.ice.jam)
###############################################################
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
############################################
### --- STEP
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
biodata.ice.nz.tmp2 <- cbind(1:length(biodata.ice.nz.tmp1[, 1]), biodata.ice.nz.tmp1[, c(1:5, 9:12, 6:7)])
# Reorganize data set
colnames(biodata.ice.nz.tmp2) <- c("id", "id.orig", colnames(biodata.ice.nz.tmp2)[3:12])
# Rename data
biodata.ice.nz <- biodata.ice.nz.tmp2
# Inspect data
#head(biodata.ice.nz)
###############################################################
###############################################################
###                   ICE Philippines
###############################################################
###                   START
###############################################################
# Choose the files you would like to use
corpus.files = list.files(path = corpus.phi, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# Load and unlist corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T)  }  )
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
  strsplit(as.character(x), splitpattern2)  }  )
# Repair broken file S1B-061
x <- corpus.tmp5[[1]][153][[1]][2]
x <- sapply(x, function(x){
  gsub("<#","<ICE-PHI:S1B-061#", x, perl = T)  }  )
corpus.tmp5[[1]][153][[1]][2] <- x
# Inspect the resulting list
corpus.tmp6 <- lapply(corpus.tmp5, function(x) {
  sapply(x, "[", 2)  }  )
# Select only the spoken files
corpus.tmp7 <- corpus.tmp6[[1]][1:342]
# Repair broken element corpus.tmp7[[1]][128]
corpus.tmp7[128] <- corpus.tmp5[[1]][128]
# Repair broken element corpus.tmp7[[167]]
corpus.tmp7[[167]][1] <- gsub("(<ICE-PHI:S2A-005S2A-005ICE-PHI:)", "<ICE-PHI:", corpus.tmp7[[167]][1])
# Clean file ids
file.ids <- lapply(unlist(corpus.tmp7), function(x) {
  sapply(x, function(y){
    y <- gsub(".*ICE-PHI:","", y, perl = T)
    y <- gsub(":[A-Z]>","", y, perl = T)
    y <- gsub("?>","", y, fixed = T)
    y <- str_trim(y)
    y <- gsub(" .*", "", y, perl = T)  }  )  }  )
file.ids <- unlist(file.ids)
file.ids <- as.vector(file.ids)
# Repair broken file.ids
file.ids[43] <- "S1A-042#337:1"
file.ids[44] <- "S1A-043#337:1"
file.ids[46] <- "S1A-045#337:1"
file.ids[78] <- "S1A-077#337:1"
file.ids[90] <- "S1A-077#337:1"
file.ids[138] <- "S1B-046#337:1"
file.ids[155] <- "S1B-063#337:1"
file.ids[159] <- "S1B-067#337:1"
file.ids[169] <- "S2A-007#337:1"
file.ids[171] <- "S2A-009#337:1"
file.ids[191] <- "S2A-028#337:1"
file.ids[258] <- "S2B-001#337:1"
file.ids[267] <- "S2B-008#337:3"
file.ids[278] <- "S2B-012#337:4"
file.ids[279] <- "S2B-013#337:1"
file.ids[293] <- "S2B-016#337:8"
file.ids[312] <- "S2B-024#337:1"
file.ids[316] <- "S2B-027#337:1"
file.ids[337] <- "S2B-045#337:1"
file.ids[339] <- "S2B-047#337:1"
file.ids <- sapply(file.ids, function(x){
  x <- gsub("#[0-9]{1,3}:","#", x, perl = T)
  x <- gsub(":","", x, perl = T)  }  )
file.ids <- as.vector(file.ids)
# Extract filename
file.names <- as.vector(unlist(lapply(file.ids, function(x) {
  x <- gsub("(#.*)", "", x)  }  )))
# Extract subfile ids
subfile.ids <- sapply(as.vector(file.ids), function(x) {
  x <- str_replace_all(x, ".*#", "")  }  )
# Clean corpus data
corpus.tmp9 <- sapply(corpus.tmp7, function(x) {
  x <- sub("(<.*?ICE-PHI)", "<ICE-PHI", x)  }  )
# Add names to corpus.tmp9
names(corpus.tmp9) <- file.names
###############################################################
# create a table out of the results
corpus.tmp10 <- as.data.frame(corpus.tmp9)
corpus.tmp11 <-t(corpus.tmp10)
corpus.tmp12 <- as.table(corpus.tmp11)
corpus.table1 <- cbind(file.names[1:length(file.names)], corpus.tmp12[,1:length(file.names)])
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
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  x <- gsub("(<ICE-PHI:)", "<", x)
  x <- strsplit( gsub("(<[A-Z][0-9][A-Z])", "~\\1", x), "~" ) }  )
all.files.unclean <- sapply(all.files.unclean, function(x) {
  x <- gsub("#X{0,1}[0-9]{0,3}:", "#", x)
  x <- gsub("#[0-9]{0,3}:>", "#?>", x)
  x <- gsub("#>", "#?>", x)  }  )
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)] , 1 , paste , collapse = " " )
names(all.files.unclean) <- file.subfile.ids
###############################################################
# Separate the speakers from the speech.units
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2) }  )
# Store speakers in extra vector
speakers <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
# Store speech.units in extra vector
speech.units <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, function(x) x[2])  }  )
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
speech.units.clean <- lapply(speech.units, function(x) {
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<X>.*)","")
  x <- str_replace_all(x, "(.*</X>)","")
  x <- str_replace_all(x, "(<X>)","")
  x <- str_replace_all(x, "(</X>)","")
#  x <- str_replace_all(x, "(<indig.*?/indig>)","")
  x <- str_replace_all(x, "(<q.*/q>)","")
  x <- str_replace_all(x, "(<&.*/&.*>)","")
  x <- str_replace_all(x, "(<O.*?/O>)","")
  x <- str_replace_all(x, "(<unclear.*?/unclear>)","")
  x <- str_replace_all(x, "(<un.*clear>)","")
  x <- str_replace_all(x, "(<[a-z]{4,}.*</[a-z]{4,}>)","")
#  x <- str_replace_all(x, "(<\\..*/.>)","")
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(<X>.*?</X>)","")
  x <- str_replace_all(x, "(<.*?>)", "")
# WARNING: THEORETICAL ISSUE
  x <- gsub(" {2,}", " ", x)
  x <- gsub(" re |'re ", "'re ", x)
  x <- gsub(" ll |'ll ", "'ll ", x)
  x <- gsub(" {0,1}Ill ", " I'll ", x)
  x <- gsub(" ve |'ve ", "'ve ", x)
  x <- gsub(" s ", "'s ", x)
  x <- gsub(" d ", "'d ", x)
  x <- gsub(" {0,1}I m ", " I'm ", x)
  x <- gsub("Im ", " I'm ", x)
  x <- gsub("Its", " It's ", x)
  x <- gsub(" its", " it's ", x)
  x <- gsub("Hes", " He's ", x)
  x <- gsub(" hes", " he's ", x)
  x <- gsub("Ive", " I've ", x)
  x <- gsub(" {0,1}Thats ", " That's ", x)
  x <- gsub(" thats ", " that's ", x)
  x <- gsub(" {0,1}Theres ", " There's ", x)
  x <- gsub(" theres ", " there's ", x)
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
  x <- gsub(" havent ", " haven't ", x)
  x <- gsub("Havent ", "Haven't ", x)
  x <- gsub("Wasnt ", "Wasn't ", x)
  x <- gsub(" wasnt ", " wasn't ", x)
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(speech.units.clean, function(x){
  tokenized <- strsplit(x, " ")  }  )
# Now, we count the words elements(words) in each speech.unit (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))} )
###############################################################
# Create a list which holds the number of speech.units
speech.unit.count <- sapply(word.count, function(x){
  sapply(x, function(y) {
    x <- 1  }  ) }  )
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# speech.unit.count, word.count)
###############################################################
speaker.and.unclean.speech.units <- mapply(cbind, speakers[], speech.units[], SIMPLIFY = F)
speaker.both.speech.units <- mapply(cbind, speaker.and.unclean.speech.units[], speech.units.clean[], SIMPLIFY = F)
names(speaker.both.speech.units) <- file.subfile.ids
# Add file.subfile.ids to speaker.both.speech.units
speaker.both.speech.units.subfile <- mapply(cbind, speaker.both.speech.units[],names(speaker.both.speech.units[]), SIMPLIFY = F)
# Add speech.unit.counts
speaker.both.speech.units.subfile.and.speech.unit.count <- mapply(cbind, speaker.both.speech.units.subfile [], speech.unit.count[], SIMPLIFY = F)
speakerinfo1 <- mapply(cbind, speaker.both.speech.units.subfile.and.speech.unit.count[], word.count[], SIMPLIFY = F)
# Add  names
names(speakerinfo1) <- file.subfile.ids
# We now need to convert the elements of the fourth and fifth column into numeric elements
speakerinfo2 <-lapply(speakerinfo1, function(x) {
  X <- as.data.frame(x[])
  X[, 5] <- as.numeric(X[, 5])
  X[, 6] <- as.numeric(X[, 6])
  x <- X }  )
###############################################################
# Rename data for later kwic searches
kwic.tb.ice.phi <- speakerinfo2
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum))) } )
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# Extract the speech.unit counts for speakers in one file
speech.unit.count.result <- lapply(X = speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum))) } )
# Simplify the results
overview.speech.unit.count.results <- sapply(speech.unit.count.result, "[[", 1)
###############################################################
# We now want to extract the speaker ids in vector format so
# that we can easily create a table out of the results
speaker.id.list <- lapply(overview.word.count.results, function(x) {
  sapply(names(x), function(y) {
    y <- sapply(y, "[[", 1) }  ) }  )
speaker.ids <- as.vector(unlist(speaker.id.list))
###############################################################
# We now want to extract the subfile ids in vector format so
# that we can easily create a table out of the results
# First we determine how many speakers are in a file
n.speakers.file <- sapply(overview.word.count.results, function(x) {
  length(x) }  )
# Then, we create a matrix with the names to be replicates in column 1
# and the number of times they are supposed to replicated in column 2
names.and.nspeakers.tb <- cbind(names(n.speakers.file), n.speakers.file)
# Create an index for repetition
index <- rep(1:nrow(names.and.nspeakers.tb), names.and.nspeakers.tb [ , 2])
# And save the result in a vector
subfile.ids.tmp1 <- names.and.nspeakers.tb[index, ]
subfiles.tmp <- as.vector(subfile.ids.tmp1[, 1])
subfiles <- gsub(".* ", "", subfiles.tmp)
###############################################################
# From the speaker.ids vector, we also extract the file names and the speaker.ids
# First, let’s extract the file names
files <- gsub(" .*", "", subfiles.tmp)
# Now, let’s extract the speaker.ids
speakers <- gsub("<.*:", "", speaker.ids)
speakers <- gsub(">", "", speakers)
speakers <- gsub(".*#", "", speakers, perl = T)
###############################################################
# We now want to extract the speech.unit counts for each speaker
# in vector format so that we can easily create a table out of the results
speech.unit.count.list <- lapply(X = overview.speech.unit.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) }  ) }  )
# Convert the list into a vector
speech.unit.counts <- as.vector(unlist(speech.unit.count.list))
###############################################################
# We now want to extract the word counts for each speaker in vector format so that we can easily create a table out of the results
word.count.list <- lapply(X = overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1) } ) }  )
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
###############################################################
# We now want to create a table with speaker id, speech.unit count and word count
# First, we create an index
id <- c(1:length(speaker.ids))
# Now, we set up the data frame
speakerinfo.ice <- cbind(id, speaker.ids, files, subfiles, speakers,
  speech.unit.counts, word.counts)
speakerinfo.ice <- as.data.frame(speakerinfo.ice)
colnames(speakerinfo.ice ) <- c("id", "file.speaker.id", "text.id",
  "subfile.id", "spk.ref", "speech.unit.count", "word.count")
# View results (without empty rows)
speakerinfo.ice.1 <- speakerinfo.ice[!speakerinfo.ice[, 2] == "", ]
speakerinfo.ice.1[, 6] <- as.numeric(speakerinfo.ice.1[, 6])
speakerinfo.ice.1[, 7] <- as.numeric(speakerinfo.ice.1[, 7])
speakerinfo.ice.1[, 1] <- 1:length(speakerinfo.ice.1[, 1])
rownames(speakerinfo.ice.1) <- speakerinfo.ice.1[, 1]
################################################################
### --- STEP
################################################################
# Load biodata
biodata.tmp1 <- read.delim(bio.phi, header = T, sep = "\t")
# Add column names
colnames(biodata.tmp1) [1] <- "text.id"
colnames(biodata.tmp1) [2] <- "subfile.id"
colnames(biodata.tmp1) [4] <- "spk.ref"
# Rename data
biodata.ice.tmp7 <- biodata.tmp1
################################################################
### --- STEP
################################################################
# We will now join the two data sets
# Transform data sets into data frames
speakerinfo.ice.1 <- as.data.frame(speakerinfo.ice.1)
biodata.ice.tmp7 <- as.data.frame(biodata.ice.tmp7)
# Join data sets (without speakers that do not occur in the corpus but do occur in the biodata spreadsheet provided by the corpus compilers) RECOMMENDED
biodata.ice.phi.tmp2 <- join(speakerinfo.ice.1, biodata.ice.tmp7, by = c("text.id", "subfile.id", "spk.ref"), type = "left")
colnames(biodata.ice.phi.tmp2) <- tolower(colnames(biodata.ice.phi.tmp2))
# Clean data
biodata.ice.phi.tmp2[, 10] <- as.vector(unlist(sapply(biodata.ice.phi.tmp2[, 10], function(x) {
  ifelse(x == "", NA, x)  }  )))
biodata.ice.phi.tmp2[, 15] <- as.vector(unlist(sapply(biodata.ice.phi.tmp2[, 15], function(x) {
  ifelse(x == "", NA, x)  }  )))
biodata.ice.phi.tmp2[, 18] <- as.vector(unlist(sapply(biodata.ice.phi.tmp2[, 18], function(x) {
  ifelse(x == "m", "male",
    ifelse(x == "f", "female", NA))    }  )))
biodata.ice.phi.tmp2[, 19] <- as.vector(unlist(sapply(biodata.ice.phi.tmp2[, 19], function(x) {
  ifelse(x == "(?)26-30", "26-30",
  ifelse(x == "2125", "21-25",
  ifelse(x == "above  50", "50+",
  ifelse(x == "above 50", "50+",
  ifelse(x == "", NA, x)))))  }  )))
biodata.ice.phi.tmp2[, 22] <- as.vector(unlist(sapply(biodata.ice.phi.tmp2[, 22], function(x) {
  ifelse(x == "", NA, x)  }  )))
biodata.ice.phi.tmp2[, 27] <- as.vector(unlist(sapply(tolower(biodata.ice.phi.tmp2[, 27]), function(x) {
  x <- gsub("ph.d.", "phd", x)
  x <- gsub("ph.d", "phd", x)
  x <- gsub("ph. d.", "phd", x)
  x <- gsub("ph. d", "phd", x)
  x <- gsub("phd/ university of louisiana at baton rouge", "phd", x)
  x <- gsub("ll.b", "ll.b.", x)
  x <- gsub("ll. b.", "ll.b.", x)
  x <- gsub("bachelor's", "b.a.", x)
  x <- gsub("master of laws", "m.a. law", x)
  x <- gsub("master's", "m.a.", x)
  x <- gsub("ma law", "m.a. law", x)
  x <- gsub("ma", "m.a.", x)
  ifelse(x == "", NA, x)  }  )))
# Delete superfluous columns
biodata.ice.phi.tmp3 <- cbind(1:length(biodata.ice.phi.tmp2[, 1]), biodata.ice.phi.tmp2[, c(1:11, 15:19, 21:22, 25, 27, 32)])
# Update column names
colnames(biodata.ice.phi.tmp3)[1] <- "id"
colnames(biodata.ice.phi.tmp3)[2] <- "orig.id"
# Convert all letters to lower case
biodata.ice.phi.tmp3[, 9] <- tolower(biodata.ice.phi.tmp3[, 9])
biodata.ice.phi.tmp3[, 10] <- tolower(biodata.ice.phi.tmp3[, 10])
biodata.ice.phi.tmp3[, 11] <- tolower(biodata.ice.phi.tmp3[, 11])
biodata.ice.phi.tmp3[, 12] <- tolower(biodata.ice.phi.tmp3[, 12])
biodata.ice.phi.tmp3[, 13] <- tolower(biodata.ice.phi.tmp3[, 13])
biodata.ice.phi.tmp3[, 14] <- tolower(biodata.ice.phi.tmp3[, 14])
biodata.ice.phi.tmp3[, 15] <- tolower(biodata.ice.phi.tmp3[, 15])
biodata.ice.phi.tmp3[, 16] <- tolower(biodata.ice.phi.tmp3[, 16])
biodata.ice.phi.tmp3[, 17] <- tolower(biodata.ice.phi.tmp3[, 17])
biodata.ice.phi.tmp3[, 18] <- tolower(biodata.ice.phi.tmp3[, 18])
biodata.ice.phi.tmp3[, 19] <- tolower(biodata.ice.phi.tmp3[, 19])
biodata.ice.phi.tmp3[, 20] <- tolower(biodata.ice.phi.tmp3[, 20])
biodata.ice.phi.tmp3[, 21] <- tolower(biodata.ice.phi.tmp3[, 21])
biodata.ice.phi.tmp3[, 22] <- tolower(biodata.ice.phi.tmp3[, 22])
biodata.ice.phi <- cbind(biodata.ice.phi.tmp3[, c(1:6, 9:22, 7:8)])
###############################################################
##################################################################
###                   Santa Barbara Corpus
###############################################################
###                   START
###############################################################
# Specify pathname of the sbc corpus
corpus.sbc = "C:\\PhD\\skripts n data\\corpora\\SBCAE\\corpusdata\\TRN"
# Define input pathnames of the metadata
bio.sbc.1 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata1.csv"
bio.sbc.2 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata2.csv"
bio.sbc.3 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata3.csv"
bio.sbc.4 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata4.csv"
# Define outputpath
out.sbc <- "C:\\PhD\\skripts n data/biodata sbcae.txt"
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = corpus.sbc,
  pattern = NULL,
  all.files = T,
  full.names = T,
  recursive = T,
  ignore.case = T,
  include.dirs = T)
###############################################################
# Load and store corpus (optimaler)
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x,
  what = "char",
  quiet = T,
  encoding = "UTF-8")
  }  )
###############################################################
# Extract the corpus file ids
# Write function to extract the file ids
file.ids <- lapply(corpus.files, function(x) {
  x <- gsub("(.* )", "", x, perl = T)
  x <- gsub("(.*S)", "S", x, perl = T)
  x <- gsub("\\.trn", "\\1", x, perl = T)  }  )
# View results
#str(file.ids)
###############################################################
# Write function to collapse the file content
corpus.tmp1 <- lapply(corpus.tmp, function(x) {
  x <- paste(x, collapse = " ")  }  )
# Add names
names(corpus.tmp1) <- file.ids
# View results
#str(corpus.tmp1)
###############################################################
# Merge the file.ids with the file content
corpus.tmp2 <- apply(cbind(file.ids, corpus.tmp1), 1, function(x) unname(x))
# View results
#str(corpus.tmp2)
###############################################################
# Extract the file content
corpus.content.orig.tmp1 <- sapply(corpus.tmp2, "[[", 2)
names(corpus.content.orig.tmp1) <- file.ids
# View results
#str(corpus.content.orig.tmp1)
###############################################################
# Clean corpus (corpus.content.orig.tmp1)
#corpus.content.orig.tmp2 <- enc2utf8(corpus.content.orig.tmp1)
corpus.content.orig.tmp2 <- corpus.content.orig.tmp1
corpus.content.orig.tmp3 <- gsub(" {2,}", " ", corpus.content.orig.tmp2)
corpus.content.orig.tmp4 <- str_trim(corpus.content.orig.tmp3, side = "both")
# Inspect the resulting file
#str(corpus.content.orig.tmp4)
###############################################################
# Create a table holding the file id and the file content
overview.corpus.tb2 <- (cbind(
  1:length(corpus.content.orig.tmp4),
  file.ids,
  corpus.content.orig.tmp4))
colnames(overview.corpus.tb2) <- c("id", "file", "content")
rownames(overview.corpus.tb2) <- c(1:length(corpus.content.orig.tmp4))
# Inspect the table
#head(overview.corpus.tb2)
# Rename the table
corpus.table2 <- overview.corpus.tb2
###############################################################
### --- STEP 2
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 3]
# Split corpus files so that each utterance is one element
all.files.unclean <- sapply(all.files, function(X) {
  x <- strsplit(gsub("([A-Z]{3,9}_{0,1}[0-9]{0,1}\\:)","~#~\\1", X), "~#~" )  }  )
# Inspect results
#str(all.files.unclean)
#all.files.unclean[[1]]
###############################################################
# Create a list of all.files.unclean with the first element
# in each element deleted
all.files.unclean <- lapply(all.files.unclean, "[", c(-1))
###############################################################
# Separate the speakers from the utterance
speakers.and.utts <- lapply(all.files.unclean, function(x){
  x <- gsub("\\:"," ", x)
  str_split(x, " ", n = 2)  }  )
# View results
#speakers.and.utts
#x <- sapply(speakers.and.utts[[2]], "[[", 1)
###############################################################
# Store speakers in extra vector
speakers.full <- lapply(speakers.and.utts, function(x){
  sapply(x, "[[", 1)
  }  )
# View results (elements of written files are empty)
#speakers.full
speakers <- lapply(speakers.full, function(x) {
  sapply(x, function(y) {
    z <- gsub("([A-Z]{0,9}.*)","\\1", y)
    z <- str_replace(z, "(\\:)","")
  }  )
}  )
# View results (elements of written files are empty)
#speakers
###############################################################
#Extract the number of utterances for each speaker of all spoken files
utt.count.tmp1 <- lapply(speakers, function(x) {
  as.data.frame(table(x))  }  )
#Add names (file.ids) to the number of utterances
names(utt.count.tmp1) <- file.ids
# View results
#str(utt.count.tmp1)
###############################################################
# Extract the number of speakers per file
speaker.count.tmp1 <- sapply(utt.count.tmp1, function(x) {
  sapply(x, function(y) {
    length(y)
    }  )
  }  )
speaker.count.tmp2 <- speaker.count.tmp1[c(seq(2, length(speaker.count.tmp1), 2))]
# View results
#speaker.count.tmp2
###############################################################
# Create a vector with file.ids with equals the length of
# speakers in the corpus
file.ids.tmp1 <- rep(file.ids, speaker.count.tmp2)
# View results
#file.ids.tmp1
#length(file.ids.tmp1)  # 296
###############################################################
# Create a vector with the utt.count of all speakers in the corpus
utt.count <- unlist(sapply(speakers, function(x) {table(x)}))
names(utt.count) <- sapply(names(utt.count),  function(x) {
    gsub("([0-9]{1,2}\\.)","", x)
  }  )
# View results
#utt.count
#names(utt.count)
#length(utt.count)  # 296
###############################################################
# Create a vector with the ids all speakers in the corpus
speaker.ids.tmp1 <- names(utt.count)
speaker.ids.tmp2 <- names(utt.count)
# View results
#speaker.ids.tmp2
#length(speaker.ids.tmp2)  # 356
###############################################################
# Crate a table, holding the file, speaker.id and the utterance
# count for each speaker
overview.corpus.table.tb1 <- cbind(c(1:length(file.ids.tmp1)),
  file.ids.tmp1,
  as.character(speaker.ids.tmp2),
  utt.count)
colnames(overview.corpus.table.tb1) <- c("id",
  "file.id",
  "speaker.id",
  "utterance.count")
# View results
#overview.corpus.table.tb1
###############################################################
### --- STEP
###############################################################
# Retrieve all utterences
# Store utterances in extra vector
# Extract the utterance content
corpus.content <- lapply(speakers.and.utts, function(x) {
  sapply(x, "[[", 2)  }  )
# View results (elements of written files are empty)
#corpus.content
###############################################################
# Create a list with all utterances but cleaned, i.e. without metas
corpus.content.clean <- lapply(X = corpus.content, function (X){
# SBC specific elements
X <- str_replace_all(X, "([0-9]{1,6}\\.[0-9]{1,3} [0-9]{1,6}\\.[0-9]{1,3})","")
X <- str_replace_all(X, "(\\.)","")
X <- str_replace_all(X, "(\\=)","")
X <- str_replace_all(X, "(\\?)","")
X <- str_replace_all(X, "(\\!)","")
X <- str_replace_all(X, "(,)","")
# WARNING: THEORETICAL ISSUE
X <- str_replace_all(X, "( [a-z]{1,2}\\-)","")
X <- str_replace_all(X, "( [A-Z]{0,1}[a-z]{1,2}\\-)","")
X <- str_replace_all(X, "([A-Z]{1,2}\\-)","")
X <- str_replace_all(X, "(\\[[0-9]{0,2})","")
X <- str_replace_all(X, "([X]{0,3}[0-9]{0,2}\\])","")
X <- str_replace_all(X, "(\\(N\\))","") # pause
X <- str_replace_all(X, "(\\(H\\))","") # inhalation 1
X <- str_replace_all(X, "(\\(Hx\\))","") # inhalation 2
X <- str_replace_all(X, "(\\%)","") # glottal stop
X <- str_replace_all(X, "(<[A-Z]{0,2}X)","") # start uncertain hearing
X <- str_replace_all(X, "([A-Z]{0,2}X>)","") # end uncertain hearing
X <- str_replace_all(X, "(<@)","") # start laughing quality
X <- str_replace_all(X, "(@>)","") # end laughing quality
X <- str_replace_all(X, "(<L2)","") # start L2 segment
X <- str_replace_all(X, "(L2>)","") # end L2 segment
X <- str_replace_all(X, "(<[A-Z]{1,3} )","") # start unknown segment
X <- str_replace_all(X, "( [A-Z]{1,3}>)","") # end unknown segment
X <- str_replace_all(X, "([X]{1,7} )","") # indecipherable syllable(s=)
X <- str_replace_all(X, "(\\([A-Z]{3,7}\\))","") # e.g. (THROAT)
# FINAL CLEAN UP
X <- str_replace_all(X, "([A-Z]{3,9})"," ") # non-word characters
X <- str_replace_all(X, "(--)"," ") # non-word characters
X <- str_replace_all(X, "(-)"," ") # non-word characters
X <- str_replace_all(X, "(@)","") # non-word characters
X <- str_replace_all(X, "(<)","") # non-word characters
X <- str_replace_all(X, "(>)","") # non-word characters
X <- str_replace_all(X, "(_\\/.*?\\/)","")
X <- str_replace_all(X, "(~)","")
X <- str_replace_all(X, "(\\()","")
X <- str_replace_all(X, "(\\))","")
X <- str_replace_all(X, "(_)","")
X <- str_replace_all(X, "(OR)","")
X <- str_replace_all(X, "[0-9]{1,15} ","")
X <- str_replace_all(X, "(\\$)","") # non-word characters
X <- gsub(" {2,}", " ", X)
# WARNING: THEORETICAL ISSUE
  X <- gsub(" {2,}", " ", X)
  X <- gsub(" re |'re ", "'re ", X)
  X <- gsub(" ll |'ll ", "'ll ", X)
  X <- gsub(" ve |'ve ", "'ve ", X)
  X <- gsub(" s ", "'s ", X)
  X <- gsub(" d ", "'d ", X)
  X <- gsub(" {0,1}I m ", " I'm ", X)
  X <- gsub("ouldnt ", "ouldn't ", X)
  X <- gsub(" cant ", " can't ", X)
  X <- gsub("Cant ", "Can't ", X)
  X <- gsub("Dont ", "Don't ", X)
  X <- gsub(" dont ", " don't ", X)
  X <- gsub("Didnt ", "Didn't ", X)
  X <- gsub(" didnt ", " didn't ", X)
  X <- gsub("Isnt ", "Isn't ", X)
  X <- gsub(" isnt ", " isn't ", X)
  X <- gsub("Arent ", "Aren't ", X)
  X <- gsub(" arent ", " aren't ", X)
  X <- gsub("Wasnt ", "Wasn't ", X)
  X <- gsub(" wasnt ", " wasn't ", X)
  X <- gsub("Thats ", "That's ", X)
  X <- gsub(" thats ", " that's ", X)
  X <- gsub("Theres ", "There's ", X)
  X <- gsub(" theres ", " there's ", X)
  X <- gsub("Shes ", "She's ", X)
  X <- gsub(" shes ", " she's ", X)
  X <- gsub("Theyre ", "They're ", X)
  X <- gsub(" theyre ", " they're ", X)
X <- gsub(" {2,}", " ", X)
X <- str_trim(X)
}  )
# View results
#corpus.content.clean
#corpus.content.clean[[1]]
###############################################################
###############################################################
# Determine the number of utterances per turn (which in SCAE is always 1)
utt.count.tmp1 <- lapply(corpus.content.clean, function(x) {
  sapply(x, function(y) length(y))  }  )
# View results
#utt.count.tmp1
# We now convert the utterance counts into numeric values
utt.count.tmp2 <- lapply(utt.count.tmp1, function(x) {
  sapply(x, function(y) as.numeric(y))   } )
# View results
# utt.count.tmp2
###############################################################
###############################################################
### --- Create a list which holds the number of words per turn
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(corpus.content.clean, function(x){
  x <- strsplit(x, " ")  }  )
# View results
#tokenized
# Now, we count the words elements(words) in each turn (list element)
word.count.tmp1 <- lapply(tokenized, function(x) {
  sapply(x, function(y) length(y))  } )
# View results
#word.count.tmp1
# We now convert the word counts into numeric values
word.count <- lapply(word.count.tmp1, function(x) {
  sapply(x, as.numeric)  }  )
# View results
#word.count
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, utterance, utterance.clean,
# utt.count, word.count)
###############################################################
corpus.info.tmp1 <- mapply(cbind,
  1:length(speakers[]),
  speakers[],
  SIMPLIFY = F)
names(corpus.info.tmp1) <- file.ids
corpus.info.tmp2 <- mapply(cbind,
  corpus.info.tmp1,
  file.ids,
  SIMPLIFY = F)
corpus.info.tmp3 <- mapply(cbind,
  corpus.info.tmp2,
  corpus.content,
  SIMPLIFY = F)
corpus.info.tmp4 <- mapply(cbind,
  corpus.info.tmp3,
  corpus.content.clean[],
  SIMPLIFY = F)
corpus.info.tmp5 <- mapply(cbind,
  corpus.info.tmp4,
  utt.count.tmp2[],
  SIMPLIFY = F)
corpus.info.tmp6 <- mapply(cbind,
  corpus.info.tmp5,
  word.count[],
  SIMPLIFY = F)
# Add row- and column names
AddRowNames <- function (X) {
x <- as.data.frame(X[])
rownames(x) <- c(1:nrow(x))
X <- x
}
corpus.info.tmp7 <-lapply(X = corpus.info.tmp6, FUN = AddRowNames)
# Add column names
corpus.info.tmp8 <- lapply(corpus.info.tmp7, function(x) {
  z <- as.data.frame(x[])
  colnames(z) <- c("file.no", "spk.ref", "text.id",
  "orig.content", "cleaned.content", "speech.unit.count", "word.count")
  x <- z
   }  )
# View results
#corpus.info.tmp8
#head(corpus.info.tmp8[[1]])
###############################################################
# Now, we convert the matrixes in the list into data frames
ConvertIntoNumeric <- function (X) {
x <- as.data.frame(X[])
x[,6] <- as.numeric(x[,6])
x[,7] <- as.numeric(x[,7])
X <- x
}
corpus.info.tmp9 <-lapply(X = corpus.info.tmp8, FUN = ConvertIntoNumeric)
# View results
#corpus.info.tmp9
# Test if transformation was successful
#is.numeric(corpus.info.tmp9[[1]][,7])
###############################################################
###############################################################
###############################################################
# Rename data for later kwik seraches
kwik.tb.sbc <- corpus.info.tmp9
###############################################################
###############################################################
###############################################################
###############################################################
# Extract the words counts for speakers in one file
word.count.result.tmp1 <- lapply(X = corpus.info.tmp9, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[7]], x[[2]], sum))) } )
# Simplify the results
word.count.result.tmp2 <- sapply(word.count.result.tmp1, "[[", 1)
names(word.count.result.tmp2) <- file.ids
# Unlist
word.count.result.tmp3 <- unlist(word.count.result.tmp2)
# View results
#word.count.result.tmp3
#length(word.count.result.tmp3)
###############################################################
# We now put all the information together in one table
overview.corpus.table.tb2 <- as.table(cbind(c(1:length(file.ids.tmp1)),
  as.character(file.ids.tmp1),
  as.character(speaker.ids.tmp2),
  utt.count,
  word.count.result.tmp3))
colnames(overview.corpus.table.tb2) <- c("id",
  "text.id",
  "speaker.id",
  "utterance.count",
  "word.count")
# Rename the data
speakerinfo.ice.scae2 <- overview.corpus.table.tb2
speakerinfo.ice.scae3 <- tolower(speakerinfo.ice.scae2)
speakerinfo.ice.scae4 <- matrix(speakerinfo.ice.scae3, ncol = 5)
speakerinfo.ice.scae5 <- as.data.frame(speakerinfo.ice.scae4)
colnames(speakerinfo.ice.scae5) <- c("id", "text.id", "speaker.id",
  "utterance.count", "word.count")
speakerinfo.ice.scae5[, 4] <- as.numeric(speakerinfo.ice.scae5[, 4])
speakerinfo.ice.scae5[, 5] <- as.numeric(speakerinfo.ice.scae5[, 5])
speakerinfo.ice.scae5 <- cbind(speakerinfo.ice.scae5[, 1], c(rep(1, sum(table(speakerinfo.ice.scae5[, 2])[1:15])), rep(2, sum(table(speakerinfo.ice.scae5[, 2])[16:30])), rep(3, sum(table(speakerinfo.ice.scae5[, 2])[31:45])), rep(4, sum(table(speakerinfo.ice.scae5[, 2])[46:60]))), speakerinfo.ice.scae5[, c(2:5)])
colnames(speakerinfo.ice.scae5) <- c("id", "part", "text.id", "spk.ref",
  "utterance.count", "word.count")
# View results
#head(speakerinfo.ice.scae5)
###############################################################
###############################################################
###############################################################
### --- STEP
###############################################################
###############################################################
###############################################################
# Load biodata
biodata1.tmp1 <- read.csv(bio.sbc.1, header = T)
biodata2.tmp1 <- read.csv(bio.sbc.2, header = F)
biodata3.tmp1 <- read.csv(bio.sbc.3, header = F)
biodata4.tmp1 <- read.csv(bio.sbc.4, header = F)
# Inspect data
#head(biodata1.tmp1)
#head(biodata2.tmp1)
#head(biodata3.tmp1)
#head(biodata4.tmp1)
###############################################################
# Repair broken file formats
biodata1.tmp2 <- cbind(rep(NA, length(biodata1.tmp1[, 1])), biodata1.tmp1, rep(1, length(biodata1.tmp1[, 1])))
colnames(biodata1.tmp2) <- c("speaker id", "name used in transcript", "gender", "age", "dialect of english", "dialect state", "current state", "highest level of education", "years of education", "occupation", "ethnicity", "part")
biodata2.tmp2 <- as.data.frame(matrix(as.vector(unlist(lapply(biodata2.tmp1, function(x) {  strsplit(as.character(x), ",")  }  ))), ncol = 11, byrow = T))
biodata2.tmp2 <- cbind(biodata2.tmp2, rep(2, length(biodata2.tmp2[, 1])))
colnames(biodata2.tmp2) <- colnames(biodata1.tmp2)
biodata3.tmp2 <- as.data.frame(matrix(as.vector(unlist(lapply(biodata3.tmp1, function(x) {  strsplit(as.character(x), ",")  }  ))), ncol = 11, byrow = T))
biodata3.tmp2 <- cbind(biodata3.tmp2, rep(3, length(biodata3.tmp2[, 1])))
colnames(biodata3.tmp2) <- colnames(biodata1.tmp2)
biodata4.tmp2 <- biodata4.tmp1[!grepl(".*sbc.*", biodata4.tmp1[, 1]) == T, ]
biodata4.tmp2[45] <- sub(",,", ",", biodata4.tmp2[45])
biodata4.tmp2 <- matrix(as.vector(unlist(lapply(biodata4.tmp2, function(x) {  strsplit(as.character(x), ",")  }  ))), ncol = 11, byrow = T)
biodata4.tmp2 <- cbind(biodata4.tmp2, rep(4, length(biodata4.tmp2[, 1])))
colnames(biodata4.tmp2) <- colnames(biodata1.tmp2)
# Bind data together
biodata.sbc.tmp1 <- rbind(biodata1.tmp2, biodata2.tmp2, biodata3.tmp2, biodata4.tmp2)
# Add id
biodata.sbc.tmp2 <- cbind(1: length(biodata.sbc.tmp1[, 1]), biodata.sbc.tmp1)
colnames(biodata.sbc.tmp2) <- c("id", "speaker.id", "spk.ref", "gender", "age", "dialect.of.english", "dialect.state", "current.state", "ed.level", "ed.years", "occupation", "ethnicity", "part")
biodata.sbc.tmp3 <- matrix(sapply(biodata.sbc.tmp2, function(x) {
  x <- tolower(x)
  }  ), ncol = 13)
biodata.sbc.tmp4 <- as.data.frame(biodata.sbc.tmp3)
# sex
biodata.sbc.tmp4[, 4] <- as.vector(unlist(lapply(biodata.sbc.tmp4[, 4], function(x) {
  ifelse(x == "NA", NA,
  ifelse(x == "", NA,
  ifelse(x == "n/a", NA,
  ifelse(x == "f", "female",
  ifelse(x == "m", "male", x
  )))))} )))
# dialect.state
biodata.sbc.tmp4[, 7] <- as.vector(unlist(lapply(biodata.sbc.tmp4[, 7], function(x) {
  ifelse(x == "NA", NA,
  ifelse(x == "", NA,
  ifelse(x == "n/a", NA,
  ifelse(x == "?", NA,
  ifelse(x == "al", "alabama",
  ifelse(x == "ca", "california",
  ifelse(x == "co", "colorado",
  ifelse(x == "de", "delaware",
  ifelse(x == "fl", "florida",
  ifelse(x == "ga", "geogia",
  ifelse(x == "id", "idaho",
  ifelse(x == "il", "illinois",
  ifelse(x == "in", "indiana",
  ifelse(x == "ks", "kansas",
  ifelse(x == "ky", "kentucky",
  ifelse(x == "la", "louisiana",
  ifelse(x == "ma", "massachusetts",
  ifelse(x == "ma/nm", "massachusetts/new mexico",
  ifelse(x == "me", "maine",
  ifelse(x == "mi", "michigan",
  ifelse(x == "mn", "minnesota",
  ifelse(x == "mo", "missouri",
  ifelse(x == "mt", "montana",
  ifelse(x == "nj", "new jersey",
  ifelse(x == "nm", "new mexico",
  ifelse(x == "ny", "new york",
  ifelse(x == "oh", "ohio",
  ifelse(x == "oh/ca", "california/ohio",
  ifelse(x == "or", "oregon",
  ifelse(x == "pa", "pennsylvania",
  ifelse(x == "sd", "south dakota",
  ifelse(x == "tx", "texas",
  ifelse(x == "va/ca", "california/virginia",
  ifelse(x == "va/nm", "new mexico/virginia",
  ifelse(x == "va/wa/nm/ca", "california/new mexico/virginia/washington",
  ifelse(x == "vt", "vermont",
  ifelse(x == "wa", "washington",
  ifelse(x == "wi", "wisconsin",
  x))))))))))))))))))))))))))))))))))))))  })))
# current.state
biodata.sbc.tmp4[, 8] <- as.vector(unlist(lapply(biodata.sbc.tmp4[, 8], function(x) {
  ifelse(x == "NA", NA,
  ifelse(x == "", NA,
  ifelse(x == "n/a", NA,
  ifelse(x == "?", NA,
  ifelse(x == "al", "alabama",
  ifelse(x == "az", "arizona",
  ifelse(x == "ca", "california",
  ifelse(x == "co", "colorado",
  ifelse(x == "fl", "florida",
  ifelse(x == "ga", "geogia",
  ifelse(x == "id", "idaho",
  ifelse(x == "il", "illinois",
  ifelse(x == "in", "indiana",
  ifelse(x == "ks", "kansas",
  ifelse(x == "ky", "kentucky",
  ifelse(x == "la", "louisiana",
  ifelse(x == "ma", "massachusetts",
  ifelse(x == "me", "maine",
  ifelse(x == "mi", "michigan",
  ifelse(x == "mn", "minnesota",
  ifelse(x == "mo", "missouri",
  ifelse(x == "mt", "montana",
  ifelse(x == "nj", "new jersey",
  ifelse(x == "nm", "new mexico",
  ifelse(x == "nv", "nevada",
  ifelse(x == "ny", "new york",
  ifelse(x == "oh", "ohio",
  ifelse(x == "or", "oregon",
  ifelse(x == "pa", "pennsylvania",
  ifelse(x == "sd", "south dakota",
  ifelse(x == "tx", "texas",
  ifelse(x == "vt", "vermont",
  ifelse(x == "wa", "washington",
  ifelse(x == "wi", "wisconsin",
  x))))))))))))))))))))))))))))))))))  })))
# ethnicity
biodata.sbc.tmp4[, 12] <- as.vector(unlist(lapply(biodata.sbc.tmp4[, 12], function(x) {
  ifelse(x == "", NA,
  ifelse(x == "?", NA,
  ifelse(x == "asian\036american", "asian-american",
  ifelse(x == "crow indian", "native american",
  ifelse(x == "chicana", "chicano/latino",
  ifelse(x == "latino/chicano", "chicano/latino",
  ifelse(x == "japanese/", "asian-american",
  ifelse(x == "black", "african-american",
  ifelse(x == "black/african-american", "african-american",
  ifelse(x == "japanese/", "asian-american",
  ifelse(x == "chicano", "chicano/latino",
  ifelse(x == "hispanic", "chicano/latino",
x))))))))))))  })))
colnames(biodata.sbc.tmp4) <- colnames(biodata.sbc.tmp2)
# education level
biodata.sbc.tmp4[, 9] <- as.vector(unlist(sapply(biodata.sbc.tmp4[, 9], function(x) {
  ifelse(x == "10th grade", "high school or lower",
  ifelse(x == "7th grade", "high school or lower",
  ifelse(x == "2nd year college", "college or higher",
  ifelse(x == "3 yrs college", "college or higher",
  ifelse(x == "3.5 yrs college", "college or higher",
  ifelse(x == "4 years", "college or higher",
  ifelse(x == "5 yrs", "college or higher",
  ifelse(x == "b.a.", "college or higher",
  ifelse(x == "b.a", "college or higher",
  ifelse(x == "b.a./b.s.", "college or higher",
  ifelse(x == "b.b.a.", "college or higher",
  ifelse(x == "b.a./m.s./m.p.a.", "college or higher",
  ifelse(x == "b.s.", "college or higher",
  ifelse(x == "ba", "college or higher",
  ifelse(x == "m.a", "college or higher",
  ifelse(x == "masters", "college or higher",
  ifelse(x == "junior in college", "college or higher",
  ifelse(x == "ma in progress", "college or higher",
  ifelse(x == "ms.ed.", "college or higher",
  ifelse(x == "ms", "college or higher",
  ifelse(x == "post m.a.", "college or higher",
  ifelse(x == "m.f.a.", "college or higher",
  ifelse(x == "post-grad", "college or higher",
  ifelse(x == "post-graduate", "college or higher",
  ifelse(x == "post-grad medicine", "college or higher",
  ifelse(x == "m.s.", "college or higher",
  ifelse(x == "ph.d.", "doctoral degree",
  ifelse(x == "ph.d. ", "doctoral degree",
  ifelse(x == "phd m", "doctoral degree",
  ifelse(x == "high school", "high school or lower",
  ifelse(x == "high school & chef school", "high school or lower",
  ifelse(x == "ba accounting", "college or higher",
  ifelse(x == "ba j", "college or higher",
  ifelse(x == "bs cr", "college or higher",
  ifelse(x == "bs", "college or higher",
  ifelse(x == "ba ma", "college or higher",
  ifelse(x == "ba/bs/college", "college or higher",
  ifelse(x == "college", "college or higher",
  ifelse(x == "community college", "college or higher",
  ifelse(x == "business school", "college or higher",
  ifelse(x == "doctor of vet. med.", "doctoral degree",
  ifelse(x == "NA", NA, x
  ))))))))))))))))))))))))))))))))))))))))))
} )))
biodata.sbc.tmp4[, 9] <- as.vector(unlist(sapply(biodata.sbc.tmp4[, 9], function(x) {
  ifelse(x == "m.d.", "doctoral degree",
  ifelse(x == "jd.", "doctoral degree",
  ifelse(x == "grade school", "high school or lower",
  ifelse(x == "high school freshman", "high school or lower",
  ifelse(x == "some high school", "high school or lower",
  ifelse(x == "senior in college", "college or higher",
  ifelse(x == "some college", "college or higher",
  ifelse(x == "massachusetts", "college or higher",
  ifelse(x == "m.a.", "college or higher",
  ifelse(x == "some co", "college or higher",
  ifelse(x == "some community college", "college or higher",
  ifelse(x == "a.a.", "college or higher",
  ifelse(x == "a.a", "college or higher",
  ifelse(x == "medical school in progress", "college or higher",
  ifelse(x == "llb", "college or higher",
  ifelse(x == "stm", "college or higher",
  ifelse(x == "professional", "college or higher",
  ifelse(x == "phd", "doctoral degree",
  ifelse(x == "elementary", "high school or lower",
  ifelse(x == "certified", NA,
  ifelse(x == "dvm", NA,
  ifelse(x == "nursing", NA,
  ifelse(x == "high school/hon.doc.", NA,
  ifelse(x == "NA", NA, x
  ))))))))))))))))))))))))
} )))
# age
biodata.sbc.tmp5 <- cbind(biodata.sbc.tmp4, biodata.sbc.tmp4[, 5])
biodata.sbc.tmp5[, 14] <- as.vector(unlist(lapply(as.numeric(biodata.sbc.tmp5[, 14]), function(x) {
  ifelse(x <= 18, "0-18",
  ifelse(x <= 25, "19-25",
  ifelse(x <= 33, "26-33",
  ifelse(x <= 41, "34-41",
  ifelse(x <= 49, "42-49", "50+"
  )))))
} )))
# Convert into data frame
biodata.sbc.tmp5 <- as.data.frame(biodata.sbc.tmp5)
# Inspect data
#head(biodata.sbc.tmp5)
#table(biodata.sbc.tmp5[, 9])
#head(speakerinfo.ice.scae5)
###############################################################
### --- CHECKING DATA
###############################################################
# check data set
test <- table(biodata.sbc.tmp5[, 3], biodata.sbc.tmp5[, 13])
check1 <- which(test[, 1] >1)
check2 <- which(test[, 2] >1)
check3 <- which(test[, 3] >1)
check4 <- which(test[, 4] >1)
spk.refs <- c(names(check1), names(check2), names(check3), names(check4))
doubles <- rbind(biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[1], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[2], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[3], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[4], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[5], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[6], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[7], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[8], ],
biodata.sbc.tmp5[biodata.sbc.tmp5[, 3] == spk.refs[9], ])
# Inspect data
#doubles
# Remove doubles from data set since tehse doubles cannot
# be unambiguously assigned
biodata.sbc.part1 <- subset(biodata.sbc.tmp5, biodata.sbc.tmp5[, 13] == 1)
biodata.sbc.part2 <- subset(biodata.sbc.tmp5, biodata.sbc.tmp5[, 13] == 2)
biodata.sbc.part3 <- subset(biodata.sbc.tmp5, biodata.sbc.tmp5[, 13] == 3)
biodata.sbc.part4 <- subset(biodata.sbc.tmp5, biodata.sbc.tmp5[, 13] == 4)
biodata.sbc.part1 <- subset(biodata.sbc.part1, biodata.sbc.part1 [, 3] != "carolyn")
biodata.sbc.part1 <- subset(biodata.sbc.part1, biodata.sbc.part1 [, 3] != "doris")
biodata.sbc.part1 <- subset(biodata.sbc.part1, biodata.sbc.part1 [, 3] != "kathy")
biodata.sbc.part2 <- subset(biodata.sbc.part2, biodata.sbc.part2 [, 3] != "dan")
biodata.sbc.part2 <- subset(biodata.sbc.part2, biodata.sbc.part2 [, 3] != "jeff")
biodata.sbc.part3 <- subset(biodata.sbc.part3, biodata.sbc.part3 [, 3] != "alice")
biodata.sbc.part3 <- subset(biodata.sbc.part3, biodata.sbc.part3 [, 3] != "don")
biodata.sbc.part4 <- subset(biodata.sbc.part4, biodata.sbc.part4 [, 3] != "fred")
biodata.sbc.part4 <- subset(biodata.sbc.part4, biodata.sbc.part4 [, 3] != "karen")
biodata.sbc.tmp6 <- rbind(biodata.sbc.part1, biodata.sbc.part2, biodata.sbc.part3, biodata.sbc.part4)
# Clean data set
biodata.sbc.tmp6 <- as.data.frame(matrix(as.vector(unlist(lapply(biodata.sbc.tmp6, function(x) {
  ifelse(x == "", NA,
  ifelse(x == " ", NA,
  ifelse(x == "n/a", NA, x)
  ))  }  ))), ncol = 14))
colnames(biodata.sbc.tmp6) <- c(colnames(biodata.sbc.tmp2), "age.group")
# Inspect results
#head(biodata.sbc.tmp6)
###############################################################
# Join data sets (bioinfo and wordcounts)
biodata.sbcae.tmp1 <- join(speakerinfo.ice.scae5, biodata.sbc.tmp6, by = c("part", "spk.ref"), type = "left")
# Inspect results
#head(biodata.sbcae.tmp1)
# Delete superfluous columns
biodata.sbcae.tmp2 <- biodata.sbcae.tmp1[, -7]
# Inspect results
#head(biodata.sbcae.tmp2)
# Rename data set
biodata.sbcae <- biodata.sbcae.tmp2[, c(1:4, 7:9, 17, 10:16, 5:6)]
colnames(biodata.sbcae)[7] <- "age.exact"
colnames(biodata.sbcae)[8] <- "age"
colnames(biodata.sbcae)[16] <- "speech.unit.count"
# Inspect resulting table
#head(biodata.sbcae)
###############################################################
### ---                    THE END
###############################################################
###############################################################
### --- Important objects
# ICE Canada
#kwic.tb.ice.can
#head(kwic.tb.ice.can)
#biodata.ice.canada
#head(biodata.ice.canada)
# ICE GB R2
#kwic.tb.ice.gb
#head(kwic.tb.ice.gb)
#biodata.ice.gbr2
#head(biodata.ice.gbr2)
# ICE India
#kwic.tb.ice.ind
#head(kwic.tb.ice.ind)
#biodata.ice.ind
#head(biodata.ice.ind)
# ICE Ireland 1.2.2
#kwic.tb.ice.ire
#head(kwic.tb.ice.ire)
#biodata.ice.ire.1.2.2
#head(biodata.ice.ire.1.2.2)
# ICE Jamaica
#kwic.tb.ice.jam
#head(kwic.tb.ice.jam)
#biodata.ice.jam
#head(biodata.ice.jam)
# ICE New Zealand
#kwic.tb.ice.nz
#head(kwic.tb.ice.nz)
#biodata.ice.nz
#head(biodata.ice.nz)
# ICE Philippines
#kwic.tb.ice.phi
#head(kwic.tb.ice.phi)
#biodata.ice.phi
#head(biodata.ice.phi)
# SBC
#kwik.tb.sbc
#head(kwik.tb.sbc)
#biodata.sbcae
#head(biodata.sbcae)
###############################################################
# Rename objects
bio.ice.can <- as.data.frame(cbind(rep("ice.can", length(biodata.ice.canada[, 1])), biodata.ice.canada))
bio.ice.gb <- as.data.frame(cbind(rep("ice.gb", length(biodata.ice.gbr2[, 1])), biodata.ice.gbr2))
bio.ice.ind <- as.data.frame(cbind(rep("ice.ind", length(biodata.ice.ind[, 1])), biodata.ice.ind))
bio.ice.ire <- as.data.frame(cbind(rep("ice.ire", length(biodata.ice.ire.1.2.2[, 1])), biodata.ice.ire.1.2.2))
bio.ice.jam <- as.data.frame(cbind(rep("ice.jam", length(biodata.ice.jam[, 1])), biodata.ice.jam))
bio.ice.nz <- as.data.frame(cbind(rep("ice.nz", length(biodata.ice.nz[, 1])), biodata.ice.nz))
bio.ice.phi <- as.data.frame(cbind(rep("ice.phi", length(biodata.ice.phi[, 1])), biodata.ice.phi))
bio.sbc <- as.data.frame(cbind(rep("sbc", length(biodata.sbcae[, 1])), rep("1", length(biodata.sbcae[, 1])), biodata.sbcae))
# Add column name
colnames(bio.ice.can)[c(1, 7, 12, 28)] <- c("corpus", "subfile", "sex", "ed.lev")
colnames(bio.ice.gb)[c(1, 5, 6)] <- c("corpus", "text.id", "subfile")
colnames(bio.ice.ind)[c(1, 5, 7, 21)] <- c("corpus", "subfile", "date", "sex")
colnames(bio.ice.ire)[c(1, 19)] <- c("corpus", "mother.tongue")
colnames(bio.ice.jam)[c(1, 5, 10, 18)] <- c("corpus", "subfile", "ed.lev", "mother.tongue")
colnames(bio.ice.nz)[c(1, 6)] <- c("corpus", "subfile")
colnames(bio.ice.phi)[c(1, 6, 10, 15, 20)] <- c("corpus", "subfile", "date", "sex", "ed.lev")
colnames(bio.sbc)[c(1, 2, 8, 14)] <- c("corpus", "subfile", "sex", "ed.lev")
###############################################################
# Combine dat sets
can.gb <- join(bio.ice.can, bio.ice.gb, by = c("corpus"), type = "full")
can.gb.ind <- join(can.gb, bio.ice.ind, by = c("corpus"), type = "full")
can.gb.ind.ire <- join(can.gb.ind, bio.ice.ire, by = c("corpus"), type = "full")
can.gb.ind.ire.jam <- join(can.gb.ind.ire, bio.ice.jam, by = c("corpus"), type = "full")
can.gb.ind.ire.jam.nz <- join(can.gb.ind.ire.jam, bio.ice.nz, by = c("corpus"), type = "full")
can.gb.ind.ire.jam.nz.phi <- join(can.gb.ind.ire.jam.nz, bio.ice.phi, by = c("corpus"), type = "full")
can.gb.ind.ire.jam.nz.phi.sbc <- join(can.gb.ind.ire.jam.nz.phi, bio.sbc, by = c("corpus"), type = "full")
bio.phd <- can.gb.ind.ire.jam.nz.phi.sbc
bio.phd <- bio.phd[order(bio.phd$corpus, bio.phd$text.id, bio.phd$spk.ref), ]
bio.phd.new <- cbind(1:length(bio.phd$corpus), bio.phd$corpus, bio.phd$text.id, bio.phd$subfile, bio.phd$spk.ref, bio.phd$sex,
  bio.phd$age, bio.phd$age.exact, bio.phd$date, bio.phd$ed.lev, bio.phd$ethnicity,
  bio.phd$mother.tongue, bio.phd$word.count)
bio.phd <- as.data.frame(bio.phd.new)
colnames(bio.phd) <- c("id", "corpus", "text.id", "subfile", "spk.ref", "sex", "age", "age.exact", "date", "ed.lev", "ethnicity", "mother.tongue", "word.count")
bio.phd$s1a <- as.vector(unlist(sapply(bio.phd$text.id, function(x) {
  x <- toupper(x)
  x <- gsub("-*", "", x)
  x <- gsub("[0-9]{3,3}", "", x)
  ifelse(x == "S1A", 1,
  ifelse(x == "SBC", 1, 0))} )))
bio.phd$date <- as.vector(unlist(sapply(bio.phd$date, function(x) {
  x <- gsub("[0-9]{2,2}-[0-9]{2,2}-", "", x)
  x <- gsub("ca-[0-9]{2,2}-", "", x)
  x <- gsub("12-2000/01-", "", x)
  x <- gsub(".*c", "", x)
  x <- gsub(".* ", "", x)
  ifelse(x == "03", NA,
  ifelse(x == 201, NA, x))})))
bio.phd <- bio.phd[bio.phd$s1a == 1, ]
bio.phd.lower <- apply(bio.phd, 2, tolower)
bio.phd <- as.data.frame(bio.phd.lower)
bio.phd$ethnicity <- as.vector(unlist(sapply(bio.phd$ethnicity, function(x) {
  ifelse(x == "white person, northern european type & native canadian", NA,
  ifelse(x == "white person, northern european type & mediterranean european/hispanic", NA, 
  ifelse(x == "white person, northern european type & chinese, japanese, or south-east asian person", NA,
  ifelse(x == "white person, northern european type", "white",
  ifelse(x == "tokelauan", NA,
  ifelse(x == "samoan", NA,
  ifelse(x == "pakeha/samoan", NA,
  ifelse(x == "pakeha/maori", NA,
  ifelse(x == "pakeha/asian", NA,
  ifelse(x == "native american/white", NA,
  ifelse(x == "native american", NA,
  ifelse(x == "mediterranean european/hispanic", NA,
  ifelse(x == "maori/pakeha", NA,
  ifelse(x == "lebanese", NA,
  ifelse(x == "cook island/pakeha", "pakeha",
  ifelse(x == "cook island maori", "maori",
  ifelse(x == "chinese, japanese, or south-east asian person", NA,
  ifelse(x == "african-american", NA,
  ifelse(x == "chinese", NA,
  ifelse(x == "chicano/white", NA,
  ifelse(x == "asian-american", NA, x)))))))))))))))))))))} )))
bio.phd$mother.tongue <- as.vector(unlist(sapply(bio.phd$mother.tongue, function(x) {
  ifelse(x == "urdu", NA,
  ifelse(x == "tagalog/english", NA, 
  ifelse(x == "tagalog & spanish", NA,
  ifelse(x == "spanish", NA,
  ifelse(x == "konkani", NA,
  ifelse(x == "kankanaey", NA,
  ifelse(x == "itawes", NA,
  ifelse(x == "italian", NA,
  ifelse(x == "ilokano", NA,
  ifelse(x == "ibanag", NA,
  ifelse(x == "hiligaynon", NA,
  ifelse(x == "german", NA,
  ifelse(x == "french (canadian)", NA,
  ifelse(x == "fookien/tagalog", NA,
  ifelse(x == "english/tagalog", NA,
  ifelse(x == "english/jamaican", NA,
  ifelse(x == "cebuano & chavacano", NA,
  ifelse(x == "cebuano", NA,
  ifelse(x == "bengali", NA,
  ifelse(x == "aklanon", NA, x))))))))))))))))))))} )))
bio.phd$word.count <- as.numeric(bio.phd$word.count)
bio.phd <- bio.phd[bio.phd$word.count >= 100, ]
head(bio.phd)
table(bio.phd$corpus)
table(bio.phd$age)
table(bio.phd$sex)
table(bio.phd$date)
table(bio.phd$ethnicity)
table(bio.phd$mother.tongue)
table(bio.phd$ed.lev)
###############################################################
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.bio.phd, showWarnings = F)
# Store the txt file in the output file
write.table(bio.phd, out.bio.phd, sep = "\t", row.names = F)
###############################################################
# Remove all lists from the current workspace
#rm(list=ls(all=T))
###############################################################