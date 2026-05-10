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
# Define input pathname of raw biodata
bio.can <- "C:\\PhD\\skripts n data\\corpora\\ICE CAN\\Headers/Spoken ICE-CAN metadata.txt"
# Define outputpath of final biodata
out.can <- "C:\\MeineHomepage\\docs\\data/BiodataIceCanada.txt"
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
# fix broken file (S1A-006 does not contain start and end tag)
corpus.tmp[6][[1]] <- c("<I>", corpus.tmp[6][[1]], "</I>")
# unlist corpus
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
###############################################################
### --- Important objects
# ICE Canada
#kwic.tb.ice.can
#head(kwic.tb.ice.can)
#biodata.ice.canada
#head(biodata.ice.canada)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.can, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.canada, out.can, sep = "\t", row.names = F)
###############################################################
###############################################################
###############################################################
###############################################################
### ---                    THE END
###############################################################
###############################################################
###############################################################
# Remove all lists from the current workspace
#rm(list=ls(all=T))
###############################################################