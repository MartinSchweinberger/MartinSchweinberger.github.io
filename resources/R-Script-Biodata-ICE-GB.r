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
corpus.gb <- "C:\\PhD\\skripts n data\\corpora\\ICE GB2\\ice-gb-2\\data"
# Define input pathname of raw biodata
bio.gb <- "C:\\PhD\\skripts n data\\corpora\\ICE GB2\\ice-gb-2\\text/sspeaker.txt"
# Define outputpath of final biodata
out.gb <- "C:\\MeineHomepage\\docs\\data/BiodataIceGbR2.txt"
###############################################################
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
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
###############################################################
### --- Important objects
# ICE GB R2
#kwic.tb.ice.gb
#head(kwic.tb.ice.gb)
#biodata.ice.gbr2
#head(biodata.ice.gbr2)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.gb, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.gbr2, out.gb, sep = "\t", row.names = F)
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
###############################################################
###############################################################