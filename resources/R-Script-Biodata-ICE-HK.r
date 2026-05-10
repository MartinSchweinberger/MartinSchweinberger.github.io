##################################################################
### --- R-Skript "Retrieving word and speech unit counts for speakers
### --- represented in the spoken component of ICE Hong Kong with R"
### --- Author: Martin Schweinberger (Dec 18th, 2013)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### ---
### --- This script was written by Martin Schweinberger
### --- (<http://www.martinschweinberger.de/blog/>).
### --- It extracts word and speech unit count from the spoken part
### --- of the Hong Kong component of the International Corpus of English
### --- (ICE HK)(<http://ice-corpora.net/ice/>).
### --- In order for this skript to work you need to have access to
### --- ICE HK.
### --- This script is made available under the GNU General Public License
### --- <http://www.gnu.org/licenses/gpl.html>.
### ---
### --- NOTE
### --- Words uttered by extra corpus speakers (annotation:
### --- start = <X>; end = </X>) are not considered in the word counts.
### ---
### --- Abbreviated forms ('ve, 're, 's etc.) are not considered full
### --- words but are regarded as part of the words to which they
### --- are attached, e.g. that's = 1 token/word; I've = 1 token/word)
### ---
### --- Each conversation is treated individually, i.e. for a file which
### --- contains several conversations among distinct or even
### --- the same speakers, the word counts for each conversation
### --- will be extracted separately from the other conversations
### --- in that file.
### ---
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### ---
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2013. "Retrieving word and speech
### --- unit counts for speakers represented in ICE Hong Kong with R",
### --- unpublished R-skript, Hamburg University.
### --- THANK YOU. Copyright Martin Schweinberger (2014).
##################################################################
###############################################################
###                   START
###############################################################
# Remove all lists from the current workspace
rm(list=ls(all=T))
# Load packages
library(tm)
library(stringr)
library(gsubfn)
library(plyr)
library(reshape)
###############################################################
# Setting options
options(stringsAsFactors = F)
# WARNING: To use this script you need to set our own paths!
# Your path should be the path to the corpus on your own computer!
# Remember to use double backslash instead of single backslash, if
# you use Windows on your maschine. The outputhpath should be the
# location where you would like to store the final data set.
# Specify pathname (corpus)
pathname = "C:\\PhD\\skripts n data\\corpora\\ICE Hong Kong"
# Define outputpath
outputpath <- "C:\\MeineHomepage\\docs\\data//BiodataIceHongKong.txt"
##############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = pathname, pattern = NULL, all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# Load and store corpus
corpus.tmp <- lapply(corpus.files, function(x) {
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
###############################################################
# Extract the file ids
file.ids <- lapply(corpus.files, function(x) {
  x <- gsub(".*(([a-z]|[A-Z])[0-9]([a-z]|[A-Z])-[0-9][0-9][0-9]).*", "\\1", x, perl = T)
  }  )
###############################################################
# Collapse the file content
corpus.tmp1 <- lapply(corpus.tmp, function(x) {
  x <- paste(x, collapse = " ")
  }  )
# Add names to the list elements
names(corpus.tmp1) <- toupper(file.ids)
###############################################################
# Merge the file.ids with the file content
corpus.tmp2 <- apply(cbind(file.ids, corpus.tmp1), 1, function(x) unname(x))
###############################################################
# Specify searchpattern
splitpattern1 = "<I>"
# Split corpus
corpus.tmp3 <- lapply(corpus.tmp2,function(x) {
  strsplit(as.character(x), splitpattern1)  }  )
# Remove all written files
corpus.tmp3 <- corpus.tmp3[1:300]
file.ids <- file.ids[1:300]
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
file.ids.spoken.tmp1 <- file.ids
###############################################################
# Create a vector holding the subfile content of the corpus
subfile.contents.tmp1 <- sapply(corpus.tmp3, "[[", 2)
# Create a list of subfile.contents1 with the first element
# in each element deleted
subfile.contents.tmp2 <- sapply(subfile.contents.tmp1, "[", c(-1))
# Create a vector of subfile.contents2
subfile.contents.tmp4 <- unlist(subfile.contents.tmp2)
###############################################################
# EXCURSION
# We will now modify the names: the file and the subfile will be separated
# Extract the file.ids
file.id.tmp1 <- sapply(names(subfile.contents.tmp4), function(x) {
  strsplit(gsub("(([a-z]|[A-Z])[0-9]([a-z]|[A-Z])-[0-9][0-9][0-9])", "\\1~", x), "~" )
  }  )
file.id.tmp2 <- lapply(file.id.tmp1, "[[", 1)
# Extract the subfiles
subfile.id.tmp1 <- gsub("([a-z]|[A-Z])[0-9]([a-z]|[A-Z])-[0-9][0-9][0-9]", "", names(subfile.contents.tmp4))
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
  1:length(file.id.tmp2),
  file.id.tmp2,
  subfile.id.tmp2,
  subfile.corpus.tmp4))
colnames(overview.subfile.corpus.tb2) <- c("id", "file", "subfile", "content")
# Repair broken elements
overview.subfile.corpus.tb2[185, 4] <- gsub("<#", "<ICE-HK:S1B-060#", overview.subfile.corpus.tb2[185, 4])
overview.subfile.corpus.tb2[186, 4] <- gsub("<#", "<ICE-HK:S1B-060#", overview.subfile.corpus.tb2[185, 4])
# Rename the table
corpus.table2 <- overview.subfile.corpus.tb2
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  x <- strsplit( gsub("(<ICE-HK:([A-Z]|[a-z]){0,1}[0-9]([A-Z]|[a-z])-[0-9]{3,3}#[A-Z]{0,1}[0-9]{1,4}:[0-9]{1,4}:[A-Z]{0,1}\\?{0,1}>)","~\\1", x), "~" )  }  )
# Create a list of all.files.unclean with the first element
# in each element deleted
all.files.unclean <- sapply(all.files.unclean, "[", c(-1))
###############################################################
# Separate the speakers from the speech.unit
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  x <- str_split(x, " ", n = 2)  }  )
###############################################################
# Store speakers in extra vector
speakers.full <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
speakers <- lapply(speakers.full, function(x) {
  x <- gsub("(.*:)","", x)
  x <- gsub("(>)","", x)
  ifelse(x == "", "?", x)  }  )
###############################################################
#Extract the number of speech.units for each speaker of all spoken files
speech.unit.count.tmp1 <- lapply(speakers, function(x) {
  x <- table(x)  }  )
###############################################################
# Extract the number of speakers per file
speaker.count.tmp2 <- sapply(speech.unit.count.tmp1, function(x) {
  x <- length(x)
  }  )
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
speaker.ids.tmp2 <- as.vector(unlist(lapply(speaker.ids.tmp1, function(x) {
  x <- gsub("(.*\\.)","", x)
}  )))
###############################################################
# Crate a table, holding the file, speaker.id and the speech.unit
# count for each speaker
overview.corpus.table.tb1 <- cbind(c(1:length(file.ids.tmp1)),
  file.ids.tmp1,
  subfile.ids.tmp1,
  as.character(speaker.ids.tmp2),
  speech.unit.count)
colnames(overview.corpus.table.tb1) <- c("id",
  "file.id",
  "subfile.id",
  "speaker.id",
  "speech.unit.count")
###############################################################
### --- STEP 3
###############################################################
# Retrieve all speech.units
# Store speech.units in extra vector
corpus.content <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 2)  }  )
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
corpus.content.clean <- lapply(corpus.content, function (x){
# START CLEAN UP
  x <- gsub(" {2,}", " ", x)
# WARNING: THEORETICAL ISSUE
  x <- str_replace_all(x, "(.*</X>)","")
# Deleting ICE GB Mark-up
  x <- str_replace_all(x, "(<O>.*?</O>)"," ")
#  x <- str_replace_all(x, "(<O>.*?<O>)"," ")
#  x <- str_replace_all(x, "(</O>.*?</O>)"," ")
  x <- str_replace_all(x, "(<quote>.*?</quote>)", "")
  x <- str_replace_all(x, "(<unclear-{0,1}[a-z]{4,5}>.*?/unclear-{0,1}[a-z]{4,5}>)", "")
  x <- str_replace_all(x, "(<unclear.*?/unclear.*?>)", "")
  x <- str_replace_all(x, "(<&>.*?</&>)", "")
  x <- str_replace_all(x, "(<.*?>)", "")
  x <- str_replace_all(x, "(\\(.*?\\))", "")
  x <- str_replace_all(x, "([A-Z]{1,8},[A-Z]{1,8})", "")
#  x <- str_replace_all(x, "(\\([a-z]{3,10},[a-z]{3,10}\\))", "")
  x <- str_replace_all(x, "([0-9]{1,3}>)", "")
  x <- str_replace_all(x, "(\\$[A-Z])", "")
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
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)
  }  )
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(corpus.content.clean, function(x) {
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
  speech.unit.count,
  word.count,
  SIMPLIFY = F)
names(corpus.info.tmp1) <- file.id.tmp2
# Add row. and column names
corpus.info.tmp2 <- lapply(corpus.info.tmp1[], function(x) {
  z <- as.data.frame(x)
  colnames(z) <- c("id", "full.speaker.id", "text.id", "subfile.id", "spk.ref", "orig.content", "final.content", "speech.unit.count", "word.count")
  x <- z  }  )
###############################################################
# Now, we convert the matrixes in the list into data frames
corpus.info.tmp12 <-lapply(corpus.info.tmp2, function(x) {
  z <- as.data.frame(x)
  z[, 8] <- as.numeric(z[, 8])
  z[, 9] <- as.numeric(z[, 9])
  x <- z  }  )
###############################################################
###############################################################
###############################################################
# Rename data for later kwik seraches
kwik.tb.ice.hk <- corpus.info.tmp12
###############################################################
###############################################################
###############################################################
# Extract the words counts for speakers in one file
word.count.result.tmp1 <- lapply(corpus.info.tmp12, function(x) {
  x <- as.data.frame(tapply(x[[9]], x[[5]], sum)) } )
# Simplify the results
word.count.result.tmp2 <- sapply(word.count.result.tmp1, "[[", 1)
names(word.count.result.tmp2) <- file.ids.spoken.tmp1
# Unlist
word.count.result.tmp3 <- unlist(word.count.result.tmp2)
###############################################################
# Extract the speech.units counts for speakers in one file
speech.unit.count.result.tmp1 <- lapply(corpus.info.tmp12, function(x) {
  x <- as.data.frame(tapply(x[[8]], x[[5]], sum)) } )
# Simplify the results
speech.unit.count.result.tmp2 <- sapply(speech.unit.count.result.tmp1, "[[", 1)
names(speech.unit.count.result.tmp2) <- file.ids.spoken.tmp1
# Unlist
speech.unit.count.result.tmp3 <- unlist(speech.unit.count.result.tmp2)
###############################################################
# We now put all the information together in one table
overview.corpus.table.tb2 <- as.table(cbind(1:length(file.ids.tmp1),
  file.ids.tmp1,
  subfile.ids.tmp1,
  as.vector(unlist((speaker.ids.tmp2))),
  speech.unit.count.result.tmp3,
  word.count.result.tmp3))
# Rename the data
speakerinfo.ice.hk.tmp1 <- overview.corpus.table.tb2
rownames(speakerinfo.ice.hk.tmp1) <- 1:length(speakerinfo.ice.hk.tmp1[, 1])
speakerinfo.ice.hk.tmp2 <- matrix(as.vector(unlist(speakerinfo.ice.hk.tmp1)), ncol = 6)
speakerinfo.ice.hk <- as.data.frame(speakerinfo.ice.hk.tmp2)
speakerinfo.ice.hk[, 5] <- as.numeric(speakerinfo.ice.hk[, 5])
speakerinfo.ice.hk[, 6] <- as.numeric(speakerinfo.ice.hk[, 6])
colnames(speakerinfo.ice.hk) <- c("id", "text.id", "subfile.id", "spk.ref", "speech.unit.count", "word.count")
# Rename the data set
biodata.ice.hk <- speakerinfo.ice.hk
###############################################################
###############################################################
###############################################################
### --- Important objects
#kwik.tb.ice.gb
#head(kwik.tb.ice.gb)
#biodata.ice.hk
#head(biodata.ice.hk)
###############################################################
###############################################################
###############################################################
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(outputpath, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.hk, outputpath, sep = "\t", row.names = F)
###############################################################
# Remove all lists from the current workspace
#rm(list=ls(all=T))
###############################################################
###############################################################
###############################################################
### ---                    THE END
###############################################################
###############################################################
###############################################################
