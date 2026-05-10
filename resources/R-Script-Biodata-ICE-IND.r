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
corpus.ind <- "C:\\PhD\\skripts n data\\corpora\\ICE India\\Corpus"
# Define input pathname of raw biodata
bio.ind <- "C:\\PhD\\skripts n data\\corpora\\ICE India\\Headers"
# Define outputpath of final biodata
out.ind <- "C:\\MeineHomepage\\docs\\data/BiodataIceIndia.txt"
###############################################################
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
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
###############################################################
###############################################################
# ICE India
#kwic.tb.ice.ind
#head(kwic.tb.ice.ind)
#biodata.ice.ind
#head(biodata.ice.ind)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.ind, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.ind, out.ind, sep = "\t", row.names = F)
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
