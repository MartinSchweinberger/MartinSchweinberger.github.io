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
corpus.nz <- "C:\\PhD\\skripts n data\\corpora\\ICE New Zealand\\Spoken"
# Define input pathname of raw biodata
bio.nz <- "C:\\PhD\\skripts n data\\corpora\\ICE New Zealand/NZGUIDE.txt"
# Define outputpath of final biodata
out.nz <- "C:\\MeineHomepage\\docs\\data/BiodataIceNewZealand.txt"
###############################################################
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
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
###############################################################
### --- Important objects
# ICE New Zealand
#kwic.tb.ice.nz
#head(kwic.tb.ice.nz)
#biodata.ice.nz
#head(biodata.ice.nz)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.nz, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.nz, out.nz, sep = "\t", row.names = F)
###############################################################
###############################################################
###############################################################
### ---                    THE END
# Remove all lists from the current workspace
#rm(list=ls(all=T))
###############################################################
###############################################################
###############################################################
