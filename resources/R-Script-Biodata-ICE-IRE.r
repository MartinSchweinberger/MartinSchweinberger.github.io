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
corpus.ire <- "C:\\PhD\\skripts n data\\corpora\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
# Define input pathname of raw biodata
bio.ire.1 <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Ireland biodata raw/Census spoken N complete HANDBOOK.txt" # (southern data)
bio.ire.2 <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Ireland biodata raw/Census spoken S complete HANDBOOK.txt" # (northern data)
# Define outputpath of final biodata
out.ire <- "C:\\MeineHomepage\\docs\\data/BiodataIceIreland.txt"
###############################################################
###############################################################
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
###############################################################
###############################################################
### --- Important objects
# ICE Ireland 1.2.2
#kwic.tb.ice.ire
#head(kwic.tb.ice.ire)
#biodata.ice.ire.1.2.2
#head(biodata.ice.ire.1.2.2)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.ire, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.ire.1.2.2, out.ire, sep = "\t", row.names = F)
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