##################################################################
### --- R-Skript "Combining biodata and word counts for speakers
### --- represented in the Santa Barbara Corpus with R"
### --- Author: Martin Schweinberger (Dec 18th, 2013)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R retrieves number of words for each
### --- speaker in the ICE corpus and
### --- merges the word counts with the biodata of the speakers
### --- provided by the compilers of the components of the ICE.
### --- NOTE
### --- Speakers who do not occur in the corpus but are included
### --- in bioinfo provided by the SBC team are left out of the final
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
### --- counts for speakers represented in the Santa Barbara Corpus
### ---  with R ", unpublished R script, Hamburg University.
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
corpus.sbc <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\corpusdata\\TRN"
# Define input pathname of raw biodata
bio.sbc.1 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata1.csv"
bio.sbc.2 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata2.csv"
bio.sbc.3 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata3.csv"
bio.sbc.4 <- "C:\\PhD\\skripts n data\\corpora\\SBCAE\\metadata/metadata4.csv"
# Define outputpath of final biodata
out.sbc <- "C:\\MeineHomepage\\docs\\data/BiodataSbcae.txt"
###############################################################
###############################################################
###############################################################
###                   Santa Barbara Corpus
###############################################################
###                   START
###############################################################
###############################################################
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "", encoding = "UTF-8")  }  )
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
###############################################################
###############################################################
### --- Important objects
###############################################################
# SBC
#kwik.tb.sbc
#head(kwik.tb.sbc)
#biodata.sbcae
#head(biodata.sbcae)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.sbc, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.sbcae, out.sbc, sep = "\t", row.names = F)
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