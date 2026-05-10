##################################################################
### --- R-Skript "Retrieving word and speech unit counts for speakers
### --- represented in the spoken component of ICE Singapore with R"
### --- Author: Martin Schweinberger (Dec 18th, 2013)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### ---
### --- This script was written by Martin Schweinberger
### --- (<http://www.martinschweinberger.de/blog/>).
### --- It extracts word and speech unit count from the spoken part
### --- of the Singapore component of the International Corpus of English
### --- (ICE Singapore)(<http://ice-corpora.net/ice/>).
### --- In order for this skript to work you need to have access to
### --- ICE Singapore.
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
### --- unit counts for speakers represented in ICE Singapore with R",
### --- unpublished R script, Hamburg University.
### --- THANK YOU. Copyright Martin Schweinberger (2014).
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
library(zoo)
###############################################################
# Setting options
options(stringsAsFactors = F)
# WARNING: To use this script you need to set our own paths!
# Your path should be the path to the corpus on your own computer!
# Remember to use double backslash instead of single backslash, if
# you use Windows on your maschine. The outputhpath should be the
# location where you would like to store the final biodata data set.
# Specify pathname of the corpus
pathname.corpus <- "C:\\PhD\\skripts n data\\corpora\\ICE SINGAPORE\\Corpus"
# Define outputpath
outputpath <- "C:\\MeineHomepage\\docs\\data/BiodataIceSingapore.txt"
###############################################################
# Prepare for loading corpus
# Choose the files you would like to use
corpus.files = list.files(path = pathname.corpus,
  pattern = NULL,
  all.files = T,
  full.names = T,
  recursive = T,
  ignore.case = T,
  include.dirs = T)
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
# Specify search pattern
splitpattern = "</I>"
# Splits corpus into parts
corpus.tmp5 <- sapply(corpus.tmp2, function(x) {
  strsplit(as.character(x), splitpattern)  }  )
# Retrieve only spoken files
corpus.tmp5 <- corpus.tmp5[[1]][1:397]
# Clean the corpus files
corpus.tmp6 <- sapply(corpus.tmp5, function(x)  {
  str_trim(x, side = "both")  }  )
###############################################################
# Extract text.ids from the list elements
full.text.ids.tmp1 <- lapply(corpus.tmp6, function(x)  {
  x <- strsplit( gsub("(.*?<ICE-SIN:[A-Z]{0,1}[a-z]{0,1}[1|2][A-Z]{0,1}[a-z]{0,1}-[0-9]{3,3}#[0-9]{0,3}:{0,1}[0-9]{0,3}:{0,1}[A-Z]{0,2}[0-9]{0,1}>)", "\\1~", x), "~" )  }  )
# Retrieve the first item from each list element &
#  vectorize the resulting list
full.text.ids.tmp2 <- as.vector(unlist(sapply(full.text.ids.tmp1, function(x)  {
  sapply(x, "[", 1)  }  )))
# Clean the text.ids
full.text.ids.tmp3 <- as.vector(sapply(full.text.ids.tmp2, function(x)  {
  x <- gsub("(.*<ICE)","<ICE", x, perl = TRUE)  }  ))
full.text.ids.tmp3 <- toupper(full.text.ids.tmp3)
###############################################################
full.text.ids.tmp4 <- as.vector(sapply(full.text.ids.tmp3, function(x)  {
  x <- gsub("(#[0-9]{1,3}:)","#", x, perl = TRUE)
  x <- gsub("(:[A-Z]>)","", x, perl = TRUE)
  x <- gsub("(<ICE-SIN:)","", x, perl = TRUE)
  x <- gsub("(>)","", x, perl = TRUE)  }  ))
# Retrieve text.ids and subfile.ids from the data
# text.ids
text.ids <- as.vector(sapply(full.text.ids.tmp4, function(x)  {
  x <- gsub("(#.*)","", x, perl = TRUE)  }  ))
# subfile.ids
subfile.ids <- as.vector(sapply(full.text.ids.tmp4, function(x)  {
  x <- gsub("(.*#)","", x, perl = TRUE)  }  ))
###############################################################
# Create a matrix out of the data
corpus.tb1 <- as.data.frame(cbind(1:length(corpus.tmp6), text.ids, subfile.ids, corpus.tmp6))
# Add column labels
colnames(corpus.tb1) <- c("id","text.id","subfile.id","corpusfile")
# Add row labels
rownames(corpus.tb1) <- c(1:length(corpus.tmp6))
# Rename table to match other ice skripts
corpus.table2 <- corpus.tb1
# View results
#head(corpus.table2)
###############################################################
### --- STEP
###############################################################
# Extract the corpus file
all.files <- corpus.table2[1:nrow(corpus.table2), 4]
# Split corpus files so that each speech.unit is one element
all.files.unclean <- sapply(all.files, function(x) {
  corpfile2 <- strsplit(gsub("(<ICE-SIN:[A-Z]{0,1}[a-z]{0,1}[1|2][A-Z]{0,1}[a-z]{0,1}-[0-9]{3,3}#[0-9]{0,3}:{0,1}[0-9]{0,3}:{0,1}[A-Z]{0,2}[0-9]{0,1}>)", "~\\1", x), "~" )  }  )
###############################################################
### --- REPIRS
# PROBLEM: line 46 in element 57 is broken!
# Repair all.files.unclean[[57]][46]
all.files.unclean[[57]][46] <- "<ICE-SIN:S1A-051#46:1:A> Question why you treat it like dirt right  "
all.files.unclean[[57]] <- c(all.files.unclean[[57]][1:46], all.files.unclean[[57]][48:length(all.files.unclean[[57]])])
# PROBLEM: line 46 in element 57 is broken!
# Repair all.files.unclean[[57]][46]
all.files.unclean[[127]][20] <- "<ICE-SIN:S1B-011#19:1> And do you recall the there has to be a balance between the part of the sludge that is wasted to the sludge that is recycled and Im sure you know why there has to be this balance right  "
all.files.unclean[[127]] <- c(all.files.unclean[[127]][1:20], all.files.unclean[[127]][22:length(all.files.unclean[[127]])])
#PROBLEM: No speaker refs in element [[127]]
all.files.unclean[[127]] <- gsub("(:1>)", ":1:?>", all.files.unclean[[127]])
#PROBLEM: No speaker refs in element [[136]]
all.files.unclean[[136]][22] <- "<ICE-SIN:s1b-020#22:1> What "
all.files.unclean[[136]] <- gsub("(:1:{0,1}>)", ":1:?>", all.files.unclean[[136]])
all.files.unclean[[136]] <- gsub("(s1b-020)", "S1B-020", all.files.unclean[[136]])
all.files.unclean[[136]] <- c(all.files.unclean[[136]][1:22], all.files.unclean[[136]][24:length(all.files.unclean[[136]])])
all.files.unclean <- sapply(all.files.unclean, function(x) {
  x <- gsub("(:1>)", ":1:?>", x)
  x <- gsub("(:1:>)", ":1:?>", x)  }  )
###############################################################
# Add names to all.files.unclean
file.subfile.ids <- apply(corpus.table2[ , c(2, 3)] , 1 , paste , collapse = "#" )
names(all.files.unclean) <- file.subfile.ids
# Delete every first item from the elements
all.files.unclean <- sapply(all.files.unclean, function(x)  {
  x <- x[2:length(x)]  }  )
###############################################################
# Separate the speakers from the speech.units
speakers.and.speech.units <- lapply(all.files.unclean, function(x) {
  str_split(x, " ", n = 2)  }  )
# Store speakers in extra vector and clan the elements
full.speakers <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, "[[", 1)  }  )
# Extract the speakers
speakers.tmp1 <- lapply(full.speakers, function(x)  {
  str_replace_all(x, "(.*?:)","")  }  )
speakers <- lapply(speakers.tmp1, function(x) gsub(">","", x, fixed = TRUE))
# Store speech.units in extra vector
speech.units <- lapply(speakers.and.speech.units, function(x) {
  sapply(x, function(x) x[2])  }  )
###############################################################
# Create a list with all speech.units but cleaned, i.e. without metas
speech.units.clean <- lapply(speech.units, function(x) {
  x <- str_replace_all(x, "(<O.*?/O>)","")
  x <- str_replace_all(x, "(<q.*?/q>)","")
  x <- str_replace_all(x, "(<&.*?/&.*>)","")
  x <- str_replace_all(x, "(<unclear.*?unclear>)","")
  x <- str_replace_all(x, "(<[a-z]{4,}.*?</[a-z]{4,}>)","")
# WARNING: THEORETICAL ISSUE
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
  x <- gsub("Its ", "It's ", x)
  x <- gsub(" its ", " it's ", x)
  x <- gsub("Shes ", "She's ", x)
  x <- gsub(" shes ", " she's ", x)
  x <- gsub("Arent ", "Aren't ", x)
  x <- gsub(" arent ", " aren't ", x)
  x <- gsub("Theyre ", "They're ", x)
  x <- gsub(" theyre ", " they're ", x)
  x <- gsub("Wasnt ", "Wasn't ", x)
  x <- gsub(" wasnt ", " wasn't ", x)
  x <- gsub("(<.*?>)", " ", x)
  x <- gsub("(\\?|\\(|\\)|\\{|\\}|\\[|\\]|\\$|\\&|\\.|-|>|<|\\?|/|=|,)", " ", x)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x)  }  )
###############################################################
### --- Create a list which holds the number of words per speech.unit
###############################################################
# First, we tokenize the list elements
tokenized <- lapply(speech.units.clean, function(x){
  tokenized <- strsplit(x, " ")
  }  )
# Now, we count the words elements(words) in each speech.unit (list element)
word.count <- lapply(tokenized, function(x) {
  sapply(x, function(y)
    length(y))     } )
###############################################################
# Create a list which holds the number of speech.units
#Extract the number of speech.units for each speaker of all spoken files
speech.unit.count.tmp1 <- lapply(word.count, function(x) {
  sapply(x, function(y) gsub(".*",  "1", y))  }  )
# Rename list
speech.unit.count <- speech.unit.count.tmp1
###############################################################
# Create a list for all files in the corpus which holds the
# entire speaker information (speaker, speech.unit, speech.unit.clean,
# speech.unit.count, word.count)
###############################################################
#
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
###############################################################
###############################################################
# Rename data for later kwik seraches
kwik.tb.ice.sin <- speakerinfo2
###############################################################
###############################################################
###############################################################
###############################################################
# Extract the words counts for speakers in one file
word.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[6]], x[[1]], sum)))
  } )
# Simplify the results
overview.word.count.results <- sapply(word.count.result, "[[", 1)
# Extract the speech.unit counts for speakers in one file
speech.unit.count.result <- lapply(speakerinfo2, function(x) {
  sapply(x, function(y) as.data.frame(tapply(x[[5]], x[[1]], sum)))
  } )
# Simplify the results
overview.speech.unit.count.results <- sapply(speech.unit.count.result, "[[", 1)
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
###############################################################
# From the speaker.ids vector, we also extract the file names and
# the speaker.ids
# First, let's extract the file names
files <- gsub("#.*", "", subfiles.tmp)
###############################################################
# We now want to extract the speech.unit counts for each speaker
# in vector format so that we can easily create a table out of
# the results
speech.unit.count.list <- lapply(X = overview.speech.unit.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)
    }  )  }  )
# Convert the list into a vector
speech.unit.counts <- as.vector(unlist(speech.unit.count.list))
###############################################################
# We now want to extract the word counts for each speaker
# in vector format so that we can easily create a table out of
# the results
word.count.list <- lapply(overview.word.count.results, function(x) {
  sapply(x, function(y) {
    sapply(y, "[[", 1)
    } )  }  )
# Convert the list into a vector
word.counts <- as.vector(unlist(word.count.list))
# Create an object holding file, subfile and speaker info
file.speaker.id.tmp1 <- paste("<", files, "#", subfiles, ":", speakers, ">", collapse = "")
file.speaker.id.tmp2 <- str_replace_all(file.speaker.id.tmp1, " ","")
file.speaker.id.tmp3 <- lapply(file.speaker.id.tmp2, function(x)  {
  x <- strsplit( gsub("(>)", "\\1~", x), "~" )  }  )
file.speaker.id <- as.vector(unlist(file.speaker.id.tmp3))
###############################################################
# We now want to create a table with speaker id, speech.unit count
# and word count
# First, we create an index
id <- c(1:length(speakers))
# Now, we set up the data frame
speakerinfo.ice.sin <- cbind(id, file.speaker.id, files, subfiles, speakers,
  speech.unit.counts, word.counts)
speakerinfo.ice.sin <- as.data.frame(speakerinfo.ice.sin)
colnames(speakerinfo.ice.sin) <- c("id", "file.speaker.id", "text.id",
  "subfile.id", "speaker.id", "speech.unit.count", "word.count")
# View results
#speakerinfo.ice.sin
# View results (without empty rows)
speakerinfo.ice.sin.1 <- speakerinfo.ice.sin[!speakerinfo.ice.sin[, 2] == "", ]
speakerinfo.ice.sin.1[, 6] <- as.numeric(speakerinfo.ice.sin.1[, 6])
speakerinfo.ice.sin.1[, 7] <- as.numeric(speakerinfo.ice.sin.1[, 7])
speakerinfo.ice.sin.1[, 1] <- 1:length(speakerinfo.ice.sin.1[, 1])
rownames(speakerinfo.ice.sin.1) <- speakerinfo.ice.sin.1[, 1]
biodata.ice.sin <- speakerinfo.ice.sin.1
# View results
#biodata.ice.sin
###############################################################
###############################################################
###############################################################
### --- Important objects
#kwik.tb.ice.sin
#head(kwik.tb.ice.sin)
#biodata.ice.sin
#head(biodata.ice.sin)
###############################################################
###############################################################
###############################################################
################################################################
### --- STEP
################################################################
###############################################################
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(outputpath, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.sin, outputpath, sep = "\t", row.names = F)
###############################################################
# Remove all lists from the current workspace
#rm(list=ls(all=T))
###############################################################
###############################################################
### ---                    THE END
###############################################################
###############################################################
