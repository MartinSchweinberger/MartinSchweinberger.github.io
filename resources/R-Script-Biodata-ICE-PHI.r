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
corpus.phi <- "C:\\PhD\\skripts n data\\corpora\\ICE Philippines\\Corpus"
# Define input pathname of raw biodata
bio.phi <- "C:\\PhD\\skripts n data\\corpora\\ICE Philippines\\Headers/ice philippines biodata spoken.txt"
# Define outputpath of final biodata
out.phi <- "C:\\MeineHomepage\\docs\\data/BiodataIcePhilippines.txt"
###############################################################
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
###############################################################
###############################################################
### --- Important objects
# ICE Philippines
#kwic.tb.ice.phi
#head(kwic.tb.ice.phi)
#biodata.ice.phi
#head(biodata.ice.phi)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.phi, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.phi, out.phi, sep = "\t", row.names = F)
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
