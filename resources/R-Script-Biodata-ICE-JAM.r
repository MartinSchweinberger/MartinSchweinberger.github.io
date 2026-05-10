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
corpus.jam <- "C:\\PhD\\skripts n data\\corpora\\ICE Jamaica"
# Define input pathname of raw biodata
bio.jam <- "C:\\PhD\\skripts n data\\biodata raw\\ICE Jamaica biodata raw/biodata_original.txt"
# Define outputpath of final biodata
out.jam <- "C:\\MeineHomepage\\docs\\data/BiodataIceJamaica.txt"
###############################################################
###############################################################
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
  scan(x, what = "char", sep = "\t", quiet = T, skipNul = T, quote = "")  }  )
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
###############################################################
### --- Important objects
###############################################################
# ICE Jamaica
#kwic.tb.ice.jam
#head(kwic.tb.ice.jam)
#biodata.ice.jam
#head(biodata.ice.jam)
###
# Save results in a txt file
# Choose a file in which to store the results
output.file <- file.create(out.jam, showWarnings = F)
# Store the txt file in the output file
write.table(biodata.ice.jam, out.jam, sep = "\t", row.names = F)
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