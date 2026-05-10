##########################################################
### --- R script "Syntactic Parsing with R"
### --- Author: Martin Schweinberger
### --- This script aims at an automated approach to syntacically parsing
### --- a sample corpus.
##########################################################
### --- write a function which syntactically parses text in corpus files
###############################################################
# write function
paRsing <- function(path){
  require("NLP")
  require("openNLP")
  require("openNLPmodels.en")
  require("stringr")
  corpus.files = list.files(path = path, pattern = NULL, all.files = T,
    full.names = T, recursive = T, ignore.case = T, include.dirs = T)
  corpus.tmp <- lapply(corpus.files, function(x) {
    scan(x, what = "char", sep = "\t", quiet = T) }  )
  corpus.tmp <- lapply(corpus.tmp, function(x){
    x <- paste(x, collapse = " ")  }  )
  corpus.tmp <- lapply(corpus.tmp, function(x) {
    x <- enc2utf8(x)  }  )
  corpus.tmp <- gsub(" {2,}", " ", corpus.tmp)
  corpus.tmp <- str_trim(corpus.tmp, side = "both")
  Corpus <- lapply(corpus.tmp, function(x){
    x <- as.String(x)  }  )
  lapply(Corpus, function(x){
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    # clear memory
    gc()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    # clear memory
    gc()
    annotated <- annotate(x, list(sent_token_annotator, word_token_annotator))
# clear memory
    gc()
# Compute the parse annotations only.
    parse_annotator <- Parse_Annotator()
    # clear memory
    gc()
    parsed <- parse_annotator(x, annotated)
    gc()
# Extract the formatted parse trees.
    parsedtexts <- sapply(parsed$features, '[[', "parse")
# Read into NLP Tree objects.
    parsetrees <- lapply(parsedtexts, Tree_parse)
    gc()
        return(list(parsedtexts, parsetrees)) 
 }  )
  }

##########################################################
##########################################################
##########################################################
# test the function
parsetest <- paRsing(path = "C:\\03-MyProjects\\PosTagging\\TestCorpus")
parsetest

##########################################################
##########################################################
##########################################################
### --- The END
##########################################################
##########################################################
##########################################################
