#################################################################
### function for creating a customized vowel chart plot with R
#################################################################
# function for producing a vowelplot which takes the argument(s):
# x = a table or data frame of the format: sub (Speaker), trial, item (Word), F1_Hz, F2_Hz, F3_Hz
# the column label of x must at leat contain:
# sub, item, trial, F1_Hz, F2_Hz, F3_Hz
# ref = a table or data frame of the format: sub (Speaker), trial, item (Word), F1_Hz, F2_Hz, F3_Hz
# the column label of x must at leat contain:
# sub, item, trial, F1_Hz, F2_Hz, F3_Hz
#################################################################
### ---                      START
#################################################################
vwlchrt <- function(x, ref){
#################################################################
#x <- "C:\\03-MyProjects\\01VowelAnalysis/T0004.txt"        # for testing
#ref <- "C:\\03-MyProjects\\01VowelAnalysis/GenAmMS.txt"    # for testing
#################################################################
  # load package
  require("vowels") # for normalization
#################################################################
  refvar <- ref
# function for transparent symbols  (taken from :
# http://stackoverflow.com/questions/12995683/any-way-to-make-plot-points-in-scatterplot-more-transparent-in-r)
  addTrans <- function(color,trans)
  {
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}
#################################################################
  # load data (input = data frame with)
  v <- read.table(x, header = T, sep = "\t")
  ref <- read.table(ref, header = T, sep = "\t")
#################################################################
# prepare data
  # convert into data frame
  v <- as.data.frame(v)
  # order data frame
  v <- v[order(v$sub, v$item, v$trial), ]
  # addcolumns
  v$F1_glide <- rep(NA, nrow(v))
  v$F2_glide <- rep(NA, nrow(v))
  v$F3_glide <- rep(NA, nrow(v))
  #  reduce data set
  v <- v[, c(1, 3:4, 6:11)]
  # normalize vowels
  v <- convert.bark(v)
  # calculate mean for each vowel
  sub <- "GenAM"#names(table(v$sub))
  F1 <- tapply(v$Z1, v$Vowel, mean)
  F2 <- tapply(v$Z2, v$Vowel, mean)
  F3 <- tapply(v$Z3, v$Vowel, mean)
  # calculate sd for each vowel
  F1sd <- tapply(v$Z1, v$Vowel, sd)
  F2sd <- tapply(v$Z2, v$Vowel, sd)
  F3sd <- tapply(v$Z3, v$Vowel, sd)
  # create a data frame from the values
  v1 <- data.frame(sub, names(F1), rep("NA", length(F1)),  F1, F2, F3, rep(NA, length(F1)), rep(NA, length(F1)), rep(NA, length(F1)))
  # change col names
  colnames(v1) <- c("speaker_id", "vowel_id", "context", "F1", "F2", "F3", "F1_glide", "F2_glide", "F3_glide")
  # add sd to data frame
  v3 <- data.frame(v1, F1sd, F2sd, F3sd)
  # add column names
  colnames(v3) <- c(gsub("'", "", colnames(v1)), "F1sd", "F2sd", "F3sd")
  colnames(v3) <- gsub(" ", "", colnames(v3))
# use only items used by b. stuhl
  selection <- c("had", "head", "heed", "hid")
  v3$rmv <- as.vector(unlist(sapply(v3$vowel_id, function(x){
    x <- ifelse(x %in% selection, "keep", "remove") } )))
  v3 <- v3[v3$rmv != "remove",]
  v3 <- v3[,c(1:12)]
#################################################################
# set up plot
  # define the axis values/labels for the plot
  z1 = seq(0, 20, 5)
  z2 = seq(0, 20, 5)
  # transform values
  x = v3$F2 - v3$F1
  y = v3$F1
  # set up plot
  symbols(x, y, circles = v3$F1sd, inches = 1/3, bg = addTrans("lightgrey", 100),
    fg = NULL, xlim = rev(range(z1)), ylim = rev(range(z2)), xlab = "Z1",
    ylab = "Z2 - Z1", add = F, main = "Individual Vowel Formant Values (Labov Normalized)")
  # add ipa symbols
  ipa <- c("\u00E6",    # had
    "e",                # head
    "i",                # heed
    "\u026A "          # hid
  )
  box()
  grid()
  # add symbols
  text(x, y, ipa, cex = .8)
  text(x, y, v3$vowel_id, pos = 1, cex = .8)
#################################################################
### --- add reference
#################################################################
  # conrefert into data frame
  ref <- as.data.frame(ref)
  # order data frame
  ref <- ref[order(ref$sub, ref$item, ref$trial), ]
  # use only items used by b. stuhl
  selection <- c("had", "head", "heed", "hid")
  ref$rmv <- as.vector(unlist(sapply(ref$item, function(x){
    x <- ifelse(x %in% selection, "keep", "remove") } )))
  ref <- ref[ref$rmv != "remove",]
  ref <- ref[,c(1:10)]
  # addcolumns
  ref$F1_glide <- rep(NA, nrow(ref))
  ref$F2_glide <- rep(NA, nrow(ref))
  ref$F3_glide <- rep(NA, nrow(ref))
  # clean files
  ref$file <- gsub(".*/", "", ref$file)
#  # adapt age and gender of speakers
#  ifelse(group == "child",
#    ref <- ref[ref$sub == "C", ],
#    ifelse(group == "female",
#      ref <- ref[ref$sub == "F", ],
#      ifelse(group == "male",
#      ref <- ref[ref$sub == "M", ], ref <- ref)))
  #  reduce data set: speaker_id, vowel_id, context, F1, F2, F3, F1_glide, F2_glide, F3_glide
  ref <- ref[, c(2:3, 5, 7:9, 11:13)]
  # normalize vowels
  ref <- convert.bark(ref)
  # calculate mean for each refowel
  F1 <- tapply(ref$Z1, ref$Vowel, mean)
  F1 <- F1[is.na(F1) == F]
  F2 <- tapply(ref$Z2, ref$Vowel, mean)
  F2 <- F2[is.na(F2) == F]
  F3 <- tapply(ref$Z3, ref$Vowel, mean)
  F3 <- F3[is.na(F3) == F]
  # calculate sd for each refowel
  F1sd <- tapply(ref$Z1, ref$Vowel, sd)
  F1sd <- F1sd[is.na(F1sd) == F]
  F2sd <- tapply(ref$Z2, ref$Vowel, sd)
  F2sd <- F2sd[is.na(F2sd) == F]
  F3sd <- tapply(ref$Z3, ref$Vowel, sd)
  F3sd <- F3sd[is.na(F3sd) == F]
  # add sd to data frame
  ref3 <- data.frame(ref$Speaker[1:4], ref$Vowel[1:4], ref$Context[1:4], F1, F2, F3, F1sd, F2sd, F3sd)
  # add column names
  colnames(ref3)[1:3] <- c("File", "Vowel", "Context")
#################################################################
  # transform refalues
  x = ref3$F2 - ref3$F1
  y = ref3$F1
  # set up plot
  symbols(x, y, circles = ref3$F1sd, inches = 1/3, bg = addTrans("lightblue", 100),
    fg = NULL, xlim = reref(range(z1)), ylim = reref(range(z2)), xlab = "",
    ylab = "", add = T, main = "")
  # add ipa symbols
  ipa <- c("\u00E6",    # had
    "e",                # head
    "i",                # heed
    "\u026A "          # hid
  )
  # add symbols
  text(x, y, ipa, cex = .8, col = "red")
  text(x, y, ref3$refowel, pos = 1, cex = .8)
  # add legend
  legend("bottomleft", inset=.05, c("Participant", "Native (General American)"), fill=c("lightgrey", "lightblue"), horiz=TRUE)

}

# group = "child" or "female" or "male"