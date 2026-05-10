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
#x <- "C:\\03-MyProjects\\01VowelAnalysis/itws001.txt"        # for testing
#ref <- "C:\\03-MyProjects\\01VowelAnalysis/RPMS.txt"         # for testing
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
  # calculate mean for each vowel
  sub <- "GenAM"#names(table(v$sub))
  F1 <- tapply(v$F1_Hz, v$item, mean)
  F2 <- tapply(v$F2_Hz, v$item, mean)
  F3 <- tapply(v$F3_Hz, v$item, mean)
  # calculate sd for each vowel
  F1sd <- tapply(v$F1_Hz, v$item, sd)
  F2sd <- tapply(v$F2_Hz, v$item, sd)
  F3sd <- tapply(v$F3_Hz, v$item, sd)
  # transform data frame for labov normalization
  # create a data frame from the values
  v1 <- data.frame(sub, names(F1), rep("NA", length(F1)),  F1, F2, F3, rep(NA, length(F1)), rep(NA, length(F1)), rep(NA, length(F1)))
  # change col names
  colnames(v1) <- c("speaker_id", "vowel_id", "context", "F1", "F2", "F3", "F1_glide", "F2_glide", "F3_glide")
  # normalize vowels
  v2 <- norm.labov(v1, G.value=NA, use.f3=FALSE, geomean=TRUE)
  # add sd to data frame
  v3 <- data.frame(v2, F1sd, F2sd, F3sd)
  # add column names
  colnames(v3) <- c(gsub("'", "", colnames(v2)), "F1sd", "F2sd", "F3sd")
  colnames(v3) <- gsub(" ", "", colnames(v3))
#################################################################
# set up plot
  # define the axis values/labels for the plot
  z1 = seq(-00, 3000, 500)
  z2 = seq(250, 1050, 100)
  # transform values
  x = v3$F2 - v3$F1
  y = v3$F1
  # set up plot
  symbols(x, y, circles = v3$F1sd, inches = 1/3, bg = addTrans("lightgrey", 100),
    fg = NULL, xlim = rev(range(z1)), ylim = rev(range(z2)), xlab = "F1 (Hz)",
    ylab = "F2 - F1 (Hz)", add = F, main = "Individual Vowel Formant Values (Labov Normalized)")
  # add ipa symbols
  ipa <- c("\u00E6",    # had
    "\u0251",           # hard
    "e",                # head
    "i",                # heed
    "\u025C",           # herd
    "\u026A ",          # hid
    "\u0254",           # hoard
    "\u0252",           # hod
    "\u028A",           # hood
    "\u028C",           # hud
    "u"                 # whod
  )
  box()
  grid()
  # add symbols
  text(x, y, ipa, cex = .8)
  text(x, y, v3$Vowel, pos = 1, cex = .8)
#################################################################
### --- add reference
#################################################################
  # conrefert into data frame
  ref <- as.data.frame(ref)
  # order data frame
  ref <- ref[order(ref$sub, ref$item, ref$trial), ]
  # calculate mean for each refowel
  sub <- "GenAM"
  F1 <- tapply(ref$F1_Hz, ref$item, mean)
  F2 <- tapply(ref$F2_Hz, ref$item, mean)
  F3 <- tapply(ref$F3_Hz, ref$item, mean)
  # calculate sd for each refowel
  F1sd <- tapply(ref$F1_Hz, ref$item, sd)
  F2sd <- tapply(ref$F2_Hz, ref$item, sd)
  F3sd <- tapply(ref$F3_Hz, ref$item, sd)
  # transform data frame for laboref normalization
  # create a data frame from the refalues
  ref1 <- data.frame(sub, names(F1), rep("NA", length(F1)),  F1, F2, F3, rep(NA, length(F1)), rep(NA, length(F1)), rep(NA, length(F1)))
  # change col names
  colnames(ref1) <- c("speaker_id", "refowel_id", "context", "F1", "F2", "F3", "F1_glide", "F2_glide", "F3_glide")
  # normalize refowels
  ref2 <- norm.labov(ref1, G.value=NA, use.f3=FALSE, geomean=TRUE)
  # add sd to data frame
  ref3 <- data.frame(ref2, F1sd, F2sd, F3sd)
  # add column names
  colnames(ref3) <- c(gsub("'", "", colnames(ref2)), "F1sd", "F2sd", "F3sd")
  colnames(ref3) <- gsub(" ", "", colnames(ref3))
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
    "\u0251",           # hard
    "e",                # head
    "i",                # heed
    "\u025C",           # herd
    "\u026A ",          # hid
    "\u0254",           # hoard
    "\u0252",           # hod
    "\u028A",           # hood
    "\u028C",           # hud
    "u"                 # whod
  )
  # add symbols
  text(x, y, ipa, cex = .8, col = "red")
  text(x, y, ref3$refowel, pos = 1, cex = .8)
  # add legend
  ifelse(refvar == refam, 
  legend("bottomleft", inset=.05, c("Participant", "Native (General American)"), fill=c("lightgrey", "lightblue"), horiz=TRUE),
  legend("bottomleft", inset=.05, c("Participant", "Native (modern Received Pronunciation)"), fill=c("lightgrey", "lightblue"), horiz=TRUE))
}