# title: creating a personalized vowel chart with R
# author: martin schweinberger
# date: 2016-06-27
# description: plotting a customized vowel chart with R
# the input data must be a data frame of the format:
# subject (Speaker), file, item, F1, and F2.
# the column label of x must at least contain:
# the reference data must be a data frame of the format:
# subject, file, item, F1, F2.
#########################################################
# remove all lists from the current workspace
rm(list=ls(all=T))
# set path to data
vowelpath <- "http://www.martinschweinberger.de/docs/data/vowels.txt"
# set path to RP data
refpath <- "http://www.martinschweinberger.de/docs/data/rpvowels.txt"
# function for transparent symbols
addTrans <- function(color,trans)
{
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
# load data
v <- read.table(vowelpath, header = T, sep = "\t")
# convert into data frame
v <- as.data.frame(v)
# order data frame
v <- v[order(v$subject, v$item, v$trial), ]
# calculate mean for each vowel
F1 <- tapply(v$F1, v$item, mean)
F2 <- tapply(v$F2, v$item, mean)
# calculate sd for each vowel
F1sd <- tapply(v$F1, v$item, sd)
F2sd <- tapply(v$F2, v$item, sd)
# create a data frame from the values
v1 <- data.frame(rep("ms", length(F1)), names(F1), rep("wordlist", length(F1)), F1, F2, F1sd, F2sd)
# adapt column names
colnames(v1) <- c("subject", "item", "context", "F1", "F2", "F1sd", "F2sd")
# define the axis values/labels for the plot
z1 = seq(-00, 3000, 500)
z2 = seq(250, 1050, 100)
# transform values
x = v1$F2 - v1$F1
y = v1$F1
# save plot to disc
#png("C:\\03-MyProjects\\00HomepageTutorials\\PersVowelchartPraat\\article\\images/vowelchart.png", width = 480, height = 480) # save plot
# set up plot
symbols(x, y, circles = v1$F1sd, inches = 1/3, bg = addTrans("lightgrey", 100),
fg = NULL, xlim = rev(range(z1)), ylim = rev(range(z2)), xlab = "F1 (Hz)",
ylab = "F2 - F1 (Hz)", add = F)
# add ipa symbols
ipa <- c("\u00E6", # had
"\u0251", # hard
"e", # head
"i", # heed
"\u025C", # herd
"\u026A ", # hid
"\u0254", # hoard
"\u0252", # hod
"\u028A", # hood
"\u028C", # hud
"u" # whod
)
box() # add box
grid() # add grid
# add symbols
text(x, y, ipa, cex = .8)
text(x, y, v1$item, pos = 1, cex = .8)
# load reference data
ref <- read.table(refpath, header = T, sep = "\t")
# convert into data frame
ref <- as.data.frame(ref)
# order data frame
ref <- ref[order(ref$subject, ref$item, ref$trial), ]
# calculate mean for each refowel
F1 <- tapply(ref$F1, ref$item, mean)
F2 <- tapply(ref$F2, ref$item, mean)
# calculate sd for each refowel
F1sd <- tapply(ref$F1, ref$item, sd)
F2sd <- tapply(ref$F2, ref$item, sd)
# create a data frame from the ref values
ref1 <- data.frame(rep("rpspk", length(F1)), names(F1), rep("wordlist", length(F1)), F1, F2)
# change col names
colnames(ref1) <- c("subject", "item", "context", "F1", "F2")
# add sd to data frame
ref2 <- data.frame(ref1, F1sd, F2sd)
# transform refalues
x = ref2$F2 - ref2$F1
y = ref2$F1
# set up plot
symbols(x, y, circles = ref2$F1sd, inches = 1/3, bg = addTrans("lightblue", 100),
fg = NULL, xlim = reref(range(z1)), ylim = reref(range(z2)), xlab = "",
ylab = "", add = T, main = "")
# add ipa symbols
ipa <- c("\u00E6", # had
"\u0251", # hard
"e", # head
"i", # heed
"\u025C", # herd
"\u026A ", # hid
"\u0254", # hoard
"\u0252", # hod
"\u028A", # hood
"\u028C", # hud
"u" # whod
)
# add symbols
text(x, y, ipa, cex = .8, col = "red")
text(x, y, ref2$refowel, pos = 1, cex = .8)
# add legend
legend("bottomleft", inset=.05, c("Participant", "Native (modern Received Pronunciation)"),
  fill=c("lightgrey", "lightblue"), horiz=F)
#dev.off()
