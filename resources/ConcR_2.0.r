###############################################################
### --- Title: "ConcR_2.0"
### --- Author: Martin Schweinberger
### --- Date:  2015-07-21
### --- Description:
### --- R script defining function "ConcR" - a concordance function for R
### --- R version 3.1.0 (2015-11-08) -- "Spring Dance"
### --- This script was written by Martin Schweinberger
### --- (<http://www.martinschweinberger.de/blog/>).
### --- It extracts concordances from corpus data.
### --- This script is made available under the GNU General Public License
### --- <http://www.gnu.org/licenses/gpl.html>.
### --- If you use it, PLEASE QUOTE it as:
### --- Schweinberger, Martin. 2015. ConcR_2.0 - an update on a concordance function for R.
### --- Unpublished R script.
### --- THANK YOU. Copyright Martin Schweinberger (2015).
##################################################################
### --- START
##################################################################
# Start defining function
ConcR <- function(path, pattern, context, all.pre = FALSE) {
  # install required packages
  #install.packages("stringr")
  #install.packages("plyr")
  # load packages
  require(plyr)
  require(stringr)
  # list files
  corpus.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
  conc <- sapply(corpus.files, function(x) {
    txt <- scan(x, what = "char", sep = "", quiet = T, skipNul = T, quote = "")
      file <- str_replace_all(x, ".*/", "")
      txt <- paste(txt, collapse = " ")
      txt <- str_replace_all(txt, " {2,}" , " ")
      txt <- str_trim(txt, side = "both")
      lngth <- nchar(txt)
#      ifelse(exact == FALSE, pattern <- paste(" {0,1}[A-Z]{0,1}[a-z]{0,}", pattern, "[a-z]{0,} {0,1}", sep=""), pattern <- pattern)
#      ifelse(exact == FALSE, pattern <- paste("[ |!|.|?|:|;]{1,}", pattern, "[ |!|.|?|:|;]{1,}", sep=""), pattern <- pattern)
      pattern <- paste("[ |!|.|?|:|;]{1,}", pattern, "[ |!|.|?|:|;]{1,}", sep="")
      idx <- str_locate_all(txt, pattern)
      idx <- idx[[1]]
      token.idx.start <- idx[,1]
      token.idx.end <- idx[,2]
      pre.idx.start <- idx[,1]-context
      pre.idx.end <- idx[,1]-1
      pre.idx.all.start <- rep(1, nrow(idx))
      pre.idx.all.end <- idx[,1]-1
      post.idx.start <- idx[,2]+1
      post.idx.end <- idx[,2]+context
      enddf <- data.frame(post.idx.end, c(rep(lngth, length(as.vector(unlist(post.idx.end))))))
      end <- apply(enddf, 1, function(x){
        ifelse(x[1] > x[2], x <- x[2], x <- x[1]) } )
      post.idx.end <- as.vector(unlist(end))
      conc.df <- data.frame(pre.idx.start, pre.idx.end, token.idx.start, token.idx.end, post.idx.start, post.idx.end, pre.idx.all.start)
      concr1 <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
        pre <- substr(txt, x[1], x[2])
        token <- substr(txt, x[3], x[4])
        post <- substr(txt, x[5], x[6])
        unlist(c(rep(file, length(pre)), pre, token, post))
        } ) }
      concr2 <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
          pre <- substr(txt, x[1], x[2])
          token <- substr(txt, x[3], x[4])
          post <- substr(txt, x[5], x[6])
          all.pre <- substr(txt, x[7], x[2])
          unlist(c(rep(file, length(pre)), pre, token, post, all.pre))
       } ) }
      ifelse(all.pre == F, conc <- concr1(conc.df, txt), conc <- concr2(conc.df, txt))
#      concn <- t(conc)
      conc <- as.data.frame(t(conc))
      return(conc)
      }  )
# df <- do.call("rbind", conc)
 df <- ldply(conc, data.frame)
  if(nrow(df) == 0) cat("No hits were found! Therefore, the number of columns does not match the number of column names!\n") else df <- df
  ifelse(ncol(df) == 0, cat("Number of columns does not match the number of column names!\n"),
    ifelse(ncol(df) == 4, colnames(df) <- c("file", "pre", "token", "post"),
    ifelse(ncol(df) == 5, colnames(df) <- c("file", "pre", "token", "post", "all.pre"),
    ifelse(ncol(df) > 5, t(df)))))
  if(nrow(df) >= 1) rownames(df) <- 1:nrow(df) else cat("No hits were found! \n")
  if(nrow(df) >= 1) return(df) else cat("No hits were found! \n")
}
###############################################################
### ---                    THE END
###############################################################
# Example
#path <- "C:\\Corpora\\original\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
#test <- ConcR(path = path, pattern = "[A|a]ll", context = 30, all.pre = F)
# inspect results
#nrow(test)



