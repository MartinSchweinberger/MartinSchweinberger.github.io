###############################################################
### --- Title: "ConcR_2.0"
### --- Author: Martin Schweinberger
### --- Date:  2016-01-15
### --- Description:
### --- R script defining function "ConcR" - a concordance function for R
### --- R version 3.1.0 (2015-11-08) -- "Spring Dance"
### --- This script was written by Martin Schweinberger
### --- (<http://www.martinschweinberger.de/blog/>).
### --- It extracts concordances from corpus data.
### --- This script is made available under the GNU General Public License
### --- <http://www.gnu.org/licenses/gpl.html>.
### --- If you use it, PLEASE QUOTE it as:
### --- Schweinberger, Martin. 2016. ConcR_2.0 - an update on a concordance function for R.
### --- Unpublished R script.
### --- THANK YOU. Copyright Martin Schweinberger (2016).
##################################################################
### --- START
##################################################################
# Start defining function
ConcR <- function(path, pattern, context, all.pre = FALSE) {
  # install required packages
  #install.packages("stringr")
  #install.packages("plyr")
  # load packages
  require(stringr)
  require(plyr)
  # list files
  corpus.files = list.files(path = path, pattern = NULL, all.files = T, full.names = T, recursive = T, ignore.case = T, include.dirs = T)
  conc <- sapply(corpus.files, function(x) {
    txt <- scan(x, what = "char", sep = "", quiet = T, skipNul = T, quote = "")
      file <- as.vector(unlist(str_replace_all(x, ".*/", "")))
      txt <- paste(txt, collapse = " ")
      txt <- str_replace_all(txt, " {2,}" , " ")
      txt <- str_trim(txt, side = "both")
      lngth <- as.vector(unlist(nchar(txt)))
      idx <- str_locate_all(txt, pattern)
      idx <- idx[[1]]
      idx1 <- as.vector(unlist(idx[,1]))
      idx2 <- as.vector(unlist(idx[,2]))
      token.idx.start <- idx1
      token.idx.end <- idx2
      pre.idx.start <- token.idx.start-context
      pre.idx.end <- token.idx.start-1
      pre.idx.all.start <- as.vector(unlist(rep(1, length(idx1))))
      pre.idx.all.end <- idx1-1
      post.idx.start <- idx2+1
      post.idx.end <- idx2+context
      enddf <- cbind(post.idx.end, c(rep(lngth, length(idx2))))
      end <- apply(enddf, 1, function(x){
        ifelse(x[1] > x[2], x <- x[2], x <- x[1]) } )
      post.idx.end <- as.vector(unlist(end))
      startdf <- cbind(post.idx.start, c(rep(1, length(idx1))))
      start <- apply(startdf, 1, function(x){
        ifelse(x[1] > x[2], x <- x[1], x <- x[2]) } )
      post.idx.start <- as.vector(unlist(start))
      conc.df <- cbind(pre.idx.start, pre.idx.end, token.idx.start, token.idx.end, post.idx.start, post.idx.end, pre.idx.all.start)
      concr1 <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
        pre <- substr(txt, x[1], x[2])
        token <- substr(txt, x[3], x[4])
        post <- substr(txt, x[5], x[6])
        tbc <- as.vector(unlist(c(rep(file, length(pre)), pre, token, post)))
        return (tbc)
        } )
      conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
      return(conc) }
      concr2 <- function(conc.df, txt){
      conc <- apply(conc.df, 1, function(x){
          pre <- substr(txt, x[1], x[2])
          token <- substr(txt, x[3], x[4])
          post <- substr(txt, x[5], x[6])
          all.pre <- substr(txt, x[7], x[2])
          tbc <- as.vector(unlist(c(rep(file, length(pre)), pre, token, post, all.pre)))
          return(tbc)
       } )
       conc <- as.matrix(conc, nrow = length(idx1), byrow = F)
      return(conc)
       }
       ifelse(all.pre == F, conc <- concr1(conc.df, txt), conc <- concr2(conc.df, txt))
      conc <- t(conc)
      return(conc)
      }  )
   df <- ldply(conc, data.frame)
   ifelse(nrow(df) >= 1, df <- df[,2:ncol(df)], df <- df)
   ifelse(ncol(df) == 4, colnames(df) <- c("file", "pre", "token", "post"),
     ifelse(ncol(df) == 5, colnames(df) <- c("file", "pre", "token", "post", "all.pre"),
       cat("Number of columns does not match the number of column names!")))
  df <- df[complete.cases(df),]
  rownames(df) <- 1:nrow(df)
  return(df)
}
###############################################################
### ---                    THE END
###############################################################
# Example
#path <- "C:\\Corpora\\original\\ICE Ireland version 1.2.2\\ICE-Ireland txt\\ICE spoken running txt"
#test <- ConcR(path = path, pattern = "[P|p]lease", context = 30, all.pre = T)
# inspect results
#head(test)




