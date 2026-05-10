################################################################
### --- R skript "Two-Level CFA (configuration frequency analyses) with R"
### --- Author: Martin Schweinberger (03/2015)
### --- The script is based on:
### --- a) Funke, Stefan. 9/2013. Package "cfa".
### --- (<http://cran.r-project.org/web/packages/cfa/cfa.pdf>)
### --- b) Bortz, Juergen, Gustav A. Lienert & Klaus Boehnke. 32008.
### --- Verteilungsfreie Methoden in der Biostatistik, 168-170.
### --- Heidelberg: Springer.
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This script performs an Analysis of configuration frequencies which is
### --- applied when testing if there is a significant difference
### --- between frequencies of configurations.
### --- This script is made available under the GNU General Public License
### --- <http://www.gnu.org/licenses/gpl.html>.
### --- CONTACT
### --- If you have questions, suggestions or you found errors
### --- or in case you would to provide feedback or have questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, PLEASE QUOTE it as:
### --- Schweinberger, Martin. 2015. "Two-Level CFA (configuration frequency analyses) with R". Unpublished R skript.
### --- University of Hamburg.
### --- THANK YOU. Copyright Martin Schweinberger (2015).
################################################################
###############################################################
###                   START
###############################################################
################################################################
### --- Write function to perform CFA on two-level configurations
################################################################
# We write a function which takes as it argument a data frame in which
# the last column holds the counts and the first two columns hold
# the configuartions
TwoLevelCFA <- function(data) {
# split data frame into configurations
  cnts <- data[, ncol(data)]
  cfg <- data[, 1:(ncol(data)-1)]
# First, we are using Funke's (2009) function and perform a cfa
  raw.cfa <- cfa(cfg = cfg, cnts = cnts)
# Next, we are determining the critical chi-squared values for
# alpha = .05, .01 and .001 BUT we are taking into account that we
# are performing multiply tests and so we are applying
# Bonferroni's correction (corrected alpha = uncorrected alpha / number of tests).
  crit.05 <- round(rep(qchisq((0.05/12), 1, lower.tail = F), nrow(data)), 4)
  crit.01 <- round(rep(qchisq((0.01/12), 1, lower.tail = F), nrow(data)), 4)
  crit.001 <- round(rep(qchisq((0.001/12), 1, lower.tail = F), nrow(data)), 4)
# We now determine the level of significance for each configuration
  sig <- as.vector(unlist(sapply(raw.cfa[[1]][, 5], function(x) {
    ifelse(x < qchisq((0.05/nrow(data)), 1, lower.tail = F), "n.s.",
    ifelse(x >= qchisq((0.001/nrow(data)), 1, lower.tail = F), "p < .001 ***",
    ifelse(x >= qchisq((0.01/nrow(data)), 1, lower.tail = F), "p < .01 **",
    ifelse(x >= qchisq((0.05/nrow(data)), 1, lower.tail = F), "p < .05 *")))) } )))
# We will now extract the columns which are of interest to us.
  new.cfa <- cbind(
    as.character(raw.cfa[[1]][, 1]),
    raw.cfa[[1]][, 2],
    round(raw.cfa[[1]][, 3], 4),
    round(raw.cfa[[1]][, 5], 4),
    crit.05, crit.01, crit.001, sig)
# We now determine the level of significance for each configuration
  type <- as.vector(unlist(apply(new.cfa, 1, function(x) {
    ifelse(x[8] == "n.s.", "n.a.",
    ifelse(x[2] < x[3], "type", "anti-type")) } )))
# Calculate an approximate effect size (phi)
  eff <- as.vector(unlist(apply(new.cfa, 1, function(x) {
    sum.obs <- sum(as.numeric(new.cfa[, 2]))
    x <- round(sqrt(as.numeric(x[4])/sum.obs), 4) } )))
# Add type vector to our data table
  cfa.rslt <- data.frame(new.cfa, type, eff)
  colnames(cfa.rslt) <- c("configuration", "obs.freq", "exp.freq", "chi.squared", "crit.x2 (.05)", "crit.x2 (.01)", "crit.x2 (.001)", "significance", "type vs. anti-type", "effect.size (phi)")
# return results
  return(cfa.rslt)
  }

###############################################################
###                   EXAMPLE
###############################################################
# create data
#corpus <- c(rep("PCEEC", 3), rep("CED", 3), rep("CLMETEV", 3), rep("BNC", 3))
#form <- c(rep(c("how+PNP", "How+ADJ", "What a"), 4))
#counts <- c(1, 29, 9, 12, 41, 29, 700, 1031, 928, 305, 731, 568)
#mydata <- as.data.frame(matrix(cbind(corpus, form), ncol = 2))
#mydata[, 3] <- as.numeric(counts)
#colnames(mydata) <- c("corpus","form","counts")
# perform Two-Level CFA
# TwoLevelCFA(mydata)