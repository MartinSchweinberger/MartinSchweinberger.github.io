###########################################################################
### --- Function for Customized Multiple Linear Regression Results
###########################################################################
##################################################################
### --- R-Skript "Function for Customized Multiple Linear Regression Results"
### --- Author: Martin Schweinberger (June 18th, 2014)
### --- R-Version: R version 3.0.1 (2013-05-16) -- "Good Sport"
### --- This R script retrieves relevant information from regression outputs of
### --- Multiple Linear Regression.
### --- NOTE
### --- This script only works for Multiple Linear Regressions.
### --- The function takes three arguments: x = a lrm object, a = a glm object, 
### --- and dpvar = a vector representing the depentent variable with numeric values of either 0 or 1.
### --- CONTACT
### --- If you have questions,suggestions or you found errors
### --- or in case you would to provide feedback, questions
### --- write an email to
### --- martin.schweinberger.hh@gmail.com
### --- CITATION
### --- If you use this script or results thereof, please cite it as:
### --- Schweinberger, Martin. 2014. "Function for Customized
### --- Multiple Linear Regression Results ", unpublished R-skript,
### --- Hamburg University.
###############################################################
###                   START
###############################################################

mlr.summary <- function(lrm, glm, dpvar) {
p.nice <- function(z) {
  as.vector(unlist(sapply(z, function(w) {
    ifelse(w < .001, return("p < .001***"),
    ifelse(w < .01, return("p < .01**"),
    ifelse(w < .05, return("p < .05*"), return(round(w, 4))))) } ))) }
cilwr <- exp(confint.default(glm))[, 1]
ciupr <- exp(confint.default(glm))[, 2]
  coefs <- summary(glm)[[12]]
  coef.df <- data.frame(
    round(coefs[, 1], 2),
    c("", round(vif(lrm), 2)),
    round(exp(coefs[, 1]), 2),
    round(cilwr, 2),
    round(ciupr, 2),
    round(coefs[, 2], 2),
    round(coefs[, 3], 2),
    round(coefs[, 4], 5),
    p.nice(coefs[, 4]))
  colnames(coef.df) <- c(colnames(coefs)[1],
    "VIF",
    "OddsRatio", "CI(2.5%)", "CI(97.5%)",
    colnames(coefs)[2],
    colnames(coefs)[3],
    colnames(coefs)[4],
    "Significance")
    
  logisticPseudoR2s <- function(LogModel) {
    dev <- LogModel$deviance
    nullDev <- LogModel$null.deviance
    modelN <-  length(LogModel$fitted.values)
    R.l <-  1 -  dev / nullDev
    R.cs <- 1- exp ( -(nullDev - dev) / modelN)
    R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
    list(c("R2 (Hosmer & Lemeshow)", round(R.l, 3)),
      c("R2 (Cox & Snell)", round(R.cs, 3)),
      c("R2 (Nagelkerke)", round(R.n, 3))) 
    }
  
  mdl.statz <- c("", "", "", "","", "", "", "", "Value")
  nbcases <- c("", "", "", "","", "", "", "", length(fitted(glm)))
  obs0 <- c("", "", "", "", "", "", "", "", sum(dpvar == 0))
  obs1  <- c("", "", "", "", "", "", "", "", sum(dpvar == 1))
  nd <- c("", "", "", "", "","", "", "", round(summary(glm)[[8]], 2))
  rd <- c("", "", "", "", "","", "", "", round(summary(glm)[[4]], 2))
  R.l <- c("", "", "", "", "","", "", "", logisticPseudoR2s(glm)[[3]][[2]])
  R.cs <- c("", "","", "", "", "", "", "", logisticPseudoR2s(glm)[[2]][[2]])
  R.n <- c("", "","", "", "", "", "", "", logisticPseudoR2s(glm)[[1]][[2]])    
  C <- c("", "", "","", "", "", "", "", round(lrm[[3]][[6]], 3))
  Dxy <- c("", "", "", "", "","", "", "", round(lrm[[3]][[7]], 3))
  AIC <- c("", "", "", "", "","", "", "", round(summary(glm)[[5]], 2))
  
  dpvarneg <- sapply(dpvar, function(x) ifelse(x == 1, 0, 1))
  correct <- sum(dpvar * (predict(glm, type = "response") >= 0.5)) + sum(dpvarneg * (predict(glm, type="response") < 0.5))
  tot <- length(dpvar)
  predict.acc <- (correct/tot)*100 

  Accuracy <- c("", "", "", "", "", "", "", "", paste(round(predict.acc, 2), "%", sep = "", collapse = ""))
  ModelLR <- c("", "", "", "","", paste("L.R. X2:", round(lrm[[3]][[3]], 2)), 
    paste("DF:", round(lrm[[3]][[4]], 0)), paste("p-value:", round(lrm[[3]][[5]], 5)),
    p.nice(round(lrm[[3]][[5]])))
  gblstz.tb <- rbind(mdl.statz, nbcases, obs0, obs1, nd, rd, R.l, R.cs, R.n, C, Dxy, AIC, Accuracy, ModelLR)
  gblstz.df <- as.data.frame(gblstz.tb)
  colnames(gblstz.df) <- colnames(coef.df)
  mlrm.tb <- rbind(coef.df, gblstz.df)
  rownames(mlrm.tb) <- c(rownames(coefs),
    "Model statistics", "Number of cases in model", "Observed misses", "Observed successes", 
    paste("Null deviance", paste("on", summary(glm)[[9]],"DF")),
    paste("Residual deviance", paste("on", summary(glm)[[7]],"DF")),
    "R2 (Nagelkerke)", "R2 (Hosmer & Lemeshow)", "R2 (Cox & Snell)",  "C", "Somers' Dxy", "AIC",
    "Prediction accuracy", "Model Likelihood Ratio Test")
  mlrm.df <- as.data.frame(mlrm.tb)
return(mlrm.df)
}
