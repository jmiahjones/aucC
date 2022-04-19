#' Quick computation of the AUC
#' 
#' Scores represent a prediction of how likely a label corresponds to a targeted
#' value. The targeted value defaults to "Yes", but these may be easily changed
#' by passing in an appropriate character vector of levels, where the targeted
#' category is the second level.
#' 
#' @param label A factor vector with levels of c("No","Yes") in any order, or a character vector taking the same values.
#' @param scores A numeric vector of the same length above. Higher scores must correspond to the "Yes" level of the labels.
#' @param levels A character vector of length at least two, corresponding to the unique values possible in \code{label}.
#' The targeted level is the one presented in the second position: e.g. \code{levels = c("No","Yes")}
#' corresponds to a targeted level of "Yes".
#' @return The AUC, defined as the area under the ROC curve computed using \code{label} and \code{scores}.
#' @examples 
#' \dontrun{
#' y=rbinom(100, size=1, prob=1/2)
#' label = factor(y, levels=c(0,1), labels=c("No", "Yes"))
#' scores = runif(100) # higher scores predict label == "Yes"
#' aucC::aucC_wrap(label=label, scores=scores)
#' 
#' label2 = factor(y, levels=c(0,1), labels=c("Apple", "Orange"))
#' aucC_wrap(label=label, scores=scores, labels=c("Apple", "Orange"))
#' }
#' @export
aucC_wrap <- function(label, scores, levels = c("No", "Yes")){
  pos_predictor = scores[label == levels[2]]
  neg_predictor = scores[label != levels[2]]
  aucC(pos_predictor, neg_predictor)
}