#' Summary statistics
#'
#' Calculates mean, standard deviation and 95% confidence intervals
#' @param x input data
#' @param measurevar variable to be measured
#' @param groupvars list of sample sites
#' @param na.rm na values
#' @param conf.interval confidence interval
#' @param .drop something which is true
#' @return Summary statistics
#' @import
#' plyr
#' @note this function is not directly called, it is required in calculating monthly threshold values e.g. TSS
#' @references
#' http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
#' @author Winston Chang \email{winston@stdout.org}

#' @export

##utils::globalVariables(c("sd", "qt"))
Summary_stats <- function(x, measurevar, groupvars=NULL, na.rm=FALSE,
                          conf.interval=.95, .drop=TRUE) {

  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(x, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm),
                           min  = min	   (xx[[col]], na.rm=na.rm),
                           max  = max	   (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

##utils::globalVariables(c("sd", "qt"))
