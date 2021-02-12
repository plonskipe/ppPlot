#'
#' @title P-P Plot One Continuous Variable
#'
#' @description Function will take in a data frame and return a P-P plot of
#'  the cumulative probability distribution (y) on
#'  the hypothetical cumulative probability distribution (x)
#'
#'  req'd packages:
#'  require(ggplot2) # for plot
#'  require(ggthemes) # for theme
#'
#' @author Paul E. Plonski
#'
#' First Created: 11 February 2021
#' Modified: 12 February 2021
#'
#' @param df
#'
#' df <- data.frame(no NAs in selected variable)
#'
#' @param name.var
#'
#' name.var <- c("selected column name as character string")
#'
#' @return P-P Plot
#'
#' @examples
#' df <- data.frame(rnorm(100)^2)
#' name.var <- c("rnorm.100..2")
#'     # !remove rows with NA if necessary prior to ppPlot
#'     df <- df[!is.na(df$rnorm.100..2),]
#' ppPlot(df, name.var)
#'
#' @export
ppPlot <- function(df, name.var) {
  require(ggplot2) # for plot
  require(ggthemes) # for theme
  n = nrow(df) # number of data points in dist
  p <- (1 : n) / n - 0.5 / n # generate hypothetical probability dist
  pp <- ggplot(df)+ # new plot
    geom_point(aes(x = p, # generated cum prob dist
                   y = sort(pnorm(.data[[name.var]],
                                  mean(.data[[name.var]]),
                                  sd(.data[[name.var]])))))+ # cum prob dist
    labs(x="Hypothetical Probability",
         y="Probability",
         title = name.var)+
    geom_abline()+ # add line
    coord_cartesian(xlim = c(0,1), ylim =c(0,1))+ # set axis range
    theme_tufte(base_size=14, base_family="sans", ticks=FALSE) # set theme
  pp  # P-P plot
}
