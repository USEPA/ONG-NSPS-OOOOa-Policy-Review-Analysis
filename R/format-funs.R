# format-funs.R

#' Format a numeric vector as currency
#' 
#' @export
currency <- function(x, digits = 2) {
  signif(x, digits = digits) %>% {
    scales::dollar_format()(.)
  } %>%
    str_replace("\\$-", "-$")
}

#' Format a numeric vector as millions of dollars
#' 
#' @export
Mdollar <- function(x, digits = 2) {
  signif(x / 10^6, digits = digits) %>% {
    scales::dollar_format()(.)
  } %>%
    format(justify = "right") %>%
    str_replace("\\$-", "-$") #%>%
    #str_c(" M")
}

#' Format a numeric vector rounded to sigfigs and apply bigmark
#' 
#' @export
rndmark <- function(x, digits) {
  signif(x, digits) %>%
    format(big.mark = ",")
}