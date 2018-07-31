my.strip <- function(which.given, ..., factor.levels,var.name) {
  levs <- if (which.given == 1) c("no","yes") #levels for your second factor (x2)
  else c(expression(paste("D and Weibull loss")), expression(paste(theta, " and Identity loss")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Discard ", D[1])), expression("KS with"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": "))
  )
}
