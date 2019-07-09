# bg colors for strips
bgColors<-c("white","light grey","white")


# Alpha0 for each Similarity measure
my.strip5a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")),
            expression(paste(delta, " = 0.04")), expression(paste(delta, " = 0.06")),
            expression(paste(delta, " = 0.08")), expression(paste(delta, " = 0.10"))
            ) #levels for your first factor (x1)

  varnames<-expression("Similarity: ")
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, 
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}

# Power for each Similarity measure, along with a0=1,0.5
my.strip6a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- #if (which.given == 1)  
    c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
#  else if (which.given == 2) c(expression(paste(delta, " Weibull")), expression(paste(delta, " identity")) ) #levels for your first factor (x1)
#  else c("25", "100")
  
  varnames<-c(expression(""))#, expression("Similarity: "), 
  #            expression(paste("Sample Size: ")) )

  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}


# Power for both stochastic Similarity measures, along with a0=1,0.5
my.strip6b <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- if (which.given == 1)  
    c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
    else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided"))) #levels for your first factor (x1)
  #  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "))#, 
  #            expression(paste("Sample Size: ")) )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}


# Power for both delta region Similarity measures, along with a0=1,0.5
my.strip6c <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- if (which.given == 1)  
    c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste(delta," = 0.04")), expression(paste(delta, " = 0.08")) ) #levels for your first factor (x1)
  #  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "))#, 
  #            expression(paste("Sample Size: ")) )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}

# Power for all delta region Similarity measures, along with a0=1,0.5
my.strip6d <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- if (which.given == 1)  
    c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste(delta," = 0.04")), expression(paste(delta, " = 0.06")), 
                               expression(paste(delta, " = 0.08")), expression(paste(delta, " = 0.10")) ) #levels for your first factor (x1)
  #  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "))#, 
  #            expression(paste("Sample Size: ")) )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}



# Bias for stacked strips
my.strip9a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste(delta," = 0.04")), expression(paste(delta, " = 0.06")),
                               expression(paste(delta," = 0.08")), expression(paste(delta, " = 0.10"))) #levels for your first factor (x1)
  text.size<-if(which.given==1) list(cex=.7) else if(which.given==2) list(cex=.7)
  
  varnames<-c(expression(""), expression("Similarity: ") )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[3-which.given],
                par.strip.text = text.size
  )
}

# Bias for both delta region Similarity measures, along with a0=1,0.5
my.strip9c <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- if (which.given == 1)  
    c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste(delta," = 0.04")), expression(paste(delta, " = 0.10")) ) #levels for your first factor (x1)
  #  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "))#, 
  #            expression(paste("Sample Size: ")) )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.8)
  )
  
}