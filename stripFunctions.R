
## Borrowing amounts

# Tester for showing all similarity measures (factors) on one figure (Figure 3)
my.strip4a <- function(which.given, ..., factor.levels,var.name) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("Stochastic Ordering 1")),expression(paste("Stochastic Ordering 2")),
         expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": "))
  )
}

# Figure 3a
my.strip5a <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                par.strip.text = list(cex=.75)
  )
}

# Figure 3b
my.strip5 <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("Stoch Ord 1-sided")), expression(paste("Stoc Ord 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                par.strip.text = list(cex=.75)
  )
}

## Type I error rate (Fig 4) and Power (Fig 5)

# Figure 4a and 5a (KS measures)
my.strip6a <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = list(cex=.75)
  )
}

# Figure 4b and 5b (SO measures)
my.strip6b <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ) #levels for your first factor (x1)
  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = list(cex=.75)
  )
}


## SD Comparison (Figure 7a and b)

my.strip6aaa <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1)  c("no", "yes") #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  #  else c("100")
  text.size<-if(which.given==1) list(cex=.75) else if(which.given==2) list(cex=.75)
  
  varnames<-c(expression(paste("Use ", D[1], "twice")), expression("Similarity"), expression(paste("Sample Size")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                par.strip.text = text.size
  )
}

## Bias (Figure 6a and b)

my.strip9a <- function(which.given, ..., factor.levels,var.name,par.strip.text) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  #  else c("100")
  text.size<-if(which.given==1) list(cex=.75) else if(which.given==2) list(cex=.75)
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = text.size
  )
}

