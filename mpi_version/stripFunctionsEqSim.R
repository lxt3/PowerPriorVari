
# bg colors for strips
bgColors<-c("white","light grey","white")

## Borrowing amounts

# Tester for showing all similarity measures (factors) on one figure (Figure 3)
my.strip4a <- function(which.given, ..., factor.levels,var.name) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("Stochastic Ordering 1")),expression(paste("Stochastic Ordering 2")),
         expression(paste("EQ 1-sided")), expression(paste("EQ 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": "))
  )
}

# Old Figure 3a
my.strip5a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste(delta," Weibull")), expression(paste(delta," identity")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[3-which.given],
                par.strip.text = list(cex=.75)
  )
}

# Old Figure 3b
my.strip5 <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("Stoch Ord 1-sided")), expression(paste("Stoc Ord 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[3-which.given],
                par.strip.text = list(cex=.75)
  )
}


# Figure 3

my.strip5a.top <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- c(expression(paste(delta," Weibull")), expression(paste("Stoch Ord 1-sided")), expression(paste("Stoc Ord 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression("Similarity"))
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[1],
                par.strip.text = list(cex=.75)
  )
}

my.strip5a.left <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <-  c("25","50", "100") #levels for your second factor (x2)

  varnames<-c( expression(paste("Sample Size")))
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[1],
                par.strip.text = list(cex=.75)
  )
}



# Slide for MDIC workshop
my.strip5a.both <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- c(expression(paste("Stoch Ord 1-sided")), expression(paste("Stoc Ord 2-sided")),
    expression(paste(delta,"= 0.2 (Weibull)")), expression(paste(delta,"= 0.2 (identity)"))
         ) #levels for your first factor (x1)
  varnames<-c( expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[1],
                par.strip.text = list(cex=.9)
  )
}


## Type I error rate (Fig 4) and Power (Fig 5)

# Figure 4a and 5a (EQ measures)

# version for 3 conditioning factors (all stacked within each panel)
my.strip6a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste(delta, " Weibull")), expression(paste(delta, " identity")) ) #levels for your first factor (x1)
  else c("25", "100")

  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
#  if(which.given>1) return(FALSE)  # for two conditioning factors (with 3rd on left using strip.left)
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, #[c(1,3)], # c(1,3) is for two conditioning
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.7)
  )
  
}

# next two are versions for 2 conditioning factors with useOuterStrips from latticeExtra pkg
my.strip6a.top <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs <-  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)

  varnames<-c(expression("") )

  strip.default(which.given, ..., factor.levels = levs, var.name=varnames, 
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=TRUE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.7)
  )
  
}
my.strip6a.left <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg,horizontal) {
  levs<-c("25", "100")

  varnames<-c(expression(paste("Sample Size: ")) )
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,  
                strip.names = c(TRUE),strip.levels=c(T),
                horizontal=FALSE,
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.7)
  )
  
}

# Figure 4b and 5b (SO measures)

# version for 3 conditioning factors (all stacked within each panel). but can be used without last factor
my.strip6b <- function(which.given, which.panel,..., factor.levels,var.name,par.strip.text, bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ) #levels for your first factor (x1)
  else c("25", "100")

#  if(which.given > 2) return(FALSE)
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) 
              )
  strip.default(which.given, which.panel,..., factor.levels = levs, var.name=varnames, # for 2 conditioning use [c(1,2)]
                strip.names = c(TRUE,TRUE,FALSE)[which.given],strip.levels=c(T,T,F)[which.given],
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.7)
  )
}



# Figure for MDIC workshop
my.strip6aboth <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")),
    expression(paste(delta, " Weibull")), expression(paste(delta, " identity")) ) #levels for your first factor (x1)

  varnames<-c(expression(""), expression("Similarity: "))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.9)
  )
}

my.strip6aboth2 <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <-  c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")),
         expression(paste(delta, "= 0.2 (Weibull)")), expression(paste(delta, "= 0.2 (identity)")) ) #levels for your first factor (x1)
  
  varnames<-c(expression("Similarity: "))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[which.given],
                par.strip.text = list(cex=.9)
  )
}

## SD Comparison (Figure 7a and b)

my.strip6aaa <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c("no", "yes") #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste(delta," Weibull")), expression(paste(delta, " Identity")) ) #levels for your first factor (x1)

  text.size<-if(which.given==1) list(cex=.7) else if(which.given==2) list(cex=.7)
  
  varnames<-c(expression(paste("Use ", D[1], " twice")), expression("Similarity"), expression(paste("Sample Size")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                bg = bgColors[3-which.given],
                par.strip.text = text.size
  )
}

## Bias (Figure 6a and b)

# for stacked strips
my.strip9a <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste(delta," Weibull")), expression(paste(delta, " Identity")) ) #levels for your first factor (x1)
  text.size<-if(which.given==1) list(cex=.7) else if(which.given==2) list(cex=.7)
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[3-which.given],
                par.strip.text = text.size
  )
}

# for outer strips

my.strip9a.top <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- c(expression(paste(delta," Weibull")),
            expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) 
                               
            )#, expression(paste(delta, " Identity")) ) #levels for your first factor (x1)
  text.size<- list(cex=.7) 
  
  varnames<-c( expression("Similarity: ") )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[1],
                par.strip.text = text.size
  )
}

my.strip9a.left <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <-  c(expression(paste("Discard ", D[1])), expression(paste("Keep ", D[1]))) #levels for your second factor (x2)

    text.size<-list(cex=.7) 
  
  varnames<-c(expression(""))
  
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                bg = bgColors[1],
                par.strip.text = text.size
  )
}

## Appendix A

# Figure A1 (EQ measures)
my.stripA1 <- function(which.given, ..., factor.levels,var.name,par.strip.text, bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else c("25", "100")
  
  varnames<-c(expression(""),  
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = list(cex=.8),
                bg=bgColors[3-which.given]
  )
}


## Appendix B

# Figure B2

my.stripB2 <- function(which.given, ..., factor.levels,var.name,par.strip.text, bg) {
  levs <- if (which.given == 1) c("25","50", "100") #levels for your second factor (x2)
  else c(expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  varnames<-c( expression(paste("Sample Size")), expression("Similarity"))
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste(": ")),
                par.strip.text = list(cex=.75),
                bg=bgColors[3-which.given]
  )
}


# Figure B4
my.stripB4 <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  else c("25", "100")
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = list(cex=.75),
                bg=bgColors[which.given]
  )
}
  
# Figure B5

my.stripB5 <- function(which.given, ..., factor.levels,var.name,par.strip.text,bg) {
  levs <- if (which.given == 1)  c(expression(paste("Keep ", D[1])), expression(paste("Discard ", D[1]))) #levels for your second factor (x2)
  else if (which.given == 2) c(expression(paste("SO 1-sided")), expression(paste("SO 2-sided")) ,
                               expression(paste("KS 1-sided")), expression(paste("KS 2-sided")) ) #levels for your first factor (x1)
  
  text.size<-if(which.given==1) list(cex=.75) else if(which.given==2) list(cex=.75)
  
  varnames<-c(expression(""), expression("Similarity: "), 
              expression(paste("Sample Size: ")) )
  strip.default(which.given, ..., factor.levels = levs, var.name=varnames,
                strip.names = c(TRUE),strip.levels=c(T),
                sep=expression(paste("")),
                par.strip.text = text.size,
                bg=bgColors[3-which.given]
  )
}


