# Continuous tables and graphs ----
# set.seed(12152018)
# n <- 15
# dat <- sample(80:120, size = n, replace = TRUE)
# datasets: sysbp, diasbp, age, weight, height

# making lower and upper bounds
lnb <- function(dat, cw=NULL, numclass=NULL){
  if(is.null(numclass))
    numclass <- 7
  minimum <- min(dat)
  maximum <- max(dat)
  if(is.null(cw)){
    cw <- ceiling((maximum-minimum)/numclass)
    if(cw==(maximum-minimum)/numclass)
      cw <- cw+1
  }
  lcl <- minimum + 0:(numclass-1)*cw
  ucl <- minimum + 1:numclass*cw - 1
  lcb <- lcl-.5
  ucb <- ucl+.5
  breaks <- c(lcb, ucb[numclass])
  list(numclass=numclass, minimum=minimum, maximum=maximum, cw=cw, lcl=lcl, ucl=ucl, lcb=lcb, ucb=ucb, breaks=breaks)
}

fd <- function(dat, cw=NULL, numclass=NULL){ #freq distribution
  bounds <- lnb(dat, cw=cw, numclass=numclass)
  numclass <- bounds$numclass
  minimum <- bounds$minimum
  maximum <- bounds$maximum
  cw <- bounds$cw
  lcl <- bounds$lcl
  ucl <- bounds$ucl
  lcb <- bounds$lcb
  ucb <- bounds$ucb
  breaks <- bounds$breaks
  freqs <- hist(dat, breaks = breaks, plot=FALSE)$counts
  rel_freqs <- paste0(freqs, "/", sum(freqs))
  sim_rel_freqs <- as.character(MASS::fractions(freqs/sum(freqs))) #or this for simplified fractions
  dec_rel_freqs <- round(freqs/sum(freqs),4)
  percentages <- paste0(round(freqs/sum(freqs), 4)*100, "%")
  cum_freqs <- cumsum(freqs)
  midpoints <- (lcl+ucl)/2
  fd <- data.frame(lcl, ucl, lcb, ucb, freqs, rel_freqs, sim_rel_freqs, dec_rel_freqs, percentages, cum_freqs, midpoints)
  fd
}

# frequency distributions
c2conts_all_text <- function(dat, cw=NULL, numclass=NULL){
  bounds <- lnb(dat, cw=cw, numclass=numclass)
  numclass <- bounds$numclass
  minimum <- bounds$minimum
  maximum <- bounds$maximum
  cw <- bounds$cw
  
  fd <- fd(dat, cw=cw, numclass=numclass)
  
  #class width work
  if(cw-1==(maximum-minimum)/numclass){ #if a whole number
    cww <- paste0("cw = (max-min)/(# classes) = (", maximum, "-", minimum,")/", numclass, " = ", (maximum-minimum)/numclass, ". As this is a whole number, round up to cw = ", cw, ".")
  } else { #otherwise
    cww <- paste0("cw = (max-min)/(# classes) = (", maximum, "-", minimum,")/", numclass, " = ", round((maximum-minimum)/numclass,4), ". Rounding up gives cw = ",cw)
  }
  
  list(`Ordered Data`=sort(dat), `Number of Classes`=numclass, `Class Width`=cww, `All Frequency Distributions` = fd)
}

# frequency histograms
fh <- function(dat, cw=NULL, numclass=NULL){
  bounds <- lnb(dat, cw=cw, numclass=numclass)
  breaks <- bounds$breaks
  p <- hist(dat, breaks = breaks, plot=FALSE)
  plot(p, xaxt="n", xlab="***Description of Data***", main="Frequency Histogram", ylim=c(0, max(p$counts))); grid(nx=NA,ny=NULL)
  axis(side=1, at=breaks, labels=breaks)
}

# relative frequency histogram
rfh <- function(dat, cw=NULL, numclass=NULL){
  bounds <- lnb(dat, cw=cw, numclass=numclass)
  breaks <- bounds$breaks
  p <- hist(dat, breaks = breaks, plot=FALSE)
  p$counts <- p$counts/length(dat)
  plot(p, xaxt="n", xlab="***Description of Data***", ylab = "Relative Frequency", main="Relative Frequency Histogram", ylim=c(0, max(p$counts))); grid(nx=NA,ny=NULL)
  axis(side=1, at=breaks, labels=breaks)
}

# frequency polygon
fp <- function(dat, cw=NULL, numclass=NULL){
  d <- fd(dat, cw=cw, numclass=numclass)
  plot(d$midpoints, d$freqs, type = "b", xlab="***Description of Data***",ylab="Frequency", main="Frequency Polygon", ylim = c(0, max(d$freqs)), xaxt="n"); grid(nx=NA,ny=NULL)
  axis(side=1, at=d$midpoints, labels=d$midpoints)
}

# relative frequency polygon
rfp <- function(dat, cw=NULL, numclass=NULL){
  d <- fd(dat, cw=cw, numclass=numclass)
  plot(d$midpoints, d$dec_rel_freqs, type = "b", xlab="***Description of Data***",ylab="Relative Frequency", main="Relative Frequency Polygon", ylim = c(0, max(d$dec_rel_freqs)), xaxt="n"); grid(nx=NA,ny=NULL)
  axis(side=1, at=d$midpoints, labels=d$midpoints)
}

# ogive: cumulative frequency polygon
ogive <- function(dat, cw=NULL, numclass=NULL){
  d <- fd(dat, cw=cw, numclass=numclass)
  plot(d$midpoints, d$cum_freqs, type = "b", xlab="***Description of Data***",ylab="Frequency", main="Ogive (Cumulative Frequency Polygon)", ylim = c(0, max(d$cum_freqs)), xaxt="n"); grid(nx=NA,ny=NULL)
  axis(side=1, at=d$midpoints, labels=d$midpoints)
}

# stem and leaf plot (just putting the first digit on the left)
slp <- function (X, scale = 1) { #base code from fmsb::gstem, edited to include a title
  .stem.out <- capture.output(aplpack::stem.leaf(X, style = "bare", m = 1, depths = FALSE))
  .stem.len <- length(.stem.out)
  plot(c(1, 2), c(1, .stem.len), type = "n", axes = FALSE, xlab = "", ylab = "", main="Stem and Leaf Plot")
  text(rep(1, .stem.len), .stem.len:1, .stem.out, pos = 4)
}



c2conts_all_plots <- function(dat, cw=NULL, numclass=NULL){
  par(mfrow=c(6,1), cex=1.2)
  fh(dat, cw=cw, numclass=numclass)
  rfh(dat, cw=cw, numclass=numclass)
  fp(dat, cw=cw, numclass=numclass)
  rfp(dat, cw=cw, numclass=numclass)
  ogive(dat, cw=cw, numclass=numclass)
  slp(dat)
  par(mfrow=c(1,1))
}




# Categorical tables and graphs ----
# set.seed(12152018)
# dat <- sample(c("A","B","AB","O"), size = n, replace = TRUE)
# datasets: blood type, fav color, race, position (sports team or business), eye color, hair color

c2cats_all_text <- function(dat){
  freqs <- table(dat)
  rn <- names(freqs)
  names(freqs) <- NULL
  freqs <- as.numeric(freqs)
  rel_freqs <- paste0(freqs, "/", sum(freqs))
  sim_rel_freqs <- as.character(MASS::fractions(freqs/sum(freqs))) #or this for simplified fractions
  dec_rel_freqs <- round(freqs/sum(freqs),4)
  percentages <- paste0(round(freqs/sum(freqs), 4)*100, "%")
  out <- data.frame(freqs, rel_freqs, sim_rel_freqs, dec_rel_freqs, percentages)
  rownames(out) <- rn
  list(`All Frequency Distributions`=out)
}

# table(dat)
# table(dat)/length(dat)
# 
# barplot(table(dat), main="Bar Chart")
# barplot(table(dat), density = 3, main="Bar Chart"); grid(nx=NA,ny=NULL)
# barplot(table(dat), col="white")
# barplot(sort(table(dat), decreasing = TRUE), col=rainbow(length(table(dat))), main="Pareto Chart")
# 
# pie(table(dat), labels = paste0(names(table(dat)), ": ",round(table(dat)/length(dat),4)*100, "%"), main="Pie Chart")

c2cats_all_plots <- function(dat){
  par(mfrow=c(3,1), cex=1.2)
  barplot(table(dat), col="white", main="Bar Chart"); grid(nx=NA,ny=NULL) #density=3 adds slashes
  barplot(sort(table(dat), decreasing = TRUE), col="white", main="Pareto Chart"); grid(nx=NA,ny=NULL)
  pie(table(dat), labels = paste0(names(table(dat)), ": ",round(table(dat)/length(dat),4)*100, "%"), main="Pie Chart")
  par(mfrow=c(1,1))
}

# if numeric, use c2cont, if not, use c2cat.
# c2s <- function(dat, cw=NULL, numclass=NULL){
#   if(is.numeric(dat)){
#     c2cont(dat, cw, numclass)
#   } else {
#     c2cat(dat)
#   }
# }


