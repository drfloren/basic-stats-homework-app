# set.seed(12152018)
# n <- 15
# dat <- sample(80:120, size = n, replace = TRUE)

# central tendancy ----
md <- function(dat){ #mode function
  x <- table(dat)
  as.numeric(names(x)[which(x==max(x))])
}

mct <- function(dat){ #measures of central tendancy
  list(mean=mean(dat), median=median(dat), mode=md(dat), midrange=diff(range(dat))/2)
}

# measures of variation ----
mv <- function(dat){
  pv <- round(sum((dat-mean(dat))^2/length(dat)),4)
  psd <- round(sqrt(pv),4)
  sv <- round(sum((dat-mean(dat))^2/(length(dat)-1)),4)
  ssd <- round(sqrt(sv),4)
  list(range=diff(range(dat)), pop_var=pv, pop_sd=psd, samp_var=sv, samp_sd=ssd)
}

# percentiles, quartiles, deciles ----
library(toOrdinal)
pw <- function(n, p){ #percentile work
  if(n*p/100==ceiling(n*p/100)){ #if a whole number
    paste0("c = (np)/100 = (", n, "*", p,")/100 = ", n*p/100, ". As this is a whole number, use the mean of the ", toOrdinal(n*p/100), " and ", toOrdinal(n*p/100+1), " numbers in the ordered dataset.")
  } else { #otherwise
    paste0("c = (np)/100 = (", n, "*", p,")/100 = ", n*p/100, ". Rounding up gives the ", toOrdinal(ceiling(n*p/100)), " position in the ordered dataseet.")
  }
}

pqd <- function(dat){
  n <- length(dat)
  d1 <- paste0("d1: ", pw(n, 10), " d1 = ", quantile(dat, probs=.1, type=2))
  d2 <- paste0("d2: ", pw(n, 20), " d2 = ", quantile(dat, probs=.2, type=2))
  d3 <- paste0("d3: ", pw(n, 30), " d3 = ", quantile(dat, probs=.3, type=2))
  d4 <- paste0("d4: ", pw(n, 40), " d4 = ", quantile(dat, probs=.4, type=2))
  d5 <- paste0("d5: ", pw(n, 50), " d5 = ", quantile(dat, probs=.5, type=2))
  d6 <- paste0("d6: ", pw(n, 60), " d6 = ", quantile(dat, probs=.6, type=2))
  d7 <- paste0("d7: ", pw(n, 70), " d7 = ", quantile(dat, probs=.7, type=2))
  d8 <- paste0("d8: ", pw(n, 80), " d8 = ", quantile(dat, probs=.8, type=2))
  d9 <- paste0("d9: ", pw(n, 90), " d9 = ", quantile(dat, probs=.9, type=2))
  q1 <- paste0("q1: ", pw(n, 25), " q1 = ", quantile(dat, probs=.25, type=2))
  q2 <- paste0("q2: ", pw(n, 50), " q2 = ", quantile(dat, probs=.5, type=2))
  q3 <- paste0("q3: ", pw(n, 75), " q3 = ", quantile(dat, probs=.75, type=2))
  iqr <- quantile(dat, probs=.75, type=2)-quantile(dat, probs=.25, type=2); names(iqr) <- NULL
  outs <- dat[which(dat < quantile(dat, probs=.25, type=2) - 1.5*iqr | dat > quantile(dat, probs=.75, type=2) + 1.5*iqr)]
  out_range <- paste0("Anything less than Q1 - 1.5*IQR = ", quantile(dat, probs=.25, type=2) - 1.5*iqr, " or greater than Q3 + 1.5*IQR = ", quantile(dat, probs=.75, type=2)+1.5*iqr, " is an outlier. In this case, ", ifelse(length(outs)==0, "there aren't any outliers.", paste0(paste(outs, collapse=", "), " are outliers.")))
  list(d1=d1,d2=d2,d3=d3,d4=d4,d5=d5,d6=d6,d7=d7,d8=d8,d9=d9, q1=q1, q2=q2, q3=q3, iqr=iqr, outliers=out_range)
}

c3s_text <- function(dat){
  list(`Ordered Dataset`=sort(dat),
       `Measures of Central Tendancy`=mct(dat), 
       `Measures of Variation`=mv(dat), 
       `Deciles and Quartiles`=pqd(dat))
}



# boxplot ----
library(qboxplot)
#qboxplot(dat~dat, horizontal = TRUE, range=0) #without outliers
#qboxplot(dat~dat, horizontal = TRUE) #with outliers

c3s_plot <- function(dat, outliers=FALSE){
  pqd <- list(q1=quantile(dat, probs=.25, type=2),
              q2=quantile(dat, probs=.5, type=2),
              q3=quantile(dat, probs=.75, type=2))
  qdat <- data.frame(dat=dat)
  if(outliers){
    qboxplot(dat~dat, horizontal = TRUE, range=0, main="Boxplot with Outliers Plotted", qtype=2, data=qdat)
    #boxplot(dat, horizontal = TRUE)
    # axis(side=1, at=min(dat), label = paste0("Min:\n",min(dat)), mgp=c(0,-4.5,0))
    # axis(side=1, at=pqd$q1, label = paste0("Q1:\n", pqd$q1), mgp=c(0,-4.5,0))
    # axis(side=1, at=pqd$q2, label = paste0("Q2:\n", pqd$q2), mgp=c(0,-4.5,0))
    # axis(side=1, at=pqd$q3, label = paste0("Q3:\n", pqd$q3), mgp=c(0,-4.5,0))
    # axis(side=1, at=max(dat), label = paste0("Max:\n", max(dat)), mgp=c(0,-4.5,0))
  } else{
    qboxplot(dat~dat, horizontal = TRUE, range=0, main="Boxplot with Outliers Not Plotted", qtype=2, data=qdat)
    #boxplot(dat, horizontal = TRUE)
    axis(side=1, at=min(dat), label = paste0("Min:\n",min(dat)), mgp=c(0,-4.5,0), tick = FALSE)
    axis(side=1, at=pqd$q1, label = paste0("Q1:\n", pqd$q1), mgp=c(0,-4.5,0), tick = FALSE)
    axis(side=1, at=pqd$q2, label = paste0("Q2:\n", pqd$q2), mgp=c(0,-4.5,0), tick = FALSE)
    axis(side=1, at=pqd$q3, label = paste0("Q3:\n", pqd$q3), mgp=c(0,-4.5,0), tick = FALSE)
    axis(side=1, at=max(dat), label = paste0("Max:\n", max(dat)), mgp=c(0,-4.5,0), tick = FALSE)
  }
}
