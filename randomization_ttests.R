setwd('~/Documents/')
getwd()
set.seed(777)
data <- readRDS('Plants.RDS')

#filter on table
data1 <- data[data$group == 1,]
data2 <- data[data$group == 2,]

#column extraction
data1_wgt <- data1$weight
data2_wgt <- data2$weight

#compute mean
mean1_wgt <- mean(data1_wgt)
mean2_wgt <- mean(data2_wgt)

#compute median
med1_wgt <- median(data1_wgt)
med2_wgt <- median(data2_wgt)

#compute stdev
sd1_wgt <- sd(data1_wgt)
sd2_wgt <- sd(data2_wgt)

#plot cdf
p1 <- ecdf(data1_wgt)
plot(p1)
p2 <- ecdf(data2_wgt)
plot(p2)

#quantile stats:
summary(fivenum(data1_wgt))
summary(fivenum(data2_wgt))

#h0: no diff in growths
#observed diff
x <- abs(mean1_wgt - mean2_wgt)
#ks stat function
g.ks <- function(a, b){ ks.test(a, b)$stat }
#variance ratio stat function
g.vr <- function(a, b){ 
  
  ratio <- sd(a)**2 / sd(b)**2
  y = max(ratio,1/ratio)
  return(y)
}

sample_ks = g.ks(data1_wgt, data2_wgt)
sample_vr = g.vr(data1_wgt, data2_wgt)

#run monte carlo randomizaion with replacement
Gsim <- NULL
for (s in 1:1e4)
{
  x = data$group
  y = data$weight
  xsim <- sample(data$group, replace=TRUE)
  yAsim <- y[xsim==1]
  yBsim <- y[xsim==2]
  g1 <- g.ks(yAsim, yBsim)
  g2 <- g.vr(yAsim, yBsim)
  Gsim <- rbind(Gsim, c(g1,g2))
  if (s%%1e3 == 0) {print(s)}
}

#plot histogram
hist(Gsim[,1],
     col="blue",
     xlab="KS Stat",
     main="Histogram of KS Stat",
     freq=F)

hist(Gsim[,2], 
     col="blue",
     xlab="VR Stat",
     main="Histogram of VR Stat",
     freq=F)

#compute p value
pval_ks = mean(Gsim[,1] >= sample_ks)
pval_vr = mean(Gsim[,2] >= sample_vr)

#demonstration of exact randomization test for y and x
y <- c(256, 159, 149, 54, 123, 248)
x <- c('R', 'R', 'R', 'N', 'N', 'N')
g1.obs <- abs( mean(y[x=="R" ]) - mean(y[x=="N" ]) )
g2.obs <- abs( t.test(y[x=="R"], y[x=="N"], var.equal=TRUE)$stat )

#create all posible combinations for exact test
n.combs <- choose(6,3)
all.combs <- combn(6,3)
dim(all.combs)
g1 <- numeric(n.combs)
g2 <- numeric(n.combs)
for (i in 1:n.combs){
  R.indices <- all.combs[,i]
  g1[i] <- abs( mean(y[R.indices]) - mean(y[-R.indices]) )
  g2[i] <- abs( t.test(y[R.indices], y[-R.indices], var.equal=TRUE)$stat )
  }
pval_mean = mean(g1>=g1.obs)
pval_ttest = mean(g2>=g2.obs)
