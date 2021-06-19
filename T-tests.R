#dataset
niftyData <- read.csv("D:/sem 6/Data Analytics/banknifty.csv")

# hist(niftyData$index)
hist(niftyData$date)
# hist(niftyData$time)
hist(niftyData$open)
hist(niftyData$high)
hist(niftyData$low)
hist(niftyData$close)

nrow(niftyData)
ncol(niftyData)

length(unique(niftyData$index))
length(unique(niftyData$date))
length(unique(niftyData$time))
length(unique(niftyData$open))
length(unique(niftyData$high))
length(unique(niftyData$low))
length(unique(niftyData$close))

niftySam1 <- niftyData[sample(nrow(niftyData), 200), 2:7]
nrow(niftySam1)
ncol(niftySam1)

niftySam2 <- niftyData[sample(nrow(niftyData), 200), 2:7]
nrow(niftySam2)
ncol(niftySam2)

sum(niftySam1 == niftySam2)

# update r first
# install.packages("arsenal")
# library(arsenal)


mean(niftyData$close)
sd(niftyData$close)
summary(niftyData$close)

# mean(niftySam1$open)
# mean(niftySam1$close)
# mean(niftySam1$high)
# mean(niftySam1$low)

# mean(niftySam2$open)
# mean(niftySam2$close)
# mean(niftySam2$high)
# mean(niftySam2$low)

m1 <- mean(niftySam1$close)
s1 <- sd(niftySam1$close)
summary(niftySam1$close)

m2 <- mean(niftySam2$close)
s2 <- sd(niftySam2$close)
summary((niftySam2$close))

#PART 1

#using inbuilt function

#one sample t test
onet <- t.test(StockSam1$DAX, mu = mean(df$DAX))
onet
if(onet$p.value < 0.05) #  p < alpha => reject
{
  print("Null Hypothesis rejected.")
}else
{
  print("Null Hypothesis accepted.")
}


wilcox.test(niftySam1$close, mu = mean(niftyData$close))
wilcox.test(niftySam2$close)

#two sample t test
twot <- t.test(StockSam1$DAX, StockSam2$DAX)
twot
if(twot$p.value < 0.05) #  p < alpha => reject
{
  print("Null Hypothesis rejected.")
}else
{
  print("Null Hypothesis accepted.")
}

#paired t test
pairt <- t.test(StockSam1$DAX, StockSam2$DAX, paired = TRUE)
pairt
if(pairt$p.value < 0.05) #  p < alpha => reject
{
  print("Null Hypothesis rejected.")
}else
{
  print("Null Hypothesis accepted.")
}


# PART 2  

# Self designed functions

#one sample t test
oneSampleTest <- function(df, mu)
{
  m <- mean(df)
  s <- sd(df)
  n <- length(df)
  cat("\nMean of population : ", mu)
  cat("\nMean of sample : ", m)
  t_calc <- (m - mu) / (s / sqrt(n))
  cat("\nValue of t_calc : ", t_calc)
  
  alpha <- 0.05
  dof <- n-1
  t_tab <- abs(qt(alpha, dof))
  cat("\nValue of t_tab : ", t_tab)
  
  if(t_tab > t_calc)
  {cat("\nNull Hypothesis accepted.")}else
  {cat("\nNull Hypothesis rejected.")}
}

oneSampleTest(StockSam1$DAX, mean(df$DAX))

#two sample t test

twoSampleTest <- function(df1, df2)
{
  m1 <- mean(df1)
  m2 <- mean(df2)
  n1 <- length(df1)
  n2 <- length(df2)
  s1 <- sd(df1)
  s2 <- sd(df2)
  t_calc <- ((m1 - m2) / sqrt((s1^2 / n1) + (s2^2 / n2)))
  
  alpha <- 0.05
  dof <- n1 + n2 - 2
  t_tab <- abs(qt(alpha, dof))
  cat('\nmean of sample 1 : ', m1)
  cat('\nmean of sample 2 : ', m2)
  cat("\nValue of t_calc : ", t_calc)
  cat("\nValue of t_tab : ", t_tab)
  
  if(t_calc>t_tab)
  {cat("\n\nNull hypothesis rejected.")}else
      {cat("\n\nNull Hypothesis accepted.")}
}

twoSampleTest(StockSam1$DAX, StockSam2$DAX)

#paired t test
pairedTest <- function(df1, df2)
{
  m1 <- mean(df1)
  m2 <- mean(df2)
  
  df <- df1 - df2
  d <- mean(df)
  n <- length(df)
  s <- sd(df)
  t_calc <- (d / (s / sqrt(n)))
  print(t_calc)
  alpha <- 0.05
  dof <- n - 1
  t_tab <- abs(qt(alpha, dof))
  cat('\nmean of sample 1 : ', m1)
  cat('\nmean of sample 2 : ', m2)
  cat("\n\nmean of paired vector : ", d)
  cat("\nValue of t_calc : ", t_calc)
  cat("\nValue of t_tab : ", t_tab)
  if(t_calc > t_tab)
  {cat("\n\nNull hypothesis rejected.")}else
  {cat("\n\nNull Hypothesis accepted.")}

}

pairedTest(StockSam1$DAX, StockSam2$DAX)

