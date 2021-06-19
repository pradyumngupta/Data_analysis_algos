library(data.table)


# temp <- tempfile()
# download.file("https://cdn.scribbr.com/wp-content/uploads//2020/03/crop.data_.anova_.zip",temp)
# data <- read.table(unz(temp, "a1.csv"))
# unlink(temp)
# 
# data

#read data from url
diet <- read.csv("https://www.sheffield.ac.uk/polopoly_fs/1.570199!/file/stcp-Rdataset-Diet.csv",
              colClasses = c("numeric", "factor", "factor", "factor", "factor", "factor", "numeric"))
head(diet)

# count(diet$Person)

data.class(diet1$Diet)

summary(diet)

# cleaning the dataset
diet <- na.omit(diet)

#checking unique values

length(unique(diet$Person))
length(unique(diet$gender))
length(unique(diet$Age))
length(unique(diet$pre.weight))
length(unique(diet$Diet))
length(unique(diet$weight6weeks))


# one way anova
one.gen <- aov(weight6weeks ~ gender, diet)
one.gen
summary(one.gen)
# plot(one.gen)

one.age <- aov(weight6weeks ~ Age, diet)
one.age
summary(one.age)
# plot(one.age)

one.height <- aov(weight6weeks ~ Height, diet)
one.height
summary(one.height)

one.prew <- aov(weight6weeks ~ pre.weight, diet)
one.prew
summary(one.prew)

one.diet <- aov(weight6weeks ~ Diet, diet)
one.diet
summary(one.diet)

one.diet <- aov(weight6weeks ~ Diet, diet)
one.diet
summary(one.diet)


# coefficients(two.gen.age)
# plot(two.gen.age)

# two way anova
# gen
two.gen.age <- aov(weight6weeks ~ gender + Age, diet)
two.gen.age
summary(two.gen.age)

two.gen.hei <- aov(weight6weeks ~ gender + Height, diet)
two.gen.hei
summary(two.gen.hei)

two.gen.prew <- aov(weight6weeks ~ gender + pre.weight, diet)
two.gen.prew
summary(two.gen.prew)

two.gen.diet <- aov(weight6weeks ~ gender + Diet, diet)
two.gen.diet
summary(two.gen.diet)

# age
two.age.hei <- aov(weight6weeks ~ Age + Height, diet)
two.age.hei
summary(two.age.hei)

two.age.prew <- aov(weight6weeks ~ Age + pre.weight, diet)
two.age.prew
summary(two.age.prew)

two.age.diet <- aov(weight6weeks ~ Age + Diet, diet)
two.age.diet
summary(two.age.diet)

# height
two.hei.prew <- aov(weight6weeks ~ Height + pre.weight, diet)
two.hei.prew
summary(two.hei.prew)

two.hei.diet <- aov(weight6weeks ~ Height + Diet, diet)
two.hei.diet
summary(two.hei.diet)

#prew
two.prew.diet <- aov(weight6weeks ~ pre.weight + Diet, diet)
two.prew.diet
summary(two.prew.diet)


# Self-made ANOVA functions

# for one way
oneAnova <- function(Factor, Outcome)
{
  #number of levels of factor
  k <- length(unique(Factor))
  
  #vectors to store the classified outcome
  v1 <- c()
  v2 <- c()
  v3 <- c()
  
  #storing the values to the vectors
  for(i in 1:length(Factor))
  {
    if (Factor[i] == 1) {v1 <- c(v1, Outcome[i])}
    if (Factor[i] == 2) {v2 <- c(v2, Outcome[i])}
    if (Factor[i] == 3) {v3 <- c(v3, Outcome[i])}
  }
  #size of each level
  n1 <- length(v1)
  n2 <- length(v2)
  n3 <- length(v3)
  
  #total outcome length
  n <- n1 + n2 + n3

  #calculating mean for each class
  m1 <- mean(v1, na.rm = TRUE)
  m2 <- mean(v2, na.rm = TRUE)
  m3 <- mean(v3, na.rm = TRUE)
  
  #total mean
  mt <- ((m1 * n1) + (m2 * n2) + (m3 * n3)) / n
  
  #sum and mean of squares between classes
  bss <- n1*(m1 - mt)^2 + n2*(m2 - mt)^2 + n3*(m3 - mt)^2
  msb <- bss / (k-1)
  cat("\nbss = ", bss, "\n msb = ", msb)
  
  #sum and mean of squares within classes
  wss <- 0
  for(j in 1:n1) {wss <- wss + (v1[j] - m1)^2}
  for(j in 1:n2) {wss <- wss + (v2[j] - m2)^2}
  for(j in 1:n3) {wss <- wss + (v3[j] - m3)^2}
  msw <- wss / (n - k)
  cat("\nwss = ", wss, "\n msw = ", msw)
  
  #Calculating f-ratio
  fRatio <- msb / msw
  cat("\nF_value obtained : ", fRatio, "\n")
  
}
oneAnova(diet$gender, diet$weight6weeks)
oneAnova(diet$Age, diet$weight6weeks)
oneAnova(diet$Height, diet$weight6weeks)
oneAnova(diet$pre.weight, diet$weight6weeks)
oneAnova(diet$Diet, diet$weight6weeks)

summary(one.diet)

# for two way
twoAnova <- function()
{
  
}


# k = 4
# v <- c(4, 5,2,1,5)
# intersect(v, 3:5)
# v1 <- c(1:k)
# v1
# v  
#   
  
k <- c(1,2,3)
u <- matrix(0,4, 3)
u
u[,1] <- c(1,2,3,4)
u[,2] <- c(3,4, 2)
u[,3] <- c(5,6,7,0)
u
df <- data.frame(u)
df




# #creating dataframe
# max.len = max(length(v1), length(v2), length(v3))
# v1 = c(v1, rep(NA, max.len - length(v1)))
# v2 = c(v2, rep(NA, max.len - length(v2)))
# v3 = c(v3, rep(NA, max.len - length(v3)))
# 
# data <- data.frame(v1, v2, v3)

