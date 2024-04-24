setwd("D:/Desktop/R files")

# 5 part 1 
dogBites <- read.csv("dogbites.1997.csv", header = TRUE)
n <- length(dogBites$daily.dogbites)
lamb <- sum(dogBites)/n
lamb
# output/answer : [1] 4.391534 



# 5 part 2.a
ppois(2,lamb) # 0.1861507


# 5 part 2.b.
# 4 and 5 since they are the closest to the mean of 4.391534

# 5 part 2.c
weeklyLamb <- lamb*7
ppois(32,weeklyLamb)
# output/answer : [1] 0.6346646

# 5 part 2.d
(choose(14,12)*ppois(2,lamb,FALSE)**12 * ppois(2,lamb)**2) + (choose(14,13) * ppois(2,lamb,FALSE)**13 * ppois(2,lamb)) + (choose(14,14) * ppois(2,lamb,FALSE)**14)
# output/answer : [1] 0.5012691

# 5 part 3
h <- hist(dogBites$daily.dogbites,
     main="",
     breaks = 22,
     xlab="Number of bites",
     ylab = "Occurence/Frequency Density",
     xlim = c(0,22),
     ylim = c(0,0.2),
     col="darkmagenta",
     freq = FALSE
     )

h <- hist(dogBites$daily.dogbites,
          main="",
          breaks = 22,
          xlab="Number of bites",
          ylab = "Occurence/Frequency Density",
          xlim = c(0,22),
          ylim = c(0,0.2),
          col="darkmagenta",
          freq = FALSE
)

plot(c(0:22),dpois(x=0:22,lamb),type ='l')

lines(c(0:22),dpois(x=0:22,lamb))

