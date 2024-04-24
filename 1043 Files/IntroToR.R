######################Slide 14
# List of pre-loaded data
data()

#Display list of available data set :
data()

#Loading  built-in dataset
data(mtcars) 

#Print the first 6 rows 
head(mtcars, 6)

######################Slide 15
#Compute mathematical expressions:
2^3+2
# Define variables and assign values:
A <- 10

######################Slide 17
# Example for "if else condition"
x <- 10
if(x > 0)
{
  print("This is Positive number")
}

######################Slide 18
# Example for "for loop"
for(i in 1:5)
{
  print (i^2)
}

######################Slide 19
# Example for "while loop"
i <- 1
while (i <=3) {
  print(i*i)
  i = i+1
}

######################Slide 20
# Stop the loop when i is 3(break)
x <- 1:5
for (i in x) {
  if (i == 3){
    break
  }
  print(i)
}

######################Slide 21
# Skip the loop when is is 3(next)
x <- 1:5
for (i in x) {
  if (i == 3){
    next
  }
  print(i)
}

######################Slide 22
#Numeric
x <- 10.5

#Integer
x <- as.integer(10.5)

#Complex
x <- 1+2i

#Logical
x <- TRUE

#Character
x <- "Intro To R"

######################Slide 23
# Print the class name ofy
y <- 8
class(y)

# Is y an integer?
is.integer(y)

#Change data type
as.character(y)

#Getting help
help(c)

######################Slide 24
X <- c(1,-2,5.3,6,-20,4) # numeric vector
print(X) 

Y <- c("one","two","three") # character vector
print(Y)

Z <- c(FALSE,TRUE,FALSE,FALSE,TRUE,FALSE) #logical vector
print(Z)

######################Slide 25
# Accessing vector elements using position.
x <- c("Jan","Feb","Mar","April")
y <- x[c(1,3,4)]
print(y)

# Accessing vector elements using negative indexing.
t <- x[c(-1,-4)]
print(t)

#Access range of values in vector
x[1:3]

# Accessing vector elements using logical indexing.
v <- x[c(TRUE,FALSE,FALSE,FALSE)]
print(v)

######################Slide 26
# Create two vectors.
v1 <- c(1,2,4,5,7,11)
v2 <- c(12,4,3,8,1,21) 

# Vector addition.
add.result <- v1+v2
print(add.result) 

# Vector substraction.
sub.result <- v1-v2
print(sub.result) 

# Vector multiplication.
multi.result <- v1*v2
print(multi.result) 

# Vector division.
divi.result <- v1/v2
print(divi.result)

#####################FLUX null value

x <- c("Java", NA, "Python", "R", NA)
anyNA(x)
is.na(x)

######################Slide 27
# Create the data frame
names <- c("Bill", "Ted", "Henry", "Joan")
ages <- c(76, 82, 104, 78)
heights <- c(1.55, 1.69, 1.49, 1.57)
myTable <- data.frame(names, ages, heights)
print(myTable)



######################Slide 28
# Rename column name
names(myTable)<-c("Names","Ages", "Heights")
print(myTable)

######################Slide 29
#Number of rows in data frame 
nrow(myTable)   

#Number of columns in data frame
ncol(myTable)

#Dimension of data frame
dim(myTable)

######################Slide 30
# View the structure of data
str(myTable) 

######################Slide 31
#Minimum value
min(myTable$Ages)

#Average value
mean(myTable$Heights)

#Standard deviation
sd(myTable$Heights)

######################Slide 32
summary(myTable)

######################Slide 33
# Accessing columns by name
myTable["Ages"]

#OR
myTable$Names 

# Accessing multiple columns 
myTable[c("Names","Ages")]

# Accessing columns by index
myTable[2]

# Accessing multiple columns 
myTable[c(1,2)]

######################Slide 34
# Accessing first row and all the columns
myTable[1,]

#Accessing a range of rows and all the columns
myTable[2:4,]


######################Slide 35
#Accessing particular cells by [row,column]
myTable[1,2]
myTable[3:4,2:3]

######################Slide 36
#Sort by 
newData <- myTable[order(myTable$Ages),]
newData <- myTable[order(myTable$Ages,myTable$Heights),]
newData <- myTable[order(myTable$Ages,myTable$Heights, decreasing = TRUE), ]
print(newData)

######################Create another table
# Create the data frame
Names <- c("Bill", "Ted", "Henry", "Joan")
Ages <- c(76, 82, 82, 78)
Heights <- c(1.55, 1.69, 1.49, 1.57)
myTable2 <- data.frame(Names, Ages, Heights)
print(myTable2)

myTable2[order(myTable2$Ages,myTable2$Heights), ]
myTable2[order(myTable2$Ages,myTable2$Heights, decreasing = TRUE), ]

######################Slide 37
merge(myTable, myTable2,by="Ages")

rbind(myTable, myTable2)
######################Slide 38
#Aggregate data frame mtcars by cyl and vs, returning means for numeric variables
aggregate(myTable2[,2:3],by= list(Ages),FUN=mean)

attach(mtcars)
aggData <- aggregate(mtcars,by= list(cyl,vs),FUN=mean, na.rm=TRUE)
print(aggData)
detach(mtcars)

######################Slide 39
# Get the current working directory.
getwd()

# Set current working directory.
# Set to whichever one that you are working on
setwd("/Users/msal0006/Downloads")

# Get the current working directory.
getwd()

######################Slide 40
# Write data into a csv file in R
write.csv(myTable,"FileName1.csv")

######################Slide 41
# read a csv file in R
myData = read.csv("FileName1.csv") # reads the csv file in R object named mydata
print(myData)

######################Slide 42
#Display Data
head(myData)

tail(myData)

######################Slide 43
# Install Packages
install.packages("moments")

# Load Packages
library(moments)

######################Slide 44
#Displaying bar chart
H <- c(25,12,43,7,51)
M <- c("Delhi","Beijing","Washington","Tokyo","Moscow")
barplot(H,xlab="City",ylab="Happiness Index",col="blue",names.arg=M, main="Happiness Index",border="red")

######################Slide 45
#Stack bar chart
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS", xlab = 
          "Number of Gears", col=c("darkblue","red"), legend =      
          rownames(counts))

######################Slide 46
#Group bar chart
counts <- table(mtcars$vs, mtcars$gear)
barplot(counts, main="Car Distribution by Gears and VS", xlab =  
          "Number of Gears", col=c("darkblue","red"),legend =     
          rownames(counts),beside=TRUE)

######################Slide 47
# Computes a histogram of the data values from the dataset named AirPassengers 
# (a default dataset in R)
hist(mtcars$hp,main="Histogram for mtcars",xlab="HP",
     border="red",col="blue")

######################Slide 48
#create a boxplot graph for the relation between mpg(miles per gallon) and cyl (number of cylinders) 
#from the well known mtcars data set.
boxplot(mpg ~ cyl, data=mtcars,xlab="Number of Cylinders",ylab="Miles Per Gallon",
        main="Mileage Data")

mytest = c(1,5,6,6,6,6,7,10)
outliers <- boxplot(mytest)$out

#newmytest <- newmytest(-which(mytest %in% outliers))
# http://rpubs.com/Mentors_Ubiqum/removing_outliers

######################Slide 49
# Use the data set “mtcars” available in the R environment to create a basic scatter plot.
input <- mtcars[,c('wt','mpg')]
# Plot the chart for cars with weight between 2.5 to 5 and mileage between 15 and 30.
plot(x=input$wt,y=input$mpg,
     xlab="Weight",
     ylab="Mileage",
     xlim=c(2.5,5),
     ylim=c(15,30),
     main="Weight vs Mileage")

######################Slide 50
# Vector of heights
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
# Vector of Weights
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

#Fitting a linear model 
fit <- lm(height~weight)
print(fit)

######################Slide 51
print(summary(fit))

######################Slide 52
# Give the chart file a name.
#png(file = "linearregression.png")

# Plot the chart.
plot(height,weight,col = "blue",main = "Height & Weight Regression",
     abline(lm(weight~height)),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")

######################Slide 53
#Install and Load the party package. 
#install.packages("party")
library(party) 

#Create the input data frame
inputData <- readingSkills[c(1:105),]
print(head(inputData))

######################Slide 54
#Give the chart file a name
#png(file = "decision_tree.png") 


#Create the tree. 
outputTree <- ctree( nativeSpeaker ~ age + shoeSize + score, data = inputData)

plot(ctree( nativeSpeaker ~ age + shoeSize + score, data = inputData))



#Plot the tree
plot(outputTree) 

########
















