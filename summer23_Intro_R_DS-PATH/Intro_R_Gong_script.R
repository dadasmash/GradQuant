## Calculator 

# Addition
2+2 

# Exponentiation
2^3

# 
2 + (3^2 - sin(46))
# 
exp(1.3)
2.71828183^1.3


## Logical Tests

1 > 2
1==1 # As in other languages, we use "==" instead of "=" to test for equality.


1 > 2 & 1 > 0.5 # The "&" means "and"
1 > 2 | 1 > 0.5 # The "|" means "or"

# Negation
!(1 > 2)
1!=1 


## Objects
x <-2
y <- 3

print(x)
z<-2*3 
print(z)

## vectors
x<-c(2,4,6,8)
z<-2:4

print(x)
print(z)

x[1]
x[2:4]

# Many mathematical operations can also be applied to vectors in R:
x <- c(1, 2, 3, 4)
y <- c(2, 4, 6, 8)
print(x + y)
y * 2

## functions 
log(50, base =12)
12^1.574314

log(8)

exp(log(8))


x <- rnorm(400, mean=50, sd=10)
hist(x)

## 
a <-2
class(a)
gender<-"female"
class(gender)

class(TRUE)
class(True)

x<-c(2,"2,3")
class(x)

x =1:6
y = as.character(x)
y

x<-as.numeric(y)

z = c(1,"canada",3)
x<-as.numeric(z)
print(x)


## Data Management
# A working directory is the default location where R looks for fles.
getwd()
#setwd("desired_path")


#Read Data
#new_data <- read.csv(your_file_path/filename, header = T)

install.packages("readxl")
library(readxl)
#new_data <- read_excel(your_file_path)

## 
setwd("C:/Users/Da/Desktop/Intro_R_GradQuant")
library("readxl")
df = read_excel("Data_June27.xlsx")
View(df)


df[1,1]
df[1,]
head(df[,2])
head(df[,"Date"])

x<-df["id"]
x<-df[["id"]]
x<-df$id


names(df)
names(df)[1]
names(df)[names(df) == "id"]
names(df)[names(df) == "id"] <- "ID"
names(df)[names(df) == "ID"] <- "id"

## Sorting
df<-df[order(df$id),]

####Data Cleaning
## "N/A" "n/a"

summary(df$Math)
summary(df$Reading)

library("dplyr")
#  use the na_if function to replace a certain value in the selected column with NA
df$Math<-na_if(df$Math, "N/A")
df$Reading<-na_if(df$Reading, "n/a")


summary(df$Math)
summary(df$Reading)
class(df$Math)

# Convert Types
df$Math<-as.numeric(df$Math)
df$Reading<-as.numeric(df$Reading)

class(df$Date)
df$Date<- as.Date(df$Date , format = "%y/%m/%d")

df<-df[order(df$Date),]


# Subset Rows (filter)
df$Date<-replace(df$Date, df$Date > as.Date('2022-07-31'),NA)

df<-df[order(df$PE,decreasing = TRUE),]
df2<-subset(df,PE < 24000,select = c('id','Reading','PE'))

# Subset Columns 

jj<- cbind(df$Math, df$Reading)
View(jj)
colnames(jj) <- c("Math", "Reading")

df2<-df[c("Date","Math")]


# Generate New column
# Use add_column() function from tidyverse
library("tidyverse")

df <- df %>%
  add_column(e = rnorm(nrow(df), mean=0, sd=1))

df <- df %>%
  add_column(u = runif(nrow(df), min = -2, max = 2))
df = subset(df, select = -c(u) )

df <- df %>%
  add_column(u = runif(nrow(df), min = -2, max = 2))


df$PE<-df$Reading*-.2+df$u
df$Math=df$Reading*.5+df$e



## Programming

# If Else
a = 2
if(a != 0) {
  print(1 / a)
} else {
  print("Not Applicable.")
}


b = c(0, 1, 2, -3, 4)
ifelse(b < 0, NA, b)


# For Loop
for (i in 1:5) {
  print(i)
}


col_name = c("Reading", "Math", "PE")
df2 = df[col_name]
df_means = vector("numeric", length = ncol(df2))

for(i in 1:ncol(df2)) {
  df_means[[i]] = mean(df2[[i]],na.rm=TRUE)
}
names(df_means) = col_name

print(df_means)

summary(df$PE)
summary(df$Reading)

summary(df$Math)


# Functions
f<-function(a,b) {
s <- a + b
   return(s)
}
f(3,2)


maxmax <-function(x,y,z){
  x1<-max(x,na.rm=TRUE)
  x2<-max(y,na.rm=TRUE)
  x3<-max(z,na.rm=TRUE)
  xx<-max(x1,x2,x3)
  return(xx)
  
}

maxmax(df$Math,df$PE,df$Reading)

#how about use number as para? 
maxmax <-function(data,x,y,z){
  x1<-max(data[x],na.rm=TRUE)
  x2<-max(data[y],na.rm=TRUE)
  x3<-max(data[z],na.rm=TRUE)
  xx<-max(x1,x2,x3)
  yy<-min(x1,x2,x3)
  
  return (c(xx,yy))
  
}




# A functional is a function that takes another function as an argument.
# e.g. sapply

####
#R Markdown
#####

#################
#Exercise 
##################


## Q1 read "Data_June27.xlsx, 
# Q1.1 use the example code to clean the data 
#Q1.2 keep observations with math scores falling within the 25th and 75th percentiles.
# Q1.3 What are the average PE scores for males and females?

## Q2 use loop to simplify the maxmax function





