# # To give you more choices, I've added another item in the assignment. 
#  You need to answer and submit just 5 questions among these:
#  
# 
# # Define an R function that removes NA values from a vector.

Remove_NA <- function(x) {
  
  x[!is.na(x)]
  
}

Remove_NA(c(2,3,6,9,NA,7,NA,12))

# 
# # Define an R function that computes the factorial of given an integer argument. 
# The output should be a vector of length 1.


factorial <- function(x) {
  # if the value of x is 0 or 1, then 1 is returned
  if (x == 0 || x == 1) {
    return (1)
  }
  else {
    return (x * factorial(x - 1)) # recursive function to calculate the factorial
  } }

factorial(7)

# 
# # Define an R function that computes the determinant of a given matrix. 
# The output should be a vector of length 1.
# 
#   determinantRek<-function(X,k) {
#     if (dim(X)[1] == 1 && dim(X)[2] == 1)
#       return(X[1,1])
#     if (dim(X)[1] == 2 && dim(X)[2] == 2)
#       return(X[1,1]*X[2,2]-X[1,2]*X[2,1])
#     else
#       s = 0
#     for (i in 1:dim(X)[2]) {
#       s = s + X[k,i]*(-1)^(k+i)*
#         determinantRek(X[-k,-i],k)
#     }
#     return(s)
#   }
#   
# 
# # Define an R function that sorts a given vector in decreasing order. 
# The output should be a vector of the same length. It should accept both numeric or character vectors.

vector_sort <- function(A){
  for (j in 2:length(A)) {
    key = A[j] 
    # insert A[j] into sorted sequence A[1,...,j-1] 
    i = j - 1 
    while (i > 0 && A[i] > key) {
      A[(i + 1)] = A[i]
      i = i - 1 
    }
    A[(i + 1)] = key
  }
  A
}
vector_sort(c(4,5,6,7,3,2,1,8,12))

vector_sort(c('z','x', 'b','a','j','y'))

####   
# 
# # Define an R function that accepts a Date (POSIXct) 
# as argument and outputs the day of the week as characters. Use modulo operator.

######

date_nw = as.POSIXct(as.Date("01/01/1970", format = "%m/%d/%Y"))

# Accepts POSIXct as input
which_weekday = function(date_nw)
  {c("Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday")[((unclass(date_nw)/86400) %% 7) + 1]}

which_weekday(date_nw)

# 
# # Create a function to compute for your net pay at work.

Salary <- function(mon.pay) {
  
  pay <- (mon.pay*12)
  
  level_1 <- ((pay-250000)*0.20) / 12
  level_2 <- (((pay-400000)*0.25) + 30000) / 12
  level_3 <- (((pay-800000)*0.30) + 130000) / 12
  level_4 <- (((pay-2000000)*0.32) + 490000) / 12
  level_5 <- (((pay-8000000)*0.35) + 2410000) / 12
  
  
  
  
  
  if (pay <= 250000) {return(mon.pay)
  }else if(pay <= 400000) return(mon.pay - level_1)
  else if (pay  <= 800000) return(mon.pay - level_2)
  else if (pay <= 2000000) return(mon.pay - level_3)
  else if (pay  <= 8000000) return(mon.pay - level_4)
  else if (pay >8000000) return(mon.pay - level_5)
  
  
}

Salary(24000)
# 




# # Create a function that accepts a vector and an integer n and returns nth highest number

vector.nth.highest <- function(A, num){
  
  
  return(sort(vector1(A), decreasing = TRUE) [num])
  }

vector1 <- function(A){
  for (j in 2:length(A)) {
    key = A[j] 
    # insert A[j] into sorted sequence A[1,...,j-1] 
    i = j - 1 
    while (i > 0 && A[i] > key) {
      A[(i + 1)] = A[i]
      i = i - 1 
    }
    A[(i + 1)] = key
  }
  A
}

vector.nth.highest(c(2,5,6,7,3,4,9,12), 2)

# 
# # Create a function that computes the compound interest of an 
# investment given the rate, time, and initial amount or principal.

compound_int.rate <- function(principal, int.rate = 0.01, timeframe = 1){
  
  
  
  return(principal * ((1 + int.rate)**timeframe -1)) }

compound_int.rate(1200000, 0.04, 5)
# 
# # Create a function isPrime(n) that accepts an integer and outputs a Boolean value 
# (TRUE or FALSE) depending whether the integer is a prime number or not.



isPrime <- function(x){
  if(sum(x/1:x==x%/%1:x)==2) {
    print("TRUE")
  }
  else {
    print("FALSE")
  }  
}

isPrime(3)


# 
# # I'll give bonus points to the one whose choice of items is not very common with the others???.
# May the odds be ever in your favor.
