library (lattice)
library(Matrix)

#The data of pacients
#This are the data from seven patients each underwent three different methods of kidney
#dialysis. The following values were obtained for weight change in kilograms between 
#dialysis sessions
Patients <- 1:7
Treatment_A <- c(2.90,2.56,2.88,2.73,2.50,3.18,2.83)
Treatment_B <- c(2.97,2.45,2.76,2.20,2.16,2.89,2.87)
Treatment_C <- c(2.67,2.62,1.84,2.33,1.27,2.39,2.39)

#Construction of the data frame for the data of patients
Patient <- data.frame(Patients,Treatment_A,Treatment_B,Treatment_C)


#The function S calculates T statsitsics for a given data frame

T <- function(X){
  T <-(7*(mean(X$Treatment_A)^2) + 7*(mean(X$Treatment_B)^2) + 7*(mean(X$Treatment_C))^2)
  return(T)
}

#The function T calculates a T statistics for given data frame X
T_1 <- function (X){
  M <- (mean(X$Treatment_A) + mean(X$Treatment_B) + mean(X$Treatment_C))/3
  T_1<- (mean(X$Treatment_A) - M)^2 + (mean(X$Treatment_B) - M)^2 + (mean(X$Treatment_C)- M)^2
  return(T_1)
}

#We calculate T and S for our data frame Patient
t<-T(Patient)
t
s <- T(Patient)
s

#
m <- factorial(3)^7
n <- factorial (21)
m/n

#because the value of m/n is too high we use Monte-Carlo simulation

##Monte Carlo symulation for caluculating p-value
#The input for the function:
#X - in data matrix, B - the number of iterations of Monte Carlo method,
#fun - the T statistics (in our case this are T() and, S())
Monte_Carlo <- function(X, B, fun){
  
  t_value <- fun(X)
  permutation_distr <- vector()
  
  for (i in 1:B){
    #we make a new matrix by taking a permutation of each line
    for (row in 1:7){
      X[row,2:4]<- sample(as.matrix(X)[row,2:4])
    }
    permutation_distr <- c(permutation_distr, fun(X))
  
  }

  #Calculation of p-value
  ident <-0
  t_bar <-mean(permutation_distr)
  for (i in 1:B){
    if (abs(permutation_distr[i]- t_bar) >= abs(t_value-t_bar)){
      ident <- ident +1      
    }
  }
  
  p <- (1+ident)/(1+B)
  print(ident)
  print(p)
  #we draw a histogram of permutation distribution
  hist(permutation_distr, nclass =100, ylab = 'Density', xlab = 'T', main ='A histogram of a permutation distribution' )
  abline(v = t_value, col = 2)  
}

#Monte Carlo for test stetistics T
Monte_Carlo(Patient,9999,T)

#Monte Carlo for test stetistics T
Monte_Carlo(Patient,9999,T_1)


