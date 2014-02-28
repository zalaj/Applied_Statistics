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

#The function T calculates a T statistics for given data frame X
T <- function (X){
  M <- (mean(X$Treatment_A) + mean(X$Treatment_B) + mean(X$Treatment_C))/3
  T <- (mean(X$Treatment_A) - M)^2 + (mean(X$Treatment_B) - M)^2 + (mean(X$Treatment_C)- M)^2
  return(T)
}

S <- function(X){
  S <-mean(X$Treatment_A)^2 + mean(X$Treatment_B)^2 + mean(X$Treatment_C)^2
  return(S)
}

m <- factorial(3)^7

??permutation
