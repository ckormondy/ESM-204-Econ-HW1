# ESM 204 Homework 1 Guide

# To start out, load in the data. You will have to set the working directory
# so that R knows where to look for the dataset. 

dat <- read.csv("HW1data.csv")

# I changed the names to make them easier to work with
colnames(dat) <- c("parcel","a","b","cost")

# Ok so now the data is loaded...

# Part B of the assingment asks us to determine how many parcels to buy if habitat for 
# species A is our only concern... 
# our budget is 10 million and we want to maximize the probability of survival

# Our first job is to write a simple function to optimize... before we get to that we need 
# to understand how to write a function in R. If you're not familiar with this try looking 
# up "User defined functions in R"

# Here's an example function that returns the negative of the natural log of a scalar
# (for reasons that will become clear later)

ln <- function(x){
  
  lnx <- -log(x)
  return(lnx)
  
}

# Thats the basic syntax.. the function takes 1 argument called x and returns one output
# Note that the reference x only exists within the function definition. The function can be 
# used on any variable, for example...

scalar <- 100
ln_scalar <- ln(scalar)

# and now ln_scalar has the value -4.60517


# For this project we need a function which takes as an input whether or not each parcel is bought and 
# returns the probability of survival. I am going to use a vector of lenght 50 as the purchase assignment

suitable <- function(purchases){
  
  suitability <- sqrt(sum(purchases*dat$a))
  
  return(-suitability)
}

# Lets go over what this function does... 
# It takes the vector of purchase decisions (Chris used the letter X in lecture)
# and returns the negative of the square root of the sum of habitat quality.
# Note that I did not include the habitat quality (dat$a) as an input to the function, 
# instead I refered to its global definition at the beginning of the script. This is lazy coding on my part
# and you are welcome to change it if you want.

# Lets try using the function supposed I bought every parcel, what would be the success probability for species A?
# To find this out I will feed the function a vector of length 50 with 1's in every entry

every_parcel = rep(1,50)
maximum_probability = suitable(every_parcel)

# Maximum_probability is -8.660254

# The last thing I show you in this guide is how to use constrOptim function to MINIMIZE a function.
# Subject to a linear constraint. This is pretty equivalent to exel's Solver but you are exposed to 
# a few more of the inner mecahnics of the solver.

# By default, most solvers are minimizers (because computer scientiests like to mimimize), apparently R
# can detect if you meant to maximize instead, but to take the uncertainty out of it I simply defined my functions to 
# return the negative of the thing we want to maximize, hence converting the problem to minimization

# lets try an example on the logarithm function I defined earlier... 

print(constrOptim(10, ln, ui=c(-1), ci=(-20), grad=NULL))

# lets take a look at whats going on here.... the arguments I gave constrOptim are as follows

# 10: is an initial guess of the solution which gives the solver a starting point to search
# note that this has to be feasible

# ln: is the name of the function to minimize

# ui: is a matrix of constraints
# ci: is a matrix of values (check the help documentation)

# grad=NULL just means that I am not supplying the derivative of the function being optimize
# which affects how the solver goes about finding a solution. In general it is much faster to solve
# something if you know the derivate.

# Lets go over the constraint. This requires a bit of Linear Algebra (Matrix Algebra)
# The contraint is: ui %*% theta >= ci
# Where %*% is the matrix product, ui is a matrix of coefficeints, and ci is the vector of boundaries
# and theta are the parameters we are choosing. For example, if we had two parameters x1 and x2 that we want >= 1
# then we would have:

# ui = [[1,0],
#       [0,1]]   (The identity matrix)

# and ci = [1,1]

# in the previous example, I put minus signs in front of ui & ci to flip the inequality

# Lets try this on the homework problem. We want to Minimize the function "suitable" by choosing a vector of 
# length 50 such that each entry is between zero and 1, and the total cost is no greater than 10. This is a bit 
# complicated so I guess I'll just show you how to set it up. Our Ui matrix is going to have 101 rows and 50 columns.
# The ci vectors is going to have 101 entries

# Starting with the constraint that each entry is greater than or equal to zero, we need an identity matrix of size 50...
# then we have the negative identity for the constraint that no entry is greater than 1
# finally the last row is each parcels cost

eye = diag(50) # The identity matrix
coefs = rbind(eye, -eye, -dat$cost)

# Now we need to set up the value of the constraint, which is a vector of 50 zeros, 50 negative 1's, and finally the 
# number 10 which is the total cost (you may need to review matrix multiplication for this to make sense)

constraints = c(rep(0,50), rep(-1,50), -10)

# Lets see if it works....

initial_guess = rep(0.4,50) 

# remember the initial guess has to be feasible which you can check 
# by typing coefs %*% initial_guess >= constraints into the console & checking they are all true

output = constrOptim(initial_guess, suitable, ui=coefs, ci=constraints, grad=NULL)

# You can reference the optimization output as follows

success_prob = -output$value
purchases = output$par

# And thats all there is to it (I think I just gave you the answer to the first part pretty much... make sure you understand all the code)
