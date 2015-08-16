# Market Maker functions


library(dplyr)

#b is base, max market maker can lose on market
#q1 are shares for event=TRUE
#q2 are number of shares for event=FALSE

Cost <- function(b,q1,q2){
  #standard cost func for hanson log market scroing rule 
  return(
    b * log(exp(q1 / b) + exp(q2 / b))
  )
}

TraderPay <- function(b,q1,q2,q){
  #given q, how much of q1 being purchased, returns how much cash the trader must pay
  # for 'buying' q2, reverse q1 and q2
  result = Cost(b,q1+q,q2) - Cost(b,q1,q2)
  return(result)
}

Price1 <- function(b,q1,q2){
  # given base, and quantity of two option shares as inputs
  # returns implied price for yes, shares of option one
  # note this would be for an infitesimal purchase of q1, use TraderPay for actual price paid.
  return(
    exp(q1 / b) / (exp(q1 / b) + exp(q2 / b))
  )
}

SharesGot <- function(b,q1,q2,c){
  # Given b, q1, and q2, and the amount of cash the person wants to pay 'c', 
  # returns the number of shares the person will buy  
  return(
    b * log(exp((c / b) + (q1 / b)) + exp((c / b) + (q2 / b)) - exp(q2 / b)) - (q1)
  )
}



SharesToPrice <- function(b,q1,q2,price){
  # returns the number of shares needed to move current price (based on b,q1,q2)
  # to the argument "price"
  return(
    b * log((-price * exp(q2/b)) / (price-1)) - q1
  )
}