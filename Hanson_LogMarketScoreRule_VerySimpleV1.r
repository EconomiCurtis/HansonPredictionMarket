source("standardFunctions.R")

###########################
##  SIMULATORS #############
############################

Bidders <- function(n=10,prob=.8,sd=.1){
  #given number of biddors, and info about their distribution of prior probabilities,
  # returns a list of biddors with certain priors, and some cash to use
  result <-    rbind(
    data.frame(
      Time = 0,
      Prob = 0,
      Cash = 0
    ),
    data.frame(
      Time = round(runif(n,0,9999), 0),
      Prob = rnorm(n,prob,sd),
      Cash = rexp(n,0.5)
    ))
  
  result =  result %>%
    dplyr::mutate(
      Prob = ifelse(Prob > 1, yes = 1, no = Prob),
      Prob = ifelse(Prob < 0, yes = 0, no = Prob)
      )
  
  return( 
    result[with(result, order(Time)),]
  )
}

##########################################
# simulator one ----
# poeple have prior, if cost of new share is lowe rthan prior, buy until out of cash
# set base, fee, size, mean and distribution of people'e priors

base = 10
fee = 0.01
Dat <- Bidders(100,.8,0.1)

system.time({

  Dat$Buy = NA
  Dat$q1 = 0
  Dat$q2 = 0
  Dat$spend = 0
  Dat1 = Dat
  
  Book = data.frame(
    Time = 0,
    q1 = 0,
    q2 = 0,
    Income = 0,
    ImpProb = 0.5,
    base = base,
    fees_toMrkMaker = 0,
    type = NA
  )
  
  for (i in 2:nrow(Dat)){
    Dati = Dat[i,]
    CurrBook = tail(Book,1)
    
    
    
    
    #buy or sell?
    if ((TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee) < Dati$Prob){
      action = "Buy"
    } else {
      action = "Sell"
    } 
    
    ##############
    ##############
    # Need function for intelligent trader, see's price and cash, adn acts appropriately....
    
    if (action == "Buy"){
      while(  Dati$Cash >= (TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee)   
              & ((TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee) < Dati$Prob)){    
        Dati$q1 = Dati$q1 + 1 
        Dati$spend = Dati$spend + (TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee)
        Dati$Cash = Dati$Cash -  (TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee)
        CurrBook$base = CurrBook$base + (fee/2)
        CurrBook$q1 = CurrBook$q1 + 1
        CurrBook$fees_toMrkMaker = CurrBook$fees_toMrkMaker + (fee/2)
        
      }      
    } else {
      while(Dati$Cash >=  (TraderPay(CurrBook$base,CurrBook$q2,CurrBook$q1,1) + fee)  
            & ((TraderPay(CurrBook$base,CurrBook$q1,CurrBook$q2,1) + fee) > Dati$Prob)){
        Dati$q2 = Dati$q2 + 1
        Dati$spend = Dati$spend + (TraderPay(CurrBook$base,CurrBook$q2,CurrBook$q1,1) + fee)
        Dati$Cash = Dati$Cash - (TraderPay(CurrBook$base,CurrBook$q2,CurrBook$q1,1) + fee)
        CurrBook$base = CurrBook$base + (fee/2)
        CurrBook$q2 = CurrBook$q2 + 1
        CurrBook$fees_toMrkMaker = CurrBook$fees_toMrkMaker + (fee/2)
      }
    }
    
  
    CurrBook <- CurrBook %>%
      dplyr::mutate(
        Time = Dati$Time[1],
        Income = (Income +  Dati$spend[1]),
        ImpProb = Price1(tail(CurrBook$base,1), q1, q2),
        type = as.character(action)
      )
    if (Dati$spend == 0) CurrBook$type = NA    
    
    #update book
    Book = rbind(
      Book,
      CurrBook) 
    

    Dati$Buy = action
    Dat[i,] = Dati
    
  }
  
  
  MarketResult <- function(Dat=Dat, Book1=Book, Result=T){
    # Result = T if option one, q1, pays
    
    BookEnd <- Book1[dim(Book1)[1],]
    
    if (Result){
      print("Option 1 pays")
      print(paste("Total Payment", BookEnd$q1[1]))
      print(paste("Income", BookEnd$Income[1] - BookEnd$q1[1]))
      print(paste("Fees", BookEnd$fees_toMrkMaker[1]))
    }
    
    
  }
  
})

Dat1
Dat
Book
plot(Book$ImpProb, type = "l", ylim = c(0,1))

MarketResult()



