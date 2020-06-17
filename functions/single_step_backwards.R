single_step_backwards <- function(data, response){
  resp.indx <- which(names(data)==response)
  y <- data[[response]]
  X <- data[,-resp.indx]
  n.pred <- dim(X)[2]
  if(n.pred > 1){
    full_adjrsq = summary(lm(y~.,data=as.data.frame(X)))$adj.r.squared
    print(paste0("Adjusted R-squared of model including all input explanatory variables: ",
                 round(full_adjrsq,5)))
    print("")
    for(i in 1:n.pred){
      red_adjrsq <- summary(lm(y~.,data=as.data.frame(X[,-i])))$adj.r.squared
      print(paste0("With variable ", names(X)[i]," removed, adj R-squared becomes: ",
                   round(red_adjrsq,5), 
                   ifelse(red_adjrsq>full_adjrsq,", an improvement ",", a deterioration "),
                   "from the input model"))
    }
  } else{
    print("Model only contains one variable.")
  }
}