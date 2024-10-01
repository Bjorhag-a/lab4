library(matlib)
# 
# data("iris")
# 
# #linreg <- function(formula, data) {
# #  return (1)  
# #}
# 
# formula <- iris$Petal.Length~Sepal.Width+Sepal.Length
# deparse(formula)
# all.vars(formula)[1]
# d <- iris
# #all.vars(formula)
# X <-model.matrix(formula, iris)
# y <- iris[[all.vars(formula)[2]]]
# y
# 
# b <- solve(t(X)%*%X) %*% t(X) %*% y
# b
# 
# #all.vars(formula)[1]
# #d[["iris"]]

linreg <- setRefClass("linreg",
    fields = list(
      formula = "formula",
      updated_formula = "formula",
      data = "data.frame",
      y = "numeric",
      beta = "array",
      residuals = "array",
      y_hat = "array",
      df = "numeric",
      res_var = "array",
      beta_var = "array",
      t_values = "array"
    ),
    methods = list(
      initialize = function(formula, data){
        .self$formula <<- formula
        # to get the correct beta the formula must be updated with the name of the dataset
        .self$updated_formula <<- as.formula(paste(substitute(data), "$", deparse(formula)))
        
         
        .self$data <<- data
        X <- model.matrix(.self$updated_formula, data)
        
        .self$y <<- iris[[all.vars(.self$updated_formula)[2]]]
        
        .self$beta <<- solve(t(X)%*%X) %*% t(X) %*% y
        
        .self$y_hat <<- X %*% .self$beta
        
        .self$residuals <<- .self$y - .self$y_hat
        
        .self$df <<-  nrow(X) - ncol(X)
        
        .self$res_var <<- (t(.self$residuals)%*%.self$residuals) / .self$df
        
        .self$beta_var <<- .self$res_var[1] * inv(t(X)%*%X)
        
        #.self$t_values <<- sapply(.self$beta, FUN = function(x){x/sqrt(.self$beta_var)})
          #.self$beta / sqrt(.self$beta_var)
        # TODO: t-values
      },
      print = function() {
        paste("linreg(formula = ",.self$formula, ", data = ", substitute(data),")")
        return_list <- list(paste("linreg(formula = ",.self$formula, ", data = ", substitute(data),")"), .self$beta)
        #return_list <- list(1, 2)
        names(return_list) <- c("Call", "Coefficients")
        #return_list
        return(return_list)
      },
      plot = function(){
        #TODO implement
      },
      resid = function(){
        return(.self$residuals)
      },
      pred = function(){
        return(.self$y_hat)
      },
      coef = function(){
        return(.self$beta)
      },
      summary = function(){
        #TODO implement
      }
   )
)

l <- linreg$new(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
# #class(l)[1]
#rownames(l$beta)
l$print()
# l$coef()
# l$updated_formula
# i<-1
# while (i <= 3){
#   print(paste(i, l$beta[[i]]))
#   #print(l$beta[[i]]/sqrt(l$beta_var[i][i]))
#   print(l$beta_var[i][i])
#   i <- i + 1
# }
# 
# class(l$beta_var)
# 
# q <- matrix(1:3, nrow=3, ncol=1)
# q / 2
