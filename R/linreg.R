library(matlib)

data("iris")

#linreg <- function(formula, data) {
#  return (1)  
#}

formula <- iris$Petal.Length~Sepal.Width+Sepal.Length
deparse(formula)
all.vars(formula)[1]
d <- iris
#all.vars(formula)
X <-model.matrix(formula, iris)
y <- iris[[all.vars(formula)[2]]]
y

b <- solve(t(X)%*%X) %*% t(X) %*% y
b

#all.vars(formula)[1]
#d[["iris"]]

linreg <- setRefClass("linreg",
    fields = list(
      formula = "formula",
      data = "data.frame",
      y = "numeric",
      beta = "array",
      residuals = "array",
      y_hat = "array",
      df = "numeric",
      res_var = "array"
    ),
    methods = list(
      initialize = function(formula, data){
        # stopifnot(formula = Petal.Length~Sepal.Width+Sepal.Length)
        # TODO: check input: does formula make sense and is data a dataset
        
        # to get the correct beta the fomula must be updated with the name of the dataset
        updated_formula <<- as.formula(paste(substitute(data), "$", deparse(formula)))
        
        .self$formula <<- updated_formula 
        .self$data <<- data
        X <- model.matrix(.self$formula, data)
        
        .self$y <<- iris[[all.vars(updated_formula)[2]]]
        
        .self$beta <- inv(t(X)%*%X) %*% t(X) %*% y
        
        .self$y_hat <<- X %*% .self$beta
        .self$residuals <<- .self$y - .self$y_hat
        .self$df <<-  nrow(X) - ncol(X)
        .self$res_var <<- (t(.self$residuals)%*%.self$residuals) / .self$df
        # TODO: rest of the formulas
      }
   )
)

l <- linreg$new(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#class(l)[1]
l$beta
