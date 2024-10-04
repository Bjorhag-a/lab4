library(matlib)
library(ggplot2)

linreg <- setRefClass("linreg",
    fields = list(
      formula = "formula",
      data_name = "name",
      updated_formula = "formula",
      data = "data.frame",
      y = "numeric",
      beta = "array",
      residuals = "array",
      y_hat = "array",
      df = "numeric",
      res_var = "array",
      beta_var = "array",
      beta_se = "numeric",
      t_values = "numeric",
      p_values = "numeric"
    ),
    methods = list(
      initialize = function(formula, data){
        
        # save the name of the dataset for later usage
        .self$data_name <<- substitute(data)
        
        .self$formula <<- formula
        # to get the correct beta the formula must be updated with the name of the dataset
        .self$updated_formula <<- as.formula(paste(.self$data_name, "$", deparse(formula)))
        
        
        .self$data <<- data
        
        # X is only used in the constructor, therefore no .self
        X <- model.matrix(.self$updated_formula, data)
        
        .self$y <<- iris[[all.vars(.self$updated_formula)[2]]]
        
        .self$beta <<- solve(t(X)%*%X) %*% t(X) %*% y
        
        .self$y_hat <<- X %*% .self$beta
        
        .self$residuals <<- .self$y - .self$y_hat
        
        .self$df <<-  nrow(X) - ncol(X)
        
        .self$res_var <<- (t(.self$residuals)%*%.self$residuals) / .self$df
        
        .self$beta_var <<- .self$res_var[1]  * inv(t(X)%*%X)
        # beta standard error
        .self$beta_se <<- sqrt(diag(.self$beta_var))
        
        # only take the diagonal elements of beta_var matrix and the result, as those are the values of interest
        .self$t_values <<- diag(sapply(.self$beta, FUN = function(x){x/sqrt(diag(.self$beta_var))}))
        .self$p_values <<- pt(abs(t_values), df, lower.tail = FALSE)
      },
      print = function() {
        cat("Call:")
        cat("\n")
        
        # format call output
        cat(paste("linreg(formula = ",deparse(.self$formula), ", data = ", .self$data_name,")", sep = ""))
        cat("\n")
        
        cat("Coefficients:")
        cat("\n")
        
        # format the coefficients output
        
        # Calculate max length of column names
        col_names <- c("(Intercept)",all.vars(.self$updated_formula)[-(1:2)])
        max_length <- pmax(nchar(col_names))
        
        # format output for table shape
        line1 <- paste(sprintf(paste0("%-", max_length, "s"), col_names), collapse = " ")
        line2 <- paste(sprintf(paste0("%-", max_length, "s"), format(beta, digits = 3)), collapse = " ")
        
        cat(line1)
        cat("\n")
        cat(line2)

      },
      plot = function(){
        #TODO implement
        #ggplot(data.frame(.self$y_hat, .self$residuals), aes(y = .self$residuals, x = .self$y_hat)) + 
        #  geom_point()
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
        
        cat(sprintf("%-15s %10s %12s %10s\n", "", "Estimate", "Std. Error", "t value"))
        cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "(Intercept)", .self$beta[1], .self$beta_se[1], .self$t_values[1]))
        cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "Sepal.Width", .self$beta[2], .self$beta_se[2], .self$t_values[2]))
        cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "Sepal.Length", .self$beta[3], .self$beta_se[3], .self$t_values[3]))
        
        #cat(sprintf("%-15s %10s %12s %10s\n", "", "Estimate", "Std. Error", "t value"))
        #cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "(Intercept)", -2.50, 0.50, -4.40))
        #cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "Sepal.Width", -1.30, 0.10, -10.90))
        #cat(sprintf("%-15s %10.2f %12.2f %10.2f   ***\n", "Sepal.Length", 1.70, 0.01, 27.50))
        
        cat(paste("Residual standard error: ", sqrt(.self$res_var), " on ", .self$df," degrees of freedom", sep = ""))
        
        #TODO: generalise it, so it can be used on other formulas as well!!!
      }
   )
)

l <- linreg$new(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
# class(l)[1]
l$summary()
l$beta_var
l$beta_se
l$res_var

