#' @title  Linear regression using ordinary linear algebra
#' 
#' @description linreg is used t o fit linear models and uses the ordinary
#'   linear algebra to  calculate the regression
#' 
#' 
#' 
#' @param formula a formula object.
#' @param data a data frame.
#' 
#' @import matlib 
#' @import ggplot2
#'



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
      stand_res = "array"

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

        .self$stand_res <<- .self$residuals / sqrt(.self$res_var)[1]
        
        .self$stand_res <<- sqrt(abs(.self$stand_res))
        
        #.self$t_values <<- sapply(.self$beta, FUN = function(x){x/sqrt(.self$beta_var)})
          #.self$beta / sqrt(.self$beta_var)
        # TODO: t-values

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
        theme <- theme(
          axis.ticks = element_line(colour = "black"),
          axis.ticks.length = unit(0.2, "cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 13, vjust = 0), 
          axis.text = element_text(size = 13, colour = "black"), 
          plot.title = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = "white", colour = "black", linetype = "solid"),
          plot.background = element_rect(colour = NA),
          axis.text.x = element_text(size = 13),
        )
    
        #Outliers
        #https://stackoverflow.com/questions/39259252/how-does-plot-lm-determine-outliers-for-residual-vs-fitted-plot
        
        outliers <- abs(.self$residuals)
        outliers_2 <- abs(.self$stand_res)
        
        outliers <- order((outliers), decreasing = TRUE)[1:3]
        outliers_2 <- order((outliers_2), decreasing = TRUE)[1:3]
        
        data_obs <- data.frame(obs = 1:nrow(.self$data))
                               
        data_obs$label <- ifelse(data_obs$obs %in% outliers, data_obs$obs, "")
        data_obs$label_2 <- ifelse(data_obs$obs %in% outliers_2, data_obs$obs, "")
        
                                                     
        rvsf <- ggplot(data.frame(.self$y_hat, .self$residuals), aes(y = .self$residuals, x = .self$y_hat)) + 
          geom_point(shape=21, size = 2) +
          geom_smooth(colour = "red", se = F, method = "glm") +
          ggtitle("Residuals vs Fitted") + 
          xlab(paste("Fitted Values \n", "linreg(", format(formula), ")")) +
          ylab("Residuals") +
          theme + geom_hline(yintercept = 0, color = "grey", linetype="dotted") +
          geom_text(label = data_obs$label, hjust = -0.3, vjust = 0.5)
        
        
        
        scaloc <- ggplot(data.frame(.self$y_hat, .self$stand_res ), aes(y = .self$stand_res, x = .self$y_hat)) + 
          geom_point(shape=21, size = 2) +
          geom_smooth(colour = "red", se = F) +
          ggtitle("Scale-Location") + 
          xlab(paste("Fitted Values \n", "linreg(", format(formula), ")")) +
          ylab(expression(sqrt("Standardized Residuals"))) +
          theme + geom_text(label = data_obs$label_2, hjust = -0.3, vjust = 0.5)
        
        
        return(list(rvsf, scaloc))
        
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
        #TODO: add p-values!!! 
      }
   )
)

l <- linreg$new(formula=Petal.Length~Sepal.Width+Sepal.Length, data=iris)
d <- linreg$new(formula=Petal.Length~Species, data=iris)
l$plot()
l$print()
l$.self$residuals
l$.self$res_var
d$.self$stand_res
d$plot()

min(d$.self$stand_res)

-0.445578965 / 0.6464805


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

