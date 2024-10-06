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
      updated_formula = "formula",
      data = "data.frame",
      y = "numeric",
      beta = "array",
      residuals = "array",
      y_hat = "array",
      df = "numeric",
      res_var = "array",
      beta_var = "array",
      t_values = "array",
      stand_res = "array"
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
        
        .self$stand_res <<- .self$residuals / sqrt(.self$res_var)[1]
        
        .self$stand_res <<- sqrt(abs(.self$stand_res))
        
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
        #TODO implement
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
