library(shiny)
library(ggplot2)
library(plyr)

readData <- function(filename, region = 0) {
  dat <- read.csv(file = filename, sep = ',', header = FALSE)
  names(dat)[c(5, 8, 9, 16, 17)] <- c('population', 'physicians', 'beds', 'income', 'region')
  if (region > '0') {
    dat <- dat[which(dat$region == as.numeric(region)), c('population', 'physicians')]
  }
  dat
}

doPredict <- function(data, population) {
  fit <- lm(data = data, physicians~population)
  ceiling(predict(fit, newdata = data.frame(x1 = c(population))))
}

residual_plot <- function(data) {
  fit <- lm(data = data, physicians~population)
  ggplot(fit, aes(.fitted, .resid)) + geom_point() +
    stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed") +
    xlab("Fitted values") + ylab("Residuals") +
    ggtitle("Residual vs Fitted Plot") + theme_bw()
}

confidence_interval <- function(data, x, alpha) {
  fit <- lm(data = data, physicians~population)
  n = dim(data)[1]
  yh = doPredict(data, x)
  sse <- sum((data$physicians - predict.lm(fit, data.frame(population = data$population)))^2)
  mse <- sse / (n - 2)
  paste("===", mse)
  s_yh <- sqrt(mse * (1/n + (x - mean(data$population))^2/sum((data$population - mean(data$population))^2)))
  yh + c(-1, 1) * qt(alpha, n - 2) * s_yh
}

anova_table <- function(data) {
  x = data$population; y = data$physicians;
  fit <- lm(data = data, physicians~population)
  n = length(y)
  predictions = predict(fit, newdata = data.frame(population = x))
  sst <- sum((y - mean(y))^2)
  sse <- sum((y - predictions)^2)
  ssr <- sst - sse
  total <- list(name = 'Total', ss = sst, df = n - 1, ms = NA, ems = NA)
  error <- list(name = 'Error', ss = sse, df = n - 2, ms = sse/(n - 2), ems = sse/(n - 2))
  emsr <- sse/(n-2) + fit$coefficients[[2]] * sum((x - mean(x))^2)
  regression <- list(name = 'Regression', ss = ssr, df = 1, ms = ssr, ems = emsr)
  correction_for_mean <- list(name = 'Correction for Mean', ss = n * mean(y)^2, df = 1)
  uncorrected_total <- list(name = 'Uncorrected Total', ss = sum(y^2), df = n)
  l <- list(regression, error, total, correction_for_mean, uncorrected_total)
  table <- ldply(l, data.frame)
  rownames(table) <- c('regression', 'error', 'total', 'correction_for_mean', 'uncorrected_total')
  table
}

beta1_test <- function(data, alpha) {
  n <- dim(data)[1]
  fit <- lm(data = data, physicians~population)
  sse <- sum((data$physicians - predict.lm(fit, data.frame(population = data$population)))^2)
  mse <- sse / (n - 2)
  sd <- sqrt(mse/sum((data$population - mean(data$population))^2))
  t <- qt(alpha, n - 2)
  t_val <- fit$coefficients[2] / sd
  p_val <- pt(t_val, n - 2, lower.tail = FALSE) * 2
  if (t_val >= t) {
    paste("T value: ", t_val, " >= ", t, " conclude Ha Beta1 != 0")
  }else {
    paste("T value: ", t_val, " < ", t, " conclude H0 Beta1 == 0")
  }
}

shinyServer(
  function(input, output) {
    loadData <- reactive({
      readData('APPENC02.txt', input$region)
    })
    prediction <- reactive({ doPredict(loadData(), input$population) })
    conf_interval <- reactive({
      confidence_interval(loadData(), input$population, input$conf_level)
    })
    
    output$population <- renderText(input$population)
    output$prediction <- renderText(prediction())
    output$x_hist <- renderPlot(ggplot(data=loadData(), aes(x=population, y=..density..)) +
                                geom_histogram(binwidth= 100000, colour="darkgreen", fill="white") +
                                labs(x="Population") + labs(y="Density") +
                                geom_vline(xintercept = mean(loadData()$population), show_guide = TRUE))
    output$y_scatter <- renderPlot(ggplot(data=loadData(), aes(x = population, y = physicians)) +
                                     geom_point() + geom_smooth(method=lm) +
                                     ggtitle("Scatter Plot") + theme_bw())
    output$residual_plot <- renderPlot(residual_plot(loadData()))
    output$conf_interval <- renderText(conf_interval())
    test_desc <- reactive({
      fit <- lm(data = loadData(), physicians~population)
      paste("H0 Beta1 = 0; Ha Beta != 0 given alpha = ", 1 - input$conf_level)
    })
    output$test_desc <- renderText({ test_desc() })
    output$test_result <- renderText({ beta1_test(loadData(), 1 - input$conf_level) })
    
    linear_model <- reactive({
      data <- readData('APPENC02.txt', input$region)
      fit <- lm(data = data, physicians~population)
      predictions <- predict(fit, data.frame(population = data$population))
      sse <- sum((data$physicians - predictions)^2)
      sst <- sum((data$physicians - mean(data$physicians))^2)
      ssr <- sum((predictions - mean(data$physicians))^2)
      list(fit = fit, sse = sse, r_squared = round(ssr/sst, 5))
    })
    output$sse <- renderText({paste("Sum of Error: ", linear_model()$sse)})
    output$coefficients <- renderText({paste("Coefficients: Beta0 = ", 
                                    round(linear_model()$fit$coefficients[1], 5),
                                    " Beta1 = ", round(linear_model()$fit$coefficients[2], 5))})
    output$r_squared <- renderText({paste("R^2: ", linear_model()$r_squared)})

    output$anova_table <- renderDataTable({ anova_table(loadData()) })
  }
)