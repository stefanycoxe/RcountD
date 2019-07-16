library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(xlsx)

shinyServer(function(input, output, session) {
  
v <- reactive({  validate(
      need(input$b0 != "", "Please enter intercept value"),
      need(input$b1 != "", "Please enter regression coefficient value"),
      need(input$b0se != "", "Please enter intercept stanadard error"),
      need(input$b1se != "", "Please enter regression coefficient standard error"),
      if (input$reg_model != 1) {
        need(input$disp != "", "You need to specify the dispersion for this model")
      }
    )  
})


hbins <- reactive({
  
  repbin <- input$reps / 100
  trunc(repbin, 0)
  
})
  
expeff <- reactive({
  
    exp(input$b1)

})

sd0 <- reactive({
  
# SD for group = 0
    if (input$reg_model == 1) {
    sd0 <- sqrt(mean0())
  } else if (input$reg_model == 2) {
    sd0 <- sqrt(input$disp * mean0())
  } else if (input$reg_model == 3) {
    sd0 <- sqrt(mean0() + (input$disp * mean0() * mean0()))
  } 
  sd0
  
})

sd1 <- reactive({
  
    # SD for group = 1
  if (input$reg_model == 1) {
    sd1 <- sqrt(mean1())
  } else if (input$reg_model == 2) {
    sd1 <- sqrt(input$disp * mean1())
  } else if (input$reg_model == 3) {
    sd1 <- sqrt(mean1() + (input$disp * mean1() * mean1()))
  }
  sd1
  
})

mean0 <- reactive({
  
  exp(input$b0)

})

mean1 <- reactive({
  
    exp(input$b0)*exp(input$b1)  
  
})

coheff <- reactive({
  
  # calculate Cohen's d as (mean1 - mean0) / SD0    
  (mean1() - mean0())/sd0()  
  
})

simb0 <- reactive({
  
  set.seed(input$randseed)
  rnorm(input$reps)*input$b0se + input$b0

})  
  
simb1 <- reactive({
  
  set.seed(input$randseed)
  rnorm(input$reps)*input$b1se + input$b1

})
  
simmean0 <- reactive({
  
  exp(simb0())
  
})
  
simmean1 <- reactive({
  
  exp(simb0())*exp(simb1())

})

simsd0 <- reactive({  

    if (input$reg_model == 1) {
    simsd0 <- sqrt(simmean0())
  } else if (input$reg_model == 2) {
    simsd0 <- sqrt(input$disp * simmean0())
  } else if (input$reg_model == 3) {
    simsd0 <- sqrt(simmean0() + (input$disp * simmean0() *simmean0()))
  }
  simsd0
  
})

simcoh <- reactive({
  
  (simmean1() - simmean0()) / simsd0()
  
})

CI <- reactive({
  
  if (input$CI_level == 1) {
    CI <- 80
  } else if (input$CI_level == 2) {
    CI <- 90
  } else if (input$CI_level == 3) {
    CI <- 95
  } else if (input$CI_level == 4) {
    CI <- 99
  }
  CI
  
})

CI_lo <- reactive({
  
  (1 - CI()/100)/2

})

CI_hi <- reactive({
  
  ((1 - CI()/100)/2) + (CI()/100)
  
})

cohLL <- reactive({
  
  quantile(simcoh(), CI_lo())
  
})

cohUL <- reactive({
  
  quantile(simcoh(), CI_hi())
  
})

expLL <- reactive({
  
  quantile(exp(simb1()), CI_lo())
  
})

expUL <- reactive({
  
  quantile(exp(simb1()), CI_hi())
  
})

cohsig <- reactive({
  
  if (cohLL() > 0 & cohUL() > 0) {
    cohsig <- "significant and positive"
  } else if (cohLL() < 0 & cohUL() < 0)  {
    cohsig <- "significant and negative"
  } else {
    cohsig <- "non significant"
  }
  cohsig
  
})

expsig <- reactive({
  
  if (expLL() > 1 & expUL() > 1) {
    expsig <- "significant and positive"
  } else if (expLL() < 1 & expUL() < 1)  {
    expsig <- "significant and negative"
  } else {
    expsig <- "non significant"
  }
  expsig
  
})

# Instructions
##########

output$poissoneq_lin <- renderUI({
    withMathJax(helpText('$$ln(\\hat Y) = b_0 + b_1 X_1 + b_2 X_2 + \\cdots + b_p X_p$$'))
})

output$poissoneq_nonlin <- renderUI({
    withMathJax(helpText('$$\\hat Y = e^{(b_0 + b_1 X_1 + b_2 X_2 + \\cdots + b_p X_p)}$$'))
})

# Output
##########

output$output1 <- renderText({
  v()
paste("When predictor = 0, the predicted outcome mean is", round(mean0(), digits = 3), "with a standard deviation of", round(sd0(), digits = 3), ".")
})

output$output2 <- renderText({
    v()
paste("When predictor = 1, the predicted outcome mean is", round(mean1(), digits = 3), "with a standard deviation of", round(sd1(), digits = 3), ".")
})

output$output_expeff <- renderText({
    v()
  paste("Exponential effect size =", round(expeff(), digits = 3))
})

output$output3 <- renderText({
    v()
paste("The outcome mean when predictor = 1 is", "<font color=\"#FF0000\"><b>", round(expeff(), digits = 3), "times larger", "</b></font>", "than the outcome mean when predictor = 0.")
})

output$output4 <- renderText({
    v()
paste("The", CI(), "% CI for this estimate is [", round(expLL(), digits = 3), ",", round(expUL(), digits = 3), "].")
})

output$expsig <- renderText({
  v()
paste("The exponential effect is", "<font color=\"#FF0000\"><b>", expsig(), "</b></font>", ".")  
})

output$output_coheff <- renderText({
    v()
  paste("Standardized mean difference (SMD) effect size =", round(coheff(), digits = 3))
  
})

output$output5 <- renderText({
    v()
paste("The outcome mean when predictor = 1 is", "<font color=\"#FF0000\"><b>", round(coheff(), digits = 3), "standard deviations larger", "</b></font>", "than the outcome mean when predictor = 0.")

})

output$output6 <- renderText({
    v()
paste("The", CI(), "% CI for this estimate is [", round(cohLL(), digits = 3), ",", round(cohUL(), digits = 3), "].")
})
  
output$cohsig <- renderText({
  v()
paste("The SMD effect is", "<font color=\"#FF0000\"><b>", cohsig(), "</b></font>", ".")  
})

# Effects plots
##########

output$expeff_plot <- renderText({
  v()
paste("The multiplicative effect is", round(expeff(), digits = 3), ". For each 1 unit increase in the predictor, the predicted outcome is multiplied by", round(expeff(), digits = 3), ".")
})

output$coheff_plot <- renderText({
  v()
paste("The SMD effect is", round(coheff(), digits = 3), ". For each 1 unit increase in the predictor, the predicted outcome increases by", round(coheff(), digits = 3), "standard deviations.")
})

poi_model <- function(x){
  exp(input$b0 + input$b1 * x)
}

output$poiplot <- renderPlot({
 
  v()
   
  y <- c(mean0(), mean1())
  sd <- c(sd0(), sd1())
  ymin <- y - sd
  ymax <- y + sd
  
  ggplot(data.frame(x=c(-3,3)), aes(x)) +
    expand_limits(y=0) +
    stat_function(fun=poi_model, geom="line", color = "blue", size = 1) +
    geom_errorbar(aes(x = c(0,1), ymin = ymin, ymax = ymax, width = 0.2)) +
    labs(x = "Predictor", y = "Outcome variable") +
    theme_classic() +
    scale_x_continuous(breaks=seq(-3,3,1)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  
})

output$expeffplot <- renderPlot({
  
    v()
  
  ymin <- expLL()
  ymax <- expUL()
  y <- expeff()
  x <- 0
  df <- data.frame(x, y, ymin, ymax)
  
ggplot(data = df, aes(x=x, y=y)) +
    geom_errorbar(width=.2, aes(ymin=ymin, ymax=ymax)) +
  geom_point(x = x, y = y) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  expand_limits(y=c(0, NA)) +
  xlim(-2, 2) + geom_hline(aes(yintercept = 1), color = "blue", size = 2) +
 labs(x = "", y = "CI for exponential effect size", title =paste(round(expeff(), digits = 3),"times larger"))
  
})

output$coheffplot <- renderPlot({
  
    v()
  
  ymin <- cohLL()
  ymax <- cohUL()
  y <- coheff()
  x <- 0
  df <- data.frame(x, y, ymin, ymax)
  
ggplot(data = df, aes(x=x, y=y)) +
    geom_errorbar(width=.2, aes(ymin=ymin, ymax=ymax)) +
  geom_point(x = x, y = y) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  xlim(-2, 2) + geom_hline(aes(yintercept = 0), color = "blue", size = 2) +
  labs(x = "", y = "CI for standardized mean difference effect size", title =paste(round(coheff(), digits = 3),"standard deviations larger")) 
})

# Distribution plots
##########

# histogram for Cohen's d effect size
output$cohplot <- renderPlot({
  
    v()
  
  simcohdf <- data.frame(simcoh = simcoh())
  
  ggplot(simcohdf, aes(simcoh)) + geom_histogram(bins = hbins(), col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Standardized mean difference effect", 
         y = "Number of replications") +
    geom_vline(xintercept = coheff(), color = "red", lwd = 1) +
    geom_vline(xintercept = cohLL(), color = "blue", linetype = 2) +
    geom_vline(xintercept = cohUL(), color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0)) 
# hist(simcoh(),
# main="Monte Carlo distribution of Cohen's d effect size",
# xlab="Cohen's d", border="blue", las=1, breaks=hbins(), prob = TRUE)
# # vertical line at the estimated Cohen's d value
# abline(v = coheff(), col = "red", lwd = 2)
# #vertical line at the lower and upper CLs
# abline(v = cohLL(), col = "royalblue", lwd = 2, lty = 3)
# abline(v = cohUL(), col = "royalblue", lwd = 2, lty = 3)
# # legend to label all lines
# legend(x = "topright", c("Estimated", "CI limits"), col = c("red", "royalblue"), lwd = c(2, 2))
})

# histogram for exponential effect size
output$expplot <- renderPlot({
  
    v()
  
  simb1df <- data.frame(simb1 = exp(simb1()))
  
  ggplot(simb1df, aes(simb1)) + geom_histogram(bins = hbins(), col = "blue", fill = "white") +
    theme_classic() +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold")) +
    labs(x = "Exponential effect", 
         y = "Number of replications") +
    geom_vline(xintercept = expeff(), color = "red", lwd = 1) +
    geom_vline(xintercept = expLL(), color = "blue", linetype = 2) +
    geom_vline(xintercept = expUL(), color = "blue", linetype = 2) +
    theme(legend.position = "right") +
    scale_y_continuous(expand = c(0,0))  
# # histogram for exponential effect size
# hist(exp(simb1()),
# main="Monte Carlo distribution of exponential effect size",
# xlab="Exponential effect", border="blue", las=1, breaks=hbins(), prob = TRUE)
# # vertical line at the estimated Cohen's d value
# abline(v = expeff(), col = "red", lwd = 2)
# # vertical line at the lower and upper CLs
# abline(v = expLL(), col = "royalblue", lwd = 2, lty = 3)
# abline(v = expUL(), col = "royalblue", lwd = 2, lty = 3)
# # legend to label all lines
# legend(x = "topright",  c("Estimated", "CI limits"), col = c("red", "royalblue"), lwd = c(2, 2))
})

# Table of results
##########

saveData <- reactive({data.frame("reg_model" = input$reg_model,
                      "b0" = input$b0, 
                      "b1" = input$b1,
                      "b0se" = input$b0se,
                      "b1se" = input$b1se,
                      "disp" = input$disp,
                      "CI" = input$CI_level,
                      "reps" = input$reps,
                      "ranseed" = input$ranseed,
                      "mean0" = mean0(),
                      "mean1" = mean1(),
                      "sd0" = sd0(),
                      "sd1" = sd1(),
                      "expeff" = expeff(),
                      "expLL" = expLL(),
                      "expUL" = expUL(),
                      "coheff" = coheff(),
                      "cohLL" = cohLL(),
                      "cohUL" = cohUL(),
                      check.names = F) 
})


output$table <- renderTable({
  
    v()
  
  saveData()
  
  })

  output$downloadData <- downloadHandler(
    filename = function() {"rcountd.csv"},
    content = function(file) {
      write.csv(saveData(), file, row.names = FALSE)
    }
  )

# References
##########

})