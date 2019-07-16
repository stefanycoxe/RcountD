library(shiny)
library(shinythemes)
library(shinydashboard)
library(ggplot2)
library(xlsx)

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  titlePanel("Effect size for Poisson regression"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #shinythemes::themeSelector(),
      
      submitButton("Calculate!"),
      
      radioButtons("reg_model", label = h4("Regression model"),
                  choices = list("Poisson regression" = 1, "Overdispersed Poisson regression" = 2, 
                                 "Negative binomial regression" = 3),selected = 1),
      
      numericInput("b0", label = h4("Intercept/constant regression coefficient"), value = NULL),
      
      numericInput("b0se", label = h4("Standard error of intercept"), value = NULL),
      
      numericInput("b1", label = h4("Regression coefficient for effect of interest"), value = NULL),
      
      numericInput("b1se", label = h4("Standard error of effect of interest"), value = NULL),
      
      numericInput("disp", label = h4("Dispersion"), value = NULL),
      
      radioButtons("CI_level", label = h4("Confidence interval %"),
                  choices = list("80% CI" = 1, "90% CI" = 2, "95% CI" = 3, "99% CI" = 4),selected = 3, inline = TRUE), 
      
      numericInput("reps", label = h4("Monte Carlo replications"), value = 2000),
      
      numericInput("ranseed", label = h4("Seed"), value = 12345),
      
      submitButton("Calculate!"),
      
      width = 3),

    mainPanel(
      
      tabsetPanel(id = "countd_tabs", type = "pills", selected = "1. Introduction -->",
        
        tabPanel("1. Introduction -->",
                 br(),

          wellPanel(
            h4("Welcome! This web tool allows users to input the results of a Poisson regression model in order to calculate two measures of effect size: an exponential effect size commonly used for Poisson models and a standardized mean difference effect size similar to Cohen's d. Confidence intervals based on Monte Carlo simulation are provided for each effect size."),
            h4("Use output from any software package -- R, SAS, SPSS, Stata, etc."),
            h4("At minimum, you must specify 1) the type of model and 2) the regression coefficients and standard errors for the intercept (constant) and the effect of interest."),
            h4("If you select an overdispersed Poisson regression model or a negative binomial model, you MUST also specify a dispersion value."),
            h4("Other input values -- confidence interval range, number of replications, and seed value -- are optional."),
            h4("<-- Enter all values to the left")),
            wellPanel(
            h3("Navigate using the tabs above ^"),
            h4("Poisson regression tab: General background information about regression models for count outcomes"),
            h4("Input values tab: Detailed information about the input values you will enter"), 
            h4("Effect sizes tab: Output here. The calculated effect size values and their interpretation"),
            h4("Effect size plot tab: Plot of the nonlinear X-Y relationship "),
            h4("Distribution plot tab: Plot of the Monte Carlo sampling distribution of the effect sizes"),
            h4("Table tab: Downloadable table of results"),
            h4("References tab: References for Poisson regression, R, and Shiny")),
          
          wellPanel(
                 h3("Contact info"),
                 h4("Stefany Coxe"),
                 h4("Florida International University"),
                 img(src='FIUlogo_H_BW.png', width = 300),
                 h4(HTML("Questions? <a href='mailto:stefany.coxe@fiu.edu?Subject=[RcountD%20question]' target='_top'>Email me!</a>")),
                 #h4("Questions?", a("Email me!", href="mailto:stefany.coxe@fiu.edu?subject = Shiny")),
                 h5("Last updated: August 2, 2018"))),
          
        tabPanel(id = "poissonregression", "2. Poisson regression -->",
          wellPanel(
          h3("Count outcomes"),
          h4("Counts and frequencies can only take on whole number values (discrete), cannot be less than 0, are typically right skewed, and often display heteroscedasticity (non-constant variance) in their relationships with other variables. For these reasons, count or frequency variables generally should not be used as outcomes in linear models such as ANOVA or linear regression that rely on the normal distribution."),
          h3("Poisson regression"),
          h4("Poisson regression is a type of generalized linear model (GLiM, McCullagh, 1984; Cameron & Trivedi, 1998) that was developed to model counts or frequencies. It revolves around the", a("Poisson distribution", href="https://en.wikipedia.org/wiki/Poisson_distribution", target="_blank"), ", which is a discrete distribution that is not defined below zero. It also displays right skew, particularly with low means. These properties closely match the properties of count variables desribed above."),
          h4("Poisson regression models a count outcome as a nonlinear function of one or more predictors:"),
          uiOutput("poissoneq_nonlin"),
          h4("where the\\(\\ b\\)s are the regression coefficients or weights and the\\(\\ X\\)s are the explanatory or predictor variables."),
          withMathJax(),
          h4("This is a nonlinear model, so the effects are interpreted differently from typical linear models like ANOVA and linear regression. The exponent (\\(\\ e\\)) in the equation means that the effect is", em("multiplicative"), ". For a one unit increase in the first predictor\\(\\ X_1\\), we expect the predicted count to be", em("multiplied"), "by\\(\\ e^{b_1}\\). For more details about interpretation of Poisson regression models, see Coxe, West, & Aiken (2009)."),
          h3("Dispersion and overdispersion"),
          h4("For the Poisson distribution, the conditional mean of the distribution is equal to its conditional variance; this is termed", em("equidispersion"), ". In real data, this property often does not hold, such that the (conditional) variance of an outcome is larger than the (conditional) mean of the outcome; this is termed", em("overdispersion"), ". Ignoring overdispersion can lead to inflated type I error."),
          h4("There are two types of models that extend Poisson regression to correctly model overdispersion: overdispersed Poisson regression and negative binomial regression. Both models include an additional parameter to model overdispersion. See Gardner, Mulvey, & Shaw (1995) for more details about these models. See", em("Dispersion"), "instructions in the", em("Input Values"), "tab for details about entering the dispersion parameter in this application."),
          h3("Effect size"),
          h4("The standard effect size measure for Poisson family regression models is the rate ratio (RR), described above and calculated as\\(\\ e^{b}\\). For a one unit increase in the predictor variable, we expect the predicted outcome to be", em("multiplied"), "by\\(\\ e^{b}\\). This measure is constant across all values of the predictor."),
          h4("An alternative measure of effect size is the", a("standardized mean difference (SMD)", href="https://en.wikipedia.org/wiki/Effect_size#Difference_family:_Effect_sizes_based_on_differences_between_means", target="_blank"), ". A common SMD effect size measure for group differences is Cohen's d (Cohen, 1988). The Poisson regression model is non-linear, so any linear effect (such as a difference) depends on the specific predictor values at which it is examined. In addition, Poisson regression models display non-constant variance (heteroscedasticity) across values of the predictor. Here, the SMD effect size is the difference in predicted outcome from \\(\\ X=0\\) to \\(\\ X=1\\), divided by the standard deviation at \\(\\ X=0\\). This is similar to the definition of effect size for other non-linear models such as logistic regression (Agresti, 2013) as well as Glass' delta for group differences (Glass, 1976)."),
          h3("Centering predictors"),
          h4("This application will provide effect sizes for a nonlinear Poisson-family regression model. Specifically, it provides the constant exponential effect size and the standardized mean difference (SMD) between\\(\\ X = 0\\) and\\(\\ X = 1\\). The SMD will be most informative if values between 0 and 1 are meaningful for the predictor. If your predictor does not having meaningful values between 0 and 1 (e.g., the predictor ranges from 1 to 5), you should", em("mean center"), "the predictor for analysis (Dalal & Zickar, 2012). This results in a predictor mean of 0, so the effect size reflects the change from the mean of the predictor to 1 unit above the mean."),
          h4("Additionally, the effect size calculated here assumes that all other predictors in the model are equal to 0. If the other predictors do not already have meaningful 0 values, you should mean center them as well.")
          )),
        
          tabPanel("3. Input values -->",
          wellPanel(
          h3("SAS output examples for reference. Required values are circled in red."),
          h4("This is real SAS output, showing which values are required"),
          h4(a("Poisson regression example", href="PoissonExample.pdf", target="_blank"), "--- Exponential effect = 1.307, SMD = 0.39"),
          h4(a("Overdispersed Poisson regression example", href="OverdispersedPoissonExample.pdf", target="_blank"), "--- Exponential effect = 1.307, SMD = 0.229"),
          h4(a("Negative binomial regression example", href="NegativeBinomialExample.pdf", target="_blank"), "--- Exponential effect = 1.481, SMD = 0.388") 
          ),
          wellPanel(
          h3("Regression model"),
          h4("Choose between Poisson regression, overdispersed Poisson regression, and negative binomial regression. The default is Poisson regression."),
          h3("Intercept coefficient and standard error"),
          h4("The (unstandardized) regression coefficient or\\(\\ b\\) for the intercept or constant term, and its standard error."),
          h3("Effect of interest coefficient and standard error"),
          h4("The (unstandardized) regression coefficient or\\(\\ b\\) for the effect of interest, and its standard error."),
          h3("Dispersion -- very important"),
          h4("Only required for overdispersed Poisson regression or negative binomial models."),
          h4("For overdispersed Poisson regression, dispersion is the square root of phi. This corresponds to the (Scale) parameter from SPSS GENLIN or the", em("square"), "of the Scale parameter from SAS GENMOD or R GLM."),
          h4("For negative binomial regression, dispersion is alpha. This corresponds to the (negative binomial) parameter from SPSS, the dispersion parameter from SAS, or 1/theta from R GLM.NB."),
          h3("Confidence interval"),
          h4("Select the confidence interval percentage you want. Options are 80, 90, 95 (default), and 99, corresponding to type I error rates of 0.2, 0.1, 0.05, and 0.01, respectively."),
          h3("Monte Carlo replications"),
          h4("The number of random number draws used in order to calculate Monte Carlo confidence intervals for the effect size. Larger values give more precise estimates but may require more time. The default is 2000."),
          h3("Seed"),
          h4("Monte Carlo methods use random number generation. To obtain the exact same results each time you use this app, use the same seed value each time. You can change the value, but you will need to use the same value to get the same results."))),
    
        tabPanel("4. Effect sizes -->", 
                 # wellPanel(
                 # h3("Means and standard deviations"),
                 # h4(textOutput("output1")),
                 # h4(textOutput("output2"))),
                 wellPanel(
                 h3(textOutput("output_expeff")),
                 h4(htmlOutput("output3")),
                 h4(textOutput("output4")),
                 h4(htmlOutput("expsig")),
                 hr(),
                 h4("Effect size = 1 indicates", em("no effect.")),
                 h4("Values greater than 1 indicate that the outcome mean when predictor = 1 is HIGHER than the outcome mean when predictor = 0."),
                 h4("Values less than 1 indicate that the outcome mean when predictor = 1 is LOWER than the outcome mean when predictor = 0.")),
                 wellPanel(
                 h3(textOutput("output_coheff")),
                 h4(htmlOutput("output5")),
                 h4(textOutput("output6")),
                 h4(htmlOutput("cohsig")),
                 hr(),
                 h4("Effect size = 0 indicates", em("no effect.")), 
                 h4("Positive values indicate that the outcome mean when predictor = 1 is HIGHER than the outcome mean when predictor = 0."),
                 h4("Negative values indicate that the outcome mean when predictor = 1 is LOWER than the outcome mean when predictor = 0."))),
        
        tabPanel("5. Effect size plot -->", 
                 wellPanel(
                 h3("Nonlinear relationship between predictor and count outcome"),
                 h4("This is the relationship between the predictor and the outcome, based on your input values"),
                 h4(textOutput("output1")),
                 h4(textOutput("output2")),
                 h4("Error bars  show plus/minus 1 standard deviation at \\(\\ X = 0\\) and \\(\\ X = 1\\)"),
                 h4(textOutput("expeff_plot")),
                 h4(textOutput("coheff_plot")),
                 plotOutput("poiplot", width = "600px"))#, 
                 # wellPanel(
                 # h3("Confidence intervals for effect sizes, relative to", em("no effect"), "line (blue)"),
                 # splitLayout(cellWidths = c("300px", "300px"), plotOutput("expeffplot"), plotOutput("coheffplot")))
                 ),
        
        tabPanel("6. Distribution plots -->", 
                 br(),
                 wellPanel(
                 h3("Monte Carlo sampling distribution of SMD effect"),
                 h4("Red line = estimate. Blue dashed lines = confidence limits."),
                 plotOutput("cohplot", width = "600px")), 
                 wellPanel(
                 h3("Monte Carlo sampling distribution of exponential effect"),
                 h4("Red line = estimate. Blue dashed lines = confidence limits."),
                 plotOutput("expplot", width = "600px"))),
         
        tabPanel("7. Table -->", 
                 tableOutput("table"), 
                 downloadButton("downloadData", "Download"),
                 h4("Downloadable file in Excel format")
                 ),
        
        tabPanel("8. References",
          br(),
          h3("Statistical references"),
          h4("Agresti, A. (2013). Categorical data analysis. John Wiley & Sons."),
          h4("Cameron, A. C. and Trivedi, P. K. (1998). Regression analysis of count data, New York: Cambridge University Press."),
          h4("Cohen, J. (1988). Statistical power analysis for the behavioral sciences. Hillsdale, NJ: Lawrence Erlbaum."),
          h4("Coxe, S., West, S. G., & Aiken, L. S. (2009). The analysis of count data: A gentle introduction to Poisson regression and its alternatives. Journal of personality assessment, 91(2), 121-136."),
          h4("Dalal, D. K., & Zickar, M. J. (2012). Some common myths about centering predictor variables in moderated multiple regression and polynomial regression. Organizational Research Methods, 15(3), 339-362."),
          h4("Gardner, W., Mulvey, E. P., & Shaw, E. C. (1995). Regression analyses of counts and rates: Poisson, overdispersed Poisson, and negative binomial models. Psychological bulletin, 118(3), 392."),
          h4("Glass, G. V. (1976). Primary, secondary, and meta-analysis of research. Educational Researcher, 5(10), 3-8."),
          h4("McCullagh, P. (1984). Generalized linear models. European Journal of Operational Research, 16(3), 285-292."),
          h3("R and Shiny references"),
          h4("Chang, W., Cheng, J., Allaire, J.J., Xie, Y., and McPherson, J. (2017). shiny: Web Application Framework for R. R package version 1.0.5.
  https://CRAN.R-project.org/package=shiny"),
          h4("R Core Team (2018). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL
  https://www.R-project.org/.")
          )
        
      )
      
    )
      
  )
  
))