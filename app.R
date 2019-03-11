library(shiny)
library(shinyjs)

# strtoi(strsplit("2,3,4,5", split = ",")[[1]])

basePriorDistribution <- c("Standard uniform", "Standard normal", "Randomly generated")
parameterType <- c("bernoulliParameterValue", "poissonParameterValue", "geometricParameterValue")
correspondingInputBoxes <- list("bernoulliParameterValue" = c("_numberHeads", "_totalFlips"), "poissonParameterValue" = "_csv", "geometricParameterValue" = "_csv")

xSubdivisions <- c()
subdivisionProbability <- c()
initialSubdivisions <- 100
likelihoodFormula <- 1 # bernoulli 

# N = total flips, z = heads
bernoulliLikelihood <- function(theta, N, z) {
  return (theta^z * theta^(N - z))
}

# lambda is poisson parameter, outcome is a string like "1,2,3,4"
poissonLikelihood <- function(lambda, outcomeString) {
  outcomes <- strtoi(strsplit(outcomeString, split = ",")[[1]])
  return ((prod(exp(-1 * lambda * outcomes)) * product(lambda^outcomes))/(product(factorial(outcomes))))
}

# lambda is poisson parameter, outcome is a string like "1,2,3,4"
geometricLikelihood <- function(theta, probabilityString) {
  
}

setInitialPriorDistribution <- function(likelihoodFormula, subdivisions, densityFunction) {
  from <- 0
  to <- 0
  if (likelihoodFormula == 2) { # poisson
    from <- 0
    to <- 100
  } else { # bernoulli or geometric
    from <- 0
    to <- 1
  }
  xSubdivisions <<- seq(from, to, length.out = subdivisions)
  subdivisionProbability <<- densityFunction(xSubdivisions)
  subdivisionProbability <<- subdivisionProbability/sum(subdivisionProbability)
}

setInitialPriorDistribution(1, initialSubdivisions, dunif)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Comparison of likelihood effect on prior and posterior probabilities using grid approximation"),
  sidebarLayout(
    sidebarPanel(helpText("A demonstration from the textbook Doing Bayesian Data Analysis of chapter 5."), 
                 br(),
                 numericInput("subdivisions", 
                              "Number of parameter subdivisions",
                              value = initialSubdivisions),
                 strong("Set initial prior distribution"),
                 radioButtons(inputId = "priorFormulas",
                              label = "Distribution of prior",
                              choices = list(
                                "Standard uniform" = 1,
                                "Standard normal" = 2,
                                "Randomly generated" = 3
                                ),
                              selected = 1
                              ),
                 actionButton("setPrior", "Set Prior distribution"),
                 br(),
                 br(),
                 radioButtons(inputId = "likelihoodFormulas", 
                              label = "Likelihood function to use",
                              choices = list(
                                "Bernoulli (Coin flipping example) [0, 1]" = 1,
                                "Poisson [0, inf)" = 2,
                                "Geometric [0, inf)" = 3
                              ),
                              selected = 1
                              ),
                 numericInput(inputId = parameterType[1], label = "Bernoulli Parameter Value", value = 0.5),
                 numericInput(inputId = paste(parameterType[1], "_numberHeads", sep = ''), label = "Number of heads", value = 0.5),
                 numericInput(inputId = paste(parameterType[1], "_totalFlips", sep = ''), label = "Total flips", value = 0.5),
                 numericInput(inputId = parameterType[2], label = "Poisson Parameter Value", value = 3),
                 numericInput(inputId = paste(parameterType[2], "_csv", sep = ''), label = "Comma seperated outcomes", "2,3,4,2"),
                 numericInput(inputId = parameterType[3], label = "Geometric Parameter Value", value = 3),
                 numericInput(inputId = paste(parameterType[3], "_csv", sep = ''), label = "Comma seperated probabilities", "2,3,4,2"),
                 
                 actionButton("update", "Compute posterior")
    ),
    mainPanel(
      plotOutput(outputId = "priorPlot"), 
      plotOutput(outputId = "likelihoodPlot"), 
      plotOutput(outputId = "posteriorPlot")
    )
  )
)

firstRunPrior <- TRUE
firstRunLikelihood <- TRUE
server <- function(input, output, session) {
  # Shows the specific input box for the likelihood chosen
  # Using shinyjs
  observe({
    if (firstRunPrior) {
      print("first run")
      output$priorPlot <- renderPlot({
        plot(xSubdivisions, subdivisionProbability)
      })
      firstRunPrior <<- FALSE
    }
    currentParameter = parameterType[strtoi(input$likelihoodFormulas)]
    show(currentParameter)
    
    # shows the boxes needed through searching the list object correspondingInputBoxes
    for (parameterToShow in correspondingInputBoxes[currentParameter]) {
      show(paste(currentParameter, parameterToShow, sep = ''))
    }
    
    # hides the unneeded boxes using the list object correspondingInputBoxes
    for (parameterToHide in setdiff(parameterType, c(currentParameter))) {
      hide(parameterToHide)
      for (boxesToHide in correspondingInputBoxes[parameterToHide]) {
        # sometimes boxesToHide is a vector with 2 or more elements
        for (eachBoxToHide in boxesToHide) { 
          hide(paste(parameterToHide, eachBoxToHide, sep = ''))
        }
      }
    }
  })
  
  #updatePrior <- eventReactive(input$setPrior, {
  updatePrior <- observeEvent(input$setPrior, {
    selectedLikelihood <- strtoi(input$likelihoodFormulas)
    selectedSubdivisions <- input$subdivisions
    if (input$priorFormulas == "1") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dunif)
    } else if (input$priorFormulas == "2") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dnorm)
    } else if (input$priorFormulas == "3") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, runif)
    }
    
    output$priorPlot <- renderPlot({
      plot(xSubdivisions, subdivisionProbability)
    })
  })
  
  updateLikelihood <- eventReactive(input$likelihoodFormulas, {
  })
  
  output$likelihoodPlot <- renderPlot({
    if (firstRunLikelihood) {
      firstRunLikelihood <<- FALSE
    } else {
      updateLikelihood()
    }
  })
  
  output$posteriorPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = 100)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
}

shinyApp(ui, server)