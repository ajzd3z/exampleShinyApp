library(shiny)
library(shinyjs)

# reference variables for output
basePriorDistribution <- c("Standard uniform", "Standard normal", "Randomly generated")
parameterType <- c("bernoulliParameterValue", "poissonParameterValue", "geometricParameterValue")
correspondingInputBoxes <- list("bernoulliParameterValue" = c("_numberHeads", "_totalFlips"), "poissonParameterValue" = "_csv", "geometricParameterValue" = "_csv")

# Global Variables 
parameterSubdivisions <- c()
subdivisionProbability <- c()
likelihoodValues <- c()
initialSubdivisions <- 100
likelihoodFormula <- 1 # bernoulli 
firstRun <- TRUE
posteriorProbability <- c()

initialHeads <- 10
initialTotalFlips <- 40
exampleOutcomeString <- "3, 3, 4, 5, 6, 3, 2, 2, 1, 3, 7, 8, 6, 4, 4"
exampleProbabilityString <- "0.65, 0.7, 0.9, 0.3, 0.2"

# N = total flips, z = heads
bernoulliLikelihood <- function(theta, N, z) {
  return (theta^z * (1 - theta)^(N - z))
}

# lambda is vector of probabilities (in this case, parameterSubdivisions), outcome is a string like "1,2,3,4"
poissonLikelihood <- function(lambdas, outcomeString) {
  outcomes <- strtoi(strsplit(outcomeString, split = ",")[[1]])
  if (outcomes == 0 || length(outcomes) == 0) {
    stop()
  }
  
  # Stop gap solution
  if (length(outcomes) < length(lambdas)) {
    outcomes <- rep_len(outcomes, length.out = length(lambdas))
  }
  
  likelihood <- vector(length = length(lambdas))
  n <- length(outcomeString)
  
  # for each likelihood entry is a product of 
  for (lambda in 1:length(lambdas)) {
    # e^-n*lambda
    poisExp = exp(-1 * n * lambda)
    # lambda^sum(outcomes)
    lambdaPower = lambda^sum(outcomes)
    denominator = prod(factorial(outcomes))

    likelihood[x] = poisExp * lambdaPower / denominator
  }
  return(likelihood)
  # return ((prod(exp(-1 * lambda * outcomes)) * prod(lambda^outcomes))/(prod(factorial(outcomes))))
}

# lambda is poisson parameter, outcome is a string like "1,2,3,4"
geometricLikelihood <- function(theta, probabilityString) {
  
}

computePosterior <- function(likelihoodValues, priorValues) {
  print(likelihoodValues * priorValues)
  print(sum(likelihoodValues * priorValues))
  return((likelihoodValues * priorValues)/sum(likelihoodValues * priorValues))
}

setInitialPriorDistribution <- function(likelihoodFormula, subdivisions, densityFunction) {
  from <- 0
  to <- 0
  if (likelihoodFormula == 2) { # poisson
    from <- 1
    to <- 100
  } else { # bernoulli or geometric
    from <- 0
    to <- 1
  }
  parameterSubdivisions <<- seq(from, to, length.out = subdivisions)
  subdivisionProbability <<- densityFunction(parameterSubdivisions)
  subdivisionProbability <<- subdivisionProbability/sum(subdivisionProbability)
}

# initialize the default information 
setInitialPriorDistribution(1, initialSubdivisions, dunif)
likelihoodValues <- bernoulliLikelihood(parameterSubdivisions, initialTotalFlips, initialHeads)

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
                 numericInput(inputId = parameterType[1], label = "True Bernoulli Parameter Value", value = 0.5),
                 numericInput(inputId = paste(parameterType[1], "_numberHeads", sep = ''), label = "Number of heads", value = initialHeads),
                 numericInput(inputId = paste(parameterType[1], "_totalFlips", sep = ''), label = "Total flips", value = initialTotalFlips),
                 
                 numericInput(inputId = parameterType[2], label = "True Poisson Parameter Value", value = 3),
                 textInput(   inputId = paste(parameterType[2], "_csv", sep = ''), label = "Comma seperated outcomes", exampleOutcomeString),
                 
                 numericInput(inputId = parameterType[3], label = "True Geometric Parameter Value", value = 3),
                 textInput(   inputId = paste(parameterType[3], "_csv", sep = ''), label = "Comma seperated probabilities", exampleProbabilityString),
                 actionButton("setLikelihood", "Set Likelihood distribution"),
                 br(),
                 br(),
                 br(),
                 helpText("Use grid method to compute posterior"),
                 actionButton("updatePosterior", "Compute posterior"),
                 actionButton("posteriorToPrior", "Copy posterior to prior")
    ),
    mainPanel(
      plotOutput(outputId = "priorPlot"), 
      plotOutput(outputId = "likelihoodPlot"), 
      plotOutput(outputId = "posteriorPlot")
    )
  )
)

server <- function(input, output, session) {
  # Shows the specific input box for the likelihood chosen
  # Using shinyjs
  observe({
    if (firstRun) {
      #print("first run")
      output$priorPlot <- renderPlot({
        plot(parameterSubdivisions, subdivisionProbability, type = "h", lty = 1, lwd = 5, pch = 20, cex = 0.5, col = "blue")
      })
      
      output$likelihoodPlot <- renderPlot({
        plot(parameterSubdivisions, bernoulliLikelihood(parameterSubdivisions, initialTotalFlips, initialHeads), lty = 1, lwd = 2, pch = 20, cex = 0.5)
      })
      
      firstRun <<- FALSE
    }
    currentParameter = parameterType[strtoi(input$likelihoodFormulas)]
    show(currentParameter)
    
    # shows the boxes needed through searching the list object correspondingInputBoxes
    for (parameterToShow in correspondingInputBoxes[currentParameter]) {
      for (boxesToShow in parameterToShow) {
          show(paste(currentParameter, boxesToShow, sep = ''))
      }
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
  
  # Updates plot upon button click (couldn't get it working using eventReactive)
  updatePrior <- observeEvent(input$setPrior, {
    selectedLikelihood <- strtoi(input$likelihoodFormulas)
    selectedSubdivisions <- input$subdivisions
    
    if (length(parameterSubdivisions) == 0 ||
        selectedLikelihood < 1 || selectedLikelihood > 3 ||
        selectedSubdivisions <= 0) {
      stop()
    }
        
    if (input$priorFormulas == "1") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dunif)
    } else if (input$priorFormulas == "2") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dnorm)
    } else if (input$priorFormulas == "3") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, runif)
    } else {
      stop()
    }
    
    output$priorPlot <- renderPlot({
      plot(parameterSubdivisions, subdivisionProbability, pch = 20, lty = 2)
    })
  })
  
  updateLikelihood <- observeEvent(input$setLikelihood, {
    
    baseFormula <- input$likelihoodFormulas
    selectedLikelihood <- strtoi(input$likelihoodFormulas)
    selectedSubdivisions <- input$subdivisions
    
    # in case the parameter support set changes
    if (input$priorFormulas == "1") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dunif)
    } else if (input$priorFormulas == "2") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, dnorm)
    } else if (input$priorFormulas == "3") {
      setInitialPriorDistribution(selectedLikelihood, selectedSubdivisions, runif)
    } else {
      stop()
    }
        
    if (length(parameterSubdivisions) == 0 ||
        selectedLikelihood < 1 || selectedLikelihood > 3) {
      stop()
    }    
    if (baseFormula == "1") { # bernoulliLikelihood
      z <- input$bernoulliParameterValue_numberHeads
      N <- input$bernoulliParameterValue_totalFlips
      likelihoodValues <<- bernoulliLikelihood(parameterSubdivisions, N, z)
    } else if (baseFormula == "2") { # poissonLikelihood
      outcomeString <- input$poissonParameterValue_csv
      likelihoodValues <<- poissonLikelihood(parameterSubdivisions, outcomeString)
      print(poissonLikelihood(parameterSubdivisions, outcomeString))
    } else if (baseFormula == "3") { # geometricLikelihood
      stop()
    } else {
      stop()
    }
    
    output$likelihoodPlot <- renderPlot({
      plot(parameterSubdivisions, likelihoodValues, lty = 1, pch = 20)
    })
  })
  
  updatePosteriorGraph <- observeEvent(input$updatePosterior, {
    posteriorProbability <<- computePosterior(likelihoodValues, subdivisionProbability)
    output$posteriorPlot <- renderPlot({
      plot(parameterSubdivisions, posteriorProbability, lty = 1, pch = 20)
    })
  })
  
  copyPosteriortoPrior <- observeEvent(input$posteriorToPrior, {
    print(subdivisionProbability == posteriorProbability)
    subdivisionProbability <<- posteriorProbability
    output$priorPlot <- renderPlot({
      plot(parameterSubdivisions, subdivisionProbability, lty = 1, pch = 20)
    })
  })
}

shinyApp(ui, server)