# Function plots top ten most likely birds seen given the observation of an input species
# Inputs: ConditionalProb is a matrix of conditional probability relationships between bird species
#         IndividualProb is a table of the likelihood of observing each species
#         SpeciesName is a string of format "House Finch" on which the output is conditioned
#         NumSpecies is and integer of the number of species to plot

library("plotly")
ProbableSpecies <- function(ConditionalProb, IndividualProb, SpeciesName, NumSpecies) {
  
  # Check that the input species exists in the dataset/check for misspellings
  if(!(SpeciesName %in% names(IndividualProb))){
    return("Species not in list of observations")
  }
  
  # Determine the ten most likely species to when also observing the input species
  SpeciesVector <- ConditionalProb[SpeciesName, ]
  NewProb <- head(sort(SpeciesVector, TRUE),NumSpecies+1)
  NewProb <- NewProb[2:(NumSpecies+1)]
  PriorProb <- as.vector(IndividualProb[names(NewProb)])
  TopSpecies <- names(NewProb)
  NewProb <- as.vector(NewProb)
  
  # Create dataframe containing old and new probabilities
  CompareSpecies <- data.frame(NewProb, PriorProb, TopSpecies)
  CompareSpecies$TopSpecies <- reorder(x = CompareSpecies$TopSpecies, X = CompareSpecies$NewProb, FUN = sum)
  
  # Plot the prior and updated probability for the top ten 
  plotlabel <- paste("eBird observation probability given", SpeciesName, "observation")
  p <- plot_ly(CompareSpecies, x = CompareSpecies$PriorProb, y = CompareSpecies$TopSpecies, 
               name = "Prior probability", type = 'scatter',
               mode = "markers", marker = list(color = "#9CA8AB"))
  p <- add_trace(p, x = CompareSpecies$NewProb, y = CompareSpecies$TopSpecies, 
                 name = "New probability", type = 'scatter', mode = "markers", 
                 marker = list(color = "#33D1FF"))
  p <- layout(p, title = plotlabel,
              xaxis = list(title = "Probability of observation"), yaxis = list(title = ""),
              margin = list(l = 175))
  return(p)
}
