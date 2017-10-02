# This script calculates a conditional probability for all bird species given the
# presence of another bird species in a set of observations (e.g. an eBird Checklist).
# The script also calculated the individual probability of observing each bird species.
# Data was calculated using the eBird Basic dataset of all birds observed 
# in the US from Jan 2016 to Dec 2016.



# Read in observations of Checklist idetifiers and Species names from eBird dataset
#Each observation containcs a Checklist identifier and Species name. 
library(readr)
SampleData <- read_csv("~/Downloads/ebd_US_201601_201612_relMay-2017_checklist_species.txt")

# Sort input data by checklist identifier and extract unique species names and checklist identifiers
print("Sorting ...")
OrderedSampleData <- SampleData[order(SampleData$`SAMPLING EVENT IDENTIFIER`, method = "radix"),]
print("Getting species list ...")
SpeciesList <- unique(SampleData$`COMMON NAME`);
print("Getting list of checklists ...")
Checklist <- unique(OrderedSampleData$`SAMPLING EVENT IDENTIFIER`)

# Determine the number of checklists and species in the dataset
NumberOfSpecies <- length(SpeciesList)
NumberOfChecklists <- length(Checklist)

# Initialize the conditional observations matrix with 0s and name the rows and columns.
# Each row will be a list of the number of times each "column" bird species was 
# recorded on the same checklist as the "row" species.
OutputMatrix <- matrix(0L, nrow = NumberOfSpecies, ncol = NumberOfSpecies)
colnames(OutputMatrix) <- SpeciesList
rownames(OutputMatrix) <- SpeciesList

# Set first checklist identifier and start species list for that checklist
WorkingChecklist <- OrderedSampleData$`SAMPLING EVENT IDENTIFIER`[1]
TempVector <- OrderedSampleData$`COMMON NAME`[1]

# Looping through the observations to detemine list of birds in each checklist
print("Generating output matrix ...")
for(i in 2:nrow(OrderedSampleData)){
  if (i %% 100000 == 0){
    print(100 * i / nrow(OrderedSampleData))
  }
  # Retreive checklist ID
  ChecklistID <- OrderedSampleData$`SAMPLING EVENT IDENTIFIER`[i]
  
  # If continuing to read in the same checklist, 
  # add new species to the species list for the current checklist.
  if(ChecklistID == WorkingChecklist){
    TempVector <- append(TempVector, OrderedSampleData$`COMMON NAME`[i])
  }else{
    
    # If reached the end of the checklist save the species list to the conditional observation matrix
    L <- length(TempVector)
    for(j in 1:L){
      for(k in 1:L){
        OutputMatrix[TempVector[j], TempVector[k]] <- OutputMatrix[TempVector[j], TempVector[k]]+1
      }
    }
    
    # Start on a new checklist
    WorkingChecklist <- ChecklistID
    TempVector <- OrderedSampleData$`COMMON NAME`[i]
  }
}

# Calculate probabilities by dividing by "row" species occurrence
print("Normalizing output matrix...")
for(i in 1:length(SpeciesList)){
  OutputMatrix[SpeciesList[i],] <- OutputMatrix[SpeciesList[i],] / OutputMatrix[SpeciesList[i], SpeciesList[i]]
}

# Determine general probability of observing a species
print("Calculating individual probabilites...")
IndividualProb <- table(OrderedSampleData$`COMMON NAME`)
IndividualProb <- IndividualProb / length(Checklist)
