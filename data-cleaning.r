# create a subset of the data in lingData.txt where all observations that
# omitted every question have been removed. Store the **number** of
# observations that you omitted as the variable <n.no.response>

lingData = read.table('lingData.txt', stringsAsFactors = FALSE)
rows = nrow(lingData)
cols = 71

  
  rowsToRemove = c()
  colOmits = c()
  rowOmits = c()
  
  for (rows in 2:rows) {
    
  sumRow = sum(as.numeric(lingData[rows,5:cols]))
  
  rowOmit = length(which(lingData[rows,5:cols] == 0))
  rowOmits = c(rowOmits,rowOmit)
  
    if (sumRow == 0) {
    rowsToRemove = c(rowsToRemove,rows)
    }
  
  }
  
  for (cols in 5:cols) {
    
  colOmit = length(which(lingData[2:rows,cols] == 0))
  colOmits = c(colOmits,colOmit)
  
  }

noOmitLingData = lingData[-rowsToRemove,]
n.no.response = length(rowsToRemove)

# plot a histogram of the number of omitted responses for each observation
# after removing observations that omitted all questions

omitvals = rowOmits[-which(rowOmits == 67)]
hist(omitvals, xlab = "questions ommited", main = "Number Ommited Questions")

# using your subset (with observations that responded to no questions
# removed) find the 99th percentile cutoff for number of questions
# omitted. Remove all observations with more than this number of omitted
# questions.

newRowOmits = rowOmits[-which(rowOmits == 67)]

non.response.cutoff = quantile(newRowOmits, 0.995)
non.response.cutoff = as.numeric(non.response.cutoff)

newRowCuts = which(newRowOmits > non.response.cutoff) + 1

cleanedData = noOmitLingData[-newRowCuts,]

# save the subset of remaining observations in a file named
# "ling-data-clean.data" 

write.table(cleanedData, file="ling-data-clean.data", row.names=F)
