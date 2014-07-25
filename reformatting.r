library(RUnit)
errMsg <- function(err) print(err)
load('reformatting-tests.rda')

# Implement the makeBinary function.
# args:
# <response.row>: a vector of integers giving the response values for each
#   question 
# <n.responses>: a vector of integers (same length as <response.row>)
#   indicating the number of possible responses for each question
#
# returns:
# a binary vector that reformats the responses of <response.row> as
# described in project1.pdf

makeBinary <- function(response.row, n.responses) {
  
  i = length(n.responses)
  b = length(response.row)
  s = sum(as.numeric(n.responses))
  empty = rep(0, s)
  g = 0
  
  for (b in 1:b) {
    a = response.row[b] + g
    empty[a] = 1
    g = g + n.responses[b]
  }

  return(empty)

}


tryCatch(checkEquals(make.binary.test1, makeBinary(make.binary.rr1,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

tryCatch(checkEquals(make.binary.test2, makeBinary(make.binary.rr2,
                                                   make.binary.nr)),
         error=function(err) errMsg(err))

# use your "makeBinary" function to reformat your "ling-data-clean.data"
# dataset. Store this as a dataframe with variable names and order **as
# indicated in project1.pdf**. Save this dataframe as the file
# "binary-ling-data.data".
    
cleanDat = read.table("ling-data-clean.data", stringsAsFactors = FALSE)
colnames = cleanDat[2,5:71]
cleanDat = cleanDat[-c(1,2),]

b = 71
maxQS = c()

for (b in 5:b) {
  maxQS = c(maxQS,max(as.numeric(cleanDat[,b])))
}

#maxQS = lapply(as.numeric(scape), max)
qlist = c()
i = length(maxQS)
# q = c(50:121)
# v = length(q)
n = nrow(cleanDat)

# for (v in 1:v) {
#   u = q[v]
  
  for (i in 1:i) {
  
    m = maxQS[i]
  
    for (m in 1:m) {
      qlist = c(qlist, paste(colnames[i], ".", m, sep = ""))

    }
  
  }

# }

dt <- as.data.frame(
  matrix(nrow = n, ncol = length(qlist) + 6,
         dimnames = list(NULL, c("ID", "CITY", "STATE", "ZIP", "lat", "long", qlist))))

#newframe = data.frame(dimnames = list(NULL, c("ID", "STATE", "ZIP", "lat", "long", qlist)))

for (n in 1:n) {
  
dt[n,1:4] = cleanDat[n,1:4]
dt[n,5:6] = cleanDat[n,72:73]
dt[n,7:ncol(dt)] = makeBinary(as.numeric(cleanDat[n,5:71]), maxQS)

}