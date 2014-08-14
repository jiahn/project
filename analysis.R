library(RUnit)
library(ggplot2)
errMsg <- function(err) print(err)
#load('reformatted-ling.data')

l.data = read.table("binary-ling-data.data", stringsAsFactors = FALSE) #loads data (54MB)
names(l.data) = l.data[1,]

full.data = l.data[-1,] #full cleaned data
full.data[is.na(full.data)] = 0 #sets all NA to 0..?
qs.data = full.data[,-c(1:4)] #only answers to questions
qs.matrix = unname(data.matrix(qs.data))

noloc.data = full.data[,-c(1:4)]

qs.hclust = dist(qs.data[1:1000,], method = 'euclidean')
qs.hclust.labels = as.vector(cutree(hclust(qs.hclust), k=2))


#input sample problems to test and observe

p064.dat = unname(data.matrix(qs.data[,99:109]))
p093.dat = unname(data.matrix(qs.data[,311:315]))
p094.dat = unname(data.matrix(qs.data[,316:321]))
p119.dat = unname(data.matrix(qs.data[,454:457]))
p109.dat = unname(data.matrix(qs.data[,414:417]))

#plots all lat vs long, very cluttered
#plot(qs.data[,c(2,1)])

#generates map of america, similar to provided graphs
# par(mfrow = c(2,2))
# plot(qs.data[which(p119.dat[,1] == 1),c(2,1)])
# plot(qs.data[which(p119.dat[,2] == 1),c(2,1)])
# plot(qs.data[which(p119.dat[,3] == 1),c(2,1)])
# plot(qs.data[which(p119.dat[,4] == 1),c(2,1)])


#plots lattitude vs longitude for a given problem data set.
#only for all responses, no responses are not pointed
plotResp = function(problem.dat) {
  
  i = ncol(problem.dat)
  par(mfrow = c(i/2,i/2))
  
  for (i in 1:i) {
    plot(qs.data[which(problem.dat[,i] == 1),c(2,1)])
  }
  
}

plotNoResp = function(problem.dat) {
  
  i = ncol(problem.dat)
  par(mfrow = c(i/2,i/2))
  
  for (i in 1:i) {
#     latlongs = qs.data[which(problem.dat[,i] == 0),]
#     lat = mean(as.numeric(latlongs[,1]))
#     long = mean(as.numeric(latlongs[,2]))
    
    plot(qs.data[which(problem.dat[,i] == 0),c(2,1)])
#     legend(0,50, c("lat","long"))
    
  }
  
} 

#outputs cluster labels for the first 1000 obs of a given problem
clusterLabels = function(problem.dat) {
  
  prob.hclust = dist(problem.dat[1:1000,], method = 'euclidean')
  prob.labels = cutree(hclust(prob.hclust), k = 25)
  return(prob.labels)
  
}

plotClusterLabels = function(problem.dat, main) {
  
  plot(clusterLabels(problem.dat), main = main)
  
}

plotClust = function(problem.dat, main) {
  
  prob.hclust = dist(problem.dat[1:500,], method = 'euclidean')
  plot(hclust(prob.hclust), main = main)
  
}

#PredictAns
#Gives prediction values (0~1) of all responses (on average) based on inputted pXXX.data.
#Use pXXX.dat[,Y] - where XXX is the question and Y is the answer choice (a, b, c, d...)
#Returns fitted values (likelihood of accurately predicting 'prediction' from 'input')
predictAns = function(input, prediction) {
  
  pred.lm = lm(prediction ~ input)
  pred = predict(pred.lm, data.frame(df = input), interval = "prediction")
  plot(pred)
  
  return(pred)
  
}


#comparing pcplots
comparePCplot = function(data1, data2) {
  x = princomp(data1)
  y = princomp(data2)
  
  par(mfrow = c(2,2))
  
  plot(x$scores, col = 'red')
  points(y$scores, col = 'blue')
}


########### test ############
plotScores = function(data,main) {
  
  pca = prcomp(data)
  
plot(pca$x[, 1],pca$x[,2], pch=16, col = 'red', main = main)
points(pca$x[, 2],pca$x[,3], pch=16, col = 'blue')
points(pca$x[, 1],pca$x[,3], pch=16, col = 'green')
abline(a=0, b=0,h=0,v=0)
}

plotLoadings = function(data,main) {
  
  pca = princomp(data)
  
plot(pca$loadings[, 1],pca$loadings[,2], pch=16, col = 'red', main = main)
points(pca$loadings[, 2],pca$loadings[,3], pch=16, col = 'blue')
points(pca$loadings[, 1],pca$loadings[,3], pch=16, col = 'green')
abline(a=0, b=0,h=0,v=0)
}