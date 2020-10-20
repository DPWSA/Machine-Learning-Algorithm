#--------------

# @Authour Nikhil N Pandarge

#--------------

#Loading Datasets:

CustomerData <- read.csv("Wholesale customers data.csv")

Customerdata <- data.frame(CustomerData)

summary(Customerdata)

sum(is.na(Customerdata))

top.n.custs <- function (Customerdata,cols,n=5)
{
  #Initialize a vector to hold customers being removed
  idx.to.remove <-integer(0)
  
  for (c in cols) # For every column in the data we passed to this function
  {
    col.order <-order(Customerdata[,c],decreasing=T) #Sort column "c" in descending order (bigger on top)
    #Order returns the sorted index (e.g. row 15, 3, 7, 1, ...) rather than the actual values sorted.
    
    idx <-head(col.order, n)#Take the first n of the sorted column C to
    idx.to.remove <-union(idx.to.remove,idx)
    #combine and de-duplicate the row ids that need to be removed
    
  }
  
  #Return the indexes of customers to be removed
  return(idx.to.remove) 
}

#How Many Customers to be Removed?
top.custs <-top.n.custs(Customerdata, cols = 1:5,n=5)
length(top.custs) 

#Examine the customers
Customerdata[top.custs,]  #Exammine the customers

#Remove the Customers from the dataset
data.rm.top<-Customerdata[-c(top.custs),] 

#Examine summary stats for the remaining data
print(summary(data.rm.top))

set.seed(76964057) #Set the seed for reproducibility
#Try K from 2 to 20
rng<-2:20

#Number of times to run the K Means algorithm
tries <-100

#Set up an empty vector to hold all of points
avg.totw.ss <-integer(length(rng))
avg.totb.ss <- integer(length(rng))
avg.tot.ss <- integer(length(rng))

# For each value of the range variable
for(v in rng){
  
  #Set up an empty vectors to hold the tries
  v.totw.ss <-integer(tries)
  b.totb.ss <- integer(tries)
  tot.ss <- integer(tries)
  
  #Run kmeans
  for(i in 1:tries){
    k.temp <-kmeans(data.rm.top,centers=v)
    
    #Store the total withinss
    v.totw.ss[i] <-k.temp$tot.withinss
    
    #Store the betweenss
    b.totb.ss[i] <- k.temp$betweenss
    
    #Store the total sum of squares
    tot.ss[i] <- k.temp$totss
  }
  
  #Average the withinss and betweenss
  avg.totw.ss[v-1] <-mean(v.totw.ss)
  avg.totb.ss[v-1] <-mean(b.totb.ss)
  avg.tot.ss[v-1] <- mean(tot.ss)
}

plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

plot(rng,avg.totb.ss,type="b", main="Total between SS by Various K",
     
     ylab="Average Total Between Sum of Squares",
     xlab="Value of K")

#Plot the ratio of betweenss/total ss and withinss / total ss for evaluation
plot(rng,avg.totb.ss/avg.tot.ss,type="b", main="Ratio of between ss / the total ss by Various K",
     ylab="Ratio Between SS / Total SS",
     xlab="Value of K")
abline(h=0.85, col="red")

plot(rng,avg.totw.ss/avg.tot.ss,type="b", main="Ratio of within ss / the total ss by Various K",
     ylab="Ratio Between SS / Total SS",
     xlab="Value of K")
abline(h=0.15, col="red")

#Create the best number of clusters, Remove columns 1 and 2
n <- 3

#return(as.integer(n))
k <-kmeans(data.rm.top[,-c(1,2)], centers=n) 


#Display&nbsp;cluster centers
print(k$centers)

#Give a count of data points in each cluster
print(table(k$cluster))

#Generate a plot of the clusters
library(cluster)
clusplot(data.rm.top, k$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

