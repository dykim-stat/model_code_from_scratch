#### 1. knn 모델짜보기 ####
#### Language : R ####
# source : https://anderfernandez.com/en/blog/code-knn-in-r/

## 1. 모델 구성 ##

euclidean_distance = function(a,b){
  # we check that they have the same number of observation
  if (length(a) == length(b)){
    sqrt(sum((a-b)^2))
  } else {
    stop('Vectors must be of the same length')
  }
}
# euclidean_distance(1:10,11:20)


## Find the k nearest neighbors
nearest_neighbors = function(x, obs, k, FUN){
  
  # Check the number of observations is the same
  if (ncol(x) !=ncol(obs)){
    stop('Data must have the same number of variables')
  }
  
  dist = apply(x,1,FUN, obs)
  
  # Find closest
  distances = sort(dist)[1:k]
  neighbor_ind = which(dist %in% distances)
  
  if (length(neighbor_ind) != k){
    warning(
      paste('Several variables with equal distance. Used k:',length(neighbor_ind))
    )
  }
  
  ret = list(neighbor_ind,distances)
  return(ret)
}


x = iris[1:nrow(iris)-1,]
obs = iris[nrow(iris),]
ind = nearest_neighbors(x[,1:4], obs[,1:4],3,euclidean_distance)[[1]]
as.matrix(x[ind,1:4])
# warning이 잘 작동함을 알 수 있다.

## Prediction of the knn algorithm in classification problems
knn_prediction = function(x,y){
  
  groups = table(x[,y])
  pred = groups[groups==max(groups)]
  return(pred)
}
knn_prediction(x[ind,], 'Species')  
