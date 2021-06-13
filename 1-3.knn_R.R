#### knn 모델짜보기 ####
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

nearest_neighbors = function(x, obs, k, FUN)
  
  # Check the number of observations is the same
  if (ncol(x) !=ncol(obs)){
    stop('Data must have the same number of variables')
  }
  
  dist = apply(x,1,FUN, obs)
  
  # Find closest
  
  library(shiny)
  runGitHub('Shiny','insooAI',ref='main')
library(shiny)
runGitHub('Shiny','insooAI',ref='main')
