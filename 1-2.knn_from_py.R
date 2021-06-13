#### knn 모델짜보기 ####
#### Python -> R ####
# source : https://www.youtube.com/watch?v=ngLyX54e1LU&list=PLqnslRFeH2Upcrywf-u2etjdxxkL8nl7E&index=1


## test set 만들기 ##
data(iris)
idx = sample(1:nrow(iris),nrow(iris)*0.8)
X_train = iris[idx,1:4]
X_test = iris[-idx,1:4]
y_train = iris$Species[idx]
y_test = iris$Species[-idx]


## 모델구성 ##
euclidean_distance = function(x1,x2){
    return(sqrt(sum((x1-x2)^2)))
}

KNN = function(k=3, X_train, y_train, X_test)

k = 3
X_train = X_train
y_train = y_train
X_test = X_test

x = X_test[1,]
predict = function(x){
  distances = rep(0,nrow(X_train))
  for (i in 1:nrow(X_train)){
    distances[i] = euclidean_distance(x,X_train[i,])
  }
  k_indices = 
  
  
}