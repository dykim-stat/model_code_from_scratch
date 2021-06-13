#### knn 모델짜보기 ####
#### Python -> R ####
# source : https://www.youtube.com/watch?v=ngLyX54e1LU&list=PLqnslRFeH2Upcrywf-u2etjdxxkL8nl7E&index=1




## 1. 모델구성 ##
euclidean_distance = function(x1,x2){
    return(sqrt(sum((x1-x2)^2)))
}

KNN = function(k=3, X_train, y_train, X_test){
  # k = 3
  # X_train = X_train
  # y_train = y_train
  # X_test = X_test
  predict_ = function(x){
    # compute distances
    distances = rep(0,nrow(X_train))
    for (i in 1:nrow(X_train)){
      distances[i] = euclidean_distance(x,X_train[i,])
    }
    
    # get k nearest samples, labels
    k_indices = order(distances)[1:k]
    k_nearest_labels = y_train[k_indices]
    
    # majority vote, most common class label
    most_common = which.max(table(k_nearest_labels))
    
    return(names(most_common))
  }
  
  predicted_labels = apply(X_test,1,predict_)
  return(predicted_labels)
}

## 2. test set 만들기 ##
data(iris)
idx = sample(1:nrow(iris),nrow(iris)*0.8)
X_train = iris[idx,1:4]
X_test = iris[-idx,1:4]
y_train = iris$Species[idx]
y_test = iris$Species[-idx]


## 3. 함수 적용해보기 ##
y_pred = KNN(k=3, X_train, y_train,X_test)
acc = mean(y_test==y_pred)
acc


## 4. 배운거 정리 & 느낀점 ##
python과의비교 = "
  np.sort : sort
  np.argsort : order
  
  파이썬에서는 list comprehension을 활용한 for문이 상당히 많다.
  이걸 R에서 다 구현하면 너무 느림....
  파이썬으로 짠 knn을 최대한 그대로 번역하는 느낌으로 해보려고 하였으며
  for문 하나는 apply로 대체. 유클리드 거리구하는것도 apply로 바꿔야 더 빠를듯.150개 밖에 안되는거도 1~2초 걸리는걸로봐서 데이터 커지면 답도없음
  파이썬에서 class 형태로 코딩하는걸 어떻게 R에서 구현해야할지 잘모르겠음. 앞으로 하다보면 감이 오겠지.
"
