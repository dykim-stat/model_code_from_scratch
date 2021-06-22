#### 3. Logistic Regression ####
#### Language : R ####
# source : https://towardsdatascience.com/logistic-regression-from-scratch-in-r-b5b122fd8e83

## 1. 모델구성 ##

## sigmoid function, inverse of logit
sigmoid <- function(z) 1 / (1 + exp(-z))

## cost function
cost <- function(theta, X, y){
  m = length(y)
  h = sigmoid(X %*% theta)
  J = (t(-y) %*% log(h) - t(1-y) %*% log(1-h)) / m
  return(J)
}

## gradient function
grad <- function(theta, X, y){
  m = length(y)
  
  h = sigmoid(X %*% theta)
  grad = (t(X) %*% (h - y)) / m
  return(grad)
}


logisticReg <- function(X, y){
  # remove NA rows
  X = na.omit(X)
  y = na.omit(y)

  # add bias term and convert to  matrix
  X = as.matrix(cbind('bias'=1,X))
  y = as.matrix(y)
  
  # initialize theta
  theta = matrix(rep(0,ncol(X)),ncol(X))
  
  # use the optim function to perform gradient descent
  costOpti = optim(theta, fn = cost, gr = grad, X = X, y = y)
  
  return(costOpti$par)
}

## probability of getting 1
logisticProb <- function(theta, X){
  X = na.omit(X)
  
  # add bias term
  X = as.matrix(cbind('bias'=1,X))
  return(sigmoid(X %*% theta))
}

## y prediction
logisticPred <- function(prob){
  return(round(prob,0))
}


## 2. sample data 만들기
N = 200
D = 2   # dimension
K = 2   # number of classes
X = data.frame()
y = data.frame()

set.seed(56)

for (j in 1:K){
  # t,m are parameters of parametric equations x1,x2
  t = seq(0,1,length.out = N)
  m = rnorm(N, j+0.5, 0.25)
  
  Xtemp = data.frame(x1 = 3*t, x2 = m - t)
  ytemp = data.frame(matrix(j-1,N,1))
  
  X = rbind(X, Xtemp)
  y = rbind(y, ytemp)
}

data = cbind(y,X)
colnames(data) = c('label',colnames(X))

library(ggplot2)
ggplot(data)+
  geom_point(aes(x=x1, y=x2, color=as.character(label)),size=2)+
  scale_colour_discrete(name='Label')+
  ylim(0,3)+coord_fixed(ratio=1)+
  ggtitle('Data to be classified')+
  theme_bw(base_size=12)+
  theme(legend.position=c(0.85,0.87))


## 3. 함수 적용해보기
## training
theta = logisticReg(X, y)

## generate a grid for decision boundary, this is the test set
grid = expand.grid(seq(0, 3, length.out=100), seq(0, 3, length.out=100))

probZ = logisticProb(theta, grid)

Z = logisticPred(probZ)
gridPred = cbind(grid, Z)


# decision boundary visualization
ggplot() +   geom_point(data = data, aes(x=x1, y=x2, color = as.character(label)), size = 2, show.legend = F) + 
  geom_tile(data = gridPred, aes(x = grid[, 1],y = grid[, 2], fill=as.character(Z)), alpha = 0.3, show.legend = F)+ 
  ggtitle('Decision Boundary for Logistic Regression') +
  coord_fixed(ratio = 1) +
  theme_bw(base_size = 12) 



## 4. 배운거 정리 & 느낀점
느낀점 <- 
"
geom_tile 함수 쓰는거 접해보았다. 경계그리는거 재밌네요
일단 지금 예제에서는 optim함수 쓰는게 어렵지 않았다.
optim함수에 익숙해지자!

"