#### 7. SVM (support vector machine) ####
#### Language : R ####
# source : https://rpubs.com/empireisme/linearsvm

## 1. 모델 구성 ##
svm_gradient <- function(x, eta=0.001, R=10000){
  X = cbind(1,x)
  n = nrow(X)
  p = ncol(X)
  w_init = rep(0,p)
  W = matrix(w_init, nrow=R+1, ncol=p, byrow=T)
  
  for (i in 1:R){
    for (j in 1:p){
      W[i+1, j] = W[i, j] + eta * sum(((y*(X %*% W[i,])) < 1) * y * X[,j])
    }
  }
  return(W)
}

getsvm <- function(x){
  w_answer = svm_gradient(x)[nrow(svm_gradient(x)),]
  return(w_answer)
}


## 2. sample data 만들기 ##
set.seed(2)
n = 5
a1 = rnorm(n)
a2 = 1 - a1 + 2*runif(n)
b1 = rnorm(n)
b2 = -1 - b1 - 2*runif(n)
x = rbind(matrix(cbind(a1,a2),ncol=2),matrix(cbind(b1,b2),ncol=2))
y = matrix(c(rep(1,n),rep(-1,n)))
plot(x, col=ifelse(y>0,4,2), pch='.', cex=7, xlab='x1', ylab='x2')

df = cbind(x,y)
colnames(df) = c('x1','x2','y(label)')
