#### 2. Linear Regression ####
#### Language : R ####
# source : https://github.com/Karimatajin/Linear-Regression-From-Scratch-Using-R/blob/master/Linear-Regression-Scratch.R

## 1. 모델 구성 ##
linearRegression = function(x,y){
  intercept = rep(1,NROW(x))
  x = cbind(intercept,x)
  
  matrix_X = as.matrix(x)
  vector_Y = as.matrix(y)
  
  betas = solve(t(matrix_X) %*% matrix_X) %*% t(matrix_X) %*% vector_Y
  betas = round(betas,2)
  return(betas)
}

PredictY = function(x,betas){
  intercept = rep(1,NROW(x))
  x = cbind(intercept,x)
  return(as.matrix(x) %*% betas)
}





## 2. sample data 만들기
x1 = 1:100
x2 = rnorm(100)
y = x1*(rnorm(100)+3)+x2
dat = data.frame(y=y,x1=x1,x2=x2)
x = dat[,c('x1','x2')]
row_number = sample(1:nrow(dat),0.8*nrow(dat))
train = dat[row_number,]
test = dat[-row_number,]
dim(train);dim(test)
train_X = subset(train, select = c(-y))
train_y = subset(train, select = c(y))
test_X = subset(test, select = c(-y))
test_y = subset(test, select = c(y))


## 3. 함수 적용해보기
betas = linearRegression(train_X,train_y)
betas

y_hat = PredictY(test_X,betas)

plot(x=test_y$y,y=y_hat)

## 4. 배운거 정리 & 느낀점
느낀점 <- 
"
오히려 그냥 일반회귀는 너무 쉬워서 자료가 없는 편인가..? 
입맛에 맞는 자료를 찾기가 어려웠다.
깃허브 뒤져서 여러개 보다가 하나 선택하긴했는데 행렬곱에 있어서 오류가 있다.
회귀만큼은 오류가 보여서 다행이다.
"