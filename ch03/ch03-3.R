# 배열(Array)
x <- array(1:8, c(2,4))    # (2, 4) 모양의 배열 생성
x
y <- array(1:5, c(2,4))
y
z <- array(1:24, c(4,3,2))
z
z[3,2,2]  # 19
z[4,3,1]  # 12

# Matrix(2차원 배열)
matrix(1:12, nrow = 3)   # array(1:12, c(3,4)) 과 동일
matrix(1:12, nrow=3, byrow=T)

# vector를 묶어 배열 생성
v1 = 1:4
v2 = 5:8
v3 = 9:12
cbind(v1,v2,v3)    # 열 단위로 묶어 배열 생성
rbind(v1,v2,v3)    # 행 단위로 묶어 배열 생성

cbind(x,y)   # 2차원 배열을 묶어도 2차원 배열 생성
rbind(x,y)

# 행렬 연산
x <- matrix(1:4, nrow=2)
y <- matrix(5:8, ncol=2)
x
y
# element-wise +,-,*
x + y
x - y
x * y
# 수학적인 행렬 곱(dot product)
x %*% y
# 전치 행렬(transpose)
t(x)
# 역행렬, 자기자신과 행렬 곱을 하면 단위행렬(I, Identity)이 됨
solve(x)
x %*% solve(x)
# 행렬식(determinant)
det(x)

# 배열 연산에 사용되는 함수
apply(x, 1, mean)    # 1: 행별로 적용 ---->
apply(x, 2, mean)
apply(x, 1, sum)
sum(x)
dim(x)

# 샘플링
a <- array(1:12, c(3,4))
sample(a)
sample(a, 4)
sample(a, 4, prob=c(1:12))
