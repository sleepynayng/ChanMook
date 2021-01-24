#### R for data science
#### [Chapter17] purrr로 하는 반복

### 17.1 준비하기
library(tidyverse)
library(purrr)

### 17.2 For루프 --> seq_along 사용
# 각 열의 중앙값 계산
df <- tibble(a=rnorm(10), b=rnorm(10), c=rnorm(10), d=rnorm(10))
output <- vector(mode = "double", length = ncol(df))
for(i in seq_along(df)) output[i] <- median(df[[i]])
print(output)

### 17.2 연습문제
# 1. mtcars 모든 열의 평균을 계산
output <- vector(mode="double", length=ncol(mtcars))
for(i in seq_along(mtcars)) output[i] <- mean(mtcars[[i]], na.rm=T)
print(output)

# 2. nycflights13:flights 각 열의 유형 확인
output <- vector(mode="character", length=ncol(nycflights13::flights))
for(i in seq_along(nycflights13::flights)){
  output[i] <- typeof(nycflights13::flights[[i]])
}
print(output)

# 3. iris 각 열의 유일한 값의 개수를 계산
output <- vector(mode="integer", length=ncol(iris))
for(i in seq_along(iris)){
  output[i] <- n_distinct(iris[[i]])
}
print(output)

# 4. 10개의 랜덤 정규분포(u=-10,0,10,100)을 각각 생성
output <- vector(mode="list", length=4)
mu <- c(-10,0,10,100)
for(i in seq_along(mu)){
  output[[i]] <- rnorm(10, mean = mu[[i]])
}
print(output)






### 17.3 For 루프 변형
# 1. 기존 객체 수정
print(df)
rescale01 <- function(x){
  rng <- range(x, na.rm=T)
  (x-rng[1])/(rng[2]-rng[1])
}
for(i in seq_along(df)){
  df[[i]] <- rescale01(df[[i]])
}
print(df)

# 2. 숫자인수가 아닌 방법으로 루프
#    --> 원소로 루프 or 이름을 따라 반복

# 3. 길이를 모르는 출력 --> combine()함수로 늘려가는 방법은 비효율적
#                       --> 리스트에 일단 저장하고 unlist() 함수 사용!
means <- 0:2
output <- vector(mode="list", length=length(means))
for(i in seq_along(means)){
  n <- sample(x = 100, size = 1)
  output[[i]] <- rnorm(n, mean=means[[i]])
}
print(output)
print(unlist(output))

# 4. 입력 길이를 모르는 시퀀스 --> while 루프 사용
flip <- function() sample(c("H", "T"), size = 1)
times <- 0; nheads <- 0
while(nheads<3){
  times <- times+1
  if(flip()=="H") nheads <- nheads+1
  else nheads <- 0
}


### 17.4 함수형
col_summary <- function(df, fun){
  output <- vector(mode="double", length=length(df))
  for(i in seq_along(df)){
    output[i] <- fun(df[[i]])
  }
  return(output)
}
col_summary(df, mean)
col_summary(df, sum)

### 17.5 맵 함수
map(df, mean)
map_dbl(df, mean)
sapply(df, mean)

split(mtcars, mtcars$cyl)
model <- mtcars %>% split(.$cyl) %>% map(function(df) lm(mpg~wt, data=df)); model
model <- mtcars %>% split(.$cyl) %>% map(~lm(mpg~wt, data=.)); model
model %>% map(summary) %>% map("r.squared")

### 17.5 연습문제
# 1. mtcars의 모든 열의 평균을 계산하라
mtcars %>% map_dbl(mean)
# 2. flights의 각 열의 유형을 확인하라
nycflights13::flights %>% map_chr(typeof)
# 3. iris 각 열에서 유일한 값의 개수를 계산하라
iris %>% map_int(function(x) length(unique(x)))
iris %>% map_int(~length(unique(.)))
# 4. 각 mu=-10, 0, 10, 100에 대해 10개의 랜덤 정규분포 샘플을 생성하라
c(-10, 0, 10, 100) %>% map(.f=function(mu) rnorm(10, mean=mu))
c(-10, 0, 10, 100) %>% map(~rnorm(10, mean=.))



### 17.6 실패 다루기 --> safely(), try() 함수 사용
safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))
str(try(log(10)))
str(try(log("a")))

x <- list(1, 10, "a")
y <- x %>% map(safely(log)); str(y)
y <- y %>% transpose(); str(y)

is_ok <- y$error %>% map_lgl(is_null)
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()

map_dbl(x, possibly(log, NA))

### 17.7 다중 인수로 매핑
mu <- list(5, 10, -3)
sigma <- list(1, 5, 10)
1:3 %>% map(~rnorm(10, mu[[.]], sigma[[.]]))
map2(mu, sigma, rnorm, n=10)
n <- list(1, 3, 5)
pmap(list(n=n, mean=mu, sd=sigma), rnorm)

f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min=-1, max=1),
  list(sd=5),
  list(lambda=10)
)
invoke_map(f, param, n=5)
