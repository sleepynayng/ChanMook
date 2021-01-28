#### R for data science
#### [Chapter18] modelr을 이용한 모델의 기초

### 18.1 준비하기
# 모델의 목표 : 진실을 알아내는 것이 아닌 유용하면서 간단한 근사치 제공
library(tidyverse)
library(modelr)
options(na.action = na.warn)

### 18.2 간단한 모델
str(sim1)
sim1 %>% ggplot(mapping=aes(x=x, y=y)) + geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)
sim1 %>% ggplot(mapping=aes(x=x,y=y)) + geom_point() +
  geom_abline(models, mapping=aes(intercept=a1, slope=a2))

model1 <- function(a, data){
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data){
  diff <- data$y - model1(mod, data)
  return(sqrt(mean(diff^2)))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2){
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% mutate(dist=purrr::map2_dbl(a1, a2, sim1_dist))
models %>% arrange(dist)
sim1 %>% ggplot(mapping=aes(x=x,y=y)) + geom_point() +
  geom_abline(filter(models, rank(dist)<=10), mapping=aes(intercept=a1, slope=a2, color=-dist))

models %>% ggplot(mapping=aes(x=a1, y=a2)) + geom_point(size=2, mapping=aes(color=rank(dist)<=10))

grid <- expand.grid(
  a1 = seq(-5, 20, length=25),
  a2 = seq(1, 3, length=25)
) %>% mutate(dist=purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% ggplot(mapping=aes(x=a1, y=a2)) + geom_point(aes(color=-dist)) +
  geom_point(data=filter(grid, rank(dist)<=10), color="red", size=4)
best <- optim(c(0,0), measure_distance, data=sim1)

ggplot(sim1, mapping=aes(x=x, y=y)) + geom_point(size=2, color="grey30") +
  geom_abline(aes(intercept=best$par[1], slope=best$par[2]))

sim1_mod <- lm(y~x, data=sim1)
coef(sim1_mod)

### 18.2 연습문제
# 1. 선형 모델의 단점은 비정상적인 값에 민감하다는 것이다. 다음의 시뮬레이션 데이터에 대해 선형 모형을
#    적합하고 결과를 시각화해보자. 서로 다른 시뮬레이션 데이터셋을 생성해보기 위해 여러번 실행해보자
sim1a <- tibble(
  x=rep(1:10, each=3),
  y=x*1.5 + 6 + rt(length(x), df=2)
)
sim1a %>% ggplot(mapping=aes(x=x, y=y)) + geom_point() + geom_smooth(method="lm")
sim1a_mod <- lm(y~x, sim1a)
sim1a_mod %>% coef()
optim(c(0,0), measure_distance, data=sim1a)

### 18.3 ~ 4 모델 시각화하기
grid <- sim1 %>% data_grid(x)
grid <- grid %>% add_predictions(sim1_mod)
sim1 %>% ggplot(mapping=aes(x=x)) + geom_point(mapping=aes(y=y)) +
  geom_line(data=grid, aes(x=x,y=pred), color="red", size=1)
sim1 <- sim1 %>% add_residuals(model=sim1_mod)
sim1 %>% ggplot(mapping=aes(x=resid)) + geom_freqpoly(binwidth=0.5)
sim1 %>% ggplot(mapping=aes(x=x, y=resid)) + geom_point() +
  geom_ref_line(h=0)

# 범주형 변수
sim2 %>% ggplot(mapping=aes(x=x ,y=y)) + geom_point()
lm.fit2 <- lm(y~x, sim2)
summary(lm.fit2)
grid <- sim2 %>% data_grid(x) %>% add_predictions(lm.fit2)
sim2 %>% ggplot(mapping=aes(x=x, y=y)) + geom_point() +
  geom_point(data=grid, aes(y=pred), color="red", size=4)

# 변주형 + 연속형 변수
sim3 %>% ggplot(mapping=aes(x=x1)) + geom_point(mapping=aes(y=y, color=x2))
grid <- sim3 %>% data_grid(x1, x2) %>% add_predictions(model=lm(y~x1*x2, data=sim3))
ggplot(data=sim3, mapping=aes(x=x1, y=y)) + geom_point(aes(color=x2)) +
  geom_line(data=grid, aes(y=pred)) + facet_wrap(~x2)
ggplot(data=sim3, mapping=aes(x=x1, y=y)) + geom_point(aes(color=x2)) +
  geom_line(data=grid, aes(y=pred, group=x2))

# 연속형 + 연속형 변수
mod1 <- lm(y~x1+x2, data=sim4)
mod2 <- lm(y~x1*x2, data=sim4)
grid <- sim4 %>% data_grid(
  x1 = seq_range(x1, 5),
  x2 = seq_range(x2, 5)
) %>% gather_predictions(mod1, mod2)

grid %>% ggplot(aes(x1, x2)) + geom_tile(aes(fill=pred)) + facet_wrap(~model)
ggplot(grid, aes(x1, pred, color=x2, group=x2)) + geom_line() + facet_grid(~model)
ggplot(grid, aes(x2, pred, color=x1, group=x1)) + geom_line() + facet_grid(~model)
