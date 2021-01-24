#### R for data science
#### [Chapter5] Exploratory Data Analysis (탐색적 데이터 분석)

### 5.1 process of EDA

# 1. 데이터에 대한 질문
# 2. 데이터 시각화, 변형 및 모델링
# 3. 질문 개선 or 새 질문

library(tidyverse)


### 5.2 Question

# 탐색을 위한 중요 질문
# 1. 변수내에 어떤 변동이 있는가?
# 2. 변수간에 공변동이 발생하는가?


### 5.3 Variation(변동) : 측정값이 변하는 정도

# 범주형변수 : 막대그래프
# 연속형변수 : 히스토그램, 빈도다각형

# 범주형변수 변동 시각화
diamonds %>% ggplot(aes(x=cut)) + geom_bar(aes(fill=cut))
diamonds %>% count(cut)

# 연속형변수 변동 시각화
diamonds %>% ggplot(aes(x=carat)) + geom_histogram(binwidth = 0.25)
diamonds %>% ggplot(aes(x=carat)) + geom_freqpoly(binwidth=0.25)
diamonds %>% count(cut_width(carat, 0.25)) %>% 
  ggplot(aes(x=1:19, y=n)) + geom_col(width = 1) 

diamonds %>% ggplot(aes(x=carat, fill=cut)) + geom_histogram(binwidth = 0.1)
diamonds %>% ggplot(aes(x=carat, color=cut)) + geom_freqpoly(binwidth = 0.1)

# 이상값 : 패턴과 맞지 않는 데이터 값으로 비정상적인 관측값
# 사후처리방법
# 1) 이상값이 포함된 행 삭제
# 2) 이상값을 결측값으로 변경
# 3) 이상값을 평균이나 추정값으로 변경
diamonds %>% ggplot(aes(x=y)) + geom_histogram(binwidth = 0.5) # 이상값존재
diamonds %>% ggplot(aes(x=y)) + geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim=c(0,10))

# 이상값 확인
diamonds %>% filter(y<=0 | y>20) %>% 
  select(price, x, y, z, everything()) %>% arrange(y)

### 5.3 연습문제
# 1. diamonds의 x,y,z 변수의 분포를 탐색
summary(diamonds[, c("x","y","z")])
diamonds %>% ggplot(aes(x=x)) + geom_histogram()
diamonds %>% ggplot(aes(x=y)) + geom_histogram()
diamonds %>% ggplot(aes(x=z)) + geom_histogram()

# 2. price의 분포를 탐색
summary(diamonds$price)
diamonds %>% ggplot(aes(x=price)) + geom_histogram(binwidth = 10)

# 3. 0.99캐럿인 다이아몬드 수와 1캐럿인 다이아몬드의 수는?
diamonds %>% filter(between(carat, 0.99, 1)) %>% count(carat)





### 5.4 결측값
nycflights13::flights %>% mutate(
  cancelled = is.na(dep_time),
  sched_hour = sched_dep_time %/% 100,
  sched_min = sched_dep_time %% 100,
  sched_dep_time = sched_hour + sched_min/60
) %>% ggplot(aes(sched_dep_time)) + geom_freqpoly(aes(color=cancelled))

### 5.4 연습문제
# 1) 히스토그램과 막대그래프의 결측값 처리 방법을 설명하라
#    ==> 히스토그램(결측값삭제), 막대그래프(결측값포함)
diamonds %>% mutate(y=ifelse(y<3|y>20, NA, y)) %>%
  ggplot(mapping=aes(x=y)) + geom_histogram()
diamonds %>% mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>%
  ggplot() + geom_bar(mapping = aes(x = cut))

# 2) 집계함수에서 na.rm=T은 NA를 제거하는 과정을 포함한다





### 5.5 공변동(covariation)
# 빈도수
ggplot(data=diamonds, mapping=aes(x=price)) +
  geom_freqpoly(mapping=aes(color=cut), binwidth=500)
# 비율
ggplot(data=diamonds, mapping=aes(x=price, y=..density..)) + 
  geom_freqpoly(mapping=aes(color=cut), binwidth=500, lwd=1.5)
# 박스상자
ggplot(data=diamonds, mapping=aes(x=cut, y=price)) + 
  geom_boxplot(aes(fill=cut))
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + 
  geom_boxplot(aes(fill=class))
ggplot(data=mpg, mapping=aes(x=reorder(class, hwy, median), y=hwy)) +
  geom_boxplot(aes(fill=class)) + coord_flip()

### 5.5 연습문제
# 1. 취소된 항공편과 그렇지 않은 항공편의 출발 시각을 시간에 따른 비율로 
#    시각화하여라

nycflights13::flights %>% mutate(
  cancelled = is.na(dep_time),
  sched_hour = sched_dep_time %/% 100,
  sched_min = sched_dep_time %% 100,
  sched_dep_time = sched_hour + sched_min/60
) %>% ggplot(aes(x=sched_dep_time, y=..density..)) + 
  geom_freqpoly(aes(color=cancelled), binwidth=0.5, lwd=1.2)

# 2. 다이아몬드 데이터셋의 어떤 변수가 다이아몬드의 가격을 예측하는데 중요한가?
#    그 변수가 cut 변수와 상관관계가 있는가?

diamonds %>% ggplot(mapping=aes(x=carat, y=price)) + 
  geom_point(aes(color=cut))
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)), orientation = "x")
diamonds %>%
  mutate(color = fct_rev(color)) %>%
  ggplot(aes(x = color, y = price)) +
  geom_boxplot()

# 3. 생략
# 4. 생략

# 5. geom_violin() vs geom_histogram() + facet_wrap() 
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()

# 6. 생략

## 두개의 범주형 변수
ggplot(data=diamonds) +
  geom_count(mapping=aes(x=cut, y=color))
diamonds %>% count(color, cut) %>%
  ggplot(mapping=aes(x=color, y=cut)) +
  geom_tile(mapping=aes(fill=n))

### 5.5 연습문제2
# 1. 색상 내에서 컷팅의 분포를 좀 더 명확하게 표시하여라
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))

# 2. geom_tile()과 dplyr를 함께 사용해서 목적지와 월에 따라 달라지는
#    비행기의 평균지연 횟수를 탐색해보자.

nycflights13::flights %>% group_by(dest, month) %>%
  summarise(delay=mean(dep_delay, na.rm=T)) %>%
  ggplot(mapping=aes(x=dest, y=month)) +
  geom_tile(mapping=aes(fill=delay)) + coord_flip()

# 3. 생략

## 두개의 연속형 변수
diamonds %>% ggplot(aes(x=carat, y=price)) +
  geom_point()
diamonds %>% ggplot(aes(x=carat, y=price)) +
  geom_point(alpha=1/100)

diamonds %>% ggplot(mapping=aes(x=carat, y=price)) +
  geom_bin2d()
diamonds %>% ggplot(mapping=aes(x=carat, y=price)) +
  geom_hex()
diamonds %>% ggplot(mapping=aes(x=carat, y=price)) +
  geom_boxplot(mapping=aes(group=cut_width(carat, 0.1)))
diamonds %>% ggplot(mapping=aes(x=carat, y=price)) +
  geom_boxplot(mapping=aes(group=cut_number(carat, 20)))

### 5.6 패턴과 모델
faithful %>% ggplot(mapping=aes(x=eruptions, y=waiting)) +
  geom_point()

library(modelr)
lm.fit <- lm(log(price)~log(carat), data=diamonds)
diamonds2 <- diamonds %>% add_residuals(lm.fit) %>%
  mutate(resid=exp(resid))
diamonds2 %>% ggplot(mapping=aes(x=carat, y=resid)) +
  geom_point(alpha=1/10)
