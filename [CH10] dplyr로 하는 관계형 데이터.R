#### R for data science
#### [Chapter10] dplyr로 하는 관계형 데이터

### 10.1 준비하기
library(tidyverse)
library(dplyr)
library(nycflights13)

### 10.2 관계형 데이터
# 1. airlines : 약어 코드와 항공사명 
nycflights13::airlines

# 2. airports : 각 공항에 대한 정보
nycflights13::airports

# 3. planes : 각 여객기에 대한 정보 
nycflights13::planes

# 4. weather : 각 공항에 대한 날씨 정보
nycflights13::weather

# 5. flights : 항공기 출발/도착에 대한 정보
nycflights13::flights


### 10.3 키(key)
planes %>% count(tailnum) %>% filter(n>1) # tailnum이 유일하게 결정
weather %>% count(year, month, day, hour, origin) %>% 
  filter(n>1) # 유일하게 결정 x

### 10.4 뮤테이팅 조인
flights2 <- flights %>% select(year:day, hour, origin, dest, tailnum, carrier)
flights2 %>% select(-origin, -dest) %>% left_join(airlines, by="carrier")
flights2 %>% select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

## 조인함수
x <- tribble(
  ~key, ~val_x,
    1, "x1",
    2, "x2",
    3, "x3"
); x

y <- tribble(
  ~key, ~val_y,
    1, "y1",
    2, "y2",
    4, "y3"
); y

inner_join(x, y, by="key") # 내부조인
left_join(x, y, by="key")  # 외부조인-왼쪽조인
right_join(x, y, by="key") # 외부조인-오른쪽조인
full_join(x, y, by="key")  # 외부조인-전체조인

## 중복키 : 조인시 가능한 모든 조합의 데카르트곱을 반환
x <- tibble(key=c(1,2,2,1), val_x=c("x1","x2","x3","x4"))
y <- tibble(key=c(1,2), val_y=c("y1", "y2"))
left_join(x,y,by="key")

## by = NULL을 사용하면 자연조인(공통변수) 실행
flights2 %>% left_join(weather)
## by = c("a"="b")를 사용해 a와 b가 같을때 조인 실행("=="아님에 주의)
flights2 %>% left_join(airports, by=c("dest"="faa"))
## merge(x,y, all.x, all.y)를 이용해 4가지 조인 구현 가능

### 10.4 연습문제
# 1. 목적지별 평균 지연 시간을 계산한 다음, airports 데이터프레임에 조인해
#    지연의 공간 분포를 표시하라
flights %>% group_by(dest) %>% summarise(avg_delay=mean(dep_delay, na.rm=T)) %>%
  left_join(airports, by=c("dest"="faa")) %>%
  ggplot(mapping=aes(x=lon, y=lat)) + borders("state") +
  geom_point(aes(color=avg_delay)) + coord_quickmap()

# 2. flights에 출발지와 목적지의 위치를 추가하라.
flights %>% select(1:3, dest, origin) %>%
  left_join(airports, by=c("dest"="faa")) %>%
  left_join(airports, by=c("origin"="faa"),  suffix=c("_dest","_origin"))

# 3. 여객기의 나이와 지연 시간 사이에 관계가 있는가?
flights %>% select(year, dep_delay, tailnum) %>% 
  left_join(planes, by="tailnum") %>% select(1:4) %>%
  mutate(old=year.x-year.y+1) %>% filter(!is.na(old)) %>%
  mutate(old=ifelse(old>=25, 25L, old)) %>% group_by(old) %>%
  summarise(mean_delay = mean(dep_delay, na.rm=T)) %>%
  ggplot(mapping=aes(x=old, y=mean_delay)) + geom_line()

# 4. 어떤 기상 조건이 지연 가능성을 더 높이는가?
flights %>% select(year:day, hour, origin, dep_delay) %>%
  left_join(weather, by=c("origin", "year", "month", "day", "hour")) %>%
  group_by(cut_width(temp, 10)) %>% 
  summarise(mean_delay=mean(dep_delay, na.rm=T))
  
# 5. 2013년 6월 13일에 무슨 일이 일어났는가?
flights %>% filter(year==2013, month==6, day==13)
weather %>% filter(year==2013, month==6, day==13)

flights %>% filter(year==2013, month==6) %>% group_by(day) %>%
  summarise(avg_delay = mean(dep_delay, na.rm=T)) %>% print(n=30)





### 10.5 필터링 조인 : 관측값에 영향
# semi_join(x,y) : y와 매치되는 x의 모든 관측값 보존
# anti_join(x,y) : y와 매치되는 x의 모든 관측값 삭제

top10_dest <- flights %>% count(dest, sort=T) %>% head(10)
flights %>% filter(dest %in% top_dest$dest)
flights %>% semi_join(top10_dest, by="dest")
flights %>% anti_join(planes, by="tailnum") %>%
  count(tailnum, sort=T)

### 10.5 연습문제
# 1. 항공편에 tailnum이 없는 것은 무엇을 의미하는가?
flights %>% filter(is.na(tailnum)) %>% count(carrier)

# 2. flights를 필터링하여 최소 100편을 운행한 여객기의 항공편만 표시하라.
tailnum_up100 <- flights %>% filter(!is.na(tailnum)) %>% 
  count(tailnum) %>% filter(n>=100)
flights %>% semi_join(tailnum_up100, by="tailnum")

tailnum_down99 <- flights %>% count(tailnum) %>% filter(n<100)
flights %>% anti_join(tailnum_down99, by="tailnum")

# 3. fueleconomy::vehicles와 fueleconomy::common을 조인하여
#    가장 많은 차량 모델의 레코드만 찾아라
# install.packages("fueleconomy")

library(fueleconomy)
vehicles %>% head()
common %>% head()
match(vehicles$model, common$model) %>% table(useNA = "always")
vehicles %>% semi_join(common, by=c("model", "make")) %>% group_by(model) %>%
  count(sort=T)

# 4. 생략
# 5. 아래 코드는 무엇을 의미하는가?
dest_anti <- anti_join(flights, airports, by=c("dest"="faa")) %>% distinct(dest)
dest_anti$dest %in% airports$faa

# 6. 각 항공기는 단일 항공사에 의해 운항되므로 항공기와 항공사 간에 암묵적인 관계가
#    있을것으로 예상된다. 이 가설을 확인하거나 기각하여라. (생략)


### 10.6 조인 문제
### 조인을 원활하게 하기 위한 방법 3가지
# 1. 기본키를 찾아라!
# 2. 기본키로 이루어진 변수값에 결측값이 있나 확인하라!
# 3. 외래키가 다른 테이블의 기본키와 잘 연결되어 있는지 확인하라!
#    ==> anti_join() 유용

### 10.7 집합 연산
# intersect(x,y) : x, y 모두에 있는 관측값 반환
# union(x,y)     : x와 y의 고유한 관측값 모두 반환
# setdiff(x,y)   : x에 있지만 y에 없는 관측값 반환
df1 <- tibble(x=1:2, y=1); df1
df2 <- tibble(x=1, y=1:2); df2

intersect(df1, df2)
union(df1, df2)
setdiff(df1, df2)
