#### R for data science
#### [Chapter13] lubridate로 하는 날짜와 시간

### 13.1 준비하기
library(tidyverse)
library(lubridate)
library(hms)
library(nycflights13)

### 13.2 날짜/시간 생성

# 1. 현재의 데이트형, 데이트타임형 생성 : today(), now()
today()
now()

# 2. 문자열에서 생성 : parse_*(), ymd, mdy, dmy 등등
parse_date("2020-01-01", format = "%Y-%m-%d")
ymd("2020-01-01")
mdy("03/12/1997")
dmy("31.Jan.2017")

# 3. 데이트(타임형)의 구성 요소들이 각 열에 걸쳐있는경우
#    --> make_date(), make_datetime() 사용
flights %>% select(year, month, day, hour, minute) %>%
  transmute(departure = make_datetime(year, month, day, hour, minute))

# time이 들어간 변수의 값들을 분으로 누적해 변형
flights %>% select(contains("time"))

make_datetime100 <- function(year, month, day, time){
  make_datetime(year, month, day, time%/%100, time%%100)
}

flights_dt <- flights %>%
  filter(!is.na(dep_time), !is.na(arr_time)) %>%
  mutate(
    dep_time = make_datetime100(year, month, day, dep_time),
    arr_time = make_datetime100(year, month, day, arr_time),
    sched_dep_time = make_datetime100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime100(year, month, day, sched_arr_time)
  ) %>% select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt %>% head(10)
flights_dt %>% ggplot(mapping=aes(x=dep_time)) + geom_freqpoly(bins=365)
flights_dt %>% group_by(month=month(dep_time), day=day(dep_time)) %>% count() %>%
  filter(month==2)

flights_dt %>% filter(dep_time<ymd(20130102)) %>%
  ggplot(mapping=aes(x=dep_time)) + geom_freqpoly(binwidth=600)

# 4. 다른유형에서 타임(데이트)형으로 변형 --> as_datetime, as_date
as_datetime(today())
as_date(now())
as_datetime(60*60*10) # 1970-01-01 부터 지난 초를 데이트타임형으로 변환
as_date(30)           # 1970-01-01 부터 지난 일수를 데이트형으로 변환

### 13.2 연습문제
# 1. 유효하지 않은 날짜를 포함한 문자열을 파싱하면 어떻게 되는가?
ymd(c("2020-01-01", "bananas"))

# 2. today()의 tzone의 인수는 무슨 역할을 하는가? ==> 시간대 설정
today(tzone="UTC")

# 3. 적절한 lubridate 함수를 이용해 다음을 파싱해라
d1 <- "January 1, 2010"; parse_date(d1, format="%B %d, %Y")
d2 <- "2015-Mar-07"; ymd(d2); parse_date(d2, "%Y-%b-%d")
d3 <- "06-Jun-2017"; dmy(d3); parse_date(d3, "%d-%b-%Y")
d4 <- c("August 19 (2015)", "July 1 (2015)")
parse_date(d4, format="%B %d (%Y)")
d5 <- "12/30/14" # 2014년 12월 30일
mdy(d5)





### 13.3 데이트-타임형 구성요소

## 13.3.1 구성요소 불러오기
# year(), month(), mday(), yday(), wday(), hour(), minute(), second()
datetime <- ymd_hms("2020-03-12 12:34:56"); datetime
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime) # 일요일부터 토요일까지 1~7 매칭
hour(datetime)
minute(datetime)
second(datetime)

Sys.setenv(lang="en")
month(datetime, label=T)
month(datetime, label=T, abbr=F)
wday(datetime, label=T, abbr=F)

flights_dt %>% mutate(wday=wday(dep_time, label=T, abbr=F)) %>% 
  ggplot(mapping=aes(x=wday)) + geom_bar(show.legend = F) + xlab("") + ylab("")

flights_dt %>% mutate(minute = minute(dep_time)) %>% group_by(minute) %>% 
  summarise(avg_delay = mean(arr_delay, na.rm=T), n=n()) %>%
  ggplot(mapping=aes(x=minute, y=avg_delay)) + geom_line()

flights_dt %>% mutate(minute=minute(sched_dep_time)) %>%
  group_by(minute) %>% summarise(avg_delay=mean(arr_delay, na.rm=T), n=n()) %>%
  ggplot(mapping=aes(y=avg_delay, x=minute)) + geom_line() + theme_classic()

## 13.3.2 반올림
# floor_date(), round_date(), ceiling_date()
flights_dt %>% count(week=floor_date(dep_time, "week")) %>%
  ggplot(mapping=aes(x=week, y=n)) + geom_line()

## 13.3.3 구성요소 수정
datetime # 2021-02-11로 날짜수정
update(datetime, month=2, day=11, year=2021)
flights_dt %>% mutate(dep_hour=update(dep_time, yday=1)) %>%
  ggplot(aes(dep_hour)) + geom_freqpoly(binwidth=300)

### 13.3 연습문제
# 1. 하루 동안 비행시간의 분포는 한 해 동안 어떻게 변화했는가?
flights_dt %>%
  filter(!is.na(dep_time)) %>%
  mutate(dep_hour = update(dep_time, yday = 1)) %>%
  mutate(month = factor(month(dep_time))) %>%
  ggplot(aes(dep_hour, color = month)) +
  geom_freqpoly(binwidth = 60 * 60)

# 2. 생략
# 3. 출발, 도착 사이의 시간과 air_time을 비교하라
flights_dt %>%
  mutate(between_dep_arr = arr_time-dep_time,
         relation = air_time - between_dep_arr) %>%
  group_by(origin, dest) %>%
  summarise(avg_relation = mean(relation, na.rm=T)) %>%
  ggplot(mapping=aes(x=origin, y=dest)) + 
  geom_tile(aes(fill=as.numeric(avg_relation))) + coord_flip()

# 4. 하루동안 평균 출발지연 시간은 어떻게 변화하는가?
flights_dt %>% mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>% summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() + geom_smooth()





### 13.4 시간범위

# 시간범위를 대표하는 3가지 클라스
# 1. 듀레이션형(duration) : 정확한 초
# 2. 피어리드형(period)   : 사람의 단위


### 1. 듀레이션형 : 정확한 초를 사용
# 베이스R에서 날짜사이 연산자 사용시 difftime형 객체가 생성됨
# 단위가 범위에 따라 변해서 번거로움
ymd(20200301)-ymd(20200228) # 윤년적용함
now()-1000000
myage <- today()-ymd(19970312)
str(myage)
# 듀레이션형은 항상 초를 사용함 ==> as.duration()
as.duration(myage)
dseconds(15)
dminutes(10)
dhours(c(12,24))
ddays(1:5)
dweeks(3)
dyears(1)


### 2. 피어리드형 : 사람의 시간으로 동작
###    --> 날짜/기간 계산할때 우리가 의도한대로 나옴
days(1)
seconds(15)
minutes(10)
hours(c(12,24))
days(7)
weeks(3)
years(1)
ymd(20200228)+days(1)
