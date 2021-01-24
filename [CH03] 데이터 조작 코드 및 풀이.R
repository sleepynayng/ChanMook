#### R for data science
#### [Chapter3] Data Manipulation (데이터 조작)

### 3.1 들어가기
library(tidyverse)
library(nycflights13)
head(flights)





### 3.2 filter : 각 조건에 맞는 서브셋 출력
filter(flights, month==1,  day==1)  # 1월  1일 비행편 서브셋
filter(flights, month==12, day==25) # 12월 5일 비행편 서브셋

data <- tibble( x = 1:10, y = sqrt(1:10) )
filter(data, y^2==2)
filter(data, near(y^2,2))

filter(flights, month==11 | month==12) # 11월 또는 12월 자료 서브셋
filter(flights, month==11|12)          # 우선순위 | > == 임을 확인
filter(flights, month %in% c(11,12))   # %in%으로 원하는 자료 서브셋

df <- tibble( x = c(1, NA, 3), y = 1:3 )
filter(df, x>1)
filter(df, is.na(x) | x>1)

### 3.2 연습문제 
# 1. 다음 조건을 만족하는 항공편을 모두 찾아라
# a) 2시간 이상 도착 지연
filter(flights, arr_delay>=120)
# b) 휴스턴(IAH, HOU)으로 운항
filter(flights, dest %in% c("IAH", "HOU"))
# c) "United", "American", "Delta" 항공사
filter(flights, carrier %in% c("AA", "DL", "UA"))
# d) 여름에 출발
filter(flights, month %in% 7:9)
# e) 2시간 이상 지연도착했지만 지연 출발x
filter(flights, arr_delay>=120, dep_delay<=0)
# f) 최소 한 시간 이상 지연 출발했지만 운항중 30분 이상 단축
filter(flights, dep_delay>=60, 
       dep_delay - arr_delay>30)
# g) 자정과 6am 사이에 출발
range(flights$dep_time, na.rm = T)
sort(unique(flights$dep_time, na.rm=T))
filter(flights, dep_time<=600 | dep_time==2400)

# 2. 다른 유용한 dplyr 필터링 도우미로 between()이 있다.
#    between(x,a,b)를 사용하면 a<=x<=b인 데이터를 참 그외 데이터를 거짓으로 반환

# 3. dep_time이 결측인 항공편이 몇 편인지 알아보시오
filter(flights, is.na(dep_time))
filter(flights, is.na(dep_time), !is.na(air_time))
sum(is.na(flights$dep_time))

# 4. NA에 관한 성질
# a. NA^0은 왜 결측이 아닌가? 
#    모든 수의 0제곱수는 1이기 때문에

# b. NA | TRUE는 왜 결측이 아닌가?
#    or연산자(|)는 단락평가하기 때문에 한쪽만 참이 오면 다른쪽은 검사X

# c. FALSE & NA는 왜 결측이 아닌가? (b와 동일)
# d. NA*0은 왜 결측인가? 0*무한대는 정의되지 않기 때문에





### 3.3 arrange() : 행 정렬 (desc로 내림차순정렬)
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

# 결측값은 항상 마지막에 정렬
df <- tibble(x=c(3,5,NA,1,9), y=1:5)
arrange(df, x)
arrange(df, desc(x))

### 3.3 연습문제
# 1) arrange()를 사용하여 모든 결측값을 앞에 오도록 정렬하라
arrange(df, desc(is.na(x)))

# 2) flights를 정렬하여 가장 지연된 항공편을 찾아라. 가장 일찍 출발한 항공편은?
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)

# 3) flights를 정렬하여 가장 속력이 빠른 항공편을 찾아라
arrange(flights, desc(distance/air_time))

# 4) 어떤 항공편이 가장 멀리 운항했는가? 가장 짧은 항공편은?
arrange(flights, distance)
arrange(flights, desc(distance))





### 3.4 select() : 열 선택
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

## 도우미함수
# 1. starts_with("abc")  : "abc"로 시작하는 변수 매칭
# 2. ends_with("xyz")    : "xyz"로 끝나는 변수 매칭
# 3. contains("ijk")     : "ijk"를 포함하는 변수 매칭
# 4. matches("regex")    : 정규표현식에 맞는 변수 매칭
# 5. num_range("x", 1:3) : x1, x2, x3에 매칭
# 6. everything()        : 선택되지 않은 변수 매칭
select(flights, air_time, distance, everything())

## 변수명 변경 : rename(dataset, after_name = before_name)
rename(flights, myyear = year, mymonth = month, myday = day)

### 3.4 연습문제
# 1) flights에서 dep_time, dep_delay, arr_time, arr_delay 선택
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4, 6, 7, 9)
select(flights, starts_with(c("dep", "arr")))
select(flights, matches("^(dep|arr)_(time|delay)$"))

# 2) select() 호출에서 한 변수 이름을 여러번 포함하면?
select(flights, year, year, year) #1번만 출력

# 3) 생략
# 4) "time"을 포함하는 변수를 추출하기 위한 아래 코드가 올바르게 작동하는가?
select(flights, contains("TIME"))
select(flights, contains("TIME", ignore.case = F))





### 3.5 mutate()    : 새로운 변수 추가
###     transmute() : 새로운 변수만 출력

flights_small <- select(flights, year:day, ends_with("delay"), 
                        distance, air_time)
mutate(flights_small,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)

transmute(flights_small,
          gain=arr_delay-dep_delay,
          speed = distance / air_time * 60)

# 새변수 바로 참조 가능
mutate(flights_small,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain/hours)

## 랭킹함수
# 1. min_rank() 
# 2. row_number()
# 3. dense_rank()
# 4. percent_rank()
# 5. cume_dist()

### 3.5 연습문제
# 1. 현재 dep_time과 sched_dep_time은 보기 편하지만 실제 연속형 숫자가 아니기에
#    계산하기는 쉽지 않다. 이들을 자정이후의 분으로 변환하라
transmute(flights,
          dep_time = (dep_time%/%100)*60 + (dep_time%%100),
          sched_dep_time = (sched_dep_time%/%100)*60 + (sched_dep_time%%100))
# 2. air_time과 arr_time - dep_time을 비교해라
transmute(flights,
          diff_time = arr_time - dep_time,
          air_time = air_time,
          relate = diff_time - air_time)

# 3. dep_time : 실제 출발 시간, sched_dep_time : 예정 출발 시간
#    dep_delay = dep_time - sched_dep_time

# 4. 랭킹함수를 사용하여 가장 지연된 10개의 항공편을 찾아라.
mutate(flights, rank = min_rank(desc(dep_delay))) %>% 
        select(rank, everything()) %>% arrange(rank) %>% head(10)

# 5. 1:3 + 1:10은 재활용법칙에 따라서 아래와 같다
#    c(1,2,3,1,2,3,1,2,3,1) + 1:10

# 6. R에는 어떤 삼각함수가 있는가?
#    sin, cos, tan, asin, acos, atan, asinh, acosh, atanh





### 3.6 summarise : 요약
###     group_by  : 그룹화 (<> ungroup 그룹화해제)
summarise(flights, delay = mean(dep_delay, na.rm=T))

# 월과 요일에 따른 평균 지연시간 계산
flights %>% group_by(month, day) %>% summarise(delay=mean(dep_delay, na.rm=T))
aggregate(flights$dep_delay, by=list(flights$month, flights$day), mean, na.rm=T)

not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>% group_by(year, month, day) %>%
        summarise(mean=mean(dep_delay))

delays <- not_cancelled %>% group_by(tailnum) %>%
        summarise(delay = mean(arr_delay)); delays
ggplot(delays, mapping=aes(x=delay)) + geom_freqpoly(binwidth=10)

delays <- not_cancelled %>% group_by(tailnum) %>%
        summarise(delay=mean(arr_delay, na.rm=T), n=n())
ggplot(data=delays, mapping=aes(x=n, y=delay)) + 
        geom_point(alpha=1/10)
delays %>% filter(n>25) %>% ggplot(mapping=aes(x=n, y=delay)) +
        geom_point(alpha=1/10)

# 타자의 타율과 타석수의 관계
library(Lahman)
batting <- as_tibble(Batting) # AB : 타석수, H : 히트수
batters <- batting %>% group_by(playerID) %>%
        summarise(
                ba = sum(H, na.rm=T) / sum(AB, na.rm=T),
                ab = sum(AB, na.rm=T))

batters %>% filter(ab>100) %>% ggplot(mapping=aes(x=ab, y=ba)) +
        geom_point() + geom_smooth(method='loess', se=F)

## n() : 카운트, n_distinct(x) : x의 유일값 카운트
not_cancelled %>% group_by(dest) %>% summarise(carriers=n()) %>%
        arrange(desc(carriers))
table(not_cancelled$dest) %>% sort(decreasing = T)
tapply(X=not_cancelled$dest, INDEX=as.factor(not_cancelled$dest), FUN = length) %>%
        sort(decreasing = T)

not_cancelled %>% group_by(dest) %>% summarise(carriers=n_distinct(carrier)) %>%
        arrange(desc(carriers))
aggregate(x = not_cancelled$dest, 
          by=list(not_cancelled$carrier, not_cancelled$dest), unique)[,'x'] %>%
        table() %>% sort(decreasing = T)

### 3.6 연습문제
# 2. count()를 사용하지 않고 not_canceled %>% count(dest)와
#    not_canceled %>% count(dest, wt=distance)와 같은 결과를 출력하여라.
not_cancelled %>% count(dest)
not_cancelled %>% group_by(dest) %>% summarise(n=n())

not_cancelled %>% count(dest, wt=distance)
not_cancelled %>% group_by(dest) %>% summarise(n=sum(distance))

# 3. 취소된 항곤편에 대한 정의 (is.na(dep_delay) | is.na(arr_delay))는 취약점이
#    존재한다. 왜 그런가?
#    ==> 출발을 해도 사고, 우회, 추락 등으로 arr_delay값이 누락되었을 수 있다
flights %>% filter(is.na(dep_delay) | is.na(arr_delay))

# 4. 일간 취소된 항곤편의 수를 살펴보고 패턴이 있는지 파악하시오.
cancelled_per_day <- flights %>%
        mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
        group_by(year, month, day) %>%
        summarise( cancelled_num = sum(cancelled), flights_num = n())
cancelled_per_day %>% ggplot(mapping=aes(x=flights_num, y=cancelled_num)) +
        geom_point()

# 5. 가장 심한 지연시간을 보인 항공사는?
flights %>% group_by(carrier) %>% summarise(delay=mean(dep_delay, na.rm=T)) %>%
        arrange(desc(delay))

flights_small %>% group_by(year, month, day) %>%
        filter(rank(desc(arr_delay))<=10)
