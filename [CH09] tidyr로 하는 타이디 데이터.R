#### R for data science
#### [Chapter9] tidyr로 하는 타이디 데이터

### 9.1 들어가기
library(tidyverse)
library(tidyr)

### 9.2 타이디(tidy) 데이터
# 하나의 데이터를 표현하는 방법은 여러가지가 있다
table1  # 타이디
table2  
table3 
table4a 
table4b
table5

### 타이디 데이터 특징 1
# 1.변수마다 해당되는 독립적인 열 존재 
# 2.관측값마다 해당되는 행이 존재
# 3.값마다 해당되는 하나의 셀이 존재

### 9.2 연습문제
# 1. 변수와 관측값이 각 샘플 테이블에서 어떻게 구성되어 있는가? (생략)
# 2. table2와 table4a + table4b에서 비율을 계산하라.
# 2-a) 연도별, 국가별로 결핵 사례 수(case)를 추출해라
cases <- table2[table2$type=="cases",c(1,2,4)] %>% rename(cases=count)
table4a
# 2-b) 연도별, 국가별로 해당하는 인구를 추출하라
population <- table2[table2$type=="population", c(1,2,4)] %>% rename(population=count)
table4b
# 2-c) 사례를 인구로 나누고 10,000을 곱해라
inner_join(cases, population, id=c("country", "year"))  %>% 
  mutate(rate=cases/population*10000) %>% select(1,2,5)

# 3 table2를 사용해서 시간 경과에 따른 사례 수의 변화를 보여라
table2 %>% filter(type=="cases") %>% ggplot(mapping=aes(x=year, y=count)) +
  geom_line(aes(color=as.factor(country))) + geom_point(aes(color=as.factor(country)))





### 9.3 Gather과 Spread

# gather() : 열 이름이 변수 이름이 아닌 변수 값으로 들어가 있을 때
table4a # 열 이름인 1999와 2000은 변수 year의 값
table4a %>% gather(`1999`, `2000`, key="year", value="cases")
table4b # 열 이름인 1999와 2000은 변수 population의 값
table4b %>% gather(`1999`, `2000`, key="year", value="population")

# spread() : 관측값이 여러 행에 흩어져 있을 때
table2 # 변수 cases와 population이 각 행에서 나타난다
table2 %>% spread(key="type", value="count")

### 9.3 연습문제
# 1. gather()와 spread()가 완벽하게 대칭이 아닌이유는?

stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1,2,1,2),
  return = c(1.88, 0.59, 0.92, 0.17)); stocks

stocks %>% spread(year, return) %>% 
  gather(`2015`, `2016`, key="year", value = "return", convert = T)

# 2. 아래 코드가 잘 동작하지 않는 이유는?
#    --> 비구문론적인 구문이 열 이름을 구성하고 있어서 backtick 사용해야됨
table4a %>% gather(1999, 2000, key="year", value="cases")
table4a %>% gather(`1999`, `2000`, key="year", value="cases", convert=T)

# 3. 다음의 티블을 펼치면 왜 에러가 나는가? 새로운 열을 추가해서 어떻게 문제를
#    해결할 수 있는가?

people <- tribble(
      ~name,        ~key,   ~value,
  #---------------|--------|--------
  "Phillip Woods",  "age",     45,
  "Phillip Woods", "height",   186,
  "Phillip Woods",  "age",     50,
  "Jessica"      ,  "age",     37,
  "Jessica"      , "height",   156
); people

people %>% spread(key=key, value=value)

# 4. 다음의 간단한 티블을 타이디하게 해라

preg <- tibble(pregnant=c("Y", "N"), male=c(NA, 20), female=c(10, 12))
preg %>% gather('male', 'female', key="sex", value="age") %>%
  select(age, everything()) %>% arrange(age)






### 9.4 Separate와 Unite

# separate() : 구분 문자가 나타나는 곳마다 쪼개서 하나의 열을 여러개의 열로
table3 # rate = cases / population
table3 %>% separate(rate, into=c("cases", "population"))
table3 %>% separate(rate, into=c("cases", "population"), 
                    sep="/", conver=T)
table3 %>% separate(year, into=c("century", "year"), sep=2)

# unite() : separate()의 반대로 여러 열을 하나의 열로 결합
table5 %>% unite(col = new, century, year, sep="")
table1 %>% unite(col = rate, cases, population, sep="/")

### 9.4 연습문제
# 1. separate()의 extra인수와 fill 인수의 역할은 무엇인가?
# extra : 일부 열의 데이터가 너무 많이 나뉠때 조절 (warn(default), drop, merge)
# fill  : 일부 데이터가 부족할 때 채워넣어줌(warn(default), right, left)

# extra : 데이터 과다 분리 시 처리 방법
tibble(x=c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra="warn")
tibble(x=c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra="merge")

# fill : 데이터 부족 시 정렬
tibble(x=c("a,b,c", "d,e", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), fill="warn")
tibble(x=c("a,b,c", "d,e", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), fill="left")


# 2. unite()와 separate()의 인수 remove는 무슨 역할을 하는가?
#    --> 원본 데이터가 어떻게 분리/병합되는지 표시

tibble(x=c("a,b,c", "d,e", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), fill="left", remove=T)
tibble(x=c("a,b,c", "d,e", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), fill="left", remove=F)





### 9.5 결측값
stocks <- tibble(
  year = rep(c(2015, 2016), c(4,3)),
  qtr = c(1:4, 2:4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
); stocks
stocks %>% spread(key=year, value=return)
stocks %>% spread(key=year, value=return) %>% 
  gather(`2015`, `2016`, key = "year", value = "return")
stocks %>% complete(year, qtr)

treatment <- tribble(
  ~person, ~treat, ~response,
  "A", 1, 7,
  NA, 2, 10,
  "B", 3, 9,
  NA,1,4
)
treatment %>% tidyr::fill(person, .direction="down")

### 9.6 사례연구
who %>% gather(contains("new"), key="key", value="cases", na.rm=T) %>%
  mutate(key=str_replace(key, "newrel", "new_rel")) %>% 
  separate(key, c("new", "type", "sexage"), sep="_") %>% 
  select(-new, -iso2, -iso3) %>% separate(sexage, c("sex", "age"), 1)
