#### R for data science
#### [Chapter20] purrr와 broom을 이용한 많은 모델

### 20.1 준비하기
library(tidyverse)
library(modelr)

### 20.2 gapminder 데이터 : 기대수명과 국가 통계량 제공
# 시각화 목표 : 시간에 따른 국가별 기대수명
library(gapminder)
head(gapminder)
gapminder %>% ggplot(mapping=aes(x=year, y=lifeExp, group=country)) + geom_line()

# 뉴질란드
nz <- filter(gapminder, country=="New Zealand")
nz %>% ggplot(aes(year, lifeExp)) + geom_line() + ggtitle("Full data = ")
nz_mod <- lm(lifeExp~year, data=nz)
nz %>% add_predictions(nz_mod) %>% ggplot(mapping=aes(x=year, y=pred)) + geom_line() +
  ggtitle("Liear trend + ")
nz %>% add_residuals(nz_mod) %>% ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, color="white", size=3) +
  geom_line() + ggtitle("Remaining pattern")

# 일반화
by_country <- gapminder %>% group_by(country, continent) %>% nest()
by_country$data[[1]] # 아프가니스탄 데이터
country_model <- function(df){
  lm(lifeExp~year,data=df)
}
models <- map(by_country$data, country_model)
models

by_country <- by_country %>% mutate(model = map(data, country_model)) 
by_country %>% filter(continent=="Europe")
by_country <- by_country %>%
  mutate(resid = map2(data, model, add_residuals))
resids <- unnest(by_country, resid)
resids %>% ggplot(aes(year, resid)) + geom_line(aes(group=country), alpha=1/3)  +
  geom_smooth()
resids %>% ggplot(aes(year, resid)) + geom_line(aes(group=country), alpha=1/3)  +
  geom_smooth() + facet_wrap(~continent)

# 모델의 성능
library(broom)
glance(nz_mod) # summary(nz_mod)를 벡터화
by_country %>% mutate(glance = map(model, glance)) %>% unnest(glance)
glance <- by_country %>% mutate(glance = map(model, glance)) %>% unnest(glance, .drop=T)
glance %>% ggplot(aes(continent, r.squared)) + 
  geom_jitter(width=0.3, aes(color=r.squared<0.75), show.legend = F)




### 20.3 리스트로 구성된 열 (List_column)
list(a=1:3, b=3:5)
data.frame(x=list(a=1:3, b=3:5)) # 베이스R에서 데이터프레임 각 원소에 리스트 만들기 힘듬
data.frame(x=I(list(1:3, 3:5)), y=c("1,2", "3,4,5"))
tibble(           # 티블에서는 쉽게 생성가능
  x=list(1:3, 3:5),
  y=c("1,2", "3,4,5")
)

### 20.4 리스트-열 생성하기
# 1. tidyr::nest() 함수 사용하기 --> 그룹화된 열은 유지되고 나머지 변수들은 리스트-열로 변경
gapminder %>% group_by(country, continent) %>% nest()
gapminder %>% nest(year:gdpPercap) # 파라미터에 들어가는 변수는 리스트-열로 변경

# 2. 백터화함수에서 생성하기
df <- tibble(x=c("a,b,c", "d,e,f,g")); df
df %>% mutate(y=str_split(x, ","))
df %>% mutate(y=str_split(x, ",")) %>% unnest()

sim <- tribble(
  ~f,     ~params,
  "runif", list(min=-1, max=1),
  "rnorm", list(sd=5),
  "rpois", list(lambda=10)
)
sim %>% mutate(sims = invoke_map(f, params, n=10))

# 3. 다중값 요약에서 생성하기
mtcars %>% group_by(cyl) %>% summarise(q=quantile(mpg))
mtcars %>% group_by(cyl) %>% summarise(q=list(quantile(mpg)))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% group_by(cyl) %>% 
  summarise(p=list(probs), q=list(quantile(mpg, probs))) %>%
  unnest()

# 4. 명명된 리스트에서 생성하기
x <- list(a=1:5, b=3:4, c=5:6)
df <- enframe(x)
df %>% mutate(smry = map2_chr(name, value, ~str_c(.x, ": ", .y[1])))


### 20.5 리스트-열 단순화하기
# 1. 리스트를 백터로 만들기
df <- tibble(x=list(letters[1:5], 1:3, runif(5)))
df %>% mutate(tpye=map_chr(x, typeof), length=map_int(x, length))
df <- tribble(
  ~x,
  list(a=1, b=2),
  list(a=2, c=4)
)
df %>% mutate(a=map_dbl(x, "a"),
              b=map_dbl(x, "b", .null=NA))
tibble(x=1:2, y=list(1:4, 1)) %>% unnest(y)

# 2. 중첩 해제하기
df <- tribble(
  ~x, ~y, ~z,
  1, c("a", "b"), 1:2,
  2, "c", 3
)
df %>% unnest(c(y, z))

df <- tribble(
  ~x, ~y, ~z,
  1, "a", 1:2,
  2, c("b", "c"), 3
)
df %>% unnest(c(y,z))
