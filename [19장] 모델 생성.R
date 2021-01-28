#### R for data science
#### [Chapter19] 모델 생성

### 19.1 준비하기
library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)

### 19.2 낮은 품질의 다이아몬드가 더 비싼 이유가 무엇인가?
###      --> 가격은 주로 무게(carat)에 의해 결정되는데 무게가 클수록 품질이 낮아지는 경향
diamonds %>% ggplot(mapping=aes(x=cut, y=price)) + geom_boxplot()
diamonds %>% ggplot(mapping=aes(x=color, y=price)) + geom_boxplot()
diamonds %>% ggplot(mapping=aes(x=clarity, y=price)) + geom_boxplot()
diamonds %>% ggplot(mapping=aes(x=carat, y=price)) + geom_hex()

diamonds %>% mutate(flag=(carat<=2.5)) %>% group_by(flag) %>%
  summarise(n=n()) %>% mutate(prop=n/sum(n)) # 2.5캐럿 이하의 다이아몬드가 전체의 99.8% 차지
diamonds %>% count(range=cut_width(carat, width = 0.1))

diamonds2 <- diamonds %>% filter(carat<=2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))
diamonds2 %>% ggplot(mapping=aes(x=lcarat, y=lprice)) + geom_hex(bins=50)
lm.fit <- lm(lprice~lcarat, data=diamonds2)
summary(lm.fit)

grid <- diamonds2 %>% data_grid(carat=seq_range(carat, 20)) %>%
  mutate(lcarat=log2(carat)) %>% add_predictions(lm.fit, var = "lprice") %>%
  mutate(price = 2^lprice)

diamonds2 %>% ggplot(mapping=aes(x=carat, y=price)) + 
  geom_hex(bins=50) + geom_line(data=grid, color="red", size=1)

diamonds2 <- diamonds2 %>% add_residuals(lm.fit, "lresid")
diamonds2 %>% ggplot(mapping=aes(x=lcarat, y=lresid)) + geom_hex(bins=50)

diamonds2 %>% ggplot(aes(x=cut, y=lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(x=color, y=lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(x=clarity, y=lresid)) + geom_boxplot()

lm.fit2 <- lm(lprice~lcarat+color+cut+clarity, data=diamonds2)
summary(lm.fit2)
grid <- diamonds2 %>%
  data_grid(cut, .model=lm.fit2) %>%
  add_predictions(lm.fit2)
grid %>% ggplot(aes(cut, pred)) + geom_point()

### 19.3 일일 운항 횟수에 어떤 영향이 있을까?
daily <- flights %>%
  mutate(date=make_date(year, month, day)) %>%
  group_by(date) %>% summarise(n=n())
daily %>% ggplot(aes(date,n)) + geom_line() + ylim(0, max(daily$n))
daily <- daily %>% mutate(wday=wday(date, label=T, abbr = F))
daily %>% ggplot(mapping=aes(x=wday, y=n)) + geom_boxplot()

lm.fit <- lm(n~wday, data=daily)
summary(lm.fit)
grid <- daily %>% data_grid(wday) %>% add_predictions(lm.fit)
ggplot(daily, aes(wday, n)) + geom_boxplot() + 
  geom_point(data=grid, aes(wday, pred), color='red', size=4)

daily <- daily %>% add_residuals(lm.fit)
daily %>% ggplot(aes(date, resid)) + geom_ref_line(h=0) + geom_line()
ggplot(daily, aes(date, resid, color=wday)) + geom_line() +
  geom_ref_line(h=0)
daily %>% dplyr::filter(!resid>=-100)
daily %>% ggplot(aes(date, resid)) + geom_ref_line(h=0) +
  geom_line(color="grey50") + geom_smooth(se=F, span=0.2, method="loess")

daily %>% filter(wday=="토요일") %>% 
  ggplot(aes(date, n)) + geom_point() + geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")
term <- function(date){
  cut(date, breaks=ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall"))
}
daily <- daily %>% mutate(season = term(date))
daily %>% filter(wday=="토요일") %>%
  ggplot(aes(date, n, color=season)) + geom_point(alpha=1/3) +
  geom_line() + scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>% ggplot(mapping=aes(wday, n,color=season)) + geom_boxplot()

mod1 <- lm(n~wday, data=daily)
mod2 <- lm(n~wday*season, data=daily)
daily %>% gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color=model)) + geom_line(size=1.2)

grid <- daily %>% data_grid(wday, season) %>%
  add_predictions(mod2, "n")

ggplot(daily, aes(wday,n)) + geom_boxplot() +
  geom_point(data=grid, color="red") + facet_wrap(~season)

mod3 <- MASS::rlm(n~wday*season, data=daily)
daily %>% add_residuals(mod3, "resid") %>% ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size=2, color="white") + geom_line()
