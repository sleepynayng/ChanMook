# R을 활용한 데이터과학 : 1~14장 복습 문제
# 경로 설정 및 패키지 로딩
setwd("C:/Users/cc933/Desktop/Mook/통계/빅데이터/기타")
library(tidyverse)
library(magrittr)
library(lubridate)
library(forcats)
library(hms)

# 파일 로딩 및 변수명 재설정
lotto <- read_csv("lotto.csv", skip=1, na="?", col_types = cols(
  "추첨일" = col_date(format="%Y.%m.%d"),
  .default = col_number()))
colnames(lotto)[3:12] <- str_c(rep(c("당첨자수","당첨금액"), times=5),
                               rep(1:5, each=2), sep="")
colnames(lotto)[13:18] <- str_c("당첨번호", 1:6, sep="")

# 이상치 확인 및 제거 과정
sapply(lotto, function(data) sum(is.na(data)))
lotto <- lotto %>% na.omit()

lotto %>% ggplot(mapping=aes(x=추첨일, y=당첨금액4)) + geom_point() +
  xlab("추첨일") + ylab("4등 당첨금액") + theme_bw()
lotto %>% ggplot(mapping=aes(x=추첨일, y=당첨금액5)) + geom_point() +
  xlab("추첨일") + ylab("5등 당첨금액") + theme_bw()
lotto %>% filter(추첨일>=ymd(20100801), 당첨금액4!=50000) %>% select(당첨금액4)
lotto %>% filter(추첨일>=ymd(20040801), 당첨금액5!=5000) %>% select(당첨금액5)
lotto <- lotto %>% filter(당첨금액4 != 999999, 
                          당첨금액5 %in% c(5000, 10000))


# 탐색적 데이터 분석(EDA)
# 1등 당첨자수와 당첨금액의 산점도를 그려라(0명 제외)
# 단, 색깔로 당첨일을 2002~2009, 2010~2014, 2015~2019년으로 구분해라.
# 이상치가 있는가? 또 당첨자수와 당첨금액간에 무슨 관계가 있는가?
lotto %>% filter(당첨자수1!=0, 당첨금액1<=2e+10) %>% 
  mutate("season"=fct_collapse(as.factor(year(추첨일)),
                  by2009 = as.character(c(2002:2009)),
                  by2014 = as.character(c(2010:2014)),
                  by2019 = as.character(c(2015:2019)))) %>%
  ggplot(mapping=aes(x=당첨자수1, y=당첨금액1)) + geom_jitter(aes(color=season), show.legend = F) + geom_smooth(se=F) +
  xlab("1등 당첨자수") + ylab("1등 당첨금액") + theme_bw()

mydata <- lotto %>% filter(당첨자수1!=0) %>% mutate(추첨년도=as.factor(year(추첨일))) %>%
  select(추첨년도, 당첨자수1, 당첨금액1)
mydata <- mydata %>% mutate("구간"=fct_collapse(추첨년도,
                  by2009 = as.character(2002:2009),
                  by2014 = as.character(2010:2014),
                  by2019 = as.character(2015:2019)))
mydata %>% ggplot(mapping = aes(x=당첨자수1, y=당첨금액1)) + geom_point(aes(color=구간)) +
  geom_smooth(se=F)

# 연도별 평균 1~2등 당첨자 수
sum1to2 <- lotto %>% mutate(당첨년도=year(추첨일)) %>% group_by(당첨년도) %>%
  summarise("1등"=mean(당첨자수1, na.rm=T), "2등"=mean(당첨자수2, na.rm=T))

sum1to2 %>% gather(2:3, key="등수", value="평균") %>%
  mutate("등수"=fct_inorder(등수)) %>% 
  ggplot(mapping=aes(x=당첨년도, y=평균, color=등수))  + 
  geom_col(position="dodge", aes(fill=등수)) + geom_line() + ylab("평균당첨자수")


