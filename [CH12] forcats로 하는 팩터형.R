#### R for data science
#### [Chapter12] forcats로 하는 팩터형

### 12.1 준비하기
library(tidyverse)
library(forcats)

### 12.2 팩터형 생성하기
month1 <- c("Dec", "Apr", "Jan", "Mar")  # 정상적인 데이터
month2 <- c("Dec", "Apr", "JAM", "Mar")  # 데이터내 오류
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") # 월별 레벨
 
x1 <- factor(month1, levels=month_levels); x1
sort(x1); sort(month1)

x2 <- factor(month2, levels=month_levels); x2
sort(x2); sort(month2)
parse_factor(month2, levels=month_levels)


## 등장하는 순서대로 레벨 설정 ==> levels=unqiue(data)
##                             ==> fct_inorder() 사용

f1 <- factor(month1, levels=unique(month1))
sort(f1)
f2 <- factor(month1, levels=fct_inorder(month1))
sort(f2)


### 12.3 종합사회조사 ==> gss_cat 데이터 사용
gss_cat %>% head()
gss_cat %>% sapply(FUN = function(x) length(unique(x)))
gss_cat %>% count(race)
gss_cat %>% count(race) %>% ggplot(mapping=aes(x=race, y=n)) + 
  geom_col() + scale_x_discrete(drop=F)


### 12.3 연습문제
# 1. rincome의 분포를 탐색해라
gss_cat %>% ggplot(mapping=aes(x=rincome)) + geom_bar()
# 2. 이 설문에서 가장 많은 relig와 partyid를 구하시오
gss_cat %>% count(relig, sort=T)
gss_cat %>% count(partyid, sort=T)
# 3. denom은 어떤 relig에 적용되는가?
gss_cat %>% count(denom)


### 12.4 팩터 순서 수정하기
# 종교에 따른 TV 시청시간의 평균 구하기
relig_summary <- gss_cat %>% group_by(relig) %>%
  summarise(age=mean(age, na.rm=T), tvhours=mean(tvhours, na.rm=T), n=n())
relig_summary %>% ggplot(mapping=aes(x=relig, y=tvhours)) + 
  geom_col(aes(fill=tvhours>3), show.legend = F) +coord_flip()

# 시청시간 높은 순서대로 재정렬하기 : fct_reorder()
ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + geom_col()
fct_reorder(relig_summary$relig, relig_summary$tvhours, mean)

relig_summary %>% mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) + geom_point()

# 소득에 따른 평균나이 시각화
gss_cat %>% group_by(rincome) %>% summarise(age=mean(age, na.rm=T), n=n()) %>%
  mutate(rincome = fct_reorder(rincome, age, max)) %>%
  ggplot(mapping=aes(x=rincome, y=age)) + geom_col() + coord_flip()

# fct_relevel(.f, level) : level에 해당되는 범주를 앞으로 가져옴
gss_cat %>% group_by(rincome) %>% summarise(age=mean(age, na.rm=T), n=n()) %>%
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>%
  ggplot(mapping=aes(x=age, y=rincome)) + geom_point()

levels(gss_cat$rincome)
levels(fct_relevel(gss_cat$rincome, "Not applicable"))

by_age <- gss_cat %>% filter(!is.na(age)) %>% group_by(age, marital) %>%
  count() %>% ungroup(marital) %>% mutate(prop = n/sum(n))
by_age %>% ggplot(mapping=aes(x=age, y=prop, color=marital)) + geom_line(lwd=1.3)
by_age %>% ggplot(mapping=aes(x=age, y=prop, color=fct_reorder2(marital, age, prop))) +
  geom_line(lwd=1.3) + labs(color="marital")

gss_cat %>% mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(mapping=aes(x=marital)) + geom_bar()
fct_infreq(gss_cat$marital) #가장 많이 나온 순서대로 레벨 정렬
gss_cat %>% count(marital)

### 12.4 연습문제
# 1. tvhours에 대해 관찰하면 의심스러운 큰 값들이 있다. 이때 평균은 좋은 요약값인가?
#    --> 평균은 이상치에 큰 영향을 받기때문에 이런 경우 로버스트한 중앙값이 더 좋다

# 2. gss_cat의 각 팩터형에 대해 레벨의 순서가 임의적인지 원칙적인지 확인하라
str(gss_cat) # marital, race, rincome, partyid, relig, denom
gss_cat %>% lapply(FUN = function(x){
  if(!is.factor(x)) return(NULL)
  else return(levels(x))
})

# 3. "해당없음"의 레벨을 맨앞으로 수정하면 그래프에서 맨 아래에 나타나는가?
#    --> coord_flip()으로 x축과 y축이 반전되어서 x축에서 맨 처음인
#        "해당없음"이 y축의 맨 아래에서 나타나게 된다.




### 12.5 팩터 레벨 수정하기 : fct_recode(), fct_collapse()
gss_cat %>% count(partyid)
gss_cat %>% 
  mutate(partyid = fct_recode(partyid,
    "Republican, strong" = "Strong republican",
    "Republican, weak"   = "Not str republican",
    "Independent, near rep" = "Ind,near rep",
    "Independent, near dem" = "Ind,near dem",
    "Democrat, weak" = "Not str democrat",
    "Democrat, strong" = "Strong democrat")) %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
    other = c("No answer", "Don't know", "Other party"),
    rep = c("Strong republican", "Not str republican"),
    ind = c("Ind,near rep", "Independent", "Ind,near dem"),
    dem = c("Not str democrat", "Strong democrat"))) %>% count(partyid)

gss_cat %>% count(relig)
gss_cat %>% mutate(relig = fct_lump(relig)) %>% count(relig)
gss_cat %>% mutate(relig = fct_lump(relig, n=5)) %>% count(relig)

### 12.5 연습문제
# 1. 민주당, 공화당, 독립정당의 비율이 시간에 지남에 따라 어떻게 변화하는가?
gss_cat %>% mutate(partyid = fct_collapse(partyid,
   other = c("No answer", "Don't know", "Other party"),
   rep = c("Strong republican", "Not str republican"),
   ind = c("Ind,near rep", "Independent", "Ind,near dem"),
   dem = c("Not str democrat", "Strong democrat"))) %>%
  group_by(year, partyid) %>% count() %>% ungroup(partyid) %>%
  mutate(prop=n/sum(n)) %>% ggplot(mapping=aes(x=year, y=prop)) +
  geom_line(aes(color=fct_reorder2(partyid, year, prop)))

# 2. 어떻게 rincome을 적은 범주 집합으로 병합할 수 있는가?
gss_cat %>% count(rincome)
levels(gss_cat$rincome)
gss_cat %>% mutate(rincome = fct_recode(rincome,
    "other" = "No answer", 
    "other" = "Don't know", 
    "other" = "Refused", 
    "other" =  "Not applicable")) %>% 
  mutate(rincome = fct_lump(rincome, n=5)) %>% mutate(rincome=fct_recode(rincome,
                                                    "under10000"="Other")) %>%
  count(rincome) %>% ggplot(mapping=aes(x=rincome, y=n)) + geom_col()
