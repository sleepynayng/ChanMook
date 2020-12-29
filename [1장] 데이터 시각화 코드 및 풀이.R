#### R for data science
#### [Chapter1] Data Visualization

library(tidyverse)

### 1. 엔진크기와 연비의 관계??
mpg %>% head() # displ : 엔진크기, hwy : 연비
ggplot(data=mpg) + 
  geom_point(mapping=aes(x=displ,y=hwy))

### 1.2.4 연습문제(p6)
ggplot(data=mpg)  # 빈그래프 생성
dim(mpg)          # 234 * 11
?mpg              # drv = 차량 바퀴 종류
ggplot(data=mpg) + 
  geom_point(mapping=aes(x=hwy, y=cyl))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=class, y=drv))





### 2. 심미성 추가 (color, size, alpha, shape) ★★
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, size=class))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, alpha=class))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, shape=class))

### 3. 심미성 수동 추가 (color)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy), color="blue")

### 1.3.1 연습문제(p12)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color="blue"))
lapply(mpg, unique) # class, fl, drv, trans, cyl, 
                    # year, model, manufacturer 범주형
                    # cty, hwy, displ 연속형 
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=cty))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class, size=class))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy), shape=16)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, stroke=cty), shape=16)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy), shape=21)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, stroke=cty), shape=21)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=displ<5)
             
             
             
             

### 5. 면분할(facet) : facet_wrap( ), facet_grid( ) ★★
ggplot(data=mpg) + 
  geom_point(mapping=aes(x=displ, y=hwy, color=class)) +
  facet_wrap(~class, nrow=2)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class)) +
  facet_grid(drv~cyl)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class)) +
  facet_grid(~cyl)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy, color=class)) +
  facet_grid(.~cyl)

### 1.5.1 연습문제
# 1) 연속형 변수로 면분할시 length(unique("variable")) 수만큼 분할된다
# 2) 면분할시 빈 셀들은 해당 분할된 공간에 자료가 없는것을 의미한다
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv~cyl)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=drv, y=cyl))

# 3) 면분할시 .은 데이터가 없는 임시 변수 역할을 수행한다
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(drv~.)
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_grid(.~drv)
# 4) 면분할의 장점 ==> 개별 데이터의 추세 파악용이
#    면분할의 단점 ==> 전체 데이터의 추세 파악어려움
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  facet_wrap(~class, nrow=2)
# 5) facet_wrap에서 nrow와 ncol은 행,열에 몇개의 면을 나타낼지를 지정해준다
?facet_wrap





### 6. 기하 객체
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv))
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ, y=hwy, linetype=drv, color=drv)) +
  geom_point(mapping=aes(x=displ, y=hwy, color=drv))

ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) +
  geom_smooth(mapping=aes(x=displ, y=hwy, group=drv))
ggplot(data=mpg) +
  geom_smooth(
    mapping=aes(x=displ, y=hwy, color=drv),
    show.legend = T)

### 코드 중복 제거
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  geom_smooth(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth()
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth()
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth(
    data=filter(mpg, class=="subcompact"),
    se=T
  )

### 1.6.1 연습문제
# 1) 선그래프   : geom_line      박스플랏 : geom_boxplot
#    히스토그램 : geom_histogram 면적     : geom_area

# 2) 아래 코드 확인
ggplot(data=mpg, mapping=aes(x=displ, y=hwy, color=drv)) +
  geom_point() + geom_smooth(se=F)

# 3) show.legend는 각 지옴함수에서 범례 표시 여부를 나타낸다
# 4) geom_smooth( )에서 se는 신뢰구간 표시 여부를 나타내며
#    또 다른 파라미터인 level로 신뢰수준을 조절할 수 있다.
# 5) 다음의 두 그래프에 차이가 있는가?
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth()
ggplot() + geom_point(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_smooth(data=mpg, mapping=aes(x=displ, y=hwy))

# 6) 다음의 그래프들을 생성하는데 필요한 R코드를 작성
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth(se=F)
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth(mapping=aes(group=drv), se=F)
ggplot(data=mpg, mapping=aes(x=displ, y=hwy, color=drv)) +
  geom_point() + geom_smooth()
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv)) + geom_smooth()
ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv)) +
  geom_smooth(mapping=aes(linetype=drv), se=F)
ggplot(data=mpg, mapping=aes(x=displ, y=hwy, fill=drv)) +
  geom_point(shape=21, color="white", stroke=4, size=3)





### 7. 통계적변환(stat) ★★★
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))       # 막대그래프 ==> 이산형 or 연속형
ggplot(data=diamonds) +
  stat_count(mapping=aes(x=cut))
ggplot(data=diamonds) +
  geom_histogram(mapping=aes(x=cut)) #  histogram ==> 연속형
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=cut))  # 빈도다각형 ==> 연속형


ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=carat))       # 막대그래프 ==> 이산형 or 연속형 
ggplot(data=diamonds) +
  geom_histogram(mapping=aes(x=carat)) #  histogram ==> 연속형
ggplot(data=diamonds) +
  geom_freqpoly(mapping=aes(x=carat))  # 빈도다각형 ==> 연속형


demo <- tribble(
  ~cut, ~freq,
  "Fair", 1610,
  "Good", 4906,
  "Very Good", 12082,
  "Premium", 13791,
  "Ideal", 21551)
table(diamonds$cut)
t(demo)

ggplot(data=demo) + geom_bar(mapping=aes(x=cut, y=freq), stat="identity")
ggplot(data=diamonds) + 
  geom_bar(mapping=aes(x=cut, y=..prop.., group=1)) # 비율표시
ggplot(data=diamonds) + 
  geom_bar(mapping=aes(x=cut, group=1)) # 빈도수

ggplot(data=diamonds) +
  stat_summary(
    mapping=aes(x=cut, y=depth),
    fun.min = min,
    fun.max = max,
    fun     = median
  )

### 1.7.1 연습문제
# 1) stat_summary( )는 geom_pointragne( )와 연관되어 있다.
ggplot(data=diamonds) + 
  geom_pointrange(mapping = aes(x=cut, y=depth, 
                                ymin=depth, ymax=depth))
?geom_pointrange

df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)
df
p <- ggplot(df, aes(trt, resp, colour = group))
p + geom_linerange(aes(ymin = lower, ymax = upper))
p + geom_pointrange(aes(ymin = lower, ymax = upper))
p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.2)
p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)

# 2) geom_col()은 geom_bar()에서 stat="identity"로 설정한 함수로
#    x,y가 주어지면 따로 계산을 하지 않고 각 x에 해당되는 y의 값으로
#    막대그래프로 그려준다.

# 3) 대부분의 지옴과 스탯은 쌍을 이루어 거의 항상 함께 사용된다.
# 4) stat_smooth()로 데이터에 대한 추세를 파악할 수 있다. 
#    해당 함수로 국소회귀 또는 일반화가법모델(GAM)을 적합한다.

# 5) 다음 그래프의 차이가 무엇인가?
#    비율로 나타낼때 group=1로 설정하지 않으면 stat_count가 x에 
#    주어진 값을 하나로 보기때문에 동일한 높이막대가 생성된다
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, y=..prop..))
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, group=1))
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, y=..prop.., group=1))






### 8 위치조정(position) ★
ggplot(data=diamonds) +  # color는 객체(x범주)의 테두리에 적용되는 색
  geom_bar(mapping=aes(x=cut, color=cut))
ggplot(data=diamonds) +  # fill은 객체(x범주)의 안쪽에 적용되는 색
  geom_bar(mapping=aes(x=cut, fill=cut))
ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut, fill=clarity))

## position 인수로 막대 그래프의 세부 위치 조정 가능 (default, postion="stack")
## "identity"(겹침)              "dodge"(barplot에서 beside=T)
## "fill"(mosaicplot과 유사)     "jitter"(관측치 미세조정, jitter함수)

# position="identity" : 두 변수의 분할표에서 x범주에 대한 각 도수의 값을
#                     : 겹쳐서 표시
ggplot(data=diamonds, mapping=aes(x=cut, color=clarity)) +
  geom_bar(position="identity", fill="black")
ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5,position="identity")
table <- sapply(unique(diamonds$cut), FUN = function(ind){
  table(diamonds$clarity[diamonds$cut==ind])
  })
colnames(table) <- unique(diamonds$cut); table

# position="dodge" : 겹치는(stacked) 객체가 서로 옆에 배치
ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(position="dodge")
barplot(table, beside = T, col=cm.colors(n=8))

# position="fill" : 두 변수의 분할표에서 x범주에 대한 각 도수의 비율을 표시
ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
         geom_bar(position="fill")
mosaicplot(t(table), xlab = "cut", ylab="clarity proportion", color=topo.colors(8))

# position="jitter" : overploting 문제를 해결하기 위해 각 관측치를 미세하게 조정
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy), position="jitter")
plot(mpg$displ, mpg$hwy, pch=16)
plot(jitter(mpg$displ), jitter(mpg$hwy), pch=16)

### 1.8 연습문제
# 1) 다음 플롯은 동일한 값을 가져서 여러 값이 하나로 표시되는 문제가 있다.
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + geom_point()
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + geom_point(position="jitter")

# 2) geom_jitter()에서 조정되는 정도를 제어하는 파라미터는 무엇인가
#    witdh(가로폭조정), height(세로폭조정)
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + geom_jitter()
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + geom_jitter(height=10)
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + geom_jitter(width=10)

# 3) geom_jitter(관측치조정) vs geom_count(동일관측치 크게 표시)
ggplot(data=mpg) +  geom_count(aes(x=cty, y=hwy))
ggplot(data=mpg) + geom_jitter(aes(x=cty, y=hwy))

# 4) geom_boxplot()의 위치 조정 기본값은 무엇인가? mpg 데이터셋 시각화를 생성하라
#    position="dodge2"를 기본값으로 사용
ggplot(data=mpg) + geom_boxplot(aes(x=drv, y=hwy, fill=drv))
ggplot(data=mpg) + geom_boxplot(aes(x=drv, y=hwy, fill=class))





### 9. 좌표계(default : coord_cartesian)
# 1) coord_filp : x축과 y축을 바꿔서 나타낸다
ggplot(data=mpg) + geom_boxplot(mapping=aes(x=class, y=hwy))
ggplot(data=mpg) + geom_boxplot(mapping=aes(x=class, y=hwy)) +
  coord_flip()
# 2) coord_quickmap : 지도에 맞는 비율로 자동 설정
library(maps)
usa <- map_data("state")
ggplot(usa, mapping=aes(x=long, y=lat, group=group)) + geom_polygon()
ggplot(usa, mapping=aes(x=long, y=lat, group=group)) + geom_polygon() +
  coord_quickmap()
# 3) coord_polar : 극좌표계
bar <- ggplot(data=diamonds) + 
  geom_bar(mapping=aes(x=cut, fill=cut),
           show.legend = F, width=1) + 
  theme(aspect.ratio = 1) + labs(x=NULL, y=NULL)
  
bar + coord_flip()
bar + coord_polar()

### 1.9 연습문제
# 1) coord_polar()를 사용하여 누적 막대 그래프를 파이차트로 바꿀수 있다.
ggplot(diamonds, aes(x=cut, fill=cut)) + geom_bar(width=1) + coord_polar()

# 2) labs()는 그래프의 축이나 범례 등에 제목, 부제목, 축이름, 범례이름 설정
# 3) coord_map과 coord_quickmap의 차이
#    coord_map      --> 지구의 곡률, 위도, 경도를 고려한 2차원 투영
#    coord_quickmap --> 위도, 경도를 고려한 2차원 투영
nz <- map_data("nz")
nzmap <- ggplot(nz, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
nzmap + coord_map()
nzmap + coord_quickmap()

# 4) 다음 플롯은 도시연비와 고속도로 연비 사이의 관계에 대해 무엇을 알려주는가?
#    일반적으로 고속도로 연비가 도시연비보다 높다고 할 수 있다.
ggplot(data=mpg, mapping=aes(x=cty, y=hwy)) + 
  geom_point() + geom_abline() + coord_fixed()
