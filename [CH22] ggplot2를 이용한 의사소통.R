#### R for data science
#### [Chapter22] 그래프를 통한 의사소통

### 22.1 준비하기
library(tidyverse)

### 22.2 라벨 : labs(title, subtitle, caption, tag)
mpg %>% ggplot(mapping=aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) + geom_smooth(se=F) + 
  labs(title = "엔진크기와 연료의 관계", 
       subtitle = "엔진크기가 커질수록 일반적으로 연비는 감소",
       caption = "캡션", tag="태크") 

mpg %>% ggplot(mapping=aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) + geom_smooth(se=F) +
  labs(
    x = "배기량(L)",
    y = "고속도로연비(mpg)",
    color = "차종"
    )

### 22.3 주석 : geom_text(x, y, label)
#             : geom_label(x, y, label) (geom_text와 유사하나 글자뒤에 사각형이 덧그려짐)
#             : ggrepel::geom_label_repel(x, y, label) (라벨이 겹치는 문제를 해결)
best_in_class <- mpg %>% group_by(class) %>%
  filter(row_number(desc(hwy))==1)

# 1. geom_text
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color=class)) +
  geom_text(data=best_in_class, aes(label=model))

# 2. geom_label
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color=class)) +
  geom_label(data=best_in_class, aes(label=model), nudge_y=2, alpha=0.8)

# 3. geom_label_repel
ggplot(mpg, aes(displ, hwy)) + 
  geom_point(aes(color=class)) +
  ggrepel::geom_label_repel(aes(label=model), data=best_in_class)

class_avg <- mpg %>% group_by(class) %>% 
  summarise(displ = median(displ), hwy = median(hwy))

# 4. 범주를 제거하고 플롯에 그리기
ggplot(mpg, aes(displ, hwy, color=class)) +
  ggrepel::geom_label_repel(aes(label=class),
                            data=class_avg, size=6,
                            label.size=0) +
  geom_point() + theme(legend.position = "none")

# 5. 플롯의 모서리에 라벨 위치하기 : hjust(가로폭조정), vjust(세로폭조정)
label <- mpg %>% summarise(displ = max(displ), hwy = max(hwy),
                           label = paste("Increasing engine size is\n", 
                                         "related to decreasing fuel enconomy"))
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label=label), data=label)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label=label), data=label, vjust="top", hjust="right")





### 22.4 스케일 ==> 일반적으로 ggplot2는 자동으로 스케일 추가
#               ==> scale_심미성_유형(breaks, labels)
#               ==> theme(legend.position) : 범례 위치 조정

mpg %>% ggplot(aes(displ, hwy)) + geom_point(aes(color=class))
mpg %>% ggplot(aes(displ, hwy)) + geom_point(aes(color=class)) +
  scale_x_continuous() + scale_y_continuous() + scale_color_discrete()

mpg %>% ggplot(aes(displ, hwy)) + geom_point() +
  scale_y_continuous(breaks=seq(15, 40, by=5))  # breaks로 y축 눈금 표시값 제어
mpg %>% ggplot(aes(displ, hwy)) + geom_point() +
  scale_y_continuous(labels=NULL) +
  scale_x_continuous(labels=NULL)

presidential %>% mutate(id=33+row_number()) %>%
  ggplot(aes(x=start, y=id)) +
  geom_point() +
  geom_segment(aes(xend=end, yend=id)) +
  scale_x_date(name=NULL, breaks=presidential$start, date_labels = "%Y")

base <- ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=class))
base + theme(legend.position = "left")
base + theme(legend.position = "right")
base + theme(legend.position = "bottom")
base + theme(legend.position = "top")
base + theme(legend.position = "none")
base + geom_smooth(se=F) + theme(legend.position = "bottom") +
  guides(color=guide_legend(nrow=1, override.aes = list(size=7)))

# 스케일 조정
ggplot(diamonds, aes(carat, price)) + geom_bin2d()
ggplot(diamonds, aes(carat, price)) + geom_bin2d() +
  scale_x_log10() + scale_y_log10()

# 색생 변경 1 : scale_color_brewer(palette)
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=drv))
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=drv)) +
  scale_color_brewer(palette = "Set1")

# 색상 변경 2 : scale_color_gradient() or scale_color_manual()
#             : 범주에 지정된 색상 존재시 사용
presidential %>% mutate(id=33+row_number()) %>%
  ggplot(aes(start, id, color=party)) + geom_point() +
  geom_segment(aes(xend=end, yend=id)) +
  scale_color_manual(values=c(Republican="red", Democratic="blue"))

# 색상 변경 3 : viridis::scale_fill_viridis()
#             : 연속형 범주에서 인지 속성이 좋은 색상사용
tibble( x=rnorm(10000), y=rnorm(10000) ) %>%
  ggplot(aes(x, y)) + geom_hex() + coord_fixed()
tibble( x=rnorm(100000), y=rnorm(100000) ) %>%
  ggplot(aes(x, y)) + geom_hex() + coord_fixed() +
  viridis::scale_fill_viridis()










### 22.5 확대 및 축소 --> coord_cartesian(xlim, ylim)
ggplot(mpg, aes(displ, hwy)) + geom_point(aes(color=class)) + geom_smooth() +
  coord_cartesian(xlim=c(5,7), ylim=c(10,30))
mpg %>% filter(displ>=5, displ<=7, hwy>=10, hwy<=30) %>%
  ggplot(aes(displ, hwy)) + geom_point(aes(color=class)) + geom_smooth()

### 22.6 테마 : theme_classic() 등 8가지 존재 (확장패키지 : ggthemes)
mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color=class)) + geom_smooth(se=F) + 
  theme_bw()
mpg %>% ggplot(aes(displ, hwy)) +
  geom_point(aes(color=class)) + geom_smooth(se=F) + 
  ggthemes::theme_excel_new()

### 22.7 저장 : ggsave()
