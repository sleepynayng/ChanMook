#### R for data science
#### [Chapter14] magrittr로 하는 파이프

### 14.1 준비하기
library(magrittr)

### 14.2 파이프 대안
# 1. 중간 단계마다 새 객체로 저장 --> 겹치는 부분은 공유되서 메모리걱정x
# 2. 원본 객체를 여러 번 덮어쓰기 --> 디버깅 고통
# 3. 함수 작성하기 --> 여러 함수 중복시 알아보기 힘듬
# 4. 파이프 사용하기 

### 14.3 파이프를 사용하지 말아야 할 경우
# 1. 현재 환경을 사용하는 함수
#    --> %>%를 사용하는 경우 임시환경에 객체를 할당해서 현재환경에서 사용x
assign("x1", 10); x1
"x1" %>% assign(10000); x1

env <- environment()
"x1" %>% assign(10000, envir = env); x1

get("x1")
"x1" %>% get()

# 2. 지연계산법을 사용하는 함수
#    R에서 함수 인수 계산은 함수 호출전이 아닌 인수 사용할때 이루어짐
#    --> 파이프는 각 요소를 순서대로 계산하므로 요류발생
tryCatch(stop("!"), error=function(e) "An error")
stop("!") %>% tryCatch(error=function(e) "An error")


# 3. 파이프 길이가 10단계 이상 넘어갈때 --> 중간 객체 생성
# 4. 다중입력 or 다중출력인 경우 파이프 적용안됨
# 5. 선형적으로 적용하면 안되는 그래프나 트리같은 구조에 적용x


### 14.4 magrittr의 기타 도구
# 1. %T>% : %>%처럼 동작하나 파이프 왼쪽에 있는 객체 출력
rnorm(100) %>% matrix(ncol=2) %T>% plot() 
rnorm(100) %>% matrix(ncol=2) %>% plot()

# 2. %$% : 데이터프레임의 변수를 명시적으로 참조할때
mtcars %$% cor(disp, mpg)

# 3. %<>% : 할당
mtcars %<>% transform(cyl=cyl*2)
