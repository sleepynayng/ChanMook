#### R for data science
#### [Chapter7] tibble(티블)

### 7.1 준비하기
library(tidyverse)
library(magrittr)

### 7.2 티블 생성하기 : tibble(), tribble()
#       데이터프레임 >> 티블 : as_tibble()

# 티블로 변경
as_tibble(iris)

# 티블 : 변수 참조 가능, 비구문론적 문자열을 변수명 사용 가능 
data.frame("x"=1:5, "y"=1, ":)"=x^2+y) # error
tibble( x=1:5, y=1, `:)`=x^2+y )       # work

tibble(
   c("a", "b"),
   c(2, 1),
   c(3.6, 8.5)
)

tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)


### 7.3 티블 vs 데이터프레임 : 화면출력, 서브셋

# 7.3.1 화면출력
# 티블 : 열이 콘솔을 넘어가지 않게 설계
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, rep=T)
)

# 명시적 설정 : print(n="행수", width="너비")
#             : option(tibble.print_max=n, tibble.print_min=m) : m행이상인 경우
#                                                              : n행만 표시
#             : option(tibble.width=Inf) : 모든 열을 출력
# 7.3.2 서브셋(subset)
df <- tibble(x=runif(5), y=rnorm(5)); df
df$x; df[["x"]]; df[[1]]
df %>% "[["("x")
df %>% .$x
df %>% .[["x"]]

### 7.4 연습문제
# 1. 어떤 객체가 티블인지 알 수 있는 방법은??
#    ==> str, class, is_tibble
df <- data.frame(x=1:10, y=letters[1:10]); df
tb <- tibble(x=1:10, y=letters[1:10]); tb
class(df); class(tb)
is_tibble(df); is_tibble(tb)
is.data.frame(df); is.data.frame(tb)

# 2. data.frame과 이에 해당하는 티블에서 다음 연산들을 비교하여라
df <- data.frame(abc=1, xyz="a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

tb <- tibble(abc=1, xyz="a")
tb$x
tb[, "xyz"]
tb[, c("abc", "xyz")]

# 3. 객체에 변수 이름을 저장하고 있는 경우, 티블에서 이 참조 변수를 어떻게
#    추출할 것인가.
var <- "abc"
df$var; tb$var       # error : no exist var column
df[[var]]; tb[[var]] # work well!

# 4. 다음의 데이터프레임에서 비구문론적 이름을 참조하는 방법을 연습해보라.

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
); annoying

# a. 1이라는 이름의 변수를 추출하기.
annoying$`1`
annoying %>% .$`1`
annoying %>% .[[1]]

# b. 1대 2의 산점도를 그리기
annoying %>% ggplot(mapping=aes(x=`1`, y=`2`)) + geom_point()

# c. 열2를 열1로 나누어 3이라는 새로운 열을 생성하기
annoying %<>% mutate(`3`=`2`/`1`)

# d. 열의 이름을 one, two, three로 변경하기
annoying %>% rename("one"=`1`, "two"=`2`, "three"=`3`)


# 5. tibble::enframe()은 어떤 동작을 하는가?
#    enframe : 원자벡터 or 리스트 -> 데이터프레임
#    deframe : 데이터프레임 -> 원자벡터 or 리스트
enframe(c(a = 5, b = 7))
enframe(list(one = 1, two = 2:3, three = 4:6))
deframe(tibble(a = 1:3))
deframe(tibble(a = as.list(1:3)))

# 6. 티블의 바닥글에 출력되는 열이름개수를 제어하는 옵션은?
print(nycflights13::flights)
print(nycflights13::flights, n_extra = 0)
