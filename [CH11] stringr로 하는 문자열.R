#### R for data science
#### [Chapter11] stringr로 하는 관계형 데이터

### 11.1 준비하기
library(tidyverse)
library(stringr)

### 11.2. 문자열기초

# 1. 문자열 생성 : 따옴표(' or ") 사용
string1 <- "문자열입니다"
string2 <- '문자열 내에 "인용문"이 포함'

# 2. 따옴표나 슬래쉬를 문자열에 포함 ==> writeLines로 출력형태 확인
string3 <- "'안녕하세요'"; string3 %>% writeLines()
string4 <- '"안녕하세요"'; string4 %>% writeLines()
string5 <- "\"안녕하세요\""; string5 %>% writeLines()
string6 <- "\\"; string6 %>% writeLines()


### 11.2 stringr 함수

# 1. str_length() : 문자열 길이
str_length(c("a", "R for data science", NA_character_))

# 2. str_c()      : 문자열 결합
str_c(letters[1:5], "1", sep=",")
str_c(letters[1:5], collapse = "-")  # 문자열 백터를 하나의 문자열로
str_c("|-",c("abc", NA),"-|")
str_c("|-", str_replace_na(c("abc", NA)), "-|")
str_c(c("a","b","c"),c("1", "2", "3"), sep="_", collapse = "+")

# 3. str_sub()    : 문자열 서브셋
x <- c("Apple", "Banana", "Pear")
str_sub(x, start=1L,  end=3L)
str_sub(x, start=-5L, end=-3L)
substr(x, start = 1, stop=3)

# 4. str_to_lower, str_to_upper, str_to_title : 대소문자변경, 첫문자만대문자
str_to_lower(x)
str_to_upper(x)
str_to_title(c("hello", "my name"))
str_to_sentence("this Is A sentence to achieve goal.\nit will be usefull")


# 5. str_sort, str_order : 문자열 정렬
#   (* 베이스함수 sort, order는 컴퓨터 지정 로캘 사용해서 정렬)
str_sort(letters, locale="en")   # 영어
str_sort(letters, locale="haw")  # 하와이어
str_order(letters, locale="en")  # 영어
str_order(letters, locale="haw") # 하와이어
str_order(c("z","a","1","가","<"))
str_sort(c("z","a","1","가","<"))

string <- c("z","a","1","가","<")
string[str_order(string)]

### 11.2 연습문제
# 1. paste()  : str_c(sep=" ")와 유사하게 동작
#    paste0() : str_c(sep="")와 유사하게 동작
paste("x",  1:5, NA); str_c("x", 1:5, NA, sep=" ")
paste0("x", 1:5, NA); str_c("x", 1:5, NA, sep="")

# 2. str_c()에서 sep은 입력 벡터사이에 넣는 문자(열)
#    collapse는 입력 벡터에서 하나의 문자열로 만들때 문자 사이의 넣는 문자(열)

# 3. str_length()와 str_sub()를 이용해 문자열 중앙 문자를 추출하는 함수를 작성
middle_string <- function(x){
  len <- str_length(x)
  if(len %% 2 == 0){
    str_sub(x, len%/%2, len%/%2+1)
  }  else str_sub(x, len%/%2, len%/%2)
}
middle_string("apple")
middle_string("bear")

# 4. str_wrap()의 기능은 무엇인가? 콘솔에 표시되는 너비 결정 + 들여쓰기/내어쓰기
thanks_path <- file.path(R.home("doc"), "THANKS")
thanks <- str_c(readLines(thanks_path), collapse = "\n")
thanks <- word(thanks, 1, 3, fixed("\n\n"))
cat(str_wrap(thanks, width = 100), "\n")
cat(str_wrap(thanks, width = 40),  "\n")
cat(str_wrap(thanks, width = 60, indent = 2), "\n")
cat(str_wrap(thanks, width = 60, exdent = 2), "\n")

# 5. str_trim()의 기능은 무엇인가
#    str_trim()   : side지정 방향으로 문자가 나올때까지 공백제거
#    str_squish() : 양쪽 공백을 제거하고 문자열 내 2개이상의 공백을 1개로 변경
#    str_pad()    : 지정된 width길이까지 공백(문자)추가
str_trim("  \t    we   love   R      \n", side="both")
str_trim("  \t    we   love   R      \n", side="left")
str_squish("\t    we   love   R      \n")
str_pad(c("a", "abc", "abcdef"), 10)
str_pad("a", 10, pad = c("-", "_", " "))

# 6. 예를 들어 벡터 c("a", "b", "c")를 문자열 "a,b,c"로 변환하는 함수를 작성해라
#    만약 길이가 0,1,2 인경우 어떻게 해야 하는지 고려해라
combine_str <- function(x, collapse=""){
  if(length(x)>=2) str_c(x, collapse = collapse)
  else if(length(x)==1) x
  else if(length(x)==0) ""
}
combine_str(letters[1:5])
combine_str(letters[1])
combine_str(x=vector())








### 11.3 정규표현식(regular expression, regexp) : 문자열의 패턴을 기술

# str_view(), str_view_all() : 정규표현식에 매칭되는 문자열들을 표시
x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")
str_view(c("abc", "a.c", "bef"), pattern="\\.")
str_view(c("\\"), pattern="\\\\") # \ == 문자열 "\\" == 정규표현식 "\\\\"

### 11.3.2 연습문제
# 1. 다음의 각 문자열이 "\", "\\", "\\\" 가 \와 매칭되지 않는 이유는?
# 2. 시퀀스 "'\를 매칭하시오
writeLines("\"'\\")
str_view("\"'\\", "\"'\\\\", match = TRUE)
# 3. 정규표현식 \..\..\..은 어떤 패턴과 매칭되는가?
str_view(".a.b.c", pattern="\\..\\..\\..")




### 앵커(anchor) : 고정
# 1. ^ : 문자열의 시작과 매칭
# 2. $ : 문자열의 끝과 매칭
x <- c("apple", "banana", "pear")
str_view(x, "a")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")
str_view(x, "^apple$")

### 11.3.4 연습문제
# 1. 문자열 "$^$"을 어떻게 매칭하겠는가
str_view("$^$", "\\$\\^\\$")
# 2. stringr::words에 담긴 문장에서 정규표현식을 구해라.
# a) 'y'로 시작
str_view(words, "^y", match = T)
# b) 'x'로 끝남
str_view(words, "x$", match = T)
# c) 정확히 세 글자
str_view(words, "^...$", match=T)
# d) 7개 이상의 글자
str_view(words, "^.......", match=T)




### 대체 구문
# \d : 임의의 숫자와 매칭
# \s : 임의의 여백 문자와 매칭
# [abc] : a or b or c와 매칭
# [^abc]: a, b, c를 제외한 문자와 매칭

### 11.3.6 연습문제
# 1. 모음으로 시작함
str_view(words, pattern="^[aeoui]", match=T)
# 2. 자음만 포함한
str_view(words, pattern="^[^aeoui]*$", match=T)
# 3. ed로 끝나지만 eed로 끝나지 않음
str_view(words, pattern="[^e]ed$", match=T)
# 4. ing 혹은 ize로 끝남
str_view(words, pattern="(ing|ize)$",match=T)
# 5. 'c 뒤를 제외하고는 i가 e앞에 항상온다'를 words를 사용해 보여라
str_view(words, pattern = "[^c]ei", match=T)
# 6. 'q' 다음은 항상 'u'인가?
str_view(words, pattern="q[^u]", match=T)




### 반복 : 패턴이 몇 회 매칭하는지 조정
# ? : 0 또는 1회
# + : 1회 이상
# * : 0회 이상
# {n}   : 정확히 n회
# {n,}  : n회 이상
# {,m}  : 최대 m회
# {n,m} :n회 이상 m회 이하

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "CC*")
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")  # 일반적으로 패턴을 만족하는 가장 긴 문자열과 매칭
str_view(x, "C{2,3}?") # ?는 패턴을 만족하는 가장 짧은문자열과 매칭

### 11.3.8 연습문제
# 1. 생략
# 2. 다음의 정규표현식이 어떤 것과 매칭하는지 말로 설명하라
# a. ^.*$ : 모든 문자와 매칭
str_view(words, "^.*$")
# b. "\\{.+\\}" --> 정규표현식 \{.+\} : {}에 둘러싸인 글자 모두 매칭
str_view(c("{hi}", "{a}"), pattern="\\{.+\\}")
# c. \d{4}-\d{2}-\d{2} : 0000-00-00 식의 숫자
str_view("0123-45-67", pattern="\\d{4}-\\d{2}-\\d{2}")
# d. "\\\\{4}" --> 정규표현식 \\{4} :백슬래쉬 4개
str_view("\\\\\\\\", "\\\\{4}")
writeLines("\\\\\\\\")

# 3. 다음 단어를 찾는 정규표현식을 구해라
# a. 세개의 자음으로 시작
str_view(words, pattern="^[^aeiou]{3}", match=T)
# b. 세개 이상의 모음이 연달아 있음
str_view(words, pattern="[aeiou]{3,}", match=T)
# c. 두 개 이상의 모음-자음 쌍이 연달아 있음
str_view(words, pattern="([aeiou][^aeiou]){2,}", match=T)





### 그룹화와 역참조 
# 그룹화 : 소괄호를 이용
# 역참조 : \1, \2와 같이 번호를 이용
str_view(fruit, pattern="(..)\\1", match=T)

### 11.3.10 연습문제

# 다음 표현식이 어떤 문자와 매칭하는가?

# a. (.)\1\1 --> 문자열 "(.)\\1\\1" --> aaa 같은 형태 매칭
str_view("caaa", "(.)\\1\\1", match=T)

# b. "(.)(.)\\2\\1" --> 정규표현식 (.)(.)\2\1 --> abba 같은 형태 매칭
str_view(words, "(.)(.)\\2\\1", match=T)

# c. (..)\1 --> 문자열 "(..)\\1" --> abab 같은 형태 매칭
str_view(words, "(..)\\1", match=T)

# d. "(.).\\1.\\1" --> 정규표현식 (.).\1.\1 --> abaca 같은 형태 매칭
str_view(words, "(.).\\1.\\1", match=T)

# e. "(.)(.)(.).*\\3\\2\\1" --> 정규표현식 (.)(.)(.).*\3\2\1 --> abcdefcba 
str_view("abcdefcba", "(.)(.)(.).*\\3\\2\\1", match=T)


# 2. 아래 조건을 만족하는 정규표현식을 작성하라.
# a. 같은 문자로 시작하고 끝남
str_view(words, "^(.).*\\1$", match=T)
# b. 두 문자 반복이 있음
str_view(words, "(..).*\\1", match=T)
# c. 적어도 세 곳에서 반복되는 문자가 있음
str_view(words, "(.+).*\\1.*\\1", match=T)







### 11.4.1 패턴을 이용한 stringr 도구 함수
## 1. 매칭탐지 : str_detect(string, pattern)
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# t로 시작하는 단어의 개수는?
sum(str_detect(words, "^t")) # 65개
# 모음으로 끝나는 단어의 비율은?
mean(str_detect(words, "[aeiou]$")) # 27.65%

# 모음을 포함하지 않는 단어의 수는?
sum(str_detect(words, "^[^aeiou]+$")) # direct
sum(!str_detect(words, "[aeiou]"))    # indirect

## 2. 문자열 서브셋 : str_subset(string, pattern)
str_subset(words, "x$")
words[str_detect(words, "x$")] # str_detect로 구현가능

df <- tibble(word = words, i=seq_along(words)); df
df %>% filter(str_detect(word, "x$"))

## 3. 한 문자에서 패턴 몇번이 발견되는지 반환
str_detect(c("a", "aa", "aba", "abacab"), "a")
str_count(c("a", "aa", "aba", "abacab"), "a")

# 단어 길이당 모음의 평균 개수를 구하시오
df <- tibble(word=words, i=seq_along(words)); df
df %>% mutate(length=str_length(word)) %>% group_by(length) %>%
  summarise(avg_vowel = mean(str_count(word, "[aeiou]")),
            freq = n()) %>% 
  ggplot(mapping=aes(x=length, y=avg_vowel)) + geom_line() +
  geom_point(mapping=aes(color=freq<50), size=3)

### 11.4.2 연습문제
# 1. 다음 문제를 정규표현식, str_detect() 두가지 방법으로 풀어라
# a. x로 시작하거나 끝나는 모든 단어를 찾아라.
str_view(words, pattern = "(^x)|(x$)", match=T)
words[str_detect(words, pattern="(^x)|(x$)")]

# b. 모음으로 시작하고 자음으로 끝나는 모든 단어를 찾아라.
str_view(words, "^[aeiou].*[^aeiou]$", match=T)
words[str_detect(words, "^[aeiou].*[^aeiou]$")]

# c. 각기 다른 5개의 모음을 하나 이상씩 포함한 단어가 있는가?
words %>% str_subset("a") %>% str_subset("e") %>% str_subset("i") %>%
  str_subset("u") %>% str_subset("o")

# 2. 생략



### 11.4.3 매칭 추출 : str_extract()
head(sentences)

# 색상이 포함된 문장 찾기
color <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(color, collapse = "|")
writeLines(color_match) # 정규표현식 확인

has_color <- str_subset(sentences, color_match)
matches <- str_extract_all(has_color, color_match)
unlist(matches) %>% table()

### 11.4.4 연습문제
# 1. flickered와 같이 비색상 단어가 색상으로 매칭된것을 해결하여라.
color <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c("(\\b(", color, ")\\b)", collapse = "|")
color_match
sentences[str_detect(sentences, color_match)]

# 2. 하버드 문장 데이터에서 다음을 추출해라.
# a. 각문장의 첫번째 단어
str_extract(sentences, pattern="\\w*")
# b. ing로 끝나는 모든 단어
str_extract_all(sentences, "\\b\\w*ing\\b", simplify = T) %>% unique()
# c. 생략




### 11.4.5 그룹화 매칭
# 단어(최소 하나 이상의 공백을 제외한 시퀀스 문자) 찾기
noun <- "(a|the|an) ([^ ]+)"
sentences %>% str_subset(noun) %>% str_extract(noun)
sentences %>% str_subset(noun) %>% str_match(noun)

### 11.4.6 연습문제
# 1. 'one', 'two', 'three' 처럼 숫자 다음에 오는 단어를 구해라
sentences %>% str_subset(pattern="(one|two|three) ([^ ]+)") %>% 
  str_match("(one|two|three) ([^ ]+)")
# 2. 생략

### 11.4.7 매칭치환 : str_replace(), str_replace_all()
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")
x <- c("1 house", "2 house", "3 house")
str_replace_all(x, c("1"="one", "2"="two", "3"="three"))

### 11.4.8 연습문제
# 1. 문자열의 모든 슬래시를 역슬래시로 치환하라
str_replace_all("010-1234-5678", pattern = "-", replacement = "\\\\")
# 2. replace_all()을 사용하여 str_to_lower()를 간단히 구현해라
str_lower <- function(string){
  mypattern <- str_c(LETTERS, "=", letters, sep="")
  str_replace_all(string, c(pattern=mypattern))
}
# 3. 생략


### 11.4.9 문자열 분할 : str_split(), pattern=boundary()
sentences %>% head(5) %>% str_split(" ")
sentences %>% head(5) %>% str_split(" ", simplify = T, n = 3)
"a|b|c|d" %>% str_split("\\|") %>% .[[1]]

x <- "This is a sentence. This is another sentence."
str_split(x, pattern = boundary("word"))
str_split("hi\t my    name is   chanmook", " ")
str_split("hi\t my    name is   chanmook", boundary("word"))

### 11.4.10 연습문제
# 1. "apples, pears, and bananas"와 같은 문자열을 개별 요소로 분할하라
str_split("apples, pears, and bananas", boundary("word"))
str_split("apples, pears, and bananas", "[, ]")

# 2. boundary("word")는 자동으로 구두점, 공백를 기준으로 분할하기에
#    단일공백만으로 분할하는 " "보다 효과적으로 분할이 가능하다.

# 3. 빈 문자열 ""로 분할하면 어떻게 되는가?
str_split(sentences[1], "")





### 11.5 기타 패턴 유형
# 사실 pattern="nana"는 pattern=regex("nana")와 동일하다.
# regex를 명시적으로 사용시 파라미터로 세부사항 조절 가능
str_view(fruit, "nana")
str_view(fruit, regex("nana"))

## regex 파라미터
# 1. ignore_case = T : 문자가 대문자나 소문자 형태 모두 매칭
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = T))

# 2. multiline = T : 앵커 ^와 $이 전체문자열에 매칭이 아닌 각 라인에 적용
x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line") %>% .[[1]]
str_extract_all(x, regex("^Line", multiline = T)) %>% .[[1]]

# 3. comment = T : 정규표현식을 이해하기 쉽게 설명과 공백 사용 가능
phone <- regex("
  \\(?     # 선택적인 여는 괄호
  (\\d{3}) # 지역번호
  [)- ]?   # 선택적인 닫는 괄호, 대시 혹은 빈칸
  (\\d{3}) # 세 자리 숫자
  [ -]?    # 선택적인 빈칸 혹은 대시
  (\\d{3}) # 세 자리 숫자
", comments=T)
str_match("514-791-8141", phone)

# 4. dotall = T : .이 줄바꿈(\n)을 포함한 모든 문자에 매칭
str_view("hi\nmyname is chanmook", regex(".+", dotall=T))
str_view("hi\nmyname is chanmook", regex(".+", dotall=F))


### regex() 대신 사용할 수 있는 함수 
# 1. fixed() : 특수 정규표현식 무시하나 속도는 3배 이상 빠름
microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, regex("the")),
  times = 20
)

# 2. coll() : 표준 대조 규칙 적용해 문자열 비교 + 로캘 선택 가능
#           : 같은 단어를 나타내는 서로 다른 문자(영어, 그리스어 등)에 유용

### 11.5.1 연습문제
# 1. regex()와 fixed()를 각각 사용해 \를 포함하는 문자열을 찾아라
slash <- c("\\", "03\\11\\2020")
str_subset(slash, regex("\\\\")) 
str_subset(slash, fixed("\\"))   # fixed 는 특수정규표현식 무시

# 2. sentences 에서 가장 많이 나오는 단어 5개는?
tibble(word = unlist(str_extract_all(sentences, boundary("word")))) %>%
  mutate(word = str_to_lower(word)) %>%
  count(word, sort = TRUE) %>%
  head(5)

