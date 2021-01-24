#### R for data science
#### [Chapter8] readr로 하는 데이터 불러오기

### 8.1 준비하기
library(tidyverse)
library(readr)

### 8.2 불러오기

### 여러가지 읽기 함수
# read_csv()   : 쉼표
# read_csv2()  : 세미콜론
# read_tsv()   : 탭(tab)
# read_delim() : 구분자
# read_fwf()   : 고정 너비 파일
# read_table() : 고정 너비 파일 (열이 공백)
# read_log()   : 로그 파일


### read_csv를 중심으로 설명
# read_csv의 파라미터 : file, skip, comment
# 1. file      : 파일의 경로와 파일명
# 2. skip      : 첫 몇줄을 스킵할지 결정
# 3. commnet   : 해당되는 문자열로 시작되는 모든 줄 생략
# 4. col_names : F일 경우 첫 행을 변수명으로 설정x 
#              : 문자형 백터일 경우 변수명으로 지정
# 5. na        : 지정된 문자열을 NA 처리해서 파일 불러옴

read_csv("a,b,c\n1,2,3\n4,5,6")

read_csv("메타 데이터 첫번째행
         메타 데이터 두번째행
         x, y, z
         1, 2, 3", skip=2)

read_csv("!이줄은 스킵할거야1
         x, y, z
         ! 이줄은 스킵할거야2
         1, 2, 3", comment="!")

read_csv("1,2,3\n4,5,6", col_names = F)
read_csv("1,2,3\n4,5,6", col_names = c("x","y","z"))
read_csv("1,2,3\n4,.,6", col_names = F, na = ".")

### 8.2 연습문제
# 1. 필드가 "|"로 분리된 파일을 읽으려면 어떤 함수를 사용하겠는가?
string <- "x|y|z\n1|2|3\n4|5|6"
string
read_delim(string, delim = "|")
read_csv(string, delim="|")
?read_csv
# 2.read_csv()의 다양한 파라미터를 살펴보자
# progess : T로 설정시 데이터 읽은 정도를 바로 표현
# n_max   : 읽을 행의 수

# 3. read_fwf()에서 열의 너비를 지정해주는 col_positions이 중요하다.
# 4. CSV 파일에서 쉼표가 포함되어 있는 경우가 있다
string <- "x,y\n1,'a,b'"
read_csv(string, quote="'")
read_delim(string, delim = ",", quote="'")

# 5. 아래 코드를 실행하여라
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")





### 8.3 파싱
# parse_*() : 문자형 백터를 입력받아 함수에 맞는 특수화된 벡터 반환
parse_logical(c("TRUE", "FALSE", "TRUE"))
parse_integer(c("1", "2", "3"))
parse_date("2020-01-01")
parse_integer(c("1", "231", ".", "456"), na=".")

# 파싱 실패시 경고 메세지 출력
# 실패가 많을 경우 problems() 사용
x <- parse_integer(c("abc", "123", "45", "6", "xyz")); x
problems(x)

# 파싱함수(파서)
# parse_logical, parse_integer   : 논리형, 정수형 파서
# parse_double, parse_number     : 수치형 파서
# parse_character                : 문자형 파서
# parse_factor                   : 범주형 파서
# parse_datetime or date or time : 데이트형 파서

## 숫자 파싱 포인트 : 소수, 단위와 결합한 숫자, 그룹화
# 1. 소수처리
parse_double("1.23")
parse_double("1,23", 
             locale=locale(decimal_mark = ",")) # 특정국가에서 소수점을 ,로 사용

# 2. 숫자앞뒤의 비수치문자 처리 --> parse_number
parse_number("$100")
parse_number(c("8.5%", "21.3%", "29.9%"))
parse_number("It cost $123.45달러\n")

# 3. 그룹화 마크 제거
parse_number("$123,456,789")
parse_number("123.456.789", 
             locale=locale(grouping_mark = ".")) # 특정 유렵 국가
parse_number("123'456'789",
             locale=locale(grouping_mark = "'")) # 스위스


## 문자 파싱 포인트 : 컴퓨터가 문자열을 포현하는 방식은 여러가지

# 인코딩(encode) : 특정 문자(기호)를 숫자에 매핑 (여러 인코딩 기법 존재)
# 디코딩(decode) : 매핑된 숫자를 사람들이 인식할 수 있는 문자로 변환
# readr 패키지는 UTF-8 (문자코드규약)을 사용

# 1. 문자 기본 표현 확인 --> charToRaw()

charToRaw("Hadely")
charToRaw("안녕")

# 2. 인코딩 & 디코딩 (default = locale(encoding="UTF-8"))

x1 <- "El Ni\xf1o was particularly bad this year"; x1
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"; x2
x3 <- "hello I love R"; x3

parse_character(x1, locale = locale(encoding = "Latin1"))
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
parse_character(x3, locale = locale(encoding = "UTF-8"))

# 인코딩을 모른다면? guess_encoding 사용!
guess_x1 <- guess_encoding(charToRaw(x1))
parse_character(x1, locale = locale(encoding = guess_x1$encoding[1]))

guess_x2 <- guess_encoding(charToRaw(x2))
parse_character(x2, locale = locale(encoding = guess_x2$encoding[1]))


## 범주형 파싱 포인트 : parse_factor()에 levels로 범주 지정
parse_factor(x=c("L", "H", "M", "H", "M", "a"),
             levels=c("L", "M", "H"), ordered=T)


## 데이트형 파싱 포인트 : 날짜|날짜-시간|시간 구분

# 날짜-시간 : parse_datetime
# 날짜      : parse_date
# 시간      : parse_time

parse_datetime( "2020-10-01T2010" )
parse_datetime( lubridate::now() %>% as.character() )
parse_date(c("2020-01-01", "2020/01/02", "2020-01/03")) # -나 /로 년월일 구분
parse_time(c("01:10 am", "01:10 pm", "20:03", "07:12:23")) 

## hms 패키지를 이용한 시간 클래스
library(hms)
# %Y, $y     : 연도
# %m, %b, %B : 월
# %d, %e     : 일
# %H, %I, %P : 시간, am/pm
# %M, %S     : 분, 초
# %., %*     : 숫자가 아닌 문자 스킵

parse_date("01/02/15", "%m/%d/%y") # 월-일-연도로 파싱
parse_date("01/02/15", "%d/%m/%y") # 일-월-연도로 파싱
parse_date("01/02/15", "%y/%m/%d") # 연도-월-일로 파싱
parse_date("$$01/02/15", "%*%d/%m/%y") # 숫자가 아닌 문자 제거

### 8.3 연습문제
# 1. locale()에서 가장 중요한 인수는 무엇인가?
# 날짜-시간 형식 : date_names, date_format, time_format
# 숫자 형식      : decimal_mark, grouping_mark
# 문자 형식      : encoding

# 2.숫자 형식에서 locale의 decimal_mark와 grouping_mark를 동일문자
#   로 설정하면 어떻게 되는가? --> 오류 발생
parse_double("1.23", locale=locale(decimal_mark = ".", grouping_mark = "."))

# 3. locale()의 date_format과 time_format은 파싱되는 데이터의
#    날짜 또는 시간의 기본 형식을 지정해준다.
parse_date(c("2020/01/01", "2020/01/02"),
           locale=locale(date_format="%Y/%m/%d"))

# 4. 생략
# 5. read_csv()와 read_csv2()는 구분자가 다르다. 각 "," ";"
# 6. 생략
# 7. 다음 데이터를 적절한 형식으로 파싱하라
parse_date("January 1, 2010", "%B %d, %Y")
parse_date("2015-Mar-07", "%Y-%b-%d")
parse_date("06-Jun-2017", "%d-%b-%Y")
parse_date(c("August 19 (2015)", "July 1 (2015)"),
          format = "%B %d (%Y)")
parse_date("12/30/14", "%m/%d/%y")
parse_time("1705", "%H%M")
parse_time("11:15:10.12 PM", "%H:%M:%OS %p")





### 8.4 파일 파싱하기 
# guess_parser() : 파싱 유형 추측(처음 1000개의 데이터로)
# parse_guess()  : 추정된 유형으로 데이터 파싱
guess_parser("2010-10-01")
guess_parser("15:01")
guess_parser("hello")
guess_parser("TRUE")
guess_parser(c("1", "5", "9"))
guess_parser(c("12,234,567"))


## 참고. challenge.csv의 y의 첫부분은 NA값이다
challenge <- read_csv(readr_example("challenge.csv"))
problems(challenge)
challenge %>% tail()
# 전략1 : 각 변수에 맞는 col_*() 함수 사용
challenge1 <- read_csv(readr_example("challenge.csv"),
                      col_types = cols(
                        x=col_double(),
                        y=col_date()
                      ))
challenge1 %>% tail()

# 전략2 : guess_max로 행수 조절
challenge2 <- read_csv(readr_example("challenge.csv"),
                       guess_max =1500)
challenge2 %>% tail()

# 전략3 : 모든열을 문자열로 읽고 type_convert 사용
challenge3 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))
challenge3
type_convert(challenge3)

# 전략4 : 파싱에 문제가 있는경우 read_lines(), read_file() 사용






### 8.5 파일에 쓰기 및 기타 읽기

# 쓰기 함수
# csv로 저장하면 열유형 사라짐
# write_csv()       : ','로 구분하여 저장
# write_tsv()       : tab으로 구분하여 저장
# write_excel_csv() : csv파일을 엑셀로 저장

# 기타 유형
# write_rds(), read_rds() : 커스텀 바이너리 형식으로 저장/읽기
# haven 패키지            : SPSS, SAS 파일 읽기
# readxl 패키지           : 엑셀파일(.xls, .xlsx)읽기
# DBI 패키지              : 데이터베이스 쿼리 실행 및 읽기/저장
