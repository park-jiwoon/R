(a<-2);
(b<-5);
ls()#리녹스에서 파일 리스트 > 변수리스트트
Sys.getenv("JAVA_HOME"); #자바도 기본적으로 설치하고 실행할 것
print(a);

x<- 5;
x
typeof(x) #강제 형변황 "double"" 숫자를 잘 구분
mode(X)    # 문자 숫자 불린
class(x) #클래스 종류


#데이터 타입을 변경하고 싶다.(as.integer)
x=as.integer(x);  #  . 은 자바에서는 클래스 내부 참조할떄 
mode(x)           # . 은 그냥 이름 (r 에서는)
typeof(x)
#객체지향프로그림-> 함수형그로그래밍 /구조적프로그래밍으로 생성

y<-x;  #값에 의한 대입  
y

x+y
x*y
x-y
x/y
y%%x #나머지 연산자 #R 에서 %% 는 연산자를 정의 할 때에도 사용!(함수)
y%/%x #몫 연산자
y^x #y의 x승

# 관계연산자(if 문 대신 사용)
x<-5;
y<- 16;
x<y
x>y
x<=5
y==16
x!=5;
# R 에서는  TRUE/FALSE 대분자
x<- c(TRUE, FALSE, 0, "6")   #combination
x
# 문자 > 숫자 > boolean
x<- 5
x+1
x
# R 에서는 대입으로 값을 변경
(x<-c(TRUE,FALSE,0,6))
(y<- c(FALSE,TRUE,FALSE,TRUE))
!x   #부정
x<-c(TRUE)
y<-c(TRUE)
x&y        # 요소별로 판단    
x%%y     # 양쪽이 모두 참일때 참
t<-1:10  # 1부터 10까지 range 연산자
t
v1<-8
v2<-15
v1
v1%in% t    #in 은 포함 연산자 하나라도 포함 되어 있다면 TRUE
v2%in% t

# 벡터 연산은 벡터화 연산을 수행 core가 6개 /부동소수점연산기가 6개라
# matrix 는 벡터가 모여있는 것 (벡터로 연산)
# 데이터 프레임은 벡터가 열로 적용
# 벡터화 연산은 multi core 를 이용해서 동시에 연산 (병렬연산)
c(1,3,5,7,9) * 2   #for 문 없이 요소별로 적용
c(1,3,5,7,9) * c(2,4)   #짝이 맞아야 한다.
c(1,3,5,7,9,10) * c(2,4)
c(2,4) *c(1,3,5,7,9,10) # 곱셈은 바뀌어도 같지

factorial(1:5) 
exp(2)   #2.718 지수
#R의 기본 데이터 타입은 벡터
exp(2:10) # 2~10// 파이썬은 끝이 포함 x 
cos( c(0,pi/4))   
sqrt(c(1,4,9,16)) #루트
#R은 대소문자 구분
sum(c(1,2,NA,3))  #NA는 결측치
1/0 #무한대
0/0 #NaN 숫자가 아니다!
Inf/NaN 
Inf/Inf
log(Inf)
Inf+NA
vec<- c(0,Inf,NaN,NA)
typeof(vec)
mode(vec)
class(vec)
is.finite(vec)   #check 함수 (유한하냐?)
is.nan(vec)    #값이 nan인지 boolen
is.na(vec)  #결측치있냐?
is.infinite(vec)
sum(vec)

# 연산자 정의
# 데이터 타입을 지정하지 않기 떄문에 함수의 리턴타입이 없음
# R의함수는 1급함수 (변수에 대입이 가능)
`%divisible%` <- function(x,y)    #함수 만드는 법
{
  if(x%%y==0) return(TRUE)    #나누어 떨어지면 TRUE
  else        return(FALSE)
}
10 %divisible% 3
10 %divisible% 2
`%divisible%`(10,5)
# 벡터 연산
x<- c(5,6,7)
y <-c(1,2,3)
x%%y         #나머지
x%*%y       #벡터의 내적/ 중요한것 *벡터는 내적 행렬에서는 행렬 곱
# 내적은 두 벡터의 요소끼리 곱한다음 다 합한 결과 값
# 내적의 결과값의 의미는 한 벡터의 크기*한 벡터의 크기 * cos(theta) 의 의미
sum(x*y)      
x%/%y         #몫

# R에서 사용하는 상수
LETTERS #알파엣을 의미 A~Z
letters #a~z 소문자
month.name
month.abb  #약어
pi
class(month.name)

# vector : 열 중심 저장
# 열 중심으로 하는 이유 : 검색이 중요한 것이 아니라, 통계량이 중요하기떄문
# 합계, 평균, 분산, 표준편차, 표준오차

a<-c('apple','orange','banana')
mode(a)
class(a)
length(a)
NROW(a) #행
NCOL(a) #열

# 벡터 배열의 인덱스는 1부터 시작(추가가 가능)
x<- c(1,5.4 , TRUE , "hello")
x
x[5]
x[5]=123
x
x[200]=100
x
x<- c(1,5.4,TRUE)
x
# 문자 >숫자 > boolean
(x=vector("list",10))  #[[3]]
(x=vector("numberic", 10))
rm(x)  #메모리에서 삭제 (공간확보가 중요)
x

# 연속적인 숫자 표현
(x<-1:7)   #범위는 정수만(지정)
(y<- 2:-2)

seq(1, 3.2 , by=0.2)    #sequence 연속적 1부터 3.2 까지 0.2씩 더해서
seq(1,5, length.out=4)
seq(1, 6 , length.out=4)    #1~ 6 까지 4개로

(b<-rep(1:4,2))  #repeatation  1~4 까지 2번 반복
(d<- rep(1:3 , each = 3))   #3번씩
(d<-rep (1:3,2,each=3))   #each 부터 적용

# indexing
x<- c(1:9)
x
x[3]
x[c(2,4)]  # 2번째 4번쨰 보여줘
x[-1]  # 1번째꺼 제외하고 보여줘

x[c(2,-4)]      # 음수, 양수 인덱스는 동시에 사용이 불가하다
x[c(2.4, 3.54)] # 소수점 인덱스는 절댓값으로 
# R에서는 연산의 갯수가 일치 해야한다
# 부족할 시 반복
x[c(TRUE,FALSE,FALSE,TRUE)]   # boolean indexing (if문)
x<-c(5:12)
x
x[x<3]
x[x>3]
x>3
x<3

# vector 에 행이름을 부여
(x <- c("first"=3 , "second"=0, "third" = 9))
x
x["second"]
x[c("first","third")]
x
x= c(-3,-2,-1,0,1,2)
x
# rm(x[1])
# x[1]=NULL
x

# 문제: x에 1,2,3 을 대입하고 y에 2,3,-,4를 대입한 다음 사칙연산
(x<-c(1,2,3))
(y<-c(2,3,-4))
x
y
sum(x)
sum(x,y)
sum(y)
sum(x)*sum(y)
sum(x)-sum(y)
sum(x)%%sum(y)
sum(x)%/%sum(y)
x+y
x-y
x*y
x/y
x%*%y

x<- 1:5
all(x>2)  #모두가 만족해야 TRUE
any(x>2)  #하나라도 만족하면 TRUE

# 데이터 정렬
x<-c(3,2,6,4,5,8,1)
x
# 매개변수가 상이해도 작동하는 이유
# R은 객체지향프로그래밍 
# 객체지향 프로그램에서 함수는 디폴트 매개변수를 갖는다
sort(x) # =sort(x, decreasing = FALSE) #오름차순
sort(x, decreasing = TRUE)  # 내림차순
x
A<-order(x)  # 인덱스 순서로 정렬
A
order(x, decreasing =TRUE)
x[order(x)] #인덱스 값에 원래 데이터를 넣어줌
# order 이 있는 이유 : 데이터가  2개 일때 다른 열을 중심하고 정렬

# NA 연산
x= c(2, NA , 3,1,4)
sum(x)
sum(x,na.rm=TRUE)
mean(x,na.rm=TRUE)   #평균
median(x,na.rm=TRUE) #중위수
# 이상치 떄문
prod(x,na.rm=TRUE)   #product

# 행 이름부여
# 주의점:원본을 변화시키는 함수가 있고 아닌 함수가 있다
vectorA<- c(1,2,3,4)
names(vectorA)<- c("국어","영어","수학","과학")
vectorA
vectorA["국어"]
vectorA[2]
vectorA[-1]
vectorA[c(1,3)]
vectorA[vectorA>5]
vectorA[c(FALSE,FALSE,TRUE)]
append(vectorA, c(3,4,5)) #데이터 추가! 값이 저장되지는 않는다다
vectorA
vectorB=append(vectorA,c(3,4,5)) # 대입을 해주어야 변화
vectorB

#CRUD
#-vector
#-c, 공간확보를 위해서 (vector)
#-read > index로 ([],vector인덱스,  음수인덱스, boolean indexing(if문))
#-update : 인덱스로 수정
#-delete : 값 삭제 불가 (삭제 시 NA)
#변수 삭제는 가능 : rm(변수)


#집합 연산
x
y
union(x,y)  #합:중복은 한개로 출력력
intersect(x,y) #교
setdiff(x,y) #차집합
setdiff(y,x)
setequal(x,y) # 두 집합이 같은지 검사


#subset  데이터중 일부만 잡아내기
x<- c(3,2,6,4,5,8,1)
subset(x,x>3)
which(x*x >8)  #값의 인덱스

# samlpe 선택
# 복원 추출 replace=T  # prob는 100%에서 확률
(x<-c(sort(sample(1:3, 3, replace=T, prob=c(0.5, 0.3, 0.2)))))
x
#which 의 결과는 인덱스
nums<-c(5,8,10,NA,3,11)
nums
which.min(nums)
which.max(nums)
nums[which.min(nums)]
nums[which.max(nums)]

# 문제
# 1. 다음과 같은 벡터 객체를 생성하시오
#1) Vector1 벡터 변수를 만들고 , "R"문자가 5회 반복되도록 하시오
#2) Vector2 벡터 변수에 1~20 까지 3 간격으로 연속된 점수를 만드시오
#3) Vector3 에는 1~10 까지 3 간격으로 연속된 정수가 3회 반복되도록 만드시오
#4) Vector3 에는 Vector2~3가 모두 포함되는 벡터를 만드시오
#5) 25~-15까지 5간격으로 벡터생성 - seq()함수 이용
#6) Vec4 에서 홀수번째 값들만 선택하여 Vec5에 할당하지오 ( 첨자이용)

#1)
(Vector1<-rep("R",5))
Vector1

#2)
Vector2<-seq(1,20 , by=3)
Vector2

#3)
rep((Vector3<-seq(1,10 ,by=3)) ,3)
(Vector3<-rep(seq(1,10 , by = 3),3))
#4)
Vector3=append(Vector2,Vector3)  # 벡터2에 벡터3을 추가
Vector3
Vector3%in%Vector2
#5)
Vec4<-seq(25,-15 , by=-5)
Vec4
#6)
(Vec5=Vec4[seq(1,length(Vec4),2)])  # 1부터 2씩 증가하는 수의 열 즉 홀수인덱스만 호출
(Vec5=Vec4[seq(1,length(Vec4))%%2!=0]) 

# 기본패키지, 설치는 되었는데 로딩이 안된 패키지, 설치도 안된 패키지

#설치된 패키지 로딩하는 방법 
#install.packages("NISTunits",dependencies=TRUE)
# 제거 romove.packages(~~)
library(NISTunits)
NISTdegTOradian(180) #컴퓨터 각도는 라디안을 사용
NISTradianTOdeg(pi)
ang<-45
(a<-NISTdegTOradian(ang))  # 각도를 라디안으로
(b<- NISTradianTOdeg(a))  

(ratio=sin(a))  #길이의 비 / 대각선하고 수직선하고의 비
#만약 대각선이 10이라면 수직선은 얼마인가
(result = 10* ratio)
#길이의 비를 라디안으로 
(ang_rad=asin(ratio))
NISTradianTOdeg(ang_rad)

#Q : 각도가 30도 이고 대각선의 길이가 5 일떄 수직하는 변의 길이는?
#Q : 밑 변의 길이는?
(radian30=NISTdegTOradian(30))
(ratio=sin(radian30))
5*ratio

(radian30=NISTdegTOradian(30))
(ratio=cos(radian30))
5*ratio

#Q : 직각을 이루는 두 변의 길이가 밑변은 10 높이는 5 라면 각도는?
(ratio=(5/10))   #tan
#길이의 비를 각도로 바꾸는 함수 atan
(result= atan(ratio))
NISTradianTOdeg(result)

# 두 벡터의 사이각은 내적을 이용
x<-c(1,2,3)
y<-c(2,3,-4)
sum(x*y)   #내적 값이 -4
(dotresult = x%*%y)
#|A||B| cos theta
(xnorm=sqrt(1^2+2^2+3^2))
(ynorm=sqrt(2^2+3^2+(-4)^2))

(costheta= dotresult/(xnorm*ynorm))
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)

#Q:사이각
x<-c(1,0,0)
y<-c(0,1,0)
dotresult= x%*%y
xnorm=sqrt(1)
ynorm=sqrt(1)
costheta=dotresult/(xnorm*ynorm)
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)

#Q 사이각
a<-c(1,2,4,1,2,4,3,6,7,2,0)
b<-c(1,3,2,3,2,2,1,2,1,1,1)
(dotresult=a%*%b)
(Anorm=sqrt(sum(a^2)))
(Bnorm=sqrt(sum(b^2)))   # 거리값
(anorm=sqrt(1^1+2^2+4^2+1^2+2^2+4^2+3^2+6^2+7^2+2^2+0^2))
(bnorm=sqrt(1^2+3^2+2^2+3^2+2^2+2^2+1^2+2^2+1+1+1))
(costheta=dotresult/(anorm*bnorm))
radianangle=acos(costheta)
NISTradianTOdeg(radianangle)

