---
title: "Team_06_Stress_Bae"
output: github_document
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#1. 요약(초록): 2012년부터 2016년까지 격년 동안의 스트레스 정도 자료에서 학력과 세대별로 스트레스 정도를 그래프 비교를 통해 특징을 찾아보고, 그러한 특징을 기반으로 어떤 요인들과 더 연관이 될지에 대해 생각해보는 과정이다. 20대부터 60대의 고졸자 및 대졸자 학력을 가진 사람들의 스트레스 정도를 비교분석한 결과, 2012, 2014, 2016년도 모두 대체로 고졸학력자의 스트레스를 느끼는 정도가 대졸학력자의 스트레스를 느끼는 정도보다 높은 것을 알 수 있다. 2012년에는 학력에 관계없이 20대의 스트레스를 느끼는 정도가 가장 높지만 2014년과 2016년에는 20대의 스트레스를 느끼는 정도가 낮다. 반면 30대와 40대의 스트레스를 느끼는 정도가 증가하고 다시 50대부터 줄어드는 경향을 보인다.

#2. 분석 주제: 우리나라 사람들의 학력 및 세대에 따른 스트레스 정도 파악해보기

#3. 데이터 선정
##데이터 선정 이유: 본인이 판단하기에, 분석하고자 하는 주제에 알맞았으며, 대한민국 전국 시민 중에서 임의로 추출된 모집단 사이에 스트레스와 학력을 포함한 다양한 변수가 존재하여 데이터간 상관관계 분석에 용이하다고 판단하였다. 
##데이터 소개: KOSIS에서 제공하는 우리나라 사회조사로, 전반적인 생활 스트레스 정도에 관한 데이터이다. 전국을 대상으로 하며, 변수로는 지역, 성별, 연령, 교육정도, 혼인상태, 경제활동, 산업, 직업, 가구소득, 흡연량 등 매우 다양하게 존재한다.

#4. 분석
##분석 목적 및 방법 : 본인이 다음 분석을 하는 목적은 학벌주의가 만연한 대한민국 사회에서 학력이라는 부분이 우리나라 각 세대의 사람들에게 어떤 영향을 미칠까 궁금했다. 이 분석을 통해 각 세대별로 학력과 스트레스 사이의 연관성을 확인해보는데 그 목적이 있다. 분석 방법으로 주로 dplyr 함수로 변수를 만들고, ggplot을 통해 그래프로 구현하기로 하였다. 변수 검토 및 전처리는 KOSIS에서 제공하는 데이터가 변수명이 모두 한글로 되어있어서 혹시 모를 오류를 방지하고자 부득이하게 미리 엑셀 파일 상에서 임의로 영어로 변경하였고, 검토 외 전처리 과정 따로 추가하지 않는 점을 양해구하고자 한다.

##분석 내용.
```{r}
library(ggplot2) 
library(dplyr) 
library(readxl)
```

##2016년 학력별 스트레스 정도는 어떠할까?
```{r}
raw_stress <- read_excel("team_06_16.xlsx", col_names = T)
stress <- raw_stress
```

```{r}
head(raw_stress) 
tail(raw_stress)
View(raw_stress)
dim(raw_stress)
str(raw_stress)
summary(raw_stress)
```
###2016년 고졸 학력 세대별 스트레스 정도 
```{r}
stress_high <- stress %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high", "30_high", "40_high", "50_high", "60_high", "over_65_high"))
```
###2016년 대졸 학력 세대별 스트레스 정도 
```{r}
stress_univ <- stress %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_univ", "30_univ", "40_univ", "50_univ", "60_univ", "over_65_univ"))
```

##2016년 (고졸+대졸)학력 세대별 스트레스 정도는 어떠할까?
```{r}
stress_adu <- stress %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high",
                        "30_high", 
                        "40_high", 
                        "50_high", 
                        "60_high", 
                        "over_65_high",
                        "20_univ", 
                        "30_univ", 
                        "40_univ", 
                        "50_univ", 
                        "60_univ",
                        "over_65_univ"))
stress_adu$ageg <- c(20, 20, 30, 30, 40, 40, 50, 50, 60, 60, 65, 65)
stress_adu$level <- c("high", "univ", "high", "univ", "high", "univ","high", "univ","high", "univ", "high", "univ")
ggplot(data = stress_adu, aes(x = ageg, y = rate, ylim(0, 90) , fill = level )) + 
  geom_col(position = "dodge") 
```
####그래프 단순 해석: 2016년 고졸자중에서는 30대, 대졸자중에서는 40대가 가장 높은 수치의 스트레스 정도를 기록하였다. 특히 20대를 제외한 30~65세대 모두 고졸자가 대졸자보다 스트레스 정도가 높았다. 고졸자 및 대졸자의 수치가 40대에서 근사치가 되었다.


##2014년 학력별 스트레스 정도는 어떠할까?
```{r}
raw_stress_14 <- read_excel("team_06_14.xlsx", col_names = T)
stress_14 <- raw_stress_14
```

```{r}
head(raw_stress_14) 
tail(raw_stress_14)
View(raw_stress_14)
dim(raw_stress_14)
str(raw_stress_14)
summary(raw_stress_14)
```

###2014년 고졸 학력 스트레스 정도 
```{r}
stress_high_14 <- stress_14 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high", "30_high", "40_high", "50_high", "60_high", "over_65_high"))
```

###2014년 대졸 학력 스트레스 정도
```{r}
stress_univ_14 <- stress_14 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_univ", "30_univ", "40_univ", "50_univ", "60_univ", "over_65_univ"))
```

##2014년 학력별 + 연령별 스트레스 정도는 어떠할까?
```{r}
stress_adu_14 <- stress_14 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high",
                        "30_high", 
                        "40_high", 
                        "50_high", 
                        "60_high", 
                        "over_65_high",
                        "20_univ", 
                        "30_univ", 
                        "40_univ", 
                        "50_univ", 
                        "60_univ",
                        "over_65_univ"))
stress_adu_14$ageg <- c(20, 20, 30, 30, 40, 40, 50, 50, 60, 60, 65, 65)
stress_adu_14$level <- c("high", "univ", "high", "univ", "high", "univ","high", "univ","high", "univ", "high", "univ")

ggplot(data = stress_adu_14, aes(x = ageg, y = rate, fill = level)) + 
  geom_col(position = "dodge") 
```
####그래프 단순 해석: 2014년 고졸자중에서는 30대, 대졸자중에서는 40대가 가장 높은 수치의 스트레스 정도를 기록하였다. 특히 20대를 제외한 30~65세대 모두 고졸자가 대졸자보다 스트레스 정도가 높았다. 고졸자 및 대졸자의 수치가 40대에서 근사치가 되었다.

##2012년 학력별 스트레스 정도는 어떠할까?
```{r}
raw_stress_12 <- read_excel("team_06_12.xlsx", col_names = T)
stress_12 <- raw_stress_12
```

```{r}
head(raw_stress_12) 
tail(raw_stress_12)
View(raw_stress_12)
dim(raw_stress_12)
str(raw_stress_12)
summary(raw_stress_12)
```

###2012년 고졸 학력 스트레스 정도 
```{r}
stress_high_12 <- stress_12 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high", "30_high", "40_high", "50_high", "60_high", "over_65_high"))
```

###2012년 대졸 학력 스트레스 정도 
```{r}
stress_univ_12 <- stress_12 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_univ", "30_univ", "40_univ", "50_univ", "60_univ", "over_65_univ"))
```

##2012년 학력별 + 연령별 스트레스 정도는 어떠할까?
```{r}
stress_adu_12 <- stress_12 %>% 
  select(-trait_1) %>%
  filter(trait_2 %in% c("20_high",
                        "30_high", 
                        "40_high", 
                        "50_high", 
                        "60_high", 
                        "over_65_high",
                        "20_univ", 
                        "30_univ", 
                        "40_univ", 
                        "50_univ", 
                        "60_univ",
                        "over_65_univ"))
stress_adu_12$ageg <- c(20, 20, 30, 30, 40, 40, 50, 50, 60, 60, 65, 65)
stress_adu_12$level <- c("high", "univ", "high", "univ", "high", "univ","high", "univ","high", "univ", "high", "univ")

ggplot(data = stress_adu_12, aes(x = ageg, y = rate, fill = level)) + 
  geom_col(position = "dodge") 

```
####그래프 단순 해석: 2012년 고졸자와 대졸자 모두에게서 20대가 가장 높은 수치의 스트레스 정도를 기록하였다. 특히 세대를 거듭할수록 점차적으로 스트레스 정도 수치가 감소하고 있음을 알 수 있다. 고졸자 및 대졸자의 수치가 그나마 20대에서 근사치가 되었다. 20대와 60대 이상 세대간 스트레스 정도 차이가 거의 반절에 가까울 정도로 차이가 있다. 

##전체 해석: 2012, 2014, 2016년도 모두 대체로 고졸학력자의 스트레스를 느끼는 정도가 대졸학력자의 스트레스를 느끼는 정도보다 높은 것을 알 수 있다. 2012년에는 학력에 관계없이 20대의 스트레스를 느끼는 정도가 가장 높지만 2014년과 2016년에는 20대의 스트레스를 느끼는 정도가 낮은 반면 30대와 40대의 스트레스를 느끼는 정도가 증가하고 다시 50대부터 줄어드는 경향을 보인다. 전체적으로 보자면 2012년부터 2016년까지 20대의 스트레스를 느끼는 정도가 점차 감소함을 알 수 있다. 이 분석에서는 20대에서 40대까지의 추이가 주로 눈 여겨볼만하다. 다른 변수(요인)와의 상관관계를 파악할 있는 분석이 더 진행되어야 될 것 같다.


## 해석

#5. 논의
##한계점, 비판점 : 2012년도부터 2016년도까지 격년의 스트레스 정도 데이터에서 학력, 세대와 관련된 비교를 하면서, 우선 연속적이지 않은 연도의 데이터라는 점에서 가장 큰 한계가 있다고 생각하였다. 더불어 스트레스 정도를 측정한 모집단 또한 동일하지 않기에 이러한 비교가 과연 신뢰가 될만한 비교인지에 대한 비판이 있을 수 있다. 무엇보다 그래프 3개를 가지고 단순비교를 하면서 특징을 뽑아냈기 때문에 언급된 변수외의 다른 변수들과의 연계성, 연관성의 부분에서 아쉬운 분석이지 않을까 하는 생각이 들었다. 

##추후 분석 방향 : 20대의 고졸자들과 대졸자들이 느끼는 스트레스 정도가 해가 갈수록 상대적으로 낮아지고, 반면 30,40대의 고졸자들과 대졸자들의 스트레스 정도가 고도화된다. 12,14,16년도의 그래프를 비교하면서 나온 가장 두드러지는 특징으로, 20대와 30,40대가 주로 생활하는 가정, 직장, 학교에서의 스트레스 정도와 연관지어 비교하여 인과관계를 파악해보는 방향으로 분석이 이루어진다면 좋을것 같다. 