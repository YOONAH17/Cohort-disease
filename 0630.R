library("readr")
library("dplyr")
library("ggplot2")
data_dir <- "C:/Users/USER/Desktop/G-2/leukemia/data/"
leukemia_raw <- read_csv(paste0(data_dir, "ref_leukemia_bz_v4.csv"))

dim(leukemia_raw)

##결측치 확인 
sum(is.na(leukemia_raw$UP1))
sum(is.na(leukemia_raw$UP2))
sum(is.na(leukemia_raw$ECNY_AGE)) 
sum(is.na(leukemia_raw$FY_BZ))    

##UP2를 numeric 타입으로 변경 
leukemia_raw$UP2 <- as.numeric(leukemia_raw$UP2)  

##UP2가 NA인 관측치 0으로 대체 (일단)
leukemia_raw$UP2[which(is.na(leukemia_raw$UP2)>0)] <- 0
sum(is.na(leukemia_raw$UP2))


##### LEUKEMIA & FY 열 생성 (in 처리군) ----------------------------------------
##### YEAR 순으로 정렬한 후,
##### 1. LEUKEMIA_BZ의 차이로 LEUKEMIA 열 생성 
##### 2. FY_BZ의 차이로 FY 열 생성


##중분류 별로, cohort 데이터에 대해, YEAR 순으로 정렬 
leukemia_perUP2 <- leukemia_raw %>%
  group_by(UP2, SEX, CAL, ECNY_AGE, YEAR) %>%
  summarise(ENR_BZ = sum(ENR_BZ), FY_BZ = sum(FY_BZ), LEUKEMIA_BZ = sum(LEUKEMIA_BZ))#ENR_BZ 안 넣어도 되는데 비교하려고 한 번 넣어봄,,


##BZ의 차이로 새로운 열 생성
leukemia_perUP2[,"LEUKEMIA"] <- diff(c(0,leukemia_perUP2$LEUKEMIA_BZ))
leukemia_perUP2[,"FY"] <- diff(c(0,leukemia_perUP2$FY_BZ))
# leukemia_perUP2[500:550,] %>% print(n = 50)


##시작연도의 BZ 값들은 그대로 사용. (초기 정보를 잃지 않도록 한다.)
leukemia_perUP2[which(diff(c(2018,leukemia_perUP2$YEAR))<=0),"LEUKEMIA"] <- 
  leukemia_perUP2[which(diff(c(2018,leukemia_perUP2$YEAR))<=0),"LEUKEMIA_BZ"] #0으로 대체할 수도
leukemia_perUP2[which(diff(c(2018,leukemia_perUP2$YEAR))<=0),"FY"] <- 
  leukemia_perUP2[which(diff(c(2018,leukemia_perUP2$YEAR))<=0),"FY_BZ"] #0으로 대체할 수도
# leukemia_perUP2[500:550,] %>% print(n = 50)


##연도별로 재정렬
leukemia_perUP2 <- leukemia_perUP2 %>% arrange(YEAR, UP2, SEX, CAL, ECNY_AGE) %>%
  select(YEAR, UP2, SEX, CAL, ECNY_AGE, LEUKEMIA, FY)
# leukemia_perUP2[500:550,] %>% print(n = 50)





##### FY_p 열 생성 (in 처리군) -------------------------------------------------
##### FY_p: 대조군 보정 시 기준이 되는, (UP2 마다의) cohort 별 근로자 수 비율 

leukemia_perUP2_sum <- leukemia_perUP2 %>% 
  group_by(YEAR, UP2) %>% 
  summarise(FY_UP2_sum = sum(FY))

leukemia_perUP2 <- left_join(leukemia_perUP2, leukemia_perUP2_sum, by=c("YEAR","UP2"))
rm(leukemia_perUP2_sum)

leukemia_perUP2 <- leukemia_perUP2 %>%
  mutate(FY_p = FY/FY_UP2_sum) %>%
  select(-FY_UP2_sum)

# ##같은 중분류 내 FY_p의 합이 1인지 확인
# for (j in 2000:2018) {
#   for (i in 0:99) {
#     print(paste("YEAR", j, "UP2",  i, "_ sum of FY_p:",
#                 sum(leukemia_perUP2[leukemia_perUP2$YEAR==j & leukemia_perUP2$UP2==i,"FY_p"]))
#           )
#   }
# }

# ##특정 연도, 특정 중분류 내 관측치가 없는 경우도 있음.
# leukemia_perUP2[leukemia_perUP2$YEAR==2018 & leukemia_perUP2$UP2==91,]






##### 대조군 데이터 생성  ------------------------------------------------------
##### 처리군 데이터를 모든 중분류에 대해서 합침으로써 얻을 수 있다.
##### 대조군에서는 발병률 정보가 필요 >> INC(발병률) == LEUKEMIA / FY 열 생성 

leukemia_total <- leukemia_perUP2 %>%
  group_by(YEAR, SEX, CAL, ECNY_AGE) %>%
  summarise(LEUKEMIA = sum(LEUKEMIA), FY = sum(FY))
# leukemia_total%>% print(n = 50) 

##original 발병률 계산 
leukemia_total$INC <- leukemia_total$LEUKEMIA / leukemia_total$FY

 


##### FY_correct & LUEKEMIA_correct 열 생성-----------------------------------
##### :보정 근로자 수 & 보정 발병자 수
#####>>각 처리군을 기준으로 보정한 대조군 78개를 얻어서 각 처리군 옆에 붙인다.

##### 1.FY_correct = FY_sum * FY_p 
#####  FY_sum:  대조군의 전체 근로자 수 (연도마다)
#####  FY_p  :  각 처리군 마다 cohort 별 근로자 수 비율
#####>> 대조군의 전체 근로자 수는 유지되면서, cohort별 비율이 처리군의 비율과 동일하게 보정된다.
 
##### 2.LUEKEMIA_correct = 1.FY_correct * INC 
#####>> 보정된 근로자 수에 대하여, 기존 발병률을 곱하여 기대 발병자 수로 보정한다. 


##1-1. FY_sum 구하기 
leukemia_total_sum <- leukemia_total %>% 
  group_by(YEAR) %>% 
  summarise(FY_sum = sum(FY))

leukemia_perUP2 <- left_join(leukemia_perUP2, leukemia_total_sum, by=c("YEAR"))

##1-2. FY_correct 생성 
leukemia_perUP2 <- leukemia_perUP2 %>% 
  mutate(FY_correct =  FY_sum * FY_p) %>%
  select(-"FY_p", -"FY_sum")


##같은 중분류 내 FY_correct의 합이 FY_sum인지 확인
leukemia_total_sum[1:6,]

for (j in 2000:2005) {
  for (i in 0:99) {
    print(paste("YEAR", j, "UP2",  i, "_ sum of FY_correct:",
                sum(leukemia_perUP2[leukemia_perUP2$YEAR==j & leukemia_perUP2$UP2==i,"FY_correct"]))
          )
  }
}
rm(leukemia_total_sum)


##2-1. LUEKEMIA_correct 생성 
leukemia_perUP2 <- left_join(leukemia_perUP2, leukemia_total[,c(1,2,3,4,7)], by=c("YEAR", "SEX", "CAL", "ECNY_AGE"))


leukemia_perUP2 <- leukemia_perUP2 %>% 
  mutate(LUEKEMIA_correct = FY_correct * INC)  %>% 
  select("YEAR","UP2","SEX","CAL","ECNY_AGE","LEUKEMIA","FY","LUEKEMIA_correct","FY_correct")












##### 비교하기------------------------------------------------------------------
##### 1.cor_over_ori: 보정 대조군/원래 대조군 발병률
##### 2.treat_over_cor: 처리군/보정 대조군 발병률

##대조군 origianal 발병률 계산 (연도별로 하나의 대조 발병률이 나옴.)
control_origin <- leukemia_total %>% 
  group_by(YEAR) %>%
  summarise(LEUKEMIA_ori_sum = sum(LEUKEMIA), #대조군의 총 발병자 수
            FY_ori_sum = sum(FY)) %>%         #대조군의 총 근로자 수 
  mutate(control_ori_p = LEUKEMIA_ori_sum / FY_ori_sum) #원래 대조군 발병률
control_origin


#YEAR별 각 UP2로 보정한 대조군의 발병률 계산 
control_correct <- leukemia_perUP2 %>%
  group_by(YEAR, UP2) %>%
  summarise(LEUKEMIA_sum = sum(LEUKEMIA),               #각 처리군의 총 발병자 수
            FY_sum = sum(FY),                           #각 처리군의 총 근로자 수 
            LEUKEMIA_cor_sum = sum(LUEKEMIA_correct),   #보정한 대조군의 총 발병자 수 
            FY_cor_sum = sum(FY_correct)                #보정한 대조군의 총 근로자 수 
            ) %>%
  mutate(treat_p = LEUKEMIA_sum / FY_sum,               #각 처리군의 발병률 
         control_cor_p = LEUKEMIA_cor_sum / FY_cor_sum) #보정한 대조군 발병률 

control_correct #같은 연도에서 중분류에 상관없이 FY_cor_sum은 모두 같고, LEUKEMIA_cor_sum중분류마다 다르게 보정됨.


compare <- left_join(control_correct[,c(1,2,7,8)], control_origin[,c(1,4)], by="YEAR")
compare <- compare %>% mutate(cor_over_ori = control_cor_p/control_ori_p, treat_over_cor = treat_p/control_cor_p)


hist(compare$cor_over_ori)
summary(compare$cor_over_ori)



##### 결과 분석을 위한 전처리  -------------------------------------------------

compare # treat_over_cor == NA 인 행은 해당 연도 해당 중분류의 모든 cohort에 대해서 발병자가 없는 경우임.
sum(is.na(compare$control_cor_p))  #대조군은 모든 중분류를 합쳐놓은 것. 해당 연도의 발병자가 아예 0인 경우는 없음. 
sum(is.na(compare$treat_over_cor)) #처리군은 그 중분류만 보는 것. 해당 중분류의 발병자가 아예 0인 경우가 있음.
compare$treat_over_cor[is.na(compare$treat_over_cor)] <- 0

###1. 원래 vs 보정
arrange(compare, cor_over_ori) %>% print(n=20)      #기존보다 작게 보정된 top10 : (해당 연도) 해당 중분류의 발병자가 아예 없어서 0으로 보정된 행 7개가 존재
arrange(compare, desc(cor_over_ori)) %>% print(n=20) #기존보다 크게 보정된 top10 : 기존에 비해 2배 이상 크게 보정됨


###2. 처리 vs 보정
arrange(compare, desc(treat_over_cor)) #대조군에 비해 15배만큼 큰 발병률을 보이는 97번 (가구내 고용활동) -> 0번 (NA) -> 39번 (환경 정화 및 복원업) -> 2번 (임업) ...

compare <- compare[!compare$UP2 %in% c(0,97,98),]  #UP1=='T' (UP2: 97,98-가구 내 고용활동~) 과 UP1==NA (UP2: NA) 삭제  
table(compare$UP2)
compare$UP2 <- as.factor(compare$UP2)
str(compare)




########
### 모든 연도에 대하여
mean_compare <- compare %>% group_by(UP2) %>% summarise(cor_over_ori=mean(cor_over_ori), treat_over_cor=mean(treat_over_cor))
##1. 원래 vs 보정
arrange(mean_compare, cor_over_ori) 
arrange(mean_compare, desc(cor_over_ori))
##2. 처리 vs 보정
arrange(mean_compare, desc(treat_over_cor))



##### 그래프 그리기 ------------------------------------------------------------
user_theme <- theme_bw() + theme(plot.title = element_text(size = 30),
                                 text = element_text(size = 25), 
                                 axis.text = element_text(size = 12),
                                 axis.title = element_text(size = 20), 
                                 strip.text = element_text(size = 12),
                                 legend.position = "top"
)

a <- arrange(compare, desc(treat_over_cor))

ggplot(a[1:10,], 
       aes(x = YEAR, y = treat_over_cor*1000)) + 
  geom_line(aes(color = UP2)) + 
  labs(y = "treat_over_cor")



