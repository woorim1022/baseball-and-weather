install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("readxl")
library(readxl)
install.packages("stringr")
library(stringr)
install.packages("readr")
library(readr)
install.packages("lubridate")
library(lubridate)
install.packages("randomForest")
library(randomForest)
#데이터 가져오기
player_d <- read_excel("player_data.xlsx")
baseball_d <- read_excel("baseball_data.xlsx")
seoul_d <- read.csv("seoul.csv", stringsAsFactors = F)
busan_d <- read.csv("busan.csv", stringsAsFactors = F)
changwon_d <- read.csv("changwon.csv", stringsAsFactors = F)
daegoo_d <- read.csv("daegoo.csv", stringsAsFactors = F)
daejeon_d <- read.csv("daejeon.csv", stringsAsFactors = F)
gwangjoo_d <- read.csv("gwangjoo.csv", stringsAsFactors = F)
incheon_d <- read.csv("incheon.csv", stringsAsFactors = F)
suwon_d <- read.csv("suwon.csv", stringsAsFactors = F)

#데이터 복사하기
seoul <- seoul_d
busan <- busan_d
changwon <- changwon_d
daegoo <- daegoo_d
daejeon <- daejeon_d
gwangjoo <- gwangjoo_d
incheon <- incheon_d
suwon <- suwon_d
baseball <- baseball_d


#필요한 변수만 추출 
seoul <- seoul %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
busan <- busan %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
changwon <- changwon %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
daegoo <- daegoo %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
daejeon <- daejeon %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
gwangjoo <- gwangjoo %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
incheon <- incheon %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)
suwon <- suwon %>% select(지점, 일시, 평균기온..C.,  일강수량.mm., 평균.상대습도...)

#변수명 바꾸기 
seoul <- seoul %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
busan <- busan %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
daegoo <- daegoo %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
daejeon <- daejeon %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
gwangjoo <- gwangjoo %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
incheon <- incheon %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
suwon <- suwon %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)
changwon <- changwon %>% rename(place = 지점, date = 일시, temp = 평균기온..C., precipi = 일강수량.mm., humi = 평균.상대습도...)


#야구데이터 시간 값 변경하기(time 변수의 데이터값 분석 가능한 형태로 수정)
baseball$time <- str_sub(baseball$time, -8, -4)
baseball <- baseball %>% select(-date)
baseball <- baseball %>% rename(date = date2)

#날씨데이터 지역 코드값 변경하기
seoul$place <- ifelse(seoul$place == "108", "서울")
busan$place <- ifelse(busan$place == "159", "부산")
daegoo$place <- ifelse(daegoo$place == "143", "대구")
daejeon$place <- ifelse(daejeon$place == "133", "대전")
changwon$place <- ifelse(changwon$place == "155", "창원")
suwon$place <- ifelse(suwon$place == "119", "수원")
gwangjoo$place <- ifelse(gwangjoo$place == "156", "광주")
incheon$place <- ifelse(incheon$place == "112", "인천")
View(seoul)

#불필요한 날짜 제거 
seoul <- seoul[-c(1:11),]
busan <- busan[-c(1:11),]
daegoo <- daegoo[-c(1:11),]
daejeon <- daejeon[-c(1:11),]
changwon <- changwon[-c(1:11),]
suwon <- suwon[-c(1:11),]
gwangjoo <- gwangjoo[-c(1:11),]
incheon <- incheon[-c(1:11),]


#baseball 데이터에 hometeam 변수 이용해서 경기장 지역 변수(place) 추가
baseball$place <- ifelse(baseball$hometeam == "LG", "서울", 
                         ifelse(baseball$hometeam == "두산", "서울",
                                ifelse(baseball$hometeam == "넥센", "서울",
                                       ifelse(baseball$hometeam == "KIA","광주", 
                                              ifelse(baseball$hometeam == "한화","대전", 
                                                     ifelse(baseball$hometeam == "NC","창원", 
                                                            ifelse(baseball$hometeam == "SK" ,"인천", 
                                                                   ifelse(baseball$hometeam == "롯데" ,"부산", 
                                                                          ifelse(baseball$hometeam == "삼성" ,"대구", 
                                                                                 ifelse(baseball$hometeam == "KT" ,"수원", NA))))))))))




#지역별 날씨데이터 통합 
weather <- bind_rows(seoul, busan, daegoo, gwangjoo, daejeon, changwon, suwon, incheon)
#통합 날씨 데이터 날짜순으로 정렬 
weather <- weather %>% group_by(date)
#야구 데이터와 날씨 데이터 병합 
baseball_weather <- left_join(baseball, weather, by = c("date", "place"))

#팀별로 데이터 분할 
dusan <- baseball_weather %>% filter(hometeam =="두산" | awayteam == "두산")
lg <- baseball_weather %>% filter(hometeam =="LG" | awayteam == "LG")
kia <- baseball_weather %>% filter(hometeam =="KIA" | awayteam == "KIA")
samsung <- baseball_weather %>% filter(hometeam =="삼성" | awayteam == "삼성")
hanhwa <- baseball_weather %>% filter(hometeam =="한화" | awayteam == "한화")
sk <- baseball_weather %>% filter(hometeam =="SK" | awayteam == "SK")
kt <- baseball_weather %>% filter(hometeam =="KT" | awayteam == "KT")
nexen <- baseball_weather %>% filter(hometeam =="넥센" | awayteam == "넥센")
nc <- baseball_weather %>% filter(hometeam =="NC" | awayteam == "NC")
lotte <- baseball_weather %>% filter(hometeam =="롯데" | awayteam == "롯데")
View(dusan)

#팀별 데이터에 score변수 추가(해당 팀의 점수만 모아놓음) 
dusan$score <- ifelse(dusan$hometeam=="두산", dusan$homescore, dusan$awayscore)
lg$score <- ifelse(lg$hometeam=="LG", lg$homescore, lg$awayscore)
samsung$score <- ifelse(samsung$hometeam=="삼성", samsung$homescore, samsung$awayscore)
kia$score <- ifelse(kia$hometeam=="KIA", kia$homescore, kia$awayscore)
hanhwa$score <- ifelse(hanhwa$hometeam=="한화", hanhwa$homescore, hanhwa$awayscore)
sk$score <- ifelse(sk$hometeam=="SK", sk$homescore, sk$awayscore)
kt$score <- ifelse(kt$hometeam=="KT", kt$homescore, kt$awayscore)
nexen$score <- ifelse(nexen$hometeam=="넥센", nexen$homescore, nexen$awayscore)
nc$score <- ifelse(nc$hometeam=="NC", nc$homescore, nc$awayscore)
lotte$score <- ifelse(lotte$hometeam=="롯데", lotte$homescore, lotte$awayscore)

summary(dusan$temp)
summary(lg$temp)
summary(samsung$temp)
summary(kia$temp)
summary(hanhwa$temp)
summary(sk$temp)
summary(kt$temp)
summary(nexen$temp)
summary(nc$temp)
summary(lotte$temp)

#기온 범위 나누기 
dusan <- dusan %>% mutate(tempg = ifelse(dusan$temp<5, "0~5",
                                         ifelse(dusan$temp<10, "05~10", 
                                                ifelse(dusan$temp<15, "10~15", 
                                                       ifelse(dusan$temp<20, "15~20", 
                                                              ifelse(dusan$temp<25, "20~25", 
                                                                     ifelse(dusan$temp<35, "25~30", "35~")))))))
lg <- lg %>% mutate(tempg = ifelse(lg$temp<5, "0~5",
                                   ifelse(lg$temp<10, "05~10", 
                                          ifelse(lg$temp<15, "10~15", 
                                                 ifelse(lg$temp<20, "15~20", 
                                                        ifelse(lg$temp<25, "20~25", 
                                                               ifelse(lg$temp<35, "25~30", "35~")))))))
samsung <- samsung %>% mutate(tempg = ifelse(samsung$temp<5, "3.9~5",
                                             ifelse(samsung$temp<10, "5~10", 
                                                    ifelse(samsung$temp<15, "10~15", 
                                                           ifelse(samsung$temp<20, "15~20", 
                                                                  ifelse(samsung$temp<25, "20~25", 
                                                                         ifelse(samsung$temp<35, "25~30", "35~")))))))
kia <- kia %>% mutate(tempg = ifelse(kia$temp<5, "0~5",
                                     ifelse(kia$temp<10, "05~10", 
                                            ifelse(kia$temp<15, "10~15", 
                                                   ifelse(kia$temp<20, "15~20", 
                                                          ifelse(kia$temp<25, "20~25", 
                                                                 ifelse(kia$temp<35, "25~30", "35~")))))))
hanhwa <- hanhwa %>% mutate(tempg = ifelse(hanhwa$temp<5, "00~5",
                                           ifelse(hanhwa$temp<10, "5~10", 
                                                  ifelse(hanhwa$temp<15, "10~15", 
                                                         ifelse(hanhwa$temp<20, "15~20", 
                                                                ifelse(hanhwa$temp<25, "20~25", 
                                                                       ifelse(hanhwa$temp<35, "25~30", "35~")))))))
sk <- sk %>% mutate(tempg = ifelse(sk$temp<5, "0~5",
                                   ifelse(sk$temp<10, "05~10", 
                                          ifelse(sk$temp<15, "10~15", 
                                                 ifelse(sk$temp<20, "15~20", 
                                                        ifelse(sk$temp<25, "20~25", 
                                                               ifelse(sk$temp<35, "25~30", "35~")))))))
kt <- kt %>% mutate(tempg = ifelse(kt$temp<5, "0~5",
                                   ifelse(kt$temp<10, "05~10", 
                                          ifelse(kt$temp<15, "10~15", 
                                                 ifelse(kt$temp<20, "15~20", 
                                                        ifelse(kt$temp<25, "20~25", 
                                                               ifelse(kt$temp<35, "25~30", "35~")))))))
nexen <- nexen %>% mutate(tempg = ifelse(nexen$temp<5, "0~5",
                                         ifelse(nexen$temp<10, "05~10", 
                                                ifelse(nexen$temp<15, "10~15", 
                                                       ifelse(nexen$temp<20, "15~20", 
                                                              ifelse(nexen$temp<25, "20~25", 
                                                                     ifelse(nexen$temp<35, "25~30", "35~")))))))
nc <- nc %>% mutate(tempg = ifelse(nc$temp<5, "0~5",
                                   ifelse(nc$temp<10, "05~10", 
                                          ifelse(nc$temp<15, "10~15", 
                                                 ifelse(nc$temp<20, "15~20", 
                                                        ifelse(nc$temp<25, "20~25", 
                                                               ifelse(nc$temp<35, "25~30", "35~")))))))
lotte <- lotte %>% mutate(tempg = ifelse(lotte$temp<5, "0~5",
                                         ifelse(lotte$temp<10, "05~10", 
                                                ifelse(lotte$temp<15, "10~15", 
                                                       ifelse(lotte$temp<20, "15~20", 
                                                              ifelse(lotte$temp<25, "20~25", 
                                                                     ifelse(lotte$temp<35, "25~30", "35~")))))))
table(is.na(dusan$score))

#팀별 데이터 결측치 제거
dusan <- dusan %>% filter(!is.na(score))
lg <- lg %>% filter(!is.na(score))
samsung <- samsung %>% filter(!is.na(score))
kia <- kia %>% filter(!is.na(score))
hanhwa <- hanhwa %>% filter(!is.na(score))
sk <- sk %>% filter(!is.na(score))
kt <- kt %>% filter(!is.na(score))
nexen <- nexen %>% filter(!is.na(score))
nc <- nc %>% filter(!is.na(score))
lotte <- lotte %>% filter(!is.na(score))


summary(dusan$score)
summary(lg$score)

#score, homescore, awayscore 변수 숫자형으로 변환 
dusan$score <- as.numeric(dusan$score)
dusan$homescore <- as.numeric(dusan$homescore)
dusan$awayscore <- as.numeric(dusan$awayscore)
lg$score <- as.numeric(lg$score)
lg$homescore <- as.numeric(lg$homescore)
lg$awayscore <- as.numeric(lg$awayscore)
samsung$score <- as.numeric(samsung$score)
samsung$homescore <- as.numeric(samsung$homescore)
samsung$awayscore <- as.numeric(samsung$awayscore)
kia$score <- as.numeric(kia$score)
kia$homescore <- as.numeric(kia$homescore)
kia$awayscore <- as.numeric(kia$awayscore)
sk$score <- as.numeric(sk$score)
sk$homescore <- as.numeric(sk$homescore)
sk$awayscore <- as.numeric(sk$awayscore)
kt$score <- as.numeric(kt$score)
kt$homescore <- as.numeric(kt$homescore)
kt$awayscore <- as.numeric(kt$awayscore)
nexen$score <- as.numeric(nexen$score)
nexen$homescore <- as.numeric(nexen$homescore)
nexen$awayscore <- as.numeric(nexen$awayscore)
nc$score <- as.numeric(nc$score)
nc$homescore <- as.numeric(nc$homescore)
nc$awayscore <- as.numeric(nc$awayscore)
lotte$score <- as.numeric(lotte$score)
lotte$homescore <- as.numeric(lotte$homescore)
lotte$awayscore <- as.numeric(lotte$awayscore)
hanhwa$score <- as.numeric(hanhwa$score)
hanhwa$homescore <- as.numeric(hanhwa$homescore)
hanhwa$awayscore <- as.numeric(hanhwa$awayscore)

class(dusan$temp)
summary(dusan$temp)
qplot(dusan$temp)
table(is.na(dusan$score))
table(is.na(dusan$temp))

#1.팀별 기온별 득점수
dusan_score_temp <- dusan %>% group_by(temp) %>% summarise(mean_score=mean(score))
lg_score_temp <- lg %>% group_by(temp) %>% summarise(mean_score=mean(score))
samsung_score_temp <- samsung %>% group_by(temp) %>% summarise(mean_score=mean(score))
kia_score_temp <- kia %>% group_by(temp) %>% summarise(mean_score=mean(score))
sk_score_temp <- sk %>% group_by(temp) %>% summarise(mean_score=mean(score))
kt_score_temp <- kt %>% group_by(temp) %>% summarise(mean_score=mean(score))
nexen_score_temp <- nexen %>% group_by(temp) %>% summarise(mean_score=mean(score))
lotte_score_temp <- lotte %>% group_by(temp) %>% summarise(mean_score=mean(score))
hanhwa_score_temp <- hanhwa %>% group_by(temp) %>% summarise(mean_score=mean(score))
nc_score_temp <- nc %>% group_by(temp) %>% summarise(mean_score=mean(score))
head(dusan_score_temp)
##################
ggplot(data=dusan_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=lg_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=samsung_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=kia_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=sk_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=kt_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=nexen_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=lotte_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=hanhwa_score_temp, aes(x=temp, y=mean_score)) + geom_line()
ggplot(data=nc_score_temp, aes(x=temp, y=mean_score)) + geom_line()
#################

class(dusan$humi)
summary(dusan$humi)
summary(lotte$humi)

qplot(dusan$humi)
table(is.na(dusan$humi))

#습도 범위 나누기
dusan <- dusan %>% mutate(humig = ifelse(humi<50, "0~50",
                                         ifelse(humi<60, "050~060", 
                                                ifelse(humi<70, "060~070", 
                                                       ifelse(humi<80, "070~080", 
                                                              ifelse(humi<90, "080~090",
                                                                     ifelse(humi<100, "090~100",
                                                                            ifelse(humi<200, "100~200", "200~350"))))))))
lg <- lg %>%  mutate(humig = ifelse(humi<50, "0~50",
                                    ifelse(humi<60, "050~060", 
                                           ifelse(humi<70, "060~070", 
                                                  ifelse(humi<80, "070~080", 
                                                         ifelse(humi<90, "080~090",
                                                                ifelse(humi<100, "090~100",
                                                                       ifelse(humi<200, "100~200", "200~350"))))))))
sk <- sk %>%  mutate(humig = ifelse(humi<50, "0~50",
                                    ifelse(humi<60, "050~060", 
                                           ifelse(humi<70, "060~070", 
                                                  ifelse(humi<80, "070~080", 
                                                         ifelse(humi<90, "080~090",
                                                                ifelse(humi<100, "090~100",
                                                                       ifelse(humi<200, "100~200", "200~350"))))))))
kt <- kt %>%  mutate(humig = ifelse(humi<50, "0~50",
                                    ifelse(humi<60, "050~060", 
                                           ifelse(humi<70, "060~070", 
                                                  ifelse(humi<80, "070~080", 
                                                         ifelse(humi<90, "080~090",
                                                                ifelse(humi<100, "090~100",
                                                                       ifelse(humi<200, "100~200", "200~350"))))))))
kia <- kia %>%  mutate(humig = ifelse(humi<50, "0~50",
                                      ifelse(humi<60, "050~060", 
                                             ifelse(humi<70, "060~070", 
                                                    ifelse(humi<80, "070~080", 
                                                           ifelse(humi<90, "080~090",
                                                                  ifelse(humi<100, "090~100",
                                                                         ifelse(humi<200, "100~200", "200~350"))))))))
samsung <- samsung %>%  mutate(humig = ifelse(humi<50, "0~50",
                                              ifelse(humi<60, "050~060", 
                                                     ifelse(humi<70, "060~070", 
                                                            ifelse(humi<80, "070~080", 
                                                                   ifelse(humi<90, "080~090",
                                                                          ifelse(humi<100, "090~100",
                                                                                 ifelse(humi<200, "100~200", "200~350"))))))))
lotte <- lotte %>%  mutate(humig = ifelse(humi<50, "0~50",
                                          ifelse(humi<60, "050~060", 
                                                 ifelse(humi<70, "060~070", 
                                                        ifelse(humi<80, "070~080", 
                                                               ifelse(humi<90, "080~090",
                                                                      ifelse(humi<100, "090~100",
                                                                             ifelse(humi<200, "100~200", "200~350"))))))))
hanhwa <- hanhwa %>%  mutate(humig = ifelse(humi<50, "0~50",
                                            ifelse(humi<60, "050~060", 
                                                   ifelse(humi<70, "060~070", 
                                                          ifelse(humi<80, "070~080", 
                                                                 ifelse(humi<90, "080~090",
                                                                        ifelse(humi<100, "090~100",
                                                                               ifelse(humi<200, "100~200", "200~350"))))))))
nc <- nc %>%  mutate(humig = ifelse(humi<50, "0~50",
                                    ifelse(humi<60, "050~060", 
                                           ifelse(humi<70, "060~070", 
                                                  ifelse(humi<80, "070~080", 
                                                         ifelse(humi<90, "080~090",
                                                                ifelse(humi<100, "090~100",
                                                                       ifelse(humi<200, "100~200", "200~350"))))))))
nexen <- nexen %>%  mutate(humig = ifelse(humi<50, "0~50",
                                          ifelse(humi<60, "050~060", 
                                                 ifelse(humi<70, "060~070", 
                                                        ifelse(humi<80, "070~080", 
                                                               ifelse(humi<90, "080~090",
                                                                      ifelse(humi<100, "090~100",
                                                                             ifelse(humi<200, "100~200", "200~350"))))))))
#2.팀별 습도별 평균득점ㅅ
dusan_score_humi <- dusan %>% group_by(humig) %>% summarise(mean_score=mean(score))
lg_score_humi <- lg %>% group_by(humig) %>% summarise(mean_score=mean(score))
sk_score_humi <- sk %>% group_by(humig) %>% summarise(mean_score=mean(score))
kt_score_humi <- kt %>% group_by(humig) %>% summarise(mean_score=mean(score))
nc_score_humi <- nc %>% group_by(humig) %>% summarise(mean_score=mean(score))
kia_score_humi <- kia %>% group_by(humig) %>% summarise(mean_score=mean(score))
samsung_score_humi <- samsung %>% group_by(humig) %>% summarise(mean_score=mean(score))
lotte_score_humi <- lotte %>% group_by(humig) %>% summarise(mean_score=mean(score))
hanhwa_score_humi <- hanhwa %>% group_by(humig) %>% summarise(mean_score=mean(score))
nexen_score_humi <- nexen %>% group_by(humig) %>% summarise(mean_score=mean(score))
###################
ggplot(data=dusan_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=lg_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=sk_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=kt_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=nc_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=kia_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=samsung_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=lotte_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=hanhwa_score_humi, aes(x=humig, y=mean_score))  + geom_col()
ggplot(data=nexen_score_humi, aes(x=humig, y=mean_score))  + geom_col()
###################




#팀별 이긴날/진날의 평균 기온, 습도
dusan$win <- ifelse(dusan$hometeam=="두산", ifelse(dusan$homescore > dusan$awayscore, "win", "lose"), ifelse(dusan$homescore < dusan$awayscore, "win","lose"))
table(dusan$win)
dusan_win_temp <- dusan %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="두산")
dusan_win_humi <- dusan %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="두산")

lg$win <- ifelse(lg$hometeam=="LG", ifelse(lg$homescore > lg$awayscore, "win", "lose"), ifelse(lg$homescore < lg$awayscore, "win","lose"))
table(lg$win)
lg_win_temp <- lg %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="LG")
lg_win_humi <- lg %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="LG")

kt$win <- ifelse(kt$hometeam=="KT", ifelse(kt$homescore > kt$awayscore, "win", "lose"), ifelse(kt$homescore < kt$awayscore, "win","lose"))
table(kt$win)
kt_win_temp <- kt %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="KT")
kt_win_humi <- kt %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="KT")

sk$win <- ifelse(sk$hometeam=="SK", ifelse(sk$homescore > sk$awayscore, "win", "lose"), ifelse(sk$homescore < sk$awayscore, "win","lose"))
table(sk$win)
sk_win_temp <- sk %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="SK")
sk_win_humi <- sk %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="SK")

kia$win <- ifelse(kia$hometeam=="KIA", ifelse(kia$homescore > kia$awayscore, "win", "lose"), ifelse(kia$homescore < kia$awayscore, "win","lose"))
table(kia$win)
kia_win_temp <- kia %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="KIA")
kia_win_humi <- kia %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="KIA")

nc$win <- ifelse(nc$hometeam=="NC", ifelse(nc$homescore > nc$awayscore, "win", "lose"), ifelse(nc$homescore < nc$awayscore, "win","lose"))
table(nc$win)
nc_win_temp <- nc %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="NC")
nc_win_humi <- nc %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="NC")

hanhwa$win <- ifelse(hanhwa$hometeam=="한화", ifelse(hanhwa$homescore > hanhwa$awayscore, "win", "lose"), ifelse(hanhwa$homescore < hanhwa$awayscore, "win","lose"))
table(hanhwa$win)
hanhwa_win_temp <- hanhwa %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="한화")
hanhwa_win_humi <- hanhwa %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="한화")

samsung$win <- ifelse(samsung$hometeam=="삼성", ifelse(samsung$homescore > samsung$awayscore, "win", "lose"), ifelse(samsung$homescore < samsung$awayscore, "win","lose"))
table(samsung$win)
samsung_win_temp <- samsung %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="삼성")
samsung_win_humi <- samsung %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="삼성")

nexen$win <- ifelse(nexen$hometeam=="넥센", ifelse(nexen$homescore > nexen$awayscore, "win", "lose"), ifelse(nexen$homescore < nexen$awayscore, "win","lose"))
table(nexen$win)
nexen_win_temp <- nexen %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="넥센")
nexen_win_humi <- nexen %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="넥센")

lotte$win <- ifelse(lotte$hometeam=="롯데", ifelse(lotte$homescore > lotte$awayscore, "win", "lose"), ifelse(lotte$homescore < lotte$awayscore, "win","lose"))
table(lotte$win)
lotte_win_temp <- lotte %>% group_by(tempg) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_temp=mean(temp)) %>% mutate(team="롯데")
lotte_win_humi <- lotte %>% group_by(humig) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose")), mean_humi=mean(humi)) %>% mutate(team="롯데")

#3.전체 팀의 기온별,습도별 이길 확률 
group_win_temp <- bind_rows(dusan_win_temp, lg_win_temp, kt_win_temp, sk_win_temp, kia_win_temp, nc_win_temp, hanhwa_win_temp, samsung_win_temp, nexen_win_temp,  lotte_win_temp)
View(group_win_temp)
group_win_humi <- bind_rows(dusan_win_humi, lg_win_humi, kt_win_humi, sk_win_humi, kia_win_humi, nc_win_humi, hanhwa_win_humi, samsung_win_humi, nexen_win_humi,  lotte_win_humi)
View(group_win_humi)
############################
ggplot(data=group_win_temp, aes(x=team, y=count_win/(count_win+count_lose), fill=tempg)) + geom_col(position = "dodge")
ggplot(data=group_win_humi, aes(x=team, y=count_win/(count_win+count_lose), fill=humig)) + geom_col(position = "dodge")
############################



#강수량 0인 값 NA로 처리
dusan$precipi <- ifelse(dusan$precipi==0.0, NA, dusan$precipi)
lg$precipi <- ifelse(lg$precipi==0.0, NA, lg$precipi)
kt$precipi <- ifelse(kt$precipi==0.0, NA, kt$precipi)
sk$precipi <- ifelse(sk$precipi==0.0, NA, sk$precipi)
nc$precipi <- ifelse(nc$precipi==0.0, NA, nc$precipi)
kia$precipi <- ifelse(kia$precipi==0.0, NA, kia$precipi)
samsung$precipi <- ifelse(samsung$precipi==0.0, NA, samsung$precipi)
nexen$precipi <- ifelse(nexen$precipi==0.0, NA, nexen$precipi)
hanhwa$precipi <- ifelse(hanhwa$precipi==0.0, NA, hanhwa$precipi)
lotte$precipi <- ifelse(lotte$precipi==0.0, NA, lotte$precipi)

#강수량 통계 확인 
summary(dusan$precipi)
summary(lg$precipi)
summary(kt$precipi)
summary(sk$precipi)
summary(nc$precipi)
summary(kia$precipi)
summary(samsung$precipi)
summary(nexen$precipi)
summary(hanhwa$precipi)
summary(lotte$precipi)

#강수량 범위 나누기 
dusan <- dusan %>% mutate(precipig = ifelse(precipi==0, "0",
                                            ifelse(precipi<=0.5, "0~0.5", 
                                                   ifelse(precipi<=1.5, "0.5~1.5", 
                                                          ifelse(precipi<=2.5, "1.5~2.5", 
                                                                 ifelse(precipi<=4.5, "2.5~4.5",
                                                                        ifelse(precipi<=6.5, "4.5~6.5",
                                                                               ifelse(precipi<=150, "6.2~", "NA"))))))))
lg <- lg %>% mutate(precipig = ifelse(precipi==0, "0",
                                      ifelse(precipi<=0.5, "0~0.5", 
                                             ifelse(precipi<=1.5, "0.5~1.5", 
                                                    ifelse(precipi<=2.5, "1.5~2.5", 
                                                           ifelse(precipi<=4.5, "2.5~4.5",
                                                                  ifelse(precipi<=6.5, "4.5~6.5",
                                                                         ifelse(precipi<=150, "6.2~", "NA"))))))))
kt <- kt %>% mutate(precipig = ifelse(precipi==0, "0",
                                      ifelse(precipi<=0.5, "0~0.5", 
                                             ifelse(precipi<=1.5, "0.5~1.5", 
                                                    ifelse(precipi<=2.5, "1.5~2.5", 
                                                           ifelse(precipi<=4.5, "2.5~4.5",
                                                                  ifelse(precipi<=6.5, "4.5~6.5",
                                                                         ifelse(precipi<=150, "6.2~", "NA"))))))))
sk <- sk %>% mutate(precipig = ifelse(precipi==0, "0",
                                      ifelse(precipi<=0.5, "0~0.5", 
                                             ifelse(precipi<=1.5, "0.5~1.5", 
                                                    ifelse(precipi<=2.5, "1.5~2.5", 
                                                           ifelse(precipi<=4.5, "2.5~4.5",
                                                                  ifelse(precipi<=6.5, "4.5~6.5",
                                                                         ifelse(precipi<=150, "6.2~", "NA"))))))))
nc <- nc %>% mutate(precipig = ifelse(precipi==0, "0",
                                      ifelse(precipi<=0.5, "0~0.5", 
                                             ifelse(precipi<=1.5, "0.5~1.5", 
                                                    ifelse(precipi<=2.5, "1.5~2.5", 
                                                           ifelse(precipi<=4.5, "2.5~4.5",
                                                                  ifelse(precipi<=6.5, "4.5~6.5",
                                                                         ifelse(precipi<=150, "6.2~", "NA"))))))))
kia <- kia %>% mutate(precipig = ifelse(precipi==0, "0",
                                        ifelse(precipi<=0.5, "0~0.5", 
                                               ifelse(precipi<=1.5, "0.5~1.5", 
                                                      ifelse(precipi<=2.5, "1.5~2.5", 
                                                             ifelse(precipi<=4.5, "2.5~4.5",
                                                                    ifelse(precipi<=6.5, "4.5~6.5",
                                                                           ifelse(precipi<=150, "6.2~", "NA"))))))))
samsung <- samsung %>% mutate(precipig = ifelse(precipi==0, "0",
                                                ifelse(precipi<=0.5, "0~0.5", 
                                                       ifelse(precipi<=1.5, "0.5~1.5", 
                                                              ifelse(precipi<=2.5, "1.5~2.5", 
                                                                     ifelse(precipi<=4.5, "2.5~4.5",
                                                                            ifelse(precipi<=6.5, "4.5~6.5",
                                                                                   ifelse(precipi<=150, "6.2~", "NA"))))))))
nexen <- nexen %>% mutate(precipig = ifelse(precipi==0, "0",
                                            ifelse(precipi<=0.5, "0~0.5", 
                                                   ifelse(precipi<=1.5, "0.5~1.5", 
                                                          ifelse(precipi<=2.5, "1.5~2.5", 
                                                                 ifelse(precipi<=4.5, "2.5~4.5",
                                                                        ifelse(precipi<=6.5, "4.5~6.5",
                                                                               ifelse(precipi<=150, "6.2~", "NA"))))))))
hanhwa <- hanhwa %>% mutate(precipig = ifelse(precipi==0, "0",
                                              ifelse(precipi<=0.5, "0~0.5", 
                                                     ifelse(precipi<=1.5, "0.5~1.5", 
                                                            ifelse(precipi<=2.5, "1.5~2.5", 
                                                                   ifelse(precipi<=4.5, "2.5~4.5",
                                                                          ifelse(precipi<=6.5, "4.5~6.5",
                                                                                 ifelse(precipi<=150, "6.2~", "NA"))))))))
lotte <- lotte %>% mutate(precipig = ifelse(precipi==0, "0",
                                            ifelse(precipi<=0.5, "0~0.5", 
                                                   ifelse(precipi<=1.5, "0.5~1.5", 
                                                          ifelse(precipi<=2.5, "1.5~2.5", 
                                                                 ifelse(precipi<=4.5, "2.5~4.5",
                                                                        ifelse(precipi<=6.5, "4.5~6.5",
                                                                               ifelse(precipi<=150, "6.2~", "NA"))))))))


dusan$rain <- ifelse(is.na(dusan$precipi), "F", "T")
lg$rain <- ifelse(is.na(lg$precipi), "F", "T")
kt$rain <- ifelse(is.na(kt$precipi), "F", "T")
sk$rain <- ifelse(is.na(sk$precipi), "F", "T")
nc$rain <- ifelse(is.na(nc$precipi), "F", "T")
kia$rain <- ifelse(is.na(kia$precipi), "F", "T")
samsung$rain <- ifelse(is.na(samsung$precipi), "F", "T")
nexen$rain <- ifelse(is.na(nexen$precipi), "F", "T")
hanhwa$rain <- ifelse(is.na(hanhwa$precipi), "F", "T")
lotte$rain <- ifelse(is.na(lotte$precipi), "F", "T")

#4.강수량별 팀별 득점수
dusan_score_precipi <- dusan %>% group_by(precipig) %>% summarise(mean_score=mean(score))
lg_score_precipi <- lg %>% group_by(precipig) %>% summarise(mean_score=mean(score))
kt_score_precipi <- kt %>% group_by(precipig) %>% summarise(mean_score=mean(score))
sk_score_precipi <- sk %>% group_by(precipig) %>% summarise(mean_score=mean(score))
nc_score_precipi <- nc %>% group_by(precipig) %>% summarise(mean_score=mean(score))
kia_score_precipi <- kia %>% group_by(precipig) %>% summarise(mean_score=mean(score))
samsung_score_precipi <- samsung %>% group_by(precipig) %>% summarise(mean_score=mean(score))
nexen_score_precipi <- nexen %>% group_by(precipig) %>% summarise(mean_score=mean(score))
hanhwa_score_precipi <- hanhwa %>% group_by(precipig) %>% summarise(mean_score=mean(score))
lotte_score_precipi <- lotte %>% group_by(precipig) %>% summarise(mean_score=mean(score))
#########################
ggplot(data=dusan_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=lg_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=kt_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=sk_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=nc_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=kia_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=samsung_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=nexen_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=hanhwa_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
ggplot(data=lotte_score_precipi, aes(x=precipig, y=mean_score)) + geom_col()
#########################

#5.비 온날/안온날 승리 확률 
dusan_win_rain <- dusan %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="두산")
lg_win_rain <- lg %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="LG")
kt_win_rain <- kt %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="KT")
sk_win_rain <- sk %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="SK")
nc_win_rain <- nc %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="NC")
kia_win_rain <- kia %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="KIA")
samsung_win_rain <- samsung %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="삼성")
nexen_win_rain <- nexen %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="넥센")
hanhwa_win_rain <- hanhwa %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="한화")
lotte_win_rain <- lotte %>% group_by(rain) %>% summarise(count_win=length(which(win=="win")), count_lose=length(which(win=="lose"))) %>% mutate(team="롯데")
group_win_precipi <- bind_rows(dusan_win_rain, lg_win_rain, kt_win_rain, sk_win_rain, nc_win_rain, kia_win_rain, samsung_win_rain, nexen_win_rain, hanhwa_win_rain,  lotte_win_rain)
#########################
ggplot(data=group_win_precipi, aes(x=team, y=count_win/(count_lose+count_win), fill=rain)) + geom_col(position = "dodge")
#########################













# 데이터 읽어오기 ####
# 1) 날씨 데이터
seoul_data <- read_csv("seoul_2.csv")
str(seoul)
suwon_data <- read_csv("suwon_2.csv")
str(suwon)
incheon_data <- read_csv("incheon_2.csv")
str(incheon)
gwangjoo_data <- read_csv("gwangjoo_2.csv")
str(gwangjoo)
daejeon_data <- read_csv("seoul_2.csv")
str(daejeon)
daegoo_data <- read_csv("daegoo_2.csv")
str(daegoo)
changwon_data <- read_csv("changwon_2.csv")
str(changwon)
busan_data <- read_csv("busan_2.csv")
str(busan)

# 2) 경기 데이터
baseball <- read_excel("baseball_data_2.xlsx")

# 3) 선수 데이터
player_data <- read_excel("player_data.xlsx")

# 데이터 전처리 ####
# 날씨 데이터
# 1) 날씨 데이터 중 필요한 변수만 select, 지점 번호를 도시이름으로 바꾸기
seoul <- seoul_data %>% select(place, date, temp, precipi,humi)
seoul$place = "seoul"
suwon <- suwon_data %>% select(place, date, temp, precipi,humi)
suwon$place = "suwon"
incheon <- incheon_data %>% select(place, date, temp, precipi,humi)
incheon$place = "incheon"
gwangjoo <- gwangjoo_data %>% select(place, date, temp, precipi,humi)
gwangjoo$place = "gwangjoo"
daejeon <- daejeon_data %>% select(place, date, temp, precipi,humi)
daejeon$place = "daejeon"
daegoo <- daegoo_data %>% select(place, date, temp, precipi,humi)
daegoo$place = "daegoo"
changwon <- changwon_data %>% select(place, date, temp, precipi,humi)
changwon$place = "changwon"
busan <- busan_data %>% select(place, date, temp, precipi,humi)
busan$place = "busan"

# 날씨 데이터 하나로 합치기
weather <- bind_rows(seoul, suwon, incheon, gwangjoo, daejeon, daegoo, changwon, busan)

# 강수량 결측치 처리
table(is.na(weather$precipi)) # 결측치 755개
hist(weather$precipi,
     main = "Frequency of Precipitation",
     xlab = "precipitation") # => 대부분 0에 분포
weather$precipi[is.na(weather$precipi)] <- 0 # 결측치 0으로 변환
table(is.na(weather$precipi)) # 결측치 0개

# 경기 데이터
# 경기 취소된 일 수
table(is.na(baseball$audience)) # 23개
# 경기 취소된 일의 데이터 삭제
baseball <- baseball %>%
  filter(!is.na(audience)) 

# 넥센이 최근 키움으로 이름을 바꿨기 때문에 이름 변경
baseball$awayteam[baseball$awayteam == "넥센"] = "키움"
baseball$hometeam[baseball$hometeam == "넥센"] = "키움"

# 경기 데이터에 파생변수 만들기
# 경기장이 있는 도시 변수 넣기(place)
baseball <- baseball %>%
  mutate(place = ifelse(hometeam == "두산" | hometeam == "LG" | hometeam == "키움", "seoul",
                        ifelse(hometeam == "KT", "suwon",
                               ifelse(hometeam == "KIA", "gwangjoo",
                                      ifelse(hometeam == "SK", "incheon",
                                             ifelse(hometeam == "한화", "daejeon",
                                                    ifelse(hometeam == "삼성", "daegoo",
                                                           ifelse(hometeam == "롯데", "busan", "changwon"))))))))

# 좌석 점유율 분석을 위한 경기장 최대 수용 인원 변수(total) 넣기
baseball <- baseball %>%
  mutate(total = ifelse(hometeam == "두산" | hometeam == "LG", 26000,
                        ifelse(hometeam == "키움", 22000,
                               ifelse(hometeam == "KT", 25000,
                                      ifelse(hometeam == "KIA", 21000,
                                             ifelse(hometeam == "SK", 26000,
                                                    ifelse(hometeam == "한화", 13000,
                                                           ifelse(hometeam == "삼성", 29000,
                                                                  ifelse(hometeam == "롯데", 27000, 11000)))))))))

# 데이터 분석 ####
# 요일별 좌석 점유율
baseball <- baseball %>%
  mutate(share = audience/total)# 점유율 변수 share

baseball$day[baseball$day == "화"] = "Tue"
baseball$day[baseball$day == "수"] = "Wed"
baseball$day[baseball$day == "목"] = "Thu"
baseball$day[baseball$day == "금"] = "Fri"
baseball$day[baseball$day == "토"] = "Sat"
baseball$day[baseball$day == "일"] = "Sun"
baseball <- transform(baseball, 
                      day = factor(day, levels = c("Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

ggplot(data=baseball,aes(x=day, y=share))+geom_boxplot()

# 월별 좌석 점유율

baseball <- baseball %>% 
  mutate(month = month(date))

ggplot(data=baseball,aes(x=month, y=share, group=month))+geom_boxplot()

# 날씨와 관중 수
# 날씨 데이터와 경기데이터 합치기(date와 place 기준으로)
data <- merge(weather, baseball, by = c("date", "place"))

# 강수량에 따른 좌석 점유율
ggplot(data=data, aes(x=precipi, y=share))+geom_point()

# 온도에 따른 좌석 점유율
# 예시: KIA
ts <- data %>% filter(hometeam == "KIA"|awayteam == "KIA")
ggplot(data=ts, aes(x=temp, y=audience))+geom_point()+stat_smooth()+ggtitle("KIA")

# 습도에 따른 좌석 점유율
ggplot(data=ts, aes(x=humi, y=share))+geom_point()+coord_cartesian(xlim = c(30, 80))+stat_smooth()+ggtitle("KIA")


# 선수의 경기력
player <- player_data %>% 
  filter(!is.na(homerun))
str(player)
player <- player %>%
  mutate(place = ifelse(hometeam == 1 | LG == 1 | 키움 == 1, "seoul",
                        ifelse(hometeam == 0 & KT == 1, "suwon",
                               ifelse(hometeam == 0 & KIA == 1, "gwangjoo",
                                      ifelse(hometeam == 0 & SK == 1, "incheon",
                                             ifelse(hometeam == 0 & 한화 == 1, "daejeon",
                                                    ifelse(hometeam == 0 &  삼성 == 1, "daegoo",
                                                           ifelse(hometeam == 0 & 롯데 == 1, "busan", "changwon"))))))))

data_p <- merge(weather, player, by = c("date", "place"))

data_p <- data_p %>%
  mutate(jamsil = ifelse(hometeam == 1 | LG == 1, 1, 0),
         suwon = ifelse(hometeam == 0 & KT == 1, 1, 0),
         gwangjoo = ifelse(hometeam == 0 & KIA == 1, 1, 0),
         incheon = ifelse(hometeam == 0 & SK == 1, 1, 0),
         daejeon = ifelse(hometeam == 0 & 한화 == 1, 1, 0),
         daegoo = ifelse(hometeam == 0 &  삼성 == 1, 1, 0),
         busan = ifelse(hometeam == 0 & 롯데 == 1, 1, 0),
         changwon = ifelse(hometeam == 0 & NC == 1, 1, 0),
         gochuck = ifelse(hometeam == 0 & 키움 == 1, 1, 0))

# 날씨와 선수 개인 경기력
rf.fit = randomForest(homerun ~ temp + humi + precipi
                      , data=data_p, mtry = floor(sqrt(7)), ntree = 500, importance = T)
rf.fit
importance(rf.fit)
plot(importance(rf.fit))

