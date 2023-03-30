install.packages("jsonlite")
library("jsonlite")
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library("dplyr")

result = fromJSON("E:/user/Downloads/C-B0027-001 .json")
         
Year = rep(seq(2010,2022),each = 12,len = 149)
date = seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month")
taipei = read.csv("E:/user/Downloads/taipei.csv",header=T,encoding = "UTF-8",sep = ",")
taipei = mutate(taipei,Year,date)
taichung = read.csv("E:/user/Downloads/taichung.csv",header=T,encoding = "UTF-8",sep = ",")
taichung = mutate(taichung,Year,date)
kaohsiung = read.csv("E:/user/Downloads/kaohsiung.csv",header=T,encoding = "UTF-8",sep = ",")
kaohsiung = mutate(kaohsiung,Year,date)
hengchun = read.csv("E:/user/Downloads/hengchun.csv",header=T,encoding = "UTF-8",sep = ",")
hengchun = mutate(hengchun,Year,date)
hualien = read.csv("E:/user/Downloads/hualien.csv",header=T,encoding = "UTF-8",sep = ",")
hualien = mutate(hualien,Year,date)
all_df = rbind(taipei,taichung,kaohsiung,hengchun,hualien)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
all_df = arrange(mutate(all, station=factor(station,levels=neworder)),station)
all_df = subset(all_df, select = c(-測站最高氣壓時間.LST., -測站最低氣壓時間.LST.,-A型蒸發量.mm.,-最高氣溫時間.LST.,-最低氣溫時間.LST.,-最大陣風風速時間.LST.,-最大十分鐘降水量起始時間.LST.,-最大六十分鐘降水量起始時間.LST.,-最大日降水量時間.LST.,-全天空日射量.MJ...,-平均日最高紫外線指數,-平均日最高紫外線指數,-月最高紫外線指數,-月最高紫外線指數時間.LST.,-風向.360degree.,-最大陣風風向.360degree.))
all_df$降水量.mm. = as.numeric(all_df$降水量.mm.)
all_df$最大十分鐘降水量.mm. = as.numeric(all$最大十分鐘降水量.mm.)
all_df$最大六十分鐘降水量.mm. = as.numeric(all$最大六十分鐘降水量.mm.)
str(all_df)

taipei_df = cbind(result$location$stationObsStatistics$stationPressure$monthly[[2]],
                 result$location$stationObsStatistics$temperature$monthly[[2]],
                 result$location$stationObsStatistics$relativeHumidity$monthly[[2]],
                 result$location$stationObsStatistics$cloudCover$monthly[[2]],
                 result$location$stationObsStatistics$sunshineDuration$monthly[[2]],
                 result$location$stationObsStatistics$precipitation$monthly[[2]],
                 result$location$stationObsStatistics$windSpeed$monthly[[2]],
                 result$location$station$stationNameEN[[2]])
taipei_df = taipei_df[,-c(3,10,12,14,16,19)]
names(taipei_df)[15] = "station"
taichung_df = cbind(result$location$stationObsStatistics$stationPressure$monthly[[25]],
                   result$location$stationObsStatistics$temperature$monthly[[25]],
                   result$location$stationObsStatistics$relativeHumidity$monthly[[25]],
                   result$location$stationObsStatistics$cloudCover$monthly[[25]],
                   result$location$stationObsStatistics$sunshineDuration$monthly[[25]],
                   result$location$stationObsStatistics$precipitation$monthly[[25]],
                   result$location$stationObsStatistics$windSpeed$monthly[[25]],
                   result$location$station$stationNameEN[[25]])
taichung_df = taichung_df[,-c(3,10,12,14,16,19)]
names(taichung_df)[15] = "station"
kaohsiung_df = cbind(result$location$stationObsStatistics$stationPressure$monthly[[9]],
                 result$location$stationObsStatistics$temperature$monthly[[9]],
                 result$location$stationObsStatistics$relativeHumidity$monthly[[9]],
                 result$location$stationObsStatistics$cloudCover$monthly[[9]],
                 result$location$stationObsStatistics$sunshineDuration$monthly[[9]],
                 result$location$stationObsStatistics$precipitation$monthly[[9]],
                 result$location$stationObsStatistics$windSpeed$monthly[[9]],
                 result$location$station$stationNameEN[[9]])
kaohsiung_df = kaohsiung_df[,-c(3,10,12,14,16,19)]
names(kaohsiung_df)[15] = "station"
hengchun_df = cbind(result$location$stationObsStatistics$stationPressure$monthly[[26]],
                 result$location$stationObsStatistics$temperature$monthly[[26]],
                 result$location$stationObsStatistics$relativeHumidity$monthly[[26]],
                 result$location$stationObsStatistics$cloudCover$monthly[[26]],
                 result$location$stationObsStatistics$sunshineDuration$monthly[[26]],
                 result$location$stationObsStatistics$precipitation$monthly[[26]],
                 result$location$stationObsStatistics$windSpeed$monthly[[26]],
                 result$location$station$stationNameEN[[26]])
hengchun_df = hengchun_df[,-c(3,10,12,14,16,19)]
names(hengchun_df)[15] = "station"
hualien_df = cbind(result$location$stationObsStatistics$stationPressure$monthly[[7]],
                   result$location$stationObsStatistics$temperature$monthly[[7]],
                   result$location$stationObsStatistics$relativeHumidity$monthly[[7]],
                   result$location$stationObsStatistics$cloudCover$monthly[[7]],
                   result$location$stationObsStatistics$sunshineDuration$monthly[[7]],
                   result$location$stationObsStatistics$precipitation$monthly[[7]],
                   result$location$stationObsStatistics$windSpeed$monthly[[7]],
                   result$location$station$stationNameEN[[7]])
hualien_df = hualien_df[,-c(3,10,12,14,16,19)]
names(hualien_df)[15] = "station"
allmean_df = rbind(taipei_df, taichung_df, kaohsiung_df, hengchun_df, hualien_df)
names(allmean_df) = c("月份","平均測站氣壓","平均氣溫","最高氣溫","最低氣溫",
                      "最高氣溫>=30℃日數","最高氣溫>=25℃日數","最低氣溫<=10℃日數",
                      "平均相對濕度","平均雲量","總日照時數","降水量","降水量>=0.1mm日數",
                      "平均風速","測站")
allmean_df$測站 = tolower(allmean_df$測站)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
allmean_df = arrange(mutate(allmean_df, 測站 = factor(測站,levels = neworder)),測站)
allmean_df$月份 = as.integer(allmean_df$月份)
allmean_df$平均測站氣壓 = as.numeric(allmean_df$平均測站氣壓)
allmean_df$平均氣溫 = as.numeric(allmean_df$平均氣溫)
allmean_df$最高氣溫 = as.numeric(allmean_df$最高氣溫)
allmean_df$最低氣溫 = as.numeric(allmean_df$最低氣溫)
allmean_df$`最高氣溫>=30℃日數`= as.numeric(allmean_df$`最高氣溫>=30℃日數`)
allmean_df$`最高氣溫>=25℃日數`= as.numeric(allmean_df$`最高氣溫>=25℃日數`)
allmean_df$`最低氣溫<=10℃日數` = as.numeric(allmean_df$`最低氣溫<=10℃日數`)
allmean_df$平均相對濕度 = as.numeric(allmean_df$平均相對濕度)
allmean_df$平均雲量 = as.numeric(allmean_df$平均雲量)
allmean_df$總日照時數 = as.numeric(allmean_df$總日照時數)
allmean_df$降水量 = as.numeric(allmean_df$降水量)
allmean_df$`降水量>=0.1mm日數` = as.numeric(allmean_df$`降水量>=0.1mm日數`)
allmean_df$平均風速 = as.numeric(allmean_df$平均風速)
str(allmean_df)


#2010~2022年各月平均溫度變化圖
names(allmean_df)[15] = "station"
ggplot()+
  geom_line(data = all_df,
            aes(x = 觀測時間.month. , 
                y = 氣溫..., 
                group = Year,
                color = Year))+
  geom_line(data = allmean_df,
            aes(x = 月份, 
                y = 平均氣溫, 
            ),color="red",size=1.3) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022年各月平均溫度變化圖',x = "月份",y = "溫度", caption = "Source: cwb opendata")


#2010~2022年各月最高溫度變化圖
ggplot(all_df,
       aes(x = 觀測時間.month. , 
           y = 最高氣溫..., 
           group = Year,
           color = Year)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  scale_y_continuous(breaks=c(20,25,30,35,38,40))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022年各月最高溫度變化圖',x = "月份",y = "溫度", caption = "Source: cwb opendata") +
  geom_line()+
  geom_hline(aes(yintercept=35), color="red", linetype="dashed")+
  geom_hline(aes(yintercept=38), color="purple", linetype="dashed")+
  geom_hline(aes(yintercept=30), color="red", linetype="dashed")


#2010~2022年各月最低溫度變化圖
ggplot(all_df,
       aes(x = 觀測時間.month. , 
           y = 最低氣溫..., 
           group = Year,
           color = Year)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  scale_y_continuous(breaks=c(5,10,14,15,20,25))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022年各月最低溫度變化圖',x = "月份",y = "溫度", caption = "Source: cwb opendata") +
  geom_line()+
  geom_hline(aes(yintercept=20), color="red", linetype="dashed")+
  geom_hline(aes(yintercept=14), color="blue", linetype="dashed")+
  geom_hline(aes(yintercept=25), color="red", linetype="dashed")


#氣溫&氣壓之關係圖
ggplot(all_df,
       aes(x = 氣溫..., 
           y = 測站氣壓.hPa.,
           color = station)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  labs(title="氣溫&氣壓之關係圖", x="氣溫(°C)", y="氣壓(hPa)", caption = "Source: cwb opendata")


#2010~2022年各月平均風速變化圖
ggplot(all_df,
       aes(x = 觀測時間.month., 
           y = 風速.m.s.,
           color = station)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  geom_point() + 
  geom_smooth(se=F) +
  theme(panel.grid.minor=element_blank())+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  labs(title="2010~2022年各月平均風速變化圖", x="月份", y="風速(m/s)", caption = "Source: cwb opendata")


#降水時數&日照時數之關係圖
ggplot(all_df,
       aes(x = 日照時數.hour., 
           y = 降水時數.hour.,
           color = station)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  labs(title="降水時數&日照時數之關係圖", x="降水時數(hour)", y="日照時數(hour)", caption = "Source: cwb opendata")


all_df %>%
  filter(降水量.mm. >= 1000) %>%降水量.mm. = 1000
  
  
#2010~2022年雨量變化圖
all_df$降水量.mm.[c(379,402,467,479,551)] = 1000
ggplot(all_df,
       aes(x = date,
           y = 降水量.mm.,
           group = Year,
           fill = 觀測時間.month.,
           color = station,)) + 
  facet_grid(station~.)+
  scale_y_continuous(limits = c(0, 1000),breaks=c(0,200,500,750,1000))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022年雨量變化圖',x = "時間",y = "降水量(mm)",fill = "月份",color = "測站", caption = "Source: cwb opendata")+
  geom_bar(stat='identity')+
  geom_hline(aes(yintercept=200), color="red", linetype="dashed")


#2010~2022降水日數變化圖
ggplot(all_df,
       aes(x = date,
           y = 降水日數.day.,
           fill = 觀測時間.month.,
           color = station,)) + 
  facet_grid(station~.)+
  scale_y_continuous(breaks=c(0, 10, 20))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022降水日數變化圖',x = "時間",y = "降水日數(day)",fill = "月份",color = "測站", caption = "Source: cwb opendata")+
  geom_bar(stat='identity')


#長期降水>0.1mm各月平均天數
ggplot(allmean_df,
       aes(x = 月份,
           y = `降水量>=0.1mm日數`,
           fill = 測站,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(測站~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '長期降水>0.1mm各月平均天數',x = "月份",y = "降水>0.1mm天數", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `降水量>=0.1mm日數`), vjust = 1.15, size = 3) 


#最高氣溫>=30°C各月平均天數
ggplot(allmean_df,
       aes(x = 月份,
           y = `最高氣溫>=30℃日數`,
           fill = 測站,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(測站~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '最高氣溫>=30°C各月平均天數',x = "月份",y = "天數", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `最高氣溫>=30℃日數`), vjust =0, size = 3) 


#最高氣溫>=25°C各月平均天數
ggplot(allmean_df,
       aes(x = 月份,
           y = `最高氣溫>=25℃日數`,
           fill = 測站,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(測站~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '最高氣溫>=25°C各月平均天數',x = "月份",y = "天數", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `最高氣溫>=25℃日數`), vjust =0, size = 3) 


#最低氣溫<=10°C各月平均天數
ggplot(allmean_df,
       aes(x = 月份,
           y = `最低氣溫<=10℃日數`,
           fill = 測站,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(測站~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '最低氣溫<=10°C各月平均天數',x = "月份",y = "天數", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `最低氣溫<=10℃日數`), vjust =0, size = 3) 


#2010-2022平均溫度距平圖
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
temp_difference_taipei = all_df[all_df$station == "taipei",] $氣溫...- allmean_df[allmean_df$測站 == "taipei",]$平均氣溫
temp_difference_taichung = all_df[all_df$station == "taichung",] $氣溫...- allmean_df[allmean_df$測站 == "taichung",]$平均氣溫
temp_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $氣溫...- allmean_df[allmean_df$測站 == "kaohsiung",]$平均氣溫
temp_difference_hengchun = all_df[all_df$station == "hengchun",] $氣溫...- allmean_df[allmean_df$測站 == "hengchun",]$平均氣溫
temp_difference_hualien = all_df[all_df$station == "hualien",] $氣溫...- allmean_df[allmean_df$測站 == "hualien",]$平均氣溫
temp_difference = c(temp_difference_taipei,temp_difference_taichung,temp_difference_kaohsiung,temp_difference_hengchun,temp_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
temp_difference_df = as.data.frame(cbind(temp_difference,station))
names(temp_difference_df) = c("溫度距平","測站")
temp_difference_df$溫度距平 = as.numeric(temp_difference_df$溫度距平)
temp_difference_df = cbind(temp_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
temp_difference_df = arrange(mutate(temp_difference_df, 測站 = factor(測站,levels = neworder)),測站)
str(temp_difference_df)

ggplot(temp_difference_df,
       aes(
         x = date,
         y = 溫度距平,
         fill = 溫度距平 > 0,
         ))+ 
  facet_grid(測站~.)+
  scale_y_continuous(breaks=c(-2, 0, 0.5,2))+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022平均溫度距平圖',x = "時間",y = "溫度距平(°C)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)+
  geom_hline(aes(yintercept=0.5), color="red", linetype="dashed")
  



#2010-2022平均雨量距平圖
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
rain_difference_taipei = all_df[all_df$station == "taipei",] $降水量.mm.- allmean_df[allmean_df$測站 == "taipei",]$降水量
rain_difference_taichung = all_df[all_df$station == "taichung",] $降水量.mm.- allmean_df[allmean_df$測站 == "taichung",]$降水量
rain_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $降水量.mm.- allmean_df[allmean_df$測站 == "kaohsiung",]$降水量
rain_difference_hengchun = all_df[all_df$station == "hengchun",] $降水量.mm.- allmean_df[allmean_df$測站 == "hengchun",]$降水量
rain_difference_hualien = all_df[all_df$station == "hualien",] $降水量.mm.- allmean_df[allmean_df$測站 == "hualien",]$降水量
rain_difference = c(rain_difference_taipei,rain_difference_taichung,rain_difference_kaohsiung,rain_difference_hengchun,rain_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
rain_difference_df = as.data.frame(cbind(rain_difference,station))
names(rain_difference_df) = c("雨量距平","測站")
rain_difference_df$雨量距平 = as.numeric(rain_difference_df$雨量距平)
rain_difference_df = cbind(rain_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
rain_difference_df = arrange(mutate(rain_difference_df, 測站 = factor(測站,levels = neworder)),測站)
str(rain_difference_df)

ggplot(rain_difference_df,
       aes(x = date,
           y = 雨量距平,
           fill = 雨量距平 > 0,
       )) + 
  facet_grid(測站~.)+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022平均雨量距平圖',x = "時間",y = "雨量距平(mm)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)

#2010-2022總日照時數距平圖
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
sun_difference_taipei = all_df[all_df$station == "taipei",] $日照時數.hour.- allmean_df[allmean_df$測站 == "taipei",]$總日照時數
sun_difference_taichung = all_df[all_df$station == "taichung",] $日照時數.hour.- allmean_df[allmean_df$測站 == "taichung",]$總日照時數
sun_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $日照時數.hour.- allmean_df[allmean_df$測站 == "kaohsiung",]$總日照時數
sun_difference_hengchun = all_df[all_df$station == "hengchun",] $日照時數.hour.- allmean_df[allmean_df$測站 == "hengchun",]$總日照時數
sun_difference_hualien = all_df[all_df$station == "hualien",] $日照時數.hour.- allmean_df[allmean_df$測站 == "hualien",]$總日照時數
sun_difference = c(sun_difference_taipei,sun_difference_taichung,sun_difference_kaohsiung,sun_difference_hengchun,sun_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
sun_difference_df = as.data.frame(cbind(sun_difference,station))
names(sun_difference_df) = c("總日照時數距平","測站")
sun_difference_df$總日照時數距平 = as.numeric(sun_difference_df$總日照時數距平)
sun_difference_df = cbind(sun_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
sun_difference_df = arrange(mutate(sun_difference_df, 測站 = factor(測站,levels = neworder)),測站)
str(sun_difference_df)

ggplot(sun_difference_df,
       aes(x = date,
           y = 總日照時數距平,
           fill = 總日照時數距平 > 0,
       )) + 
  facet_grid(測站~.)+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X軸標題
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y軸標題
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#標題
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022總日照時數距平圖',x = "時間",y = "總日照時數(hr)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)
