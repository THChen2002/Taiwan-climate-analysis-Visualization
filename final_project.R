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
all_df = subset(all_df, select = c(-�����̰������ɶ�.LST., -�����̧C�����ɶ�.LST.,-A���]�o�q.mm.,-�̰���Ůɶ�.LST.,-�̧C��Ůɶ�.LST.,-�̤j�}�����t�ɶ�.LST.,-�̤j�Q���������q�_�l�ɶ�.LST.,-�̤j���Q���������q�_�l�ɶ�.LST.,-�̤j�魰���q�ɶ�.LST.,-���ѪŤ�g�q.MJ...,-������̰����~�u����,-������̰����~�u����,-��̰����~�u����,-��̰����~�u���Ʈɶ�.LST.,-���V.360degree.,-�̤j�}�����V.360degree.))
all_df$�����q.mm. = as.numeric(all_df$�����q.mm.)
all_df$�̤j�Q���������q.mm. = as.numeric(all$�̤j�Q���������q.mm.)
all_df$�̤j���Q���������q.mm. = as.numeric(all$�̤j���Q���������q.mm.)
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
names(allmean_df) = c("���","������������","�������","�̰����","�̧C���",
                      "�̰����>=30�J���","�̰����>=25�J���","�̧C���<=10�J���",
                      "�����۹����","�������q","�`��Ӯɼ�","�����q","�����q>=0.1mm���",
                      "�������t","����")
allmean_df$���� = tolower(allmean_df$����)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
allmean_df = arrange(mutate(allmean_df, ���� = factor(����,levels = neworder)),����)
allmean_df$��� = as.integer(allmean_df$���)
allmean_df$������������ = as.numeric(allmean_df$������������)
allmean_df$������� = as.numeric(allmean_df$�������)
allmean_df$�̰���� = as.numeric(allmean_df$�̰����)
allmean_df$�̧C��� = as.numeric(allmean_df$�̧C���)
allmean_df$`�̰����>=30�J���`= as.numeric(allmean_df$`�̰����>=30�J���`)
allmean_df$`�̰����>=25�J���`= as.numeric(allmean_df$`�̰����>=25�J���`)
allmean_df$`�̧C���<=10�J���` = as.numeric(allmean_df$`�̧C���<=10�J���`)
allmean_df$�����۹���� = as.numeric(allmean_df$�����۹����)
allmean_df$�������q = as.numeric(allmean_df$�������q)
allmean_df$�`��Ӯɼ� = as.numeric(allmean_df$�`��Ӯɼ�)
allmean_df$�����q = as.numeric(allmean_df$�����q)
allmean_df$`�����q>=0.1mm���` = as.numeric(allmean_df$`�����q>=0.1mm���`)
allmean_df$�������t = as.numeric(allmean_df$�������t)
str(allmean_df)


#2010~2022�~�U�륭���ū��ܤƹ�
names(allmean_df)[15] = "station"
ggplot()+
  geom_line(data = all_df,
            aes(x = �[���ɶ�.month. , 
                y = ���..., 
                group = Year,
                color = Year))+
  geom_line(data = allmean_df,
            aes(x = ���, 
                y = �������, 
            ),color="red",size=1.3) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022�~�U�륭���ū��ܤƹ�',x = "���",y = "�ū�", caption = "Source: cwb opendata")


#2010~2022�~�U��̰��ū��ܤƹ�
ggplot(all_df,
       aes(x = �[���ɶ�.month. , 
           y = �̰����..., 
           group = Year,
           color = Year)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  scale_y_continuous(breaks=c(20,25,30,35,38,40))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022�~�U��̰��ū��ܤƹ�',x = "���",y = "�ū�", caption = "Source: cwb opendata") +
  geom_line()+
  geom_hline(aes(yintercept=35), color="red", linetype="dashed")+
  geom_hline(aes(yintercept=38), color="purple", linetype="dashed")+
  geom_hline(aes(yintercept=30), color="red", linetype="dashed")


#2010~2022�~�U��̧C�ū��ܤƹ�
ggplot(all_df,
       aes(x = �[���ɶ�.month. , 
           y = �̧C���..., 
           group = Year,
           color = Year)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  scale_y_continuous(breaks=c(5,10,14,15,20,25))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022�~�U��̧C�ū��ܤƹ�',x = "���",y = "�ū�", caption = "Source: cwb opendata") +
  geom_line()+
  geom_hline(aes(yintercept=20), color="red", linetype="dashed")+
  geom_hline(aes(yintercept=14), color="blue", linetype="dashed")+
  geom_hline(aes(yintercept=25), color="red", linetype="dashed")


#���&���������Y��
ggplot(all_df,
       aes(x = ���..., 
           y = ��������.hPa.,
           color = station)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  labs(title="���&���������Y��", x="���(�XC)", y="����(hPa)", caption = "Source: cwb opendata")


#2010~2022�~�U�륭�����t�ܤƹ�
ggplot(all_df,
       aes(x = �[���ɶ�.month., 
           y = ���t.m.s.,
           color = station)) + 
  facet_grid(.~station)+
  scale_x_continuous(breaks=seq(1:12))+
  geom_point() + 
  geom_smooth(se=F) +
  theme(panel.grid.minor=element_blank())+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  labs(title="2010~2022�~�U�륭�����t�ܤƹ�", x="���", y="���t(m/s)", caption = "Source: cwb opendata")


#�����ɼ�&��ӮɼƤ����Y��
ggplot(all_df,
       aes(x = ��Ӯɼ�.hour., 
           y = �����ɼ�.hour.,
           color = station)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  labs(title="�����ɼ�&��ӮɼƤ����Y��", x="�����ɼ�(hour)", y="��Ӯɼ�(hour)", caption = "Source: cwb opendata")


all_df %>%
  filter(�����q.mm. >= 1000) %>%�����q.mm. = 1000
  
  
#2010~2022�~�B�q�ܤƹ�
all_df$�����q.mm.[c(379,402,467,479,551)] = 1000
ggplot(all_df,
       aes(x = date,
           y = �����q.mm.,
           group = Year,
           fill = �[���ɶ�.month.,
           color = station,)) + 
  facet_grid(station~.)+
  scale_y_continuous(limits = c(0, 1000),breaks=c(0,200,500,750,1000))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022�~�B�q�ܤƹ�',x = "�ɶ�",y = "�����q(mm)",fill = "���",color = "����", caption = "Source: cwb opendata")+
  geom_bar(stat='identity')+
  geom_hline(aes(yintercept=200), color="red", linetype="dashed")


#2010~2022��������ܤƹ�
ggplot(all_df,
       aes(x = date,
           y = �������.day.,
           fill = �[���ɶ�.month.,
           color = station,)) + 
  facet_grid(station~.)+
  scale_y_continuous(breaks=c(0, 10, 20))+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010~2022��������ܤƹ�',x = "�ɶ�",y = "�������(day)",fill = "���",color = "����", caption = "Source: cwb opendata")+
  geom_bar(stat='identity')


#��������>0.1mm�U�륭���Ѽ�
ggplot(allmean_df,
       aes(x = ���,
           y = `�����q>=0.1mm���`,
           fill = ����,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(����~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '��������>0.1mm�U�륭���Ѽ�',x = "���",y = "����>0.1mm�Ѽ�", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `�����q>=0.1mm���`), vjust = 1.15, size = 3) 


#�̰����>=30�XC�U�륭���Ѽ�
ggplot(allmean_df,
       aes(x = ���,
           y = `�̰����>=30�J���`,
           fill = ����,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(����~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '�̰����>=30�XC�U�륭���Ѽ�',x = "���",y = "�Ѽ�", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `�̰����>=30�J���`), vjust =0, size = 3) 


#�̰����>=25�XC�U�륭���Ѽ�
ggplot(allmean_df,
       aes(x = ���,
           y = `�̰����>=25�J���`,
           fill = ����,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(����~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '�̰����>=25�XC�U�륭���Ѽ�',x = "���",y = "�Ѽ�", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `�̰����>=25�J���`), vjust =0, size = 3) 


#�̧C���<=10�XC�U�륭���Ѽ�
ggplot(allmean_df,
       aes(x = ���,
           y = `�̧C���<=10�J���`,
           fill = ����,)) + 
  scale_x_continuous(breaks=seq(1:12))+
  facet_grid(����~.)+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '�̧C���<=10�XC�U�륭���Ѽ�',x = "���",y = "�Ѽ�", caption = "Source: cwb opendata")+
  geom_histogram(stat='identity')+
  geom_text(aes(label = `�̧C���<=10�J���`), vjust =0, size = 3) 


#2010-2022�����ū׶Z����
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
temp_difference_taipei = all_df[all_df$station == "taipei",] $���...- allmean_df[allmean_df$���� == "taipei",]$�������
temp_difference_taichung = all_df[all_df$station == "taichung",] $���...- allmean_df[allmean_df$���� == "taichung",]$�������
temp_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $���...- allmean_df[allmean_df$���� == "kaohsiung",]$�������
temp_difference_hengchun = all_df[all_df$station == "hengchun",] $���...- allmean_df[allmean_df$���� == "hengchun",]$�������
temp_difference_hualien = all_df[all_df$station == "hualien",] $���...- allmean_df[allmean_df$���� == "hualien",]$�������
temp_difference = c(temp_difference_taipei,temp_difference_taichung,temp_difference_kaohsiung,temp_difference_hengchun,temp_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
temp_difference_df = as.data.frame(cbind(temp_difference,station))
names(temp_difference_df) = c("�ū׶Z��","����")
temp_difference_df$�ū׶Z�� = as.numeric(temp_difference_df$�ū׶Z��)
temp_difference_df = cbind(temp_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
temp_difference_df = arrange(mutate(temp_difference_df, ���� = factor(����,levels = neworder)),����)
str(temp_difference_df)

ggplot(temp_difference_df,
       aes(
         x = date,
         y = �ū׶Z��,
         fill = �ū׶Z�� > 0,
         ))+ 
  facet_grid(����~.)+
  scale_y_continuous(breaks=c(-2, 0, 0.5,2))+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022�����ū׶Z����',x = "�ɶ�",y = "�ū׶Z��(�XC)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)+
  geom_hline(aes(yintercept=0.5), color="red", linetype="dashed")
  



#2010-2022�����B�q�Z����
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
rain_difference_taipei = all_df[all_df$station == "taipei",] $�����q.mm.- allmean_df[allmean_df$���� == "taipei",]$�����q
rain_difference_taichung = all_df[all_df$station == "taichung",] $�����q.mm.- allmean_df[allmean_df$���� == "taichung",]$�����q
rain_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $�����q.mm.- allmean_df[allmean_df$���� == "kaohsiung",]$�����q
rain_difference_hengchun = all_df[all_df$station == "hengchun",] $�����q.mm.- allmean_df[allmean_df$���� == "hengchun",]$�����q
rain_difference_hualien = all_df[all_df$station == "hualien",] $�����q.mm.- allmean_df[allmean_df$���� == "hualien",]$�����q
rain_difference = c(rain_difference_taipei,rain_difference_taichung,rain_difference_kaohsiung,rain_difference_hengchun,rain_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
rain_difference_df = as.data.frame(cbind(rain_difference,station))
names(rain_difference_df) = c("�B�q�Z��","����")
rain_difference_df$�B�q�Z�� = as.numeric(rain_difference_df$�B�q�Z��)
rain_difference_df = cbind(rain_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
rain_difference_df = arrange(mutate(rain_difference_df, ���� = factor(����,levels = neworder)),����)
str(rain_difference_df)

ggplot(rain_difference_df,
       aes(x = date,
           y = �B�q�Z��,
           fill = �B�q�Z�� > 0,
       )) + 
  facet_grid(����~.)+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022�����B�q�Z����',x = "�ɶ�",y = "�B�q�Z��(mm)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)

#2010-2022�`��ӮɼƶZ����
date = rep(seq(as.Date('2010/01/01'), as.Date('2022/05/01'),by = "month"),5)
sun_difference_taipei = all_df[all_df$station == "taipei",] $��Ӯɼ�.hour.- allmean_df[allmean_df$���� == "taipei",]$�`��Ӯɼ�
sun_difference_taichung = all_df[all_df$station == "taichung",] $��Ӯɼ�.hour.- allmean_df[allmean_df$���� == "taichung",]$�`��Ӯɼ�
sun_difference_kaohsiung = all_df[all_df$station == "kaohsiung",] $��Ӯɼ�.hour.- allmean_df[allmean_df$���� == "kaohsiung",]$�`��Ӯɼ�
sun_difference_hengchun = all_df[all_df$station == "hengchun",] $��Ӯɼ�.hour.- allmean_df[allmean_df$���� == "hengchun",]$�`��Ӯɼ�
sun_difference_hualien = all_df[all_df$station == "hualien",] $��Ӯɼ�.hour.- allmean_df[allmean_df$���� == "hualien",]$�`��Ӯɼ�
sun_difference = c(sun_difference_taipei,sun_difference_taichung,sun_difference_kaohsiung,sun_difference_hengchun,sun_difference_hualien)
station = rep(c("taipei","taichung","kaohsiung","hengchun","hualien"),each = 149)
sun_difference_df = as.data.frame(cbind(sun_difference,station))
names(sun_difference_df) = c("�`��ӮɼƶZ��","����")
sun_difference_df$�`��ӮɼƶZ�� = as.numeric(sun_difference_df$�`��ӮɼƶZ��)
sun_difference_df = cbind(sun_difference_df,date)
neworder = c("taipei","taichung","kaohsiung","hengchun","hualien")
sun_difference_df = arrange(mutate(sun_difference_df, ���� = factor(����,levels = neworder)),����)
str(sun_difference_df)

ggplot(sun_difference_df,
       aes(x = date,
           y = �`��ӮɼƶZ��,
           fill = �`��ӮɼƶZ�� > 0,
       )) + 
  facet_grid(����~.)+
  geom_smooth(aes(group = 1), method="lm", se = F,show.legend = F,size = 1.2, color = "black")+
  theme(axis.title.x = element_text(hjust = 0.5,face="bold",size=15))+#X�b���D
  theme(axis.title.y = element_text(hjust = 0.5,face="bold",size=15))+#Y�b���D
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=15))+#���D
  theme(panel.grid.minor=element_blank())+
  labs(title = '2010-2022�`��ӮɼƶZ����',x = "�ɶ�",y = "�`��Ӯɼ�(hr)",fill = "", caption = "Source: cwb opendata")+
  geom_bar(stat='identity',show.legend = F)