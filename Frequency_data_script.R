getwd()
install.packages("readxl")
library("readxl")
library("ggplot2")

setwd("D:/USGS Internship Projects/R and Jupyter Files")

Frequency_o_pm<- read_excel("Frequency_data_O_PM_Flip.xlsx")

#ggplot(data=Frequency_o_pm, aes(x=`Time (Ma)`))+geom_histogram()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Only Start PM`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y= `Half Duration PM` ))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Only Start SOD`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Only Start SOS`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Entire Duration PM`))+geom_line()       
ggplot(Frequency_o_pm, aes(x=Bin,y=`Entire duration SOS`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Entire Duration SOD`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Half Duration SOS`))+geom_line()
ggplot(Frequency_o_pm, aes(x=Bin,y=`Half Duration SOD`))+geom_line()
 
ggplot(Frequency_o_pm, aes(x=Bin)) + geom_line(aes(y=`Only Start PM`),color="red")+ geom_line(aes(y=`Only Start SOS`),color="blue")+ geom_line(aes(y=`Only Start SOD`),color="deepskyblue")

ggplot(Frequency_o_pm, aes(x=Bin)) + geom_line(aes(y=`Entire Duration PM`),color="red")+ geom_line(aes(y=`Entire duration SOS`),color="blue")+ geom_line(aes(y=`Entire Duration SOD`),color="deepskyblue")
labels <- c("1500","1000","500","0")
ggplot(Frequency_o_pm, aes(x=Bin)) + geom_line(aes(y=`Half Duration PM`),color="red",lwd=1)+ geom_line(aes(y=`Half Duration SOS`),color="blue",lwd=1)+ geom_line(aes(y=`Half Duration SOD`),color="deepskyblue",lwd=1)+
  theme_light()+labs(title= "Frequency of Orogens and Passive Margins", x="Time (Ma)", y="Frequency")+theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5))+guides()
  
ggsave("Frequency Half.png",plot = last_plot(),dpi=300)

Frequency_Phan <- subset(Frequency_o_pm, Bin >= 14)
       
#Phanerozoic Frequency
ggplot(Frequency_Phan, aes(x=`Time (Ma)`)) + geom_line(aes(y=`Half Duration PM`),color="red",lwd=1)+ geom_line(aes(y=`Half Duration SOS`),color="blue",lwd=1)+ geom_line(aes(y=`Half Duration SOD`),color="deepskyblue",lwd=1)+
  theme_light()+labs(title= "Phanerozoic Frequency of Orogens and Passive Margins", x="Time (Ma)", y="Frequency")+theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5))+scale_x_reverse(breaks= c(600,400,200),labels=c("600"="500","400"="300","200"="100"))
ggsave("Frequency Half Phan.png",plot = last_plot(),dpi=300)

#Phanerozoic Frequency
ggplot(Frequency_Phan, aes(x=`Time (Ma)`)) + geom_line(aes(y=`Entire Duration PM`),color="red",lwd=1)+ geom_line(aes(y=`Entire duration SOS`),color="blue",lwd=1)+ geom_line(aes(y=`Entire Duration SOD`),color="deepskyblue",lwd=1)+
  theme_light()+labs(title= "Phanerozoic Frequency of Orogens and Passive Margins", x="Time (Ma)", y="Frequency")+theme(axis.title.x = element_text(vjust = -1.5, hjust = 0.5))+scale_x_reverse()
ggsave("Frequency Half Phan.png",plot = last_plot(),dpi=300)
