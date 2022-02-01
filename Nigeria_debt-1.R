library(tidyverse)
library(readxl)
library(gganimate)
World_debt <- read_excel("C:/Users/hp/Downloads/API_DT.DOD.DECT.CD_DS2_en_excel_v2_3469410.xls",
                                                         skip = 3)
glimpse(World_debt)

nigeria_debt<- World_debt %>%filter(`Country Name` == "Nigeria") %>%
   select(-c("Country Code":"1998"))%>%
  pivot_longer("1999":"2020") %>%
  rename(Country= `Country Name`, Year= name, Debt= value)
nigeria_debt<-nigeria_debt %>% mutate(year= as.numeric(nigeria_debt$Year)) %>%
  select(-c(Year))
nigeria_debt$Administrartion<- c("Obasanjo","Obasanjo","Obasanjo","Obasanjo",
                                 "Obasanjo","Obasanjo","Obasanjo","Obasanjo",
                                 "Yar'Adua/Jonathan","Yar'Adua/Jonathan",
                                 "Yar'Adua/Jonathan","Yar'Adua/Jonathan",
                                 "Jonathan","Jonathan","Jonathan","Jonathan",
                                 "Buhari","Buhari","Buhari","Buhari","Buhari",
                                 "Buhari")
nigeria_debt$FX_Rate<- c(98.2,100.6,113.4,126.9,137,132.3,129,127,116.8,131.3,
                         148.9,149.2,156.7,157.5,155.7,158.6,197,253.5,305.8,307,326,381)
#Debt trend
nigeria_debt %>% ggplot(aes(year,Debt, color= Administrartion))+
  geom_point()+geom_line()+theme_minimal()+scale_x_continuous(breaks = c(1999, 2003,2007,2011,2015,2020))+
  scale_y_continuous(breaks =c(20000000000,30000000000,40000000000,50000000000,60000000000,70000000000),
                     labels = c("$20B","$30B","$40B","$50B","$60B","$70B"))+
  labs(title = "Nigeria external debt trend from 1999 to 2020",
       x="Year", y="",color="Administration",
       caption = "Data Source: https://data.worldbank.org/indicator/DT.DOD.DECT.CD?
       Akinola Samson
       @Samson_Akinola1")

#Animation
anim<-nigeria_debt %>% ggplot(aes(year,Debt, color= Administrartion))+
  geom_point()+geom_line()+theme_minimal()+scale_x_continuous(breaks = c(1999, 2003,2007,2011,2015,2020))+
  scale_y_continuous(breaks =c(20000000000,30000000000,40000000000,50000000000,60000000000,70000000000),
                     labels = c("$20B","$30B","$40B","$50B","$60B","$70B"))+
  labs(title = "Nigeria external debt trend from 1999 to 2020",
       x="Year", y="",color="Administration",
       caption = "Data Source: https://data.worldbank.org/indicator/DT.DOD.DECT.CD?
       Akinola Samson
       @Samson_Akinola1")+geom_text(aes(x=min(year), y=min(Debt),
                label= as.factor(year)), hjust=-1.6, vjust= -0.2, alpha= 0.5, col= "gray",size= 17)+transition_reveal(year)
animate(anim, height= 500, width= 800, fps = 30, duration = 30,
        end_pause = 60, res= 100)
anim_save("nigeria debt.gif")

#Debt against FX_Rate
nigeria_debt %>% ggplot(aes(FX_Rate, Debt, color= Administrartion))+
  geom_point()+geom_line()+theme_minimal()+scale_x_continuous(breaks = c(100,150,200,250,300,350,400))+
  scale_y_continuous(breaks =c(20000000000,30000000000,40000000000,50000000000,60000000000,70000000000),
                                                              labels = c("$20B","$30B","$40B","$50B","$60B","$70B"))+
  labs(title = "External debt against FX Rate trend of Nigeria from 1999 to 2020",
         x="Naira to Dollar Exchange Rate", y="External debt",
         caption = "Data Sources: https://data.worldbank.org/indicator/DT.DOD.DECT.CD? | thecable.ng
         Akinola Samson
       @Samson_Akinola1")
#Debt againt FX_Rate animation
anim2<- nigeria_debt %>% ggplot(aes(FX_Rate, Debt, color= Administrartion))+
  geom_point()+geom_line()+theme_minimal()+scale_x_continuous(breaks = c(100,150,200,250,300,350,400))+
  scale_y_continuous(breaks =c(20000000000,30000000000,40000000000,50000000000,60000000000,70000000000),
                     labels = c("$20B","$30B","$40B","$50B","$60B","$70B"))+
  labs(title = "External debt against FX Rate trend of Nigeria from 1999 to 2020",
       x="Naira to Dollar Exchange Rate", y="External debt",
       caption = "Data Sources: https://data.worldbank.org/indicator/DT.DOD.DECT.CD? | thecable.ng
         Akinola Samson
       @Samson_Akinola1")+geom_text(aes(x=min(FX_Rate), y=min(Debt),
                                         label= as.factor(year)), hjust=-1.6, vjust= -0.2, alpha= 0.5, col= "gray",size= 17)+transition_reveal(year)
animate(anim2, height= 500, width= 800, fps = 30, duration = 30,
        end_pause = 60, res= 100)
anim_save("Debt to Fx_rate.gif")
