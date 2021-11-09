library(readxl)
read_excel("Global import of cocoa beans.xlsx")->data
library(tidyverse)
library(ggtext)
library(reshape2)

data%>%
  spread(Region,`Import in tonnes`)%>%
  rowwise()%>%
  mutate(Total=sum(Americas,`Asia & Oceania`,Europe))%>%
  mutate(AmericaP=round((Americas/Total)*100))%>%
  mutate(AsiaP=round((`Asia & Oceania`/Total)*100))%>%
  mutate(EuropeP=round((Europe/Total)*100))%>%
  select(Year,AmericaP,AsiaP,EuropeP)->data1

data1
gather(data1,region,value,2:4)->data2

hsize=2.5
data2%>%
  mutate(hsize=2.5)->data2


ggplot(data2, aes(x=hsize,y=value, fill=region,label=paste0(value,"%")))+
  geom_col(colour="white")+
  geom_text(position=position_stack(vjust=0.5),size=3, fontface="bold")+
  coord_polar(theta = "y")+
  xlim(0.2,hsize+0.5)+
  facet_wrap(~Year,ncol = 3)+
  scale_fill_manual(values=c("#00A7FF","#39B600","#F8766D"),labels=c("Americas","Asia and Oceania","Europe"))+
  theme(axis.title = element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="black"),
        legend.background = element_rect(fill="black"),
        legend.key = element_rect(fill="black"),
        legend.text = element_text(colour="white",size=11),
        strip.background = element_rect(fill="black"),
        strip.text = element_text(colour="white",face="bold",size=12),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.margin = unit(c(0.5,1,0.5,1),"cm"),
        plot.title = element_markdown(colour="white",size=15,face="bold",margin=margin(b=12)),
        plot.subtitle = element_markdown(colour="white",size=11,margin=margin(b=24)),
        plot.caption = element_text(colour="white",size=8,hjust=0,margin=margin(t=12))
        )+
  labs(title="<span style= 'color:#F8766D'>EUROPE <span style= 'color:white'>ACCOUNTS FOR THE LARGEST SHARE OF COCOA BEAN IMPORTS</span>",
       subtitle = "**<span style= 'color:#F8766D'>Europe** <span style= 'color:white'>accounts for over 60 per cent of the global coco bean import every year. This is because, **<span style= 'color:#F8766D'>Europe** <span style= 'color:white'>is the
                           <br>world's largest chocolate manufacturer and exporter market.The **<span style= 'color:#F8766D'>European** <span style= 'color:white'>cocoa market is very diverse, as
<br>**<span style= 'color:#F8766D'>European** <span style= 'color:white'>buyers source cocoa beans from different origins and of different qualities to cater for the broad
<br>demand of a diverse cocoa and chocolate industry.<br><br><br>
Among other regions, while **<span style= 'color:#39B600'>Asia and Oceania** <span style= 'color:white'>together account for around 23 per cent, <span style= 'color:#00A7FF'>**Americas** <span style= 'color:white'>account for<br>
nearly 15 per cent of the global cocoa bean imports</span>",
caption= "Date: Centre for the Promotion of Imports| Design: @annapurani93")->plot


ggsave("coffee.png",plot,width=9,height=7.2) 
ggsave("coffee.pdf",plot,width=9,height=7.2) 
