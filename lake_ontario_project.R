library(tidyverse)
library(broom)
library(sf)

## read csv 
data3 = read_csv("LAKE_ONTARIO_Water_Quality_2000-present.csv")

## data clearing and wrangling
temp = data3 %>%
  filter(FULL_NAME ==  "TEMPERATURE (OF WATER)" ) %>%
  group_by(STN_DATE) %>%
  summarize(temp = mean(VALUE))

ph = data3 %>%
  filter(FULL_NAME == "PH" ) %>%
  group_by(STN_DATE) %>%
  summarize(ph = mean(VALUE))

clear = data3 %>%
  filter(FULL_NAME == "TRANSPARENCY" ) %>%
  group_by(STN_DATE) %>%
  summarize(transparency = mean(VALUE))

nh3  = data3 %>%
  filter(FULL_NAME == "AMMONIA NITROGEN,SOLUBLE") %>%
  group_by(STN_DATE) %>%
  summarize(nh3 = mean(VALUE))

other_nitrogen =  data3 %>% 
  filter(FULL_NAME == "NITRATE+NITRITE NITROGEN,FILTERED") %>%
  group_by(STN_DATE) %>%
  summarize(other_n = mean(VALUE))

phosphorous  = data3 %>%
  filter(FULL_NAME == "PHOSPHOROUS,TOTAL" ) %>%
  group_by(STN_DATE) %>%
  summarize(phosphorous = mean(VALUE))

oxygen  =   data3 %>%
  filter(FULL_NAME == "OXYGEN,CONCENTRATION DISSOLVED") %>%
  group_by(STN_DATE) %>%
  summarize(oxygen = mean(VALUE))

data3 = data3 %>%
  filter(VALUE != -99.0000)

geo_data = data3 %>%
  select(STN_DATE,LATITUDE_DD,LONGITUDE_DD) %>%
  group_by(STN_DATE,LATITUDE_DD,LONGITUDE_DD) %>%
  summarize()

d1  = left_join(nh3,other_nitrogen,by="STN_DATE") 

d1 = d1 %>%
  mutate(total_nitrogen = nh3 + other_n) %>%
  select(STN_DATE,total_nitrogen)

d1 = left_join(d1,ph,by="STN_DATE") 

d2 = left_join(d1,temp,by="STN_DATE")

d3 = left_join(d2,oxygen,by="STN_DATE")

d4 = left_join(d3,phosphorous,by="STN_DATE")

d5 = left_join(d4,clear,by="STN_DATE")

d5 = left_join(d5,geo_data,by="STN_DATE")

d6 =  d5 %>%
  mutate(year = format(d5$STN_DATE,format="%Y"),
         month =format(d5$STN_DATE,format="%m")) %>%
  filter(year < 2018) 




## Summary statistics 
summary_stat = function(data) {
  c((length(data) - sum(is.na(data))),sd(data,na.rm = TRUE),
    min(data,na.rm = TRUE), as.vector(quantile(data,prob=c(.25),na.rm = TRUE)), mean(data,na.rm = TRUE), as.vector(quantile(data,prob=c(.75),na.rm = TRUE)),
    max(data,na.rm = TRUE))
}

data_summary  = matrix(rep(1:7,5),ncol=7,byrow=TRUE)

data_summary[1,] = summary_stat(d6$transparency)

data_summary[2,] = summary_stat(d6$temp)

data_summary[3,] = summary_stat(d6$oxygen)

data_summary[4,] = summary_stat(d6$total_nitrogen)

data_summary[5,] = summary_stat(d6$phosphorous)

colnames(data_summary) <- c( "N","Std Dev", "Min","1st Qu","Mean","3rd Qu","Max")

rownames(data_summary) <- c("transparency","temperature","oxygen","Nitrogen",
                            "phosphorous")

names(dimnames(data_summary)) <- c("Key Variable", "Summary Statistics")

data_summary

### removing outlier 
d6 = d6%>%
  filter(total_nitrogen != 682.3222)
mean(d6$total_nitrogen)

### Plot linear regressions
d6 %>%
  filter ( year < 2013,transparency > 45) %>%
  ggplot(mappin=aes(x=transparency,y=phosphorous,group=year,color=year)) +
  geom_point() +
  geom_smooth(method = lm,se=FALSE)  +
  labs(title = "Water transparency in Lake Ontario vs phosphorus in water",
       subtitle = "The majority of data was collected in April",
       x="transparency (%)",
       y="phosphorus (mg P/L)") +
  theme(plot.title = element_text(hjust=0.5), 
        plot.subtitle = element_text(hjust=0.5) ) + 
  facet_wrap(~ year,nrow=3) 

### Applying Linear regression to predict watertransparency from selected variables
regression = lm(transparency~total_nitrogen+phosphorous+temp+oxygen,data=d6)

### Table 
reg_tidy = tidy(regression,conf.int=TRUE) 
reg_tidy

### Plot 
d6 = na.omit(d6) %>%
  mutate(predicion = fitted(regression),
         observed_class = if_else(transparency >= 75,"More than 75",""),
         observed_class = if_else((transparency < 75 & transparency   
                                   >=50),"Between 50 and 74",observed_class),
         observed_class = if_else(transparency <50,"Less than 50",observed_class),
  ) 

d6$observed_class = factor(d6$observed_class, levels = c("More than 75", "Between 50 and 74", "Less than 50"))

ggplot(data=d6,mapping=aes(x=predicion,y=transparency,group=observed_class,color=observed_class)) +
  geom_point() + 
  geom_abline(intercept = 1) +
  labs(title="Predicted water transparency vs observed data", x = "predicted (%)",
       y ="observed(%)") +
  theme(plot.title = element_text(hjust=0.5))
  
  
  
  
## Additional visualization/analysis 
### read geo data
localarea_boundary <- st_read("Aquatic_resource_area_polygon_segment_.shp")
discription_boundary = read_csv("Aquatic_resource_area_polygon_segment_.csv")

### plot Map
d8 = d6 %>%
  filter(LONGITUDE_DD < -79.2 & LATITUDE_DD > 43.2 | 
           ((LONGITUDE_DD < -78.2 & LONGITUDE_DD > -79.2) & 
              (LATITUDE_DD < 44   &  LATITUDE_DD > 43.5)))

d8 = na.omit(d8) %>% 
  mutate(
    phosphorous_level =  if_else(phosphorous < 0.010,"low",""),
    phosphorous_level =  if_else((phosphorous >= 0.010 
                                  & phosphorous <= 0.035),"medium",phosphorous_level),
    phosphorous_level =  if_else(phosphorous > 0.035,"high",phosphorous_level))
d8$phosphorous_level =  factor(d8$phosphorous_level, levels = c("high", "medium", "low"))

zzz = localarea_boundary[128433,]

ggplot() + geom_sf(data=zzz) +
  geom_point(data=d8,mapping=aes(y=LATITUDE_DD,x=LONGITUDE_DD,group=phosphorous_level,color=phosphorous_level)) 
  + scale_color_brewer(palette="Set1") +
  labs(title = "Phosphorus levels of lake water each observed location", subtitle="Observations are from Canadian side of Lake Ontario",x="Longtitude",y="Latitude")



### Plot bar plot
d9 = d8 %>%
  mutate(
    year_group =  if_else(year <= 2007,"2001~2007",""),
    year_group =  if_else(year >= 2007,"2007~2012",year_group))

d90 = d9 %>%
  group_by(phosphorous_level,year_group) %>%
  summarise(N = n()) %>%
  mutate(freq = if_else(year_group == "2001~2007",N/(8+128+26),0),
         freq = if_else(year_group == "2007~2012",N/(4+95+25),freq),
         percent = round((freq*100), 2))

d90 %>%
  ggplot(mapping=aes(x=year_group,y=percent,group=phosphorous_level,fill=phosphorous_level)) +
  geom_col(position = "dodge2") + 
  labs(title = "Comparison of phosphorous levels of lake water in 2 different groups of years", subtitle="Observation are from Canadian side of Lake Ontario",x="Year",y="Percent")




