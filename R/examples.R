library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gganimate)

## Reading in data
pums <- read_csv('Data/psam_p41.csv')
pums


## The pipe operator
sum(1:10)
1:10 %>% sum()

exp(sqrt(sum(1:10)))
1:10 %>% sum() %>% sqrt() %>% exp()



## Select
pumsReduced <- pums %>% 
  select(PUMA, SALARY=WAGP, WEIGHT=PWGTP, AGE=AGEP, RACE=RAC1P, HICOV, NATIVITY)

pumsFinal <- pumsReduced %>% select(-NATIVITY)
str(pumsFinal)



## Filter

pumsFinal <- pumsFinal %>% filter(SALARY > 0)
str(pumsFinal)

pumsFinal %>% filter(RACE %in% 1:3) %>% select(AGE) %>% summary()


## Mutate
pumsFinal <- pumsFinal %>% mutate(WSALARY=WEIGHT*SALARY) %>%
  mutate(RACE=case_when(RACE==1 ~ "White",
                        RACE==2 ~ "Black",
                        RACE==3 ~ "American Indian",
                        RACE==4 ~ "Alaska Native",
                        RACE %in% c(5,8) ~ "Other",
                        RACE==6 ~ "Asian",
                        RACE==7 ~ "Pacific Islander",
                        RACE==9 ~ "Two or more races"))
str(pumsFinal)


## Grouping and summarizing

ests <- pumsFinal %>% group_by(PUMA) %>% summarize(UW=mean(SALARY), HT=sum(WSALARY)/sum(WEIGHT))
ests



## Joins
band_members
band_instruments


  ## Left join
band_members %>% left_join(band_instruments, by="name")


  ## Right join
band_members %>% right_join(band_instruments, by="name")


  ## Inner join

band_members %>% inner_join(band_instruments, by="name")


  ## Full join

band_members %>% full_join(band_instruments, by="name")



## Long vs. Wide Data

set.seed(1)
mice <- data.frame(MouseID=1:6, Treatment=c(rep("Sugar",3),rep("No Sugar",3)), Before=rnorm(6,3, sd=0.5), After=rnorm(6,c(5,5,5,3,3,3), sd=0.5))
mice # wide


mice %>% 
  pivot_longer(3:4, names_to="Measurement", values_to="Time") # long

  
  

comps <- data.frame(Company=c("Google", "Amazon"), Q1=c(15.6, 9.8), Q2=c(19.9,12.2), Q3=c(23.1,8.7), Q4=c(24.9,12.0))
comps <- comps %>% pivot_longer(-1, names_to="Quarter", values_to="EPS")

comps # long 

comps %>% pivot_wider(names_from=Quarter, values_from=EPS) # wide

  
##################
##### GGPLOT #####
##################





mpg


## Creating your first plot!

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point()


ggplot(mpg, aes(x=displ, y=hwy, size=cty)) +
  geom_point() +
  geom_smooth(method="lm", se=F)


ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color=class)) +
  geom_smooth(method="loess", se=T)

  
  

## Fixing aesthetics  

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(aes(color="blue")) # wrong

ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(color="blue") # right




## geom_bar()
ggplot(pumsFinal, aes(x=RACE)) + 
  geom_bar()


## geom_histogram()
ggplot(pumsFinal, aes(x=SALARY)) +
  geom_histogram() 


## geom_density()
ggplot(pumsFinal, aes(x=SALARY)) +
  geom_density(fill="green", alpha=0.2) 


## geom_boxplot()
ggplot(pumsFinal, aes(x=RACE, y=SALARY)) +
  geom_boxplot() 


## geom_violin()
ggplot(pumsFinal, aes(x=RACE, y=SALARY)) +
  geom_violin() 


## Facet Wrapping
ggplot(pumsFinal, aes(x=AGE, y=SALARY)) +
  geom_point(alpha=0.1) +
  facet_wrap(~RACE, scales='free')


## Axes and Title
ggplot(pumsFinal, aes(x=RACE)) + 
  geom_bar()+
  xlab("This is the X-axis")+
  ylab("This is the Y-axis")+
  ggtitle("This is the Title")


## Legends
ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(size=3, aes(color=class, shape=class))+
  scale_color_discrete(name="Legend Title" )


ggplot(mpg, aes(x=displ, y=hwy)) +
  geom_point(size=3, aes(color=class, shape=class))+
  scale_color_discrete(name="Legend Title" )+
  scale_shape_discrete(name="Legend Title")

  
## Labels
ggplot(mpg %>% filter(class=='midsize' & year==2008), 
       aes(x=displ, y=hwy))+
  geom_point()+
  geom_label_repel(label.padding = 0.1, label.size=0.1, 
                   aes(label=model, fill=manufacturer))

ggplot(mpg %>% filter(year==2008), aes(x=displ, y=hwy))+
  geom_point()+
  geom_label_repel(data=mpg %>% 
                     filter(year==2008 & manufacturer=='ford'),
                   aes(label=model))

  
## Themes
ggplot(mpg, aes(x=class)) +
  geom_bar() +
  theme_economist()


ggplot(mpg, aes(x=class)) +
  geom_bar() +
  theme_solarized()

  
  
  
## Maps
  

md <- map_data('state')
ggplot(md, aes(x=long, y=lat, group=group))+
  geom_polygon(fill=NA, color='black')+
  theme_map()


## Maps (PUMAS)
pumasOR <- read_rds('Data/pumasOR.rds')
ggplot(pumasOR, aes(x=long, y=lat, group=group))+
  geom_polygon(fill=NA, color='black')+
  theme_map()



## Maps (PUMAS) Cont.
pumasOR <- pumasOR %>% left_join(ests, by=c("id"="PUMA")) %>% pivot_longer(8:9, names_to="Type", values_to="Estimate")
ggplot(pumasOR, aes(x=long, y=lat, group=group))+
  geom_polygon(color='black', aes(fill=Estimate))+
  facet_wrap(~Type, nrow=1)+
  scale_fill_viridis_c()+
  coord_map()+
  theme_map()


## Animations
tsACS <- read_rds('Data/tsACS.rds')


p1 <- ggplot()+
  geom_polygon(data=tsACS,
               aes(x=long, y=lat, group=group, fill=estimate),
               color="black",  size=0.25)+
  theme_map()+
  coord_map()+
  scale_fill_viridis_c(name="Average Income")+
  theme(legend.position="right",plot.title = element_text(hjust = 0.5,face="bold"))+
  labs(title = "Average Income by County in {closest_state}") +
  transition_states(Year, transition_length = 2, state_length = 1)
animate(p1)

