if(!file.exists("./data")){dir.create("./data")}
fileurl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile = "data/StormData.csv", method = "curl" )
# reading data
storm<- read.csv("data/StormData.csv")
#load packages
library(dplyr)
library(vctrs)
# Question 1 : Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

## Creating a new data frame with the necessery variables where the observations are grouped on the basis of the EVTYPE
q1_group <- storm %>% select(FATALITIES, INJURIES, EVTYPE) %>% 
                       group_by(EVTYPE) %>%
                       mutate(Casualties = FATALITIES + INJURIES)
#Sum up the injuries and fatalities for each type of event and arrange in descending order
q1_answer<-aggregate(cbind(FATALITIES, INJURIES, Casualties) ~ EVTYPE, data = q1_group, sum) 
q1_answer <-q1_answer[order(q1_answer$Casualties, q1_answer$FATALITIES, q1_answer$INJURIES, decreasing = TRUE),]
#Remove the previous data frames for clearance 
rm(q1_group)

# Question 1 plot
library(ggplot2)
q1_answer_plot<- q1_answer[1:15,]
q1_answer_plot <- rbind(
          cbind(select(q1_answer_plot, c(EVTYPE,"COUNT" = FATALITIES)),"CATEGORY" = "FATALITIES"),
          cbind(select(q1_answer_plot, c(EVTYPE,"COUNT" = INJURIES)),"CATEGORY" = "INJURIES"),
          cbind(select(q1_answer_plot, c(EVTYPE, "COUNT"= Casualties)), "CATEGORY" = "CASUALTIES")
)
q1_answer_plot %>%
          ggplot(aes(fill=CATEGORY, y=COUNT, x=reorder(EVTYPE,-COUNT)))+
          geom_bar(position = "dodge", stat = "identity")+
          labs(title = "Q1.Top 15 Harmful events", x="Event type", y = "Count")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
 
#Question2 : Across the United States, which types of events have the greatest economic consequences?
#In the storm data the damage estimates were entered as dollar amounts (PROPDMG) but the amount was rounded to three digits and the magnitude was signified by Kfor thousands, M for million and B for billion

#Create a new variable where letter are changed to coressponding numbers and then use it to multiply the numbers
q2<- storm %>% select(EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG,CROPDMGEXP)
q2$PROPDMG_AMOUNT<- with(q2,
                            ifelse(PROPDMGEXP %in% c("k","K"), PROPDMG*1000,
                                   ifelse(PROPDMGEXP %in% c("m","M"), PROPDMG*1000000,
                                          ifelse(PROPDMGEXP %in% c("b","B"), PROPDMG*1000000000,
                                                 0)
                                          )
                            )
)
q2$CROPDMG_AMOUNT<- with(q2,
                         ifelse(CROPDMGEXP %in% c("k","K"), CROPDMG*1000,
                                ifelse(CROPDMGEXP %in% c("m","M"), CROPDMG*1000000,
                                       ifelse(CROPDMGEXP %in% c("b","B"), CROPDMG*1000000000,
                                              0)
                                )
                         )
)
q2<- q2 %>% mutate(TOTAL_DMG_VALUE = CROPDMG_AMOUNT+PROPDMG_AMOUNT) %>%
                   select(EVTYPE, TOTAL_DMG_VALUE)
q2_answer<- aggregate(TOTAL_DMG_VALUE~EVTYPE, data = q2, sum)
q2_answer<- arrange(q2_answer, desc(TOTAL_DMG_VALUE))
#Plot 2
library(ggplot2)
q2_answer_plot<- q2_answer[1:15,]
q2_answer_plot %>%
          ggplot(aes(y=TOTAL_DMG_VALUE, x=reorder(EVTYPE,-TOTAL_DMG_VALUE)))+
          geom_bar(position = "dodge", stat = "identity")+
          labs(title = "Q2.Events with greatest economic consequences", x="Event type", y = "US Dollars")+
          theme(axis.text.x = element_text(angle = 45, hjust = 1)
                ) 
          
