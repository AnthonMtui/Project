#Data Analysis on Ihi

#get working directory
getwd()
#set a new working directory
setwd("C:/Users/Anthon/Desktop/Data Analytics study/Data Analytics/R/Projects Learning/IHI")

#Importing data in R
library(readr)
Malaria<- read_csv("Malaria_burden_in_XYZ.csv", 
                   col_types = cols(suspected = col_integer(), 
                                    `confirmed case` = col_integer(), 
                                    Population = col_integer()))
View(Malaria)

#Cleaning data
library(tidyverse)

#Check for duplicates
library(janitor)
get_dupes(Malaria, c(District, year))
#No duplicates found.

#Use the names() to get column names and copy without making a mistake
names(Malaria)

#Check for missing values(NAs)
Malaria %>%
  select("District", "year", "suspected", "confirmed case",
         "Population") %>%
  filter(! complete.cases(.))

#The dataset has 4 NAs. View the data set to see if self-handling NAs is posssible
View(Malaria)
#Self-handling is impossible. Omit the rows with NAs
Malaria <- na.omit(Malaria)

#Change the column names(suspected, confirmed case, year)
#The columns should start with a capital. Remove the space between
#the columns replace with an underscore
colnames(Malaria)[2:4] = c("Year", "Suspected", "Confirmed_cases")

#Check the negative values/  in columns. They dont have a negative nature
Malaria %>%
  select(District, Year, Confirmed_cases, Suspected, Population) %>%
  filter(Population < 1 | Confirmed_cases < 0 | Suspected < 0 | Year < 2000)

#In all filters we kept. We see that the data set has negative values.
#Change the values into positives.
Malaria$Population[Malaria$Population == -7828] <- 7828
Malaria$Population[Malaria$Population == -6674] <- 6674
Malaria$Population[Malaria$Population == -6410] <- 6410
#The negatives have been removed

#Switch the arrangements of the column. Suspected should be after Confirmed cases
Malaria <- Malaria[ , c(1,2,4,3,5)]
#We now have a TIDY DATA

#Check summary to identify mean, Q1, Q3.
summary(Malaria)

#Analysis of the data starts here.
#Total sums of (Confirmed and suspected) for all the districts in the year
#I added the yearly count to put the whole context of how many years were used to calculate the sum
MalariaYearly = Malaria %>%
  group_by(Year) %>%
  summarize(Confirmed_cases = sum(Confirmed_cases),
            Suspected = sum(Suspected),
            YearlyCount = n())

#Find the percentage of positive Malaria yearly
MalariaPercentageYearly = Malaria %>%
  group_by(Year) %>%
  summarize(Confirmed_cases = sum(Confirmed_cases),
            Suspected = sum(Suspected),
            YearlyCount = n()) %>%
  mutate(Percentage = Confirmed_cases / Suspected)

#Add the plot of MalariaPercentageYearly
#Add Libraries to plot
library(ggplot2)
library(ggalt)
library(scales)
library(forcats)

MalariaPercentageYearly %>%
  ggplot(aes(Year, Percentage)) +
  scale_y_continuous(labels = percent_format()) +
  geom_line()+
  labs(title = "Change in Malaria confirmed in years")

#How does malaria incidence per thousand population vary among districts
MalariaPrevalence = Malaria %>%
  mutate(Population1000 = Population / 1000) %>%
  mutate(Prevalence_rate = Confirmed_cases / Population1000)%>%
  select(-(Population))%>%
  select(-(Population1000))

#For that comparison. Draw a compound line graph
MalariaPrevalence %>%
  ggplot(aes(Year, Prevalence_rate, colour = District))+
  geom_point(size = 0.5, alpha = 0.8 )+
  geom_line(size = 0.3)+
  theme_minimal()+
  labs(title = "Malaria incidence per thousand population")
  
#Incidence change on each District
MalariaPrevalence %>%
  filter(District %in% sample(unique(District))) %>%
  ggplot(aes(Year, Prevalence_rate, color = District))+
  geom_line()+
  scale_y_continuous(labels = scales :: percent_format())

#After getting the prevalence rate. We can use it to create 
#change per each district. Then we rbind the districts. to get all values aligned.
#Change. to answer qn1.
#Wakabi
Wakabi = MalariaPrevalence %>%
  filter(District == "Wakabi")%>%
  mutate(Previousrate = lag(Prevalence_rate),
         Change = Prevalence_rate - Previousrate,
         ChangePercentage = (Change/Previousrate)* 100)%>%
  select(-(Previousrate))


#Okoye
Okoye = MalariaPrevalence %>%
  filter(District == "Okoye")%>%
  mutate(Previousrate = lag(Prevalence_rate),
         Change = Prevalence_rate - Previousrate,
         ChangePercentage = (Change/Previousrate)* 100)%>%
  select(-(Previousrate))

#Shuri
Shuri = MalariaPrevalence %>%
  filter(District == "Shuri")%>%
  mutate(Previousrate = lag(Prevalence_rate),
         Change = Prevalence_rate - Previousrate,
         ChangePercentage = (Change/Previousrate)* 100)%>%
  select(-(Previousrate)) 

#Dora Milaje
DoraMilaje = MalariaPrevalence %>%
  filter(District == "Dora Milaje")%>%
  mutate(Previousrate = lag(Prevalence_rate),
         Change = Prevalence_rate - Previousrate,
         ChangePercentage = (Change/Previousrate)* 100)%>%
  select(-(Previousrate)) 

#Then after that we bind the tables
Changetable <- rbind(Wakabi, Okoye, Shuri, DoraMilaje)


#To answer qn 1. On Change over the years on respective districts.
Wakabi %>%
  ggplot()+
  geom_col(aes(Year, ChangePercentage))+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("Wakabi Malaria Cases Change") 

Okoye %>%
  ggplot()+
  geom_col(aes(Year, ChangePercentage))+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("Okoye Malaria Cases Change")
  
Shuri %>%
  ggplot()+
  geom_col(aes(Year, ChangePercentage))+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("Shuri Malaria Cases Change")

DoraMilaje %>%
  ggplot()+
  geom_col(aes(Year, ChangePercentage))+
  geom_hline(yintercept = 0)+
  theme_classic()+
  ggtitle("DoraMilaje Malaria Cases Change")
  
#Overall change over the Years. Last year - First Year
WakabiT = Wakabi %>%
  filter(Year == 2002 | Year == 2014) %>%
  mutate(PreviousConfirmed = lag(Confirmed_cases),
         ChangeT = Confirmed_cases - PreviousConfirmed,
         ChangePercentage = (ChangeT/PreviousConfirmed)* 100)%>%
  select(-(PreviousConfirmed)) 

OkoyeT = Okoye %>%
  filter(Year == 2002 | Year == 2014) %>%
  mutate(PreviousConfirmed = lag(Confirmed_cases),
         ChangeT = Confirmed_cases - PreviousConfirmed,
         ChangePercentage = (ChangeT/PreviousConfirmed)* 100)%>%
  select(-(PreviousConfirmed)) 

ShuriT = Shuri %>%
  filter(Year == 2002 | Year == 2014) %>%
  mutate(PreviousConfirmed = lag(Confirmed_cases),
         ChangeT = Confirmed_cases - PreviousConfirmed,
         ChangePercentage = (ChangeT/PreviousConfirmed)* 100)%>%
  select(-(PreviousConfirmed)) 

DoraMilajeT = DoraMilaje %>%
  filter(Year == 2002 | Year == 2014) %>%
  mutate(PreviousConfirmed = lag(Confirmed_cases),
         ChangeT = Confirmed_cases - PreviousConfirmed,
         ChangePercentage = (ChangeT/PreviousConfirmed)* 100)%>%
  select(-(PreviousConfirmed)) 

Over <- rbind(WakabiT, OkoyeT, ShuriT, DoraMilajeT)

Over = Over %>%
  select(c(District, Year, Confirmed_cases))

#pivot_wider
Over = Over %>%
  pivot_wider(names_from = Year, 
              values_from = Confirmed_cases)
  
colnames(Over)[2:3] = c("start", "end")

#Draw a dumbbell plot to show change over the years
ggplot(Over) + 
  geom_dumbbell(aes(x = start, xend = end,
                    y = District),
                color = "darkgrey",  # Color of the line
                size = 1.5,            # Line width
                size_x = 2.5,               #Size of the dot, start 
                size_xend = 2.5,            #size of the dot, end
                dot_guide = FALSE,   # Whether to add a guide from origin to X or not
                colour_x = "#F69541",    # Color of the X point
                colour_xend = "#699DC6") + # Color of the X end point
  geom_text(aes(x = start, y = District, label="2002"),
            color = "#F69541", size=2.5, vjust=-1.5,
            fontface="bold", family="Lato") +
  geom_text(aes(x = end, y = District, label="2014"),
            color = "#699DC6", size=2.5, vjust=-1.5,
            fontface="bold", family="Lato") +
  labs(x = "Confirmed_cases", y = "District",                                                
    title = "Malaria Change in each district from start of the project to present",        #title of the plot
  )

#Find the sum of both suspected and confirmed_cases as per Districts
Malaria2 = Malaria %>%
  group_by(District)%>%
  summarize(Confirmed_cases = sum(Confirmed_cases))
#the select fn kept saying Suspected not found I can use the joins to combine the next table

#sum of the Suspected
Malaria3 = Malaria %>%
  group_by(District)%>%
  summarize(Suspected = sum(Suspected))

#Join Malaria2 and Malaria3
Malaria2 = Malaria2 %>%
  full_join(Malaria3, by = "District")

#Plot Grouped bar graph
library(ggplot2)
library(reshape2)
Malaria2 <- melt(Malaria2, id = "District")

ggplot(Malaria2)+
  geom_bar(aes(x = District, y = value, fill = variable),
           position = "dodge", stat = "identity")+
  coord_flip()+
  labs(title = "Sum of confirmed cases and suspected per Districts",
       subtitle = "Sum over all the period")

library(ggplot2)
library(ggalt)

#Find total of confirmed and suspected cases
colSums(Malaria4[, c(3,4)], na.rm = TRUE)

#Create a barplot of total confirmed and suspected cases
names <- c("confirmed_cases", "suspected") 
values <- c(9549, 22506)

Malaria44  <- data.frame(names, values)

Malaria44 %>%
  ggplot(aes(x = names, y = values))+
  geom_bar(stat = "Identity")+
  coord_flip()+
  labs(title = "sum of confirmed and suspected cases",
       x = "",
       y = "total cases")

#Create a boxplot for each district
#Wakabi boxplot
ggplot(Wakabi, aes(x = Confirmed_cases))+
  geom_boxplot()+
  labs(title = "Wakabi boxplot")

#Shuri boxplot
ggplot(Shuri, aes(x = Confirmed_cases))+
  geom_boxplot()+
  labs(title = "Shuri boxplot")

#Okoye boxplot
ggplot(Okoye, aes(x = Confirmed_cases))+
  geom_boxplot()+
  labs(title = "Okoye boxplot")

#DoraMilaje boxplot
ggplot(DoraMilaje, aes(x = Confirmed_cases))+
  geom_boxplot()+
  labs(title = "Dora Milaje boxplot")

Changetable2 = Changetable %>%
  select(-(Suspected)) %>%
  select(-(Prevalence_rate)) %>%
  select(-(Change)) %>%
  select(-(ChangePercentage))


Over1 <- Changetable %>%
  filter(Year == 2002 | Year == 2014) %>%
  group_by(District)%>%
  mutate(change = last(Year) - first(Year))%>%
  select(Prevalence_rate)%>%
  select()%>%
  select() 

Wakabi %>%
  select(Confirmed_cases) %>%
  summary()

DoraMilaje %>%
  select(Confirmed_cases) %>%
  summary()

Shuri %>%
  select(Confirmed_cases) %>%
  summary()

Okoye %>%
  select(Confirmed_cases) %>%
  summary()

MalariaPrevalence2 = MalariaPrevalence %>%
  filter(District == Wakabi) %>%
  select(Prevalence_rate) %>%
  summary()





write.csv(Malaria, file = "Malaria.csv")
write.csv(Malaria_yearly, file = "Malaria_yearly.csv")
write.csv(MalariaPrevalence, file = "MalariaPrevalence")
write.csv(MalariaPercentageYearly, file = "MalariaPercentageYearly")
write.csv(Over, file = "Over.csv")
