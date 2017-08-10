#1.Consider only those participants who have all the data points
#2. Rank the participants in terms : . Swimming . Table Tennis . Shooting . Gymnastics . Total
#Medal
#3. Rank the Categories in terms of Age.(Higher the Age,Higher the Rank)
#4. Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting .
#Gymnastics . Total Medal

#1.Consider only those participants who have all the data points

mydata<-read.csv("E://Back up 1/ACADGILD/olympic_data.csv", header= TRUE, sep = ",", na.strings = "NA")
mydata

# By using complete.cases, the null or the missing values within the dataset 
#get removed.

olympic_data <- mydata[complete.cases(mydata), ]
olympic_data

#############################

#2. Rank the participants in terms : . Swimming . Table Tennis . Shooting . Gymnastics . Total
#Medal

install.packages("dplyr")
install.packages("gapminder")
install.packages("tidyverse")

library(dplyr)
library(gapminder)
library(tidyverse)

# Conversion of .csv file to a Data frame
df<-data.frame(olympic_data)
df

# Sorting the data in the given/particular column in a Descending order
# For this purpose, a combination of order and descending functions are used.
# Order function will display results in Ascending Order
# Combining Order() with desc() will display results in Descending Order
sort_data_frame<-df[with(df, order(desc(Total.Medals))),]
sort_data_frame

# Grouping the contents of the given/particular column Row-wise
# For this purpose, usage of arrange function is implemented
# arrange() will arrange/group the rows in Ascending Order for the 
#desired Column Only.
row_wise_grouping<-df%>%arrange(Sport)
row_wise_grouping

# Usage of filter() to select/pick-out the participants in selected/choseen sports
# in Descending order which is by default.
# Selected/Chosen Sports are Swimming, Table Tennis, Shooting, Gymnastics

swim_filter<-df%>%filter(Sport=="Swimming") 
swim_filter


tt_filter<-df%>%filter(Sport=="Table Tennis")
tt_filter


shoot_filter<-df%>%filter(Sport=="Shooting")
shoot_filter


gymnast_filter<-df%>%filter(Sport=="Gymnastics")
gymnast_filter

# To obtain Medal Tally in descending order, combination of 
#Order()  and desc() functions are used. This will display the Medal Tally 
# of the participants from various sports in descending order.

medal_tally<-df[with(df, order(desc(Total.Medals))),]
medal_tally

#To apply Ranks to the Specified/Selected Groups as desired, we need to apply
# a combination of dense_rank() which forms a part of "dplyr package" and mutate().
#dense_rank() is applied in a reverse order and is combined with descending function
# to produce the desired results from high to low i.e. in descending order.

# Assigning Ranks to Swimming Competition 
# A comination of Mutate() and desc() functions are used.
# By using Mutate(), a new column gets generated.
# For this assignment/exercise, we are creating a Rank column
# Herein Ranks are assigned in descending order. 
swimming_rank<-swim_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))
swimming_rank
  
# Assigning Ranks to Table Tennis Competition 
table_tennis_rank<-tt_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))
table_tennis_rank

# Assigning Ranks to Shooting Competition 
shooting_rank<-shoot_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))
shooting_rank

# Assigning Ranks to Gymnastics Competition 
gymnastics_rank<-gymnast_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))
gymnastics_rank

# Assigning Ranks to the Total Medal Tally (or Medals Won)
medal_tally_ranking<-df %>% mutate(rank = dense_rank(desc(Total.Medals)))
medal_tally_ranking

#####################################

#3. Rank the Categories in terms of Age.(Higher the Age,Higher the Rank)

#ANS 3:
  Age_Categorization<- df %>% select (Sport, Age) %>% 
        group_by(Sport) %>%
        filter(Age == max(Age)) %>%
        distinct() %>% 
        summarise(Age = max(Age)) %>% 
        arrange (desc(Age)) %>% 
        mutate(Rank = row_number(desc(Age)))
      View(Age_Categorization)
########################################

#4. Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting .
#Gymnastics . Total Medal

year_wise_medal_tally<-sort_data_frame<-df[with(df, order(Year,desc(Total.Medals))),]
swim_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Swimming")
yearly_swimming_rank<-swim_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mt <- yearly_swimming_rank[order(yearly_swimming_rank$Year), ]
demo_A <- by(mt, mt["Year"], head, n=4)
Swimming_Rankers_Top_4<-Reduce(rbind, demo_A)
View(Swimming_Rankers_Top_4)


year_wise_medal_tally<-sort_data_frame<-df[with(df, order(Year,desc(Total.Medals))),]
tt_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Table Tennis")
yearly_tt_rank<-tt_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mu <- yearly_tt_rank[order(yearly_tt_rank$Year), ]
demo_B <- by(mu, mu["Year"], head, n=4)
TT_Rankers_Top_4<-Reduce(rbind, demo_B)
View(TT_Rankers_Top_4)


year_wise_medal_tally<-sort_data_frame<-df[with(df, order(Year,desc(Total.Medals))),]
shooting_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Shooting")
yearly_shooting_rank<-shooting_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mv <- yearly_shooting_rank[order(yearly_shooting_rank$Year), ]
demo_C <- by(mv, mv["Year"], head, n=4)
Shooting_Rankers_Top_4<-Reduce(rbind, demo_C)
View(Shooting_Rankers_Top_4)

year_wise_medal_tally<-sort_data_frame<-df[with(df, order(Year,desc(Total.Medals))),]
gymnastics_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Gymnastics")
yearly_gymnastics_rank<-gymnastics_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mw <- yearly_gymnastics_rank[order(yearly_gymnastics_rank$Year), ]
demo_D <- by(mw, mw["Year"], head, n=4)
Gymnastics_Rankers_Top_4<-Reduce(rbind, demo_D)
View(Gymnastics_Rankers_Top_4)

year_wise_medal_tally<-sort_data_frame<-df[with(df, order(Year,desc(Total.Medals))),]
yearly_olympic_games_ranking<-year_wise_medal_tally %>% mutate(rank=dense_rank(desc(Total.Medals)))

mx <- yearly_olympic_games_ranking[order(yearly_olympic_games_ranking$Year),] 
demo_finale<- by(mx, mx["Year"], head, n=2)                                  
Medal_Tally_Top_4<-Reduce(rbind, demo_finale)
View(Medal_Tally_Top_4)
