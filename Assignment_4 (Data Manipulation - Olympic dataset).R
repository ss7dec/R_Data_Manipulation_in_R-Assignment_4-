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

#Since there are are multiple rows with same Athletes over the years
# it becomes necessary to merge the rows having the same name
# For eg. Michael Phelps - Swimming has won medals in 2008, 2012 and so on..
Olympics<-df %>%
  group_by(Athlete) %>%
  summarise_all(funs(first(na.omit(.))))
View(Olympics)

#[c("Athlete","Age","Country","Year","Closing.Date","Sport","Gold.Medals","Silver.Medals","Bronze.Medals","Total.Medals")]
# Sorting the data in the given/particular column in a Descending order
# For this purpose, a combination of order and descending functions are used.
# Order function will display results in Ascending Order
# Combining Order() with desc() will display results in Descending Order
sort_data_frame<-Olympics[with(Olympics, order(desc(Total.Medals))),]
View(sort_data_frame)


# Grouping the contents of the given/particular column Row-wise
# For this purpose, usage of arrange function is implemented
# arrange() will arrange/group the rows in Ascending Order for the 
#desired Column Only.
row_wise_grouping<-Olympics%>%arrange(Sport)
View(row_wise_grouping)

# Usage of filter() to select/pick-out the participants in selected/choseen sports
# in Descending order which is by default.
# Selected/Chosen Sports are Swimming, Table Tennis, Shooting, Gymnastics

swim_filter<-Olympics%>%filter(Sport=="Swimming") 
View(swim_filter)


tt_filter<-Olympics%>%filter(Sport=="Table Tennis")
View(tt_filter)


shoot_filter<-Olympics%>%filter(Sport=="Shooting")
View(shoot_filter)


gymnast_filter<-Olympics%>%filter(Sport=="Gymnastics")
View(gymnast_filter)

# To obtain Medal Tally in descending order, combination of 
#Order()  and desc() functions are used. This will display the Medal Tally 
# of the participants from various sports in descending order.

medal_tally<-Olympics[with(Olympics, order(desc(Total.Medals))),]
View(medal_tally)

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
View(swimming_rank)
  
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
# Herein we are applying Sorting at 3 levels in the given 
#order of preference  as mentioned below:
# a) Sports in Ascending Order
# b) Total Medals in Descending Order
#NOTE: Total Medals is being incorporated (though NOT specified in the given question),
# in order to imply Business Sense or Business perspective
# c) Descending Age for each Sports category

medal_tally_ranking_wtAge<-Olympics %>% mutate(rank = dense_rank(Age,desc(Total.Medals)))
medal_tally_ranking_wtAge

age_demo<-with(medal_tally_ranking, order(Sport), desc(Total.Medals), desc(Age))
age_demo
#medal_tally_ranking<-df %>% mutate(rank = dense_rank(desc(Total.Medals)))
#swimming_rank<-swim_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))
#age_demo<-with(medal_tally_ranking, order(Sport)

row_wise_grouping_A<-Olympics%>%arrange(Sport,desc(Total.Medals),(rank=dense_rank(desc(Age))))
row_wise_grouping_A


row_wise_grouping_B<-row_wise_grouping_A%>% mutate(rank=dense_rank(desc(Age)))
row_wise_grouping_B

str(rank)
class(rank)

check<-function (row_wise_grouping_B, na.last = TRUE, ties.method = c("first"))
check

row_wise_grouping_C<-with(row_wise_grouping_B,order(rank))
row_wise_grouping_C

#medal_tally<-df[with(df, order(desc(Total.Medals))),]
row_wise_grouping_D<-row_wise_grouping_B[with(row_wise_grouping_B,order(rank)),]
row_wise_grouping_D

age_demo_ranking<-medal_tally_ranking %>% mutate(rank = dense_rank(desc(Total.Medals,Age)))
age_demo_ranking

sports_category_wt_age_demo<-Olympics[with(Olympics, order(Sport), desc(Total.Medals), desc(Age)),]
sports_category_wt_age_demo


Olympics %>% 
  group_by(year) %>% 
  mutate(yrrank = rank(-count, ties.method = "first"))

sports_category_wt_age_demo %>%
  group_by(Sport)
  mutate(rank=rank(-Age,ties.method="first"))
  
  df$year.rank <- ave(count, year, FUN = function(x) rank(-x, ties.method = "first"))
         
sports_category_wt_age_demo$Age_Rank<- ave(count, Sports, FUN = function(x) rank(-sports_category_wt_age_demo,ties.method = "first"))      
         
seniority_age_rank<-sports_category_wt_age_demo %>% mutate[rank=Sport,desc (Total.Medals),desc(Age)]

sorted <- Olympics %>% 
  arrange(group, -value) %>%
  group_by(group) %>%
  mutate(rank=row_number())
print.data.frame(sorted)

sorted_1<-sports_category_wt_age_demo %>%
  arrange(Sport, -Total.Medals, -Age) %>%
  group_by(Sport) desc(Total.Medals) desc(-Age)%>%
  mutate(rank=row_number())
print.data.frame(sorted_1)

Olympics %>%
  group_by(customer_name) %>%
  mutate(my_ranks = order(order_values, order_dates, decreasing=TRUE))

eg<-sports_category_wt_age_demo %>%
  group_by(Sport) %>%
  order_by(-Age)
  mutate(rank=order(desc(Total.Medals),desc(Age), decreasing=TRUE))
eg

data.frame(Olympics, t(apply(-d, 1, rank, ties.method='min')))

eg_2<-data.frame(sports_category_wt_age_demo,t(apply(-sports_category_wt_age_demo,1,rank,ties.method='min')))
eg_2

apply(-x, 1, rank)

eg_3<-apply(sports_category_wt_age_demo,1,rank)
eg_3

mydf$rank=unlist(with(mydf,tapply(number,ID,rank)))
sports_category_wt_age_demo$rank=unlist(with(sports_category_wt_age_demo,tapply(Total.Medals,Age,rank)))

# Assigning Ranks based on Age Categorization:

seniority_age_rank<-sports_category_wt_age_demo %>% mutate(rank=dense_rank(Sport,desc (Total.Medals),desc(Age)))
seniority_age_rank
########################################

#4. Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting .
#Gymnastics . Total Medal

year_wise_medal_tally<-sort_data_frame<-Olympics[with(Olympics, order(Year,desc(Total.Medals))),]
swim_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Swimming")
yearly_swimming_rank<-swim_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mt <- yearly_swimming_rank[order(yearly_swimming_rank$Year), ]
demo_A <- by(mt, mt["Year"], head, n=4)
Swimming_Rankers_Top_4<-Reduce(rbind, demo_A)
Swimming_Rankers_Top_4


#swim_filter<-df%>%filter(Sport=="Swimming")
#swimming_rank<-swim_filter %>% mutate(rank=dense_rank(desc(Total.Medals)))

#yearly_swimming_rank<-swimming_rank %>% mutate(rank=dense_rank(desc(Total.Medals)))
#yearly_swimming_rank

#age_demo<-with(medal_tally_ranking, order(Sport), desc(Total.Medals), desc(Age))
#age_demo

year_wise_medal_tally<-sort_data_frame<-Olympics[with(Olympics, order(Year,desc(Total.Medals))),]
tt_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Table Tennis")
yearly_tt_rank<-tt_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mu <- yearly_tt_rank[order(yearly_tt_rank$Year), ]
demo_B <- by(mu, mu["Year"], head, n=4)
TT_Rankers_Top_4<-Reduce(rbind, demo_B)
TT_Rankers_Top_4


year_wise_medal_tally<-sort_data_frame<-Olympics[with(Olympics, order(Year,desc(Total.Medals))),]
shooting_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Shooting")
yearly_shooting_rank<-shooting_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mv <- yearly_shooting_rank[order(yearly_shooting_rank$Year), ]
demo_C <- by(mv, mv["Year"], head, n=4)
Shooting_Rankers_Top_4<-Reduce(rbind, demo_C)
Shooting_Rankers_Top_4

year_wise_medal_tally<-sort_data_frame<-Olympics[with(Olympics, order(Year,desc(Total.Medals))),]
gymnastics_filter_yearly<-year_wise_medal_tally%>%filter(Sport=="Gymnastics")
yearly_gymnastics_rank<-gymnastics_filter_yearly %>% mutate(rank=dense_rank(desc(Total.Medals)))

mw <- yearly_gymnastics_rank[order(yearly_gymnastics_rank$Year), ]
demo_D <- by(mw, mw["Year"], head, n=4)
Gymnastics_Rankers_Top_4<-Reduce(rbind, demo_D)
Gymnastics_Rankers_Top_4

year_wise_medal_tally<-sort_data_frame<-Olympics[with(Olympics, order(Year,desc(Total.Medals))),]
yearly_olympic_games_ranking<-year_wise_medal_tally %>% mutate(rank=dense_rank(desc(Total.Medals)))

mx <- yearly_olympic_games_ranking[order(yearly_olympic_games_ranking$Year),] 
demo_finale<- by(mx, mx["Year"], head, n=2)                                  
Medal_Tally_Top_4<-Reduce(rbind, demo_finale)
Medal_Tally_Top_4
