#Code for main data ----

#loading data sets into environment
state_cases <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time_1230.csv", header = TRUE)
state_population <- read.csv("nst-est2019-alldata.csv", header = TRUE)

#finding state abbreviations for cases and death data from CDC
unique(state_cases[order(state_cases$state),]$state)

#finding state names for Census Data on population
unique(state_population$NAME)

#Removing United States, Northeast, Midwest, South, West Regions, and Puerto Rico from Data
state_population <- state_population[-c(1, 2, 3, 4, 5, 57,14),]

#[MAIN] SECTION 2: [Data Integration] - Mapping data values & merging data sets

library(dplyr)
library(wrapr)
library(ggplot2)
library(scales)
library(plotly)

#mapping values between data sets based on State abbreviations
map <- qc(Alabama = AL, Alaska = AK, Arizona = AZ, Arkansas = AR,
          California = CA, Colorado = CO, Connecticut = CT, Delaware = DE,
          Florida = FL, Georgia = GA, Hawaii = HI, 
          Idaho = ID, Illinois = IL, Indiana = IN, Iowa = IA, Kansas = KS,
          Kentucky = KY, Louisiana = LA, Maine = ME, Maryland = MD,
          Massachusetts = MA, Michigan = MI, Minnesota = MN, Mississippi = MS, 
          "District of Columbia" = DC,
          Missouri = MO, Montana = MT, Nebraska = NE, Nevada = NV,
          "New Hampshire" = NH, "New Jersey" = NJ, "New Mexico" = NM, "New York" = NY,
          "North Carolina" = NC, "North Dakota" = ND, Ohio = OH, Oklahoma = OK,
          Oregon = OR, Pennsylvania = PA, "Rhode Island" = RI,
          "South Carolina" = SC, "South Dakota" = SD, Tennessee = TN, Texas = TX,
          Utah = UT, Vermont = VT, Virginia = VA, Washington = WA,
          "West Virginia" = WV, Wisconsin = WI, Wyoming = WY)
state_population$STATE_AB <- map[state_population$NAME]          
state_population <- state_population %>%
  select("STATE_AB", everything())

#Only saving the the estimates for state population 
state_population <- state_population[c(1,6,3,7,18)]

# Merging two data sets based on State Abbreviations, making main data
data <- merge(state_cases, state_population, by.x="state", by.y="STATE_AB")
data$cases_perc <- data$tot_cases/data$POPESTIMATE2019*100
data$death_perc <- data$tot_death/data$tot_cases*100
data$death_perc_by_population <- data$tot_death/data$POPESTIMATE2019*100
data <- data %>%
  select(c("state", "NAME", "cases_perc", "death_perc", "death_perc_by_population"), everything())
data$submission_date <- as.Date(as.character(data$submission_date),format='%m/%d/%Y')

#Integrating potential factors data set
cofounders <- read.csv("US_states_religion_and_politics.csv", header = TRUE)
colnames(cofounders)[1]<-c("State")
urban_perc <- read.csv("PctUrbanRural_State.csv", header = TRUE)

#Only keeping columns related to Urban Area percentages and Urbanized Area percentages
urban_perc <- urban_perc[c(2,5,6,7,8,9,11,13,14)]

#it shows up as just "State" for Mac, but "?..State" for Windows, please change accordingly
additional_data <- merge(urban_perc, cofounders, by.x="STATENAME", by.y="State")

data <- merge(data, additional_data, by.x="NAME",by.y="STATENAME")

#import age_distribution_data
age_distribution <- read.csv("age_distribution_data.csv", header = TRUE)
colnames(age_distribution)[1]<-c("State")
data <- merge(data, age_distribution, by.x="NAME", by.y="State")

#Cleaning the environment of unnecessary data sets
remove(cofounders,urban_perc,state_population,state_cases,age_distribution,additional_data,map)

#If we want to only look at the most recent situation, the day our data was last collected: 
#current_situation<-data%>%filter(submission_date=="10/24/2020") 

#[MAIN] SECTION 3: [Data Modification] - Finding the date of the first case for each State 

library(plyr)

#filtering dates with 0 total cases
initial <- data%>%
  filter(tot_cases!=0)

start_dates <- ddply(initial, .var = "NAME", .fun = function(x) {
  return(subset(x, tot_cases %in% min(tot_cases)))
}
)

start_dates <- ddply(start_dates, .var = "NAME", .fun = function(x) {
  return(subset(x, submission_date %in% min(submission_date)))
}
)

#only need columns "Name" and "start_date" to merge
start_dates <- start_dates[c(1,6)]
colnames(start_dates) <- c("Name","start_date")

#Merge start_dates with original data with State Names
data_adjusted <- merge(start_dates, data, by.x="Name", by.y="NAME")

#Creating column "days_since_first_case" for a numerical determination of how long Covid-19 has been in the state
data_adjusted <- data_adjusted%>%
  mutate(days_since_first_case = as.numeric(submission_date-start_date))%>%
  mutate(days_since_start = as.numeric(submission_date-as.Date('2020-01-22')))%>%  
  select(c("Name", "submission_date", "days_since_first_case", "days_since_start"), everything())%>%
  filter(days_since_first_case >= 0)%>%filter(submission_date<as.Date('2020-12-13'))

#To look at data under a chronological, alphabetical ordered format:
#click the header of column "submission_date" first, then click the header of column "Name"

#remove unnecessary data from environment: initial, cofounders, and start_dates from environment
remove(start_dates)
remove(initial)

#[MAIN] SECTION 4: [Data Clustering] - Grouping States by Polynomial Regression Coefficients 

#Running and Returning Coefficient and its Signs from linear model tests
data_adjusted <- data_adjusted[-c(60,61)]

#Death_perc_pop
coefficients <- data.frame("hi","hi")
colnames(coefficients)<-c("State","Death_Perc_Pop_Coefficient")
for (name in unique(data_adjusted$Name)){
  T <- data_adjusted %>%
    filter(Name==name)
  regression <- lm(death_perc_by_population ~ days_since_first_case + I(days_since_first_case^2), data = T)
  coefficients <- rbind(coefficients,c(name,regression$coefficients[3]))
}

coefficients <- coefficients[-1,]

#updating data_adjusted with coefficient signs 
data_adjusted <- merge(data_adjusted,coefficients,by.x="Name", by.y="State")

death_pop_stat <- rep(0,nrow(data_adjusted))
for (i in 1:nrow(data_adjusted)){
  if (data_adjusted$Death_Perc_Pop_Coefficient[i]>0){death_pop_stat[i]<-1}
}
data_adjusted <- cbind(data_adjusted,death_pop_stat)

data_adjusted$death_pop_stat <- factor(data_adjusted$death_pop_stat, 
                                       levels = c("0", "1"), 
                                       labels = c("Negative", "Positive"))


#remove unnecessary data from environment: name, T, and regression
remove(name)
remove(T)
remove(regression)
remove(death_pop_stat)
remove(i)
remove(coefficients)
remove(data)


monthly_test <- data_adjusted %>%
  filter(days_since_first_case == 0 | days_since_first_case == 30 | days_since_first_case == 60 | days_since_first_case == 90|
           days_since_first_case == 120 | days_since_first_case == 150 | days_since_first_case == 180 |
           days_since_first_case == 210 | days_since_first_case == 240 | days_since_first_case == 270)
monthly_test$new_death <- 0
monthly_test <- monthly_test[with(monthly_test, order(submission_date)),]
monthly_test <- monthly_test[with(monthly_test, order(Name)),]
row.names(monthly_test) <- NULL
for (c in 1:nrow(monthly_test)){
  monthly_test[c,18] <- ifelse(monthly_test[c,2]==monthly_test[c,5], monthly_test[c,15], monthly_test[c,15]-monthly_test[c-1,15])
}
monthly_test$monthly_death_pop <- monthly_test$new_death/monthly_test$POPESTIMATE2019*100
monthly_test<-monthly_test%>%
  filter(!is.na(monthly_death_pop))

monthly_test2 <- as.data.frame(tapply(monthly_test$monthly_death_pop, monthly_test$Name,median))
colnames(monthly_test2) <- c("Median_Death_Rate_by_Month") 
monthly_test2 <- tibble::rownames_to_column(monthly_test2, "Name")
data <- merge(monthly_test2, monthly_test, by.x="Name", by.y="Name")

covid_monthly <- data
colnames(covid_monthly)[1] <- 'STATENAME'

monthly <- covid_monthly[,c(1,63,4)]
month<-rbind(monthly$monthly_death_pop[1:10])

for (c in 1:49){
  month<-rbind(month,rbind(monthly$monthly_death_pop[((c*10)+1):(10*(c+1))]))
}
row.names(month)<-unique(monthly$STATENAME)
month <- month[,-c(1)]

library(amap)
cluster2 <- Dist(month,method='correlation')
fits<-hclust(cluster2, method="complete")

test <- cutree(fits, k=2)
test <- as.data.frame(test,row.names =NULL)
test <- tibble::rownames_to_column(test, "State")
covid_monthly <- merge(covid_monthly, test, by.x = 'STATENAME', by.y = 'State')
colnames(covid_monthly)[ncol(covid_monthly)] <- "corr2_label"

#corr4_3_label
test <- cutree(fits, k=4)
test <- as.data.frame(test,row.names =NULL)
test <- tibble::rownames_to_column(test, "State")
covid_monthly <- merge(covid_monthly, test, by.x = 'STATENAME', by.y = 'State')
colnames(covid_monthly)[ncol(covid_monthly)] <- "corr4_label"


covid_monthly$corr2_label <- factor(covid_monthly$corr2_label, 
                                    levels = c("1", "2"), 
                                    labels = c("Gradual", "Spike"))
covid_monthly$corr4_3_label= case_when(covid_monthly$corr4_label == 3 ~ "Initial_Spike",
                                       covid_monthly$corr4_label == 4 ~ "Initial_Spike",
                                       covid_monthly$corr4_label == 2 ~ "Two_Humped",
                                       covid_monthly$corr4_label == 1 ~ "Recent_Spike")

covid_monthly$corr3_label= case_when(covid_monthly$corr4_3_label == "Initial_Spike" ~ 1,
                                     covid_monthly$corr4_3_label == "Recent_Spike" ~ 3,
                                     covid_monthly$corr4_3_label == "Two_Humped" ~ 2)
data_adjusted$daily_death_pop <- data_adjusted$new_death/data_adjusted$POPESTIMATE2019*100
newest <- newest <- ddply(covid_monthly[,c(1,4,64,65,66,67)], .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 
newest <- newest[,-c(2)]
data_adjusted <- merge(data_adjusted, newest,by.x= "Name", by.y="STATENAME")
data_adjusted  <- data_adjusted [with(data_adjusted , order(submission_date)),]
data_adjusted  <- data_adjusted [with(data_adjusted , order(Name)),]
row.names(data_adjusted ) <- NULL


#cumulative
usa <- as.data.frame(tapply(data_adjusted$tot_cases, data_adjusted$submission_date, sum))
usa<-cbind(usa,tapply(data_adjusted$tot_death, data_adjusted$submission_date, sum))
usa<-cbind(usa,tapply(data_adjusted$POPESTIMATE2019, data_adjusted$submission_date, sum))
colnames(usa)<-c('total_cases','total_death','total_population')
usa$total_death_perc <- usa$total_death/usa$total_population*100
usa$total_cases_perc <- usa$total_cases/usa$total_population*100
usa <- tibble::rownames_to_column(usa, "submission_date")
usa$submission_date <- as.Date(usa$submission_date)

simulation <- data_adjusted[data_adjusted$days_since_first_case<271,]
usa2 <- as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum))
usa2<-cbind(usa2,tapply(simulation$tot_cases, simulation$days_since_first_case, sum))
usa2<-cbind(usa2,tapply(simulation$POPESTIMATE2019, simulation$days_since_first_case, sum))
colnames(usa2)<-c('total_death','total_cases','total_population')
usa2$total_death_perc <- usa2$total_death/usa2$total_population*100
usa2$total_cases_perc <- usa2$total_cases/usa2$total_population*100
usa2$mortality_rate <- usa2$total_death/usa2$total_cases
usa2 <- tibble::rownames_to_column(usa2, "days_since_first_case")
usa2$days_since_first_case <- as.numeric(usa2$days_since_first_case)

#regression grouped
negative<-simulation[simulation$death_pop_stat=='Negative',]
positive<-simulation[simulation$death_pop_stat=='Positive',]
regression <- cbind(as.data.frame(tapply(negative$tot_death, negative$days_since_first_case, sum)),
                    tapply(negative$tot_cases, negative$days_since_first_case, sum),rep(c('Negative'),each=271))
colnames(regression)<-c('total_death','total_cases','regression')
regression <- tibble::rownames_to_column(regression, "days_since_first_case")
temp <- cbind(as.data.frame(tapply(positive$tot_death, positive$days_since_first_case, sum)),
              tapply(positive$tot_cases, positive$days_since_first_case, sum),rep(c('Positive'),each=271))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
regression <- rbind(regression,temp)
temp <- cbind(as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum)),
              tapply(simulation$tot_cases, simulation$days_since_first_case, sum),rep(c('Composite'),each=271))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
regression <- rbind(regression,temp)
regression$days_since_first_case <- as.numeric(regression$days_since_first_case)


new_death <- regression$total_death
new_cases <- regression$total_cases
for (i in (c(0,271,542))){
  for (c in (1:271)){
    if (c>1){
      index = i+c
      new_death[index]<- regression$total_death[index]-regression$total_death[index-1]
      new_cases[index]<- regression$total_cases[index]-regression$total_cases[index-1]  
    }}}

regression$new_death <- new_death
regression$new_cases <- new_cases
regression$mortality_rate <- regression$total_death/regression$total_cases
regression$new_mortality_rate <- regression$new_death/regression$new_cases
regression$total_death_perc <- regression$total_death/327533774*100
regression$total_cases_perc <- regression$total_cases/327533774*100
regression<-regression[regression$new_death>0,]
regression<-regression[regression$new_cases>0,]
regression$new_death_perc <- regression$new_death/327533774*100
regression$new_cases_perc <- regression$new_cases/327533774*100



#corr4_3_label grouped
initial_spike<-simulation[simulation$corr4_3_label=='Initial_Spike',]
recent_spike<-simulation[simulation$corr4_3_label=='Recent_Spike',]
two_humped<-simulation[simulation$corr4_3_label=='Two_Humped',]
cluster <- cbind(as.data.frame(tapply(initial_spike$tot_death, initial_spike$days_since_first_case, sum)),
                 tapply(initial_spike$tot_cases, initial_spike$days_since_first_case, sum),rep(c('Initial_Spike'),each=271))
colnames(cluster)<-c('total_death','total_cases','correlation_cluster')
cluster <- tibble::rownames_to_column(cluster, "days_since_first_case")
temp <- cbind(as.data.frame(tapply(recent_spike$tot_death, recent_spike$days_since_first_case, sum)),
              tapply(recent_spike$tot_cases, recent_spike$days_since_first_case, sum),rep(c('recent_Spike'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
temp <- cbind(as.data.frame(tapply(two_humped$tot_death, two_humped$days_since_first_case, sum)),
              tapply(two_humped$tot_cases, two_humped$days_since_first_case, sum),rep(c('Two_humped'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
temp <- cbind(as.data.frame(tapply(simulation$tot_death, simulation$days_since_first_case, sum)),
              tapply(simulation$tot_cases, simulation$days_since_first_case, sum),rep(c('Composite'),each=271))
colnames(temp)<-c('total_death','total_cases','correlation_cluster')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
cluster <- rbind(cluster,temp)
cluster$days_since_first_case <- as.numeric(cluster$days_since_first_case)


new_death <- cluster$total_death
new_cases <- cluster$total_cases
for (i in (c(0,271,542,813))){
  for (c in (1:271)){
    if (c>1){
      index = i+c
      new_death[index]<- cluster$total_death[index]-cluster$total_death[index-1]
      new_cases[index]<- cluster$total_cases[index]-cluster$total_cases[index-1]  
    }}}

cluster$new_death <- new_death
cluster$new_cases <- new_cases
cluster$mortality_rate <- cluster$total_death/cluster$total_cases
cluster$new_mortality_rate <- cluster$new_death/cluster$new_cases
cluster$total_death_perc <- cluster$total_death/327533774*100
cluster$total_cases_perc <- cluster$total_cases/327533774*100
cluster<-cluster[cluster$new_death>0,]
cluster<-cluster[cluster$new_cases>0,]
cluster$new_death_perc <- cluster$new_death/327533774*100
cluster$new_cases_perc <- cluster$new_cases/327533774*100

remove(new_cases, new_death, index, i, c, initial_spike, 
       recent_spike, two_humped, test, temp, monthly_test,
       monthly_test2, monthly, month, negative, positive, data)


#Graphing Figures for main data ----

#Figure1 - Cumulative Cases
ggplot(usa, aes(x=submission_date, y=total_cases_perc)) +
  geom_line() + labs(title = "Cumulative COVID-19 Cases Percentage in the United States", 
                     y = "COVID-19 Cases Percentage", x="Calendar Dates")+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 2 - Cumulative Deaths
ggplot(usa, aes(x=submission_date, y=total_death_perc)) +
  geom_line() + labs(title = "Cumulative COVID-19 Deaths Percentage in the United States", 
                     y = "COVID-19 Deaths Percentage", x="Calendar Dates")+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 3 - Cumulative Deaths (Days since first case)
ggplot(usa2, aes(x=days_since_first_case, y=total_death_perc)) +
  geom_line() + labs(title = "Cumulative COVID-19 Deaths Percentage in the United States", 
                     y = "COVID-19 Deaths Percentage", x="Days Since First Case (Daily Submission)")+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 4 - Cumulative Death (Black)
ggplot(simulation, aes(x=days_since_first_case, y=death_perc_by_population, group=Name)) +
  geom_line()+ labs(title = "COVID-19 Deaths Percentage Trends in each State", y="COVID-19 Deaths Percentage",
                    x="Days Since First Case (Daily Submission)",color="2nd order polynomial") +scale_color_manual(values=c("#00BFC4","#F8766D")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 5 - Cumulative Death (death_pop_stat colorized)
ggplot(simulation, aes(x=days_since_first_case, y=death_perc_by_population, group=Name,color=death_pop_stat)) +
  geom_line()+ labs(title = "COVID-19 Deaths Percentage Trends in each State (clustered)", y="COVID-19 Deaths Percentage",
                    x="Days Since First Case (Daily Submission)",color="2nd order polynomial") +scale_color_manual(values=c("#00BFC4","#F8766D")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 6 - Cumulative Cases (death_pop_stat colorized)
ggplot(simulation, aes(x=days_since_first_case, y=cases_perc, group=Name,color=death_pop_stat)) +
  geom_line()+ labs(title = "COVID-19 Cases Percentage Trends in each State (clustered)", y="COVID-19 Cases Percentage",
                    x="Days Since First Case (Daily Submission)",color="2nd order polynomial") +scale_color_manual(values=c("#00BFC4","#F8766D")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 7 - New Death Distribution
ggplot(data=simulation, aes(x=days_since_first_case, y=daily_death_pop, group = Name, color = death_pop_stat)) +
  geom_line()+labs(title ='Distribution of Daily New COVID-19 Deaths (clustered)',x="Days Since First Case (Daily Submission)",
                   y="Daily New COVID-19 Deaths Percentage",color="2nd order polynomial") +scale_color_manual(values=c("#00BFC4","#F8766D")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 8 - regression grouped
#1-death_perc
ggplot(regression, aes(x=days_since_first_case, y=total_death_perc, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative COVID-19 Deaths Percentage Trend in the United States", 
                     y = "COVID-19 Deaths Percentage", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#2-cases_perc
ggplot(regression, aes(x=days_since_first_case, y=total_cases_perc, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative COVID-19 Cases Percentage Trend in the United States", 
                     y = "COVID-19 Cases Percentage", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#3-new_cases
ggplot(regression, aes(x=days_since_first_case, y=new_cases_perc, group = regression, color = regression)) +
  geom_line() + labs(title = "New COVID-19 Cases Percentage Trend in the United States", 
                     y = "New COVID-19 Cases Percentage", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#4-new_deaths
ggplot(regression, aes(x=days_since_first_case, y=new_death_perc, group = regression, color = regression)) +
  geom_line() + labs(title = "New COVID-19 Deaths Percentage Trend in the United States", 
                     y = "New COVID-19 Deaths Percentage", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#5-mortality rate
ggplot(regression, aes(x=days_since_first_case, y=mortality_rate, group = regression, color = regression)) +
  geom_line() + labs(title = "COVID-19 Mortality Rate Trend in the United States", 
                     y = "COVID-19 Mortality Rate", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#6-new mortality rate
ggplot(regression, aes(x=days_since_first_case, y=new_mortality_rate, group = regression, color = regression)) +
  geom_line() + labs(title = "New COVID-19 Mortality Rate Trend in the United States", 
                     y = "New COVID-19 Mortality Rate", x="Days Since First Case (Daily Submission)", color = "2nd order polynomial")+
  scale_color_manual(values=c('#9e9e9e','#00BFC4','#F8766D')) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 9 Covid_Monthly (black)
ggplot(covid_monthly, aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)")+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 10 Covid_Monthly (2nd order)
ggplot(covid_monthly, aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=death_pop_stat)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="2nd order polynomial") +scale_color_manual(values=c("#00BFC4","#F8766D")) + 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 11 Covid_Monthly (corr4_3_label)
ggplot(covid_monthly, aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=corr4_3_label)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="Centered Correlation") +scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#Figure 12
#cluster grouped
#1
ggplot(cluster, aes(x=days_since_first_case, y=total_death_perc, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "Cumulative COVID-19 Deaths Percentage Trend in the United States", 
                     y = "COVID-19 Deaths Percentage", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#2
ggplot(cluster, aes(x=days_since_first_case, y=total_cases_perc, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "Cumulative COVID-19 Cases Percentage Trend in the United States", 
                     y = "COVID-19 Cases Percentage", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#3
ggplot(cluster, aes(x=days_since_first_case, y=new_cases_perc, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "New COVID-19 Cases Percentage Trend in the United States", 
                     y = "New COVID-19 Cases Percentage", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#4
ggplot(cluster, aes(x=days_since_first_case, y=new_death_perc, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "New COVID-19 Deaths Percentage Trend in the United States", 
                     y = "New COVID-19 Deaths Percentage", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#5
ggplot(cluster, aes(x=days_since_first_case, y=mortality_rate, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "COVID-19 Mortality Rate Trend in the United States", 
                     y = "COVID-19 Mortality Rate", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#6
ggplot(cluster, aes(x=days_since_first_case, y=new_mortality_rate, group = correlation_cluster, color = correlation_cluster)) +
  geom_line() + labs(title = "New COVID-19 Mortality Rate Trend in the United States", 
                     y = "New COVID-19 Mortality Rate", x="Days Since First Case (Daily Submission)",color="Centered Correlation")+
  scale_color_manual(values=c('#9e9e9e',"#6fcfd1",'#e66155', '#faa134')) 

#Figure 13 Cluster Dendrogram
plot(fits, label=rownames(cluster2), hang=-1,main = 'Cluster Dendrogram - Centered Correlation Distance')
rect.hclust(fits, k=4, border="red")

#Figure 14 Corr4_3_label
#1
ggplot(covid_monthly[covid_monthly$corr4_label==1,], aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=STATENAME)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State - Cluster 1", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="Centered Correlation")

#2
ggplot(covid_monthly[covid_monthly$corr4_label==2,], aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=STATENAME)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State - Cluster 2", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="Centered Correlation")

#3
ggplot(covid_monthly[covid_monthly$corr4_label==3,], aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=STATENAME)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State - Cluster 3", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="Centered Correlation")

#4
ggplot(covid_monthly[covid_monthly$corr4_label==4,], aes(x=days_since_first_case, y=monthly_death_pop, group=STATENAME,color=STATENAME)) +
  geom_line()+ labs(title = "Monthly New COVID-19 Deaths Percentage Trends by State - Cluster 4", y="Monthly New COVID-19 Deaths Percentage",
                    x="Days Since First Case (Monthly Submission)",color="Centered Correlation")

newest <- ddply(covid_monthly, .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 

#Box Chart
boxplot(cases_perc~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Cases Percentage (%)", main = "Cases Percentage by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))
boxplot(death_perc~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Mortality Rate (%)", main = "Mortality Rate by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))
boxplot(death_perc_by_population~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Death Percentage", main = "Death Percentage by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))
boxplot(POPPCT_URBAN~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Urban Population Percentage (%)", main = "Urban Population Percentage by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))
boxplot(AREAPCT_URBAN~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Urban Area Percentage (%)", main = "Urban Area Percentage by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))

boxplot(POPDEN_URBAN~as.factor(corr4_3_label), data = newest, xlab = "Correlation Clustered Groups", 
        ylab = "Urban Area Percentage (%)", main = "Urban Area Percentage by Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))


#Figure 15 Correlation Cluster Map US

all_states <- map_data("state")
newest$STATENAME <- tolower(newest$STATENAME)
temp <- newest[,c(1,8,9,10,62,66)]
dc <- as.data.frame(t(as.data.frame(as.vector(rep(0,6)))))
dc[c(1,6)]<-c('district of columbia','Initial_Spike')
colnames(dc)<-colnames(temp)
temp <- rbind(temp,dc)
all_states <- merge(all_states, temp, by.x="region", by.y="STATENAME")
p <- ggplot(data = all_states,
            aes(x = long, y = lat,
                group = group, fill = as.factor(corr4_3_label),
                text = paste0(region,'<br>Cases Perc:', cases_perc, 
                              '<br>Death Perc_pop:', death_perc_by_population,
                              '<br>Death Perc:', death_perc,
                              '<br>death_pop_stat:', death_pop_stat)))+ geom_polygon(color = "gray90", size = 0.1)  + 
  scale_fill_manual(values = c("#6fcfd1",'#e66155', '#faa134'))+
  labs(title = "Centered Correlation Cluster US Map", fill = "Centered Correlation")
p
p <- plotly::ggplotly(p, tooltip="text")
p

table(newest$corr4_3_label,newest$death_pop_stat)

key1 <- highlight_key(simulation, ~Name)
base <- plot_ly(key1) 

base %>%
  group_by(Name) %>%
  add_lines(x = ~days_since_first_case, y = ~death_perc_by_population, color=~death_pop_stat, colors=~
              c('#F8766D','#00BFC4'),
            text = ~paste("State:", Name, '<br>Death_Perc_by_Poulation:', death_perc_by_population))%>%
  layout(title = 'Cumulative COVID-19 Deaths Percentage Trend by State',
         xaxis = list(title="Days Since First Case (Daily Submission)"),
         yaxis = list(title="Cumulative COVID-19 Deaths Percentage"))%>%
  highlight(on = "plotly_click", dynamic = TRUE, selectize = TRUE)
newest$death_perc_by_population_rank <- rank(newest$death_perc_by_population)
key1 <- highlight_key(covid_monthly, ~STATENAME )
base <- plot_ly(key1) 

base %>%
  group_by(STATENAME) %>%
  add_lines(x = ~days_since_first_case, y = ~monthly_death_pop, color=~corr4_3_label,colors = c("#6fcfd1",'#e66155', '#faa134'),
            text = ~paste("State:", STATENAME, '<br>Urban_perc_label:'))%>%
  layout(title = 'Monthly New COVID-19 Deaths Percentage By State (Clustered)',
         xaxis = list(title="Days Since First Case (Monthly Submission)"),
         yaxis = list(title="Monthly New COVID-19 Deaths Percentage"))%>%
  highlight(on = "plotly_click", dynamic = TRUE, selectize = TRUE)


#for corr4_label
base %>%
  group_by(STATENAME) %>%
  add_lines(x = ~days_since_first_case, y = ~monthly_death_pop, color=~corr4_3_label,
            text = ~paste("State:", STATENAME, '<br>Urban_perc_label:'))%>%
  layout(title = 'Monthly New COVID-19 Deaths Percentage By State (Clustered)',
         xaxis = list(title="Days Since First Case (Monthly Submission)"),
         yaxis = list(title="Monthly New COVID-19 Deaths Percentage"))%>%
  highlight(on = "plotly_click", dynamic = TRUE, selectize = TRUE)

#Some relevant plots for Introduction
ggplot(covid_monthly[covid_monthly$STATENAME=='Connecticut' |covid_monthly$STATENAME=='South Carolina',], aes(x=days_since_first_case, y=death_perc_by_population, color=STATENAME)) +
  geom_line() + labs(title = 'Monthly New COVID-19 Deaths Percentage Trend', 
                     y = "Monthly New COVID-19 Deaths Percentage", x='Days Since First Case', 
                     color = 'State Name')+scale_color_manual(values=c("#00BFC4","#F8766D"))

ggplot(covid_monthly[covid_monthly$STATENAME=='Connecticut' |covid_monthly$STATENAME=='South Carolina',], aes(x=days_since_first_case, y=monthly_death_pop, color=STATENAME)) +
  geom_line() + labs(title = 'Monthly New COVID-19 Deaths Percentage Trend', 
                     y = "Monthly New COVID-19 Deaths Percentage", x='Days Since First Case', 
                     color = 'State Name')+geom_line(aes(x = quantile(days_since_first_case, 0.48)), color = "black", linetype = "dashed")+scale_color_manual(values=c("#00BFC4","#F8766D"))

ggplot(newest, aes(x=start_date,  fill = corr4_3_label)) +
  geom_histogram(position='identity')+ labs(title = "Start Date Distribution", y="Counts",
                                            x="Start Dates") +scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+
  scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+theme(legend.position = 'top')
view <- as.data.frame(cbind(c('Initial Spike', 'Two Humped', 'Recent Spike'),c(1,1,1)))
ggplot(view, aes(x=V2,  fill = V1)) +
  geom_histogram(stat='count')+ labs(title = "Correlation Cluster Colors", fill='Correlation Clusters',
                                     x="Colors") +scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+
  scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+theme(legend.position = 'top')



newest <- ddply(covid_monthly, .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 
mean_cases <- as.data.frame(tapply(newest$cases_perc, newest$corr4_3_label, mean))
median_cases <- as.data.frame(tapply(newest$cases_perc, newest$corr4_3_label, median))
mean_death_pop <- as.data.frame(tapply(newest$death_perc_by_population, newest$corr4_3_label, mean))
median_death_pop <- as.data.frame(tapply(newest$death_perc_by_population, newest$corr4_3_label, median))
mean_death_perc <- as.data.frame(tapply(newest$death_perc, newest$corr4_3_label, mean))
median_death_perc <- as.data.frame(tapply(newest$death_perc, newest$corr4_3_label, median))
current_cases_perc <- as.data.frame(tapply(newest$cases_perc, newest$corr4_3_label, max))
current_death_perc <- as.data.frame(tapply(newest$death_perc, newest$corr4_3_label, max))
current_death_pop <- as.data.frame(tapply(newest$death_perc_by_population, newest$corr4_3_label, max))
urban_area_perc <- as.data.frame(tapply(newest$AREAPCT_URBAN, newest$corr4_3_label, mean))
urban_pop_perc <- as.data.frame(tapply(newest$POPPCT_URBAN, newest$corr4_3_label, mean))


names(mean_cases) <- names(median_cases) <- names(mean_death_pop) <- 
  names(median_death_pop)<- names(mean_death_perc) <- names(median_death_perc)<- 
  names(current_cases_perc)<- names(current_death_perc)<- names(current_death_pop) <- names(urban_area_perc)<-
  names(urban_pop_perc)
mean_median <- cbind(mean_cases,median_cases,mean_death_pop,median_death_pop,mean_death_perc,median_death_perc,
                     current_cases_perc,current_death_perc,current_death_pop,urban_area_perc,urban_pop_perc)
colnames(mean_median) <- c("mean_cases","median_cases","mean_death_pop","median_death_pop","mean_death_perc",
                           "median_death_perc","current_cases_perc","current_death_perc","current_death_pop",
                           "urban_area_perc","urban_pop_perc")
mean_median <- tibble::rownames_to_column(mean_median, "corr4_3_label")
remove(mean_cases)
remove(median_cases)
remove(mean_death_pop)
remove(median_death_pop)
remove(mean_death_perc)
remove(current_cases_perc)
remove(current_death_perc)
remove(current_death_pop)
remove(urban_area_perc)
remove(urban_pop_perc)

#Bar Chart
ggplot(mean_median,aes(x=as.factor(corr4_3_label), y = mean_cases)) +
  geom_bar(aes(fill = as.factor(corr4_3_label)),stat='identity')+
  labs(x='Cluster Groups', y='Mean Cumulative Cases Percentage', title = 'Mean Cumulative Cases Percentage by Clusters',
       fill = 'Cluster Groups')+scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))
ggplot(mean_median,aes(x=as.factor(corr4_3_label), y = mean_death_perc)) +
  geom_bar(aes(fill = as.factor(corr4_3_label)),stat='identity')+
  labs(x='Cluster Groups', y='Mean Mortality Rate', fill = 'Cluster Groups', 
       title = 'Mean Cumulative Mortality Rate by Clusters')+
  scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))
ggplot(mean_median,aes(x=as.factor(corr4_3_label), y = mean_death_pop)) +
  geom_bar(aes(fill = as.factor(corr4_3_label)),stat='identity')+
  labs(x='Cluster Groups', y='Mean Cumulative Deaths Percentage', fill = 'Cluster Groups', 
       title = 'Mean Cumulative Deaths Percentage by Clusters')+
  scale_fill_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))


ggplot(mean_median,aes(x=as.factor(corr4_3_label), y = mean_cases)) +
  geom_bar(aes(fill = as.factor(corr4_3_label)),stat='identity')

negative<-covid_monthly[covid_monthly$death_pop_stat=='Negative',]
positive<-covid_monthly[covid_monthly$death_pop_stat=='Positive',]
usa <- cbind(as.data.frame(tapply(negative$tot_death, negative$days_since_first_case, sum)),
             tapply(negative$tot_cases, negative$days_since_first_case, sum),rep(c('negative'),each=10))
colnames(usa)<-c('total_death','total_cases','regression')
usa <- tibble::rownames_to_column(usa, "days_since_first_case")
temp <- cbind(as.data.frame(tapply(positive$tot_death, positive$days_since_first_case, sum)),
              tapply(positive$tot_cases, positive$days_since_first_case, sum),rep(c('positive'),each=10))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
usa <- rbind(usa,temp)
temp <- cbind(as.data.frame(tapply(covid_monthly$tot_death, covid_monthly$days_since_first_case, sum)),
              tapply(covid_monthly$tot_cases, covid_monthly$days_since_first_case, sum),rep(c('composite'),each=10))
colnames(temp)<-c('total_death','total_cases','regression')
temp <- tibble::rownames_to_column(temp, "days_since_first_case")
usa <- rbind(usa,temp)

usa<-cbind(usa,rep(c(327533774),each=30))
colnames(usa)<-c('days_since_first_case','total_death','total_cases','regression','total_population')
usa$days_since_first_case <- as.numeric(usa$days_since_first_case)

#usa$total_death_perc <- usa$total_death/usa$total_population*100
#usa$total_cases_perc <- usa$total_cases/usa$total_population*100
#usa$mortality_rate <- usa$total_death/usa$total_cases
new_death <- rep(0,each=30)
new_cases <- rep(0,each=30)
for (i in (c(0,10,20))){
  for (c in (1:10)){
    if (c>1){
      index = i+c
      new_death[index]<- usa$total_death[index]-usa$total_death[index-1]
      new_cases[index]<- usa$total_cases[index]-usa$total_cases[index-1]  
    }}}

usa$new_death <- new_death
usa$new_cases <- new_cases

ggplot(usa, aes(x=days_since_first_case, y=total_death, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative Death Percentage By Population in the United States", 
                     y = "Total Death/Total Population", x="Days Since First Case (Daily Submission)")+
  scale_color_manual(values=c('#000000','#00BFC4','#F8766D')) 

ggplot(usa, aes(x=days_since_first_case, y=total_cases, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative Death Percentage By Population in the United States", 
                     y = "Total Death/Total Population", x="Days Since First Case (Daily Submission)")+
  scale_color_manual(values=c('#000000','#00BFC4','#F8766D')) 

ggplot(usa, aes(x=days_since_first_case, y=new_cases, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative Death Percentage By Population in the United States", 
                     y = "Total Death/Total Population", x="Days Since First Case (Daily Submission)")+
  scale_color_manual(values=c('#000000','#00BFC4','#F8766D')) 

ggplot(usa, aes(x=days_since_first_case, y=new_death, group = regression, color = regression)) +
  geom_line() + labs(title = "Cumulative Death Percentage By Population in the United States", 
                     y = "Total Death/Total Population", x="Days Since First Case (Daily Submission)")+
  scale_color_manual(values=c('#000000','#00BFC4','#F8766D')) 

# Code for Entropy--DAYS SINCE FIRST CASE  ----
county <- read.csv('PctUrbanRural_County.csv', header=TRUE)
county2<-county%>%
  filter(STATENAME!='District of Columbia' & STATENAME!='Puerto Rico')
newest <- ddply(covid_monthly, .var = "STATENAME", .fun = function(x) {
  return(subset(x, days_since_first_case %in% max(days_since_first_case)))
}
) 
test_data_25 <- data_adjusted%>%
  filter(tot_death > 10)
test_data_25 <- ddply(test_data_25[c(1,2)], .var = "Name", .fun = function(x) {
  return(subset(x, submission_date %in% min(submission_date)))
}
)
names(test_data_25) <- c("Name", "First_benchmark_date")
newest <- merge(newest, test_data_25, by.x="STATENAME", by.y="Name")
state_info <- newest
#C ADJUSTED
adjusted_entropy <- as.data.frame(unique(county2$STATENAME))
colnames(adjusted_entropy)<-'State'
index <- c(9,14)
for (num in index){
  data5 <- c(0)
  for (name in unique(county2$STATENAME)){
    hold <- 0
    temp <- county2[county2$STATENAME==name,]
    for (i in 1:nrow(temp)){
      if (temp[i,num] == 0){
        next
      }       
      c = temp[i,num]/sum(temp$AREA_COU)
      product = -c*log(c)
      hold <- cbind(hold,product)    
    } 
    c = sum(temp[,num])/sum(temp$AREA_COU)
    hold <- cbind(sum(hold)/c-log(sum(temp$AREA_COU)))
    data5 <- rbind(data5, hold)
  }
  data5 <- as.data.frame(data5[-c(1),])
  rownames(data5)<-NULL
  colnames(data5) <- c(paste('Final_Entropy_',colnames(county[num]),sep=''))
  data5 <- sapply(data5, as.numeric)
  adjusted_entropy <- cbind(adjusted_entropy,data5)
}


state_info <- merge(state_info, adjusted_entropy[,c(1,2,3)], by.x='STATENAME',by.y='State')

#C ADJUSTED
index <- c(7,12)
for (num in index){
  data5 <- c(0)
  for (name in unique(county2$STATENAME)){
    hold <- 0
    temp <- county2[county2$STATENAME==name,]
    for (i in 1:nrow(temp)){
      if (temp[i,num] == 0){
        next
      }       
      c = temp[i,num]/sum(temp$POP_COU)
      product = -c*log(c)
      hold <- cbind(hold,product)    
    } 
    c = sum(temp[,num])/sum(temp$POP_COU)
    hold <- cbind(sum(hold)/c-log(sum(temp$POP_COU)))
    data5 <- rbind(data5, hold)
  }
  data5 <- as.data.frame(data5[-c(1),])
  rownames(data5)<-NULL
  colnames(data5) <- c(paste('Final_Entropy_',colnames(county[num]),sep=''))
  data5 <- sapply(data5, as.numeric)
  adjusted_entropy <- cbind(adjusted_entropy,data5)
}

remove(product,num,name,index,i,c,data5,hold,county,temp)
state_info <- merge(state_info, adjusted_entropy[,c(1,4,5)], by.x='STATENAME',by.y='State')
state_info$start_date <- as.Date(state_info$start_date)
state_info$start_day <- as.numeric(state_info$start_date - as.Date('2020-01-22'))
state_info$bench_day <- as.numeric(state_info$First_benchmark_date - as.Date('2020-01-22'))

#Graphing figures for Entropy 
library(car)

#AREA
#Scatter Plot
plot(log(death_perc)~Final_Entropy_AREA_URBAN, data = state_info, xlab ='Urban Area Entropy',
     ylab ='log(Mortality Rate)', main = 'Mortality Rate vs Urban Area Entropy')
plot(log(death_perc_by_population)~Final_Entropy_AREA_URBAN, data = state_info, xlab ='Urban Area Entropy',
     ylab ='log(Death Percentage)', main = 'Death Percentage vs Urban Area Entropy')
plot(cases_perc~Final_Entropy_AREA_URBAN, data = state_info, xlab ='Urban Area Entropy',
     ylab ='Cases Percentage', main = 'Cases Percentage vs Urban Area Entropy')
plot(log(bench_day)~Final_Entropy_AREA_URBAN, data = state_info, xlab ='Urban Area Entropy',
     ylab ='log(Days To 10 Deaths)', main = 'COVID-19 timing vs Urban Area Entropy')
boxplot(Final_Entropy_AREA_URBAN~corr4_3_label, data = state_info, xlab = "Correlation Clustered Groups", 
        ylab = "Urban Area Entropy", main = "Urban Area Entropy In Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))
boxplot(Final_Entropy_AREA_URBAN~as.factor(corr4_label), data = state_info, col = c('#e66155', '#faa134',"#6fcfd1",'#3f63e8'), 
          xlab = "Correlation Clustered Groups", 
        ylab = "Urban Area Entropy", main = "Urban Area Entropy In Clusters")

#Histogram Plot
hist(log(state_info$death_perc))
hist(log(state_info$death_perc_by_population))
hist(state_info$cases_perc)
hist(log(state_info$bench_day))
hist(state_info$Final_Entropy_AREA_URBAN)
hist(state_info$Final_Entropy_POP_URBAN)

#Linear Modeland ANOVA tests
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc), data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc), method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc), method = c("spearman"))
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc_by_population), data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc_by_population), method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$death_perc_by_population), method = c("spearman"))
summary(lm(Final_Entropy_AREA_URBAN~cases_perc, data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, state_info$cases_perc, method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, state_info$cases_perc, method = c("spearman"))
summary(lm(Final_Entropy_AREA_URBAN~log(bench_day), data = state_info))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$bench_day), method = c("pearson"))
cor.test(state_info$Final_Entropy_AREA_URBAN, log(state_info$bench_day), method = c("spearman"))
my_anova <- lm(Final_Entropy_AREA_URBAN~corr4_3_label, data=state_info)
Anova(my_anova)
summary(lm(Final_Entropy_AREA_URBAN~corr4_3_label, data=state_info))

#Interactions between variables
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc)*log(death_perc_by_population), data = state_info))
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc)*log(cases_perc), data = state_info))
summary(lm(Final_Entropy_AREA_URBAN~log(death_perc_by_population)*log(cases_perc), data = state_info))
summary(lm(Final_Entropy_AREA_URBAN~log(bench_day)*log(death_perc), data = state_info))
summary(lm(Final_Entropy_AREA_URBAN~log(bench_day)*log(death_perc_by_population), data = state_info))
summary(lm(Final_Entropy_AREA_URBAN~log(bench_day)*log(cases_perc), data = state_info))

#Scatter Plot with color
ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=log(death_perc), color=corr4_3_label)) +
  geom_point() + labs(title = "Mortality Rate vs Urban Area Entropy", 
                      y = "log(Mortality Rate)", x='Urban Area Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

#corr4 Scatter Plot with color
ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=log(death_perc), color=as.factor(corr4_label))) +
  geom_point() + labs(title = "Mortality Rate vs Urban Area Entropy", 
                      y = "log(Mortality Rate)", x='Urban Area Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c('#e66155', '#faa134',"#6fcfd1",'#3f63e8'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=cases_perc, color=corr4_3_label)) +
  geom_point() + labs(title = "Cases Percentage vs Urban Area Entropy", 
                      y = "Cases Percentage", x='Urban Area Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggplot(state_info, aes(x=Final_Entropy_AREA_URBAN, y=log(bench_day), color=corr4_3_label)) +
  geom_point() + labs(title = "COVID-19 timing vs Urban Area Entropy", 
                      y = "log(Days To 10 Deaths)", x='Urban Area Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))

#POP
#Scatter Plot
plot(log(death_perc)~Final_Entropy_POP_URBAN, data = state_info, xlab ='Urban Population Entropy',
     ylab ='log(Mortality Rate)', main = 'Mortality Rate vs Urban Population Entropy')
plot(log(death_perc_by_population)~Final_Entropy_POP_URBAN, data = state_info, xlab ='Urban Population Entropy',
     ylab ='log(Death Percentage)', main = 'Death Percentage vs Urban Population Entropy')
plot(cases_perc~Final_Entropy_POP_URBAN, data = state_info, xlab ='Urban Population Entropy',
     ylab ='Cases Percentage', main = 'Cases Percentage vs Urban Population Entropy')
plot(log(bench_day)~Final_Entropy_POP_URBAN, data = state_info, xlab ='Urban Population Entropy',
     ylab ='log(Days To 10 Deaths)', main = 'COVID-19 timing vs Urban Population Entropy')
boxplot(Final_Entropy_POP_URBAN~corr4_3_label, data = state_info, xlab = "Correlation Clustered Groups", 
        ylab = "Urban Population Entropy", main = "Urban Population Entropy In Clusters",col=c("#6fcfd1",'#e66155', '#faa134'))

#Histogram Plot
hist(log(state_info$death_perc))
hist(log(state_info$death_perc_by_population))
hist(state_info$cases_perc)
hist(log(state_info$bench_day))
hist(state_info$Final_Entropy_POP_URBAN)
hist(state_info$Final_Entropy_POP_URBAN)

#Linear Modeland ANOVA tests
summary(lm(Final_Entropy_POP_URBAN~log(death_perc), data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc), method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc), method = c("spearman"))
summary(lm(Final_Entropy_POP_URBAN~log(death_perc_by_population), data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc_by_population), method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$death_perc_by_population), method = c("spearman"))
summary(lm(Final_Entropy_POP_URBAN~cases_perc, data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, state_info$cases_perc, method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, state_info$cases_perc, method = c("spearman"))
summary(lm(Final_Entropy_POP_URBAN~log(bench_day), data = state_info))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$bench_day), method = c("pearson"))
cor.test(state_info$Final_Entropy_POP_URBAN, log(state_info$bench_day), method = c("spearman"))
my_anova <- lm(Final_Entropy_POP_URBAN~corr4_3_label, data=state_info)
Anova(my_anova)
summary(lm(Final_Entropy_POP_URBAN~corr4_3_label, data=state_info))

#Interactions between variables
summary(lm(Final_Entropy_POP_URBAN~log(death_perc)*log(death_perc_by_population), data = state_info))
summary(lm(Final_Entropy_POP_URBAN~log(death_perc)*log(cases_perc), data = state_info))
summary(lm(Final_Entropy_POP_URBAN~log(death_perc_by_population)*log(cases_perc), data = state_info))
summary(lm(Final_Entropy_POP_URBAN~log(bench_day)*log(death_perc), data = state_info))
summary(lm(Final_Entropy_POP_URBAN~log(bench_day)*log(death_perc_by_population), data = state_info))
summary(lm(Final_Entropy_POP_URBAN~log(bench_day)*log(cases_perc), data = state_info))

#Scatter Plot with color
ggplot(state_info, aes(x=Final_Entropy_POP_URBAN, y=log(death_perc), color=corr4_3_label)) +
  geom_point() + labs(title = "Mortality Rate vs Urban Population Entropy", 
                      y = "log(Mortality Rate)", x='Urban Population Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggplot(state_info, aes(x=Final_Entropy_POP_URBAN, y=cases_perc, color=corr4_3_label)) +
  geom_point() + labs(title = "Cases Percentage vs Urban Population Entropy", 
                      y = "Cases Percentage", x='Urban Population Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))+ 
  scale_y_continuous(labels = function(x) paste0(x, "%"))

ggplot(state_info, aes(x=Final_Entropy_POP_URBAN, y=log(bench_day), color=corr4_3_label)) +
  geom_point() + labs(title = "COVID-19 timing vs Urban Population Entropy", 
                      y = "log(Days To 10 Deaths)", x='Urban Population Entropy', color='Correlation Cluster')+
  scale_color_manual(values=c("#6fcfd1",'#e66155', '#faa134'))

library(plotly)
key1 <- highlight_key(state_info, ~STATENAME )
base <- plot_ly(key1) 

base %>%
  group_by(STATENAME) %>%
  add_markers(x = ~Final_Entropy_AREA_URBAN, y = ~log(death_perc), color=~death_pop_stat,colors = c("#6fcfd1",'#e66155', '#faa134'),
            text = ~paste("State:", STATENAME, '<br>Urban_perc_label:'))%>%
  layout(title = 'Mortality Rate vs Urban Area Entropy',
         xaxis = list(title="Urban Area Entropy"),
         yaxis = list(title="Mortality Rate"))%>%
  highlight(on = "plotly_click", dynamic = TRUE, selectize = TRUE)











