library(dplyr)

library(hflights)
data("hflights")
df=hflights
df.tbl = tbl_df(df)
glimpse(df.tbl)

#variable
#select, remove columns
#mutate, 

#observations
#filter, remove rows
#arrange, order


#summarize

h1=hflights %>%
  select(ActualElapsedTime,AirTime,ArrDelay,DepDelay)


h2=hflights%>%
  select(starts_with("A"))

dim(h2)


#starts_with("X"): every name that starts with "X",
#ends_with("X"): every name that ends with "X",
#contains("X"): every name that contains "X",
#matches("X"): every name that matches "X", where "X" can be a regular expression,
#num_range("x", 1:5): the variables named x01, x02, x03, x04 and x05,
#one_of(x): every name that appears in x, which should be a character vector.

# Print out a tbl containing just ArrDelay and DepDelay
hflights %>%
  select(ArrDelay, DepDelay)


# Print out a tbl as described in the second instruction, using both helper functions and variable names
hflights %>%
  select(ends_with("Num"), contains("Carrier") ,starts_with("Cancell"))

# Print out a tbl as described in the third instruction, using only helper functions.
hflights %>%
  select(ends_with("Time"), ends_with("Delay"))


# hflights and dplyr are loaded and ready to serve you.

names(hflights)

# Add the new variable ActualGroundTime to a copy of hflights and save the result as g1.
g1= hflights %>% 
  mutate(ActualGroundTime = ActualElapsedTime - AirTime )

# Add the new variable GroundTime to g1. Save the result as g2.
g2 =g1 %>% 
  mutate(GroundTime = TaxiIn+TaxiOut )

# Add the new variable AverageSpeed to g2. Save the result as g3.
g3 =g2 %>%
  mutate(AverageSpeed =Distance/AirTime *60)

# Print out g3
g3



# Select the flights that had JFK as their destination: c1
c1 = hflights%>%
  filter(Dest == "JFK")

# Combine the Year, Month and DayofMonth variables to create a Date column: c2
c2 = c1%>%
  mutate(Date = paste(Year,Month,DayofMonth,sep = "-"))

# Print out a selection of columns of c2
c2%>%
  select(Date, DepTime, ArrTime, TailNum)


#How many weekend flights flew a distance of more than 
#1000 miles but had a total taxiing time below 15 minutes?

class(hflights$DayOfWeek)


names(hflights)
hflights$DayOfWeek
gg=hflights %>%
  mutate(taxall = TaxiIn + TaxiOut)  %>%
  filter(Distance>1000, taxall <15, DayOfWeek %in% c(7,1) )

dim(gg)

#arrange() can be used to rearrange rows 
#according to any type of data. 
#If you pass arrange() a character variable, 
#for example, R will rearrange the rows in alphabetical 
#order according to values of the variable. 
#If you pass a factor variable, R will rearrange the 
#rows according to the order of the levels in your factor 
#(running levels() on the variable reveals this order).

# Definition of dtc
dtc <- filter(hflights, Cancelled == 1, !is.na(DepDelay))

# Arrange dtc by departure delays
dtc%>%
  arrange(DepDelay)

# Arrange dtc so that cancellation reasons are grouped
ca=dtc%>%
  group_by(CancellationCode) %>%
arrange(DepDelay)
glimpse(ca)
head(ca,50)

# Arrange dtc according to carrier and departure delays
dtc%>%
  group_by(UniqueCarrier)%>%
  arrange(DepDelay)

# Arrange according to carrier and decreasing departure delays

arrange(hflights,UniqueCarrier, desc( DepDelay))

# Arrange flights by total delay (normal order).

arrange(hflights, ArrDelay+DepDelay)

# Print out a summary with variables min_dist and max_dist
hflights %>%
  summarise(min_dist=min(Distance),
            max_dist= max(Distance))


# Print out a summary with variable max_div
hflights %>%
  filter(Diverted ==1) %>%
  summarise(max_div = max(Distance))

#You can use any function you like in summarise() 
#so long as the function can take a vector of data and return a single number. 
#R contains many aggregating functions, as dplyr calls them:

#min(x) - minimum value of vector x.
#max(x) - maximum value of vector x.
#mean(x) - mean value of vector x.
#median(x) - median value of vector x.
#quantile(x, p) - pth quantile of vector x.
#sd(x) - standard deviation of vector x.
#var(x) - variance of vector x.
#IQR(x) - Inter Quartile Range (IQR) of vector x.
#diff(range(x)) - total range of vector x.

# Remove rows that have NA ArrDelay: temp1
temp1 = hflights%>%
  filter(!is.na(ArrDelay))


# Generate summary about ArrDelay column of temp1
temp1 %>%
  summarize(average = mean(ArrDelay),
            sd = sd(ArrDelay),
            latest = max(ArrDelay),
            earliest= min(ArrDelay))


# Keep rows that have no NA TaxiIn and no NA TaxiOut: temp2
temp2 = hflights %>%
  filter(!is.na(TaxiIn)) %>%
  filter(!is.na(TaxiOut))

# Print the maximum taxiing difference of temp2 with summarise()
summarize(temp2, max_taxi_diff= max(abs(TaxiIn - TaxiOut)))


#dplyr provides several helpful aggregate functions of its own, 
#in addition to the ones that are already defined in R. These include:
  
#first(x) - The first element of vector x.
#last(x) - The last element of vector x.
#nth(x, n) - The nth element of vector x.
#n() - The number of rows in the data.frame or group of observations that summarise() describes.
#n_distinct(x) - The number of unique values in vector x.

# hflights is available with full names for the carriers
names(hflights)
hflights$Cancelled

# Generate summarizing statistics for hflights
summarise(hflights,
          n_obs = n(),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest))

# All American Airline flights
aa <- filter(hflights, UniqueCarrier == "American")

# Generate summarizing statistics for aa 
summarise(aa, 
          n_flights = n(),
          n_canc = sum(Cancelled==1),
          avg_delay = mean(ArrDelay, na.rm=TRUE))

#pipe
c(1,2,3) %>% sum()
c(1,2,3,4) %>% sum(na.rm=TRUE)


# Write the 'piped' version of the English sentences.
hflights %>%
  mutate(diff= TaxiOut-TaxiIn) %>%
  filter(!is.na(diff)) %>%
  summarize(avg=mean(diff))


# Chain together mutate(), filter() and summarise()
names(hflights)

hflights %>%
  mutate(RealTime = ActualElapsedTime +100) %>%
  mutate(mph = Distance/RealTime*60) %>%
  filter(!is.na(mph) & mph<70) %>%
  summarise(n_less=n(),
            n_dest= n_distinct(Dest),
            min_dist= min(Distance),
            max_dist=max(Distance))


# Finish the command with a filter() and summarise() call
names(hflights)
hflights %>%
  mutate( RealTime =ActualElapsedTime+100, mph = Distance / RealTime * 60) %>%
  filter(mph<105 | Cancelled==1 | Diverted==1)  %>%
  summarize(n_non=n(),
            n_dest= n_distinct(Dest),
            min_dist= min(Distance),
            max_dist=max(Distance))


names(hflights)

# Count the number of overnight flights
hflights %>%
  filter(!is.na(DepTime ) & !is.na(ArrTime))  %>%
  filter(ArrTime <DepTime) %>%
  summarize(num= n())

# Make an ordered per-carrier summary of hflights
hflights %>%
  group_by(UniqueCarrier) %>%
  summarise(p_canc = mean(Cancelled==1)*100 ,
            avg_delay =mean(ArrDelay, na.rm=TRUE)) %>%
  arrange(avg_delay, p_canc)

#You can also combine group_by() with mutate(). 
#When you mutate grouped data, mutate() will calculate the new variables independently for each group. 
#This is particularly useful when mutate() uses the rank() function, 
#that calculates within-group rankings. rank() takes a group of values and 
#calculates the rank of each value within the group, e.g.

rank(c(21, 22, 24, 23))


#filter() the hflights tbl to only keep observations for which ArrDelay is not NA and positive.
#Use group_by() on the result to group by UniqueCarrier.
#Next, use summarise() to calculate the average ArrDelay per carrier. Call this summary variable avg.
#Feed the result into a mutate() call: create a new variable, rank, calculated as rank(avg).
#Finally, arrange by this new rank variable

# Ordered overview of average arrival delays per carrier
hflights  %>%
  filter(!is.na(ArrDelay) & ArrDelay>0) %>%
  group_by(UniqueCarrier) %>%
  summarise(avg = mean(ArrDelay)) %>%
  mutate(rank = rank(avg)) %>%
  arrange(rank)


# How many airplanes only flew to one destination?
hflights %>%
  group_by(TailNum) %>%
  summarise(n_dest = n_distinct(Dest)) %>%
  filter(n_dest==1) %>%
  summarise(nplanes = n())

# Find the most visited destination for each carrier
hflights %>%
  group_by(UniqueCarrier, Dest) %>%
  summarise(n=n()) %>%
  mutate(rank =rank(desc(n))) %>%
  filter(rank ==1)