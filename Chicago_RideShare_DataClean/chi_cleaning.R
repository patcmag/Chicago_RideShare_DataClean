library(ggplot2)
library(plyr)
library(jsonlite)

d = read.csv("~/Desktop/Transportation_Network_Providers_-_Trips.csv")
communities = read.csv("~/Downloads/chicago-community-areas.csv")

cdf = t(as.data.frame(communities))
name = as.data.frame(cdf[-1,1:1])
rownames(name) <-NULL
names(name) <- c("Community.Area")
name["Number"] <- seq(1,77)

names(d)
dim(d)
head(d[1:5,],5)

head(name[1:5,],5)
names(name)
dim(name)

#merge rideshare data with commmunity area names 
chidf <- merge(d, name, by.x="Pickup.Community.Area", by.y="Number", all.x=TRUE, sort=FALSE)
names(chidf)[names(chidf)=='Community.Area'] <- 'Pickup.Community.Area.Name'

chidf <- merge(chidf, name, by.x="Dropoff.Community.Area", by.y="Number", all.x=TRUE, sort=FALSE)
names(chidf)[names(chidf)=='Community.Area'] <- 'Dropoff.Community.Area.Name'

chidf <- chidf[c(seq(3,21), 2, 22, 1, 23 )] #arranges the new columns at the end of the DF
names(chidf)
dim(chidf)
head(chidf[1:5,],50)

na_count = sum(is.na(chidf$Dropoff.Community.Area))
na_ratio = na_count/dim(chidf)[1] 
na_ratio
#about 5% of pickup.community.area are NAs; 7% dropoff.community.area are NAs

#hist(ca1$Dropoff.Community.Area) #this works

#separate data by pickup community area, compute metrics, and save as individual JSON
for (i in seq(1,77)) {
  #isolate all records where trip picks up in CA i
  full_ca = chidf[(chidf$Pickup.Community.Area==i & !is.na(chidf$Pickup.Community.Area)),]
  
  #create data frame with most common destination CA number as first column
  ca_metrics <- as.data.frame(head(names(sort(table(full_ca$Dropoff.Community.Area),decreasing=TRUE)),1))
  names(ca_metrics) <- c("Most.Freq.Drop.CA")
  
  #create additional columns with metrics for that CA
  ca_metrics["Most.Freq.Drop.Name"] = head(names(sort(table(full_ca$Dropoff.Community.Area.Name),decreasing=TRUE)),1)
  ca_metrics["SecondMost.Freq.Drop.CA"] = array(head(names(sort(table(full_ca$Dropoff.Community.Area),decreasing=TRUE)),2))[2]
  ca_metrics["SecondMost.Freq.Drop.Name"] = array(head(names(sort(table(full_ca$Dropoff.Community.Area.Name),decreasing=TRUE)),2))[2]
  ca_metrics["AvgFare"] = mean(full_ca$Fare)
  ca_metrics["AvgTripMiles"] = mean(full_ca$Trip.Miles)
  #ca_metrics["AvgTripSeconds"] = mean(full_ca$Trip.Seconds)
  ca_metrics["AvgTripMinutes"] = mean(full_ca$Trip.Seconds/60)
  
  ca_name <- gsub(" ", "_", toString(name[i:i,1]), fixed=TRUE)
  dir_file = paste("~/Desktop/ca_metrics/pickupFrom_",ca_name,"_metrics.json",sep="")
  #write.csv(ca_metrics,file=dir_file,row.names=FALSE)
  
  ca_json = toJSON(ca_metrics,pretty = TRUE)
  write(ca_json,dir_file)
}


#separate data by pickup community area and save as individual csvs
for (i in seq(1,77)) {
  cax = chidf[(chidf$Pickup.Community.Area==i & !is.na(chidf$Pickup.Community.Area)),]
  ca_name = toString(name[i:i,1])
  dir_file = paste("~/Desktop/ca_data/pickupFrom_",ca_name,".csv",sep="")
  write.csv(cax, file=dir_file)
}



