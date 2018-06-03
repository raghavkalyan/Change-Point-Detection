##Set Working Directory
setwd("D:\\Surbana Jurong\\Lift Fault Prediction\\Data")

##Import Raw Data
rawdata<- read.csv("Book1.csv")
rawdata <- na.omit(rawdata)
rawdata<-as.data.frame(rawdata)
Hours <- format(as.POSIXct(strptime(rawdata$createdon,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
Hours <- as.data.frame(Hours)
View(Hours)
rawdata <- cbind(rawdata, Hours)

##-----------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
##Initializing Variables
arr=0
i=1
j=1
splitdata=0

##While Loop to Calculate Mode for every 50 elements
while(i<=length(rawdata$pressure)-1) {
  splitdata<-rawdata$pressure[i:(i+49)]  
  arr[j] = modeest::mfv(splitdata)
  
  i=i+50
  j=j+1
}
##Store Mode Values in an array and write it to a CSV file
arr <- na.omit(arr)
write.csv(arr, 'Book1_Mode.csv')

require(ggplot2)

##---------------------------------------------------------------------------------------------------
vec <- read.csv("Book1_Mode.csv")
#Hours=0
#Hours <- format(as.POSIXct(strptime(rawdata$createdon,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")

#head(vec)
vec<-vec$x
vec <- vec
vec <- as.numeric(vec)

#this finds your trend start/stops
idx <- c(cumsum(rle(abs(diff(vec))>10)$lengths)+1)
idx
vec[idx]

#create new vector of change points:

newVec <- vec[idx]
print(newVec)
# [1] 100317 100432 100441 100983 100986 100767 100755

#(opt.) to ignore the first and last observation as a change point:
#idx <- idx[which(idx!=1 & idx!=length(vec))]

#update new vector if you want the "opt." restrictions applied:
#newVec <- vec[idx]
#print(newVec)
# [1] 100317 100432 100441 100983 100986 100767

#you can split newVec by start/stop change points like this:
start_changepoints <- newVec[c(TRUE,FALSE)]
print(start_changepoints)
# [1] 100317 100441 100986
(start_changepoints)
end_changepoints <- newVec[c(FALSE,TRUE)]
print(end_changepoints)
# [1] 100432 100983 100767
end_changepoints
require(ggplot2)

#preps data for plot
df <- data.frame(vec,trends=NA,cols=NA)
df$trends[idx] <- idx
df$cols[idx] <- c("green","red")

#plot
ggplot(df, aes(x=1:NROW(df),y=vec)) +
  geom_line() +
  geom_point() +
  geom_vline(aes(xintercept=trends, col=cols), 
             lty=2, lwd=1) +
  scale_color_manual(values=na.omit(df$cols),
                     breaks=na.omit(unique(df$cols)),
                     labels=c("Start","End")) +
  xlab("Index") +
  ylab("Value") +
  guides(col=guide_legend("Trend State"))

length(start_changepoints)
idx

##----------------------------------------------------------------------------
# Extracting Timestamp From Rawdata from Mode Data (Trip Start & End Time)
##----------------------------------------------------------------------------
rawtime=0
rawpressure=0
rawcreated=0
rawHours=0
a=0
a=idx*50
k=1
#write.csv(rawdata$Hours, "dEbug1.csv")
while (k<= (length(idx)-1)) {
  while(a[k]>((idx[k]*50)-50) && rawdata$pressure[a[k]]!= vec[idx[k]]) {
    a[k]=a[k]-1
    
  }
  rawtime[k] <- rawdata$timestamp[a[k]]
  rawpressure[k]<- rawdata$pressure[a[k]]
  rawcreated[k] <- rawdata$createdon[a[k]]
  rawHours[k] <- rawdata$Hours[a[k]]
  k=k+1 
  print(rawdata$Hours)
}

new <- cbind(rawtime, rawpressure, rawcreated, rawHours)

write.csv(new, 'Debug.csv')
rawtime=as.data.frame(rawtime)
View(rawdata)
unique(rawdata$Hours)

#rawcreated=as.character(rawcreated)
rawpressure
rawtime
rawcreated
View(as.data.frame(rawHours))
y<-table(unlist(rawHours))
y <- y/2
y
y<-as.integer(y)
y
obs <- which(rawdata$timestamp%in%rawtime$rawtime)
par(mfrow=c(1,1))
#plot(rawdata$pressure[obs],xlim=c(3468,3825)   )

new <- cbind(rawtime, rawpressure, rawcreated, rawHours)

View(new)



#View(new)
plot(new$rawpressure, type = 'l')
rawtime
#View(rawtime)

plot(x=rawdata$timestamp,y=rawdata$pressure, type='l',xlim=c(524675836,527089536))

x=new$rawtime[1:nrow(new)]
y=new$rawpressure[1:nrow(new)]

points(x,y,col='red',type='p')

##-------------------------------------------------------------------------------------------------

##-----------------------------------------------------------------------------------------
##-----------------------------------------------------------------------------------------
Hours=0
Hours <- format(as.POSIXct(strptime(rawdata$createdon,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H")
#Dates <- format(as.POSIXct(strptime(rawdata$createdon,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%Y/%m/%d")
Hours <- as.integer(as.character(Hours))
#new <- cbind(new, Hours)n
#h <- c(1:24)
h <- match(c(1:24), Hours)
h <- na.omit(h)
ht <- rawdata$timestamp[h]
length(ht)
hp <- rawdata$pressure[h]
hp
hc <- rawdata$createdon[h]
hc
#plot(x=rawdata$timestamp,y=rawdata$pressure, type='l', xlim=c(ht[1],ht[2]))
#points(x=new$rawtime[1:nrow(new)],y=new$rawpressure[1:nrow(new)],col='red',type='p')
length(new$rawtime)
#split(new$rawcreated, cut(new$rawcreated, breaks=ht))
u <- split(new$rawtime, cut(new$rawtime, breaks = ht,labels = paste0('ov',1:(length(ht)-1))))
u[1]
sum(r$counts)
summary(r)
length()
