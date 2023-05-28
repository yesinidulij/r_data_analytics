
library("dplyr")
example= read.csv("C:/Users/user/Documents/r programing/archive/vgsales.csv",stringsAsFactors = TRUE)

example=tbl_df(example)
example
View(example)

# descriptive statistics
example$Platform
unique(example$Platform)
length(unique(example$Platform))

# calculate NA_Sales mean,median,range
mean(example$NA_Sales)
median(example$NA_Sales)
range(example$NA_Sales)

summary(example)

#calculating missing value 
mean(example$NA_Sales,na.rm = TRUE)
is.na(example$NA_Sales)
sum(!is.na(example$NA_Sales))
#histogram
ggplot(example,aes(NA_Sales))+geom_histogram(bins=30)
ggplot(example,aes(NA_Sales))+geom_histogram(bins=80)
ggplot(example,aes(EU_Sales))+geom_histogram(bins=30)


#scatter
ggplot(example,aes(NA_Sales,EU_Sales,Publisher))+geom_point()

#t-test
examplea=filter(example,Global_Sales>=20)
View(examplea)
t.test(NA_Sales~Genre,data=examplea)



#Linear Regression
fit<-lm(EU_Sales~NA_Sales,data=examplea)
summary(fit)
ggplot(examplea,aes(x=EU_Sales,y=NA_Sales))+geom_point()+geom_smooth(method = "lm")


#aditionals
#in fuction for specifying values
df=filter(example,Genre %in% c("Platform","Action"))

#
colSums(is.na(example))

# for help
?ifelse()

#for adding new column on table

example$new_col <- ifelse(example$Year==2013,1,0)
example=mutate(example,id2=ifelse(example$Year==2013,0,1),id3=2)


# for arranging tables
new=arrange(example,Year)

#for multiple functions
df<-example %>% filter(Publisher=="Nintendo")
df<-example %>% filter(Publisher=="Nintendo")
df1<-example %>% group_by(Platform) %>% summarise(m=mean(NA_Sales))
df2<-example %>% group_by(Platform) %>% summarise(m=mean(NA_Sales),cust=n())

#for ploting graph
plot(example$Platform)
plot(example$Platform,xlab="names",ylab="frequency",col="green",ylim=c(0,2100),main="paltform distribution")


#for ploting histogram
hist(df2$m)
hist(df2$m,breaks = 10,col="palegreen4",border = "white")

# for ploting scatter diagram

plot(x=example$EU_Sales,y=example$Global_Sales,type = "p",main="scatter plot",xlab = "EU SALES",ylab = "global sales",pch=20)




#for transposing dimensions
d <-t(example)

#for creating some data frame
a<-data.frame(letter=LETTERS[8:12],a=c(1:5))
b<-data.frame(letter=LETTERS[sample(10)],b=runif(10))

#for merging it
c<-merge(a,b)
c<-merge(a,b,all=TRUE)


#matrix manipulation
m<-matrix(seq(from=5,to=100,by=5),nrow=5,ncol=4)
# for adding matrix
m+m
#for product of rows
apply(m,1,prod)

#for the mean of columns
apply(m,2,prod)

