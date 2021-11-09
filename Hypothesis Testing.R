# import dataset : csv type 
# using complete path 
salary<-read.csv(file = "C:/personal/imarticus/basic_salary2.csv")

# relative path 
salary<-read.csv(file="basic_salary2.csv",na.strings = c("","","NA"))

# using file.choose()
salary<-read.csv(file.choose())

summary(salary)
head(salary,15)
tail(salary,20)

# subset : index based , condition based 
# index based : 
# fetch rows from 20 to 40 
salary[20:40,]

# fetch First_name and ba for all emp 
salary[,c("First_Name","ba")]

# fetch first name and location for emp having index 10 to 21
salary[10:21,c("First_Name","Location")]

# fetch first name ba and ms for emp having index 1 to 10 , 15 to 20 and 33 to 41
salary[c(1:10,15:20,33:41),c("First_Name","ba","ms")]

# count total percentage of missing entries 
# total count of missing entries 

summary(salary)
sum(is.na(salary))# total True entries from is.na
is.na(salary)
colSums(is.na(salary))
11/(nrow(salary)*ncol(salary))

# mean and median for ba and ms column 
mean(salary$ba,na.rm = T)
median(salary$ba,na.rm = T)
var(salary$ba,na.rm = T)# squared diff of each data point wrt its mean value 
sd(salary$ba,na.rm = T) # sqrt of var 
summary(salary$ba)

# impute NA with meaningful value 
# na can be imputed with CT [ num : median / cat : mode]
salary$ba[is.na(salary$ba)]<-median(salary$ba,na.rm = T)
salary$ms[is.na(salary$ms)]<-median(salary$ms,na.rm = T)




