#IMPORT MODULE
library("ggplot2")
library("ggplot")
library('ggpubr')
library('stringr')
library("dplyr")
library("hrbrthemes")
library("viridis")
library("tidyselect")
library("data.table")


#IMPORT DATASET
Data<-read.csv("C:\\Users\\ziad\\Downloads\\Placement_Data_Full_Class.csv")


#set field datatypes
str(Data)
Data$Fedu<-as.character(Data$Fedu)
Data$Medu<-as.character(Data$Medu)
options(scipen=10000)

#REMOVE OUTLIERS
#replace salary NAs with average
summary(Data$salary)
Data$salary[is.na(Data$salary)] <- mean(Data$salary, na.rm = TRUE)


#AGE
#analysis 1.1
temp<-data.frame(table(Data$age))
names(temp)=c("Age","Frequency")

ggplot(temp,aes(x=Age,y=Frequency,fill=Age))+geom_bar(stat="identity")+
  theme_ipsum()+
  scale_y_continuous(breaks=seq(0,9000,500))

#1.2
ggplot(data = Data, aes(x = age, y = salary)) +
  geom_boxplot() +
  theme_ipsum()

#1.3
job_role_counts <- data.frame(table(Data$age, Data$specialisation))
names(job_role_counts) <- c("age", "Job Role", "Count")

ggplot(data = job_role_counts, aes(x = age, y = Count, fill = `Job Role`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Job Role")+
  theme_ipsum()

#1.4
ggplot(data = Data, aes(x = etest_p, fill = age)) +
  geom_density(binwidth = 5, alpha = 0.8, position = "identity",,color="black") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "Age") +
  scale_x_continuous(breaks=seq(0,100,20))
theme_ipsum()


#1.5
ggplot(data = Data, aes(x = ssc_p, fill = age)) +
  geom_density(binwidth = 5, alpha = 0.8, position = "identity",,color="black") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "Age") +
  scale_x_continuous(breaks=seq(0,100,20))
theme_ipsum()

#1.6
ggplot(data = Data, aes(x = hsc_p, fill = age)) +
  geom_density(binwidth = 5, alpha = 0.8, position = "identity",,color="black") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "Age") +
  scale_x_continuous(breaks=seq(0,100,20))
theme_ipsum()

#1.7
ggplot(data = Data, aes(x = mba_p, fill = age)) +
  geom_density(binwidth = 5, alpha = 0.8, position = "identity",,color="black") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "Age") +
  scale_x_continuous(breaks=seq(0,100,20))
theme_ipsum()

#1.8
ggplot(data = Data, aes(x = age, fill = etest_p)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"), name = "E test gardes") +
  ylab("Count") +
  xlab("Age") +
  theme_ipsum()

#FAMILY SUPPORT
#2.1
ggplot(Data, aes(x = famsup)) + 
  geom_bar(fill = "#69b3a2", color = "black", alpha = 0.8) +
  scale_x_discrete(name = "Family Support", labels = c("No", "Yes")) +
  scale_y_continuous(name = "Frequency",breaks=seq(0,9000,500)) +
  theme_ipsum()

#2.2
ggplot(Data,aes(x=famsup,y=address))+geom_count(aes(color=..n..))+
  theme_ipsum()


#2.3
ggplot(Data, aes(x = famsup, y = salary, fill = famsup)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"), name = "Family Support",
                    labels = c("Yes", "No")) +
  theme_ipsum()

#2.4
ggplot(Data, aes(x = famsup, fill = Mjob)) +
  geom_bar(position = "dodge", color = "black") +
  theme_ipsum()

#2.5
support_yes <- subset(Data, famsup == "yes")
support_no <- subset(Data, famsup == "no")

# Create histogram for students with family support
p1 <- ggplot(support_yes, aes(x = `workex`, fill = `workex`,color="black")) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "with family support", y = "Count") +
  theme_ipsum()

# Create histogram for students without family support
p2 <- ggplot(support_no, aes(x = `workex`, fill = `workex`,color="black")) +
  geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(x = "without family support", y = "Count") +
  theme_ipsum()

# Combine the two plots side-by-side
gridExtra::grid.arrange(p1, p2, ncol = 2)

#2.6
# Subset the data by family support
yes_fs <- subset(Data, famsup == "yes")
no_fs <- subset(Data, famsup == "no")
mean(no_fs$ssc_p)
mean(yes_fs$ssc_p)

# Create a bar plot of average grades by family support
ggplot(data = Data, aes(x = famsup, y = ssc_p, fill = famsup)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(values = c("#FF6666", "#66CCFF")) +
  labs(x = "Family Support", y = "E test Grade", fill = "") +
  theme_ipsum() +
  theme(legend.position = "none")


#2.7
yes_fs <- subset(Data, famsup == "yes")
no_fs <- subset(Data, famsup == "no")
mean(no_fs$hsc_p)
mean(yes_fs$hsc_p)

ggplot(data = Data, aes(x = famsup, y = hsc_p, fill = famsup)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(values = c("#FF6666", "#66CCFF")) +
  labs(x = "Family Support", y = "Average Grade", fill = "") +
  theme_ipsum() +
  theme(legend.position = "none")

#2.8
no_fs <- subset(Data, famsup == "no")
yes_fs <- subset(Data, famsup == "yes")
mean(no_fs$degree_p)
mean(yes_fs$degree_p)

ggplot(data = Data, aes(x = famsup, y = degree_p, fill = famsup)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(values = c("#FF6666", "#66CCFF")) +
  labs(x = "Family Support", y = "E test Grade", fill = "") +
  theme_ipsum() +
  theme(legend.position = "none")

#2.9
ggplot(data = Data, aes(x = famsup, fill = etest_p)) + 
  geom_bar(color = "black", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Family Support", y = "Count", fill = "E test grade") +
  theme_ipsum()


#2.10

family_specialization <- Data[, c("famsup", "specialisation")]
counts <- table(family_specialization)


df <- as.data.frame(counts)
names(df) <- c("familysupport", "specialisation", "count")

ggplot(df, aes(x = specialisation, y = count, fill = familysupport)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of Family Support on Specialisation",
       x = "Specialisation",
       y = "Count",
       fill = "Family Support")

#RECRUITMENT  
#3.1
cor(Data$mother_education, AssignmentData$father_education, use = "pairwise.complete.obs",method = c("spearman"))
Data<-Data%>%
  group_by(specialisation)%>%
  summarise(AverageSalary=mean(salary, na.rm=TRUE))
Data

#3.2
#first graph: bar graph to show the average according to degree specialization
ggplot(data=Data, mapping=aes(x=specialisation, y=AverageSalary))+
  geom_col(color="black", position = "dodge")+
  ggtitle("Average Salary based on degree specialization")+
  ylab("Average Salary")+
  xlab("Degree")+
  theme_ipsum()
#second grap: dumbell graph to show the range of salary for degree specialization
Data<-Data %>%
  group_by(specialisation) %>%
  summarise(thirdQ=quantile(salary,0.75,na.rm=TRUE),
            FirstQ=quantile(salary,0.25,na.rm=TRUE))
Data

#3.3
#Analysis 4:how does area type affect the job of the student member's parents?#for father:
A3<-Data%>%
  group_by(address, Fjob)%>%
  count() #na.rm is used to ignore all NA values
A3
ggplot(data=A3, mapping=aes(x=Fjob, group=address, y=n, fill=address))+
  geom_col(color="black", position = "dodge")+
  ggtitle("Distribution of types of jobs of father by area Type ")+
  ylab("number of father")+
  xlab("jobs")+
  theme_ipsum()

#3.4
#for mother:
A3<-Data%>%
  group_by(address, Mjob)%>%
  count() #na.rm is used to ignore all NA values
A3
ggplot(data=A3, mapping=aes(x=Mjob, group=address, y=n, fill=address))+
  geom_col(color="black", position = "dodge")+
  ggtitle("Distribution of types of jobs of mother by area Type ")+
  ylab("number of mother")+
  xlab("jobs")+
  theme_ipsum()

#3.5
ggplot(Data, aes(x=salary, fill = age, group=age))+geom_boxplot()+
  xlab("salary") + ylab("age")+
  labs(title="Boxplot of salary according to age")+
  theme_ipsum()

#3.6
A3<-Data%>%
  group_by(etest_p,age)%>%
  summarise(AverageSalary=mean(salary,na.rm=TRUE)) #na.rm is used to ignore all NA values
A3
ggplot(data=A3, mapping=aes(x=etest_p, group=age, y=AverageSalary, fill=age))+
  geom_col(color="black", position = "dodge")+
  ggtitle("Average salary for age based on their E test grade ")+
  ylab("Average salary")+
  xlab("E test grade")+
  theme_ipsum()


#3.7
w<-Data %>% group_by(Fjob)%>%
  summarise(count= n()/nrow(Data)*100)
x<-unique(w$Fjob)
m1<-w$count
p1<-pie3D(m1, labels = x,main = "percenatage of job of fathers",col = c("green","blue","red","yellow","orange"))
p1
#3.8
w1<-Data %>% group_by(mother_job)%>%
  summarise(count= n()/nrow(Data)*100)
x<-unique(w1$mother_job)
m1<-w1$count
p2<-pie3D(m1, labels = x,main = "percenatage of job of mother ",col = c("green","blue","red","yellow","orange"))
p2
ggarrange(p1,p2,nrow=1,ncol=2,common.legend=TRUE)


#3.9
A1<-Data%>%
  group_by(ssc_p)%>%
  summarise(AverageSalary=y=mean(ssc_b))
A1
A2<-Data%>%
  group_by(ssc_p)%>%
  summarise(AverageSalary=median(ssc_b))
A2
ggplot(A1, aes(ssc_p, AverageSalary, fill=ssc_p)) + geom_bar(stat="identity")
ggplot(A1, aes(ssc_p, AverageSalary, fill=ssc_p)) + geom_bar(stat="identity")
A2<-Data%>%
  group_by(ssc_p)%>%
  summarise(AverageSalary=max(ssc_b))
A2
A2<-Data%>%
  group_by(ssc_p)%>%
  summarise(AverageSalary=min(ssc_b))
A2


#3.9
d2<-Data%>%
  group_by(address, age)%>%
  summarise(AverageSalary=mean(salary,na.rm=TRUE))
d2
#3.9: plot the data using line graph
ggplot(data=d2, mapping=aes(x=address, y=AverageSalary, group=age,color=age))+
  geom_line() +
  labs(title="Average Monthly salary according to Number of area type")+
  theme_ipsum()


#3.10
A4<-Data%>%
  group_by(employment_status = status)%>%
  summarise(average_salary = mean(employment_test))
A4
ggplot(A1, aes(ssc_p, AverageSalary)) + geom_bar(stat="identity")
#conclusion: not placed individuals on average score



#GENDER

#4.1

temp<-data.frame(table(Data$gender))


# create a bar graph using ggplot
ggplot(data = temp, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Gender Group", y = "Number of Students") +
  ggtitle("Number of Students in Different Genders Groups") +
  theme_ipsum()

#4.2
df_location_gender <- Data %>% 
  group_by(address) %>% 
  summarize(avg_gender = mean(gender)) 

# create a pie chart
ggplot(df_location_gender, aes(x = "", y = avg_gender, fill = address)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_ipsum()

#4.3
ggplot(data = Data, aes(x = gender, fill = specialisation)) +
  geom_bar(position = "dodge") +
  
  # Add labels and title
  labs(x = "Gender Group", y = "Number of Students",
       fill = "Job Role", title = "Genders Differences in Job Roles")

# Set theme
theme_ipsum()

#4.4
grades_by_gender <- Data %>%group_by(gender) %>%summarize(mean_grade = mean(ssc_b))

# Create the bar graph
ggplot(grades_by_gender, aes(x = gender, y = mean_grade, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Mean Secondary School Grade") +
  ggtitle("Gender Differences in Secondary School Grades") +
  coord_cartesian(ylim = c(70, 75))
theme_ipsum()


#4.5

grades_by_age <- Data %>%group_by(age) %>%summarize(mean_grade = mean(hsc_p))

ggplot(grades_by_age, aes(x = age, y = mean_grade, fill = age)) +
  geom_bar(stat = "identity") +
  labs(x = "Age", y = "Mean High School Grade") +
  ggtitle("Age Differences in High School Grades") +
  coord_cartesian(ylim = c(70, 78.33))
theme_ipsum()

#4.6
grades_by_gender <- Data %>%group_by(gender) %>%summarize(mean_grade = mean(degree_p))

# Create the bar graph
ggplot(grades_by_gender, aes(x = gender, y = mean_grade, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Mean Degree School Grade") +
  ggtitle("Gender Differences in Degree  Grades") +
  coord_cartesian(ylim = c(72, 78.86))
theme_ipsum()


#4.7
ggplot(data = Data, aes(x = gender, fill = etest_p)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#0072B2", "#E69F00"), 
                    name = "E test grade",
                    labels = c(66, 96.8)) +
  labs(x = "Gender",
       y = "Number of Employees",
       title = "Genders Differences in E test grades") +
  theme_ipsum()

#4.8
df_filtered <- Data %>% filter(!is.na(gender) & !is.na(specialisation))

# group the data by gender and specialisation and count the number of students
df_grouped <- df_filtered %>% group_by(gender, specialisation) %>% summarize(count = n())

# create the pie chart
ggplot(df_grouped, aes(x="", y=count, fill=specialisation)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  facet_wrap(~gender) +
  theme_void() +
  theme(legend.position = "bottom")


#PAID ACTIVITIES

#5.1
ggplot(data = Data, aes(x = degree_t, fill = hsc_p)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Degree Type by High School Grade",
       x = "Degree Type",
       y = "Density") +
  theme_ipsum()

#5.2
ggplot(data = Data, aes(x = degree_t, y = etest_p)) +
  geom_point() +
  labs(x = "Degree Type", y = "E-Test Grades") +
  theme_ipsum()

#5.3
ggplot(data = Data, aes(x = hsc_b, y = degree_t)) +
  geom_boxplot(fill = "#69b3a2", alpha = 0.8) +
  labs(x = "High School Location", y = "Degree Type") +
  theme_ipsum()

#5.4
# Create box plot
ggplot(data = Data, aes(x = degree_p, y = etest_p, fill = age)) +
  geom_boxplot() +
  labs(title = "Comparison of E test Grades by Degree grade and age",
       x = "Degree grade", y = "E test Grade", fill = "age") +
  theme_ipsum()

#5.5
# Subset the data for only secondary school location and secondary school grade
secondary_school <- Data %>% select(ssc_b, ssc_p)

# Summarize the data by location and calculate the mean grade for each location
summary <- secondary_school %>% group_by(ssc_b) %>% 
  summarise(mean_grade = mean(ssc_p))

# Create a pie chart to show the distribution of grades by location
ggplot(summary, aes(x = ssc_b, y = mean_grade, fill = ssc_b)) +
  geom_bar(stat = "identity") +
  labs(x = "Gender", y = "Mean Secondary School Grade") +
  ggtitle("Gender Differences in Secondary School Grades") +
  coord_cartesian(ylim = c(70, 75))
theme_ipsum()

#5.6
ggplot(data = Data, aes(x = hsc_p, y = hsc_b)) + 
  geom_boxplot(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "High School Grade", y = "High School Course", 
       title = "Comparison of High School Course by High School Grades") +
  theme_ipsum()

#5.7
ggplot(data = Data, aes(x = etest_p, y = workex)) + 
  geom_violin(fill = "#69b3a2", alpha = 0.7) +
  labs(x = "E test grade", y = "Work Experuence", 
       title = "Distribution of Work Experience by E test grade") +
  theme_ipsum()

#5.8    
ggplot(data = Data, aes(x = etest_p, y = salary)) +
  geom_point() +
  labs(title = "Relationship between E test Grades and Salary Levels",
       x = "E test Grades",
       y = "Salary Levels")
