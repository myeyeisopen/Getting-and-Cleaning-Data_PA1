Getting-and-Cleaning-Data_PA1
=============================
###load the  test file
x_test<-read.table("UCI HAR Dataset/test/X_test.txt") 
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")  
y_test<-read.table("UCI HAR Dataset/test/y_test.txt") 

###mark the test file
mark_test<-mark_test<-data.frame(data=c(rep("test",nrow(subject_test))))

###combine the test data by column
df_test<-cbind(mark_test,subject_test,y_test,x_test)

###load the train data
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")

###mark the train data
mark_train<-data.frame(data=c(rep("train",nrow(subject_train))))

###combine train data by column
df_train<-cbind(mark_train,subject_train,y_train,x_train)

###combine test and train data
df<-rbind(df_train,df_test)

###rename the column names
colnames(df)[1:3]<-c("data","subject","activity")

###calculate the mean and standard deviation
library(dplyr)
mean<-rowMeans(df[,4:561])
sd<-apply(df[,4:561],1,sd)
mydf<-cbind(select(df,data,subject,activity),Mean=mean,SD=sd)

###create a function that can replace the activity_NUM to activity name
mgsub <- function(pattern, replacement, x, ...) {
        if (length(pattern)!=length(replacement)) {
                stop("pattern and replacement do not have the same length.")
        }
        result <- x
        for (i in 1:length(pattern)) {
                result <- gsub(pattern[i], replacement[i], result, ...)
        }
        result
}

###replace the activity_NUM to activity names
activity_num<-c("1","2","3","4","5","6")
activity<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING",
            "LAYING")
mydf$Activity<-mgsub(activity_num,activity,mydf$activity)

###group my data by subject and activity
by_mydf<-group_by(mydf,subject,Activity)

###calculate the mean and the mean of standard deviation by group
result_df<-summarize(by_mydf,mean(Mean),mean(SD))

###write the table out with name "tidy data set.txt"
write.table(result_df,"tidy data set.txt",row.name=FALSE)

