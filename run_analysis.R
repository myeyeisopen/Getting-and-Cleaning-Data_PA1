x_test<-read.table("UCI HAR Dataset/test/X_test.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt")
mark_test<-mark_test<-data.frame(data=c(rep("test",nrow(subject_test))))
df_test<-cbind(mark_test,subject_test,y_test,x_test)
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")
mark_train<-data.frame(data=c(rep("train",nrow(subject_train))))
df_train<-cbind(mark_train,subject_train,y_train,x_train)
df<-rbind(df_train,df_test)
colnames(df)[1:3]<-c("data","subject","activity")
library(dplyr)
mean<-rowMeans(df[,4:561])
sd<-apply(df[,4:561],1,sd)
mydf<-cbind(select(df,data,subject,activity),Mean=mean,SD=sd)

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

activity_num<-c("1","2","3","4","5","6")
activity<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING",
            "LAYING")

mydf$Activity<-mgsub(activity_num,activity,mydf$activity)

by_mydf<-group_by(mydf,subject,Activity)
result_df<-summarize(by_mydf,mean(Mean),mean(SD))

write.table(result_df,"tidy data set.txt",row.name=FALSE)



