library(dplyr)

setwd("./UCI HAR Dataset")


features <- read.table("features.txt")
activity.labels <- read.table("activity_labels.txt")
names(activity.labels) <- c("label.codes","activity")

feature.vector <- as.vector(features$V2)

#read training text files
training <- read.table("train/X_train.txt")
names(training)<- feature.vector
training.labels <- read.table("train/Y_train.txt")
names(training.labels)<- "label.codes"
train.activity.unsorted <- merge(training.labels,activity.labels, by= "label.codes", all.x = TRUE)
# resort into original activity order
train.activity.data<-train.activity.unsorted[match(training.labels$label.codes,train.activity.unsorted$label.codes),]
subject.train <- read.table("train/subject_train.txt")
names(subject.train) <- "subject"
complete.trainig <- cbind(subject.train,train.activity.data,training)


#read test text files
test <- read.table("test/X_test.txt")
names(test)<- feature.vector
test.labels <- read.table("test/Y_test.txt")
names(test.labels)<- "label.codes"
test.activity.unsorted <- merge(test.labels,activity.labels, by= "label.codes", all.x = TRUE)
# resort into original activity order
test.activity.data<-test.activity.unsorted[match(test.labels$label.codes,test.activity.unsorted$label.codes),]
subject.test <- read.table("test/subject_test.txt")
names(subject.test) <- "subject"
complete.test <- cbind(subject.test,test.activity.data,test)

# Step 1 solution - merge the training and test data
complete.training.test.data <- rbind(complete.trainig,complete.test)

# identify the features (variable columns) that include mean or std in them

feature.is.mean <- grepl("[Mm]ean",feature.vector)
feature.is.std <- grepl("[Ss]td",feature.vector)
feature.keep.list <- feature.vector[feature.is.mean|feature.is.std]
  #remove features that have mean in the name, but do not appear to be means
  with.mean.but.not.mean <- c("angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)", 
                                "angle(tBodyGyroMean,gravityMean)","angle(tBodyGyroJerkMean,gravityMean)",
                                "angle(X,gravityMean)","angle(Y,gravityMean)","angle(Z,gravityMean)")
  feature.keep.list <-feature.keep.list[!feature.keep.list %in% with.mean.but.not.mean]
all.cols <- c("subject","label.codes","activity", feature.keep.list)

# Step 2 solution - keep only the mean and std variables

  #mean.std.data <- select(complete.training.test.data, feature.keep.list)
  mean.std.data<- complete.training.test.data[,all.cols]
  #remove features that have mean in the name, but do not appear to be means
   

# Steps 3 & 4 already performed.  Label names present, and variable names head dataframe column names.

#Step 5
data.split <- group_by(mean.std.data, subject, activity)
average.by.subject.and.activity <- summarize_all(data.split, funs(mean) )
write.table(average.by.subject.and.activity,"C:/Work/R-Course/GettingAndCleaningData/Assignment/Step5_Data.txt",row.name=FALSE) 
