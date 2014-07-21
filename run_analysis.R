train.x = read.table('./train/X_train.txt')
test.x = read.table('./test/X_test.txt')
train.y = read.table('./train/y_train.txt')
test.y = read.table('./test/y_test.txt')

subject_train = read.table('./train/subject_train.txt')
subject_test = read.table('./test/subject_test.txt')

activity_labels = read.table('activity_labels.txt')
features = read.table('features.txt')

# Merge the training and test sets
all.x = rbind(train.x,test.x)

# Extract the mean and sd for each measure

colnames(all.x) = c(as.character(features[,2]))
Mean = grep("mean()", colnames(all.x),fixed = TRUE)
SD = grep("std()",colnames(all.x),fixed = TRUE)

MeanSD = all.x[,c(Mean,SD)]

# Add the descriptive activity names to the data set

all.y = rbind(train.y,test.y)
all.activity = cbind(all.y,MeanSD)
colnames(all.activity)[1] = "Activity"

# Labels the data with activity names

activity_labels[,2] = as.character(activity_labels[,2])

for (i in 1:length(all.activity[,1])){
    all.activity[i,1] = activity_labels[all.activity[i,1],2]
}

# Create a data set with the average of each variable for each activity
subject.all = rbind(subject_train,subject_test)
all = cbind(subject.all,all.activity)

colnames(all)[1] = "Subject"
tidy.data = aggregate( all[,3] ~ Subject+Activity, data = all, FUN="mean")

for (i in 4:ncol(all)){
    tidy.data[,i] = aggregate(all[,i] ~ Subject+Activity, data = all, FUN = "mean")[,3]
}

colnames(tidy.data)[3:ncol(tidy.data)] = colnames(MeanSD)

write.table(tidy.data, file = "data.txt")