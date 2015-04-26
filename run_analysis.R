setwd("~/2014-2015/Spring2015/Math378/getdata-projectfiles-UCI HAR Dataset")

#extracts test and train data
x_test<-read.table("UCI HAR Dataset/test/X_test.txt")
y_test<-read.table("UCI HAR Dataset/test/Y_test.txt")
x_train<-read.table("UCI HAR Dataset/train/X_train.txt")
y_train<-read.table("UCI HAR Dataset/train/Y_train.txt")

#combines the test and train data
comb_x<-rbind(x_train,x_test)
comb_y<-rbind(y_train,y_test)
comb<-cbind(comb_x,comb_y)

#transposes column names
col.names<-read.table("UCI HAR Dataset/features.txt")
colnames(comb)<-t(col.names[,2])

#reads the subject values for test and train, then combines
sub.test<-read.table("UCI HAR Dataset/test/subject_test.txt")
sub.train<-read.table("UCI HAR Dataset/train/subject_train.txt")
sub.total<-rbind(sub.train,sub.test)

act_lbls<-read.table("UCI HAR Dataset/activity_labels.txt")

#standard deviation and mean of rows
std.row<-grep("std()",colnames(comb))
std.cols<-comb[,std.row]
mean.row<-grep("mean()",colnames(comb))
mean.cols<-comb[,mean.row]

#removes the mean frequencies and combines
meanFreqcols<-grep("meanFreq",colnames(mean.cols))
mean.cols<-mean.cols[meanFreqcols]
data<-cbind(sub.total,comb_y,mean.cols,std.cols)

#sets up the tidy data set
tidydata<-c()
subjects<-c(1:30)

colnames(data)[1]<-"Subjects"
colnames(data)[2]<-"Activities"

#cycles through activities to add to tidyframe, only finds mean for specific subject/data point
for(i in 1:30){
  for(j in 1:6){
    false<-subset(data,Subjects==subjects[i]&Activities==act_lbls[j,1])
    vector<-apply(false,2,mean)
    new_vector<-cbind(subjects[i],act_lbls[j,2],t(vector))
    tidydata<-rbind(tidydata,new_vector)
  }
}

colnames(tidydata)[1]="Subjects"
colnames(tidydata)[2]="Activity"

write.table(tidydata,"Tidydata.txt",row.name=FALSE)
