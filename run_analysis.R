run_analysis <- function() { 
2   ## To use call source("~/run_analysis.R") correcting for file location, then run_analysis() 
3    
4   ## Import the data sets in a way that makes sense 
5    
6   ## Import the test data 
7    
8   X_test<-read.table("./UCI HAR Dataset/test/X_test.txt") 
9   Y_test<-read.table("./UCI HAR Dataset/test/y_test.txt") 
10   subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt") 
11    
12   ## Import the training data 
13   X_train<-read.table("./UCI HAR Dataset/train/X_train.txt") 
14   Y_train<-read.table("./UCI HAR Dataset/train/y_train.txt") 
15   subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt") 
16    
17   ## Task 1) Merge the training and the test sets to create one data set. 
18 
 
19   ## Combine the X data, Y data, and subject row identification into full versions of each 
20   X_full<-rbind(X_test, X_train) 
21   Y_full<-rbind(Y_test, Y_train) 
22   subject_full<-rbind(subject_test, subject_train) 
23 
 
24   ## Now the data frames are joined, it's worth naming the columns in x_full from features.txt 
25   features <- read.table("./UCI HAR Dataset/features.txt") 
26   colnames(X_full)<-features[,2] 
27 
 
28   ## Task 2) Extract only the measurements on the mean and standard deviation for each measurement 
29    
30   ## The columns with the desired measurements are labeled using mean() and std() so grepl on the column names 
31   ## looking for partial matches will flag them. '|' will create a vector that is true if either is matched. 
32   rightcols<- grepl("mean()",colnames(X_full)) | grepl("std()",colnames(X_full)) 
33 
 
34   ## Then putting the new columns in a pared down data frame is simple: 
35   X_mean_std <- X_full[,rightcols] 
36 
 
37   ## Task 3) Uses descriptive activity names to name the activities in the data set 
38   ## Task 4) Appropriately labels the data set with descriptive activity names.  
39   ## There is some ambiguity in the distinction between these tasks, I'm doing it all at once. 
40 
 
41   ## The activity labels are found in a file in the director, for reusability, they are read in not hardcoded. 
42    
43   activities<-read.table("./UCI HAR Dataset/activity_labels.txt") 
44    
45   ## While translating Y_full into human readable names I'l also make it a factor. 
46   Y_factor <- as.factor(Y_full[,1]) 
47    
48   ## mapvalues from the plyr package can do this efficiently. Declaring the library. 
49   library(plyr) 
50 
 
51   Y_factor <- mapvalues(Y_factor,from = as.character(activities[,1]), to = as.character(activities[,2])) 
52    
53   ## Y_factor is now a factor with the 6 named levels, the same length as the height of X_mean_std, so it can be added 
54   ## using cbind, putting it first for ease. 
55   X_mean_std <- cbind(Y_factor, X_mean_std)   
56 
 
57   ## Setting the name of the new first column to "activity" seems helpful too 
58   colnames(X_mean_std)[1] <- "activity" 
59 
 
60   ## I also want a column of subject IDs for later so I'll repeat the process with subject_full 
61 
 
62   X_mean_std <- cbind(subject_full, X_mean_std) 
63   colnames(X_mean_std)[1] <- "subject" 
64 
 
65   ## X_mean_std should now be a data frame with the subject ids in the first column the activity name in the second  
66   ## and then all the columns of variables that contained mean() and std() in their names. 
67 
 
68   ## Task 5) Creates a second, independent tidy data set with the average of each variable for each activity and each  
69   ## subject.  
70    
71   ## The goal is to take the average for each column of all values where subject and activity are the same, and to 
72   ## sort the resulting data so the first six rows are each activity for subject one, then the six for subject two etc. 
73 
 
74   ## This can be done using the reshape functions introduced in the lecture (indebted to David Hood Community TA on the  
75   ## coursera course forum for this suggestion) which requires declaring the library 
76    
77   library(reshape2) 
78 
 
79   X_melt<- melt(X_mean_std,id.vars=c("subject","activity")) 
80   Xav_tidy <- dcast(X_melt, subject + activity ~ ..., mean) 
81 
 
82   ## Xav_tidy is now the tidy dataset required. Now return it and we're done 
83 
 
84   return(Xav_tidy) 
85    
86 } 
