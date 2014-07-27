run_analysis <- function()
{
    ##Read in features.txt, which contains the vector labels for each of the datasets.
    vector_labels <- read.table("features.txt")
    
    ##Determine which vector labels contain the mean or standard deviation.
    log_sel_vectors <- grepl("mean|std", as.vector(vector_labels[,2]))
    
    ##Create vector containing column class for all selected columns (numeric) and NULL for all unselected columns (not mean or standard deviation).
    vector_class <- vector(mode = "character", length = nrow(vector_labels))
    for (i in 1:nrow(vector_labels))
    {
        if(log_sel_vectors[i]==FALSE)
        {
            vector_class[i] <- "NULL"
        }
        else
        {
            vector_class[i] <- "character"
        }
    }
    
    
    ##Read in test and train datasets, along with respective activity and subject labels.
    test <- read.table("X_test.txt",col.names = as.vector(vector_labels[,2]),colClasses = vector_class)
    test_activities <- read.table("y_test.txt", col.names = "Activity Code")
    test_subjects <- read.table("subject_test.txt",col.names = "Subject ID")
    train <- read.table("X_train.txt",col.names = as.vector(vector_labels[,2]),colClasses = vector_class)
    train_activities <- read.table("y_train.txt", col.names = "Activity Code")
    train_subjects <- read.table("subject_train.txt", col.names = "Subject ID")
    
    ##Combine data, activity labels, and subject labels for the test data and the train data separately.
    ##Also adding 2 columns:
      ## "Data Source": identifies whether data is from test dataset or train dataset.
      ## "Activity ID": placeholder empty column for translated Activity codes.
    all_test <- cbind("Data Source" = rep("test", nrow(test)),test_subjects, test_activities, "Activity" = vector(mode = "character", length = nrow(test)), test)
    all_train <- cbind("Data Source" = rep("train", nrow(train)),train_subjects, train_activities, "Activity" = vector(mode = "character", length = nrow(train)), train)
    
    ##Combine test and train data. Sort by Subject ID.
    data <- arrange(rbind(all_test, all_train),Subject.ID)
    data[,5:ncol(data)] <- sapply(data[,5:ncol(data)],as.numeric)
        
    ##Translate Activity ID Code into Activity (character describing activity).  
    data[,"Activity"] <- as.character(data[,"Activity"])
    activities <- c("Walking", "Walking_Upstairs", "Walking_Downstairs", "Sitting", "Standing", "Laying")
    for(i in 1:nrow(data))
    {        
        index <- data[i,"Activity.Code"]
        data[i,"Activity"] <- activities[index]  
    }
    
    ##Create means and standard deviation for each measurement by Subject ID and Activity.
    ##First four columns were excluded from mean and standard deviation calculations since they do not represent measurements.
    means <- aggregate(sapply(data[,5:ncol(data)],as.numeric), by=list(data$Subject.ID,data$Activity),FUN=mean)
    sds <- aggregate(data[,5:ncol(data)], by=list(data$Subject.ID,data$Activity),FUN=sd)
    
    ##Label each column of "means" and "stdevs" as being a mean or standard deviation, respectively.
    ##First two columns were also renamed.
    colnames(means)[1:2] <- c("Subject.ID","Activity")
    colnames(sds)[1:2] <- c("Subject.ID","Activity")
    cname_means <- colnames(means)
    cname_sds <- colnames(sds)
    for(i in 3:length(cname_means))
    {
        cname_means[i] <- paste(cname_means[i],"Mean", sep = " - ")
    }
    for (i in 3:length(cname_sds))
    {
        cname_sds[i] <- paste(cname_sds[i],"SD", sep = " - ")
    }
    colnames(means) <- cname_means
    colnames(sds) <- cname_sds 
    
    ##Combine means and sds.
    ##sum_data <- cbind(means,sds)
    sum_data <- join(means,sds,c("Subject.ID","Activity"))
    write.table(sum_data, file = "Tidy Data - Means and SDs.txt")
    write.csv(sum_data, file = "Tidy Data - Means and SDs.csv")
    return(sum_data) 
}