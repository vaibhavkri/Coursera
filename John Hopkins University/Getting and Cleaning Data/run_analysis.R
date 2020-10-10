library(reshape2)

# Load the various datasets
test.subject <- read.table("./test/subject_test.txt")
test.x <- read.table("./test/X_test.txt")
test.y <- read.table("./test/y_test.txt")

train.subject <- read.table("./train/subject_train.txt")
train.x <- read.table("./train/X_train.txt")
train.y <- read.table("./train/y_train.txt")

features <- read.table("./features.txt")
activity.labels <- read.table("./activity_labels.txt")

# Merge the test and train subject datasets
subject <- rbind(test.subject, train.subject)
colnames(subject) <- "subject"

# Merge the test and train labels, applying the textual labels
label <- rbind(test.y, train.y)
label <- merge(label, activity.labels, by=1)[,2]

# Merge the test and train main dataset, applying the textual headings
data <- rbind(test.x, train.x)
colnames(data) <- features[, 2]

# Merge all three datasets
data <- cbind(subject, label, data)

# Create a smaller dataset containing only the mean and std variables
search <- grep("-mean|-std", colnames(data))
data.mean.std <- data[,c(1,2,search)]

# Compute the means, grouped by subject/label
melted = melt(data.mean.std, id.var = c("subject", "label"))
means = dcast(melted , subject + label ~ variable, mean)

# Save the resulting dataset
write.table(means, file="./data/tidy_data.txt")

# Output final dataset
means
