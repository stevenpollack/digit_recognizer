### WARNING:
### this needs to be run from kag_digit_id/
### ---------------------
library(data.table)

### check to see if data is loaded.
if (!"digit.data" %in% ls()) {
  # check to see if training_data.Rdata exists and load it
  if (system("ls data_and_benchmarks | grep -c -e '^training_data.Rdata$'", intern=T) == "1") {
    load(file="data_and_benchmarks/training_data.Rdata")
  } else { # doesn't exist, so make it
    ### check that train.csv exists
    if (system("ls data_and_benchmarks | grep -c -e '^train.csv$'", intern=T) == "1") {
      digit.data <- read.csv(file="data_and_benchmarks/train.csv",header=T)
      ### condition data
      rownames(digit.data) <- paste("image",1:42000,sep="")
      digit.data[,1] <- as.factor(digit.data[,1])

      ### cast as data.table
      digit.data <- data.table(digit.data)
      
      # key on label
      setkey(x=digit.data,label)

      ### save digit.data into .Rdata file
      save(digit.data, file="data_and_benchmarks/training_data.Rdata")
    } else {
      stop("Either you do not have data_and_benchmarks/train.csv or you are calling this script from somewhere other than the top-level of the project directory.")
    }
  }
}