# @head: MECAI | MAI5024
# @auth: Andre Perez
# @mail: andre.marcos.perez@usp.br

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list=ls())
cat("\014")

# ------------------------------------------------------------------------------
# -- Variable ------------------------------------------------------------------
# ------------------------------------------------------------------------------

# -- Environment

PATH.INPUT  <- "dataset"
PATH.OUTPUT <- "dataset"
FILE.INPUT  <- "kaggle-etl.csv"

# -- Script

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("sqldf")) {
  install.packages("sqldf");
  library("sqldf");
}

# ------------------------------------------------------------------------------
# -- Functions -----------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c("NA"), encoding="latin1", stringsAsFactors = FALSE);

# ----------------------------------------
# -- Feature Engineering -----------------
# ----------------------------------------

# Drop rows

data <- sqldf("select * from data where Age in ('18-21','22-24','25-29') and (Experience = '0-1' or Experience = '1-2') and Job <> 'Student'");

# Drop sensible cols 

data$Age <- NULL;
data$Gender <- NULL;

# TBD

data$Education <- NULL;
data$TimeCoding <- NULL;
data$DataCoding <- NULL;
data$Experience <- NULL;
data$MLYearCoding <- NULL;
data$Undergraduate <- NULL;

# Dummy non tech vars

data.job <- sqldf("select Job from data where Job like 'Data %' group by Job");

for (index in 1:nrow(data)) {
  
  # gender
  
  #feature <- data$Gender[index];
  
  #if (feature == "Male") {
  #  data$Gender[index] <- 1;
  #}
  #else{
  #  data$Gender[index] <- 0;
  #}
  
  # job (target)
  
  feature <- data$Job[index];
  
  if (grepl(feature, toString(data.job))) {
    data$Job[index] <- 1;
  }
  else{
    data$Job[index] <- 0;
  }
}

# Dummy tech vars

for (index in 1:ncol(data)) {
  feature <- data[index];
  feature[!is.na(feature)] <- 1;
  feature[is.na(feature)]  <- 0;
  data[index] <- feature;
}

# save engineerd dataframe

write.csv(data, file=file.path(PATH.OUTPUT, "kaggle-preprocessing.csv"), row.names=FALSE);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT", "PATH.OUTPUT"));
rm(list = c("index", "feature", "data.job"));

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")