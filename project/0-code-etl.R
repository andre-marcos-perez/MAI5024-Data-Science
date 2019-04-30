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
FILE.INPUT  <- "kaggle.csv"

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

QueryHelperQuestion <- function(nQuestion, nCol, firstComma){
  
  index <- 1;
  question <- "";
  
  if (!firstComma) {
    question <- paste0(question, " Q", nQuestion, "_Part_", index);
    index <- 2;
  }
  
  for (i in index:nCol) {
    question <- paste0(question, ", Q", nQuestion, "_Part_", i);
  }
  
  return(question)
}

ColnamesHelperQuestions <- function(nQuestion, colname){
  
  striped <- strsplit(toString(colname), ' - ');
  striped <- striped[[1]][length(striped[[1]])];
  striped <- paste0("Q", nQuestion, "_", striped);
  
  while(grepl("/", striped)){
    striped <- sub("/", "_", striped);
  }
  
  while(grepl(" ", striped)){
    striped <- sub(" ", "_", striped);
  }
  
  while(grepl("\\+", striped)){
    striped <- sub("\\+", "", striped);
  }
  
  while(grepl("\\(", striped)){
    striped <- sub("\\(", "", striped);
  }
  
  while(grepl("\\)", striped)){
    striped <- sub("\\)", "", striped);
  }
  
  while(grepl("\\.", striped)){
    striped <- sub("\\.", "", striped);
  }
  
  while(grepl("\\#", striped)){
    striped <- sub("\\#", "Sharp", striped);
  }
  
  while(grepl("-", striped)){
    striped <- sub("-", "", striped);
  }
  
  return(striped);
}


# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c(""," ","NA","na","-1"), encoding="latin1");

# ----------------------------------------
# -- Clean -------------------------------
# ----------------------------------------

# - get relevant questions

data <- sqldf(paste0("select Q1, Q2, Q4, Q5, Q6, Q8", 
                     QueryHelperQuestion("13", 13, TRUE),
                     QueryHelperQuestion("14",  9, TRUE),
                     QueryHelperQuestion("15",  5, TRUE),
                     QueryHelperQuestion("16", 16, TRUE),
                     QueryHelperQuestion("19", 17, TRUE),
                     QueryHelperQuestion("21", 11, TRUE),
                     ", Q23, Q24, Q25", 
                     QueryHelperQuestion("27", 18, TRUE),
                     QueryHelperQuestion("28", 42, TRUE),
                     QueryHelperQuestion("29", 26, TRUE),
                     QueryHelperQuestion("30", 23, TRUE),
                     " from data"));

# save question row and eliminate from dataset

questions <- sqldf("select * from data where rowid = 1");
data <- sqldf("select * from data where rowid > 1");

# rename cols

colnames(data)[1] <- "Gender";
colnames(data)[2] <- "Age";
colnames(data)[3] <- "Education";
colnames(data)[4] <- "Undergraduate";
colnames(data)[5] <- "Job";
colnames(data)[6] <- "Experience";

for (index in 7:19) {
  colnames(data)[index] <- ColnamesHelperQuestions("13", questions[index]);
}

for (index in 20:28) {
  colnames(data)[index] <- ColnamesHelperQuestions("14", questions[index]);
}

for (index in 29:33) {
  colnames(data)[index] <- ColnamesHelperQuestions("15", questions[index]);
}

for (index in 34:49) {
  colnames(data)[index] <- ColnamesHelperQuestions("16", questions[index]);
}

for (index in 50:66) {
  colnames(data)[index] <- ColnamesHelperQuestions("19", questions[index]);
}

for (index in 67:77) {
  colnames(data)[index] <- ColnamesHelperQuestions("21", questions[index]);
}

colnames(data)[78] <- "TimeCoding";
colnames(data)[79] <- "DataCoding";
colnames(data)[80] <- "MLYearCoding";

for (index in 81:98) {
  colnames(data)[index] <- ColnamesHelperQuestions("27", questions[index]);
}

for (index in 99:139) {
  colnames(data)[index] <- ColnamesHelperQuestions("28", questions[index]);
}

for (index in 141:166) {
  colnames(data)[index] <- ColnamesHelperQuestions("29", questions[index]);
}

for (index in 167:189) {
  colnames(data)[index] <- ColnamesHelperQuestions("30", questions[index]);
}

# save cleaned dataframe

write.csv(data, file=file.path(PATH.OUTPUT, "kaggle-etl.csv"), row.names=FALSE);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT", "PATH.OUTPUT"));
rm(list = c("questions", "index"))

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")
