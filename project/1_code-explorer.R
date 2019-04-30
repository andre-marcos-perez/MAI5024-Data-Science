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

SAVE.OUTPUT <- TRUE;
PATH.INPUT  <- "dataset"
PATH.OUTPUT <- "output"
FILE.INPUT  <- "kaggle-etl.csv"

# -- Script

COLOR.BARP.DAT <- '#42B3D5';
COLOR.BARP.NON <- '#B1D643';

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("sqldf")) {
  install.packages("sqldf");
  library("sqldf");
}

if (!require("ggplot2")) {
  install.packages("ggplot2");
  library("ggplot2");
}

# ------------------------------------------------------------------------------
# -- Functions -----------------------------------------------------------------
# ------------------------------------------------------------------------------

IndexHelperQuestions <- function(nQuestion){
  
  question.df <- data.frame();
    
  if (nQuestion == "13") {question.df <- data.frame(begin=7, end=19);}
  else if (nQuestion == "14") {question.df <- data.frame(begin=20,  end=28);}
  else if (nQuestion == "15") {question.df <- data.frame(begin=29,  end=33);}
  else if (nQuestion == "16") {question.df <- data.frame(begin=34,  end=49);}
  else if (nQuestion == "19") {question.df <- data.frame(begin=50,  end=66);}
  else if (nQuestion == "21") {question.df <- data.frame(begin=67,  end=77);}
  else if (nQuestion == "27") {question.df <- data.frame(begin=81,  end=98);}
  else if (nQuestion == "28") {question.df <- data.frame(begin=99,  end=139);}
  else if (nQuestion == "29") {question.df <- data.frame(begin=141, end=166);}
  else if (nQuestion == "30") {question.df <- data.frame(begin=167, end=189);}
  else {question.df <- NULL;}
  
  return(question.df)
}

AggregateHelperQuestions <- function(nQuestion, dataset, datasetName){
  
  question_agg <- data.frame(feature="", frequency=0);
  index.helper <- IndexHelperQuestions(nQuestion);
  
  for (index in index.helper$begin:index.helper$end) {
    print(paste0("select ", colnames(dataset[index]), ", count(*) feature from ", datasetName, " where ", colnames(dataset[index]), " <> 'NA' group by ", colnames(dataset[index])));
    result.set <- sqldf(paste0("select ", colnames(dataset[index]), ", count(*) feature from ", datasetName, " where ", colnames(dataset[index]), " <> 'NA' group by ", colnames(dataset[index])));
    colnames(result.set) <- c("feature","frequency");
    result.set$frequency <- 100 * round(result.set$frequency / nrow(dataset), 4);
    question_agg <- rbind(question_agg, result.set);
  }
  
  result.set <- data.frame(feature="None/Other", frequency=0);
  
  for (index in 1:nrow(dataset)) {
    if (all(is.na(dataset[index.helper$begin:index.helper$end][index,]))) {
      result.set$frequency <- result.set$frequency + 1;
    }
  }
  
  result.set$frequency <- 100 * round(result.set$frequency / nrow(dataset), 4);
  question_agg <- rbind(question_agg, result.set);
  question_agg <- sqldf("select * from question_agg where rowid > 1");
  
  return(question_agg);
}

AggregateMonoHelperQuestions <- function(Question, dataset, datasetName){
  
  question_agg <- data.frame(feature="", frequency=0);
  
  print(paste0("select ", Question, ", count(*) feature from ", datasetName, " where ", Question, " <> 'NA' group by ", Question));
  result.set <- sqldf(paste0("select ", Question, ", count(*) feature from ", datasetName, " where ", Question, " <> 'NA' group by ", Question));
  colnames(result.set) <- c("feature","frequency");
  result.set$frequency <- 100 * round(result.set$frequency / nrow(dataset), 4);
  question_agg <- rbind(question_agg, result.set);
  question_agg <- sqldf("select * from question_agg where rowid > 1");
  
  return(question_agg);
}

PlotHelperQuestions <- function(data.feature, filename, color, title, subtitle){
  
  plot.var <- ggplot(data=data.feature, aes(x=feature, y=frequency)) +
    geom_bar(col="white", size=.1, fill=color, stat="identity") +
    geom_text(aes(label=frequency), vjust=-0.2, size=2.5) +
    labs(title=title, subtitle=subtitle , y="Relative frequency (%)", x="") +
    ylim(0,100) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust = 1));
  print(plot.var);
  
  if (SAVE.OUTPUT) {
    ggsave(paste0(PATH.OUTPUT, filename), device = "png", width = 9.8, height = 5.68);
  }
}

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c("NA"), encoding="latin1");

# ----------------------------------------
# -- explorer------------------------------
# ----------------------------------------

# eliminate non male or female rows

data <- sqldf("select * from data where Gender in ('Male','Female')");

# get young (18-29 years old) and new (0-2 years of experience) data related professionals 

data_prf_dat <- sqldf("select * from data where Age in ('18-21','22-24','25-29') and (Experience = '0-1' or Experience = '1-2') and Job like 'Data%'")
subtitle = paste0("Young (18-29 years old) and new (0-2 years of experience) data related professionals (", round(100 * (nrow(data_prf_dat) / nrow(data)), 2),"% answers)");

feature.agg <- AggregateMonoHelperQuestions("Gender", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-gender.png", COLOR.BARP.DAT, "Gender", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Age", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-age.png", COLOR.BARP.DAT, "Age", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Education", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-education.png", COLOR.BARP.DAT, "Education", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Undergraduate", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-undergrad.png", COLOR.BARP.DAT, "Undergraduate", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Job", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-job.png", COLOR.BARP.DAT, "Job", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Experience", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-exp.png", COLOR.BARP.DAT, "Experience", subtitle);

feature.agg <- AggregateMonoHelperQuestions("TimeCoding", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-time-coding.png", COLOR.BARP.DAT, "TimeCoding", subtitle);

feature.agg <- AggregateMonoHelperQuestions("DataCoding", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-data-coding.png", COLOR.BARP.DAT, "DataCoding", subtitle);

feature.agg <- AggregateMonoHelperQuestions("MLYearCoding", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-ml-time-coding.png", COLOR.BARP.DAT, "MLYearCoding", subtitle);

feature.agg <- AggregateHelperQuestions("13", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-ide.png", COLOR.BARP.DAT, "Integrated Development Environment", subtitle);

feature.agg <- AggregateHelperQuestions("14", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-notebook.png", COLOR.BARP.DAT, "Notebook", subtitle);

feature.agg <- AggregateHelperQuestions("15", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-cloud-service.png", COLOR.BARP.DAT, "Cloud Service", subtitle);

feature.agg <- AggregateHelperQuestions("16", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-language.png", COLOR.BARP.DAT, "Programming Language", subtitle);

feature.agg <- AggregateHelperQuestions("19", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-ml-framework.png", COLOR.BARP.DAT, "ML Framework", subtitle);

feature.agg <- AggregateHelperQuestions("21", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-visual-lib.png", COLOR.BARP.DAT, "Visualization Library", subtitle);

feature.agg <- AggregateHelperQuestions("27", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-cloud-product.png", COLOR.BARP.DAT, "Cloud Product", subtitle);

feature.agg <- AggregateHelperQuestions("28", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-ml-product.png", COLOR.BARP.DAT, "ML Product", subtitle);

feature.agg <- AggregateHelperQuestions("29", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-db-relational.png", COLOR.BARP.DAT, "Relational Database", subtitle);

feature.agg <- AggregateHelperQuestions("30", data_prf_dat, "data_prf_dat");
PlotHelperQuestions(feature.agg, "/explorer/dat/bar-bigdata-product.png", COLOR.BARP.DAT, "Big Data & Analytics Product", subtitle);

# get young (18-29 years old) and new (0-2 years of experience) non-data related professionals 

data_prf_non <- sqldf("select * from data where Age in ('18-21','22-24','25-29') and (Experience = '0-1' or Experience = '1-2') and Job <> 'Student' and Job not like 'Data%'")
subtitle <- paste0("Young (18-29 years old) and new (0-2 years of experience) non data related professionals (", round(100 * (nrow(data_prf_non) / nrow(data)), 2),"% answers)");

feature.agg <- AggregateMonoHelperQuestions("Gender", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-gender.png", COLOR.BARP.NON, "Gender", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Age", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-age.png", COLOR.BARP.NON, "Age", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Education", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-education.png", COLOR.BARP.NON, "Education", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Undergraduate", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-undergrad.png", COLOR.BARP.NON, "Undergraduate", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Job", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-job.png", COLOR.BARP.NON, "Job", subtitle);

feature.agg <- AggregateMonoHelperQuestions("Experience", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-exp.png", COLOR.BARP.NON, "Experience", subtitle);

feature.agg <- AggregateMonoHelperQuestions("TimeCoding", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-time-coding.png", COLOR.BARP.NON, "TimeCoding", subtitle);

feature.agg <- AggregateMonoHelperQuestions("DataCoding", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-data-coding.png", COLOR.BARP.NON, "DataCoding", subtitle);

feature.agg <- AggregateMonoHelperQuestions("MLYearCoding", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-ml-time-coding.png", COLOR.BARP.NON, "MLYearCoding", subtitle);

feature.agg <- AggregateHelperQuestions("13", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-ide.png", COLOR.BARP.NON, "Integrated Development Environment", subtitle);

feature.agg <- AggregateHelperQuestions("14", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-notebook.png", COLOR.BARP.NON, "Notebook", subtitle);

feature.agg <- AggregateHelperQuestions("15", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-cloud-service.png", COLOR.BARP.NON, "Cloud Service", subtitle);

feature.agg <- AggregateHelperQuestions("16", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-language.png", COLOR.BARP.NON, "Programming Language", subtitle);

feature.agg <- AggregateHelperQuestions("19", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-ml-framework.png", COLOR.BARP.NON, "ML Framework", subtitle);

feature.agg <- AggregateHelperQuestions("21", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-visual-lib.png", COLOR.BARP.NON, "Visualization Library", subtitle);

feature.agg <- AggregateHelperQuestions("27", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-cloud-product.png", COLOR.BARP.NON, "Cloud Product", subtitle);

feature.agg <- AggregateHelperQuestions("28", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-ml-product.png", COLOR.BARP.NON, "ML Product", subtitle);

feature.agg <- AggregateHelperQuestions("29", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-db-relational.png", COLOR.BARP.NON, "Relational Database", subtitle);

feature.agg <- AggregateHelperQuestions("30", data_prf_non, "data_prf_non");
PlotHelperQuestions(feature.agg, "/explorer/non/bar-bigdata-product.png", COLOR.BARP.NON, "Big Data & Analytics Product", subtitle);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT", "PATH.OUTPUT", "SAVE.OUTPUT"));
rm(list = c("COLOR.BARP.DAT", "COLOR.BARP.NON"));
rm(list = c("feature.agg","subtitle"))

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")
