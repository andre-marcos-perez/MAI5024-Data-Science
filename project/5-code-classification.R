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

GLOB.ENVIR <- "models.RDATA"
load(GLOB.ENVIR);
rm(list = c("FILE.INPUT", "PATH.INPUT", "GLOB.ENVIR", "data"));

SAVE.OUTPUT <- TRUE;
PATH.INPUT  <- "dataset"
PATH.OUTPUT <- "output"
FILE.INPUT  <- "kaggle-preprocessing-student.csv"

# -- Script

COLOR.BARP.DAT <- '#D64343';
COLOR.BARP.NON <- '#8C43D6';

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
  
  if (nQuestion == "13") {question.df <- data.frame(begin=2, end=14);}
  else if (nQuestion == "14") {question.df <- data.frame(begin=15,  end=23);}
  else if (nQuestion == "15") {question.df <- data.frame(begin=24,  end=28);}
  else if (nQuestion == "16") {question.df <- data.frame(begin=29,  end=44);}
  else if (nQuestion == "19") {question.df <- data.frame(begin=45,  end=61);}
  else if (nQuestion == "21") {question.df <- data.frame(begin=62,  end=72);}
  else if (nQuestion == "27") {question.df <- data.frame(begin=73,  end=90);}
  else if (nQuestion == "28") {question.df <- data.frame(begin=91,  end=132);}
  else if (nQuestion == "29") {question.df <- data.frame(begin=133, end=158);}
  else if (nQuestion == "30") {question.df <- data.frame(begin=159, end=181);}
  else {question.df <- NULL;}
  
  return(question.df)
}

AggregateHelperQuestions <- function(nQuestion, dataset, datasetName){
  
  question_agg <- data.frame(feature="", frequency=0);
  index.helper <- IndexHelperQuestions(nQuestion);
  
  for (index in index.helper$begin:index.helper$end) {
    print(paste0("select ", colnames(dataset[index]), ", count(*) feature from ", datasetName, " where ", colnames(dataset[index]), " <> '0' group by ", colnames(dataset[index])));
    result.set <- sqldf(paste0("select ", colnames(dataset[index]), ", count(*) feature from ", datasetName, " where ", colnames(dataset[index]), " <> '0' group by ", colnames(dataset[index])));
    result.set[1] <- colnames(dataset[index]);
    colnames(result.set) <- c("feature","frequency");
    result.set$frequency <- 100 * round(result.set$frequency / nrow(dataset), 4);
    question_agg <- rbind(question_agg, result.set);
  }
  
  result.set <- data.frame(feature="None/Other", frequency=0);
  
  for (index in 1:nrow(dataset)) {
    if (all(dataset[index.helper$begin:index.helper$end][index,] == 0)) {
      result.set$frequency <- result.set$frequency + 1;
    }
  }
  
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

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c("NA"), encoding="UTF-8");

classification <- as.data.frame(predict(model.rf, data));
colnames(classification) <- "Job";
data <- cbind(classification, data);

# Data Science Students
data_std_dat <- sqldf("select * from data where Job = '0'");
subtitle = paste0("Students classified as Data Scientists (", round(100 * (nrow(data_std_dat) / nrow(data)), 2),"% of students.)");

feature.agg <- AggregateHelperQuestions("13", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-ide.png", COLOR.BARP.DAT, "Integrated Development Environment", subtitle);

feature.agg <- AggregateHelperQuestions("14", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-notebook.png", COLOR.BARP.DAT, "Notebook", subtitle);

feature.agg <- AggregateHelperQuestions("15", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-cloud-service.png", COLOR.BARP.DAT, "Cloud Service", subtitle);

feature.agg <- AggregateHelperQuestions("16", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-language.png", COLOR.BARP.DAT, "Programming Language", subtitle);

feature.agg <- AggregateHelperQuestions("19", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-ml-framework.png", COLOR.BARP.DAT, "ML Framework", subtitle);

feature.agg <- AggregateHelperQuestions("21", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-visual-lib.png", COLOR.BARP.DAT, "Visualization Library", subtitle);

feature.agg <- AggregateHelperQuestions("27", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-cloud-product.png", COLOR.BARP.DAT, "Cloud Product", subtitle);

feature.agg <- AggregateHelperQuestions("28", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-ml-product.png", COLOR.BARP.DAT, "ML Product", subtitle);

feature.agg <- AggregateHelperQuestions("29", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-db-relational.png", COLOR.BARP.DAT, "Relational Database", subtitle);

feature.agg <- AggregateHelperQuestions("30", data_std_dat, "data_std_dat");
PlotHelperQuestions(feature.agg, "/classification/dat/bar-bigdata-product.png", COLOR.BARP.DAT, "Big Data & Analytics Product", subtitle);

# Non Data Science Students
data_std_non <- sqldf("select * from data where Job = '1'");
subtitle = paste0("Students classified as Non Data Scientists (", round(100 * (nrow(data_std_non) / nrow(data)), 2),"% of students.)");

feature.agg <- AggregateHelperQuestions("13", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-ide.png", COLOR.BARP.NON, "Integrated Development Environment", subtitle);

feature.agg <- AggregateHelperQuestions("14", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-notebook.png", COLOR.BARP.NON, "Notebook", subtitle);

feature.agg <- AggregateHelperQuestions("15", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-cloud-service.png", COLOR.BARP.NON, "Cloud Service", subtitle);

feature.agg <- AggregateHelperQuestions("16", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-language.png", COLOR.BARP.NON, "Programming Language", subtitle);

feature.agg <- AggregateHelperQuestions("19", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-ml-framework.png", COLOR.BARP.NON, "ML Framework", subtitle);

feature.agg <- AggregateHelperQuestions("21", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-visual-lib.png", COLOR.BARP.NON, "Visualization Library", subtitle);

feature.agg <- AggregateHelperQuestions("27", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-cloud-product.png", COLOR.BARP.NON, "Cloud Product", subtitle);

feature.agg <- AggregateHelperQuestions("28", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-ml-product.png", COLOR.BARP.NON, "ML Product", subtitle);

feature.agg <- AggregateHelperQuestions("29", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-db-relational.png", COLOR.BARP.NON, "Relational Database", subtitle);

feature.agg <- AggregateHelperQuestions("30", data_std_non, "data_std_non");
PlotHelperQuestions(feature.agg, "/classification/non/bar-bigdata-product.png", COLOR.BARP.NON, "Big Data & Analytics Product", subtitle);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT"));
#rm(list = c("train.ctrl","data"));

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")
