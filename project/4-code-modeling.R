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

PATH.INPUT <- "dataset"
FILE.INPUT <- "kaggle-preprocessing.csv"
GLOB.ENVIR <- "models.RDATA"

# -- Script

LOAD.DATA = TRUE;

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("caret")) {
  install.packages("caret");
  library("caret");
}

# ------------------------------------------------------------------------------
# -- Functions -----------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c("NA"), encoding="UTF-8");

# ----------------------------------------
# -- Modeling ----------------------------
# ----------------------------------------

if(LOAD.DATA == TRUE) {
  
  load(GLOB.ENVIR);
  
} else {
  
  data$Job <- as.factor(data$Job);

  # train control
  
  train.ctrl <- trainControl(
    method = "cv",
    number = 10,
    verboseIter = TRUE
  );
  
  # model
  
  print(" - Logistic Regression")
  model.lr.rt <- Sys.time();
  model.lr <- train(Job ~ ., data, method="logreg", trControl=train.ctrl);
  model.lr.rt <- Sys.time() - model.lr.rt;
  model.lr.cm <- confusionMatrix(predict(model.lr, data), data$Job);
  
  print(" - Random Florest")
  model.rf.rt <- Sys.time();
  model.rf <- train(Job ~ ., data, method="rf", trControl=train.ctrl);
  model.rf.rt <- Sys.time() - model.rf.rt;
  model.rf.cm <- confusionMatrix(predict(model.rf, data), data$Job);
  
  print(" - XGBoost")
  model.xg.rt <- Sys.time();
  model.xg <- train(Job ~ ., data, method="xgbDART", trControl=train.ctrl);
  model.xg.rt <- Sys.time() - model.xg.rt;
  model.xg.cm <- confusionMatrix(predict(model.xg, data), data$Job);
  
  save.image(GLOB.ENVIR);
  
}

# wilcoxon signed-ranks test

wilcox.test(model.lr.cm$byClass, model.rf.cm$byClass, paired = TRUE, conf.int = TRUE);
wilcox.test(model.lr.cm$byClass, model.xg.cm$byClass, paired = TRUE, conf.int = TRUE);
wilcox.test(model.rf.cm$byClass, model.xg.cm$byClass, paired = TRUE, conf.int = TRUE);

# var importance

model.rf.varimp <- varImp(model.rf)$importance;
model.rf.varimp <- cbind(rownames(model.rf.varimp), model.rf.varimp);
model.rf.varimp <- as.data.frame(model.rf.varimp[order((model.rf.varimp$Overall), decreasing = TRUE),]);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT", "GLOB.ENVIR"));
rm(list = c("train.ctrl","data"));

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")
