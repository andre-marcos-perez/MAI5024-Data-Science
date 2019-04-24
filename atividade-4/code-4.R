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
FILE.INPUT.TRAIN <- "iris.csv"

# -- Script

NEIGHBORS <- c(1, 3, 6, 9);
set.seed(1234);

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

if (!require("caret")) {
  install.packages("caret");
  library("caret");
}

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Read data ---------------------------
# ----------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT.TRAIN));
colnames(data) <- c("sLength", "sWidth", "pLength", "pWidth", "class");

# ----------------------------------------
# -- Varying K ---------------------------
# ----------------------------------------

for(k in NEIGHBORS){
  
  tuneGrid <- expand.grid(
    kmax=k,
    distance=2,
    kernel=c('optimal')
  );
  
  print("# ------------------------------------------------------------------------------");
  print("# -- 10x random subsampling ----------------------------------------------------");
  print("# ------------------------------------------------------------------------------");
  
  train.ctrl <- trainControl(
    method = "repeatedcv",
    repeats =  10
  );
  
  model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
  model.cm <- caret::confusionMatrix(predict(model, data), data$class);
  
  print(paste0("# -- k = ", k, " ---------------------------------------------------------------------"));
  print("# ------------------------------------------------------------------------------");
  print(".");
  print(model.cm);
  print(".");
  
  print("# ------------------------------------------------------------------------------");
  print("# -- 10-fold cross validation --------------------------------------------------");
  print("# ------------------------------------------------------------------------------");
  
  train.ctrl <- trainControl(
    method = "cv",
    number = 10
  );
  
  model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
  model.cm <- caret::confusionMatrix(predict(model, data), data$class);
  
  print(paste0("# -- k = ", k, " ---------------------------------------------------------------------"));
  print("# ------------------------------------------------------------------------------");
  print(".");
  print(model.cm);
  print(".");
  
  print("# ------------------------------------------------------------------------------");
  print("# -- Leave-one-out -------------------------------------------------------------");
  print("# ------------------------------------------------------------------------------");
  
  train.ctrl <- trainControl(
    method = "LOOCV",
    number = 10
  );
  
  model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
  model.cm <- caret::confusionMatrix(predict(model, data), data$class);
  
  print(paste0("# -- k = ", k, " ---------------------------------------------------------------------"));
  print("# ------------------------------------------------------------------------------");
  print(".");
  print(model.cm);
  print(".");
  
}

# ----------------------------------------
# -- Fixed K -----------------------------
# ----------------------------------------

train.ctrl <- trainControl(
  method = "LOOCV",
  number = 10,
);

print("# ------------------------------------------------------------------------------");
print("# -- Minkowski distance with r=1 -----------------------------------------------");
print("# ------------------------------------------------------------------------------");

tuneGrid <- expand.grid(
  kmax=6,
  distance=1,
  kernel=c('optimal')
);

model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);

print(".");
print(model.cm);
print(".");

print("# ------------------------------------------------------------------------------");
print("# -- Minkowski distance with r=2 -----------------------------------------------");
print("# ------------------------------------------------------------------------------");

tuneGrid <- expand.grid(
  kmax=6,
  distance=2,
  kernel=c('optimal')
);

model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);

print(".");
print(model.cm);
print(".");

print("# ------------------------------------------------------------------------------");
print("# -- Minkowski distance with r=3 -----------------------------------------------");
print("# ------------------------------------------------------------------------------");

tuneGrid <- expand.grid(
  kmax=6,
  distance=3,
  kernel=c('optimal')
);

model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);

print(".");
print(model.cm);
print(".");

print("# ------------------------------------------------------------------------------");
print("# -- Minkowski distance with r=4 -----------------------------------------------");
print("# ------------------------------------------------------------------------------");

tuneGrid <- expand.grid(
  kmax=6,
  distance=4,
  kernel=c('optimal')
);

model <- caret::train(class ~ ., data, method="kknn", trControl=train.ctrl, tuneGrid=tuneGrid);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);

print(".");
print(model.cm);
print(".");

print("# ------------------------------------------------------------------------------");
print("# -- Multinomial Logistic Regression  ------------------------------------------");
print("# ------------------------------------------------------------------------------");

model <- caret::train(class ~ ., data, method="multinom", trControl=train.ctrl);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);
 
print(".");
print(model.cm);
print(".");

print("# ------------------------------------------------------------------------------");
print("# -- Random Forest -------------------------------------------------------------");
print("# ------------------------------------------------------------------------------");

model <- caret::train(class ~ ., data, method="rf", trControl=train.ctrl);
model.cm <- caret::confusionMatrix(predict(model, data), data$class);

print(".");
print(model.cm);
print(".");

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT.TRAIN", "PATH.INPUT", "NEIGHBORS"));
rm(list = c("model", "model.cm", "k", "train.ctrl"))
