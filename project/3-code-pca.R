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
FILE.INPUT  <- "kaggle-preprocessing.csv"
FILE.INPUT.STD <- "kaggle-preprocessing-student.csv"

# -- Script

cpv.limit <- 80;

# ------------------------------------------------------------------------------
# -- Library -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Functions -----------------------------------------------------------------
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT), na.strings=c("NA"), encoding="UTF-8");
data.std <- read.csv(file.path(PATH.INPUT, FILE.INPUT.STD), na.strings=c("NA"), encoding="UTF-8");

# ----------------------------------------
# -- PCA ---------------------------------
# ----------------------------------------

# - professionals

target <- data$Job;
data$Job <- NULL;
data <- as.data.frame(sapply(data, as.numeric));

data.pca <- princomp(data, cor=TRUE, scores=TRUE);
data.pca.var <- data.pca$sdev ^ 2;
data.pca.cpv <- cumsum((100 * data.pca.var) / sum(data.pca.var));

data.transformed <- t(data.pca$loadings[,1:which(data.pca.cpv > cpv.limit)[1]]) %*% t(data) ;
data.transformed <- data.frame(t(data.transformed));
colnames(data.transformed) <- colnames(data.pca$loadings)[1:which(data.pca.cpv > cpv.limit)[1]];
data.transformed <- cbind(target, data.transformed);
colnames(data.transformed)[1] <- "Job";
write.csv(data.transformed, file=file.path(PATH.OUTPUT, "kaggle-pca.csv"), row.names=FALSE);

# students

data.std <- as.data.frame(sapply(data.std, as.numeric));

data.transformed <- t(data.pca$loadings[,1:which(data.pca.cpv > cpv.limit)[1]]) %*% t(data.std) ;
data.transformed <- data.frame(t(data.transformed));
colnames(data.transformed) <- colnames(data.pca$loadings)[1:which(data.pca.cpv > cpv.limit)[1]];
write.csv(data.transformed, file=file.path(PATH.OUTPUT, "kaggle-pca-std.csv"), row.names=FALSE);

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT", "PATH.INPUT","PATH.OUTPUT"));
rm(list = c("cpv.limit", "data.pca.cpv", "data.pca.var","target"));

# ------------------------------------------------------------------------------
# -- Done ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

print("# ---------------------------------------------------------------------")
print("# -- Done -------------------------------------------------------------")
print("# ---------------------------------------------------------------------")