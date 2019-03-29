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
PATH.OUTPUT <- "atividade-2/output"
FILE.INPUT.TRAIN <- "train.csv"

# -- Script

COLOR.HIST <- '#42B3D5';
COLOR.BARP <- '#D68C43';
COLOR.BOXP <- '#B1D643';
SAVE.OUTPUT <- TRUE;

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

if (!require("reshape2")) {
  install.packages("reshape2");
  library("reshape2");
}

# ------------------------------------------------------------------------------
# -- Main ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

# ----------------------------------------
# -- Read data ---------------------------
# ----------------------------------------

data <- read.csv(file.path(PATH.INPUT, FILE.INPUT.TRAIN), na.strings=c(""," ","na","(Other)","NA","0"));

# ----------------------------------------
# -- Explore quantitative data -----------
# ----------------------------------------

# -- Get quantitative features

data.quantitative <- sqldf("SELECT 
                              LotFrontage, LotArea, MasVnrArea, BsmtFinSF1, 
                              BsmtFinSF2, BsmtUnfSF, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, LowQualFinSF, 
                              GrLivArea, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, BedroomAbvGr, 
                              KitchenAbvGr, TotRmsAbvGrd, Fireplaces, GarageYrBlt, GarageCars, GarageArea, 
                              WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, 
                              MiscVal, SalePrice
                            FROM data");

# -- Loop through features

for (i in 1:ncol(data.quantitative)) {
  feature <- data.quantitative[,i];
  
  # Get NA's rate
  
  na.rate <- round((100*sum(is.na(feature))/nrow(data)), digits = 2);
  if(na.rate) {
    feature <- na.omit(feature);
  }
  
  # -- Histogram
  
  bw <- 2*IQR(feature)/(length(feature)^(1/3));
  if(bw < 1) {
    bw <- 1;
  }
  
  plot.var <-
    ggplot(as.data.frame(feature), aes(feature)) +
    geom_histogram(binwidth=bw, col="white", size=.1, fill=COLOR.HIST) +
    labs(title=paste0(colnames(data.quantitative[i]), " | Histograma"), subtitle = paste0("Taxa de NA = ", na.rate, "%"), y="Absolute frequency", x=colnames(data.quantitative[i])) +
    theme_minimal();
  print(plot.var);
  
  if (SAVE.OUTPUT) {
    #ggsave(paste0(PATH.OUTPUT,"/num/hist-", colnames(data.quantitative[i]), ".png"), device = "png");
  }
}

# ----------------------------------------
# -- Explore qualitative data ------------
# ----------------------------------------

# -- Get qualitative features

data.qualitative <- sqldf("SELECT
                            MSSubClass, MSZoning, Street, Alley, LotShape, LandContour, Utilities, 
                            LotConfig, LandSlope, Neighborhood, Condition1, Condition2, BldgType, 
                            HouseStyle, OverallQual, OverallCond, YearBuilt, YearRemodAdd, RoofStyle, 
                            RoofMatl, Exterior1st, Exterior2nd, MasVnrType, ExterQual, ExterCond, 
                            Foundation, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1, BsmtFinType2, 
                            Heating, HeatingQC, CentralAir, Electrical, KitchenQual, Functional, FireplaceQu, 
                            GarageType, GarageFinish, GarageQual, GarageCond, PavedDrive, PoolQC, Fence, 
                            MiscFeature, SaleType, SaleCondition, MoSold, YrSold
                           FROM data");

# -- Loop through features

for (i in 1:ncol(data.qualitative)) {
  feature <- data.qualitative[,i];
  
  # Get NA's rate
  
  na.rate <- round((100*sum(is.na(feature))/nrow(data)), digits = 2);
  if(na.rate) {
    feature <- na.omit(feature);
  }
  
  # -- Bar plot
  
  plot.var <- ggplot() +
    geom_bar(aes(feature), col="white", size=.1, fill=COLOR.BARP) +
    labs(title=paste0(colnames(data.qualitative[i]), " | Gráfico de barras"), subtitle = paste0("Taxa de NA = ", na.rate, "%"), y="Absolute frequency", x=colnames(data.qualitative[i])) +
    theme_minimal() +
    coord_flip();
  print(plot.var);
  
  if (SAVE.OUTPUT) {
    #ggsave(paste0(PATH.OUTPUT,"/cat/bar-", colnames(data.qualitative[i]), ".png"), device = "png");
  }
}

# ----------------------------------------
# -- Explore qualitative vs target data --
# ----------------------------------------

target  <- data.quantitative$SalePrice;

for (i in 1:ncol(data.qualitative)) {
  feature <- data.qualitative[,i];

  # Get NA's rate
  
  na.rate <- round((100*sum(is.na(feature))/nrow(data)), digits = 2);
  
  # -- Boxplot plot feature vs target
  
  plot.var <- ggplot(na.omit(melt(data.frame(feature, target), id.vars="feature")), aes(x=feature, y=value)) +
    geom_boxplot(aes(group=feature), width=0.8, fill=COLOR.BOXP) +
    labs(title=paste0(colnames(data.qualitative[i]), " vs SalePrice | Box Plot"), subtitle = paste0("Taxa de NA = ", na.rate, "%"), y="SalePrice (U$)", x=colnames(data.qualitative[i])) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=90, vjust=0.6, size = 10, hjust = 1));
  print(plot.var);
  
  if (SAVE.OUTPUT) {
    #ggsave(paste0(PATH.OUTPUT,"/num-cat/box-", colnames(data.qualitative[i]), "-vs-SalesPrice.png"), device = "png");
  }
  
}

# ----------------------------------------
# -- Explore quanti. vs quanti. data -----
# ----------------------------------------

data.quantitative.na.zero <- data.quantitative;
data.quantitative.na.zero[is.na(data.quantitative.na.zero)] <- 0
data.quantitative.corr <- round(cor(data.quantitative.na.zero), 2);

plot.var <- ggplot(melt(data.quantitative.corr), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  labs(title="Quantitative Attributes | Correlation matrix", subtitle="", y="", x="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, vjust=0.6, size = 10, hjust = 1));
print(plot.var);

if (SAVE.OUTPUT) {
  ggsave(paste0(PATH.OUTPUT,"/num-num/correlation-matrix.png"), device = "png");
}

# ------------------------------------------------------------------------------
# -- Clean ---------------------------------------------------------------------
# ------------------------------------------------------------------------------

rm(list = c("FILE.INPUT.TRAIN", "PATH.INPUT", "PATH.OUTPUT"));
rm(list = c("COLOR.BARP", "COLOR.BOXP", "COLOR.HIST", "SAVE.OUTPUT"));
rm(list = c("i", "bw", "na.rate", "feature", "target", "plot.var"));
rm(list = c("data.quantitative.na.zero", "data.quantitative.corr"))