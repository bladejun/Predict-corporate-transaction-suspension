# Set Working Directory
setwd("C:/Users/yyj94/Desktop/네이버 DSC 프로젝트/DSC2019_예선_데이터")

# Import data
exchange <- read.csv(file = "st_data_exchange.csv", header = TRUE, fill = TRUE)
check_na(exchange)
foreginCharge <- read.csv(file = "st_data_foreignCharge.csv", header = TRUE, fill = TRUE)
check_na(foreginCharge)
goldForeign <- read.csv(file = "st_data_goldForeign.csv", header = TRUE, fill = TRUE)
check_na(goldForeign)
goldKorea <- read.csv(file = "st_data_goldKorea.csv", header = TRUE, fill = TRUE)
check_na(goldKorea)
goodsForeign <- read.csv(file = "st_data_goodsForeign.csv", header = TRUE, fill = TRUE)
check_na(goodsForeign)
itemChargeFull <- read.csv(file = "st_data_itemChargeFull.csv", header = TRUE, fill = TRUE)
check_na(itemChargeFull)
koreaIndex <- read.csv(file = "st_data_koreaIndex.csv", header = TRUE, fill = TRUE)
check_na(koreaIndex)
oilForeign <- read.csv(file = "st_data_oilForeign.csv", header = TRUE, fill = TRUE)
check_na(oilForeign)
oilKorea <- read.csv(file = "st_data_oilKorea.csv", header = TRUE, fill = TRUE)
check_na(oilKorea)

# Save as RData
#save.image(file = "DSC_Finance.RData")

# Load by RData
load(file = 'DSC_Finance.RData')

# Import library
library(tidyverse)

# Check NA Function
check_na <- function(data) {
  sapply(data, function(x) { sum(is.na(x))})
}

# trade stop proportion
itemChargeFull$trade_stop_yn %>% table() %>% prop.table() %>% round(digits = 2)
# Proportion of trade stop company is 99 percent not 1   

# Change data type : from int to date
itemChargeFull$date <- as.Date(str_sub(as.character(itemChargeFull$date), 3), "%y%m%d")

# Check data struct
str(itemChargeFull)

# select numeric data type column
numeric_cname <- itemChargeFull %>% purrr::keep(is.numeric)  

# Show number All
options(scipen = 100) 

# compare average of each data column by trade_stop_yn group(Y or N)
new <- data.frame()
for(i in 1:length(colnames(numeric_cname))) {
  yes_group <- itemChargeFull %>%
                  select(colnames(numeric_cname)[i], trade_stop_yn) %>% 
                  dplyr::filter(trade_stop_yn == "Y")
  no_group <- itemChargeFull %>%
                  select(colnames(numeric_cname)[i], trade_stop_yn) %>% 
                  dplyr::filter(trade_stop_yn == "N")
  yes <- data.frame(colname = colnames(numeric_cname)[i], trade_stop_yn = "Y", AVG = mean(yes_group[,1], na.rm = TRUE))
  no  <- data.frame(colname = colnames(numeric_cname)[i], trade_stop_yn = "N", AVG = mean(no_group[,1], na.rm = TRUE))
  new <- rbind(new, rbind(yes, no))
}

# Add trad_stop_yn column
numeric_cname$trade_stop_yn <- itemChargeFull$trade_stop_yn  


#########################################################################################
# change data to calculate relative difference
for(i in seq(from = 3, to = nrow(new), by =2)) {
  if(new[i, "AVG"] > new[i+1, "AVG"]) {
    prop = (new[i+1, "AVG"] * 100) / new[i, "AVG"]
    new[i+1, "AVG"] <- prop
    new[i, "AVG"] <- 100
  } else{
    prop = (new[i, "AVG"] * 100) / new[i+1, "AVG"]
    new[i, "AVG"] <- prop
    new[i+1, "AVG"] <- 100
  }
}

new[39,"AVG"] <- 1
new[40,"AVG"] <- 10.22756
new[41, "AVG"] <- 1
new[42, "AVG"] <- 27.58843

# Use ggplot2 to visualize the relative difference of each variables
new %>% 
  dplyr::filter(colname %in% colnames(data), colname != "date") %>% 
  ggplot(mapping = aes(x = trade_stop_yn, y = AVG, fill = trade_stop_yn)) +
  geom_bar(stat="identity") +
  facet_wrap(~colname, nrow = 5, ncol = 3)

# Check NA value
check_na(itemChargeFull)

# sales_increasing_rate has many NA
# Finance data has big difference by everday, So we can`t predict how value it is 
# So we remove data not fill

# EDA 
# remove column which has many NA but still maintain Y / N each proportion
# especially N proportion

check_na(data)
nrow(data)
data$trade_stop_yn %>% table()

data <- itemChargeFull %>% 
  dplyr::filter(!is.na(sales_increasing_rate))

data <- data %>% 
  dplyr::filter(!is.na(frgn_hold_ratio))

data <- data %>% 
  dplyr::filter(!is.na(prev_quant))

data <- data %>% 
  dplyr::filter(!is.na(net_income))

data <- data %>% 
  dplyr::filter(!is.na(operating_profit))

data <- data %>% 
  dplyr::filter(!is.na(market_sum))

data <- data %>% 
  dplyr::filter(!is.na(sales))

data <- data %>% 
  select(-c(acc_quant, high_val, low_val, open_val, operating_profit_increasing_rate,
            change_rate, change_val))

str(data)


###########################################

# Divide data 0.7 for trainset other for testset
index <- createDataPartition(data$trade_stop_yn, p = 0.7, list = FALSE)

# Make trainset, testset data
trainSet <- data[index, ]
testSet  <- data[-index, ]

# Check proportion y / n
trainSet$trade_stop_yn %>% table() %>% prop.table()
testSet$trade_stop_yn %>% table() %>% prop.table()

# Set seed
set.seed(seed = 123)

# Random Forest Model
fitRFC <- randomForest(x = trainSet[, -c(1:4,20)], 
                       y = trainSet[, 20], 
                       xtest = testSet[, -c(1:4,20)], 
                       ytest = testSet[, 20], 
                       ntree = 100, 
                       importance = TRUE, 
                       do.trace = 50, 
                       keep.forest = TRUE)

# Predict values
trPred <- fitRFC$predicted

# Real values
trReal <- trainSet$trade_stop_yn

# Print confusionmatrix
confusionMatrix(data = trPred, reference = trReal, positive = 'Y')

# Draw ROC Curve
getROC(trReal, trPred)

# Use ggplot2 to visualize the index how predict well by Random Forest Model
getCf <- function(matrix) {
  value <- matrix$byClass[1:4]
  name <- c("Sensitivity", "Specificity", "Pos_Pred", "Neg_Pred")
  df <- data.frame(name, value)
  ggplot(df, mapping = aes(x = reorder(name, -value), y = value, fill = name)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(value, digits = 4)), vjust = -0.2) +
    theme_wsj()
}
getCf(a)

# variable importance
importance    <- importance(fitRFC)

# Make data.frame
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'], digits = 0))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Variables)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip() + 
  theme_wsj() 
