# R-leaf

## Data

### Import 

Load the dataset, add column names, convert class index to string and remove observation index

```R
require(caret)
require(dplyr)
require(Hmisc)
require(corrplot)
data = read.csv("leaf.csv")
colnames(data) = c("Cl", "Sn", "Ec", "Ar", "El", "So", "Sc", "If", "Mid", "Lo", "Ai", "Ac", "Sm", "Tm", "Un", "En")
data$Cl = sprintf('Class %02s', data$Cl)
data = data[, -2]
data = data %>% select(-Cl,Cl)
data$Cl <- as.factor(data$Cl)
```
### Outliers

Create boxplot for each available class, find possible outliers and store their index

```R
input <- data
t_out_val <- c()
t_out_ind <- c()
t_sub_ind <- c()
t_tmp_ind <- c()
out_ind <-c()
for(i in unique(input$Cl)) {
  for(k in colnames(input)) {
    t_out_val <- append(t_out_val, boxplot.stats(subset(input, Cl == i)[,k])$out)
    t_sub_ind <- as.numeric(rownames(subset(input, Cl == i)))[1]
    t_tmp_ind <- which(subset(input, Cl == i)[,k] %in% c(t_out_val))
    t_out_ind <- append(t_out_ind, t_tmp_ind + t_sub_ind)
  }
  boxplot(filter(input, Cl == i)[1:14], main= i)
  mtext(paste("Possible outliers indexes: ", paste(unique(t_out_ind), collapse = ", ")))
  print(summary(subset(input, Cl == i)))
  t_out_val <- c()
  out_ind <- append(out_ind, t_out_ind)
  t_out_ind <- c()
  readline(prompt="Press [enter] to proceed")
}
out_ind <-unique(out_ind)
```

### Filter and Barplots

Remove outliers from the original dataset and create barplots of Class count distribution

```R
no_outliers <- filter(data, !row_number() %in% c(out_ind))
barplot(table(data$Cl),
        ylab="Count",
        ylim = c(0, 18),
        main = "Class Distribution: Original",
        las=2,
        col=rgb(0.2,0.4,0.6,0.6))
readline(prompt="Press [enter] to proceed")
barplot(table(no_outliers$Cl),
        ylab="count",
        ylim = c(0, 18),
        main = "Class Distribution: No outliers",
        las=2,
        col=rgb(0.2,0.4,0.6,0.6))
```

### Feature scaling and plots

```R
scaled <- as.data.frame(scale(data[1:14]))
scaled$Cl <- data$Cl
scaled_shape <-scaled[1:7]
scaled_shape$Cl <-scaled$Cl
scaled_texture <-scaled[8:15]
plot(scaled_shape)
readline(prompt="Press [enter] to proceed")
plot(scaled_texture)
```

### Corrplot

Compute correlation matrix to identify and remove features with low correlation repect to what we want to predict (Class) and high correlation with other features

```R
input <- data
input['Cl'] <- as.numeric(input$Cl)
corr <- cor(input)
corrplot(corr, method="color", addCoef.col = "black")
```

### Feature selection

Remove less relevant features from the dataset

```R
reduced <- subset(scaled, select = -c(Lo,Tm,En,Ac))
input <- reduced
input['Cl'] <- as.numeric(input$Cl)
corr <- cor(input)
corrplot(corr, method="color", addCoef.col = "black")
```

### Data split

Split "original" "scaled" and "reduced" dataset into train and test with a static division 70/30

```R
set.seed(4)
split <- createDataPartition(scaled$Cl, p = 0.7, list = FALSE)

train_data <- data[split,]
train_scaled <- scaled[split,]
train_reduced <- reduced[split,]
train_name <- c("original","scaled","reduced")
train_list <- list(train_data, train_scaled, train_reduced)
train_list <- setNames(train_list, train_name)

test_data <- data[-split,]
test_scaled <- scaled[-split,]
test_reduced <- reduced[-split,]
test_name <- c("original","scaled","reduced","original","scaled","reduced")
test_list <- list(test_data, test_scaled, test_reduced, test_data, test_scaled, test_reduced)
test_list <- setNames(test_list, test_name)
```

### Class distribution

Plot observations count for each class

```R
barplot(table(train_scaled$Cl),
        col=rgb(0.2,0.4,0.6,0.6),
        ylim = c(0, 20),
        main = "Class Distribution: Original",
        las=2)
```

### Balancing training dataset - Upsampling

```R
train_list_upsample <- list()
for(k in train_list) {
    set.seed(4)
    train_data_up <- upSample(k[,-ncol(k)], k$Cl, yname = "Cl")
    train_list_upsample <-append(train_list_upsample, list(train_data_up))
    table(train_data_up$Cl)
}
train_name_upsample <- c("original up","scaled up","reduced up")
train_list_upsample <- setNames(train_list_upsample, train_name_upsample)

# Append upsampled data to our list
train_list <- append(train_list, train_list_upsample)

# Plot upsampled class distribution
barplot(table(train_list_upsample[[2]]$Cl),
        col=rgb(0.2,0.4,0.6,0.6),
        ylim = c(0, 20),
        main = "Class Distribution: Upsampled",
        las=2)
```

### Train and predict

```R
model_list <- list()
cm_list <- list()
model_name <- c("nb", "rpart","knn","svmRadial","rf")
for(i in model_name) {
  for(k in seq_along(train_list)) {
    message(sprintf("Training model %s with %s data",i,names(train_list[k])))
    input <- train_list[k]
    model <- train(Cl~., train_list[[k]], method=i)
    model_list <- append(model_list, list(model))
    message(sprintf("Model Accuracy: %f",max(model$results["Accuracy"])))
    predicted <- predict(model,test_list[[k]])
    cm <- confusionMatrix(predicted, test_list[[k]]$Cl)
    cm_list <- append(cm_list, list(cm))
    message(sprintf("Test Accuracy: %f", cm$overall['Accuracy']))
  }
}

out_model_name <- c("nb o",   "nb s",   "nb r",
                    "nb o up","nb s up","nb r up",
                    "rt o",   "rt s",   "rt r",
                    "rt o up","rt s up","rt r up",
                    "kn o",   "kn s" ,  "kn r",
                    "kn o up","kn s up","kn r up",
                    "sv o",   "sv s",   "sv r",
                    "sv o up","sv s up","sv r up",
                    "rf o",   "rf s",   "rf r",                        
                    "rf o up","rf s up","rf r up")
                    
model_list <- setNames(model_list, out_model_name)
cm_list <- setNames(cm_list, out_model_name)

model_results <- resamples(model_list)
summary(model_results)
```
### k-folds CV ROC and AUC

```R
require(multiROC)
require(ggplot2)
for(k in seq_along(train_list)) {
  message(sprintf("Training Random Forest with %s data",names(train_list[k])))
   
  #Train best model using k-fold cross validation
  control <- trainControl(method="cv", number=10)
  rf_res = train(Cl~ ., train_list[[k]], method="rf", trControl=control)
  message(sprintf("Model Accuracy: %f",max(rf_res$results["Accuracy"])))
  rf_pred = predict(rf_res, test_list[[k]], type = 'prob')

  rf_pred <- data.frame(rf_pred)
  colnames(rf_pred) = c("1",   "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9", "10",
                        "11", "12", "13", "14", "15", "22", "23", "24", "25", "26",
                        "27", "28", "29", "30", "31", "32", "33", "34", "35", "36")
  colnames(rf_pred) <- paste(colnames(rf_pred), "_pred_RF")

  true_label <- dummies::dummy(test_list[[k]]$Cl, sep = ".")
  true_label <- data.frame(true_label)
  colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
  colnames(true_label) <- paste(colnames(true_label), "_true")
  final_df <- cbind(true_label, rf_pred)

  roc_res <- multi_roc(final_df, force_diag=T)
  plot_roc_df <- plot_roc_data(roc_res)
  #print(unlist(roc_res$AUC))

  print(ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=0.9) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                   colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black")))

  #Verify predictions of models
  rf_pred = predict(rf_res, test_list[[k]])
  cm <- confusionMatrix(rf_pred, test_list[[k]]$Cl)
  message(sprintf("Test Accuracy: %f", cm$overall['Accuracy']))
  readline(prompt="Press [enter] to proceed")
}
```

