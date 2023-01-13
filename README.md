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
