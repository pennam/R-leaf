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
