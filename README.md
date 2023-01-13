# R-leaf

## Data

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
