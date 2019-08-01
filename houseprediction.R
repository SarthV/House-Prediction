library(tidyverse)
library(reshape2)
housing = read.csv('./housing.csv')
head(housing)
summary(housing)
colnames(housing) 
par(mfrow = c(2,5))
ggplot(data = melt(housing), mapping = aes(x = value)) + 
  geom_histogram(bin = 30) + facet_wrap(~variable, scales = 'free_x')
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)
housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households
drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]
head(housing)
categories = unique(housing$ocean_proximity)
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)
for(cat in categories){
  cat_housing[, cat] = rep(0, times= nrow(cat_housing))
}
head(cat_housing)
for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

head(cat_housing)
cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))

tail(cat_housing)
drop = c('ocean_proximity', 'median_house_value')
housing_num = housing[,!names(housing) %in% drop]
head(housing_num)
scaled_housing_num = scale(housing_num)
head(scaled_housing_num)
cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)                  
head(cleaned_housing)
set.seed(1023)
sample = sample.int(n = nrow(cleaned_housing), size = floor(0.8*nrow(cleaned_housing)), replace = FALSE)
train = cleaned_housing[sample, ]
test = cleaned_housing[-sample, ]
head(test)
head(train)
nrow(train) + nrow(test) == nrow(cleaned_housing)
library('boot')
?cv.glm
?glm  
glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
head(glm_house)
k_fold_error = cv.glm(cleaned_housing, glm_house, K = 5)
head(k_fold_error)
k_fold_error$delta     
glm_cv_rmse = sqrt(k_fold_error$delta)[1]
glm_cv_rmse
glm_house$coefficients
library('randomForest')
?randomForest
names(train)
set.seed(1738)
train_y = train[, "median_house_value"]
train_x = train[, names(train)!= "median_house_value"]
rf_model = randomForest(train_x, y = train_y, data = train, ntree = 500, importance = TRUE)
names(rf_model)
rf_model$importance
oob_prediction = predict(rf_model) #leaving out a data source forces OOB predictions
#you may have noticed that this is avaliable using the $mse in the model options.
#but this way we learn stuff!
train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse
test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse