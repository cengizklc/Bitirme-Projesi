
library(readxl)
car_price_prediction <- read_excel("C:/Users/cengz/Desktop/car_price_prediction.xlsx")

str(car_price_prediction)

summary(car_price_prediction)

car_price <- car_price_prediction

sum(duplicated(car_price$ID)) #### Tekrar eden veriler var mý diye kontrol edildi

car_price<- car_price[!duplicated(car_price$ID), ]  #tekrar eden veriler çýkartýldý

car_price<- car_price[,-1]# ID deðiþkeniyle iþimiz bitti ve çýkardýk

View(car_price)

set.seed(123)
cp<-data.frame(car_price)
View(cp)
dim(cp)
sum(is.na(cp))


#### VERÝ ÖNÝÞLEME #######
cp$Levy
cp$Levy[cp$Levy=="-"]<- 0                 # harç ödemesi olmayan araçlara 0 atandý
cp$Levy=as.numeric(cp$Levy)
###########################
str(cp$Doors)
                                                                        library(tidyverse)
unique(cp$Doors)
cp$Doors[cp$Doors=="04-May"] <- 4

cp$Doors[cp$Doors=="02-Mar"] <- 2
cp$Doors[cp$Doors==">5"] <- 5
cp$Doors=as.numeric(cp$Doors)




#Price deðiþkeninin boxplotý incelendi

library(ggplot2)

ggplot(cp, aes(x = "", y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_text(aes(label = paste0("Q1: ", quantile(Price)[2])),
            x = 0.8, y = quantile(cp$Price)[2], vjust = -1) +
  geom_text(aes(label = paste0("Median: ", median(Price))),
            x = 0.8, y = median(cp$Price), vjust = -1) +
  geom_text(aes(label = paste0("Q3: ", quantile(Price)[4])),
            x = 0.8, y = quantile(cp$Price)[4], vjust = -1) +
  geom_text(aes(label = paste0("IQR: ", IQR(Price))),
            x = 0.8, y = median(cp$Price), vjust = -2) +
  labs(title = "Boxplot of Price", x = "", y = "Price")






#Outlier sorunu gözlendi 





# Calculate the IQR
Q1 <- quantile(cp$Price, 0.25, na.rm = TRUE)
Q3 <- quantile(cp$Price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR




outliers <- cp[cp$Price < lower_bound | cp$Price > upper_bound, ]
outlier_count <- nrow(outliers)

cp <- cp[cp$Price >= lower_bound & cp$Price <= upper_bound, ]
dim(cp)






cp <- cp[cp$Price > 600, ]



#outlierlar çýkarýldý




ggplot(cp, aes(x = "", y = Price)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_text(aes(label = paste0("Q1: ", quantile(Price)[2])),
            x = 0.8, y = quantile(cp$Price)[2], vjust = -1) +
  geom_text(aes(label = paste0("Median: ", median(Price))),
            x = 0.8, y = median(cp$Price), vjust = -1) +
  geom_text(aes(label = paste0("Q3: ", quantile(Price)[4])),
            x = 0.8, y = quantile(cp$Price)[4], vjust = -1) +
  geom_text(aes(label = paste0("IQR: ", IQR(Price))),
            x = 0.8, y = median(cp$Price), vjust = -2) +
  labs(title = "Boxplot of Price", x = "", y = "Price")





#price deðiþkeninde outlier sorununun büyük ölçüde çözüldüðü yorumlandý




View(cp)





################ modeller arasýnda yetersiz veri kontrolu yapalým




model_tablosu <- table(cp$Model)  

#Bazý modellerin veri sayýsýnýn yalnýzca 1 olduðu görüldü bu sebeple en çok geçen modeller üzerine yoðunlaþmaya karar verildi



en_cok_gecen_model50 <- names(sort(model_tablosu, decreasing = TRUE)[1:50])

cp <- cp[cp$Model%in%en_cok_gecen_model50,]
dim(cp)




## mileage deðiþkeninin km yazýsý kaldýrýlýp numeric hale getirildi
## 
str(cp$Mileage)
mileage_chr_values <- strsplit(cp$Mileage,split=" " ,fixed = T)       
mileage_count <- length(mileage_chr_values)
new_mileage_values <- vector()
for (i in 1:mileage_count){
  new_mileage_values[i] <- as.numeric(mileage_chr_values[[i]][1])
}
cp$Mileage <- new_mileage_values
cp$Mileage <- new_mileage_values
str(cp$Mileage)

########################################################################################

                                                                        













library(ggplot2)

ggplot(cp, aes(x = "", y = Mileage)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Mileage", x = "", y = "Mileage")

#Mileage deðiþkeninde de outlier sorunu gözlendi




# Calculate the IQR
Q1 <- quantile(cp$Mileage, 0.25, na.rm = TRUE)
Q3 <- quantile(cp$Mileage, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR




outliers <- cp[cp$Mileage < lower_bound | cp$Mileage > upper_bound, ]


cp <- cp[cp$Mileage <= upper_bound, ]  #outlierlardan yalnýzca upper bound üstündekiler çýkarýldý sebebi ise 0 kilometre aracýn mümkün olup 20 milyon gibi bir kilometreye sahip araclarýn mümkün olmayacaðýydý


dim(cp)




library(ggplot2)

# Q1, Q2 (median) ve IQR hesaplamalarý
q1 <- quantile(cp$Mileage, 0.25)
q2 <- median(cp$Mileage)
iqr <- IQR(cp$Mileage)

ggplot(cp, aes(x = "", y = Mileage)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  geom_text(aes(label = paste("Q1:", q1, "\nQ2:", q2, "\nIQR:", iqr)), 
            x = 1, y = q2, vjust = 1.5, color = "black") +
  labs(title = "Boxplot of Mileage", x = "", y = "Mileage")


#tekar boxplot gözlendý



library(ggplot2)

ggplot(cp, aes(x = "", y = Levy)) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  labs(title = "Boxplot of Levy", x = "", y = "Levy")



library(dplyr)




# engine.volume numeric hale getirildi
engine <- strsplit(cp$Engine.volume,split=" " ,fixed = T)       
engine2 <- length(engine)
engine3 <- vector()
for (i in 1:engine2){
  engine3[i] <- as.numeric(engine[[i]][1])
}
cp$Engine.volume <- engine3
cp$Engine.volume<- engine3

str(cp)

## color 

renk_tablosu <- table(cp$Color)
en_cok_gecen_renkler <- names(sort(renk_tablosu, decreasing = TRUE)[1:14])
cp <- cp[cp$Color%in%en_cok_gecen_renkler,]


dim(cp)

### Category
ct_tablosu <- table(cp$Category)

en_cok_kategori <- names(sort(ct_tablosu , decreasing = TRUE)[1:8])
cp <- cp[cp$Category%in%en_cok_kategori,]


### Fueltype

ft_tablosu <- table(cp$Fuel.type)

en_cok_ft <- names(sort(ft_tablosu, decreasing = TRUE)[1:4])
cp <- cp[cp$Fuel.type%in%en_cok_ft,]


#Gearboxtype


gb_tablosu <- table(cp$Gear.box.type)
en_cok_gb <- names(sort(gb_tablosu , decreasing = TRUE)[1:3])
cp <- cp[cp$Gear.box.type%in%en_cok_gb,]





###############################
dim(cp)






library(baguette)
library(bonsai)
library(tidymodels)
library(tidyverse)
library(tidymodels)


summary(cp)




                                                                   
                                                                   library(magrittr)
                                                                    library(parsnip)
                                                                    library(workflows)
                                                                     library(yardstick)
                                                                      library(rsample)

                                                             
                                                            library(yardstick)
                                                             library(rsample)








################################################# DATA VÝSULATÝON ###################################################






library(plotly)
library(ggplot2)


# Price Histogram oluþturma
price_hist <- ggplot(cp, aes(x = Price)) +
  geom_histogram(fill = "green", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean_val, color = "Mean"), linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median_val, color = "Median"), linetype = "dotted", size = 1.2) +
  labs(x = "Price", y = "Count", title = "Histogram of Price") +
  theme_minimal() +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue")) +
  annotate("text", x = max(cp$Price), y = 1000, label = paste("Mean:", formatC(mean_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 800, label = paste("Median:", formatC(median_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 600, label = paste("Max:", formatC(max_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 400, label = paste("Min:", formatC(min_val, format = "f")), hjust = 1)

p_hist




# Mileage Histogram oluþturma
# 
Mileage_hist <- ggplot(cp, aes(x = Mileage )) +
  geom_histogram(fill = "green", color = "black", bins = 30) +
  geom_vline(aes(xintercept = mean_val, color = "Mean"), linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = median_val, color = "Median"), linetype = "dotted", size = 1.2) +
  labs(x = "Mileage ", y = "Count", title = "Histogram of Mileage ") +
  theme_minimal() +
  scale_color_manual(values = c("Mean" = "red", "Median" = "blue")) +
  annotate("text", x = max(cp$Price), y = 1000, label = paste("Mean:", formatC(mean_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 800, label = paste("Median:", formatC(median_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 600, label = paste("Max:", formatC(max_val, format = "f")), hjust = 1) +
  annotate("text", x = max(cp$Price), y = 400, label = paste("Min:", formatC(min_val, format = "f")), hjust = 1)

Mileage_hist




#markalarýn ortalama fiyatý barplot





average_price <- cp %>% group_by(Manufacturer) %>% summarise(Average_Price = mean(Price))

price_brand <- plot_ly(average_price, x = ~Manufacturer, y = ~Average_Price, type = 'bar', color = ~Manufacturer,
                       colors = c("#764AF1","#F2F2F2","#F32424","#333C83","#F24A72","#FDAF75","#EAEA7F","#FF5F00","#B20600","#00092C")) %>%
  layout(title = 'Average Price by Manufacturer', xaxis = list(title = 'Manufacturer'), yaxis = list(title = 'Average Price'))

price_brand


#MODELLERÝN ORTALAMA FÝYATLARI BARPLOT

library(ggplot2)

avg_price_by_model <- cp %>%
  group_by(Model) %>%
  summarise(avg_price = mean(Price))

p_model <- ggplot(avg_price_by_model, aes(x = Model, y = avg_price, fill = Model)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Model", y = "Average Price", title = "Average Price by Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p_model





#ÜRETÝM YILLARINA GÖRE ORTALAMA FÝYATLAR BAR
library(dplyr)

mean_prices <- cp %>%
  group_by(Prod..year) %>%
  summarise(mean_price = mean(Price))

p_Prod..year<- ggplot(mean_prices, aes(x = Prod..year, y = mean_price, fill = Prod..year)) +
  geom_col(color = "black", position = "dodge") +
  labs(x = "Prod..year", y = "Mean Price", title = "Prod..year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))







#RENGE GÖRE ORTALAMA FÝYAT BAR


mean_prices <- cp %>%
  group_by(Color) %>%
  summarise(mean_price = mean(Price))

p_Prod..year<- ggplot(mean_prices, aes(x = Color, y = mean_price, fill = Color)) +
  geom_col(color = "black", position = "dodge") +
  labs(x = "Color", y = "Mean Price", title = "Color") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#Category göre ortalama fiyat


mean_prices <- cp %>%
  group_by(Category) %>%
  summarise(mean_price = mean(Price))

p_Prod..year<- ggplot(mean_prices, aes(x = Category, y = mean_price, fill = Category)) +
  geom_col(color = "black", position = "dodge") +
  labs(x = "Category", y = "Mean Price", title = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Yakýt tipi daðýlýmý bar

p_fuel <- ggplot(cp, aes(x = Fuel.type, fill = Fuel.type)) +
  geom_bar(color = "black") +
  labs(x = "Fuel Type", y = "Count", title = "Distribution of Cars by Fuel Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p_fuel



# Drive Wheels için Pie Chart
p_drive_wheels <- cp %>%
  count(Drive.wheels) %>%
  plot_ly(labels = ~Drive.wheels, values = ~n, type = "pie",
          textinfo = "label+percent",
          marker = list(colors = rainbow(nrow(.), alpha = 0.7))) %>%
  layout(title = "Distribution of Cars by Drive Wheels")


p_drive_wheels 

# KORELASYON

library(ggplot2)
library(reshape2)

cor_matrix <- cor(cp[, c("Price", "Levy", "Prod..year", "Engine.volume", "Mileage", "Cylinders", "Doors", "Airbags")])

melted_cor <- melt(cor_matrix)
melted_cor$label <- round(melted_cor$value, 2)

ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = label), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Correlation Matrix", x = "Variables", y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











##################################################################################################################
#                                       MODEL KURMA ÝÞLEMLERÝ 
###################################################################################################################


library(tidymodels)

dim(cp)
# TEST TRAÝN DATA AYIRMA 
set.seed(123)
split <- initial_split(cp, prop = 0.8)
train <- training(split)
test <- testing(split)
auto_cv <- vfold_cv(train, v = 10)



rec <- recipe(Price ~ ., data = cp) %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())



library(kernlab)




# Support Vector Machines##
svm_model <- 
  svm_rbf(mode = "regression",
          cost = tune(),
          rbf_sigma = tune(),
          engine = "kernlab"
  )

svm_wf <-
  workflow() %>%
  add_model(svm_model) %>% 
  add_recipe(rec)
svm_wf




svm_results <-
  svm_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae))

svm_results %>%
  collect_metrics()



# Final Hyperparameter
param_final <- svm_results %>%
  select_best(metric = "rmse")

svm_wf <- svm_wf %>%
  finalize_workflow(param_final)
svm_wf




# last fit
svm_fit <- svm_wf %>%
  last_fit(split)



# Test Data Predictions
test_performance <- svm_fit %>% collect_predictions()
test_performance

df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "SVM RESULTS Actual vs. Predicted Values (First 100)", x = "Index", y = "Value")

# Performance metrics
auto_metrics1 <- metric_set(rsq, rmse, mae)
auto_metrics(data = test_performance, truth = Price, estimate = .pred)




################## K - Nearest Neighbor #####################
knn_model <- 
  nearest_neighbor( mode = "regression",
                    neighbors = tune(),
                    weight_func = tune(),
                    dist_power = tune(),
                    engine = "kknn"
  )

knn_wf <-
  workflow() %>%
  add_model(knn_model) %>% 
  add_recipe(rec)
knn_wf



install.packages("kknn")
library(kknn)
# Hyperparameter Tuning
knn_results <-
  knn_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae)
  )

knn_results %>%
  collect_metrics()


# Final Hyperparameter
param_final <- knn_results %>%
  select_best(metric = "rmse")

knn_wf <- knn_wf %>%
  finalize_workflow(param_final)
knn_wf

# last fit
knn_fit <- knn_wf %>%
  last_fit(split)

# Test Data Predictions
test_performance <- knn_fit %>% collect_predictions()
test_performance

df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "kNN Actual vs. Predicted Values (First 100)", x = "Index", y = "Value")

# Performance metrics
auto_metrics <- metric_set(rsq, rmse, mae)
auto_metrics(data = test_performance, truth = Price, estimate = .pred)

# Decision Trees
dt_model <- 
  decision_tree(mode = "regression",
                cost_complexity = tune(),
                tree_depth = tune(),
                min_n = tune(),
                engine = "rpart"
  )

dt_wf <-
  workflow() %>%
  add_model(dt_model) %>% 
  add_recipe(rec)
dt_wf



# Hyperparameter Tuning
dt_results <-
  dt_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae)
  )

dt_results %>%
  collect_metrics()



# Final Hyperparameter
param_final <- dt_results %>%
  select_best(metric = "rmse")

dt_wf <- dt_wf %>%
  finalize_workflow(param_final)
dt_wf


# last fit
dt_fit <- dt_wf %>%
  last_fit(split)


# Test Data Predictions
test_performance <- dt_fit %>% collect_predictions()
test_performance



df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "DT Actual vs. Predicted Values (First 100)", x = "Index", y = "Value")




# Performance metrics
auto_metrics <- metric_set(rsq, rmse, mae)
auto_metrics(data = test_performance, truth = Price, estimate = .pred)


########################## Random Forest ##########################
rf_model <- 
  rand_forest(mode = "regression",
              mtry = tune(),
              trees = tune(),
              min_n = tune(),
              engine = "ranger"
  )

rf_wf <-
  workflow() %>%
  add_model(rf_model) %>% 
  add_recipe(rec)
rf_wf

install.packages("ranger")
library(ranger)
# Hyperparameter Tuning
rf_results <-
  rf_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae)
  )

rf_results %>%
  collect_metrics()

# Final Hyperparameter
param_final <- rf_results %>%
  select_best(metric = "rmse")

rf_wf <- rf_wf %>%
  finalize_workflow(param_final)
rf_wf


# last fit
rf_fit <- rf_wf %>%
  last_fit(split)

# Test Data Predictions
test_performance <- rf_fit %>% collect_predictions()
test_performance

 






############ df ##############################

df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "RF Actual vs. Predicted Values(First 100)", x = "Index", y = "Value")








# Performance metrics
auto_metrics <- metric_set(rsq, rmse, mae)
auto_metrics(data = test_performance, truth = Price, estimate = .pred)


################################## XGBoost ################################################



# Extreme Gradient Boosting
xgboost_model <- 
  boost_tree( mode = "regression",
              mtry = tune(),
              trees = tune(),
              min_n = tune(),
              tree_depth = tune(),
              learn_rate = tune(),
              loss_reduction = tune(),
              sample_size = tune(),
              stop_iter = tune(),
              engine = "xgboost"
  )


xgboost_wf <-
  workflow() %>%
  add_model(xgboost_model) %>% 
  add_recipe(rec)
xgboost_wf
# Hyperparameter Tuning
xgboost_results <-
  xgboost_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae)
  )
xgboost_results %>%
  collect_metrics()







# Final Hyperparameter
param_final <- xgboost_results %>%
  select_best(metric = "rmse")

xgboost_wf <- xgboost_wf %>%
  finalize_workflow(param_final)

xgboost_wf



# last fit
xgboost_fit <- xgboost_wf %>%
  last_fit(split)


# Test Data Predictions
test_performance <- xgboost_fit %>% collect_predictions()
test_performance






df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "XGBOOST Actual vs. Predicted Values", x = "Index", y = "Value")







# Performance metrics
auto_metrics <- metric_set(rsq, rmse, mae)

auto_metrics(data = test_performance, truth = Price, estimate = .pred)

#############################################################################



#LÝGHT GBM
install.packages("bonsai")
library(bonsai)
install.packages("lightgbm")
library(lightgbm)



lgbm_model <- 
  boost_tree( mode = "regression",
              mtry = tune(),
              trees = tune(),
              min_n = tune(),
              tree_depth = tune(),
              learn_rate = tune(),
              loss_reduction = tune(),
              engine = "lightgbm"
  )

lgbm_wf <-
  workflow() %>%
  add_model(lgbm_model) %>% 
  add_recipe(rec)
lgbm_wf


# Hyperparameter Tuning
lgbm_results <-
  lgbm_wf %>% 
  tune_grid(resamples = auto_cv,
            metrics = metric_set(rsq, rmse, mae)
  )

lgbm_results %>%
  collect_metrics()

# Final Hyperparameter
param_final <- lgbm_results %>%
  select_best(metric = "rmse")

lgbm_wf <- lgbm_wf %>%
  finalize_workflow(param_final)
lgbm_wf




# last fit
lgbm_fit <- lgbm_wf %>%
  last_fit(split)


# Test Data Predictions
test_performance <- lgbm_fit %>% collect_predictions()
test_performance


df <- data.frame(Actual = test_performance$Price[1:100], Predicted = test_performance$.pred[1:100])
ggplot(df) + 
  geom_line(aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line(aes(x = 1:100, y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "lgbm Actual vs. Predicted Values (First 100)", x = "Index", y = "Value")






auto_metrics <- metric_set(rsq, rmse, mae)
auto_metrics(data = test_performance, truth = Price, estimate = .pred)


# Gerekli kütüphaneleri yükleme
library(tidyverse)
library(tidymodels)
library(h2o)
library(agua)
# H2O istemcisini baþlatma
h2o.init()

# Veri kümesini yükleme ve ön iþleme adýmlarýný gerçekleþtirme
data <- data.frame(cp)  # Veri kümesini yükleyin ve uygun þekilde düzenleyin

# Veri kümesini training ve test setlerine ayýrma
set.seed(123)
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)




# Baðýmlý deðiþkeni belirleme
response <- "Price"

# H2O çerçevelerine dönüþtürme
h2o_train <- as.h2o(train)
h2o_test <- as.h2o(test)

# Ön iþleme adýmlarýný belirleme
preprocess <- recipe(Price ~ ., data = train) %>%
  step_naomit(all_predictors()) %>%
  step_dummy(all_nominal())

# Model spesifikasyonu

  model_spec <- boost_tree(
    mode = "regression",
    engine = "h2o_gbm",
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune()
  )
  


# Tidy modeli oluþturma
workflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_spec) %>%
  tune_grid(
    resamples = auto_cv,
    grid = 10,
    metrics = metric_set(rsq, rmse, mae))

# Modelin eðitimi
trained_model <- workflow %>% 
  fit(data = train)

# Test veri setinde tahmin yapma
predictions <- trained_model %>% 
  predict(new_data = test_data) %>% 
  pull(.pred)

# Gerçek ve tahmin edilen deðerlerin karþýlaþtýrýlmasý
comparison <- data.frame(Actual = test_data$Price, Predicted = as.vector(predictions))

# Tahmin sonuçlarýný görselleþtirme
ggplot(comparison[1:100, ], aes(x = 1:100, y = Actual, color = "Actual")) +
  geom_line() +
  geom_line(data = comparison[1:100, ], aes(y = Predicted, color = "Predicted")) +
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  labs(title = "GBM Actual vs. Predicted Values (First 100)", x = "Index", y = "Value")


# Performans metrikleri
auto_metrics <- metric_set(rsq, rmse, mae)

auto_metrics(data = comparison, truth = Actual, estimate = Predicted)

# H2O istemcisini kapatma
h2o.shutdown()


























































































