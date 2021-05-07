library(tidyverse) 

library(readxl)  # read xls files

library(NbClust) # find 

library(knitr) 	# find best no cluster

library(tidymodels) # contains ML related libraries 

library(flexclust)

library(funtimes)

library(NbClust)

library(cluster)

library(caret)

library(factoextra)   # load required packages

theme_set(theme_light())



# Read the downloaded excel datafile

vehicle_models <- read_excel("D:/Level 5/Semester 2/Machine Learning/CW/vehicles.xlsx") %>%
  
  janitor::clean_names() %>%
  
  mutate(class = as_factor(class))

  summary(vehicle_models)     # Get the view of how the dataset looks like



vehicle_models %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'van'")



vehicle_models %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: 'bus'")



vehicle_models %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: saab")



vehicle_models %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Outlier Detection for class: opel")



vehicles_bus = vehicle_models %>%
  
  filter(class == "bus") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

  View(vehicles_bus)


vehicles_van = vehicle_models %>%
  
  filter(class == "van") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

  View(vehicles_van)



vehicles_opel = vehicle_models %>%
  
  filter(class == "opel") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

  View(vehicles_opel)


vehicles_saab = vehicle_models %>%
  
  filter(class == "saab") %>%
  
  mutate(across(2:19, ~squish(.x, quantile(.x, c(.05, .95)))))

  View(vehicles_saab)



combined = bind_rows(list(vehicles_bus,vehicles_opel,vehicles_saab,vehicles_van)) %>%
  
  arrange(samples)



  print(combined)


combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "bus") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.shape = NA) +
  
  labs(title = "Transformed Outliers class: 'bus'")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "saab") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.shape = NA) +
  
  labs(title = "Transformed Outliers for class: saab")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "van") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot(outlier.shape = NA) +
  
  labs(title = "Transformed Outliers for class: van")



combined %>%
  
  pivot_longer(2:19,names_to = "labels") %>%
  
  filter(class == "opel") %>%
  
  mutate(class = fct_reorder(class,value,median)) %>%
  
  ggplot(aes(class, value, fill = reorder(labels,value))) +
  
  geom_boxplot() +
  
  labs(title = "Transformed Outliers for class: opel")



#Remove the sample name and the class name. Both of these will be remove so that only n

#numerical data is left for the algorithm.



vehicleModel_DataPoints = combined %>%
  
  select(-samples,-class)

#Now that we have the "vehicleModel_DataPoints" dataset, scalling is performed

preprocessed_vehicleData = vehicleModel_DataPoints %>%
  
  mutate(across(everything(),scale))

set.seed(123)

#Perform the kmenas using the NbClust function

#use Euclidean for distance

cluster_eucidean = NbClust(preprocessed_vehicleData, distance="euclidean",min.nc=2,max.nc=15,method="kmeans",index="all")

#Use manhattan for distance

cluster_manhattan = NbClust(preprocessed_vehicleData, distance="manhattan",min.nc=2,max.nc=10,method="kmeans",index="all")



# Elbow method
fviz_nbclust(preprocessed_vehicleData, kmeans, method = "wss") +
  
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  
  labs(subtitle = "Elbow method") # add subtitle


# Silhouette method

fviz_nbclust(preprocessed_vehicleData, kmeans, method = "silhouette") +
  
  labs(subtitle = "Silhouette method")

# Gap statistic

set.seed(42)

fviz_nbclust(preprocessed_vehicleData, kmeans,
             
             nstart = 25,
             
             method = "gap_stat",
             
             nboot = 500
             
) + # reduce it for lower computation time (but less precise results)
  
  labs(subtitle = "Gap statistic method")

nbclust_out <- NbClust(
  
  data = preprocessed_vehicleData,
  
  distance = "euclidean",
  
  min.nc = 2, # minimum number of clusters
  
  max.nc = 5, # maximum number of clusters
  
  method = "kmeans" 
  
)

# create a dataframe of the optimal number of clusters

nbclust_Vehicleplots <- data.frame(clusters = nbclust_out$Best.nc[1, ])

# select only indices which select between 2 and 5 clusters

nbclust_Vehicleplots <- subset(nbclust_Vehicleplots, clusters >= 2 & clusters <= 5)

# create plot

ggplot(nbclust_Vehicleplots) +
  
  aes(x = clusters) +
  
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  
  theme_minimal()



set.seed(42)

model <- kmeans(preprocessed_vehicleData, centers = 2)

sil <- silhouette(model$cluster, dist(preprocessed_vehicleData))

fviz_silhouette(sil)

fviz_cluster(model, preprocessed_vehicleData, ellipse.type = "norm")

model$cluster

model$centers

y <- as.numeric(vehicle_models$class)

x <-  as.factor(model$cluster)

table(vehicle_models$class,model$cluster)


library(caret)
confusionMatrix(
  
  factor(model$cluster, levels = 1:4),
  
  factor(y, levels = 1:4)
)

set.seed(42)

model <- kmeans(preprocessed_vehicleData, centers = 3)

sil <- silhouette(model$cluster, dist(preprocessed_vehicleData))

fviz_silhouette(sil)

fviz_cluster(model, preprocessed_vehicleData, ellipse.type = "norm")

model$cluster

model$centers

table(vehicle_models$class,model$cluster)

model
y <- as.numeric(vehicle_models$class)

x <-  as.factor(model$cluster)

table(vehicle_models$class,model$cluster)

model$cluster

vehicle_models$class

result <- confusionMatrix(
  
  factor(model$cluster, levels = 1:4),
  
  factor(y, levels = 1:4)
)

result

precision <- result$byClass['Pos Pred Value'] 

recall <- result$byClass['Sensitivity']

precision <- as.numeric(precision)

recall

set.seed(42)

model <- kmeans(preprocessed_vehicleData, centers = 4, nstart = 20)

sil <- silhouette(model$cluster, dist(preprocessed_vehicleData))

fviz_silhouette(sil)

fviz_cluster(model, preprocessed_vehicleData, ellipse.type = "norm")

model$cluster
model$centers

table(vehicle_models$class,model$cluster)
model

model$cluster

vehicle_models$class

result <- confusionMatrix(
  
  factor(model$cluster, levels = 1:4),
  
  factor(y, levels = 1:4)
)

result

precision <- result$byClass['Pos Pred Value'] 

recall <- result$byClass['Sensitivity']

precision <- as.numeric(precision)




set.seed(42)

model <- kmeans(preprocessed_vehicleData[, 6:18], centers = 4)

sil <- silhouette(model$cluster, dist(preprocessed_vehicleData))

fviz_silhouette(sil)

fviz_cluster(model, preprocessed_vehicleData, ellipse.type = "norm")

model$cluster
model$centers

table(vehicle_models$class,model$cluster)

