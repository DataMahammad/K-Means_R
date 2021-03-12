library(tidyverse)
library(factoextra)
library(NbClust)
library(cluster)
library(plotly)
library(data.table)
library(inspectdf)


dataset <- fread("Mall_Customers.csv")

dataset %>% glimpse()

dataset %>% inspect_na()

dataset$CustomerID <- dataset$CustomerID %>% as.character()

names <- dataset %>% names()

dataset$Gender %>% table() %>% prop.table()

names <- names %>% str_replace_all(" ","_") %>% 
                   str_replace_all("\\(","") %>% 
                   str_replace_all("\\)","") %>% 
                   str_replace_all("k\\$","") %>% 
                   str_replace_all("1-100","")
        

names <- names %>% gsub("Gender","gender",.)
names <- names %>% gsub("Age","age",.)
names <- names %>% gsub("Annual_Income","annual_income",.)
names <- names %>% gsub("Spending_Score","spending_score",.)

names(dataset) <- names


df <- dataset %>% select(-c(1:2)) %>% scale()

df %>% glimpse()

df %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
#Preferred number of clusters --> 4


df %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
#Preferred number of clusters --> 8

df %>% 
  fviz_nbclust(kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")
#Preferred number of clusters --> 6

#Let's take 4

set.seed(123)
kmeans <- df %>% kmeans(centers = 4)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 4,
                plotchar = F,
                main = 'Clusters of customers')

#Let's take 6

set.seed(123)
kmeans <- df %>% kmeans(centers = 6)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 6,
                plotchar = F,
                main = 'Clusters of customers')

#Let's take 8

set.seed(123)
kmeans <- df %>% kmeans(centers = 8)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 8,
                plotchar = F,
                main = 'Clusters of customers')



g <- dataset %>% 
  ggplot(aes(annual_income_,spending_score_,
             color = y_kmeans)) +
  geom_point(aes(text = age),size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Annual Income", 
       y="Spending Score",
       title="Iris",
       subtitle="5 clusters")

g


#Let's take 5(my own personal preference)

set.seed(123)
kmeans <- df %>% kmeans(centers = 5)
y_kmeans <- kmeans$cluster %>% as_factor()

df %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 5,
                plotchar = F,
                main = 'Clusters of customers')


names

g <- dataset %>% 
  ggplot(aes(annual_income_,spending_score_,
             color = y_kmeans)) +
  geom_point(aes(text = age),size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Annual Income", 
       y="Spending Score",
       title="Iris",
       subtitle="5 clusters")
g


