library(readr)
library(dplyr)
library(ggplot2)

# Загрузка данных
df1 <- read_csv("~/R/lab_5_1/googleplaystore.csv")
df2 <- read_csv("~/R/lab_5_1/googleplaystore_user_reviews.csv")
# Проверка размера генеральной совокупности
nrow(df1)
nrow(df2)

# Агрегируем (Находим средние значения) данные из второй таблицы по приложению
df2_agg <- df2 %>%
  filter(!is.na(Sentiment_Polarity)) %>% # Удаляем строки с пропущенными значениями
  group_by(App) %>%
  summarise(
    avg_sentiment_polarity = mean(Sentiment_Polarity, na.rm = TRUE),
    avg_sentiment_subjectivity = mean(Sentiment_Subjectivity, na.rm = TRUE),
    review_count = n()
  ) %>%
  ungroup() # Снимаем группировку

# Удаляем дубликаты по названию приложения (оставляем первую встретившуюся запись)
df1_unique <- df1 %>%
  distinct(App, .keep_all = TRUE)

# Объединяем таблицы ТОЛЬКО по приложениям, присутствующим в обеих таблицах
combined_data <- df1_unique %>%
  inner_join(df2_agg, by = "App")

nrow(combined_data)

# Выбираем и преобразуем переменные для кластеризации
cluster_data <- combined_data %>%
  select(
    Rating,
    Reviews,
    Installs,
    avg_sentiment_polarity,
    avg_sentiment_subjectivity,
  ) %>%
  mutate(
    # Преобразуем Installs в числовой (пример: "10,000+" -> 10000)
    Installs = as.numeric(gsub("[^0-9]", "", Installs))
  )

# Случайная выборка 200 строк
set.seed(123)
random_indexes <- sample(nrow(cluster_data), 200)
cluster_data <- cluster_data[random_indexes, ]


# Замена NA медианными значениями
cluster_data <- cluster_data %>%
  mutate(
    avg_sentiment_polarity = ifelse(is.na(avg_sentiment_polarity),
                                    median(avg_sentiment_polarity, na.rm = TRUE),
                                    avg_sentiment_polarity),
    avg_sentiment_subjectivity = ifelse(is.na(avg_sentiment_subjectivity),
                                        median(avg_sentiment_subjectivity, na.rm = TRUE),
                                        avg_sentiment_subjectivity)
  )

cluster_data <- as.data.frame(cluster_data)
app_names <- combined_data$App[random_indexes]
rownames(cluster_data) <- app_names


# Сводная статистика по числовым переменным
summary_stats <- cluster_data %>%
  select(Rating, Reviews, Installs,
         avg_sentiment_polarity, avg_sentiment_subjectivity) %>%
  summary()

print(summary_stats)

class(cluster_data)
# Параметры графиков
par(mar = c(4, 4, 2, 1))

# 1. Рейтинг
boxplot(cluster_data$Rating, 
        main = "Распределение рейтингов",
        ylab = "Rating",
        col = "lightblue",
        staplewex = 1,   # Ширина "усов"
        whisklty = 1,    # Стиль линии усов (1 = сплошная)
        outpch = 16)     # Символы для выбросов

# 2. Тональность отзывов
boxplot(cluster_data$avg_sentiment_polarity, 
        main = "Тональность отзывов",
        ylab = "Sentiment Polarity",
        col = "gold")



# Гистограмма рейтингов
ggplot(cluster_data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
  labs(title = "Распределение рейтингов приложений", x = "Рейтинг", y = "Количество") +
  theme_minimal()


# Гистограмма тональности отзывов
ggplot(cluster_data, aes(x = avg_sentiment_polarity)) +
  geom_histogram(binwidth = 0.05, fill = "gold", color = "black") +
  labs(title = "Распределение тональности отзывов", x = "Тональность", y = "Количество") +
  theme_minimal()


cluster_data_not_normalized <- cluster_data

#Нормализация данных
maxs <- apply(cluster_data, 2, max) 
mins <- apply(cluster_data, 2, min) 
cluster_data <- as.data.frame(scale(cluster_data, center = mins, scale = maxs - mins))

class(cluster_data)

#Евклидово расстояние
dist.app <- dist(cluster_data)
#Результаты кластерного анализа
clust.app <- hclust(dist.app, "ward.D")


library (factoextra) 
library (cluster) 

# Метод Локтя
fviz_nbclust(cluster_data, kmeans, method = "wss")

# Метод Силуэта
fviz_nbclust(cluster_data, kmeans, method = "silhouette") + 
  labs(subtitle = "Silhouette method")

#посчитать статистику разрыва,  базирующуюся на числе кластеров K.max =15: 
gap_stat <- clusGap(cluster_data, FUN = kmeans, nstart = 15, K.max = 10, B = 15) 
#plot number of clusters vs. gap statistic 
fviz_gap_stat(gap_stat) 

#Алгоритм на основе консенсуса 
library(parameters) 
n_clust <- n_clusters(cluster_data, 
                      package = c("easystats", "NbClust", "mclust"), 
                      standardize = FALSE) 
n_clust 
plot(n_clust)

class(cluster_data)

plot(clust.app)
plot(clust.app, app_names, cex=0.5) 
rect.hclust(clust.app, k=3, border="red")


groups <- cutree(clust.app, k=3)

#  в 1-ом кластере 
g1<-colMeans(cluster_data[groups==1, ]) 
#  во 2-ом кластере 
g2<-colMeans(cluster_data[groups==2, ]) 
#  в 3-ем кластере 
g3<-colMeans(cluster_data[groups==3, ])

df<-data.frame(g1, g2, g3) 
df1<-t(df) 
df<-t(df1) 
barplot(df, col=c("red","green","blue","yellow", "pink")) #  построим график


#Поработав над графиком, получим те же данные, несколько в другом виде 
barplot(df, ylim=c(0,2),   
        main = "Groups of Apps", axes = FALSE,  
        col=c("red","green","blue","yellow", "pink"), beside=TRUE) 
axis(2, at = 0:2, labels = 0:2) 
legend("top", legend = rownames(df), col=c("red","green","blue","yellow", "pink"), lwd=
         10, bty = "n") 

km.res <- kmeans(cluster_data, 3, nstart = 10) 

#Боксплоты
boxplot(cluster_data$Rating ~ km.res$cluster, data = cluster_data, ylab = "Rating",  frame = FALSE, col = "lightgray")
boxplot(cluster_data$Reviews ~ km.res$cluster, data = cluster_data, ylab = "Reviews",  frame = FALSE, col = "lightgray")
boxplot(cluster_data$Installs ~ km.res$cluster, data = cluster_data, ylab = "Installs",  frame = FALSE, col = "lightgray")
boxplot(cluster_data$avg_sentiment_polarity ~ km.res$cluster, data = cluster_data, ylab = "avg_sentiment_polarity",  frame = FALSE, col = "lightgray")
boxplot(cluster_data$avg_sentiment_subjectivity ~ km.res$cluster, data = cluster_data, ylab = "avg_sentiment_subjectivity",  frame = FALSE, col = "lightgray")


# Добавление столбца категории
#cluster_data$Category = combined_data$Category[random_indexes]

#Кластеризация по k-means
#km.res <- kmeans(cluster_data, 3, nstart = 10) 
fviz_cluster(km.res, cluster_data, palette = "Set2", ggtheme = theme_minimal())

#построение scatterplot
pairs(cluster_data) 
pairs(cluster_data, main= "Приложения", col = c("red","green","blue")) 
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")   
pairs(cluster_data, main= "Приложения по популярности",pch = 19,  cex = 0.8, 
      col = my_cols[km.res$cluster], 
      lower.panel=NULL)

#Трехмерная кластеризация
library("scatterplot3d")

colors <- c("#999999", "#E69F00", "#56B4E9") 
colors <- colors[as.numeric(km.res$cluster)] 
s3d <- scatterplot3d(cluster_data[, 1:3], main= "Приложения по популярности", pch = 16, color=colors) 
legend("topright", legend = levels(as.factor(km.res$cluster)), 
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16) 

colors <- c("#999999", "#E69F00", "#56B4E9") 
colors <- colors[as.numeric(km.res$cluster)] 
s3d <- scatterplot3d(cluster_data[, c(1, 2, 4)], main= "Приложения по популярности", pch = 16, color=colors)
legend("topright", legend = levels(as.factor(km.res$cluster)), 
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16) 
