# Добавляем вектор groups в исходный dataset и преобразуем его в фактор
cluster_data_not_normalized$Cluster <- as.factor(groups)

# Разделяем данные на обучающую (70%) и тестовую (30%) выборки
set.seed(123)
train_index <- sample(1:nrow(cluster_data_not_normalized), 
                      size = round(0.7 * nrow(cluster_data_not_normalized)))
train_data <- cluster_data_not_normalized[train_index, ]
test_data <- cluster_data_not_normalized[-train_index, ]

# Убедимся, что распределение классов примерно одинаковое в обеих выборках
table(train_data$Cluster)
table(test_data$Cluster)

### 1. Наивный Байесовский классификатор ###
library(klaR)

# Обучаем модель
nb_model <- NaiveBayes(Cluster ~ ., data = train_data)

# Прогнозируем на тестовых данных
nb_pred <- predict(nb_model, test_data)

# Матрица ошибок и точность
nb_conf_matrix <- table(test_data$Cluster, nb_pred$class)
nb_accuracy <- sum(diag(nb_conf_matrix)) / sum(nb_conf_matrix)
print("Наивный Байесовский классификатор:")
print(nb_conf_matrix)
print(paste("Точность:", round(nb_accuracy, 3)))

### 2. Дерево решений (с использованием библиотеки party) ###
library(party)

# Обучаем модель дерева решений
ctree_model <- ctree(Cluster ~ ., data = train_data)

# Визуализируем дерево
plot(ctree_model, main="Дерево решений (библиотека party)")

# Прогнозируем на тестовых данных
ctree_pred <- predict(ctree_model, test_data)

# Матрица ошибок и точность
ctree_conf_matrix <- table(test_data$Cluster, ctree_pred)
ctree_accuracy <- sum(diag(ctree_conf_matrix)) / sum(ctree_conf_matrix)
print("Дерево решений (party):")
print(ctree_conf_matrix)
print(paste("Точность:", round(ctree_accuracy, 3)))

### 3. Случайный лес ###
library(randomForest)

# Обучаем модель случайного леса
rf_model <- randomForest(Cluster ~ ., data = train_data, ntree = 500)

# Прогнозируем на тестовых данных
rf_pred <- predict(rf_model, test_data)

# Матрица ошибок и точность
rf_conf_matrix <- table(test_data$Cluster, rf_pred)
rf_accuracy <- sum(diag(rf_conf_matrix)) / sum(rf_conf_matrix)
print("Случайный лес:")
print(rf_conf_matrix)
print(paste("Точность:", round(rf_accuracy, 3)))

### Сравнение результатов ###
comparison <- data.frame(
  Method = c("Naive Bayes", "Decision Tree (party)", "Random Forest"),
  Accuracy = c(nb_accuracy, ctree_accuracy, rf_accuracy)
)

print("Сравнение методов:")
print(comparison)

# Визуализация сравнения
library(ggplot2)
ggplot(comparison, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Accuracy, 3)), vjust = -0.3) +
  ylim(0, 1) +
  labs(title = "Сравнение точности классификаторов", 
       y = "Точность", x = "Метод") +
  theme_minimal()
