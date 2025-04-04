#install.packages("dplyr")
library(rvest)
library(dplyr)

# Функция для извлечения и обработки данных по году
process_year <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  html_data <- read_html(url)
  table <- html_table(html_nodes(html_data, 'table'))[[2]] %>% as.data.frame()
  table[1:nrow(table), 1] <- rownames(table)  # Обновляем номера строк
  table$Year <- year  # Добавляем столбец с годом
  table$'Climate Index'[table$'Climate Index' == "-"] <- NA_real_  # Явное указание типа NA вместо "-"
  table$'Climate Index' <- as.double(table$'Climate Index')
  return(table)
}

# Обрабатываем все годы с 2014 по 2021
years <- 2014:2021
combined_data <- lapply(years, process_year) %>% bind_rows()

# Фильтруем данные для нужных стран
selected_countries <- c("Canada", "Australia", "Bulgaria", "Poland", "Ukraine")
result <- combined_data %>% 
  filter(Country %in% selected_countries) %>%
  arrange(Country, Year)

# Просмотр результата
print(result)

# Создаем отдельные датафреймы для каждой страны
df_Canada <- result %>% filter(Country == "Canada")
df_Australia <- result %>% filter(Country == "Australia")
df_Bulgaria <- result %>% filter(Country == "Bulgaria")
df_Poland <- result %>% filter(Country == "Poland")
df_Ukraine <- result %>% filter(Country == "Ukraine")


#График 1
# 1. Настройка цветов для стран
colors <- c("red", "blue", "green", "orange", "purple")
metrics <- setdiff(names(result), c("Rank", "Country", "Year"))
names(colors) <- selected_countries

# 2. Создаем разметку для графиков и легенды
layout_matrix <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
layout_matrix <- cbind(layout_matrix, rep(13, 3)) # Добавляем колонку для легенды

# 3. Устанавливаем параметры расположения
layout(layout_matrix, widths = c(rep(1, 4), 0.5)) # Легенда занимает 0.5 от ширины графика
par(mar = c(4, 4, 3, 1), oma = c(0, 0, 0, 2))

# 4. Рисуем все графики
for(i in seq_along(metrics)) {
  metric <- metrics[i]
  y_lim <- range(result[[metric]], na.rm = TRUE)
  
  plot(NA, 
       xlim = range(years), 
       ylim = y_lim,
       main = metric, 
       xlab = "", 
       ylab = "")
  
  for(country in selected_countries) {
    country_data <- result[result$Country == country, ]
    lines(country_data$Year, country_data[[metric]], 
          col = colors[country], 
          lwd = 2)
    points(country_data$Year, country_data[[metric]], 
           col = colors[country], 
           pch = 16)
  }
  title(xlab = "Годы", ylab = "Значения")
  
}

# 5. Рисуем общую легенду
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", 
       legend = selected_countries, 
       col = colors, 
       lty = 1, 
       lwd = 2, 
       pch = 16,
       cex = 1.2, 
       bty = "n", 
       title = "Страны")

# 6. Восстанавливаем стандартные настройки
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)




#График 2
# 1. Подготовка данных
selected_countries <- c("Canada", "Australia", "Bulgaria", "Poland", "Ukraine")
metrics <- setdiff(names(result), c("Rank", "Country", "Year"))

# Получаем данные о рангах
rank_data <- result %>% 
  select(Country, Year, Rank) %>%
  arrange(Country, Year) %>%
  mutate(Rank = as.numeric(Rank))

# 2. Настройка графического устройства
# Увеличиваем правый отступ для легенды
par(mar = c(5, 4, 4, 8), xpd = FALSE)  # Правое поле 8 вместо стандартных 4

# 3. Создаем основной график
plot.new()
plot.window(xlim = range(years), 
            ylim = c(max(rank_data$Rank) + 10, 1))  # Ось Y перевернута
title(main = "Динамика рейтингов стран (2014-2021)",
      xlab = "Год", 
      ylab = "Место в рейтинге")
axis(1, at = years)
axis(2, at = seq(1, max(rank_data$Rank) + 5, by = 5))

# 4. Добавляем сетку, которая не выходит за пределы графика
# Задаем цвет с прозрачностью для построения сетки
transparent_gray <- rgb(0.8, 0.8, 0.8, alpha = 0.5) # 0.5 - 50% прозрачности

abline(v = years, col = transparent_gray, lty = "solid") # вертикальные линии
abline(h = seq(1, max(rank_data$Rank) + 5, by = 5), col = transparent_gray, lty = "solid") # горизонтальные линии

# 5. Рисуем линии для каждой страны
colors <- c("red", "blue", "green", "orange", "purple")
names(colors) <- selected_countries

for(country in selected_countries) {
  country_ranks <- rank_data[rank_data$Country == country, ]
  lines(country_ranks$Year, country_ranks$Rank, 
        col = colors[country], lwd = 2)
  points(country_ranks$Year, country_ranks$Rank, 
         col = colors[country], pch = 16)
  
  # Подписи значений
  text(years, 
       country_ranks$Rank,
       labels = country_ranks$Rank,
       pos = 1, col = colors[country], cex = 0.6, xpd = TRUE)
}

# 6. Добавляем легенду в увеличенное правое поле
legend(x = max(years) + 0.5,  # Сдвигаем вправо от последнего года
       y = par("usr")[4],  #  Сверху по вертикали
       legend = selected_countries, 
       col = colors, 
       lty = 1, 
       lwd = 2, 
       pch = 16,
       bty = "n",  # Без рамки
       x.intersp = 0.8, xpd = TRUE)  # Уменьшаем расстояние между элементами

# 7. Восстанавливаем стандартные параметры
par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)





#Часть 2. Сбор информации со страницы
library(rvest)
library(dplyr)

# URL страницы
url <- "https://www.afisha.ru/krasnodar/museum/"

# Загрузка HTML страницы
page <- read_html(url)

selector_names <- "._yjkz>.CjnHd.y8A5E.vVS2J" 
nodes_names <- html_nodes(page, selector_names)
names <- html_text(nodes_names, trim=TRUE)
selector_addresses <- "._yjkz>.hmVRD.DiLyV" 
addresses <- html_text(html_nodes(page, selector_addresses), trim=TRUE)
hrefs <- html_attr(nodes_names, "href")
 
base_url <- "https://www.afisha.ru"
full_urls <- paste0(base_url, hrefs)

selector_descr <- ".rtbal.It9Zd.GqJw2>.nLIEl>.aEVDY>div"
nodes_descr <- html_nodes(page, selector_descr)
descriptions <- html_text(nodes_descr, trim=TRUE)

df <- data.frame(names, addresses, full_urls, descriptions)
View(df)
