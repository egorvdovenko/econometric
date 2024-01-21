library(forecast)

data <- read.csv2("data.csv", header = FALSE, sep = ";", dec = ",", col.names = c("Month", "Riverflow"))

# Построить график временного ряда (1)

plot(data$Riverflow, type = "o")

# Временной ряд имеет периодическую тенденцию и описывается аддитивной моделью
# Построим коррелограмму (2)

acf(data$Riverflow, type = "correlation", plot = TRUE)

# По коррелограмме определяем наличие тренда и сезонных колебаний с периодом, равным 12 месяцам
# Проведём сглаживание временного ряда методом скользящего среднего

sn <- ma(data$Riverflow, order = 12, centre = TRUE)
plot(data$Riverflow, type = "o")

# Отобразим сглаженный ряд (3)
lines(sn, col = "green")

# Рассчитаем сезонную компоненту временного ряда

# Вычтем из значений временного ряда сглаженное значение
A <- matrix(data = data$Riverflow - sn, nrow = 12)
SM <- apply(A, 1, function(x) mean(x, na.rm = TRUE))
# Запишем сезонную составляющую временного ряда
M.S <- rep(SM, times = 12)

# Рассчитаем тренд временного ряда

# Удалим сезонную составляющую из временного ряда
Tr <- data$Riverflow[1:length(M.S) * 3] - M.S
# Cформируем значения t
T <- seq(from = 1, to = 144)
# Построим линейную регрессию
regM <- lm(Tr ~ T)
# Запишем тренд для временного ряда
M.Trend <- coef(regM)[1] + coef(regM)[2] * T

# Рассчитаем значения временного ряда по модели

M.fit <- M.Trend + M.S
# График временного ряда
plot(data$Riverflow, type = "o")
# График модели временного ряда (4)
lines(M.fit, col = "red")
# График тренда временного ряда (5)
lines(M.Trend, col = "green")
# Рассчитаем MAPE
sum(abs((data$Riverflow[1:length(M.S) * 3] - M.fit) / data$Riverflow[1:length(M.S) * 3])) / length(data$Riverflow[1:length(M.S) * 3]) * 100
# 182.1968
# Относительная ошибка аппроксимации равна 182,20%

# Построим прогноз

# Создадим массив для хранения прогноза
M.F <- array(dim = 12)
# Создадим массив для времени прогноза
T1 <- seq(from = 145, to = 144 + 12)
# Рассчитаем прогнозные значения
M.F <- (coef(regM)[1] + coef(regM)[2] * T1) + SM
# График временного ряда
plot(data$Riverflow, type="o")
# График модели временного ряда
lines(M.fit, col = "red")
# График прогноза (6)
lines(x = T1, y = M.F, col = "green")
abline(regM)