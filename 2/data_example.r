library("forecast")

# Корреляционный анализ

data <- read.table("data.txt", header = TRUE)

# Строим диаграмму рассеяния (1)

plot(data$ex, data$en)

# На диаграмме видим отсутствие линейной зависимости
# Коэффициент корреляции будет низким

# Находим значение коэффициента корреляции

cor_result <- cor(data$ex, data$en)
print(cor_result)

# Коэффициент корреляции: 0.1621786
# Значение коэффицента < 0.5, что говорит о слабой корреляционной зависимости между этими переменными
# Таким образом, на основе анализа диаграммы рассеяния и коэффициента корреляции можно сделать вывод о том, что линейной зависимости между переменными не наблюдается

# Проверка значимости коэффициента корреляции

cor_test_result <- cor.test(data$ex, data$en)
print(cor_test_result)

# p-value = 0.1246 > 0.1
# В данном случае, уровень значимости (p-value) превышает выбранный порог (0.1), поэтому мы не можем считать результат статистически значимым на уровне 0.1
# Таким образом, на основе проведенного теста, нет статистически значимой корреляции между среднедушевыми денежными доходами и числом турпакетов, реализованных населению

# Парная нелинейная регрессия

data <- read.table("data.txt", header = TRUE)

# data <- data[-which(data$en > 1000000), c(1, 2)]
# data <- data[-which(data$ex > 25000), c(1, 2)]

# Отсортируем строки по возрастанию значений экзогенной переменной

data <- data[order(data$ex), ]

# Проведем регрессионный анализ

# Исследуем линейную регрессию

lm_res <- lm(formula = data$en ~ data$ex)
summary(lm_res)

# Проверим значимость регрессии:
# Multiple R-squared: 0.0263
# Коэффициент детерминации равен 0,0263 – низкое значение.
# p-value: 0.1246
# p-value = 0.1246 > 0,1, следовательно, регрессия не значима.

# Проверим значимость коэффициентов уравнения:
# (Intercept) -48280.41 Std. Error 135539.92, t value -0.356, Pr(>|t|) 0.723
# p-value для свободного члена (Intercept) = 0.723 > 0,1, следовательно, свободный член не значим.
# data$ex 11.02 Std. Error 7.11, t value 1.551, Pr(>|t|) 0.125
# p-value для коэффициента при переменной (data$ex) = 0.125 > 0,1, следовательно, коэффициент при переменной не значим.

# Вывод:
# Оба коэффициента не значимы, и уравнение регрессии в целом также не значимо для описания зависимости между переменными.

# Исследуем логарифмическую регрессию en = p1 + p2 * log(ex) + ε

lm_log_res <- lm(formula = data$en ~ log(data$ex))
summary(lm_log_res)

# Проверим значимость регрессии:
# Multiple R-squared: 0.0378
# Коэффициент детерминации равен 0.0378 – низкое значение.
# p-value: 0.06478
# p-value = 0.06478 < 0.1, следовательно, регрессия значима.

# Проверим значимость коэффициентов уравнения:
# (Intercept) -2710332 Std. Error 1527868, t value -1.774, Pr(>|t|) 0.0795
# p-value для свободного члена (Intercept) = 0.0795 < 0.1, следовательно, свободный член значим.
# log(data$ex) 294169 Std. Error 157317, t value 1.870, Pr(>|t|) 0.0648
# p-value для коэффициента при переменной (log(data$ex)) = 0.0648 < 0.1, следовательно, коэффициент при переменной также значим.

# Вывод:
# Оба коэффициента значимы, и уравнение регрессии в целом также значимо для описания зависимости между переменными.

# Запишем уравнение регрессии с правильной точностью коэффициентов

format(coef(lm_log_res), digits = 12)

# en = -2710331.688825 + 294168.910664 * log(ex) + ε

# Построим график регрессии (2)

plot(data$ex, data$en)
curve(coef(lm_log_res)[1] + coef(lm_log_res)[2] * log(x), add = TRUE, col = "red")

# Проведем анализ остатков

# Найдем математическое ожидание остатков

mean(lm_log_res$residuals)

# -2.002637e-12
# Математическое ожидание остатков равно нулю

#  Построим график остатков (3)

plot(lm_log_res$residuals)
abline(h = 0, col = "red")

# Остатки независимы и распределены неравномерно

# Найдем относительную ошибку аппроксимации MAPE

accuracy(lm_log_res)

# MAPE: 11097
# Регрессию нельзя использовать для прогнозирования

# Исследуем гиперболическую регрессию en = p1 + p2 / ex + ε

nls_hyper_res <- nls(en ~ p1 + p2 / ex, data = data, start = list(p1 = 1, p2 = 1))
summary(nls_hyper_res)

# Проверка значимости регрессии:
# p-value: 0.00831
# p-value = 0.00831 < 0.1, следовательно, регрессия значима.

# Проверка значимости коэффициентов уравнения:
# p1 5.141e+05 Std. Error 1.904e+05, t value 2.700, Pr(>|t|) 0.00831
# p-value для коэффициента p1 = 0.00831 < 0.1, следовательно, p1 значим.
# p2 -5.766e+09 Std. Error 2.857e+09, t value -2.018, Pr(>|t|) 0.04661
# p-value для коэффициента p2 = 0.04661 < 0.1, следовательно, p2 значим.

# Вывод:
# Регрессия является статистически значимой, так как p-value для регрессии и обоих коэффициентов меньше уровня значимости 0.1.

# Запишем уравнение регрессии с правильной точностью коэффициентов

format(coef(nls_hyper_res), digits = 12)

# en = 514142.543083 + -5766124228.826840 / ex + ε

# Построим график регрессии (4)

plot(data$ex, data$en)
curve(coef(nls_hyper_res)[1] + coef(nls_hyper_res)[2] / x, add = TRUE, col = "red")

# Проведем анализ остатков

#  Построим график остатков (5)

plot(nls_hyper_res$m$resid())
abline(h = 0, col = "red")

# Остатки независимы и распределены неравномерно

# Найдем относительную ошибку аппроксимации MAPE

sum(abs((data$en - (coef(nls_hyper_res)[1] + coef(nls_hyper_res)[2] / data$ex)) / data$en)) / length(data$en) * 100

# MAPE: 9938.119
# Регрессию нельзя использовать для прогнозирования