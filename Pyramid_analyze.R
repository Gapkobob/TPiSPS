options(scipen = 999)
library("readxl")
library("ggplot2")
library("httr")
library("jsonlite")
library("markdown")

# Загрузка данных
External <- GET("https://api.etherscan.io/api?module=contract&action=getabi&address=0x02C60D28be3338014Fef3Fdf50a3218B946C0609&type=external&apikey=public")

Internal <- GET("https://api.etherscan.io/api?module=contract&action=getabi&address=0x02C60D28be3338014Fef3Fdf50a3218B946C0609&type=internal&apikey=public")


total_invested <- sum(External$value) #Инвестированно в проект
total_out <- sum(Internal$Value) #Выведенно из проекта
invested_per_day <-
  aggregate(External["value"], by = External["date"], sum) #Сколько инвестировали в этот день
out_per_day <-
  aggregate(Internal["Value"], by = Internal["Date"], sum) #Сколько вывели вывели в этот день
days_alive <- length(out_per_day$Date)


total_in_progress <- c() # Динамика инвестированных средств (наростающий итог)
total_in_progress[1] <- invested_per_day$value[1]
for (i in seq(2, length(invested_per_day$value), by = 1)) {
  total_in_progress[i] <-
    total_in_progress[i - 1] + invested_per_day$value[i]
}



total_out_progress <- c() # Динамика выведенных средств
total_out_progress[1] <- out_per_day$Value[1]
for (i in seq(2, length(out_per_day$Value), by = 1)) {
  total_out_progress[i] <-
    total_out_progress[i - 1] + out_per_day$Value[i]
}



l <- length(total_in_progress)
if (l < days_alive) {
  total_in_progress[(l + 1):days_alive] <- total_in_progress[l]
}

bank_that_day <- total_in_progress - total_out_progress

total_in_progress
total_out_progress
bank_that_day

l <- c(1:days_alive)

#Динамика проекта
ggplot() +
  labs(
    title = "Динамика проекта",
    x = "Дни",
    y = "ETH",
    colour = "Легенда"
  ) + scale_x_continuous(breaks = seq(1, days_alive, 3)) +
  geom_line(aes(l, total_in_progress, colour = "Прогресс депозитов", size = I(2))) +
  geom_line(aes(l, bank_that_day, colour = "Банк в момент времени", size = I(2))) +
  geom_line(aes(l, total_out_progress, colour = "Прогресс выводов", size = I(2)))


a <- invested_per_day$value
b <- out_per_day$Value
if (length(a) < length(b)) {
  a[(length(a) + 1):length(b)] <- 0
}


#Анализ счетов
x <- aggregate(External["value"], by = External["from"], sum) #Введено этим адресом
y <- aggregate(Internal["Value"], by = Internal["To"], sum) #Выведено этим адресом

#Таблица инвесторов
z <- c()
for (i in seq(1, length(x$from), by = 1)) {
  for (j in seq(1, length(y$To), by = 1)) {
    if (x$from[i] == y$To[j]) {
      z[i] <- y$Value[j]
      break
    } else {
      z[i] <- 0
    }
  }
}

percent <- (z / x$value - 1) * 100

t1 <-
  data.frame(
    Address = x$from,
    In = x$value,
    Out = z,
    Percent_made = percent
  )

n_profit <- sum(percent > 0) #Количество прибыльных
n_loss <- sum(percent < 0) #Количество убыточных
n_dead_loss <- sum(percent < -90) #Количетсов "мертвых" инвесторов

max_profit <- max(percent)
max_loss <- min(percent)
avg_profit <- sum(percent[percent > 0]) / n_profit
avg_loss <- sum(percent[percent < 0]) / n_loss
avg_loss_wo_dead <- sum(percent[percent < 0 & percent > (-90)]) / (n_loss - n_dead_loss)

