#1a
getwd()
setwd("C:/Users/Carrusca/Downloads")
teste <- read.csv("DADOS1.csv")
dados <- read.csv("DADOS1.csv", skip = 2)
dados$s <- as.numeric(dados$s) 
dados$Anos <- as.POSIXct(dados$s, origin = "1970-01-01", tz = "GMT")
colnames(dados)[colnames(dados) == "bbl.d.2"] <- "Bomba 1"
colnames(dados)[colnames(dados) == "bbl.d.5"] <- "Bomba 2"

#1b
library(ggplot2)
dados_filtrados <- dados[dados$Anos >= as.POSIXct("2013-08-04 00:00:00", tz = "GMT") & 
                           dados$Anos < as.POSIXct("2013-08-05 00:00:00", tz = "GMT"), 
                         c("Anos", "K.1", "K.3", "K.5")]
ggplot(dados_filtrados, aes(x = Anos)) +
  geom_line(aes(y = K.1, color = "K.1"), size = 1) +
  geom_line(aes(y = K.3, color = "K.3"), size = 1) +
  geom_line(aes(y = K.5, color = "K.5"), size = 1) +
  labs(title = "Comparação de temperatura do motor nas bombas 1, 2 e 3",
       x = "Data e Hora",
       y = "Temperatura do motor") +
  scale_color_manual(name = "Bomba", 
                     values = c("K.1" = "red", "K.3" = "blue", "K.5" = "green")) +
  theme_bw()


#1c)

library(dplyr)
library(ggplot2)

dados_filtrados_junto <- reshape2::melt(dados_filtrados, id.vars = "Anos")

ggplot(data = dados_filtrados_junto, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Bomba", y = "Temperatura (°C)",
       title = "Temperatura do motor nas bombas 1, 2 e 3\nem 4 de agosto de 2013") +
  scale_x_discrete(labels = c("Bomba 1", "Bomba 2", "Bomba 3"))

stats <- aggregate(value ~ variable, data = dados_filtrados_junto, 
                   FUN = function(x) c(min = min(x), quantile(x, c(0.25, 0.5, 0.75)), max = max(x)))

# imprimindo os valores relevantes
select(stats, variable, value)



#1d)

# Load the necessary packages
library(tidyverse)
#1
# Create a subset of your data for March 2014
oilRate_march <- dados[dados$Anos >= as.POSIXct("2014-03-01 00:00:00", tz = "GMT") & 
                         dados$Anos < as.POSIXct("2014-04-01 00:00:00", tz = "GMT"), ]

# Reshape the data into long format
oilRate_march_long <- oilRate_march %>%
  pivot_longer(cols = c("Bomba 1", "Bomba 2"), names_to = "Gasoline Station", values_to = "Value") %>%
  mutate(Anos = as.Date(Anos))

# Calculate the average per day for each gasoline station
oilRate_march_avg <- oilRate_march_long %>%
  group_by(Anos, `Gasoline Station`) %>%
  summarise(Average = mean(Value))

# Create a bar plot
grafico_barras <- oilRate_march_avg %>%
  ggplot(aes(x = Anos, y = Average, fill = `Gasoline Station`)) +
  geom_col(position = "dodge") +
  labs(title = "Comparação das Médias Diárias",
       subtitle = "Estações de Gasolina 1 e 2 - Março de 2014",
       x = "Data", y = "Média",
       fill = "Estação de Gasolina") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Display the bar plot
print(grafico_barras)
#2
oilRate_year <- dados %>%
  filter(Anos >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") & Anos < as.POSIXct("2014-06-1 00:00:00", tz = "GMT"))

# Group by month and calculate average of "Bomba 1" values
monthly_avg <- oilRate_year %>%
  group_by(month = format(Anos, "%Y-%m")) %>%
  summarise(total_bomba1 = mean(`Bomba 1`, na.rm = TRUE))

# Find the month with the highest average "Bomba 1" value
max_month <- monthly_avg %>%
  filter(total_bomba1 == max(total_bomba1))

# Print the month with the highest average "Bomba 1" value
cat("The month with the highest average 'Bomba 1' value is:", max_month$month)

# Convert the "month" column to a factor with ordered levels for correct sorting
monthly_avg$month <- factor(monthly_avg$month, levels = unique(monthly_avg$month), ordered = TRUE)

# Create a bar plot with the month with the highest total_bomba1 value highlighted
ggplot(data = monthly_avg, aes(x = month, y = total_bomba1)) +
  geom_bar(stat = "identity", fill = ifelse(monthly_avg$month == max_month$month, "red", "steelblue")) +
  labs(x = "Month", y = "Total Bomba1", title = "Monthly Average Total for Bomba1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability


#3
set.seed(300)
amostra <- sample(1:365,10)
amostra <- amostra - 1

dados_ex3 <- dados[dados$Anos >= as.POSIXct("2013-06-01 00:00:00", tz = "GMT") & 
                     dados$Anos < as.POSIXct("2014-06-1 00:00:00", tz = "GMT"), 
                   c("Anos", "Bomba 1", "Bomba 2")]


dados_ex3$Anos <- as.Date(dados_ex3$Anos)
dados_ex3$`Bomba_1`<- as.numeric(dados_ex3$`Bomba 1`)
dados_ex3$`Bomba_2`<- as.numeric(dados_ex3$`Bomba 2`)
dados_ex3 <- dados_ex3 %>%
  group_by(Anos)%>%
  summarise(Bomba_1 = mean(Bomba_1), Bomba_2 = mean(Bomba_2))



dados_ex3$Anos <- as.numeric(difftime(dados_ex3$Anos, as.Date("2013-06-01")))
dados_ex3$Anos <- dados_ex3$Anos / 24 / 60 / 60
boxp <- subset(dados_ex3, Anos %in% amostra)
boxplot(boxp[,c(2,3)], outline=T)





#4
library(car)
dados_ex4 <- boxp[,c(2,3)]

dados_ling <- stack(dados_ex4)

leveneTest(dados_ling$values,dados_ling$ind,center=mean)

shapiroBomba1 <- shapiro.test(dados_ex4$Bomba_1)
print(shapiroBomba1)
shapiroBomba2 <- shapiro.test(dados_ex4$Bomba_2)
print(shapiroBomba2)

#que é normal porque sao acima de o valor de variancia


t.test(dados_ex4$Bomba_1,dados_ex4$Bomba_2,alternative="greater",paired=F,v.equal=F)

#5
mean(dados_ex4$Bomba_1)
mean(dados_ex4$Bomba_2)

