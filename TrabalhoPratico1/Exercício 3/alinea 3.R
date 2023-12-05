#3) dividir as 99 viaturas em tês grupos com 4 cilindros, 6 e 8

# ler os dados do arquivo csv
dados <- read.csv("C:/Users/kefil/OneDrive/Documentos/ANADI/trabalho1/DADOS3.csv")
View (dados)

# criar dataframes para cada grupo de cilindros
cilindro_4 <- subset(dados, Cylinders == 4)
cilindro_6 <- subset(dados, Cylinders == 6)
cilindro_8 <- subset(dados, Cylinders == 8)

# teste de normalidade de Lilliefors para cada grupo de cilindros
# foi optado o teste de Lilliefors uma vez que é o mais aconselhado para amostras n>=30
library(nortest)

teste_norm_4c <- lillie.test(cilindro_4$Acceleration)
teste_norm_6c <- lillie.test(cilindro_6$Acceleration)
teste_norm_8c <- lillie.test(cilindro_8$Acceleration)

# imprimir os resultados dos testes de normalidade
print(paste("4 cilindros: D =", round(teste_norm_4c$statistic, 3), "p =", round(teste_norm_4c$p.value, 4)))
print(paste("6 cilindros: D =", round(teste_norm_6c$statistic, 3), "p =", round(teste_norm_6c$p.value, 4)))
print(paste("8 cilindros: D =", round(teste_norm_8c$statistic, 3), "p =", round(teste_norm_8c$p.value, 4)))

# teste de Kruskal-Wallis para verificar diferenças significativas entre os grupos
kruskal.test(Acceleration ~ Cylinders, data = dados)


# O teste de Kruskal-Wallis indica que há diferenças significativas na aceleração entre os grupos de cilindros
# com um p-value 2.795e-11
# podemos concluir que há diferenças significativas na aceleração entre os grupos de 4, 6 e 8 cilindros.


#b) 

#i)

# Criar uma variável dummy para os cilindros
dados$Cylinders_dummy <- as.factor(ifelse(dados$Cylinders == 4, "4 cilindros", ifelse(dados$Cylinders == 6, "6 cilindros", "8 cilindros")))

# Modelo de regressão linear múltipla
modelo <- lm(Acceleration ~ Cylinders_dummy + Weight + Horsepower, data = dados)

# Resumo do modelo
summary(modelo)

#ii)

nova_obs <- data.frame(Weight = 2950, Horsepower = 100, Cylinders_dummy = "4 cilindros")
nova_obs$Acceleration <- predict(modelo, newdata = nova_obs)
nova_obs$Acceleration

