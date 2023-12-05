setwd("C:/Users/saol0/Documents/anadi_1200625_1200628_1200882/TrabalhoPratico2")
setwd("C:/Users/Carrusca/Documents/anadi_1200625_1200628_1200882/TrabalhoPratico2")
setwd("C:/Users/kefil/OneDrive/Documentos/anadi_1200625_1200628_1200882/TrabalhoPratico2")

# Bibliotecas
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(corrplot)
library(neuralnet)
library(caret)
library(rpart)
library(rpart.plot)
library(neuralnet)


# Ex1
# Ler o arquivo CSV
ciclismo <- read.csv("ciclismo.csv", header = TRUE, sep = ",")

# Ex2
# Converter a coluna "dob" para o formato de data (ano/mês/dia)
ciclismo$dob <- ymd(ciclismo$dob) 

# Calcula a idade com a função interval
ciclismo$Age <- interval(ciclismo$dob, Sys.Date()) %/% years(1)

# Ex3
# Resumo estatístico dos atributos numéricos
summary(ciclismo[, c("Age", "altitude_results", "vo2_results", "hr_results")])

# Constatar os dados


# Gráfico de barras para o atributo 'gender'
ggplot(ciclismo, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de género dos ciclistas")

# O género feminino encontra-se em superioridade em número do que os homens

# Calculando o número máximo e mínimo de ciclistas por continente
max_cyclists <- max(table(ciclismo$Continent))
min_cyclists <- min(table(ciclismo$Continent))

# Definindo as cores das barras
bar_colors <- ifelse(table(ciclismo$Continent) == max_cyclists, "red",
                     ifelse(table(ciclismo$Continent) == min_cyclists, "blue", "steelblue"))

# Obtendo os continentes com o máximo e mínimo de ciclistas
continents_max_min <- names(table(ciclismo$Continent)[c(table(ciclismo$Continent) == max_cyclists |
                                                          table(ciclismo$Continent) == min_cyclists)])

# Gráfico de barras para o atributo 'Continent'
ggplot(ciclismo, aes(x = Continent, fill = Continent)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = bar_colors, guide = guide_legend(title = "Ciclistas",
                                                              override.aes = list(shape = NA),
                                                              labels = continents_max_min)) +
  labs(title = "Distribuição dos ciclistas por continente") +
  theme(legend.position = "none")
# Constar factos sobre os dados


# Gráfico de caixa para o atributo 'Pro level' e 'vo2_results'
ggplot(ciclismo, aes(x = Pro.level, y = vo2_results)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Distribuição dos resultados de VO2 por nível profissional", x = "Nível de competição do ciclista", y="Volume de oxigênio máximo")

#Conseguimos concluir que os ciclistas com Pro level a World Tour têm uma mediana
#superior aos de Continental, ou seja, conseguem ter melhores resultados a 
#nível de oxigénio máximo. Ambos têm dois outliers, sendo que, ainda assim os 
#outliers dos WorldTour têm resultados superiores comparando com os Continental


# Gráfico de barras para comparar resultados de altitude entre ciclistas com e sem treinos completos
ggplot(ciclismo, aes(x = Winter.Training.Camp, y = altitude_results, fill = Winter.Training.Camp)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Comparação dos resultados de altitude entre ciclistas com e sem treinos completos",
       x = "Treinos de Inverno",
       y = "Resultados de Altitude Médios") +
  scale_fill_manual(values = c("gray", "steelblue"), labels = c("Não Completos", "Completos"))

# Os atletas que completaram o campo de treinos do inverno atingiram melhores resultados
# em média do que os que não fizeram


# Gráfico de caixa para comparar resultados de frequência cardíaca entre continentes
ggplot(ciclismo, aes(x = Continent, y = hr_results, fill = Continent)) +
  geom_boxplot() +
  labs(title = "Comparação dos resultados de frequência cardíaca entre continentes",
       x = "Continente",
       y = "Resultados de Frequência Cardíaca") +
  theme(legend.position = "none")

# Conseguiu-se analisar que os a Ásia, Austrália e Europa têm 1 outlier cada um,
# que provalvelmente serão erros técnicos na leitura. O continente que apresenta
# a mediana de resutados de frequência cardíaca mais baixos é a Africa e com o
# mais alto é a Ásia


# Gráfico de dispersão para comparar resultados de altitude, frequência cardíaca e idade
ggplot(ciclismo, aes(x = hr_results, y = altitude_results, color = Age)) +
  geom_point() +
  labs(title = "Relação entre resultados de altitude, frequência cardíaca e idade",
       x = "Resultados de Frequência Cardíaca",
       y = "Resultados de Altitude",
       color = "Idade") +
  scale_color_viridis_c()

# Apesar de ser um gráfico que mostra a dispersão de idades na relação entre 
# resultados de frequência cardíaca e altitude, não é fácil tirar conclusões sobre a 
# idade com melhor desempenho nesta relação, por isso, decidimos analisar em separado




# Criar faixas de idade
ciclismo$faixa_idade <- cut(ciclismo$Age, breaks = c(16, 23, 27, 31, 35, 40), labels = c("16-22", "23-26", "27-30", "31-34", "35-40"), include.lowest = TRUE, right = TRUE)

# Calcular a média dos resultados de altitude por faixa de idade
media_altitude <- ciclismo %>%
  group_by(faixa_idade) %>%
  summarize(media_altitude = mean(altitude_results, na.rm = TRUE))

# Identificar a faixa de idade com o melhor resultado de altitude
melhor_resultadoAlt <- media_altitude %>%
  filter(media_altitude == max(media_altitude))

# Identificar a faixa de idade com o pior resultado de altitude
pior_resultadoAlt <- media_altitude %>%
  filter(media_altitude == min(media_altitude))

# Reordenar as faixas de idade
media_altitude <- media_altitude %>%
  mutate(faixa_idade = factor(faixa_idade, levels = c("16-22", "23-26", "27-30", "31-34", "35-40")))

# Gráfico de barras para analisar a média dos resultados de altitude por faixa de idade
ggplot(media_altitude, aes(x = faixa_idade, y = media_altitude)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_bar(data = melhor_resultadoAlt, aes(fill = faixa_idade), stat = "identity", fill = "red") +
  geom_bar(data = pior_resultadoAlt, aes(fill = faixa_idade), stat = "identity", fill = "blue") +
  labs(title = "Média dos resultados de altitude por faixa de idade",
       x = "Faixa de Idade",
       y = "Média dos Resultados de Altitude")


#Primeiro, analisámos os resultados das médias das alturas alcançadas por idade. 
#Foi testemunhado que dos 16 até aos 40 anos é quando se atinge os melhores resultados e em média, 
#faixa de idades com os melhores resultados na altitude é entre os 23-26 anos, enquanto que a pior é dos 16-22.


# Calcular a média dos resultados de altitude por faixa de idade
media_hr <- ciclismo %>%
  group_by(faixa_idade) %>%
  summarize(media_hr = mean(hr_results, na.rm = TRUE))

# Identificar a faixa de idade com o melhor resultado
melhor_resultadoHr <- media_hr %>%
  filter(media_hr == max(media_hr))

# Identificar a faixa de idade com o pior resultado
pior_resultadoHr <- media_hr %>%
  filter(media_hr == min(media_hr))

# Reordenar as faixas de idade
media_hr <- media_hr %>%
  mutate(faixa_idade = factor(faixa_idade, levels = c("16-22", "23-26", "27-30", "31-34", "35-40")))

# Gráfico de barras para analisar a média dos resultados de frequência cardíaca por faixa de idade
ggplot(media_hr, aes(x = faixa_idade, y = media_hr)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_bar(data = melhor_resultadoHr, aes(fill = faixa_idade), stat = "identity", fill = "red") +
  geom_bar(data = pior_resultadoHr, aes(fill = faixa_idade), stat = "identity", fill = "blue") +
  labs(title = "Média dos resultados de frequência cardíaca por faixa de idade",
       x = "Faixa de Idade",
       y = "Média dos Resultados de frequência cardíaca")

#Depois, analisamos as médias de resultados de frequência cardíaca por faixa de idade.
#Foi também testemunhado que dos 16 até aos 40 anos é quando se atinge os melhores resultados e em média, 
#faixa de idades com os melhores resultados na altitude é entre os 23-26 anos, enquanto que a pior é dos 16-22.

#Exercício 4
#alinea a

# Identificar valores ausentes (NA)
na_indices <- is.na(ciclismo)

# Verificar se há valores NA em cada variável
colSums(na_indices)

# Limpar o dataSet removendo as observações com NA
dados_limpos <- ciclismo[complete.cases(ciclismo), ]

# Verificar as dimensões do conjunto de dados original e do conjunto de dados limpo
dim(ciclismo)  # Dimensões do conjunto de dados original
dim(dados_limpos)  # Dimensões do conjunto de dados limpo

#conclusão: através dos resultados obtidos, concluimos que não existem valores ausentes em nenhuma das 
#variaveis, indicado pelo valor 0 em todos os valores de saida

#alinea b)

# Histograma para altitude_results
ggplot(ciclismo, aes(x = altitude_results)) +
  geom_histogram()

# Histograma para vo2_results
ggplot(ciclismo, aes(x = vo2_results)) +
  geom_histogram()

# Histograma para hr_results
ggplot(ciclismo, aes(x = hr_results)) +
  geom_histogram()

#alinea c)

# Especificar as configurações do controle do RFE
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Definir o número de atributos a serem selecionados
num_atributos <- 5

# Definir o nome ou índice da coluna alvo em ciclismo
target_coluna <- "nome_coluna_alvo"  # Substitua pelo nome ou índice correto

# Definir x e y com base nos dados de ciclismo
x <- ciclismo[, -which(names(ciclismo) == target_coluna)]
y <- ciclismo[[target_coluna]]

# Verificar o número de amostras em x e y
if (nrow(x) != length(y)) {
  stop("O número de amostras em x e y não é o mesmo.")
}

# Realizar a seleção de atributos
resultado_selecao <- rfe(x = x, y = y, sizes = num_atributos, rfeControl = ctrl)

# Exibir os resultados da seleção de atributos
print(resultado_selecao)


#alinea d)

# Selecionar apenas as colunas numéricas
colunas_numericas <- sapply(ciclismo, is.numeric)
ciclismo_numericas <- ciclismo[, colunas_numericas]

# Normalização dos dados numéricos
ciclismo_normalizado <- as.data.frame(scale(ciclismo_numericas))



#5 
ciclismo_normalizado <- ciclismo_normalizado[, -1]

correlation_matrix <- cor(ciclismo_normalizado)

corrplot(correlation_matrix, is.corr = FALSE, method = "circle", type = "upper", tl.cex = 0.8)

# Exercício 6
set.seed(123)

dataSet6 <- ciclismo_numericas

sample <- sample(c(TRUE, FALSE), nrow(dataSet6), replace=TRUE, prob=c(0.7,0.3))
dataSet6.train <- dataSet6[sample,]
dataSet6.test <- dataSet6[!sample,]

slr.model <- lm(altitude_results ~ hr_results, data = dataSet6.train)
summary(slr.model) 

plot(dataSet6.train$hr_results, dataSet6.train$altitude_results, col = "steelblue", main = "altitude_results ~ hr_results", xlab = "hr_results", ylab = "altitude_results")
abline(slr.model$coefficients[1],slr.model$coefficients[2], col='red')

slr.pred <- predict(slr.model, dataSet6.test)
dif<-dataSet6.test$altitude_results-slr.pred

MAE <- mean(abs(dif))
RMSE <- sqrt(mean(dif^2))

mlr.model <- lm(altitude_results ~ ., 
                data = dataSet6.train)

summary(mlr.model)$coef

mlr.pred <- predict(mlr.model,dataSet6.test)

dif <- dataSet6.test$altitude_results-mlr.pred  

mae <- mean(abs(dif))
rmse <- sqrt(mean(dif^2))

# Exercício 7 e 8

# Configurar a semente aleatória para reprodução dos resultados
set.seed(123)

# Selecionar o conjunto de dados numericos para o exercício
dataSet7 <- ciclismo_numericas 

# Dividir o conjunto de dados em treinamento e teste usando amostragem aleatória
sample <- sample(c(TRUE, FALSE), nrow(dataSet7), replace=TRUE, prob=c(0.7,0.3))
dataSet7.train <- dataSet7[sample,]
dataSet7.test <- dataSet7[!sample,]

# Regressão linear múltipla (Exercício 7a)
mlr.model <- lm(vo2_results ~ ., data = dataSet7.train)

#Sumário do modelo
summary(mlr.model)$coef
mlr.pred <- predict(mlr.model,dataSet7.test)

#Cáluclo dos indicadores de erro
mae <- MAE(mlr.pred,dataSet7.test$altitude_results)
rmse <- RMSE(mlr.pred,dataSet7.test$altitude_results)

mae
rmse

# Árvore de regressão (Exercício 7b)
tree.model <- rpart(vo2_results ~ ., method="anova", data=dataSet7.train)

# Plot para verificação
rpart.plot(tree.model)
rpart.plot(tree.model,digits=3, fallen.leaves = TRUE, type = 4, extra = 101)

tree.pred <- predict(tree.model, dataSet7.test)

#Cálculo dos indicadores 
mae1 <- MAE(tree.pred,dataSet7.test$altitude_results)
rmse1 <- RMSE(tree.pred,dataSet7.test$altitude_results)

mae1
rmse1

# Rede neural (Exercício 7c)
# Normalização do dataSet

normalise <- function(y) {
  (y - min(y)) / (max(y) - min(y))
}

#Normalizar os dados
dataSet7.norm <- as.data.frame(lapply(dataSet7, normalise))

dataSet7.norm.train <- dataSet7.norm[sample,]
dataSet7.norm.test <- dataSet7.norm[!sample,]

# Definir o número de nós internos (parâmetro a ser variado)
numnodes <- 1

# Criação do modelo da rede neural
neuralNetwork.model <- neuralnet(vo2_results ~ ., data = dataSet7.norm.train, hidden = numnodes)

# Plotar a rede neural
plot(neuralNetwork.model)

# Fazer previsões com a rede neural
neuralNetwork.pred <- compute(neuralNetwork.model, dataSet7.norm.test)

minmaxdesnorm <- function(x, goal.attrib) {
  return (x * (max(goal.attrib) - min(goal.attrib)) + min(goal.attrib))
}

# Desnormalizar as previsões
neuralNetwork.pred.vo2 <- minmaxdesnorm(neuralNetwork.pred$net.result, dataSet7$vo2_results)
test.vo2 <- minmaxdesnorm(dataSet7.norm.test$vo2_results, dataSet7$vo2_results)

# Calcular MAE e RMSE
neuralNetwork.mae <- mean(abs(neuralNetwork.pred.vo2 - test.vo2))
neuralNetwork.rmse <- sqrt(mean((neuralNetwork.pred.vo2 - test.vo2)^2))



# Verificar se os resultados obtidos são  significativos, 5%,
# Modelo de regressão linear múltipla e modelo de rede neuronal

# p-value =  2.148e-12 < 0.05 - distribuição não normal
lillie.test(mlr.pred - neuralNetwork.pred.vo2) # amostra > 30

# p-value = 0.1729 > 0.05 - não há diferença significativa entre os modelos
wilcox.test(mlr.pred, neuralNetwork.pred.vo2, paired = TRUE) 


#CLASSIFICAÇÃO 

# Bibliotecas
library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(Metrics)
library(caret)
library(class)
library(FNN)
library(nortest)
library(moments)

#Exercicio 1

# capacidade preditiva relativamente ao atributo “Pro_level”
# One hot encoding 
col_catego <- c("Team", "Background", "Continent")

dummy_obj <- dummyVars(~., data = ciclismo[, col_catego])

data_encoded <- predict(dummy_obj, newdata = ciclismo)

# Transformar os dados encoded em dataframe
ciclismo_encoded <- as.data.frame(data_encoded)

# Adicionar os atributos numéricos do dataframe ciclismo ao dataframe ciclismo_encoded
ciclismo_encoded$altitude_results <- ciclismo$altitude_results
ciclismo_encoded$vo2_results <- ciclismo$vo2_results
ciclismo_encoded$hr_results <- ciclismo$hr_results
ciclismo_encoded$age <- ciclismo$Age
ciclismo_encoded$gender <- as.numeric(ciclismo$gender == 'male')
ciclismo_encoded$ProLevel <- as.numeric(ciclismo$Pro.level == 'Continental')
ciclismo_encoded$WinterTrainingCamp <- as.numeric(ciclismo$Winter.Training.Camp == 'completed')

# Renomeando as colunas do dataframe
colnames(ciclismo_encoded) <- c( 'TeamgroupA', 'TeamgroupB', 'TeamgroupC', 'TeamgroupD', 'TeamgroupE', 'BackgroundCobblestones', 'BackgroundHill',
                                 'BackgroundMountain','BackgroundNone','BackgroundSprinter','BackgroundTimeTrial','ContinentAfrica','ContinentAsia','ContinentAustralia',
                                 'ContinentEurope','ContinentNorthAmerica','ContinentSouthAmerica','altitude_results','vo2_results', 'hr_results', 'gender', 'age','ProLevel','WinterTrainingCamp')

# garantir a reprodutibilidade dos resultados
set.seed(123)

# dataset exercício 1
normalize <- function(x) {   (x - min(x)) / (max(x) - min(x)) }
dados_ex1 <- ciclismo_encoded
dados_ex1.norm <- as.data.frame(lapply(dados_ex1, normalize))


sample <- sample(c(TRUE, FALSE), nrow(dados_ex1.norm), replace=TRUE, prob=c(0.7,0.3))
dados_ex1.train <- dados_ex1.norm[sample,]
dados_ex1.test <- dados_ex1.norm[!sample,]

# Árvore de Decisão  - com todos os atributos
tree.model = rpart(ProLevel ~ . , data = dados_ex1.train, method = "class")

rpart.plot(
  tree.model,
  digits = 3,
  fallen.leaves = TRUE,
  type = 3
)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Decisão")


tree.pred = predict(tree.model, dados_ex1.test, type = 'class')

#confusionMatrix
matrix.conf <- table(dados_ex1.test$ProLevel, tree.pred)

#plot(tree.pred)
df <- data.frame(Predictions = tree.pred)
estatistica(matrix.conf)

#Rede Neuronal
dados_ex1.norm <- as.data.frame(lapply(dados_ex1, normalize))

colnames(dados_ex1.norm) <- c( 'GroupA', 'GroupB', 'GroupC', 'GroupD', 'GroupE', 'Cobblestones', 'Hill',
                                'Mountain','NoneBackground','Sprinter','TimeTrial','Africa','Asia','Australia',
                                'Europe','NorthAmerica','SouthAmerica','altitude_results','vo2_results', 'hr_results', 'age', 'gender','ProLevel',
                                'WinterTrainingCamp')

dados_ex1.norm.train <- dados_ex1.norm[sample,]
dados_ex1.norm.test <- dados_ex1.norm[!sample,]

train_labels <- dados_ex1.norm[sample, "ProLevel"]
test_labels <- dados_ex1.norm[!sample, "ProLevel"]


#internal nodes
numnodes <- 3


neuralNetwork.model <-
  neuralnet(
    ProLevel ~ gender + vo2_results + hr_results + age + altitude_results + WinterTrainingCamp + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
    data = dados_ex1.norm.train,
    hidden = numnodes,
    stepmax=1e7
  )
plot(neuralNetwork.model)

neuralNetwork.pred <- predict(neuralNetwork.model, dados_ex1.norm.test)
neuralNetwork.pred <- ifelse(neuralNetwork.pred > 0.5, "1", "0")

accuracy_neuralNetwork <- sum(neuralNetwork.pred == dados_ex1.norm.test$ProLevel) / length(dados_ex1.norm.test$ProLevel) * 100
matrix.conf <- table(dados_ex1.norm.test$ProLevel, neuralNetwork.pred)
estatistica(matrix.conf)


# KNN

k <- c()
accuracy <- c()
for (i in seq(1, 50, 2)){
  
  knn.pred <- knn(train = dados_ex1.norm.train,
                  test = dados_ex1.norm.test,
                  cl = train_labels, k=i) 
  
  conMatrix <- table(test_labels, knn.pred)
  
  accuracy <- c(accuracy, sum(diag(conMatrix))/sum(conMatrix))
  
  k <- c(k,i)
}

resultViz <- data.frame(k, accuracy)
resultViz[resultViz$accuracy == max(resultViz$accuracy), ]
plot(resultViz$k, resultViz$accuracy)

k[which.max(accuracy)]
plot(
  resultViz$k,
  resultViz$accuracy,
  col = ifelse(
    resultViz$accuracy == max(resultViz$accuracy),
    'red1',
    'black'
  )
)

#modelo knn
modelo <- knn(train = dados_ex1.norm.train,
                 test = dados_ex1.norm.test,
                 cl = train_labels,
                 k = k[which.max(accuracy)]
) 
# Confusion Matrix
matrix.conf <- table(dados_ex1.norm.test$ProLevel, modelo)
estatistica(matrix.conf)


# 2 melhores modelos - k-fold cross validation (knn e árvore de decisão)

cvf <- 10
folds <- sample(1:cvf, nrow(dados_ex1), replace = TRUE)

#Fold size
table(folds)

k=k[which.max(accuracy)]

accuracy <- matrix(nrow = cvf, ncol = 2)

NumCols <- dim(dados_ex1)[2]

for (i in 1:cvf){
  
  train.cv <- dados_ex1.norm[folds != i, ]
  test.cv <- dados_ex1.norm[folds == i, ]
  
  train_labels <- dados_ex1[folds != i, "ProLevel"]
  testLabels <- dados_ex1[folds == i, "ProLevel"]
  
  knn.pred <- knn(train=train.cv[,-NumCols], test=test.cv[,-NumCols], cl=train_labels, k) 
  cfmatknn <- table(testLabels,knn.pred)
  
  rpart.model <- rpart(ProLevel ~ ., method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  cfmatrpart <- table(testLabels,rpart.pred)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatrpart))/sum(cfmatrpart)) 
}

accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)

# Existe diferença significativa no desempenho dos dois melhores modelos  obtidos  anteriormente?

shapiro.test(accuracy[,1]-accuracy[,2]) # p-value =  0.02661, existe normalidade

t.test<-t.test(accuracy[,1], accuracy[,2], paired = TRUE)

p_value <- t.test$p.value # p-value < alpha

# como p-value < alpha, rejeita-se a hipótese nula (H0) e conclui-se que existe diferença significativa no desempenho dos dois melhores modelos obtidos anteriormente
# como a média da acurácia do modelo de KNN é superior à média da acurácia do modelo de árvore de decisão, conclui-se que o modelo de KNN é o melhor modelo

# Comparação dos modelos
# Accuracy; Sensitivity; Specificity e F1. 
estatistica <- function(matrix.conf) {
  accuracy <- 100 * round((matrix.conf[1, 1] + matrix.conf[2, 2]) / sum(matrix.conf), 4)
  recall = matrix.conf[1, 1] / (matrix.conf[1, 1] + matrix.conf[1, 2])
  precision = matrix.conf[1, 1] / (matrix.conf[1, 1] + matrix.conf[2, 1])
  f1 = (2 * precision * recall) / (precision + recall)
  specifity <- matrix.conf[2,2] / sum(matrix.conf[2,])
  
  
  message("accuracy: ", accuracy, "%")
  message("Recall: ", recall)
  message("precision: ", precision)
  message("F1: ", f1)
  message("specifity: ", specifity)
  
  my_list <-
    list(
      "F1" = f1,
      "precision" = precision,
      "recall" = recall,
      "accuracy" = accuracy,
      "specifity" = specifity
      
    )
  return(my_list)
}

#----------------------------------------------------------------------------------

#EXERCÍCIO 2


# Assegurar a replicabilidade dos resultados
set.seed(123)

# Dataset exercício 2
dados_ex2 <- ciclismo_encoded
dados_ex2.norm <- as.data.frame(lapply(dados_ex2, normalize))

colnames(dados_ex2.norm) <- c( 'GroupA', 'GroupB', 'GroupC', 'GroupD', 'GroupE', 'Cobblestones', 'Hill',
                                'Mountain','NoneBackground','Sprinter','TimeTrial','Africa','Asia','Australia',
                                'Europe','NorthAmerica','SouthAmerica','altitude_results','vo2_results', 'hr_results', 'age', 'gender','ProLevel',
                                'WinterTrainingCamp')

sample <- sample(c(TRUE, FALSE), nrow(dados_ex2.norm), replace=TRUE, prob=c(0.7,0.3))
dados_ex2.train <- dados_ex2.norm[sample,]
dados_ex2.test <- dados_ex2.norm[!sample,]

# Árvore de Decisão 

tree.model = rpart(WinterTrainingCamp ~ . , data = dados_ex2.train, method = "class")

rpart.plot(
  tree.model,
  digits = 3,
  fallen.leaves = TRUE,
  type = 3
)

rpart.plot(tree.model, digits=3, cex=0.85, main="Árvore de Decisão")


tree.pred = predict(tree.model, dados_ex2.test, type = 'class')

#confusionMatrix
matrix.conf <- table(dados_ex2.test$WinterTrainingCamp, tree.pred)
estatistica(matrix.conf)

# Rede Neuronal

#-------------------------------
#internal node
numnodes <- 1


neuralNetwork.model <-
  neuralnet(
    WinterTrainingCamp ~ gender + vo2_results + hr_results + age + altitude_results + ProLevel + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
    data = dados_ex2.train,
    hidden = numnodes,
    stepmax=1e7
  )
plot(neuralNetwork.model)

neuralNetwork.pred <- predict(neuralNetwork.model, dados_ex2.test)
neuralNetwork.pred <- ifelse(neuralNetwork.pred > 0.5, "1", "0")

matrix.conf <- table(dados_ex2.test$WinterTrainingCamp, neuralNetwork.pred)
estatistica(matrix.conf)

# k-fold cross validation

cvf <- 10
folds <- sample(1:cvf, nrow(dados_ex2), replace = TRUE)

#Fold size
table(folds)

accuracy <- matrix(nrow = cvf, ncol = 2)

nuCols <- dim(dados_ex2)[2]

for (i in 1:cvf){
  
  train.cv <- dados_ex2.norm[folds != i, ]
  test.cv <- dados_ex2.norm[folds == i, ]
  
  train_labels <- dados_ex2[folds != i, "WinterTrainingCamp"]
  test_labels <- dados_ex2[folds == i, "WinterTrainingCamp"]
  
  # Rede Neuronal
  neuralNetwork.model <-
    neuralnet(
      WinterTrainingCamp ~ gender + vo2_results + hr_results + age + altitude_results + ProLevel + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
      data = train.cv,
      hidden = numnodes,
      stepmax=1e7
    )
  
  # Fazer as previsões 
  predictions_neuralNetwork <- predict(neuralNetwork.model,test.cv)
  predictions_neuralNetwork <- ifelse(predictions_neuralNetwork > 0.5, "1", "0")
  
  # Matriz de confusão
  cfmatknn <- table(test_labels,predictions_neuralNetwork)
  
  
  # Árvore de Decisão
  rpart.model <- rpart(WinterTrainingCamp ~ ., method="class", data=train.cv)
  rpart.pred <- predict(rpart.model, test.cv, type = "class")
  cfmatrpart <- table(test_labels,rpart.pred)
  
  accuracy[i, ] <- c( sum(diag(cfmatknn))/sum(cfmatknn),
                      sum(diag(cfmatrpart))/sum(cfmatrpart)) 
}

accuracy

apply(accuracy,2,mean)
apply(accuracy,2,sd)

# Existe diferença significativa no desempenho dos dois melhores modelos  obtidos  anteriormente?


shapiro.test(accuracy[,1]-accuracy[,2]) # p-value =  0.02661, existe normalidade

t.test<-t.test(accuracy[,1], accuracy[,2], paired = TRUE)

p_value <- t.test$p.value # p-value < alpha

#EXERCICIO 3


# Capacidade preditiva relativamente ao atributo "Gender"
set.seed(123)

# Rede Neural
nn.model <- neuralnet(
  gender ~ altitude_results + vo2_results + hr_results + age + ProLevel + WinterTrainingCamp + GroupA + GroupB + GroupC + GroupD + GroupE + Cobblestones + Hill + Mountain + NoneBackground + Sprinter + TimeTrial + Africa + Asia + Australia + Europe + NorthAmerica + SouthAmerica,
  data = dados_ex1.norm.train,
  hidden = numnodes,
  stepmax = 1e7
)

nn.pred <- predict(nn.model, dados_ex1.norm.test)
nn.pred <- ifelse(nn.pred > 0.5, "male", "female")

matrix.conf_nn <- table(dados_ex1.norm.test$gender, nn.pred)
estatistica(matrix.conf_nn)

# KNN
k <- 5  # Defina o valor de k corretamente

train_labels <- train_labels[1:nrow(dados_ex1.norm.train)]  # Ajuste os rótulos de treinamento

knn.pred <- knn(
  train = dados_ex1.norm.train,
  test = dados_ex1.norm.test,
  cl = train_labels,
  k = k
)

matrix.conf_knn <- table(dados_ex1.norm.test$gender, knn.pred)
estatistica(matrix.conf_knn)

# 2 melhores modelos - k-fold cross validation (rede neuronal e KNN)
cvf <- 10
folds <- sample(1:cvf, nrow(dados_ex1), replace = TRUE)

accuracy <- matrix(nrow = cvf, ncol = 2)

for (i in 1:cvf) {
  train.cv <- dados_ex1.norm[folds != i, ]
  test.cv <- dados_ex1.norm[folds == i, ]
  
  train_labels <- dados_ex1[folds != i, "gender"]
  test_labels <- dados_ex1[folds == i, "gender"]
  
  nn.pred <- predict(nn.model, test.cv)
  nn.pred <- ifelse(nn.pred > 0.5, "male", "female")
  cfmat_nn <- table(test_labels, nn.pred)
  
  knn.pred <- knn(
    train = train.cv[, -NumCols],
    test = test.cv[, -NumCols],
    cl = train_labels,
    k
  )
  cfmat_knn <- table(test_labels, knn.pred)
  
  accuracy[i, ] <- c(
    sum(diag(cfmat_nn)) / sum(cfmat_nn),
    sum(diag(cfmat_knn)) / sum(cfmat_knn)
  )
}

# Média e desvio padrão da taxa de acerto
mean_accuracy <- apply(accuracy, 2, mean)
sd_accuracy <- apply(accuracy, 2, sd)

mean_accuracy
sd_accuracy

# Teste de diferença significativa
shapiro.test(accuracy[, 1] - accuracy[, 2])  # Teste de normalidade

t.test(accuracy[, 1], accuracy[, 2], paired = TRUE)  # Teste t-pareado

# Comparação dos modelos
estatistica(matrix.conf_nn)
estatistica(matrix.conf_knn)