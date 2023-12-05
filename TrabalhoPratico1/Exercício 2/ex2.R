# Exercício 2) Pretende-se comparar a precisão de 6 algoritmos de Machine Learning: SVM, DT, KN, RF, ML e GB. Para 
# o efeito calculou-se a precisão de cada algoritmo sobre 10 conjuntos de dados: D1, D2, …, D10. Os dados 
# encontram-se guardados no ficheiro DADOS2.csv

#Preparação dos dados

# Primeiro precisamos instalar as libraries para as poder as usar
install.packages("corrplot") # usado para corrplot
library(corrplot)
install.packages("reshape")   # usado para melt
install.packages("reshape2")  # usado para melt
library(reshape2)
library(reshape)
install.packages("dplyr")     # usado para filtrar, selecionar, organizar, resumir e juntar dados
library(dplyr)
install.packages("rstatix")     # usado para identify_outliers
library(rstatix)
install.packages("car")      # usado para leveneTest
library(car)
install.packages("stats")      
library(stats)

# Depois é necessário carregar os dados que serão analisados
# O setwd muda dependendo do pc que esteja
setwd("C:/Users/sergio.andre.lopes/OneDrive - Accenture/Desktop/Exercício 2")
setwd("C:/Users/saol0/Documents/anadi_1200625_1200628_1200882/Exercício 2")
data <- read.csv("DADOS2.csv", header = FALSE) 

# Elimina-se as duas primeiras colunas porque não precisamos delas para a análise
# e só iria prejudicar os resultados
data <- data[,-1]
data <- data[,-1]

# Além disso, é também necessário eliminar a primeira linha, que são os dados do  cabeçalho, 
#ficando assim na tabela apenas com os dados que necessitamos
data <- data[-1,]

# Torna-se agora necessário mudar o nome das colunas, como pedido no enunciado, para ser 
# possível identificar (que foi alterado no passo anterior)
colnames(data) <- c("SVM", "DT", "KN", "RF", "ML", "GB")

# E agora mudamos os nomes das linhas, como pedido no enunciado
rownames(data) <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
View(data)

# Mudar o dataset em numérico
data <- as.data.frame(lapply(data, as.numeric))

# Exercício 2a) 
# Averigue se existe correlação entre a precisão de cada par de algoritmos
# (Apresente a matriz de correlações e comente os resultados)

# É necessário se fazer o teste da normalidade de Shapiro-Wilk, quando n<30

# Vamos assumir o nível de significância alfa=0.05

# H0: é normalmente distribuída
# H1: não é normalmente distribuída

shapiro.test(data$SVM) # o p-value=0.2687 como é > alfa, logo não se rejeita H0
shapiro.test(data$DT) # o p-value=0.0677 como é > alfa, logo não se rejeita H0
shapiro.test(data$KN) # o p-value=0.6926 como é > alfa, logo não se rejeita H0
shapiro.test(data$RF) # o p-value=0.3138 como é > alfa, logo não se rejeita H0
shapiro.test(data$ML) # o p-value=0.0214 como é < alfa, logo rejeita-se H0 e pode-se concluir que não segue 
                        # distribuição normal
shapiro.test(data$GB) # o p-value=0.5125 como é > alfa, logo não se rejeita H0

# Depois disto, ao analisar os testes de shapiro, como todos à exceção de 1 seguem uma distribuição normal,
# deve-se usar o Pearson

# calcula a matriz de correlação das colunas de um objeto de dados
matrix_correl <- cor(data) ; matrix_correl

# Cria o gráfico de correlação a partir de uma matriz de correlação
corrplot(matrix_correl, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("white", "green"))(100), addgrid.col = "black")

# A matriz de correlação tem os coeficientes de correlação de Pearson, visto serem valores compreendidos 
# entre -1 e 1. Além disso, O coeficiente de correlação de Pearson é uma medida estatística que indica 
# o grau de correlação linear entre duas variáveis quantitativas

# Interpretação de resultados:
# O coeficiente de correlação de Pearson será próximo de -1, se dois algoritmos produzem RESULTADOS OPOSTOS

# O coeficiente de correlação de Pearson será próximo de 0, se dois algoritmos produzem RESULTADOS INDEPENDENTES

# O coeficiente de correlação de Pearson será próximo de 1, se dois algoritmos produzem RESULTADOS SEMELHANTES

# Como o coeficiente de correlação de Pearson é superior a 0,7, então é considerado correlação forte

matrix_correl_spearman <- cor(data, method = "spearman") ; matrix_correl_spearman

# Serve para criar múltiplas figuras ou gráficos em uma única janela gráfica
par(mfrow = c(2,1))

# Apresentar a matriz de correlação em forma de gráfico
corrplot(matrix_correl, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("white", "green"))(100), 
         addgrid.col = "black", 
         title="Pearson",
         mar=c(0,0,2,0))

corrplot(matrix_correl_spearman, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black", 
         col = colorRampPalette(c("white", "green"))(100), 
         addgrid.col = "black",
         title="Spearman",
         mar=c(0,0,2,0))

# Exercício 2b) 
# Efetue um teste de hipótese para averiguar se existem diferenças significativas entre a precisão dos 
# diferentes algoritmos

# Organização dos dados fornecidos
# Coloca os dados em duas colunas distintas, Algoritmo e Precisão
# Cada linha do objeto "data_melted" representa uma combinação única de valores das variáveis
data_melted <- melt(data)

# Dá os nomes às colunas
colnames(data_melted) = c("Algoritmo", "Precisão")

#Usa-se para ver os dados 
View(data_melted)

# Decidimos seguir o teste paramétrico One-way ANOVA, visto ser uma ferramenta útil quando se pretende comparar 
# as médias de 2 ou mais grupos independentes (neste caso temos 6) e verificar se existem diferenças estatisticamente
# significativas entre elas, no entanto, para se puder usar, precisamos de algumas regras:
# - O tipo de algoritmo é a variável independente e tem 6 grupos(acima dos 2 necessários)
# - A precisão do algoritmo é variável dependente, sendo que deve ser normalmente distribuída por cada grupo, e contínua
# - Os grupos são independentes entre si
# - Não devem ter outliers significativos
# - A variância deve ser igual para cada grupo

# Depois, para se poder visualizar a distribuição dos dados dos algoritmos, usamos o boxplot
boxplot(data, main = 'Precisão dos diferentes algoritmos', xlab = 'Algoritmos', ylab = 'Precisão')

# Ao observarmos os dados relativamente à precisão do algoritmo ML conseguimos observar outliers significativos. 

# Seguindo para o teste de normalidade de Shapiro-Wilk (quando n é inferior a 30)

# Comprova-se que o algoritmo ML não segue distribuição normal, como já observado na alínea a

# Depois, vamos observar a Homogeneidade de variâncias do teste de Levene

# H0: as variâncias são iguais
# H1: existe pelo menos um par de grupos cujas variâncias são diferentes

leveneTest(Precisão ~ Algoritmo, data_melted,center=mean)
# Como p=0.0001259<0.05, então rejeita-se H0, logo as variâncias dos grupos não são todas iguais

# Podemos concluir que os testes paramétricos não são aplicáveis, visto que nem todos os grupos se assume
# uma distribuição normal e as variâncias dos grupos

# No entanto, em testes não paramétricos, temos o Friedman Test visto que é usado para comparar 3 ou mais 
# grupos emparelhados ou repetidos numa variável contínua e como se usa os mesmos 10 conjuntos de dados, 
# usa-se Friedman Test

# Primeiro, para se puder testar os dados para o Friedman, necessita de se construir de uma coluna a representar
# os Datasets
Dataset <- c(rep(1:10,6))

data_organized <- cbind(Dataset, data_melted)
View(data_organized)
# Ficamos assim com os dados devidamente organizados em datasets e fica em falta verficar se os types associados aos
# valores de cada variável está apropriada
glimpse(data_organized)

# Transforma-se os valores int em factor de variável Dataset
data_organized$Dataset <- factor(data_organized$Dataset)

# H0: Não existem diferenças significativas entre a precisão dos algoritmos
# H1: Existe pelo menos um algoritmo cuja precisão é diferente dos restantes

# Teste Friedman
friedman_test <- friedman.test(Precisão ~ Algoritmo | Dataset, data = data_organized); friedman_test

# Como p-value=0.1212>alfa=0.05, não se rejeita H0, logo não se pode afirmar que existem diferenças significativas
# entre as precisões dos diferentes tipos de algoritmos



# Exercício 2c)
# Justifique todas as opções e decisões tomadas na alínea anterior. No caso de haver diferenças significativa
# faça um estudo post-hoc do teste que efetuou

# À medida que for fazendo o exercício está descrita a justificação