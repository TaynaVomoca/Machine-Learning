#####################################
#Explorando e preparando os dados
#####################################

#Removendo a primeira e a ultima coluna
dados_cancer <- Dados_cancer[c(-1, -33)]

#Tabela de frequancia para os rótulos
table(dados_cancer$diagnosis)

#Codifificando a variável como fator
dados_cancer$diagnosis <- factor(dados_cancer$diagnosis)

#Criando uma funçao que normaliza
normaliza <- function(x){return((x - min(x))/(max(x) - min(x)))}

#Normalizando os dados numéricos
dados_cancer_n <- as.data.frame(lapply(dados_cancer[2:31], normaliza))

#Criando datasets de treino e teste
#80% para treino e 20% para teste

dados_cancer_treino <- dados_cancer_n[1:469,]
dados_cancer_teste <- dados_cancer_n[470:568,]

#Separando rótulos
dados_cancer_treino_rotulos <- dados_cancer[1:469,1]$diagnosis
dados_cancer_teste_rotulos <- dados_cancer[470:568,1]$diagnosis

###############################################
#Aplicação do k~NN
###############################################

install.packages("class")
install.packages("caret")
library(class)
library(caret)

predicoes <- knn(train = dados_cancer_treino, test = dados_cancer_teste, cl = dados_cancer_treino_rotulos, k = 3)

confusionMatrix(dados_cancer_teste_rotulos, predicoes)

##########################################
#Escolhendo o k
##########################################

acuracia <- c() #serve para criar vetor

for(k in 1:30){

set.seed(1234)
  
predicoes <- knn(train = dados_cancer_treino, test = dados_cancer_teste, cl = dados_cancer_treino_rotulos, k = k)

matriz <- confusionMatrix(dados_cancer_teste_rotulos, predicoes)

acuracia <- c(acuracia, matriz[['overall']]['Accuracy'])
}

plot(1:30, 1-acuracia, type = 'l')

predicoes <- knn(train = dados_cancer_treino, test = dados_cancer_teste, cl = dados_cancer_treino_rotulos, k = 6)

confusionMatrix(dados_cancer_teste_rotulos, predicoes)




