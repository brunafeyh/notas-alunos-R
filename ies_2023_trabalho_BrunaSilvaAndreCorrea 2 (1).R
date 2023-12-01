# Nome Completo dos Alunos: Bruna Carolina da Silva Feyh e Andre Luis Correa Cano

## Parte 1 - 50% - R Básico

# Descrição do trabalho: fazer uma analise das notas de uma turma de 35 alunos de uma escola pública;

#Selecione uma amostra aleatoria de 15 alunos entre os 35 possĩveis
#Cada número gerado na amostra será considerado o identificador único de cada aluno 

alunos <- 1:35

alunos

amostra <- sample(alunos, 15)

amostra

#Calcule e imprima na tela (sem casas decimais) o % de representatividade dos 15 
#em relação ao total de alunos da turma

porcentagem <- (length(amostra) / length(alunos)) * 100

cat(sprintf("%.0f%%\n", porcentagem))

#As notas dadas respectivamente por aluno sorteado foram: 
#150, 152, 145, 157, 167, 172, 175, 170, 165, 177, 162, 180, 160, 155, 147
#Crie um vetor com as notas da primeira avaliação. 
#Nome do vetor: av1_notas

av1_notas<- c(150, 152, 145, 157, 167, 172, 175, 170, 165, 177, 162, 180, 160, 155, 147)

av1_notas

#Imprima na tela as notas da primeira avaliação em ordem crescente

sort(av1_notas)

#Calcule a soma e a média da avaliação 1
#Não usar a função mean()
#Nome da variavel: av1_media

av1_soma <- sum(av1_notas)
av1_soma

av1_media <- av1_soma / length(av1_notas)
av1_media

#Compare as notas de cada aluno com a média da avaliação
#Imprima na tela a nota do aluno e uma das duas mensagens seguintes:
#'nota do aluno <identificador do aluno>: <nota> - acima da média
#'nota do aluno <identificador do aluno>: <nota> - abaixo ou igual à média
# Obs: neste exercício utilize FOR ou WHILE

for (i in 1:length(av1_notas)) {
  if (av1_notas[i] > av1_media) {
    cat("Nota do aluno", amostra[i], ":", av1_notas[i], "- acima da média\n")
  } else {
    cat("Nota do aluno", amostra[i], ":", av1_notas[i], "- abaixo ou igual à média\n")
  }
}

#Divida as notas da avaliação 1 em 3 grupos: menores que 160, entre 160 e 170 e maiores que 170
#Imprima na tela os grupos de notas
#nome das variáveis: av1_notas_grupo1, av1_notas_grupo2, av1_notas_grupo3

av1_notas_grupo1 <- av1_notas[av1_notas < 160]
av1_notas_grupo2 <- av1_notas[(av1_notas >= 160) & (av1_notas <= 170)]
av1_notas_grupo3 <- av1_notas[av1_notas > 170]

cat("Notas do Grupo 1:", av1_notas_grupo1, "\n")
cat("Notas do Grupo 2:", av1_notas_grupo2, "\n")
cat("Notas do Grupo 3:", av1_notas_grupo3, "\n")

#Calcule a quantidade de notas em cada um dos grupos criados para a avaliação 1
#Nome das variáveis: av1_qtde_grupo1, av1_qtde_grupo2, av1_qtde_grupo3

av1_qtde_grupo1 <- length(av1_notas_grupo1)
av1_qtde_grupo2 <- length(av1_notas_grupo2)
av1_qtde_grupo3 <- length(av1_notas_grupo3)

av1_qtde_grupo1
av1_qtde_grupo2
av1_qtde_grupo3

#crie um vetor com as quantidades de notas de cada grupo da avaliação 1
#O nome do vetor deve ser: av1_qtde_notas_grupo


av1_qtde_notas_grupo <- c(av1_qtde_grupo1, av1_qtde_grupo2, av1_qtde_grupo3)

av1_qtde_notas_grupo

#Calcule o % das quantidades de notas de cada grupo em relação a quantidade total de notas da amostra
#Nome das variaveis: av1_percentual_grupo1, av1_percentual_grupo2 e av1_percentual_grupo3
#Obs: Arrendonde para excluir as casas decimais

av1_percentual_grupo1 <- round(av1_qtde_grupo1 / length(av1_notas) * 100)
av1_percentual_grupo2 <- round(av1_qtde_grupo2 / length(av1_notas) * 100)
av1_percentual_grupo3 <- round(av1_qtde_grupo3 / length(av1_notas) * 100)

av1_percentual_grupo1
av1_percentual_grupo2
av1_percentual_grupo3

#crie um vetor com o % das quantidades de notas de cada grupo da avaliação 1
#Nome da veriaveis: av1_percentual_notas_grupo

av1_percentual_notas_grupo <- c(av1_percentual_grupo1, av1_percentual_grupo2, av1_percentual_grupo3)
av1_percentual_notas_grupo

#crie uma matriz que mostre a quantidade de cada grupo e o % de cada grupo
#Nome da variavel: av1_matrix

# Criando uma matriz com a quantidade de cada grupo e o % de cada grupo
av1_matrix <- matrix(NA, nrow = 3, ncol = 2)
colnames(av1_matrix) <- c("Quantidade", "Percentual")

# Preenchendo a matriz com informações
av1_matrix[, "Quantidade"] <- c(av1_qtde_grupo1, av1_qtde_grupo2, av1_qtde_grupo3)
av1_matrix[, "Percentual"] <- round(av1_matrix[, "Quantidade"] / sum(av1_matrix[, "Quantidade"]) * 100)

# Visualizando a matriz
cat("Matriz av1_matrix:\n", av1_matrix, "\n")
av1_matrix

#Após um tempo, a pesquisa foi refeita com os mesmos alunos anteriormente sorteados, obedecendo rigorosamente a ordem de sorteio
#As novas notas da avaliação de cada aluno foram: 103 162 193 155 124 119 186 136 127 157 112 142 177 147 115
#Crie um vetor com as notas da avaliação 2
#Nome do vetor: av2_notas 

av2_notas <- c(103, 162, 193, 155, 124, 119, 186, 136, 127, 157, 112, 142, 177, 147, 115)
av2_notas

#Calcule a média e a mediana da avaliação 2
#Nome da variavel: av2_media e av2_mediana

av2_media <- sum(av2_notas) / length(av2_notas)
av2_mediana <- median(av2_notas)
av2_media
av2_mediana

#Compare as duas médias das duas avaliacoes e imprima qual delas é a maior
#Utilize a estrutura de IF

# Comparando as médias das avaliações
if (av2_media > av1_media) {
  cat("A média da Avaliação 2 é maior do que a média da Avaliação 1.\n")
} else {
  cat("A média da Avaliação 1 é maior do que a média da Avaliação 2.\n")
}

#A expectativa é que a média geral da avaliação 2 fosse 10% superior à média da avaliação 1.
#Calcule quanto seria essa média esperada
#Nome da variavel: av2_media_esperada

av2_media_esperada <- av1_media * 1.1
av2_media_esperada

#Calcule a diferença entre a média obtida e a esperada da avaliação 2
#Nome da variavel: av2_media_diff

av2_media_diff <- av2_media - av2_media_esperada
cat("Diferença entre a média obtida e a esperada da Avaliação 2:", av2_media_diff, "\n")

#Para cada aluno, calcule a média de suas notas e a diferença entre as notas 
#da primeira avaliação e da segunda avaliação 
#Armazene a diferenca em um vetor chamado av12_diferenca

av12_diferenca <- av1_notas - av2_notas
av12_diferenca

#crie uma matriz contendo: 
#o número de cada aluno sorteado, sua primeira nota, sua segunda nota, a amplitude entre as notas e a nota média de suas notas
#nome da variavel: tabela_aluno

tabela_aluno <- cbind(amostra, av1_notas, av2_notas, abs(av1_notas - av2_notas), rowMeans(cbind(av1_notas, av2_notas)))
colnames(tabela_aluno) <- c("Número do Aluno", "Nota Avaliação 1", "Nota Avaliação 2", "Diferença", "Média")

#salvar a tabela_aluno em um arquivo csv
#nome do arquivo: tabela_aluno.csv
write.csv(tabela_aluno, "tabela_aluno.csv", row.names = FALSE)

#Crie uma função chamada 'calcular_medidas' que receba somente um vetor com as notas de uma avaliacao (1 ou 2)
#A função retornar: maior nota, menor nota, média (1 casa decimal) e amplitude das notas 
#Obs: Pode usar a função mean() para calcular a média das notas do vetor
calcular_medidas <- function(notas) {
  maior <- max(notas)
  menor <- min(notas)
  media <- sum(notas) / length(notas)
  amplitude <- maior - menor
  return(c(maior, menor, round(media, 1), amplitude))
}
av1_notas
calcular_medidas(av1_notas)

#crie um vetor de dois elementos, sendo o número 1 representando a primeira avaliação 1 e 
#o número 2, representado a segunda avaliação
#nome da variavel: avaliacao

avaliacao <- c(1, 2)
avaliacao

#crie uma matriz contendo: 
#o número da avaliação (nome da variavel: avaliacao);
#a maior nota de cada avaliacao (nome da variavel: av12_maior);
#a menor nota de cada avaliacao (nome da variavel: av12_menor);
#a amplitude (diferenca) entre a maior e menor nota de cada avaliacao (nome da variavel: av12_amplitude);
#a media de cada avaliacao (nome da variavel: av12_media);
#nome da matriz: tabela_avaliacao

#salvar a tabela em um arquivo excel
#nome do arquivo: tabela_avaliacao.xlsx

av12_maior <- c(max(av1_notas), max(av2_notas))
av12_maior
av12_menor <- c(min(av1_notas), min(av2_notas))
av12_menor
av12_amplitude <- av12_maior - av12_menor
av12_amplitude
av12_media <- c(mean(av1_notas), mean(av2_notas))
av12_media

# Criando a matriz tabela_avaliacao
tabela_avaliacao <- cbind(avaliacao, av12_maior, av12_menor, av12_amplitude, av12_media)
tabela_avaliacao
colnames(tabela_avaliacao) <- c("Avaliação", "Maior Nota")


# Visualizando a matriz
tabela_avaliacao

# Salvar a matriz em um arquivo Excel
write.table(tabela_avaliacao, "tabela_avaliacao.xlsx")

## Parte 2 - 50% - R com Estatística

#Utilizando o dataset TREES disponível no R, responda as questões abaixo:
#selecione duas variáveis, elabore o gráfico de dispersão e explique o gráfico;

# Utilizando o dataset TREES disponível no R
data(trees)

# Selecionando duas variáveis e elaborando um gráfico de dispersão
# Vamos escolher "Girth" e "Height"
plot(trees$Girth, trees$Height, main = "Gráfico de Dispersão - Girth vs. Height", 
     xlab = "Girth", ylab = "Height", pch = 16, col = "blue")

print("O gráfico de dispersão gerado com as variáveis Girth e Height do dataset TREES mostra uma relação positiva entre as duas variáveis. Ou seja, árvores com maior girth tendem a ter maior altura.
A relação é linear, mas não é perfeita. Há alguns pontos que se afastam da linha de tendência. Isso pode ser devido a fatores como:
Espécie da árvore. Algumas espécies de árvores são naturalmente mais altas ou mais baixas do que outras.
Condições ambientais. Árvores que crescem em condições ambientais mais favoráveis, como solo fértil e clima adequado, tendem a ser mais altas do que árvores que crescem em condições ambientais menos favoráveis.
A relação entre Girth e Height é importante para a compreensão do crescimento e da saúde das árvores")

#utilizando o dataset IRIS, disponivel no R
#selecione uma variável qualitativa nominal, elabore a tabela de frequencia absoluta e relativa;
#selecione uma variável quantitativa contínua, elabore a tabela de frequencia absoluta e relativa;

# Utilizando o dataset IRIS disponível no R
data(iris)

# Selecionando uma variável qualitativa nominal (Species) e elaborando a tabela de frequência
tabela_freq_qualitativa <- table(iris$Species)
cat("Tabela de Frequência Absoluta:\n", tabela_freq_qualitativa, "\n")

# Calculando a tabela de frequência relativa
tabela_freq_relativa <- prop.table(tabela_freq_qualitativa) * 100
cat("Tabela de Frequência Relativa (%):\n", tabela_freq_relativa, "\n")

# Selecionando uma variável quantitativa contínua (Sepal.Length) e elaborando a tabela de frequência
tabela_freq_quantitativa <- table(iris$Sepal.Length)
cat("Tabela de Frequência Absoluta (Variável Quantitativa):\n", tabela_freq_quantitativa, "\n")

# Calculando a tabela de frequência relativa para a variável quantitativa
tabela_freq_relativa_quantitativa <- prop.table(tabela_freq_quantitativa) * 100
cat("Tabela de Frequência Relativa (% - Variável Quantitativa):\n", tabela_freq_relativa_quantitativa, "\n")

#utilizando o dataset TITANIC disponivel no R, responda às questões abaixo:
#selecione uma variavel qualitativa ordinal;
#elabore um gráfico de barras

# Carregar o dataset TITANIC
data(Titanic)

# Criar uma tabela de frequências de sobreviventes por classe
tab <- xtabs(Freq ~ Class + Survived, data = Titanic)

# Selecionar apenas os sobreviventes
tab <- tab[, "Yes"]

# Criar um gráfico de barras
barplot(tab, main = "Sobreviventes por classe no Titanic", xlab = "Classe", ylab = "Número de sobreviventes")
