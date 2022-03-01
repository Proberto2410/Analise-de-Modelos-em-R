################## REGRESSÃO LINEAR MÚLTIPLA ####################

# Objetivo é fazer uma análise, em função do meu APRENDIZADO, usando as 
# ferramentas RapidMiner para ETL e do R para Modelagem da Regressão Linear. 
# Reconhecendo de antemão que ainda tenho muito de aprender neste aspecto, parti-
# cularmente em explorar a potencialidade gráfica do R.

# Espero que à este script eu possa, no futuro, acrescentar outros como resultado 
# desta etapa de aprendizagem.

# Para este exercício foi utilizado o dataset imports_85. 

# Imports_85 é uma base de dados com 205 casos (linhas) e 26 variáveis (colunas).
# Este conjunto de dados consiste em três tipos de entidades: 
  
# (a) a especificação de um automóvel em termos de várias características, 
# (b) sua classificação de risco de seguro atribuída, 
# (c) suas perdas normalizadas em uso em comparação com outros carros. 

# A segunda classificação corresponde ao grau em que o automóvel é mais arriscado
# do que seu preço indica. Os carros recebem inicialmente um símbolo de fator de 
# risco associado ao seu preço. 

# Então, se for mais arriscado (ou menos), este símbolo é ajustado movendo-o para
# cima (ou para baixo) na escala. Os atuários chamam esse processo de "simbolização".
# Um valor de +3 indica que o automóvel é arriscado, -3 que provavelmente é 
# bastante seguro.

# O terceiro fator é o pagamento de perda média relativa por ano de veículo segurado.
# Este valor é normalizado para todos os automóveis dentro de uma determinada 
# classificação de tamanho (pequeno de duas portas, peruas, esportes / especialidades,
# etc) e representa a perda média por carro por ano.


# Para esse exercício foi feito ETL via o RapidMiner e reduzimos o dataset
# para um subset com somente 5 (cinco) variáveis, sendo Preco a variável
# Dependente. 

# Temos entre as 5 variaveis, o Tamanho do Motor e a Economia de Combus-
# tível em perímetro urbano. Sabe-se que o preço de veículo pode ser 
# influenciado pelo tamanho do motor e seu consumo de combustível. O tamanho do 
# motor também pode ser referido como "cilindrada do motor" ou "cilindrada do 
# motor" e é a medição do volume total dos cilindros do motor. 

# Quanto maior o tamanho do motor, mais espaço haverá para ar e combustível
# dentro dele. Tradicionalmente, o tamanho de um motor ditava quanta potência 
# ele produziria e, embora ainda seja geralmente o caso hoje, a introdução de 
# motores turboalimentados modernos nos últimos anos significou que motores 
# menores são muito mais potentes do que costumavam ser.

# Convém mencionar que esse script tenta explorar a existencia de relação entre
# as variáveis, apenas como uma correlação, sem qualquer interpretação de causa
# e efeito, e que podemos verificar se um modelo funciona bem para dados de 
# muitas maneiras diferentes.

# Nessa etapa de aprendizagem, estaremos utilizando os resíduos para nossa 
# análise uma vez que os resíduos são sobras da variável de resultado após 
# ajustar um modelo e podem revelar padrões inexplicáveis nos dados pelo modelo
# ajustado.
#
# Dessa forma iremos criar dois modelos de análise em nosso exercício. No 
# Modelo1 usaremos as variáveis Risco e Perdas_Normalizadas como indepen-
# dentes e no Modelo2 usaremos as variáveis Tamanho_Motor e Economia_Cidade
# e, em ambos Modelos, teremos a variável Preco como dependente.

### I) VERIFICAR OS PRESSUPOSTOS DA REGRESSÃO LINEAR ###
# a) análise gráfica
# b) normalidade
# c) existência de outliers severos
# d) independência dos resíduos
# e) homocedasticidade
# f) ausência de multicolinearidade


### II) CRIAR UM MODELO DE REGRESSÃO LINEAR MÚLTIPLA
### III) COMPARAR DIFERENTES MODELOS DE REGRESSÃO
### IV) DESCREVER OS RESULTADOS

## passo 1: INSTALANDO E CARREGANDO PACOTES/LIBRARIES:
if(!require(pacman)) install.packages('pacman')
library(pacman)

pacman::p_load(tidyverse, dplyr, car, readr, rstatix, lmtest, ggpubr,
               QuantPsyc, psych, scatterplot3d)


## passo 2: LENDO A BASE DE DADOS:
# Session --> Set Working Directory --> Choose Directory
# File --> Import Dataset --> From Text(readr)...

#carprice <- read_csv("Imports_85subset.csv")
#View(carprice)

carprice <- read_csv("Imports_85subset.csv", 
                             col_types = cols(Risco = col_double()))
View(carprice)
glimpse(carprice)

# contagem de observações omissas
sum(is.na(carprice))

# O uso da função 'glimpse' mostra que há valores omissos na Varíavel
# Perdas_Normalizadas e a função 'sum' identifica o total de 41 obser-
# vações nulas. 

# Optamos por substituir essas observações omissas pela Mediana
carprice$Perdas_Normalizadas <- carprice$Perdas_Normalizadas %>% replace_na(
  median(carprice$Perdas_Normalizadas, na.rm = T))

# verificação do resultado
sapply(carprice, function(x) sum(is.na(x)))

## passo 3: VERIFICAR OS PRESSUPOSTOS PARA A REGRESSÃO LINEAR ##

# construção do modelo1 com a função lm
mod1 <- lm(Preco ~ Risco + Perdas_Normalizadas, carprice)

# 3.a) análise gráfica dos pressupostos
par(mfrow=c(2,2)) # para visualizar quatro diferentes tipos de gráficos

plot(mod1)

# Output de quatro gráficos

## 1º - Residuals vs Fitted ==> a visualização deste gráfico vai nos mostrar
# se os resíduos tem padrões não lineares.

# Se você encontrar resíduos igualmente espalhados ao redor de uma linha
# horizontal sem padrões distintos, isso é uma boa indicação de que você não 
# tem relações não lineares.

# uma linha vermelha bem próxima à linha pontilhada mostra que há linearidade


## 2º - Normal Q-Q ==> o gráfico mostra se os resíduos tem distribuição normal.
# devemos verificar se os resíduos estão alinhados na linha pontilhada.

# claramente se observa que os resíduos não tem distribuição normal.

## 3º - Scale-Location ==> este gráfica vai nos mostrar se os resíduos estão 
# distribuídos igualmente ao longo do invertalos da varíavel independente. 

# a intençao aqui é verificar a suposição de uma homogeneidade na variancia dos
# resíduos (homocedasticidade), através da visualização de uma linha com
# pontos igualmente espalhados.

## 4º - Residuals vc Leverage ==> busca-se identificar pontos com capacidade de
# influenciar a linha de regressão.

# Nem todos os outliers são influentes na análise de regressão linear. Mesmo que
# os dados tenham valores extremos, eles podem não ser influentes para determinar
# uma linha de regressão.

# O resultado da regressão pode ser alterado se esses pontos forem excluídos.

par(mfrow=c(1,1))


# 3.b) Teste de Normalidade dos Resíduos
shapiro.test(mod1$residuals)
# H0: os resíduos tem distribuição normal
# H1: os resíduos não tem distribuição normal
# Alfa igual a 5%

# com o p-value igual a 7.798e-16, menor que Alfa, podemos rejeitar
# H0 pois há evidencia estatistica de que os resíduos não tem distri-
# buiçao normal, como visualizado no QQ-Plot.

# 3.c) Verificação de existência de outliers nos resíduos
# (o outuput será umm resumo dos resíduos padronizados)
summary(rstandard(mod1))
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -1.074689 -0.686504 -0.310329  0.000634  0.349516  4.193123

# nota-se que há outliers com valores acima de 3

# 3.d) Independência dos Resíduos(Durbin-Watson)
durbinWatsonTest(mod1)
# a estatística do teste durbinwatson tem que estar próxima de 2 para afirmarmos
# que há independência dos dados (não há autocorrelação).

# Se há autocorrelação, o erro padrão será subestimado e podemos concluir indevida-
# mente que as variáveis independentes são significativas quando na realidade
# não são.

# H0: não existe autocorrelação
# H1: existe autocorrelação

# lag Autocorrelation D-W Statistic p-value
#   1        0.755193     0.4829435       0
# Alternative hypothesis: rho != 0

# com o Dw igual a 0.4829 podemos rejeitar H0 pois há evidencia estatística de
# que existe autocorrelação.


# 3.e) Homocedasticidade (Breusch-Pagan) ==> homogeneidade das variancias
# OBS: só funciona adequadamente quando há distribuição normal e independência
bptest(mod1)
# H0: há homocedasticidade nos resíduos
# H1: não há homocedasticidade nos resíduos
# Alfa igual a 5%

# BP = 0.24545, df = 2, p-value = 0.8845 maior que Alfa, não rejeitar H0, pois 
# há evidencia estatística de que há homocedasticidade.

# 3.f) Ausência de multicolinearidade ==> alta correlação entre as variáveis
# independentes.

# há multicolinearidade quando o coeficiente de correlação de Pearson for maior
# que 0.90 ou 0.80

pairs.panels(carprice)

# No plot a diagonal superior contém os coeficientes de correlação, lembrando
# que nosso interesse está em averiguar correlação entre as variáveis indepen-
# dentes.


# Ainda outra maneira de medir a multicolinearidade é o fator de inflação da 
# variância (VIF). Se o VIF for igual a 1 não há multicolinearidade entre os 
# fatores, mas se o VIF for maior que 1, as preditoras podem estar 
# moderadamente correlacionadas.
vif(mod1)

#   Risco     Perdas_Normalizadas 
#   1.264688        1.264688 



### passo 4: CRIAÇÃO E COMPARAÇÃO DE UM OUTRO MODELO PARA COMPARAÇÃO ###

# 4.a) construção do modelo2 com a função lm
mod2 <- lm(Preco ~ Economia_Cidade + Tamanho_Motor, carprice)


### 4.b) COMPARARANDO DIFERENTES MODELOS DE REGRESSÃO
# nesta etapa iremos verificar os coeficientes, R² e demais informações.

summary(mod1)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          9063.94    2236.87   4.052 7.24e-05 
#  Risco               -1001.77     492.73  -2.033   0.0433  
#  Perdas_Normalizadas    41.28      19.29   2.140   0.0336 

#   Adjusted R-squared:  0.01916

# O intercepto mostra o 'impacto' no valor da Variável Dependente(Preco) quando
# o valor das variáveis independentes são zero

# Analisando o coeficiente da variável independente Risco temos o seu valor igual
# a -1001.77 e o p-value 0.0433, menor que 0.05 com o t-value testando as se-
# guintes hipoteses:

# H0: coeficiente é igual a zero
# H1: coeficiente não é igual a zero
# Alfa igual a 5%

# com o p-value igual a 0.0433, podemos rejeitar H0 pois há evidencia estatísti-
# ca de que o coeficiente não é igual a zero.


summary(mod2)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      909.249   2261.462   0.402    0.688    
# Economia_Cidade -218.740     54.604  -4.006 8.68e-05 
# Tamanho_Motor    140.373      8.578  16.364  < 2e-16 

# Adjusted R-squared:  0.7592

# Ao compararmos os valores dos outputs, notamos que há diferenças significativas
# nos coeficientes e particularmente no R².

# O R² ajustado explica a variação dos dados ==> ou seja: o quanto da variância
# da variável dependente pode ser explicada pela variável independente.

# Um alto valor de R² ajustado mostra o quanto a variável independente pode 
# explicar a variável dependente.

# o output do summary dos dois modelos mostra que o R² ajustado do mod2 é bem 
# maior que o R² ajustado do mod1, mostrando assim que há evidencia estatística
# de que o modelo2 explica melhor os dados do que o modelo1.


## Obtenção dos coeficientes padronizados
lm.beta(mod1)
#     Risco       Perdas_Normalizadas 
# -0.1585401           0.1668664
# O coeficiente padronizado de ambas as variáveis independentes do mod1
# tem um grau de associação numéricamente próximos porém em direções opostas

lm.beta(mod2)
#   Economia_Cidade   Tamanho_Motor 
#       -0.1818620       0.7428767
# O coeficiente padronizado da variável independente Tamanho_Motor do mod2
# está mais associado ao valor da Variável dependente Preco.

## Comparação de Modelos ##
# O R² é uma das formas de comparar modelos mas não é a melhor delas. Vamos
# utilizar o AIC e o BIC para esta finalidade.

# O Critério de Informação Akaike, ou AIC, é um método para pontuar e selecionar
# um modelo.

# O AIC estima a quantidade relativa de informação perdida por um determinado
# modelo: quanto menos informações um modelo perde, maior a qualidade desse
# modelo e menor a pontuação AIC.

AIC(mod1, mod2)
#       df      AIC
# mod1  4     4260.746
# mod2  4     3972.837

# O Critério de Informação Bayesiano, ou BIC, é um método para pontuar e 
#selecionar um modelo.

# Tem esse nome baseado no campo de estudo do qual foi derivado: probabilidade
# Bayesiana e inferência. 

BIC(mod1, mod2)
#       df      BIC
# mod1  4   4274.039
# mod2  4   3986.129

# Verifica-se que o modelo2 é melhor classificado tanto no critério AIC quanto
# no critério BIC.

#### Passo 5: GRÁFICO DE DISPERSÃO ####

grafico3D <- scatterplot3d(carprice$Preco/1000 ~ carprice$Tamanho_Motor + 
                             carprice$Economia_Cidade,
                       pch = 16, angle = 45, color = 'red', box = F,
                       ylab = 'Economia Cidade', xlab = 'Tamanho do Motor', 
                       zlab = 'Preço(mil)')


####################### CONCLUSÃO #################################

# A Regressão linear múltipla mostrou que o Tamanho do Motor tem efeitos
# sobre o Preço do veículo.

######################  #########  ################################




## Referencias Consultadas ###

# https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data.
# Informações da Fonte: Criador/Doador: Jeffrey C. Schlimmer (Jeffrey.Schlimmer@a.gp.cs.cmu.edu)

# Introdução aos modelos de regressão linear-Flávia Chein, Coleção
# Metodologias de Pesquisa-ENAP

# Agradecimento especial a Fernanda Peres (https://fernandafperes.com.br/) por 
# partilhar seus conhecimentos estatísticos.

# Sobre gráficos para a análise dos pressupostos de regressão linear
# https://data.library.virginia.edu/diagnostic-plots/

# https://machinelearningmastery.com/probabilistic-model-selection-measures/


