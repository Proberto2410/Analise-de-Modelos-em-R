################## REGRESS�O LINEAR M�LTIPLA ####################

# Objetivo � fazer uma an�lise, em fun��o do meu APRENDIZADO, usando as 
# ferramentas RapidMiner para ETL e do R para Modelagem da Regress�o Linear. 
# Reconhecendo de antem�o que ainda tenho muito de aprender neste aspecto, parti-
# cularmente em explorar a potencialidade gr�fica do R.

# Espero que � este script eu possa, no futuro, acrescentar outros como resultado 
# desta etapa de aprendizagem.

# Para este exerc�cio foi utilizado o dataset imports_85. 

# Imports_85 � uma base de dados com 205 casos (linhas) e 26 vari�veis (colunas).
# Este conjunto de dados consiste em tr�s tipos de entidades: 
  
# (a) a especifica��o de um autom�vel em termos de v�rias caracter�sticas, 
# (b) sua classifica��o de risco de seguro atribu�da, 
# (c) suas perdas normalizadas em uso em compara��o com outros carros. 

# A segunda classifica��o corresponde ao grau em que o autom�vel � mais arriscado
# do que seu pre�o indica. Os carros recebem inicialmente um s�mbolo de fator de 
# risco associado ao seu pre�o. 

# Ent�o, se for mais arriscado (ou menos), este s�mbolo � ajustado movendo-o para
# cima (ou para baixo) na escala. Os atu�rios chamam esse processo de "simboliza��o".
# Um valor de +3 indica que o autom�vel � arriscado, -3 que provavelmente � 
# bastante seguro.

# O terceiro fator � o pagamento de perda m�dia relativa por ano de ve�culo segurado.
# Este valor � normalizado para todos os autom�veis dentro de uma determinada 
# classifica��o de tamanho (pequeno de duas portas, peruas, esportes / especialidades,
# etc) e representa a perda m�dia por carro por ano.


# Para esse exerc�cio foi feito ETL via o RapidMiner e reduzimos o dataset
# para um subset com somente 5 (cinco) vari�veis, sendo Preco a vari�vel
# Dependente. 

# Temos entre as 5 variaveis, o Tamanho do Motor e a Economia de Combus-
# t�vel em per�metro urbano. Sabe-se que o pre�o de ve�culo pode ser 
# influenciado pelo tamanho do motor e seu consumo de combust�vel. O tamanho do 
# motor tamb�m pode ser referido como "cilindrada do motor" ou "cilindrada do 
# motor" e � a medi��o do volume total dos cilindros do motor. 

# Quanto maior o tamanho do motor, mais espa�o haver� para ar e combust�vel
# dentro dele. Tradicionalmente, o tamanho de um motor ditava quanta pot�ncia 
# ele produziria e, embora ainda seja geralmente o caso hoje, a introdu��o de 
# motores turboalimentados modernos nos �ltimos anos significou que motores 
# menores s�o muito mais potentes do que costumavam ser.

# Conv�m mencionar que esse script tenta explorar a existencia de rela��o entre
# as vari�veis, apenas como uma correla��o, sem qualquer interpreta��o de causa
# e efeito, e que podemos verificar se um modelo funciona bem para dados de 
# muitas maneiras diferentes.

# Nessa etapa de aprendizagem, estaremos utilizando os res�duos para nossa 
# an�lise uma vez que os res�duos s�o sobras da vari�vel de resultado ap�s 
# ajustar um modelo e podem revelar padr�es inexplic�veis nos dados pelo modelo
# ajustado.
#
# Dessa forma iremos criar dois modelos de an�lise em nosso exerc�cio. No 
# Modelo1 usaremos as vari�veis Risco e Perdas_Normalizadas como indepen-
# dentes e no Modelo2 usaremos as vari�veis Tamanho_Motor e Economia_Cidade
# e, em ambos Modelos, teremos a vari�vel Preco como dependente.

### I) VERIFICAR OS PRESSUPOSTOS DA REGRESS�O LINEAR ###
# a) an�lise gr�fica
# b) normalidade
# c) exist�ncia de outliers severos
# d) independ�ncia dos res�duos
# e) homocedasticidade
# f) aus�ncia de multicolinearidade


### II) CRIAR UM MODELO DE REGRESS�O LINEAR M�LTIPLA
### III) COMPARAR DIFERENTES MODELOS DE REGRESS�O
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

# contagem de observa��es omissas
sum(is.na(carprice))

# O uso da fun��o 'glimpse' mostra que h� valores omissos na Var�avel
# Perdas_Normalizadas e a fun��o 'sum' identifica o total de 41 obser-
# va��es nulas. 

# Optamos por substituir essas observa��es omissas pela Mediana
carprice$Perdas_Normalizadas <- carprice$Perdas_Normalizadas %>% replace_na(
  median(carprice$Perdas_Normalizadas, na.rm = T))

# verifica��o do resultado
sapply(carprice, function(x) sum(is.na(x)))

## passo 3: VERIFICAR OS PRESSUPOSTOS PARA A REGRESS�O LINEAR ##

# constru��o do modelo1 com a fun��o lm
mod1 <- lm(Preco ~ Risco + Perdas_Normalizadas, carprice)

# 3.a) an�lise gr�fica dos pressupostos
par(mfrow=c(2,2)) # para visualizar quatro diferentes tipos de gr�ficos

plot(mod1)

# Output de quatro gr�ficos

## 1� - Residuals vs Fitted ==> a visualiza��o deste gr�fico vai nos mostrar
# se os res�duos tem padr�es n�o lineares.

# Se voc� encontrar res�duos igualmente espalhados ao redor de uma linha
# horizontal sem padr�es distintos, isso � uma boa indica��o de que voc� n�o 
# tem rela��es n�o lineares.

# uma linha vermelha bem pr�xima � linha pontilhada mostra que h� linearidade


## 2� - Normal Q-Q ==> o gr�fico mostra se os res�duos tem distribui��o normal.
# devemos verificar se os res�duos est�o alinhados na linha pontilhada.

# claramente se observa que os res�duos n�o tem distribui��o normal.

## 3� - Scale-Location ==> este gr�fica vai nos mostrar se os res�duos est�o 
# distribu�dos igualmente ao longo do invertalos da var�avel independente. 

# a inten�ao aqui � verificar a suposi��o de uma homogeneidade na variancia dos
# res�duos (homocedasticidade), atrav�s da visualiza��o de uma linha com
# pontos igualmente espalhados.

## 4� - Residuals vc Leverage ==> busca-se identificar pontos com capacidade de
# influenciar a linha de regress�o.

# Nem todos os outliers s�o influentes na an�lise de regress�o linear. Mesmo que
# os dados tenham valores extremos, eles podem n�o ser influentes para determinar
# uma linha de regress�o.

# O resultado da regress�o pode ser alterado se esses pontos forem exclu�dos.

par(mfrow=c(1,1))


# 3.b) Teste de Normalidade dos Res�duos
shapiro.test(mod1$residuals)
# H0: os res�duos tem distribui��o normal
# H1: os res�duos n�o tem distribui��o normal
# Alfa igual a 5%

# com o p-value igual a 7.798e-16, menor que Alfa, podemos rejeitar
# H0 pois h� evidencia estatistica de que os res�duos n�o tem distri-
# bui�ao normal, como visualizado no QQ-Plot.

# 3.c) Verifica��o de exist�ncia de outliers nos res�duos
# (o outuput ser� umm resumo dos res�duos padronizados)
summary(rstandard(mod1))
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -1.074689 -0.686504 -0.310329  0.000634  0.349516  4.193123

# nota-se que h� outliers com valores acima de 3

# 3.d) Independ�ncia dos Res�duos(Durbin-Watson)
durbinWatsonTest(mod1)
# a estat�stica do teste durbinwatson tem que estar pr�xima de 2 para afirmarmos
# que h� independ�ncia dos dados (n�o h� autocorrela��o).

# Se h� autocorrela��o, o erro padr�o ser� subestimado e podemos concluir indevida-
# mente que as vari�veis independentes s�o significativas quando na realidade
# n�o s�o.

# H0: n�o existe autocorrela��o
# H1: existe autocorrela��o

# lag Autocorrelation D-W Statistic p-value
#   1        0.755193     0.4829435       0
# Alternative hypothesis: rho != 0

# com o Dw igual a 0.4829 podemos rejeitar H0 pois h� evidencia estat�stica de
# que existe autocorrela��o.


# 3.e) Homocedasticidade (Breusch-Pagan) ==> homogeneidade das variancias
# OBS: s� funciona adequadamente quando h� distribui��o normal e independ�ncia
bptest(mod1)
# H0: h� homocedasticidade nos res�duos
# H1: n�o h� homocedasticidade nos res�duos
# Alfa igual a 5%

# BP = 0.24545, df = 2, p-value = 0.8845 maior que Alfa, n�o rejeitar H0, pois 
# h� evidencia estat�stica de que h� homocedasticidade.

# 3.f) Aus�ncia de multicolinearidade ==> alta correla��o entre as vari�veis
# independentes.

# h� multicolinearidade quando o coeficiente de correla��o de Pearson for maior
# que 0.90 ou 0.80

pairs.panels(carprice)

# No plot a diagonal superior cont�m os coeficientes de correla��o, lembrando
# que nosso interesse est� em averiguar correla��o entre as vari�veis indepen-
# dentes.


# Ainda outra maneira de medir a multicolinearidade � o fator de infla��o da 
# vari�ncia (VIF). Se o VIF for igual a 1 n�o h� multicolinearidade entre os 
# fatores, mas se o VIF for maior que 1, as preditoras podem estar 
# moderadamente correlacionadas.
vif(mod1)

#   Risco     Perdas_Normalizadas 
#   1.264688        1.264688 



### passo 4: CRIA��O E COMPARA��O DE UM OUTRO MODELO PARA COMPARA��O ###

# 4.a) constru��o do modelo2 com a fun��o lm
mod2 <- lm(Preco ~ Economia_Cidade + Tamanho_Motor, carprice)


### 4.b) COMPARARANDO DIFERENTES MODELOS DE REGRESS�O
# nesta etapa iremos verificar os coeficientes, R� e demais informa��es.

summary(mod1)
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          9063.94    2236.87   4.052 7.24e-05 
#  Risco               -1001.77     492.73  -2.033   0.0433  
#  Perdas_Normalizadas    41.28      19.29   2.140   0.0336 

#   Adjusted R-squared:  0.01916

# O intercepto mostra o 'impacto' no valor da Vari�vel Dependente(Preco) quando
# o valor das vari�veis independentes s�o zero

# Analisando o coeficiente da vari�vel independente Risco temos o seu valor igual
# a -1001.77 e o p-value 0.0433, menor que 0.05 com o t-value testando as se-
# guintes hipoteses:

# H0: coeficiente � igual a zero
# H1: coeficiente n�o � igual a zero
# Alfa igual a 5%

# com o p-value igual a 0.0433, podemos rejeitar H0 pois h� evidencia estat�sti-
# ca de que o coeficiente n�o � igual a zero.


summary(mod2)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      909.249   2261.462   0.402    0.688    
# Economia_Cidade -218.740     54.604  -4.006 8.68e-05 
# Tamanho_Motor    140.373      8.578  16.364  < 2e-16 

# Adjusted R-squared:  0.7592

# Ao compararmos os valores dos outputs, notamos que h� diferen�as significativas
# nos coeficientes e particularmente no R�.

# O R� ajustado explica a varia��o dos dados ==> ou seja: o quanto da vari�ncia
# da vari�vel dependente pode ser explicada pela vari�vel independente.

# Um alto valor de R� ajustado mostra o quanto a vari�vel independente pode 
# explicar a vari�vel dependente.

# o output do summary dos dois modelos mostra que o R� ajustado do mod2 � bem 
# maior que o R� ajustado do mod1, mostrando assim que h� evidencia estat�stica
# de que o modelo2 explica melhor os dados do que o modelo1.


## Obten��o dos coeficientes padronizados
lm.beta(mod1)
#     Risco       Perdas_Normalizadas 
# -0.1585401           0.1668664
# O coeficiente padronizado de ambas as vari�veis independentes do mod1
# tem um grau de associa��o num�ricamente pr�ximos por�m em dire��es opostas

lm.beta(mod2)
#   Economia_Cidade   Tamanho_Motor 
#       -0.1818620       0.7428767
# O coeficiente padronizado da vari�vel independente Tamanho_Motor do mod2
# est� mais associado ao valor da Vari�vel dependente Preco.

## Compara��o de Modelos ##
# O R� � uma das formas de comparar modelos mas n�o � a melhor delas. Vamos
# utilizar o AIC e o BIC para esta finalidade.

# O Crit�rio de Informa��o Akaike, ou AIC, � um m�todo para pontuar e selecionar
# um modelo.

# O AIC estima a quantidade relativa de informa��o perdida por um determinado
# modelo: quanto menos informa��es um modelo perde, maior a qualidade desse
# modelo e menor a pontua��o AIC.

AIC(mod1, mod2)
#       df      AIC
# mod1  4     4260.746
# mod2  4     3972.837

# O Crit�rio de Informa��o Bayesiano, ou BIC, � um m�todo para pontuar e 
#selecionar um modelo.

# Tem esse nome baseado no campo de estudo do qual foi derivado: probabilidade
# Bayesiana e infer�ncia. 

BIC(mod1, mod2)
#       df      BIC
# mod1  4   4274.039
# mod2  4   3986.129

# Verifica-se que o modelo2 � melhor classificado tanto no crit�rio AIC quanto
# no crit�rio BIC.

#### Passo 5: GR�FICO DE DISPERS�O ####

grafico3D <- scatterplot3d(carprice$Preco/1000 ~ carprice$Tamanho_Motor + 
                             carprice$Economia_Cidade,
                       pch = 16, angle = 45, color = 'red', box = F,
                       ylab = 'Economia Cidade', xlab = 'Tamanho do Motor', 
                       zlab = 'Pre�o(mil)')


####################### CONCLUS�O #################################

# A Regress�o linear m�ltipla mostrou que o Tamanho do Motor tem efeitos
# sobre o Pre�o do ve�culo.

######################  #########  ################################




## Referencias Consultadas ###

# https://archive.ics.uci.edu/ml/machine-learning-databases/autos/imports-85.data.
# Informa��es da Fonte: Criador/Doador: Jeffrey C. Schlimmer (Jeffrey.Schlimmer@a.gp.cs.cmu.edu)

# Introdu��o aos modelos de regress�o linear-Fl�via Chein, Cole��o
# Metodologias de Pesquisa-ENAP

# Agradecimento especial a Fernanda Peres (https://fernandafperes.com.br/) por 
# partilhar seus conhecimentos estat�sticos.

# Sobre gr�ficos para a an�lise dos pressupostos de regress�o linear
# https://data.library.virginia.edu/diagnostic-plots/

# https://machinelearningmastery.com/probabilistic-model-selection-measures/


