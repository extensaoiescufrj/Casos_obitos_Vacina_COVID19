### CASOS E ÓBITOS DE SRAG POR COVID19 no Estado do Rio de Janeiro
### COBERTURA VACINAL CONTRA COVID-19 no Estado do Rio de Janeiro
# UNIVERSIDADE FEDERAL DO RIO DE JANEIRO
# INSTITUTO DE ESTUDOS EM SAÚDE COLETIVA
# PROJ. EXTENSÃO - Apoio às ações de vigilância epidemiológica no enfrentamento da epidemia de COVID-19
### BANCO DE SRAG CONSOLIDADO ATÉ 30/09/2021 - SIVEP GRIPE
### BANCO DE VACINAÇÃO CONSOLIDADO ATÉ 08/02/2022 - Extraído do TABNET do ERJ
### Alunos: Édnei César, Mariana Costa e Pedro Mattos
### Orientadores: Natália Paiva, Ana Paula e Antonio José

library(dplyr)
library(ggplot2)
library(readxl)
library(ggplot2)
library(stringr)
library(reshape2)
library(openxlsx)
library(lubridate)
library(gridExtra)
library(epiDisplay)
library(zoo)
library(tidyverse)

# BASES ORIGINAIS SIVEP
 
corona_2020 <- read.csv("INFLUD-20-09-2021_2020.csv", sep = ";", dec = ",")
corona_2021 <- read.csv("INFLUD21-20-09-2021.csv", sep = ";", dec = ",")

# FILTRANDO A BASE APENAS PARA A SEMANA EPIDEMIOLOGICA 

table(corona_2020$SEM_NOT)
table(corona_2021$SEM_NOT)
 
corona_2020_jul <- corona_2020[corona_2020$SEM_NOT >= 31,]
 
corona_2021_jul <- corona_2021[corona_2021$SEM_NOT <= 31,]
 
table(corona_2020_jul$SEM_NOT)
table(corona_2021_jul$SEM_NOT)
 
rm(corona_2020)
rm(corona_2021)
 
# Filtando para o estado do RIO DE JANEIRO

table(corona_2020_jul$SG_UF)
table(corona_2021_jul$SG_UF)

corona_2020_jul_rj <- corona_2020_jul%>%
  filter(SG_UF == "RJ")

corona_2021_jul_rj <- corona_2021_jul%>%
  filter(SG_UF == "RJ")

# REMOVENDO AS BASES 

rm(corona_2020_jul)
rm(corona_2021_jul)
 
#  Filtrando pra SRAG por COVID-19

table(corona_2020_jul_rj$CLASSI_FIN)
table(corona_2021_jul_rj$CLASSI_FIN)
 
# 5 é por covid e 4 não especificada

covid_2020_rj <- corona_2020_jul_rj %>%
   filter(CLASSI_FIN == "5")
 
covid_2021_rj <- corona_2021_jul_rj %>%
   filter(CLASSI_FIN == "5")

# REMOVENDO AS BASES

rm(corona_2020_jul_rj)
rm(corona_2021_jul_rj)

# SALVANDO AS BASES

write.csv2(covid_2021_rj, "covid_2021_rj.csv")
write.csv2(covid_2020_rj, "covid_2020_rj.csv")



#Bases

corona_2020 <- read.csv2("covid_2020_rj.csv")
corona_2021 <- read.csv2("covid_2021_rj.csv")

# Selecionando as variáveis

corona_2020 <- corona_2020 %>%
 select(SEM_NOT, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO, DT_NASC, NU_IDADE_N,
        TP_IDADE, CS_RACA, CS_ESCOL_N, ID_MN_RESI, CLASSI_FIN, EVOLUCAO)

corona_2021 <- corona_2021 %>%
select(SEM_NOT, DT_SIN_PRI, DT_NOTIFIC, CS_SEXO, DT_NASC, NU_IDADE_N,
       TP_IDADE, CS_RACA, CS_ESCOL_N, ID_MN_RESI, CLASSI_FIN, EVOLUCAO)

# JUNTANDO AS DUAS BASES EM UMA

covid_rj_2020_2021 <- bind_rows(corona_2020, corona_2021)

# SALVANDO

write.xlsx(covid_rj_2020_2021, "vacinacao_rj.xlsx")



# CORRIGINDO A BASE

vacina <- read.xlsx("vacinacao_rj.xlsx")

# Filtrando para 18 anos ou mais 

vacina$idade.anos <- NULL
vacina$idade.anos[vacina$TP_IDADE == "3"] <- vacina$NU_IDADE_N[vacina$TP_IDADE == "3"]
vacina$idade.anos[vacina$TP_IDADE == "2"] <- vacina$NU_IDADE_N[vacina$TP_IDADE == "2"]/12
vacina$idade.anos[vacina$TP_IDADE == "1"] <- vacina$NU_IDADE_N[vacina$TP_IDADE == "1"]/365.25


table(vacina$idade.anos)

# Filtrando para idade >= que 18

vacina <- vacina %>%
  filter(idade.anos >= 18)

# Criando faixa etária

vacina <- vacina %>%
  mutate(fx_etaria = cut(idade.anos, breaks = c(18,30, 40, 50, 60, 70, 80, Inf),
                         right = FALSE,
                         labels = c("18 a 29","30 a 39", "40 a 49", "50 a 59", "60 a 69",
                                    "70 a 79", "80 ou mais"))) 

table(vacina$fx_etaria)

# Reclassificando as variáveis

names(vacina)

vacina <- vacina %>%
  mutate(CS_SEXO = as.factor(CS_SEXO),
         CS_RACA = as.factor(CS_RACA),
         CS_ESCOL_N = as.factor(CS_ESCOL_N),
         CLASSI_FIN = as.factor(CLASSI_FIN),
         EVOLUCAO = as.factor(EVOLUCAO),
         ID_MN_RESI = as.factor(ID_MN_RESI))


table(vacina$DT_SIN_PRI)

# Corrigindo a data

vacina$DT_SIN_PRI <- lubridate::as_date(vacina$DT_SIN_PRI, format = "%d/%m/%Y")
vacina$DT_NOTIFIC <- lubridate::as_date(vacina$DT_NOTIFIC, format = "%d/%m/%Y")


# Filtrando para 01 de agosto de 2020 até 31 de agosto de 2021 

vacina <- filter(vacina, DT_NOTIFIC >= "2020-08-01" & DT_NOTIFIC <= "2021-08-31")

### Sexo 

summary(vacina$CS_SEXO)

# mundando os labels

vacina <- vacina %>%
  mutate(CS_SEXO = recode(CS_SEXO, `F` = "Feminino", `I` = "Ignorado", `M` = "Masculino"))

# mudando a ordem das variáveis

vacina$CS_SEXO <- factor(vacina$CS_SEXO, levels = c("Masculino", "Feminino" ,"Ignorado"))


### Raça

summary(vacina$CS_RACA)

# mudando os labels

vacina <- vacina %>%
  mutate(CS_RACA = recode(CS_RACA, `1` = "Branca", `2` = "Preta", `3` = "Amarela",`4` = "Parda",`5` = "Índigena", `9` = "Ignorado"))

# mudando a ordem das variáveis

vacina$CS_RACA <- factor(vacina$CS_RACA, levels = c("Branca", "Parda" ,"Preta", "Amarela", "Índigena", "Ignorado"))

# colocando NA'S em ignoradO

vacina$CS_RACA[is.na(vacina$CS_RACA)] <- "Ignorado"

### Escolaridade

summary(vacina$CS_ESCOL_N)

# mudando os labels

vacina <- vacina %>%
  mutate(CS_ESCOL_N = recode(CS_ESCOL_N, `0` = "Sem escolaridade", `1` = "Fundamental 1º ciclo",
                             `2` = "Fundamental 2º ciclo", `3` = "Médio",`4` = "Superior", 
                             `5` = "Não se aplica", `9` = "Ignorado"))


# criando nova categoria "Fundamental"

vacina$CS_ESCOL_N <- factor(vacina$CS_ESCOL_N, levels = c("Sem escolaridade", "Fundamental 1º ciclo",       
                                                          "Fundamental 2º ciclo" ,"Médio",                      
                                                          "Superior", "Não se aplica", "Ignorado", "Fundamental" )) 


# colocando "Fundamental 1º ciclo" e "Fundamental 2º ciclo" na categoria"Fundamental"

vacina$CS_ESCOL_N[vacina$CS_ESCOL_N == "Fundamental 1º ciclo"] <- "Fundamental"
vacina$CS_ESCOL_N[vacina$CS_ESCOL_N == "Fundamental 2º ciclo"] <- "Fundamental"

# colocando "Não se aplica" e NA's em "Ignorado"

vacina$CS_ESCOL_N[vacina$CS_ESCOL_N == "Não se aplica"] <- "Ignorado"

vacina$CS_ESCOL_N[is.na(vacina$CS_ESCOL_N)] <- "Ignorado"


# mudando a ordem das variaveis

vacina$CS_ESCOL_N <- factor(vacina$CS_ESCOL_N, levels = c("Sem escolaridade", "Fundamental" ,"Médio",                      
                                                          "Superior","Ignorado"))

summary(vacina$CS_ESCOL_N) # Verificar


### Evolução

summary(vacina$EVOLUCAO)

# mudando os labels

vacina <- vacina %>%
  mutate(EVOLUCAO = recode(EVOLUCAO, `1` = "Cura", `2` = "Óbito por SRAG",
                           `3` = "Óbito por outras causas",`9` = "Ignorado"))


# criando nova categoria "Em cuidados hospitalares"

vacina$EVOLUCAO <- factor(vacina$EVOLUCAO, levels = c("Cura", "Óbito por SRAG", "Óbito por outras causas",
                                                      "Ignorado", "Em cuidados hospitalares")) 


# colocando "Ignorado" em "Em cuidados hospitalares"

vacina$EVOLUCAO[vacina$EVOLUCAO == "Ignorado"] <- "Em cuidados hospitalares"


# colocando NA'S em "Em cuidados hospitalares"

vacina$EVOLUCAO[is.na(vacina$EVOLUCAO)] <- "Em cuidados hospitalares"


# excluindo a categoria "Óbito por outras causas"

vacina <- vacina %>%
  filter(! (EVOLUCAO %in% c("Óbito por outras causas"))) # sai N = 66

# mudando a ordem das categorias

vacina$EVOLUCAO<- factor(vacina$EVOLUCAO, levels = c("Cura", "Óbito por SRAG" ,"Em cuidados hospitalares"))

summary(vacina$EVOLUCAO) # verificar


### Classificação final do caso

summary(vacina$CLASSI_FIN)

# mudando os labels

vacina <- vacina %>%
  mutate(CLASSI_FIN = recode(CLASSI_FIN, `5` = "SRAG por COVID-19"))


# Criando uma base de data, casos, obitos e faixa etaria

vacina$ano <- year(vacina$DT_NOTIFIC)

teste1 <- vacina %>%
  group_by(ID_MN_RESI, DT_NOTIFIC,ano, SEM_NOT, fx_etaria, .drop=F) %>%
  arrange(ano)  %>%
  summarise(casos_graves=n(), obitos_graves=sum(EVOLUCAO=="Óbito por SRAG", na.rm=T))

teste_geral <- teste1 %>%
  group_by(fx_etaria,DT_NOTIFIC, ano,SEM_NOT) %>%
  summarise(casos = sum(casos_graves),
            obitos = sum(obitos_graves),
            .groups = "drop_last")


# Fazendo um calculo de letalidade

teste_geral$letalidade <- round(teste_geral$obitos / teste_geral$casos, 2)

# Criando a variavel mes e semana epidemiologica

teste_geral$mes <- month(teste_geral$DT_NOTIFIC)
teste_geral$ano <- ifelse(teste_geral$SEM_NOT <= 53, 2020, 2021)
teste_geral$ano <- year(teste_geral$DT_NOTIFIC)
teste_geral$mes_extenso <- ifelse(teste_geral$mes <= 12, teste_geral$mes, teste_geral$mes-12)
teste_geral$mes_extenso <- month(teste_geral$mes_extenso, label=T)


# separando a base em 2020 e 2021

teste_2020 <- teste_geral%>%
  filter(ano == "2020")%>%
  group_by(mes_extenso, casos, obitos, letalidade)

teste_2021 <- teste_geral%>%
  filter(ano == "2021")%>%
  group_by(mes_extenso, casos, obitos, letalidade)

# Base de vacinas

doses <- read.xlsx("doses_erj.xlsx")

# Mudando o nome da variavel

names(doses)[1] <- "mes_extenso"

# Juntando as bases

e <- merge(teste_2021, doses, by= ("mes_extenso"), all.x = T)

e

# Juntando 2020 e 2021

casos.ob.vac <- bind_rows(teste_2020, e)

# Criando o grafico

p1 <- casos.ob.vac%>%
  ggplot(aes(x = DT_NOTIFIC, group = 1))+
  geom_line(aes(y = casos, colour = "Casos"), size = 1)+
  geom_line(aes( y = obitos, colour = "Óbitos"), size = 1)+
  geom_smooth(aes(y = cob.1adose*5), colour = "black", alpha = 0.9, linetype = "dashed", se = FALSE)+
  annotate(x= date("2020-08-30"),y= 250 ,label= "- - Cob.vacinal (%): Esquema incompleto",hjust = "left", geom= "text", colour = "black")+
  annotate(geom = "text", x = date("2021-08-31"), y = 348, label = "64,99%", hjust = "left", color = "black")+
  geom_smooth(aes( y = cob.esqcompleto*5), colour = "blue", alpha = 0.9, linetype = "dashed", se = FALSE)+
  annotate(x= date("2020-08-30"),y= 233 ,label= "- - Cob.vacinal (%): Esquema completo" ,hjust = "left", geom= "text", color = "blue")+
  annotate(geom = "text", x = date("2021-08-31"), y = 178, label = "32,83%", hjust = "left", color = "blue")+
   scale_y_continuous(name = "Frequência", 
                     sec.axis = sec_axis(~./5, name = "Cobertura Vacinal (%)",
                     labels = function(b) { paste0(round(b * 1, 0), "%")})) + 
  labs(colour = "")+
  xlab("Data de notificação") + 
  ggtitle("Serie temporal dos casos e óbitos por covid-19 no Rio de Janeiro, 2020-2021")+
  theme_minimal(base_size = 15
                ) + #15
  scale_x_date(date_labels = "%b-%y", breaks = "1 month")+
  theme(text = element_text(size = 100), #10
        strip.text = element_text(size = 90),#9
        legend.text = element_text(size = 20), #20
        legend.title = element_text(size = 20), #20
        legend.key.size = unit(10,"points"))+ #10
  theme_bw()+
        theme(
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"), 
          legend.key = element_rect(fill = "transparent", colour = NA), 
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"))
        


p1 

#Salvando o grafico com fundo transparente

ggsave(p1, filename = "Grafico 1_vacinacao_rj.png",  
       width = 34.3, height = 15,units = "cm", device='png',bg = "transparent", dpi = 800)


