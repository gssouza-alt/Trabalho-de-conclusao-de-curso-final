library(rbcb)
library(lubridate)
#library(sidrar)
library(tidyverse)
library(ggplot2)
library(readxl)
#library(mFilter)
library("forecast")
library("tseries")
library(vars)
options(scipen = 999)


#######Obtendo dados########

df = data.frame()

df= get_series("13667", start_date = '2000-03-01',end_date = '2023-12-31')
names(df) = c("date","Inad_pub")

df$Inad_pri = get_series("13673", start_date = '2000-03-01',end_date = '2023-12-31')$`13673`
df$Inad_est = get_series("13679", start_date = '2000-03-01',end_date = '2023-12-31')$`13679`
df$Spub = get_series("2007", start_date = '2000-03-01',end_date = '2023-12-31')$`2007`
df$Spri = get_series("12106", start_date = '2000-03-01',end_date = '2023-12-31')$`12106`
df$Sest = get_series("12150", start_date = '2000-03-01',end_date = '2023-12-31')$`12150`
df$Pib = get_series("4380", start_date = '2000-03-01',end_date = '2023-12-31')$`4380`
df$Dep_compul = get_series("1828", start_date = '2000-03-01',end_date = '2023-12-31')$`1828`
df$Cred_pib = get_series("20622", start_date = '2000-03-01',end_date = '2023-12-31')$`20622`

##Selic é uma serie diária. Portanto, será tratada para de tonar mensal como os demais dados

Selic = get_series("1178", start_date = '2000-03-01',end_date = '2009-12-31')
Selic2 = get_series("1178", start_date = '2010-01-01',end_date = '2019-12-31')
Selic3 = get_series("1178", start_date = '2020-01-01',end_date = '2023-12-31')
Selic = rbind(Selic, Selic2, Selic3)
Selic[1]$date = as.Date(format(Selic[1]$date, "%Y-%m-01"))
#Selic = data.frame(Selic)
df$Selic = Selic[!duplicated(Selic$date),]$`1178`
remove(Selic, Selic2, Selic3)


##Dolar é uma serie diária. Portanto, será tratada para de tonar mensal como os demais dados

Dolar = get_series("1", start_date = '2000-03-01',end_date = '2023-12-31')
Dolar[1]$date = as.Date(format(Dolar[1]$date, "%Y-%m-01"))
#Dolar = data.frame(Dolar)
df$Dolar = Dolar[!duplicated(Dolar$date),]$`1`
remove(Dolar)

#Demais series eram complicadas para extração por R. Portando, foram manuseados em uma planilha de Excel

dados2 <- read_excel("C:/Users/santa/Downloads/Base auxiliar (2).xlsx") ## Troque o diretorio se necessario


df = cbind(df, dados2[,c(2:13)])
remove(dados2)


#dados tratados

#df = read.csv("df.csv")


#write.csv(df,file = "df2.csv")
### Função para deflacionar e logaritmizar valores monetários, para evitar problemas com outliers

deflation = function(arr){
  n = length(arr)
  dArr =arr*(df$Expec_ipca[n]/df$Expec_ipca)
  return(dArr)
}

#series calculadas

df$jurosdiff = c(NA, diff(df$Selic))
df$defSalarial = df$Sal_Min_Nec - df$Sal_Min
df$jurosReal = df$Selic - df$Expec_ipca
df$Salarios = log(deflation(df$Rend_ped))
df$Arr_IR = deflation(df$Arr_IR)
df$ln_Arr_IRPF = deflation(df$Arr_IRPF)
df$Arr_IRPJ = deflation(df$Arr_IRPJ)
df$Arr_IRRF = deflation(df$Arr_IRRF)
df$Sal_Min = deflation((df$Sal_Min))
df$Sal_Min_Nec = deflation(df$Sal_Min_Nec) #Salário minimo necessário segundo o DIEESE
df$PibReal = log(deflation(df$Pib))
df$base_Mo = deflation(df$`Base monetária`)
df$base_MoA = deflation(df$`Base monetária ampliada`)
df$defSalarial = deflation(df$defSalarial)
df$Dep_compul = deflation(df$Dep_compul)
df$lnSpri = log(deflation(df$Spri))

# Calculo da inadimplecia total
# A inadimplencia total do SFN é uma média ponderada com das inadimplencias de cada setor

df$Inad_sfn = ((df$Inad_pub/100*df$Spub +
                  df$Inad_pri/100*df$Spri + 
                  df$Inad_est/100*df$Sest)
               /(df$Spub+df$Sest+df$Spri))*100


#Plot da relação credito PIB

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Cred_pib, color = "Cred_pib")) +
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("Cred_pib" = "blue"))+
  theme_light()

#Plot de inadimplência

ggplot(df[,], aes(x = date)) +
  geom_line(aes(y = Inad_pri, color = "Inad_pri")) +
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("Inad_pri" = "blue"))+
  theme_light()

summary(df$Inad_pri)



#Plot Selic

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Selic, color = "Selic")) +
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("Selic" = "blue"))+
  theme_light()

summary(df$Selic)

#Plot IPCA

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Expec_ipca, color = "Expec_ipca")) +
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("Expec_ipca" = "blue"))+
  theme_light()


summary(df$Expec_ipca)


#Plot PIB

ggplot(df, aes(x = date)) +
  geom_line(aes(y = PibReal, color = "PibReal")) +
  labs(title = "", x = "Data", y = "") +
  scale_color_manual(values = c("PibReal" = "blue"))+
  theme_light()

summary(df$PibReal)


#Plot Salarios

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Salarios, color = "Salarios")) +
  labs(title = "", x = "Data", y = "") +
  scale_color_manual(values = c("Salarios" = "blue"))+
  theme_light()

summary(df$Salarios)


#Estacionariedade

df_ts = ts(df[1:286,c("Inad_pri","Selic", "Salarios", "PibReal", "Expec_ipca")],start = c(2000,03),frequency = 12)

cor(df_ts)

ndiffs(df_ts)

testVar <- function(df_ts, variaveisModelo){
  resultados <- data.frame()
  numVars = 1:length(variaveisModelo)
  for (i in numVars) {
    adf1 = ur.df(df_ts[,i],type = "trend", selectlags = "AIC")
    if (0.01 > adf1@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[1, i] = sprintf("%.2f***", adf1@testreg$coefficients["z.lag.1","t value"])
    }else if(0.05 > adf1@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[1, i] = sprintf("%.2f**", adf1@testreg$coefficients["z.lag.1","t value"])
    }else if(0.1 > adf1@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[1, i] = sprintf("%.2f*", adf1@testreg$coefficients["z.lag.1","t value"])
    }else {resultados[1, i] = sprintf("%.2f",  adf1@testreg$coefficients["z.lag.1","t value"])}
    
    adf2 = ur.df(df_ts[,i],type = "drift", selectlags = "AIC")
    if (0.01 > adf2@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[2, i] = sprintf("%.2f***", adf2@testreg$coefficients["z.lag.1","t value"])
    }else if(0.05 > adf2@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[2, i] = sprintf("%.2f**", adf2@testreg$coefficients["z.lag.1","t value"])
    }else if(0.1 > adf2@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[2, i] = sprintf("%.2f*", adf2@testreg$coefficients["z.lag.1","t value"])
    }else {resultados[2, i] = sprintf("%.2f",  adf2@testreg$coefficients["z.lag.1","t value"])}
    
    adf3 = ur.df(df_ts[,i],type = "none", selectlags = "AIC")
    if (0.01 > adf3@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[3, i] = sprintf("%.2f***", adf3@testreg$coefficients["z.lag.1","t value"])
    }else if(0.05 > adf3@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[3, i] = sprintf("%.2f**", adf3@testreg$coefficients["z.lag.1","t value"])
    }else if(0.1 > adf3@testreg$coefficients["z.lag.1","Pr(>|t|)"]){resultados[3, i] = sprintf("%.2f*", adf3@testreg$coefficients["z.lag.1","t value"])
    }else {resultados[3, i] = sprintf("%.2f",  adf3@testreg$coefficients["z.lag.1","t value"])}
    
    kpss1 = kpss.test(df_ts[,i], null="Trend")
    if (0.01 >= kpss1$p.value){resultados[4, i] = sprintf("%.2f***", kpss1$statistic)
    }else if(0.05 >= kpss1$p.value){resultados[4, i] = sprintf("%.2f**", kpss1$statistic)
    }else if(0.1 > kpss1$p.value){resultados[4, i] = sprintf("%.2f*", kpss1$statistic)
    }else {resultados[4, i] = sprintf("%.2f", kpss1$statistic)}
    
    
    kpss2 = kpss.test(df_ts[,i], null="Level")
    if (0.01 >= kpss2$p.value){resultados[5, i] = sprintf("%.2f***", kpss2$statistic)
    }else if(0.05 >= kpss2$p.value){resultados[5, i] = sprintf("%.2f**", kpss2$statistic)
    }else if(0.1 > kpss2$p.value){resultados[5, i] = sprintf("%.2f*", kpss2$statistic)
    }else {resultados[5, i] = sprintf("%.2f", kpss2$statistic)}
    
    pp1 = pp.test(df_ts[,i], type = "Z(alpha)" )
    if (0.01 > pp1$p.value){resultados[6, i] = sprintf("%.2f***", pp1$statistic)
    }else if(0.05 > pp1$p.value){resultados[6, i] = sprintf("%.2f**", pp1$statistic)
    }else if(0.1 > pp1$p.value){resultados[6, i] = sprintf("%.2f*", pp1$statistic)
    }else {resultados[6, i] = sprintf("%.2f", pp1$statistic)}
    
    pp2 = pp.test(df_ts[,i], type = "Z(t_alpha)" )
    if (0.01 > pp2$p.value){resultados[7, i] = sprintf("%.2f***", pp2$statistic)
    }else if(0.05 > pp2$p.value){resultados[7, i] = sprintf("%.2f**", pp2$statistic)
    }else if(0.1 > pp2$p.value){resultados[7, i] = sprintf("%.2f*", pp2$statistic)
    }else {resultados[7, i] = sprintf("%.2f", pp2$statistic)}
    
  }
  colnames(resultados) = variaveisModelo[numVars]
  row.names(resultados) = c('adf_trend','adf_drift','adf_none','kpss_trend',
                            "kpss_level",'pp_Z(alpha)', "pp_Z(t_alpha)")
  
  
  return(view(resultados))
}


testVar(df_ts, c("Inad_pri","Selic", "Salarios", "PibReal", "Expec_ipca"))#Selic não é estácionaria de ordem 0, ln_Hiato é a unica serie que passa em todos os teste de está cionariedade


df_ts = diff(df_ts)

#Plot de variáveis
par(mfrow=c(2,2))
plot(x= df$date[2:286],y = diff(df$Selic) ,type="l", col="blue", lwd=2, xlab="Data", ylab="%", main="Taxa Selic (Selic)")
plot(x= df$date[2:286],y = diff(df$PibReal) ,type="l", col="blue", lwd=2, xlab="Data", ylab="Valor", main="PIB (ln_Hiato)")
plot(x= df$date[2:286],y = diff(df$Salarios) ,type="l", col="blue", lwd=2, xlab="Data", ylab="Valor", main="Salário Médio (ln_Salarios)")
plot(x= df$date[2:286],y = diff(df$Expec_ipca) ,type="l", col="blue", lwd=2, xlab="Data", ylab="%", main="Expectativa de inflação (Expec_ipca)")

par(mfrow=c(1,1))

#exogen

#Função para gerar Dummies
create_dummies <- function(start, end, data){
  aux = rep(0, length(data[,1]))
  aux[rep(as.Date(start), length(data[,1])) <= as.Date(data$date) &
        rep(as.Date(end), length(data[,1])) >= as.Date(data$date)] = 1
  return(aux)
}

df$dummySubprime = create_dummies("2008-09-01", "2009-09-01", df) #Dummie para a crise do subprime em 2008
df$dummyEstagflacao = create_dummies("2014-12-01", "2017-05-01", df) 
df$dummyCovid1 = create_dummies("2020-05-01", "2021-07-01", df) #Dummie para o covid 2019 no Brasil Pronampe inicio e sacionamento
df$dummyCovid = create_dummies("2021-08-01", "2023-05-01", df) #Dummie para o covid 2019 no Brasil Pronampe permanente e fim da covid no brasil

dummies = ts(df[2:286,c('dummySubprime', "dummyEstagflacao","dummyCovid1")],start = c(2000,03),frequency = 12)

#Estimção

VARselect(df_ts,
          lag.max=12, season = 12,type = 'both',
          exogen = dummies)$criteria #Teste seleção de defasagens VAR.



md1 = VAR(df_ts,
          p = 2,season = 12,  type = 'both',
          exogen = dummies) 

summary(md1)



causality(md1, cause = "Inad_pri")
causality(md1, cause = "Selic")
causality(md1, cause = "Expec_ipca")
causality(md1, cause = "PibReal")
causality(md1, cause = "Salarios")
md1

### O teste da presença de correlação serial nos resíduos:
## Para interpretar essas estatísticas, observe que um valor de p maior que 5% 
## indica que há uma ausência de correlação serial.
serial.test(md1)
serial.test(md1, lags.pt = 18) 
serial.test(md1, lags.pt = 24)


### O teste da presença de correlação serial nos resíduos:
## Para interpretar essas estatísticas, observe que um valor de p maior que 5% 
## indica que há uma ausência de Heteroscedasticidade.
arch.test(md1, multivariate.only = FALSE)



#Teste de normalidade

hist(md1$varresult$Inad_pri$residuals)

plot(md1$varresult$Inad_pri$residuals)
abline(h = 0, col = "red", lwd = 2)

normality.test(md1, multivariate.only = TRUE)


#impulso resposta

#Inad_pri
irf.Inad <- irf(md1, impulse = c("Inad_pri"), response = "Inad_pri", 
                n.ahead = 12, boot = TRUE, ci = 0.90,runs = 1000)
plot(irf.Inad, ylab = "ouput", main = "")

#IPCA
irf.Inad <- irf(md1, impulse = c("Expec_ipca"), response = "Inad_pri", 
                n.ahead = 12, boot = TRUE, ci = 0.90,runs = 1000)
plot(irf.Inad, ylab = "ouput", main = "")

#Selic

irf.Inad <- irf(md1, impulse = c("Selic"), response = "Inad_pri", 
                n.ahead = 12, boot = TRUE, ci = 0.90, runs = 1000)
plot(irf.Inad, ylab = "ouput", main = "")


#Pib

irf.Inad <- irf(md1, impulse = c("PibReal"), response = "Inad_pri", 
                n.ahead = 12, boot = TRUE, ci = 0.90, runs = 1000)
plot(irf.Inad, ylab = "ouput", main = "")


#salarios

irf.Inad <- irf(md1, impulse = c("Salarios"), response = "Inad_pri", 
                n.ahead = 12, boot = TRUE,  ci = 0.90, runs = 1000)
plot(irf.Inad, ylab = "ouput", main = "")


#lnSpri

irf.Inad <- irf(md1, impulse = c("lnSpri"), response = "Inad_pri", 
                n.ahead = 24, boot = TRUE, ci = 0.90)
plot(irf.Inad, ylab = "ouput", main = "Shock from lnSpri")

#AJUSTE DO MODELO NA AMOSTRA

dfFit = data.frame(tempo = 1:length(md1$varresult$Inad_pri$fitted.values))
dfFit$fit = md1$varresult$Inad_pri$fitted.values
dfFit$observado = md1$datamat$Inad_pri

ggplot(dfFit, aes(x = tempo)) +
  geom_line(aes(y = fit, color = "fit")) +
  geom_line(aes(y = observado, color = "observado")) +
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("fit" = "red","observado" = "blue"))+
  theme_light()


#predição

predictions <- predict(md1, n.ahead = 12, ci = 0.90, dumvar = dummies[0:12,])
plot(predictions, names = "Inad_pri")



dfout = data.frame(predictions$fcst$Inad_pri)


dfout$date = get_series("13667", start_date = '2024-01-31',end_date = '2024-12-01')$date

dfout$Inad_pri = diff(get_series("13673", start_date = '2023-12-31',end_date = '2024-12-30')$`13673`)


ggplot(dfout, aes(x = date)) +
  geom_line(aes(y = fcst, color = "Inad_pri_Prevista")) +
  geom_line(aes(y = Inad_pri, color = "Inad_pri_Real"))+
  geom_line(aes(y = lower, color = "Confiança 90%")) +
  geom_line(aes(y = upper, color = "Confiança 90%"))+
  labs(title = "", x = "Data", y = "%") +
  scale_color_manual(values = c("Inad_pri_Prevista" = "blue",  "Inad_pri_Real" = "red", "Confiança 90%" = "black" ))+
  theme_light()

