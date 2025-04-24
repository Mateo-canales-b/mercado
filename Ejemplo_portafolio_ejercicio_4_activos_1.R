# Profesor Guillermo Yañez

# Antes de iniciar:

# Probaremos una nueva librería para bajar datos de Yahoo Finance:
# https://docs.ropensci.org/yfR/
if(!require("yfR")) install.packages("yfR")
library(yfR)
# probemos algunas cosas inmediatamente:

# yf_collection_get("SP500") #Atención que esto es largo! Baja toda la base

library(BatchGetSymbols) # Esta librería va a ser reemplazada por yfR

sp500 <- GetSP500Stocks()
sp500$Tickers

write.table(sp500,row.names=TRUE,col.names=NA,file="gspc.xls",sep=";") 

# De esta base puede elegir sus acciones a analizar

# Exportemos a Excel

# Agradecimientos a los alumnos de la U. de Chile 2019 
# para lo siguiente (adaptado por el profesor): 
# Jorge Galán, Nicolás Ganter, M. Ignacia Juez, Boris Travisany, Francisco Venegas.
# Seleccione un conjunto de 4 acciones, un índice de mercado y un índice sectorial del mercado US. 
# Suponga que sólo existen estos activos en la economía 
# 1. Obtenga la frontera eficiente 
# 2. Obtenga la LMC (ayuda: Obtenga el portafolio de mercado) 
# 3. Compare con el rendimiento y varianza del portafolio de mercado elegido. ¿A qué se debe la diferencia? 


# PREGUNTA 1, 2 y 3--------------------------------------------------------


if(!require("BiocManager")) install.packages("BiocManager")
if(!require("xtable")) install.packages("xtable")
# La siguiente debe ser cargada así para que corra en 3.61
install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
require(IntroCompFinR)

# Vamos a pedir varias librerías de una vez con pacman:
library(pacman)
p_load("readxl","quantmod","tidyverse","reshape2","fBasics","PerformanceAnalytics","quadprog",
       "IntroCompFinR", "xtable","formattable")
# NDX es Nasdaq100, IXIC es Nasdaq composite, TNX es treasury notes
tickers <- c("GOOG","NFLX", "AMZN", "META", "^NDX", "^IXIC", "^TNX")

#Obtener los retornos 
getSymbols(tickers, src= "yahoo", from = "2018-01-01", to = "2022-09-30", periodicity = "monthly")

# https://finance-r.netlify.app/quantmod.html

tickers.2 <- c("GOOG","NFLX", "AMZN", "META", "NDX", "IXIC")
tickers.tnx <- c("TNX")
#Tomar sólo los precios de cierre
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply
list <- lapply(tickers.2, function(x) Cl(get(x)))

#Para el caso de la tasa libre de riesgo, convertirla a porcentaje (/100) 
# y dejarla en retornos mensuales (/12)
list.tnx <- lapply(tickers.tnx, function(x) Cl(get(x))/1200)
#Unir todos los precios de cierre en listaS por separadas
precio.cierre <- do.call(merge,list)
precio.cierre.tnx <- do.call(merge,list.tnx)
#Remover objetos que no son necesarios
rm(tickers, tickers.2, AMZN, META, GOOG, IXIC, NFLX, list, NDX, TNX, tickers.tnx, list.tnx)

#Cálculo de retornos (para activos riesgosos)
# apply funciona con data frame así como lapply es para listas (el 2 se refiere a columnas)
delta <- function(x) {Delt(x, type = "arithmetic")}
retornos <- data.frame(apply(precio.cierre, 2, delta))
retornos$fecha <- index(precio.cierre)
#Unir bases 
rownames(retornos) <- retornos[,7] # Agregamos fechas a nombre filas
retornos.final <- merge(x=retornos, y=precio.cierre.tnx, by= "row.names")
rownames(retornos.final) <- retornos.final[,1]
retornos.final$Row.names <- NULL # Una forma de eliminar la columna Row.names
retornos.final <- na.locf(retornos.final[c(7,1,2,3,4,5,6,8)])

#Remover bases no necesarias
rm(precio.cierre, precio.cierre.tnx, retornos)


#Calcular y graficar riesgo/retorno

#Estadística descriptiva
mean <- apply(retornos.final[,2:7], 2,
              function(x){
                mean(x)
                }
              )

percent(mean) # Recordemos percent es de formattable 
# con apply los retornos quedan con el nombre de columna
mean.rf <- apply(retornos.final[8], 2, function(x) mean(x)) 
# Otra forma de aplicar una función en la misma línea sin usar {}
percent(mean.rf)
sd <- apply(retornos.final[,2:7], 2, function(x) sd(x))
percent(sd)
cov <- cov(retornos.final[2:7])
cov2 <- cov(retornos.final[2:5])

tab.retornos <- matrix(append(mean,sd),nrow =2, ncol=6,byrow=TRUE)
colnames(tab.retornos) <- c("Google","Netflix","Amazon","Facebook","Nasdaq100","Nasdaq")
rownames(tab.retornos) <- c("E(r)","D std")
tab.retornos <- as.table(tab.retornos)
percent(tab.retornos)

# Podemos exportar la tabla a latex usando xtable
print(xtable(tab.retornos, type = "latex"), file = "tab.retornos.tex")

#Gráfico trade off riesgo-retorno:

g.tradeoff <- ggplot(mapping = aes(sd, mean, label = c("GOOG","NFLX","AMZN","META","NDX", "IXIC")))
g.tradeoff <- g.tradeoff + geom_point() + geom_text(hjust = 0, vjust = 0)
g.tradeoff <- g.tradeoff + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(-0.005,0.02))
g.tradeoff <- g.tradeoff + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g.tradeoff <- g.tradeoff + theme_bw() + xlab("Riesgo") + ylab("Retorno") +  ggtitle("Trade-off Riesgo-Retorno",
                                                                                    subtitle = "4 Activos riesgosos, 2 índices")
g.tradeoff

#Portafolio de mínima varianza
#Estadística descriptiva 4 activos en la economía
mean.4risky <- apply(retornos.final[,2:5], 2, function(x) mean(x))
mean.4risky
sd.4risky <- apply(retornos.final[,2:5], 2, function(x) sd(x))
cov.4risky <- cov(retornos.final[2:5])
cov.4risky

# https://rdrr.io/rforge/IntroCompFinR/man/globalMin.portfolio.html
globalmin <- globalMin.portfolio(mean.4risky, cov.4risky, shorts = T)
globalmin

tab.minimavarianza <- matrix(c(globalmin$er,globalmin$sd), ncol=1, byrow=TRUE )
colnames(tab.minimavarianza) <- c("Port. MinV")
rownames(tab.minimavarianza) <- c("E(r)", "D std")
tab.minimavarianza <- as.table(tab.minimavarianza)
percent(tab.minimavarianza)

tab.weight <- matrix(c(globalmin$weights), ncol=4, byrow=TRUE)
colnames(tab.weight) <- c("Google","Netflix","Amazon","Facebook")
rownames(tab.weight) <- c("Ponderaciones")
tab.weight <- as.table(tab.weight)
percent(tab.weight)


#Gráfico portafolio mínima varianza
g.minvar <- ggplot(mapping = aes(label = c("Min V"))) +
  geom_point(mapping = aes(globalmin$sd, globalmin$er,
                                                color = "1",
                                                ))
g.minvar <- g.minvar + geom_point(mapping = aes(sd.4risky, mean.4risky, 
                                                color = "2",
                                                label = c("GOOG","NFLX",
                                                          "AMZN","META")))
g.minvar <- g.minvar + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),
                                          limits = c(-0.005,0.02))
g.minvar <- g.minvar + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),
                                          limits = c(0,0.2))
g.minvar <- g.minvar + scale_color_manual("", values = c("black", "red"),
                                          labels = c("MinV", "Activos riesgosos"))
g.minvar <- g.minvar + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g.minvar <- g.minvar + ggtitle("Trade-off Riesgo-Retorno",
                               subtitle = "4 activos riesgosos & minima varianza")
g.minvar

#Portafolio de igual retorno que las acciones

#Retornos igual a cada activo pero mínima varianza
port.goog <- efficient.portfolio(mean.4risky, cov.4risky, mean[1], shorts = T)
port.goog
goog_w <- percent(port.goog$weights)

port.nflx <- efficient.portfolio(mean.4risky, cov.4risky, mean[2], shorts = TRUE)
port.nflx
nflx_w <- percent(port.nflx$weights)


port.amzn <- efficient.portfolio(mean.4risky, cov.4risky, mean[3], shorts = TRUE)
port.amzn

amzn_w <- percent(port.amzn$weights)


port.meta <- efficient.portfolio(mean.4risky, cov.4risky, mean[4], shorts = TRUE)
port.meta

meta_w <- percent(port.meta$weights)

tabla <- matrix(c(goog_w,nflx_w,amzn_w,meta_w),nrow=4,ncol=4,byrow=T)

rownames(tabla) <- c("Google","Netflix","Amazon","Facebook")
colnames(tabla) <- c("Google","Netflix","Amazon","Facebook")

print(percent(tabla))

# Verifiquemos que los % sumen 1:

apply(tabla, 1, function(x) sum(x))

#Objeto con medias y desv est de cada objeto
mean.2 <- c(port.goog$er, port.nflx$er, port.amzn$er, port.meta$er)
sd.2 <- c(port.goog$sd, port.nflx$sd, port.amzn$sd, port.meta$sd)

# Demostremos que podemos generar portafolios con menor d.std usando los 4 activos:

tabla2 <- matrix(c(mean.2,sd.2,sd[1:4]),nrow=3,ncol=4,byrow=T)

rownames(tabla2) <- c("Retorno","D.STD*","D.STD indiv")
colnames(tabla2) <- c("Google","Netflix","Amazon","Facebook")

print(percent(tabla2))

 
#Graficar portafolios
g.port <- ggplot() + geom_point(mapping = aes(sd.4risky, mean.4risky, color = "1"))
g.port <- g.port + geom_point(mapping = aes(sd.2, mean.2, color = "2"))
g.port <- g.port + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "3"))
g.port <- g.port + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(-0.005,0.02))
g.port <- g.port + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g.port <- g.port + scale_color_manual("", values = c("blue", "red", "green"), labels = c("Retorno x riesgo",
                                                                                         "Retornos para frontera",
                                                                                         "Min var."))
g.port <- g.port + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g.port <- g.port + ggtitle("Trade-off Riesgo-Retorno", subtitle = "4 activos riesgosos & minima varianza")
g.port

# Tasa libre de riesgo 
risk_free <- mean.rf
risk_free

# Portafolio tangente a la LMC
port.tang <- tangency.portfolio(mean.4risky, cov.4risky, risk_free, shorts = TRUE)

#sharpe ratio
sharpe.ratio <- (port.tang$er - risk_free)/port.tang$sd
sharpe.ratio

#Comparación ratio de sharpe con los índices
#Ratio de sharpe NASDAQ 
mean.ixic <- apply(retornos.final[7], 2, function(x) mean(x))
sd.ixic <- apply(retornos.final[7], 2, function(x) sd(x))
sharpe.ratio.ixic <- (mean.ixic - risk_free)/sd.ixic
sharpe.ratio.ixic
mean.ixic
sd.ixic
#Ratio de sharpe NASDAQ100
mean.ndx <- apply(retornos.final[6], 2, function(x) mean(x))
sd.ndx <- apply(retornos.final[6], 2, function(x) sd(x))
sharpe.ratio.ndx <- (mean.ndx - risk_free)/sd.ndx
sharpe.ratio.ndx
mean.ndx
sd.ndx

#Graficar LMC
g.LMC <- ggplot() + geom_point(mapping = aes(sd.4risky, mean.4risky, color = "1"))
g.LMC <- g.LMC + geom_point(mapping = aes(sd.2, mean.2, color = "2"))
g.LMC <- g.LMC + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "3"))
g.LMC <- g.LMC + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "4"))
g.LMC <- g.LMC + geom_abline(intercept = risk_free, slope = sharpe.ratio)
g.LMC <- g.LMC + scale_y_continuous(breaks = seq(0,0.2, by = 0.01),limits = c(-0.005,0.06))
g.LMC <- g.LMC + scale_x_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(0,0.2))
g.LMC <- g.LMC + scale_color_manual("", values = c("blue", "red", "orange", "green"),labels = c("Activos riesgosos",
                                                                                                "Portafolios sobre frontera",
                                                                                                "Portafolio de mercado",
                                                                                                "Port. Minima varianza"))
g.LMC <- g.LMC + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g.LMC <- g.LMC + ggtitle("Trade-off Riesgo-Retorno", subtitle = "4 activos riesgosos & minima varianza")
g.LMC


#Frontera Eficiente

eff.front.short <- efficient.frontier(mean.4risky, cov.4risky, nport = 1000, alpha.min = -10,
                                      alpha.max = 6, shorts = TRUE)
eff.front.short
port.tang

g.eficiente <- ggplot() + geom_point(mapping = aes(eff.front.short$sd, eff.front.short$er, color = "1"))
g.eficiente <- g.eficiente + geom_point(mapping = aes(sd.4risky, mean.4risky, color = "2"))+ theme_gray()
g.eficiente <- g.eficiente + geom_point(mapping = aes(sd.2, mean.2, color = "3"))
g.eficiente <- g.eficiente + geom_point(mapping = aes(port.tang$sd, port.tang$er, color = "4"))
g.eficiente <- g.eficiente + geom_point(mapping = aes(globalmin$sd, globalmin$er, color = "5"))
g.eficiente <- g.eficiente + geom_abline(intercept = risk_free, slope = sharpe.ratio,color ="purple")
g.eficiente <- g.eficiente + scale_y_continuous(breaks = seq(0,0.2, by = 0.02),limits = c(-0.005,0.1))
g.eficiente <- g.eficiente + scale_x_continuous(breaks = seq(0,0.2, by = 0.05),limits = c(0,0.3))
g.eficiente <- g.eficiente + scale_color_manual("", values = c("gray","blue", "red", "orange", "green"),
                                                labels = c("Frontera",
                                                           "Activos riesgosos",
                                                           "Retornos para frontera",
                                                           "Portafolio mercado","Min var."))
g.eficiente <- g.eficiente + theme_bw() + xlab("Riesgo") + ylab("Retorno")
g.eficiente <- g.eficiente + ggtitle("Trade-off Riesgo-Retorno", subtitle = "4 activos riesgosos & mínima varianza")
g.eficiente

