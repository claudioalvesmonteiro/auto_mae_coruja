# carregar as bibliotecas necessarias para estimar o modelo de ITS
library(nlme)
library(car)
library(tidyverse)

# abrir a base de dados

library(haven)

maecoruja<-read_sav("C:/Users/Dalson/Desktop/maecoruja.sav")
View(maecoruja)

# Modelos de regressão (ols)

m1_ols <- lm(taxa_mort_pop_pe~ tempo + nivel + tendencia,
                 data=maecoruja)

summary(m1_ols)
pred_ols1<-fitted(m1_ols)
pred_ols1


# grafico modelo1

plot(maecoruja$tempo,maecoruja$taxa_mort_pop_pe,
     ylab="Taxa Mortalidade (pop) ",
     ylim=c(0,1),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:12], fitted(m1_ols)[1:12], col="red", lwd = 2)

lines(maecoruja$tempo[13:22], fitted(m1_ols)[13:22], col="red", lwd = 2)

segments(1, m1_ols$coefficients[1] + m1_ols$coefficients[2], 22, 
         m1_ols$coefficients[1] +  m1_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")

# modelo 2


m2_ols <- lm(taxa_mort_nasc_vivos_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m2_ols)
pred_ols2<-fitted(m2_ols)
pred_ols2


# grafico modelo2

plot(maecoruja$tempo,maecoruja$taxa_mort_nasc_vivos_pe,
     ylab="Taxa Mortalidade (nascidos vivos) ",
     ylim=c(0,50),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:12], fitted(m2_ols)[1:12], col="red", lwd = 2)

lines(maecoruja$tempo[13:22], fitted(m2_ols)[13:22], col="red", lwd = 2)

segments(1, m2_ols$coefficients[1] + m2_ols$coefficients[2], 22, 
         m2_ols$coefficients[1] +  m2_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")



# modelo 3

m3_ols <- lm(taxa_mort_pop_br~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m3_ols)
pred_ols3<-fitted(m3_ols)
pred_ols3


# grafico modelo3

plot(maecoruja$tempo,maecoruja$taxa_mort_pop_br,
     ylab="Taxa Mortalidade (pop) ",
     ylim=c(0,1),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:12], fitted(m3_ols)[1:12], col="red", lwd = 2)

lines(maecoruja$tempo[13:22], fitted(m3_ols)[13:22], col="red", lwd = 2)

segments(1, m3_ols$coefficients[1] + m3_ols$coefficients[2], 22, 
         m3_ols$coefficients[1] +  m3_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")


# modelo 4

m4_ols <- lm(taxa_morte_nasc_vivos_br~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m4_ols)
pred_ols4<-fitted(m4_ols)
pred_ols4

# grafico modelo4

plot(maecoruja$tempo,maecoruja$taxa_morte_nasc_vivos_br,
     ylab="Taxa Mortalidade (nascidos vivos) ",
     ylim=c(0,50),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:12], fitted(m4_ols)[1:12], col="red", lwd = 2)

lines(maecoruja$tempo[13:22], fitted(m4_ols)[13:22], col="red", lwd = 2)

segments(1, m4_ols$coefficients[1] + m4_ols$coefficients[2], 22, 
         m4_ols$coefficients[1] +  m4_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")


# teste de autocorrelação (Durbin-Watson)


dwt (m1_ols,max.lag=12, alternative="two.sided")

dwt (m2_ols,max.lag=12, alternative="two.sided")

dwt (m3_ols,max.lag=12, alternative="two.sided")

dwt (m4_ols,max.lag=12, alternative="two.sided")

# Gráficos dos resíduos


plot(fitted(m1_ols), residuals(m1_ols))

plot(fitted(m2_ols), residuals(m2_ols))

plot(fitted(m3_ols), residuals(m3_ols))

plot(fitted(m4_ols), residuals(m4_ols))

plot(its$time, its$Predito_n_cvli_df_datasus_Modelo_1)


# Modelos de regressão (ols)

m5_ols <- lm(taxa_obitos_mat_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m5_ols)
pred_ols5<-fitted(m5_ols)
pred_ols5

# grafico modelo5

plot(maecoruja$tempo,maecoruja$taxa_obitos_mat_pe,
     ylab="Taxa Mortalidade Materna",
     ylim=c(0,1),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m5_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m5_ols)[12:22], col="red", lwd = 2)

segments(1, m5_ols$coefficients[1] + m5_ols$coefficients[2], 22, 
         m5_ols$coefficients[1] +  m5_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")


# m6

m6_ols <- lm(taxa_obitos_mat_br~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m6_ols)
pred_ols6<-fitted(m6_ols)
pred_ols6

# grafico modelo6

plot(maecoruja$tempo,maecoruja$taxa_obitos_mat_br,
     ylab="Taxa Mortalidade Materna",
     ylim=c(0,1),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m6_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m6_ols)[12:22], col="red", lwd = 2)

segments(1, m6_ols$coefficients[1] + m6_ols$coefficients[2], 22, 
         m6_ols$coefficients[1] +  m6_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")


# m7

m7_ols <- lm(taxa_obitos06_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m7_ols)
pred_ols7<-fitted(m7_ols)
pred_ols7

# grafico modelo7

plot(maecoruja$tempo,maecoruja$taxa_obitos06_pe,
     ylab="Taxa Mortalidade Infantil",
     ylim=c(0,20),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m7_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m7_ols)[12:22], col="red", lwd = 2)

segments(1, m7_ols$coefficients[1] + m7_ols$coefficients[2], 22, 
         m7_ols$coefficients[1] +  m7_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")

# m7

m7_ols <- lm(taxa_obitos06_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m7_ols)
pred_ols7<-fitted(m7_ols)
pred_ols7

# grafico modelo7

plot(maecoruja$tempo,maecoruja$taxa_obitos06_pe,
     ylab="Taxa Mortalidade Infantil",
     ylim=c(0,20),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m7_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m7_ols)[12:22], col="red", lwd = 2)

segments(1, m7_ols$coefficients[1] + m7_ols$coefficients[2], 22, 
         m7_ols$coefficients[1] +  m7_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")

# m8

m8_ols <- lm(taxa_obitos727_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m8_ols)
pred_ols8<-fitted(m8_ols)
pred_ols8

# grafico modelo8

plot(maecoruja$tempo,maecoruja$taxa_obitos727_pe,
     ylab="Taxa Mortalidade Infantil",
     ylim=c(0,10),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m8_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m8_ols)[12:22], col="red", lwd = 2)

segments(1, m8_ols$coefficients[1] + m8_ols$coefficients[2], 22, 
         m8_ols$coefficients[1] +  m8_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")

# m9

m9_ols <- lm(taxa_obitos28364_pe~ tempo + nivel + tendencia,
             data=maecoruja)

summary(m9_ols)
pred_ols9<-fitted(m9_ols)
pred_ols9

# grafico modelo8

plot(maecoruja$tempo,maecoruja$taxa_obitos28364_pe,
     ylab="Taxa Mortalidade Infantil",
     ylim=c(0,20),
     xlab="",
     pch=20,
     col="pink",
     xaxt="n")

# Add x-axis year labels
axis(1, at=1:22, labels=maecoruja$year)


# Label the policy change
abline(v=12,lty=2)

# plot the first line segment

lines(maecoruja$tempo[1:11], fitted(m9_ols)[1:11], col="red", lwd = 2)

lines(maecoruja$tempo[12:22], fitted(m9_ols)[12:22], col="red", lwd = 2)

segments(1, m9_ols$coefficients[1] + m9_ols$coefficients[2], 22, 
         m9_ols$coefficients[1] +  m9_ols$coefficients[2]*22, 
         lty = 2, lwd = 2, col = "red")

