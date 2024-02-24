####Setting enviromnent####
library(Benchmarking)
library(car)
library(readxl)
library(writexl)
library(dplyr)
library(zoo)
ape <- read_xlsx("/home/caleb/Documents/FACULDADE/Semestres/Periodo_6/optimization/eda_project/TabuModelo 1.xlsx")
ape <- ape %>% select(1:12) 
attach(ape)
####Input####
ape[Ano == "2018" & Mês == "1", which(colnames(ape) == "Oficiais...4")] <- ape[Ano == "2018" & Mês == "2", which(colnames(ape) == "Oficiais...4")] 
ape[Ano == "2018" & Mês == "1", which(colnames(ape) == "Praça...5")] <- ape[Ano == "2018" & Mês == "2", which(colnames(ape) == "Praça...5")]
ran <- range(ape$AIS...1)
ran <- (ran[1]:ran[2])
ran #vetor com as AIS
a <- ape %>% filter(Ano == "2018")
for (i in ran) {
  new_var_name <- paste0("a", i)
  new_var_value <- a %>% filter(AIS...1 == i)
  new_var_value[new_var_value$Viaturas...6 == 0, which(colnames(new_var_value) == "Viaturas...6")] <- NA
  new_var_value <- ts(new_var_value,start=c(2018,1), frequency = 12) 
  new_var_value <- na.locf(new_var_value, fromLast = TRUE)
  new_var_value <- as_tibble(new_var_value)
  
  assign(new_var_name, new_var_value)
} #break into each AIS of 2018 and apply na.locf
ape2018 <- tibble() #empty dataframe
for (i in ran){ 
  nameof <- paste0("a", i)
  ape2018 <- bind_rows(ape2018, get(nameof))
  rm(list = nameof, envir = base::environment())
} #bind all AIS into one dataframe of 2018
ape[Ano == "2018",] <- ape2018 # coloca de volta em ape
which(ape$Oficiais...4 == 0) #Checking for zeros in the input
which(ape$Praça...5 == 0) #...
which(ape$Viaturas...6 == 0) #...
x1 <- (ape$Oficiais...4) #inputs
x2 <- (ape$Praça...5) #...
x3 <- (ape$Viaturas...6) #...
x <- matrix(c(x1,x2,x3), ncol =  3) #input matrix
x
####Output####
#Efficiency Output Matrix eliminating zero values by adding 0.1
y1 <- (ape$`Cumprimento de mandato...7` + 0.1)         
y2 <- (ape$CVLI...8 + 0.1)
y3 <- (ape$CVP...9 + 0.1)         
y4 <- (ape$`Porte ilegal de armas...10` + 0.1)
y5 <- (ape$`Tráfico de drogas...11` + 0.1)
y  <- matrix(c(y1,y2,y3,y4,y5), ncol = 5)   
y
####Efficiency(Whole Year)####
E <- dea(x,y, XREF = x, YREF = y, ORIENTATION = 'out', RTS = 'vrs', SLACK = FALSE)
escore <- 1/E$eff #deixa as eficiências entre 0 e 1
escore <- round(escore, digits = 5) 
Eff_Results <- data.frame(Farrell = E$eff, escore, x, y, AIS...1, Ano, Mês)
which(Eff_Results$escore == 1) #indica quais tem eficiência 1
count(Eff_Results[escore == 1,]) #indica quantos tem eficiência 1
dea.plot(x,y, RTS = "vrs", txt = TRUE)
####Efficiency(Each Year)####
anos <- c("2018","2019","2020","2021")
for (i in anos) {
new_var_name <- paste0("ape", i)
x_new_name <- paste0("x", i)
y_new_name <- paste0("y", i)
new_var_value <- ape %>% filter(Ano == i)
x1 <- (new_var_value$Oficiais...4)
x2 <- (new_var_value$Praça...5)
x3 <- (new_var_value$Viaturas...6)
y1 <- (new_var_value$`Cumprimento de mandato...7` + 0.1)         
y2 <- (new_var_value$CVLI...8 + 0.1)
y3 <- (new_var_value$CVP...9 + 0.1)         
y4 <- (new_var_value$`Porte ilegal de armas...10` + 0.1)
y5 <- (new_var_value$`Tráfico de drogas...11` + 0.1)
  
x_new_value  <- matrix(c(x1,x2,x3), ncol =  3) #Efficiency input Matrix
y_new_value  <- matrix(c(y1,y2,y3,y4,y5), ncol = 5) #Efficiency Output Matrix

assign(new_var_name, new_var_value)
assign(x_new_name, x_new_value)
assign(y_new_name, y_new_value)
}
E2018 <- dea(x2018,y2018, XREF = x, YREF = y, ORIENTATION = 'out', RTS = 'vrs', SLACK = FALSE)
E2019 <- dea(x2019,y2019, XREF = x, YREF = y, ORIENTATION = 'out', RTS = 'vrs', SLACK = FALSE)
E2020 <- dea(x2020,y2020, XREF = x, YREF = y, ORIENTATION = 'out', RTS = 'vrs', SLACK = FALSE)
E2021 <- dea(x2021,y2021, XREF = x, YREF = y, ORIENTATION = 'out', RTS = 'vrs', SLACK = FALSE)
escore2018 <- round(1/E2018$eff, digits = 5)
escore2019 <- round(1/E2019$eff, digits = 5)
escore2020 <- round(1/E2020$eff, digits = 5)
escore2021 <- round(1/E2021$eff, digits = 5)
Eff_Results_2018 <- data.frame(Farrell=E2018$eff, escore, x2018, y2018, ape2018$AIS...1, ape2018$Ano, ape2018$Mês)
Eff_Results_2019 <- data.frame(Farrell=E2019$eff, escore, x2019, y2019, ape2019$AIS...1, ape2019$Ano, ape2019$Mês)
Eff_Results_2020 <- data.frame(Farrell=E2020$eff, escore, x2020, y2020, ape2020$AIS...1, ape2020$Ano, ape2020$Mês)
Eff_Results_2021 <- data.frame(Farrell=E2021$eff, escore, x2021, y2021, ape2021$AIS...1, ape2021$Ano, ape2021$Mês)
dea.plot(x2018,y2018, RTS = "vrs", txt = TRUE)
dea.plot(x2019,y2019, RTS = "vrs", txt = TRUE)
dea.plot(x2020,y2020, RTS = "vrs", txt = TRUE)
dea.plot(x2021,y2021, RTS = "vrs", txt = TRUE)
####  1. Ranges de escore de eficiência em porcentagem (OK!) ####
count(as.data.frame(escore[escore < 0.25]))/count(as.data.frame(escore)) * 100
count(as.data.frame(escore[0.25 <= escore & escore < 0.50]))/count(as.data.frame(escore)) * 100
count(as.data.frame(escore[0.50 <= escore & escore < 0.75]))/count(as.data.frame(escore)) * 100
count(as.data.frame(escore[0.75 <= escore & escore < 1]))/count(as.data.frame(escore)) * 100
count(as.data.frame(escore[escore == 1]))/count(as.data.frame(escore)) * 100
which(escore == 1) #indica quais linhas tem eficiência 1
Eff_Results <- data.frame(Farrell = E$eff, escore, x, y, AIS...1, Ano, Mês)
count(Eff_Results[escore == 1,])
Eff_Results[escore <= quantile(escore, 0.05),]
Eff_Results[escore >= quantile(escore, 0.90),]
####  2. AIS EFICIENTES (CADA ANO) OK! 3.AIS MAIS INEFICIENTES(CADA ANO) (OK!)####
Eff_Results_2018 <- Eff_Results %>% filter(Ano == "2018")
Eff_Results_2019 <- Eff_Results %>% filter(Ano == "2019")
Eff_Results_2020 <- Eff_Results %>% filter(Ano == "2020")
Eff_Results_2021 <- Eff_Results %>% filter(Ano == "2021")
count(Eff_Results_2018[Eff_Results_2018$escore == 1,])
count(Eff_Results_2019[Eff_Results_2019$escore == 1,])
count(Eff_Results_2020[Eff_Results_2020$escore == 1,])
count(Eff_Results_2021[Eff_Results_2021$escore == 1,])
count(Eff_Results_2018[Eff_Results_2018$escore <= quantile(Eff_Results_2018$escore, 0.10),])
count(Eff_Results_2018[Eff_Results_2018$escore >= quantile(Eff_Results_2018$escore, 0.90),])
Eff_Results_2019[Eff_Results_2019$escore <= quantile(Eff_Results_2019$escore, 0.05),]
Eff_Results_2019[Eff_Results_2019$escore >= quantile(Eff_Results_2019$escore, 0.90),]
Eff_Results_2020[Eff_Results_2020$escore <= quantile(Eff_Results_2020$escore, 0.05),]
Eff_Results_2020[Eff_Results_2020$escore >= quantile(Eff_Results_2020$escore, 0.90),]
Eff_Results_2021[Eff_Results_2021$escore <= quantile(Eff_Results_2021$escore, 0.05),]
count(Eff_Results_2021[Eff_Results_2021$escore >= quantile(Eff_Results_2021$escore, 0.90),])
#Extra : valor médio da eficiência para cada unidade
for (i in ran) {
  new_var_name <- paste0("aft2018_a", i)
  new_var_value <- Eff_Results_2018 %>% filter(AIS...1 == i)
  new_var_value <- new_var_value$escore
  new_var_value <- round(sum(new_var_value)/12, digits = 4)
  Eff_Results_aft2018_sum <- append(Eff_Results_2018_sum, new_var_value)
  assign(new_var_name, new_var_value)
}
Eff_Results_2018_sum <- vector()
####  4. ANOS COM MAIOR E MENOR VARIAÇÃO DA EFICIÊNCIA (OK!) ####
sd(Eff_Results_2018$escore) #Maior
sd(Eff_Results_2019$escore)
sd(Eff_Results_2020$escore) #Menor
sd(Eff_Results_2021$escore)
hist(Eff_Results_2018$escore)
hist(Eff_Results_2019$escore)
hist(Eff_Results_2020$escore)
hist(Eff_Results_2021$escore)
####  5. Potências de melhoria (OK!)####
pot_improv <- round((1-escore) * y, digits = 5)
pot_improv
Eff_Results <- Eff_Results %>% bind_cols(pot_improv)
View(Eff_Results)
####  6. Potênciais de melhorias totais por output por ano (OK!)####
# por output
sum(Eff_results$...14) 
sum(Eff_results$...15)
sum(Eff_results$...16)
sum(Eff_results$...17)
sum(Eff_results$...18)
# por ano
E_R_18 <- filter(Eff_Results, Ano == "2018")
pot_improv_18 <- sum(E_R_18$...14) + sum(E_R_18$...15) + sum(E_R_18$...16) + sum(E_R_18$...17) + sum(E_R_18$...18)
pot_improv_18
E_R_19 <- filter(Eff_Results, Ano == "2019")
pot_improv_19 <- sum(E_R_19$...14) + sum(E_R_19$...15) + sum(E_R_19$...16) + sum(E_R_19$...17) + sum(E_R_19$...18)
pot_improv_19
E_R_20 <- filter(Eff_Results, Ano == "2020")
pot_improv_20 <- sum(E_R_20$...14) + sum(E_R_20$...15) + sum(E_R_20$...16) + sum(E_R_20$...17) + sum(E_R_20$...18)
pot_improv_20
E_R_21 <- filter(Eff_Results, Ano == "2021")
pot_improv_21 <- sum(E_R_21$...14) + sum(E_R_21$...15) + sum(E_R_21$...16) + sum(E_R_21$...17) + sum(E_R_21$...18)
pot_improv_21
#Por ano e output
sum(E_R_18$...14) 
sum(E_R_18$...15)
sum(E_R_18$...16)
sum(E_R_18$...17)

sum(E_R_19$...18)
sum(E_R_19$...14) 
sum(E_R_19$...15)
sum(E_R_19$...16)

sum(E_R_20$...17)
sum(E_R_20$...18)
sum(E_R_20$...14) 
sum(E_R_20$...15)

sum(E_R_21$...16)
sum(E_R_21$...17)
sum(E_R_21$...18)
sum(E_R_21$...14)
####  7. Unidades Benchmarks para melhores práticas####
#Benchmark agrupado pelas unidades ineficientes do junta aos peers
#perguntar a perplexity example in R using the chull() and lines() functions to draw a convex plane under the curve
#Benchmark agrupado pelos peers das unidades ineficientes
as.character(ape$AIS...1)
as.character(ape$Ano)
as.character(ape$Mês)
vector1 <- as.character(ape$AIS...1)
vector2 <- as.character(ape$Ano)
vector3 <- as.character(ape$Mês)
combined_matrix <- cbind(vector1, vector2, vector3)
concatenated_strings <- apply(combined_matrix, 1, function(x) paste(x, collapse = "-"))
View(peers(E, NAMES = concatenated_strings))
####  8. Apresentar fronteiras (chull() e lines()?)####
dea.plot(x,y,RTS = "vrs", txt = TRUE) #plot da fronteira
####  9. Evolução da eficiência ao longo do tempo (soma de toda eficiência e comparar os anos ???/de cada unidade???) (graficamente)####
#usar ggplot2::geom_point()

#### 10. Correlação (Crime X effic.)####
# Fica pro relatório final (falta a variável crime...)

#valor médio da eficiência para cada unidade
for (i in ran) {
  new_var_name <- paste0("aft2018_a", i)
  new_var_value <- Eff_Results_aft2018 %>% filter(AIS...1 == i)
  new_var_value <- new_var_value$escore
  new_var_value <- round(sum(new_var_value)/12, digits = 4)
  Eff_Results_aft2018_sum <- append(Eff_Results_aft2018_sum, new_var_value)
  assign(new_var_name, new_var_value)
}
Eff_Results_aft2018_sum



####Exporting####
#Exporta o csv dos resultados de eficência
writexl::write_xlsx(Eff_Results, file = "/home/caleb/Documents/FACULDADE/Semestres/Periodo_6/optimization/eda_project/results_eff.csv")
writexl::write_xlsx(Eff_Results, file = "/home/caleb/Documents/FACULDADE/Semestres/Periodo_6/optimization/eda_project/results_eff.csv")
