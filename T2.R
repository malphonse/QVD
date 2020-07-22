##Graphing Lux of Lac, Xen20 and Xen41

library(tidyverse)

##-------------/-----Lum-lac--------/------

##Data table
Lum_lac

##Average values:
LL <- Lum_lac %>%
  mutate(minQVD = c(`LAC-QVD` + `LAC-QVD_1` + `LAC-QVD_2`) / 3) %>%
  mutate(`10QVD`= c(`LAC+10` + `LAC+10_1` + `LAC+10_2`) / 3) %>%
  mutate(`100QVD` = c(`LAC+100` + `LAC+100_1` + `LAC+100_2`) / 3) %>%
  mutate(`20mgQVD` = c(`LAC+ 20mg` + `LAC+ 20mg_1` + `LAC+ 20mg_2`) / 3)

##Select variables to plot
LL_data <- LL %>%
  select(Time, minQVD, `10QVD`, `100QVD`, `20mgQVD`)

LL_data

##Pivot the table to graph

LL_graph <- LL_data %>%
  pivot_longer(c(minQVD, `10QVD`, `100QVD`, `20mgQVD`), 
               names_to = "Tx", values_to = "Lum")

LL_graph

##Graphing the curve

Lacfig1B <- ggplot(LL_graph, aes(Time, Lum, color = Tx)) +
  scale_y_continuous(limits = c(0, 500))

figure1B <- Lacfig1B + geom_smooth(method = NULL, size = 2, span = 0.5, se = TRUE) +
  geom_rug(color = "grey50") +
  theme_minimal()

figure1B + theme(legend.position = "top")


##-------------//-----Lum-Xen20--------//------

##Data table
Lum_Xen20

##Average values:
LX2 <- Lum_Xen20 %>%
  mutate(minQVD = c( `Xen20-QVD`+ `Xen20-QVD_1` + `Xen20-QVD_2`) / 3) %>%
  mutate(`10QVD`= c(`Xen20+10` + `Xen20+10_1` + `Xen20+10_2`) / 3) %>%
  mutate(`100QVD` = c(`Xen20+100` + `Xen20+100_1` + `Xen20+100_2`) / 3) %>%
  mutate(`20mgQVD` = c(`Xen20+20mg` + `Xen20+20mg_1` + `Xen20+20mg_2`) / 3)

##Select variables to plot
LX2_data <- LX2 %>%
  select(Time, minQVD, `10QVD`, `100QVD`, `20mgQVD`)

LX2_data

##Pivot the table to graph

LX2_graph <- LX2_data %>%
  pivot_longer(c(minQVD, `10QVD`, `100QVD`, `20mgQVD`), 
               names_to = "Tx", values_to = "Lum")

LX2_graph

##Graphing the curve

Xenfig1D <- ggplot(LX2_graph, aes(Time, Lum, color = Tx))+
  scale_y_continuous(limits = c(0, 500))


figure1D <- Xenfig1D + geom_smooth(method = NULL, size = 2, span = 0.5, se = TRUE) +
  geom_rug(color = "grey50") +
  theme_minimal()

figure1D + theme(legend.position = "top")

##-------------///-----Lum-Xen41-------///------

##Data table
Lum_Xen41

##Average values:
LX4 <- Lum_Xen41 %>%
  mutate(minQVD = c( `Xen41-QVD` + `Xen41-QVD_1` + `Xen41-QVD_2`) / 3) %>%
  mutate(`10QVD`= c(`Xen41+10` + `Xen41+10_1` + `Xen41+10_2`) / 3) %>%
  mutate(`100QVD` = c(`Xen41+100` + `Xen41+100_1` + `Xen41+100_2`) / 3) %>%
  mutate(`20mgQVD` = c(`Xen41+20mg` + `Xen41+20mg_1` + `Xen41+20mg_2`) / 3)

##Select variables to plot
LX4_data <- LX4 %>%
  select(Time, minQVD, `10QVD`, `100QVD`, `20mgQVD`)

LX4_data

##Pivot the table to graph

LX4_graph <- LX4_data %>%
  pivot_longer(c(minQVD, `10QVD`, `100QVD`, `20mgQVD`), 
               names_to = "Tx", values_to = "Lum")

LX4_graph

##Graphing the curve

Xenfig1G <- ggplot(LX4_graph, aes(Time, Lum, color = Tx))


figure1G <- Xenfig1G + geom_smooth(method = NULL, size = 2, span = 0.5, se = TRUE) +
  geom_rug(color = "grey50") +
  theme_minimal()

figure1G + theme(legend.position = "top")











