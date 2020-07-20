##Preparing to graph Growth curve
##Open Library
library(tidyverse)
library(RColorBrewer)

display.brewer.all()

OD600_LAC

##Select columns to plot----
 
plot <- OD600_LAC %>%
  select(Time, minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`)

##View plot
plot

##Wrangle (Pivot) to make 2 variable to plot

graph <- plot %>%
pivot_longer(c(minQVD, `10ug/mL QVD`, `100ug/ml QVD`, `20mg/kg QVD`), 
             names_to = "condition", values_to = "reading"
             )
##View graph

graph

#graph with ggplot

a <- ggplot(graph, aes(x = Time, y = reading, color = condition))

Figure1 <- a + geom_smooth() +
  facet_wrap(~condition, nrow = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

Figure1A <- a + geom_smooth(method = "loess", size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View Figure
Figure1
Figure1A + theme(legend.position = "top")


##Using mutate to calculate mean----

##Calculate mean and add new column with values 
av <- OD600_LAC %>%
  mutate(minusQVD = c(`LAC::lux - QVDOPH` + `LAC::lux - QVDOPH_1` +`LAC::lux - QVDOPH_2`)/ 3 ) %>%
  mutate(`10ugQVD` = c(`LAC::lux +10ug/mL QVDOPH` + `LAC::lux +10ug/mL QVDOPH_1` + `LAC::lux +10ug/mL QVDOPH_2`)/3 ) %>%
  mutate(`100ugQVD` = c(`LAC::lux +100ug/mL QVDOPH` + `LAC::lux +100ug/mL QVDOPH_1` + `LAC::lux +100ug/mL QVDOPH_2`)/3) %>%
  mutate(`20mgQVD` = c(`LAC::lux + 20mg/kg (35gms) QVDOPH` + `LAC::lux + 20mg/kg (35gms) QVDOPH_1` + `LAC::lux + 20mg/kg (35gms) QVDOPH_2`)/3)

av

##Preparing a sub-table for plotting graph
av_graph <- av %>%
  select(`Time`, minusQVD, `10ugQVD`, `100ugQVD`, `20mgQVD`)

##Pivot the coloumns for graph
renderav_graph <- av_graph %>%
  pivot_longer(c(minusQVD, `10ugQVD`, `100ugQVD`, `20mgQVD`), 
               names_to = "condition", values_to = "reading")

##Graphing
lacfig <- ggplot(renderav_graph, aes(x = Time, y = reading, color = condition))


FigureA1 <- lacfig + geom_smooth(method = "loess", size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View graph
Figure1A + theme(legend.position = "top")



##---------------$----$CFU data to plot------------$$------

CFU_lac

##Pivot long to graph:

CFU <- CFU_lac %>%
  pivot_longer(c(`LAC::lux - QVDOPH`, `LAC::lux +10ug/mL QVDOPH`, `LAC::lux +100ug/mL QVDOPH`, `LAC::lux + 20mg/kg (35gms) QVDOPH`), 
               names_to = "Tx", values_to = "CFU_count")
##View CFU
CFU

##Arrange the data table by labels:
CFU %>% arrange(Tx)
 
##Create graph with x and y axis organized:
CFU_graph <- ggplot(CFU, aes(Tx, CFU_count, fill = Tx)) +
  scale_x_discrete(limits = c("LAC::lux - QVDOPH",
  "LAC::lux +10ug/mL QVDOPH",
  "LAC::lux +100ug/mL QVDOPH", 
  "LAC::lux + 20mg/kg (35gms) QVDOPH")) +
  scale_y_continuous(limits = c(0,1.5e+09))

##Graphing the table
Fig1B <- CFU_graph + geom_violin(size = 0.25, trim = "FALSE", alpha=0.5, show.legend = FALSE) + 
  geom_rug(color = "grey50") +
  geom_point(shape = 21, size = 7, position = "jitter") +
  theme_minimal()

Fig1Bb <- Fig1B + geom_boxplot(data = NULL, width=0.1)

##View figure drawn
Fig1Bb + theme(legend.position = "none")









##---------------$$--------------Xen20--------------$$-----

## Calculate average and add new columns 

Xen20 <- OD600_Xen20 %>%
  mutate(minQVD_X2 = c(`Xen20 - QVDOPH` + `Xen20 - QVDOPH_1` + `Xen20 - QVDOPH_2`) / 3) %>%
  mutate(tenQVD_X2 = c(`Xen20 +10ug/mL QVDOPH` + `Xen20 +10ug/mL QVDOPH_1` + `Xen20 +10ug/mL QVDOPH_2`) / 3) %>%
  mutate(hundQVD_X2 = c(`Xen20 + 100ug/mL QVDOPH` + `Xen20 + 100ug/mL QVDOPH_1` + `Xen20 + 100ug/mL QVDOPH_2`) / 3) %>%
  mutate(twent_X2 = c(`Xen20 + 20mg/Kg (35gms) QVDOPH` + `Xen20 + 20mg/Kg (35gms) QVDOPH_1` + `Xen20 + 20mg/Kg (35gms) QVDOPH_2`) / 3)

Xen20  

##Preparing sub-table for plotting graph

Xen20_graph <- Xen20 %>%
  select(`Time (hrs)`, minQVD_X2, tenQVD_X2, hundQVD_X2, twent_X2)

Xen20_graph

##Pivot long for graph

ggXen20 <- Xen20_graph %>%
  pivot_longer(c(minQVD_X2, tenQVD_X2, hundQVD_X2, twent_X2), 
               names_to = "condition", values_to = "reading")

ggXen20

##Graphing
Xen20fig <- ggplot(ggXen20, aes(x = `Time (hrs)`, y = reading, color = condition)) +
  scale_y_continuous(limits = c(-0.1, 1.0))


FigureD1 <- Xen20fig + geom_smooth (size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View graph
FigureD1 + theme(legend.position = "top")



##---------------$$--------------Xen41--------------$$-----

## Calculate average and add new columns 

Xen41 <- OD600_Xen41 %>%
  mutate(minQVDX4 = c(`Xen41 - QVDOPH` + `Xen41 - QVDOPH_1`+ `Xen41 - QVDOPH_2`) / 3) %>%
  mutate(tenQVDX4 = c(`Xen41 +10ug/mL QVDOPH`+ `Xen41 +10ug/mL QVDOPH_1`+ `Xen41 +10ug/mL QVDOPH_2`) / 3) %>%
  mutate(hundQVDX4 = c(`Xen41 +100ug/mL QVDOPH`+ `Xen41 +100ug/mL QVDOPH_1` + `Xen41 +100ug/mL QVDOPH_2`) / 3) %>%
  mutate(twentX4 = c(`Xen41 + 20mg/kg (35gms) QVDOPH` + `Xen41 + 20mg/kg (35gms) QVDOPH_1`+ `Xen41 + 20mg/kg (35gms) QVDOPH_2`) / 3)
 

Xen41  

##Preparing sub-table for plotting graph

Xen41_graph <- Xen41 %>%
  select(`Time (hrs)`, minQVDX4, tenQVDX4, hundQVDX4, twentX4)

Xen41_graph

##Pivot long for graph

ggXen41 <- Xen41_graph %>%
  pivot_longer(c(minQVDX4, tenQVDX4, hundQVDX4, twentX4), 
               names_to = "condition", values_to = "reading")


##Graphing
Xen41fig <- ggplot(ggXen41, aes(x = `Time (hrs)`, y = reading, color = condition))


FigureF1 <- Xen41fig + geom_smooth (size = 2) +
  geom_rug(color = "grey50") +
  theme_minimal()

##View graph
FigureF1 + theme(legend.position = "top")



















