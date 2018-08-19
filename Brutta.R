library(tidyverse)
library(modelr)

pol <- read_csv("Polizze.csv")
#se riesci imposta come NA i MISS

#dataset con chi ha provocato uno o più incidenti recando danni alla compagnia
bad <- pol %>%
  filter(num_sinistri != 0 & costo_plafonato > 0)



#_______________________________________________________________________________

#vediamo in qualu regione si concentrano maggiormente i sinistri della compagnia

#dataframe con frequenza sinistri per regione
rfreq <- aggregate(pol[, 7], list(regione=pol$regione), mean) %>%
  rename(freq_sinistri=num_sinistri)
#togliamo basilicata che ha freq 0 
#fai vedere prob sinistro e oss per basilicata per giustivicare che la hai tolta
rfreq<-rfreq[-2,]
#quantità di sinistri per regione
ggplot(data = rfreq) +
  geom_point(mapping = aes(x = freq_sinistri, y = regione))
#aggiusta colori e cose estetiche

#fai in blu i puntini con le migliori e in rosso le peggiori
#poi fai il grafico per provincia (rifai aggregate o fai un data con regioni e prov e join)
#solo delle blu e rosse, dove metti le provincie delle blu in blu e delle rosse in rosso


#fai la stessa cosa con sesso (sfreq), eta (efreq) ,alimentazione (afreq)

#___________________________________________________________________________________

#vedi se fare la cosa dei ested e variare qualche .. vs .. per l'età

#__________________________________________________________________________________