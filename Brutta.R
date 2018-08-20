library(tidyverse)
library(modelr)


pol <- read_csv("Polizze.csv", na = "MISS")
#se riesci imposta come NA i MISS

#dataset con chi ha provocato uno o più incidenti recando danni alla compagnia
bad <- pol %>%
  filter(num_sinistri != 0 & costo_plafonato > 0)



#_______________________________________________________________________________

#vediamo in qualu regione si concentrano maggiormente i sinistri della compagnia

#dataframe con frequenza sinistri per regione
rfreq <- aggregate(pol[, 7], list(regione=pol$regione), mean) %>%
  rename(freq_sinistri=num_sinistri)

#grafico frequenze
ggplot(rfreq,aes(regione,freq_sinistri,fill=freq_sinistri)) +
  geom_col() +
  labs(
    title = "Probabilità di sinistro per regione italiana",
    x = "Regioni",      
    y = "Probabilità di sinistro",
    fill = "Probabilità di sinistro") +
  scale_y_continuous(breaks = seq(0, 0.12, by = 0.02)) +
  theme_linedraw() +
  theme_grey()

#come mai Basilicata ha frequenza 0? Sarà che ho poche obs?
nrow(filter(pol, regione == "Basi"))/nrow(pol)
#in effetti sì, meno della prob di sinistro:
mean(pol$num_sinistri)

#togliamo allora la basilicata:
rfreq<-rfreq[-2,]

#ora, quali sono le regioni migliori e quali peggiori?
#ecco un grafico con cui è più chiaro
ggplot(rfreq, aes(x = freq_sinistri, y = regione)) +
  geom_point(size=2) +
  geom_point(data=filter(rfreq, freq_sinistri<=0.065),color="blue",size=3) +
  geom_point(data=filter(rfreq, freq_sinistri>=0.1),color="red",size=3) +
  labs(
    title = "Probabilità di sinistro per regione italiana",
    subtitle= "Quali regioni sono le 'peggiori' e quali le 'migliori'",
    x = "Probabilità di sinistro",      
    y = "Regioni")

#questa differenza varrà anche a livelo provinciale? Vediamolo solo per le regioni più critiche
#(questo poi sarà per dire che lo si fa a livello provinciale, ma farlo con tutte le prov era troppo messy)

#per ordinare le provincie nel grafico 
rank=tibble(x=c("Emil","Vall","Lomb","Friu","Marc","Sard","Ligu","Camp","Moli"),y=1:9)

#estraiamo la prob sinistro per le provincie delle regioni blu e rosse
pfreq<- aggregate(pol[, 7], list(PV_targa=pol$PV_targa), mean) %>%
  rename(prov=PV_targa) %>%
  rename(freq_sinistri=num_sinistri) %>%
  mutate(Regione=pol$regione[match(prov, pol$PV_targa)]) %>%
  filter(Regione=="Emil"|Regione=="Friu"|Regione=="Lomb"|Regione=="Marc"|Regione=="Vall"|
           Regione=="Sard"|Regione=="Moli"|Regione=="Ligu"|Regione=="Camp") %>%
  mutate(Rank=rank$y[match(Regione, rank$x)]) %>%
  filter(freq_sinistri != 0) %>%
  arrange(Rank)
  
#per mantenere l'ordine delle provincie ottenuto sopra nel grafico
pfreq$Provincia <- factor(pfreq$prov, as.character(pfreq$prov))
  

#grafico delle provincie d'interesse, varrà la differenza anche a liv provinciale?
ggplot(pfreq, aes(x = freq_sinistri, y = Provincia, color=Regione, order = Rank)) +
  geom_point(size=2)+
  scale_colour_manual(
    values = c(Emil="#000066", Vall="#0000CC", Lomb="#3333FF", Friu="#3399CC", Marc="#66CCFF", 
               Sard="#FF3333", Ligu="#FF0033", Camp="#CC0000", Moli="#990000")) +
  labs(
    title = "Probabilità di sinistro nelle provincie",
    subtitle= "Le 'migliori' regioni (in blu) e 'peggiori' (in rosso) a livello provinciale",
    x = "Probabilità di sinistro",      
    y = "Provincie") 

#a isernia ogni anno si ha quasi il 2% di prob di avere un sinistro! 



#fai la stessa cosa con sesso (sfreq), eta (efreq) ,alimentazione (afreq)

#___________________________________________________________________________________

#vedi se fare la cosa dei ested e variare qualche .. vs .. per l'età

#__________________________________________________________________________________