library(tidyverse)
library(modelr)


pol <- read_csv("Polizze.csv", na = "MISS") %>%
  rename(costo=costo_plafonato)



#dataset con chi ha provocato uno o più incidenti
#la classe bad è per chi ha provocato danni e good chi li ha subiti
sinis <- pol %>%
  filter(num_sinistri != 0 & costo > 0)


#__________________________________________________________________________________

#fai un introduzione dove spieghi la funzione attuariale e il perchè bisogna diversificare il rischio
#spiega lo scopo della data challenge: trovre strutture nei dati che diano idea di come diversificare la valutazione rischio

#poi inizia parlando della frequnza sinistri, falla vedere e fai un primo esempio di considerazione diversificata:
#seppur per legge non si può, vediamo la differenza in base al sesso:
sfreq <- aggregate(pol[, 7], list(sesso=pol$sesso), mean) %>%
  rename(freq_sinistri=num_sinistri)
#falla vedere con uno knitr figo su Markdown, vedi ?knitr::kable
View(sfreq)
#_______________________________________________________________________________

#ora inizia con quelle sensate:

#vediamo in quale regione si concentrano maggiormente i sinistri della compagnia

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
nrow(filter(pol, regione == "Basi")) #ne ho 16
#in effetti sì, la prob di sinistro:
mean(pol$num_sinistri)


#togliamo allora la basilicata:
rfreq<-rfreq[-2,]

#ora, quali sono le regioni migliori e quali peggiori?
#ecco un grafico con cui è più chiaro
ggplot(rfreq, aes(x = freq_sinistri, y = regione)) +
  geom_point(size=2) +
  geom_point(data=filter(rfreq, freq_sinistri<=0.06),color="blue",size=3) +
  geom_point(data=filter(rfreq, freq_sinistri>=0.1),color="red",size=3) +
  geom_text(aes(label=regione), alpha=0.3)
  labs(
    title = "Probabilità di sinistro per regione italiana",
    subtitle= "Quali regioni sono le 'peggiori' e quali le 'migliori'",
    x = "Probabilità di sinistro",      
    y = "Regioni") +

#questa differenza varrà anche a livelo provinciale? Vediamolo solo per le regioni più critiche
#(questo poi sarà per dire che lo si fa a livello provinciale, ma farlo con tutte le prov era troppo messy)

#per ordinare le provincie nel grafico 
rank=tibble(x=c("Cala","Vene", "Emil","Sard","Ligu","Camp","Moli"),y=1:7)

#estraiamo la prob sinistro per le provincie delle regioni blu e rosse
pfreq<- aggregate(pol[, 7], list(PV_targa=pol$PV_targa), mean) %>%
  rename(prov=PV_targa) %>%
  rename(freq_sinistri=num_sinistri) %>%
  mutate(Regione=pol$regione[match(prov, pol$PV_targa)]) %>%
  filter(Regione=="Cala"|Regione=="Vene"|Regione=="Emil"|
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
    values = c(Cala="#330066", Vene="#3333FF", Emil="#3399CC", 
               Sard="#FF6666", Ligu="#FF3333", Camp="#FF0033", Moli="#990000")) +
  labs(
    title = "Probabilità di sinistro nelle provincie",
    subtitle= "Le 'migliori' regioni (in blu) e 'peggiori' (in rosso) a livello provinciale",
    x = "Probabilità di sinistro",      
    y = "Provincie") 

#a isernia ogni anno si ha quasi il 20% di prob di avere un sinistro! 
#in calabria molta variabilità, ma Reggio cal solo 2.5%

#___________________________________________________________________________________

#frequenze per età
efreq <- aggregate(pol[, 7], list(eta=pol$eta), mean) %>%
  rename(freq_sinistri=num_sinistri)

#con frequenza
ggplot(efreq, aes(eta, freq_sinistri)) +
  geom_line(color="blue", size=1) +
  labs(
    title = "Probabilità di sinistro",
    subtitle = "Trend in base all'aumento dell'età del guidatore", 
    x = "Età",
    y = "Probabilità di sinistro",
    fill = "Probabilità di sinistro") +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  geom_hline(aes(yintercept=mean(pol$num_sinistri)), linetype=2, alpha=0.5)

#il trend mostra una graduale diminuzione della robabilità al crescere dell'età
#a parte una piccola risalita nelle età più avanzate

#___________________________________________________________________________________

#con la potenza
ggplot(pol, aes(HP_fiscali))+
  geom_bar() +
  coord_cartesian(ylim=c(0,200)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) 
#togliamo <6 e >33

#frequenze per potenza del veicolo
hfreq <- aggregate(pol[, 7], list(HP_fiscali=pol$HP_fiscali), mean) %>%
  rename(freq_sinistri=num_sinistri) %>%
  filter(HP_fiscali > 5 & HP_fiscali < 34)


ggplot(hfreq, aes(HP_fiscali, freq_sinistri)) +
  geom_line(color="#FF6600", size=1) +
  labs(
    title = "Probabilità di sinistro",
    subtitle = "Trend in base all'aumento di cilindrata dei veicoli", 
    x = "Cilindrata / 100",
    y = "Probabilità di sinistro",
    fill = "Probabilità di sinistro") +
  geom_hline(aes(yintercept=mean(pol$num_sinistri)), linetype=2, alpha=0.5)

#dai veicoli meno potenti ai veicoli fino a 2500 di cilindrata il trend va salendo
#nei veicoli più potenti (oltre i 3000) sembra scendere, c'è poi un picco in quelli potentissimi (circa 4000)
#___________________________________________________________________________________

#con l'età del veicolo
ggplot(pol, aes(anni_auto))+
  geom_bar() +
  coord_cartesian(ylim=c(0,200)) +
  scale_x_continuous(breaks = seq(0, 50, by = 1)) 
#togliamo la coda sopra i 27


#frequenze per età del veicolo
vfreq <- aggregate(pol[, 7], list(anni_auto=pol$anni_auto), mean) %>%
  rename(freq_sinistri=num_sinistri) %>%
  filter(anni_auto >= 0 & anni_auto < 28)


ggplot(vfreq, aes(anni_auto, freq_sinistri)) +
  geom_line(color="#663366", size=1) +
  labs(
    title = "Probabilità di sinistro",
    subtitle = "Trend in base all'aumento dell' età del veicolo", 
    x = "Anni dall'immatricolazione",
    y = "Probabilità di sinistro",
    fill = "Probabilità di sinistro") +  
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +
  geom_hline(aes(yintercept=mean(pol$num_sinistri)), linetype=2, alpha=0.5)

#aparte nei veicoli più vecchi dove c'è variabilità, probabilmente legata allo scarso numero di osservazioni,
#il trend sembra dire che la probabilità cali all'aumentare dell'anzianità del veicolo

#______________________________________________________________________________________


#frequenze per tipo di alimntazione
afreq <- aggregate(pol[, 7], list(alimentazione=pol$alimentazione), mean) %>%
  rename(freq_sinistri=num_sinistri)

ggplot(afreq, aes(alimentazione, freq_sinistri, fill=freq_sinistri)) +
  geom_col() +
  coord_polar() +
  theme_minimal() +
  scale_fill_gradient(low="white", high="#009900", limits=c(0,0.25)) +
  scale_y_continuous(breaks = seq(2, 10, by = 1)) +
  labs(
    title = "Probabilità di sinistro per tipologia di alimentazione del veicolo",
    x = "",      
    y = "",
    fill = "Probabilità di sinistro")

#anche in questo caso si evidenzia una diversa probabilità in base all'alimentazione
#alcune tipologie la hanno molto alta e altre molto bassa

#___________________________________________________________________________________
#Iniziamo ora a considerare il costo dei sinistri
#_______________________________________________________________________________________


#vediamo i boxplot per le regioni

#mettiamoli in ordine dalla regione con più frequenza di sinistri a quella con meno

sinis2 = sinis %>%
  filter(regione != "NA")

ggplot(sinis2, aes(regione, costo)) +
  geom_boxplot(color="#993300", fill="#FFFFCC") +
  coord_cartesian(ylim=c(0,15000)) +
  labs(
    title = "Ammontare del costo dei sinistri nelle regioni italiane",
    x = "Regioni",
    y = "Costo del sinistro") 


#il costo si mantiene pressochè uguale nelle varie regioni
#in piemonte si registrano al primo quartile costi un poò più bassi
#in calabria grande variabilità
#avevamo osservato già una variabilità tra la prob di sinistro nelle regioni calabresi



#____________________________________________________________________________________

#vedi se fare la cosa dei ested e variare qualche .. vs .. per l'età

#__________________________________________________________________________________