library(tidyverse)



pol <- read_csv("Polizze.csv", na = "MISS") %>%
  rename(costo=costo_plafonato)



#dataset con chi ha provocato uno o più incidenti
sinis <- pol %>%
  filter(num_sinistri != 0 & costo > 0)



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#________________________________________________________________________________________


#__________________________________________________________________________________


#seppur per legge non si può, vediamo la differenza in base al sesso:
sfreq <- aggregate(pol[, 7], list(sesso=pol$sesso), mean) %>%
  rename(freq_sinistri=num_sinistri)
#falla vedere con uno knitr su Markdown
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
p1=ggplot(rfreq, aes(x = freq_sinistri, y = regione)) +
  geom_point(size=2) +
  geom_point(data=filter(rfreq, freq_sinistri<=0.06),color="blue",size=3) +
  geom_point(data=filter(rfreq, freq_sinistri>=0.1),color="red",size=3) +
  geom_text(aes(label=regione), alpha=0.3, hjust = 0, nudge_x = 0.001) +
  labs(
    title = "Probabilità di sinistro per regione italiana",
    subtitle= "Quali regioni sono le 'peggiori' e quali le 'migliori'",
    x = "Probabilità di sinistro",      
    y = "Regioni") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p1

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
p2=ggplot(pfreq, aes(x = freq_sinistri, y = Provincia, color=Regione, order = Rank)) +
  geom_point(size=2)+
  scale_colour_manual(
    values = c(Cala="#330066", Vene="#3333FF", Emil="#3399CC", 
               Sard="#FF6666", Ligu="#FF3333", Camp="#FF0033", Moli="#990000")) +
  geom_text(aes(label=Provincia), alpha=0.3, hjust = 0, nudge_x = 0.001) +
  labs(
    title = "Probabilità di sinistro nelle provincie",
    subtitle= "Le 'migliori' regioni (in blu) e 'peggiori' (in rosso) a livello provinciale",
    x = "Probabilità di sinistro",      
    y = "Provincie") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

multiplot(p1, p2)
#come si vede, vale anche a liv provinciale
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
    x = "Età del guidatore",
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
#nei veicoli più potenti (oltre i 3000) sembra scendere, ma aumenta la variabilità
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
#REGIONI

#vediamo i boxplot per le regioni


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
#di fatti, in Calabria:
Cal <- sinis %>%
  filter(regione=="Cala")

ggplot(Cal, aes(PV_targa, costo)) +
  geom_boxplot(color="#993300", fill="#FFFFCC") +
  coord_cartesian(ylim=c(0,26000)) +
  labs(
    title = "Ammontare del costo dei sinistri nelle province calabresi",
    x = "Province",
    y = "Costo del sinistro") 

#Sembra in questo casso esserci una differenza significativa tra le province
#si guardi ad esempio  tra Catanzaro e .., in cui i costi hanno alta variabilità e sono generalmente superiori
#La Calabria era tra le regioni da favorire, ma forse non la provincia di..

#vediamo in termini di costo medio
rcost <- aggregate(sinis[, 8], list(regione=sinis$regione), mean) %>%
  rename(costo_med=costo)

p3=ggplot(rcost, aes(x = costo_med, y = regione)) +
  geom_point(size=2) +
  geom_point(data=filter(rcost, costo_med<=3500),color="blue",size=3) +
  geom_point(data=filter(rcost, costo_med>=4000),color="red",size=3) +
  geom_text(aes(label=regione), alpha=0.3, hjust = 0, nudge_x = 100) +
  labs(
    title = "Costo medio del sinistro per regione italiana",
    x = "Costo medio sinistro",      
    y = "Regioni") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p3

#la provincia di ... è probabilmente quella che fa alzare di così tanto il costo medio:
Cal_cost <- aggregate(Cal[, 8], list(PV_targa=Cal$PV_targa), mean) %>%
  rename(costo_med=costo)

ggplot(Cal_cost, aes(x = costo_med, y = PV_targa)) +
  geom_point(size=2) +
  geom_point(data=filter(Cal_cost, costo_med<=3500),color="blue",size=3) +
  geom_point(data=filter(Cal_cost, costo_med>=4000),color="red",size=3) +
  geom_text(aes(label=PV_targa), alpha=0.3, hjust = 0, nudge_x = 100) +
  scale_x_continuous(breaks = seq(2000, 10000, by = 1000)) +
  labs(
    title = "Costo medio del sinistro per provincia calabrese",
    x = "Costo medio sinistro",      
    y = "Province") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
#proprio come pensato..
#___________________________________________________________________________________

#con età

ecost <- aggregate(sinis[, 8], list(eta=sinis$eta), mean) %>%
  rename(costo_med=costo) %>%
  filter(eta < 90)

ggplot(sinis, aes(eta, costo)) +
  geom_line(data=ecost, aes(eta, costo_med), color="blue", size=1, alpha=0.3) +
  geom_smooth(method = lm) +
  labs(
    title = "Andamento del costo dei sinistri al crescere dell'età del guidatore",
    subtitle = "Paragone con il trend del costo medio",
    x = "Età del guidatore",      
    y = "Costo del sinistro") 

#si è visto precedentemente che la sinistrosità decresce con l'età
#anche il costo! bisogna quindi avere una verta prudenza nell'assicurare i giovani


#_____________________________________________________________________________________
#con potenza (non sign)

hcost <- aggregate(sinis[, 8], list(HP_fiscali=sinis$HP_fiscali), mean) %>%
  rename(costo_med=costo) %>%
  filter(HP_fiscali < 28)

sinis2 <- filter(sinis, HP_fiscali < 28 )
  

ggplot(sinis2, aes(HP_fiscali, costo)) +
  geom_line(data=hcost, aes(HP_fiscali, costo_med), color="#FF6600", size=1, alpha=0.3) +
  geom_smooth(method = lm, color="#FF6600") +
  labs(
    title = "Andamento del costo dei sinistri al crescere della cilindrata del veicolo",
    subtitle = "Paragone con il trend del costo medio",
    x = "Cilindrata / 100",      
    y = "Costo del sinistro") 

#e a basse cilindrate?
hcost2 <- aggregate(sinis[, 8], list(HP_fiscali=sinis$HP_fiscali), mean) %>%
  rename(costo_med=costo) %>%
  filter(HP_fiscali < 11)

sinis22 <- filter(sinis, HP_fiscali < 11 )

ggplot(sinis22, aes(HP_fiscali, costo)) +
  geom_line(data=hcost2, aes(HP_fiscali, costo_med), color="#FF6600", size=1, alpha=0.3) +
  geom_smooth(method = lm, color="#FF6600") +
  labs(
    title = "Andamento del costo dei sinistri al crescere della cilindrata del veicolo",
    subtitle = "Paragone con il trend del costo medio",
    x = "Cilindrata / 100",      
    y = "Costo del sinistro") 
#__________________________________________________________________________________

#NON SIGN

#con eta veicolo

ggplot(sinis, aes(anni_auto, costo)) +
  geom_smooth(method = lm, color="#663366") +
  labs(
    title = "Andamento del costo dei sinistri al crescere dell'età del veicolo",
    x = "Anni dall'immatricolazione",      
    y = "Costo del sinistro") 

#_________________________________________________________________________________________

#con alimentazione
#per alcune abbiamo poche obs:
ggplot(sinis, aes(alimentazione)) +
  geom_bar(color = "#006600", fill = "#99FF99") +
  coord_cartesian(ylim=c(0,50)) +
  geom_hline(aes(yintercept=10), linetype=2, alpha=0.5, color="red") 
#togliamo 02, 12, 19, P3, P4

sinis3 <- sinis %>%
  filter (alimentazione != "02" & alimentazione != "12" & alimentazione != "19" & alimentazione != "P3" & alimentazione != "P4")

acost <- aggregate(sinis[, 8], list(alimentazione=sinis$alimentazione), mean) %>%
  rename(costo_med=costo) %>%
  filter (alimentazione != "02" & alimentazione != "12" & alimentazione != "19" & alimentazione != "P3" & alimentazione != "P4")

#con costo medio
ggplot(acost, aes(alimentazione, costo_med, fill=costo_med)) +
  geom_col() +
  coord_polar() +
  theme_minimal() + 
  scale_fill_gradient(low="#CCFFCC", high="#009900", limits=c(1800,9000)) +
  scale_y_continuous(breaks = seq(10000, 10001, by = 1)) +
  labs(
    title = "Costo medio del sinistro per tipologia di alimentazione del veicolo",
    x = "",      
    y = "",
    fill = "Costo medio sinistro")
#la 24 sembra avere costi alti

#con costo
ggplot(sinis3, aes(alimentazione, costo)) +
  geom_boxplot(color = "#006600", fill = "#FFFFCC") +
  coord_cartesian(ylim=c(0,15000)) +
  labs(
    title = "Ammontare del costo dei sinistri per tipologia di alimentazione del veicolo",
    x = "Codice tipologia di alimentazione",
    y = "Costo del sinistro") 

#infatti notiamo che la 24 è quella che presenta maggior variabilità e raggiunge i costi più elevati
#bassi costi per la 22
#si dovrebbe prezzare allora meno la 22 e più la 24..

#_______________________________________________________________________________________

#Si possono poi vedere analisi incrociate. Ad esempio Valutiamo se la relazione tra costo e età 
#vale e in che modo sulle regioni che ne determinano uno basso o elevato

sinis4 <- sinis %>%
  filter(regione == "Lomb" | regione == "Sard" | regione == "Umbr" | regione == "Cala")


p4=ggplot(sinis4, aes(eta, costo, color=regione)) +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(
    values = c(Lomb = "#000099", Sard = "#3333FF", Umbr = "#FF0000", Cala= "#CC0000")
  ) +
  labs(
    title = "Andamento del costo dei sinistri al crescere dell'età del guidatore",
    subtitle = "Condizionatamente alla regione",
    x = "Età del guidatore",      
    y = "Costo del sinistro") 

multiplot(p3, p4, cols=2)

#si vede come le rette siano per le regioni a costi alti più in alto
#per quanto riguarda l'Umbria questo effetto dell'età sembra essere ancora più marcato
#lo è un po' meno in Lombradia dove la pendenza è minima



#_______________________________________________________________________________________


#____________________________________________________________________________________
