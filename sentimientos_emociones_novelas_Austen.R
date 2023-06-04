# Carga de los paquetes
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(tm)
library(tidyverse)
library(tidytext)

# Carga de las novelas a analizar
# Carga del documento Sense and Sensibility
sas.full.v = scan("sense_and_sensibility.txt", what = "character", sep ="\n")

# Preprocesamiento: Aislar datos iniciales y finales
start.sas.v = which(sas.full.v=="CHAPTER I.")
end.sas.v = which(sas.full.v=="THE END")
start.metadata.sas.v = sas.full.v[1:start.sas.v-1]
end.metadata.sas.v = sas.full.v[(end.sas.v+1):length(sas.full.v)]
metadata.sas.v = c(start.metadata.sas.v,end.metadata.sas.v)

# Creación de variable con la obra Sense and Sensibility
sas.lines.v = sas.full.v[start.sas.v:end.sas.v]
sas.v = paste(sas.lines.v, collapse = " ")

# Preprocesamiento de Pride and Prejudice
pap.full.v = scan("pride_and_prejudice.txt", what = "character", sep ="\n")
start.pap.v = which(pap.full.v=="Chapter 1")
end.pap.v = which(pap.full.v==" uniting them.")
start.metadata.pap.v = pap.full.v[1:start.pap.v-1]
end.metadata.pap.v = pap.full.v[(end.pap.v+1):length(pap.full.v)]
metadata.pap.v = c(start.metadata.pap.v,end.metadata.pap.v)
pap.lines.v = pap.full.v[start.pap.v:end.pap.v]
pap.v = paste(pap.lines.v, collapse = " ")

# Preprocesamiento de Mansfield Park
mp.full.v = scan("mansfield_park.txt", what = "character", sep ="\n")
start.mp.v = which(mp.full.v=="CHAPTER I")
end.mp.v = which(mp.full.v=="FINIS.")
start.metadata.mp.v = mp.full.v[1:start.mp.v-1]
end.metadata.mp.v = mp.full.v[(end.mp.v+1):length(mp.full.v)]
metadata.mp.v = c(start.metadata.mp.v,end.metadata.mp.v)
mp.lines.v = mp.full.v[start.mp.v:end.mp.v]
mp.v = paste(mp.lines.v, collapse = " ")

# Preprocesamiento de Emma
em.full.v = scan("emma.txt", what = "character", sep ="\n")
start.em.v = which(em.full.v=="VOLUME I")
end.em.v = which(em.full.v=="FINIS")
start.metadata.em.v = em.full.v[1:start.em.v-1]
end.metadata.em.v = em.full.v[(end.em.v+1):length(em.full.v)]
metadata.em.v = c(start.metadata.em.v,end.metadata.em.v)
em.lines.v = em.full.v[start.em.v:end.em.v]
em.v = paste(em.lines.v, collapse = " ")

# Preprocesamiento de Northanger Abbey
nab.full.v = scan("northanger_abbey.txt", what = "character", sep ="\n")
start.nab.v = which(nab.full.v=="CHAPTER 1")
end.nab.v = which(nab.full.v=="parental tyranny, or reward filial disobedience.")
start.metadata.nab.v = nab.full.v[1:start.nab.v-1]
end.metadata.nab.v = nab.full.v[(end.nab.v+1):length(nab.full.v)]
metadata.nab.v = c(start.metadata.nab.v,end.metadata.nab.v)
nab.lines.v = nab.full.v[start.nab.v:end.nab.v]
nab.v = paste(nab.lines.v, collapse = " ")

# Preprocesamiento de Persuasion
per.full.v = scan("persuasion.txt", what = "character", sep ="\n")
start.per.v = which(per.full.v=="Chapter 1")
end.per.v = which(per.full.v=="Finis")
start.metadata.per.v = per.full.v[1:start.per.v-1]
end.metadata.per.v = per.full.v[(end.per.v+1):length(per.full.v)]
metadata.per.v = c(start.metadata.per.v,end.metadata.per.v)
per.lines.v = per.full.v[start.per.v:end.per.v]
per.v = paste(per.lines.v, collapse = " ")

# Tokenización por palabras de las novelas
sas.words.l = get_tokens(sas.v)
pap.words.l = get_tokens(pap.v)
mp.words.l = get_tokens(mp.v)
em.words.l = get_tokens(em.v)
nab.words.l = get_tokens(nab.v)
per.words.l = get_tokens(per.v)

# Número de palabras de cada novela
length(sas.words.l) # Sense and Sensibility
length(pap.words.l) # Pride and Prejudice
length(mp.words.l) # Mansfield Park
length(em.words.l) # Emma
length(nab.words.l) # Northanger Abbey
length(per.words.l) # Persuasion

# Extracción de sentimientos con NRC
sentimientos.df.sas = get_nrc_sentiment(sas.words.l) # Sense and Sensibility
sentimientos.df.pap = get_nrc_sentiment(pap.words.l) # Pride and Prejudice
sentimientos.df.mp = get_nrc_sentiment(mp.words.l) # Mansfield Park
sentimientos.df.em = get_nrc_sentiment(em.words.l) # Emma
sentimientos.df.nab = get_nrc_sentiment(nab.words.l) # Northanger Abbey
sentimientos.df.per = get_nrc_sentiment(per.words.l) # Persuasion

# Muestra con seis unigramas de todas las novelas
head(sentimientos.df.sas)
head(sentimientos.df.pap)
head(sentimientos.df.mp)
head(sentimientos.df.em)
head(sentimientos.df.nab)
head(sentimientos.df.per)

# Tabla resumen de los valores de cada novela
summary(sentimientos.df.sas)
summary(sentimientos.df.pap)
summary(sentimientos.df.mp)
summary(sentimientos.df.em)
summary(sentimientos.df.nab)
summary(sentimientos.df.per)

# Gráficos de evolución valencia sentimental
sentimientos.valencia.sas = (sentimientos.df.sas$negative *-1) + 
  sentimientos.df.sas$positive
sentimientos.valencia.sas
simple_plot(
  sentimientos.valencia.sas,
  title = "Evolución sentimental en 'Sense and Sensibility'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)
sentimientos.valencia.pap = (sentimientos.df.pap$negative *-1) + 
  sentimientos.df.pap$positive
sentimientos.valencia.pap
simple_plot(
  sentimientos.valencia.pap,
  title = "Evolución sentimental en 'Pride and Prejudice'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)
sentimientos.valencia.mp = (sentimientos.df.mp$negative *-1) + 
  sentimientos.df.mp$positive
sentimientos.valencia.mp
simple_plot(
  sentimientos.valencia.mp,
  title = "Evolución sentimental en 'Mansfield Park'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)
sentimientos.valencia.em = (sentimientos.df.em$negative *-1) + 
  sentimientos.df.em$positive
sentimientos.valencia.em
simple_plot(
  sentimientos.valencia.em,
  title = "Evolución sentimental en 'Emma'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)
sentimientos.valencia.nab = (sentimientos.df.nab$negative *-1) + 
  sentimientos.df.nab$positive
sentimientos.valencia.nab
simple_plot(
  sentimientos.valencia.nab,
  title = "Evolución sentimental en 'Northanger Abbey'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)
sentimientos.valencia.per = (sentimientos.df.per$negative *-1) + 
  sentimientos.df.per$positive
sentimientos.valencia.per
simple_plot(
  sentimientos.valencia.per,
  title = "Evolución sentimental en 'Persuasion'",
  legend_pos = "top",
  lps = 10,
  window = 0.1
)

# Conteo de palabras por sentimiento
# Sense and Sensibility
pal.pos.sas = sas.words.l[sentimientos.df.sas$positive> 0]
pal.pos.sas.orden = sort(table(unlist(pal.pos.sas)), decreasing = TRUE)
head(pal.pos.sas.orden, n=20)
pal.neg.sas = sas.words.l[sentimientos.df.sas$negative> 0]
pal.neg.sas.orden = sort(table(unlist(pal.neg.sas)), decreasing = TRUE)
head(pal.neg.sas.orden, n=20)
# PrideandPrejudice
pal.pos.pap = pap.words.l[sentimientos.df.pap$positive>0]
pal.pos.pap.orden = sort(table(unlist(pal.pos.pap)),decreasing = TRUE)
head(pal.pos.pap.orden,n = 20)
pal.neg.pap = pap.words.l[sentimientos.df.pap$negative>0]
pal.neg.pap.orden = sort(table(unlist(pal.neg.pap)),decreasing = TRUE)
head(pal.neg.pap.orden,n = 20)
# MansfieldPark
pal.pos.mp = mp.words.l[sentimientos.df.mp$positive>0]
pal.pos.mp.orden = sort(table(unlist(pal.pos.mp)),decreasing = TRUE)
head(pal.pos.mp.orden,n = 20)
pal.neg.mp = mp.words.l[sentimientos.df.mp$negative>0]
pal.neg.mp.orden = sort(table(unlist(pal.neg.mp)),decreasing = TRUE)
head(pal.neg.mp.orden,n = 20)
# Emma
pal.pos.em = em.words.l[sentimientos.df.em$positive>0]
pal.pos.em.orden = sort(table(unlist(pal.pos.em)),decreasing = TRUE)
head(pal.pos.em.orden,n = 20)
pal.neg.em = em.words.l[sentimientos.df.em$negative>0]
pal.neg.em.orden = sort(table(unlist(pal.neg.em)),decreasing = TRUE)
head(pal.neg.em.orden,n = 20)
# NorthangerAbbey
pal.pos.nab = nab.words.l[sentimientos.df.nab$positive>0]
pal.pos.nab.orden = sort(table(unlist(pal.pos.nab)),decreasing = TRUE)
head(pal.pos.nab.orden,n = 20)
pal.neg.nab = nab.words.l[sentimientos.df.nab$negative>0]
pal.neg.nab.orden = sort(table(unlist(pal.neg.nab)),decreasing = TRUE)
head(pal.neg.nab.orden,n = 20)
# Persuasion
pal.pos.per = per.words.l[sentimientos.df.per$positive>0]
pal.pos.per.orden = sort(table(unlist(pal.pos.per)),decreasing = TRUE)
head(pal.pos.per.orden,n = 20)
pal.neg.per = per.words.l[sentimientos.df.per$negative>0]
pal.neg.per.orden = sort(table(unlist(pal.neg.per)),decreasing = TRUE)
head(pal.neg.per.orden,n = 20)

# Nube de palabras
# Positivo
nube.posit.full.v = c(
  paste(sas.words.l[sentimientos.df.sas$positive> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$positive > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$positive > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$positive > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$positive > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$positive > 0], collapse = " "))
nube.posit.full.v = iconv(nube.posit.full.v, "latin1", "UTF-8")
nube.posit = Corpus(VectorSource(nube.posit.full.v))
nube.pos.tdm = TermDocumentMatrix(nube.posit)
nube.pos.tdm = as.matrix(nube.pos.tdm)
head(nube.pos.tdm)
colnames(nube.pos.tdm) = c('Sentimiento positivo', NA,NA, NA,NA,NA)
head(nube.pos.tdm)
set.seed(365)
comparison.cloud(nube.pos.tdm, random.order = FALSE,
                 colors = c("darkgreen", "darkgreen", "darkgreen", "darkgreen", 
                            "darkgreen", "darkgreen"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

# Negativo
nube.negat.full.v = c(
  paste(sas.words.l[sentimientos.df.sas$negative> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$negative > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$negative > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$negative > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$negative > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$negative > 0], collapse = " "))
nube.negat.full.v = iconv(nube.negat.full.v, "latin1", "UTF-8")
nube.negat = Corpus(VectorSource(nube.negat.full.v))
nube.neg.tdm = TermDocumentMatrix(nube.negat)
nube.neg.tdm = as.matrix(nube.neg.tdm)
head(nube.neg.tdm)
colnames(nube.neg.tdm) = c('Sentimiento negativo', NA,NA, NA,NA,NA)
head(nube.neg.tdm)
set.seed(365)
comparison.cloud(nube.neg.tdm, random.order = FALSE,
                 colors = c("darkred", "darkred", "darkred", "darkred", "darkred", 
                            "darkred"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)

# Gráficos de barras de emociones
barplot(
  colSums(prop.table(sentimientos.df.sas[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Sense and Sensibility'",
  xlab= NULL, ylab = NULL)
barplot(
  colSums(prop.table(sentimientos.df.pap[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Pride and Prejudice'",
  xlab= NULL, ylab = NULL)
barplot(
  colSums(prop.table(sentimientos.df.mp[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Mansfield Park'",
  xlab= NULL, ylab = NULL)
barplot(
  colSums(prop.table(sentimientos.df.em[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Emma'",
  xlab= NULL, ylab = NULL)
barplot(
  colSums(prop.table(sentimientos.df.nab[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Northanger Abbey'",
  xlab= NULL, ylab = NULL)
barplot(
  colSums(prop.table(sentimientos.df.per[, 1:8])),
  space = 0.1,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Dark2"),
  main = "Emociones en 'Persuasion'",
  xlab= NULL, ylab = NULL)

# Frecuencia de términos por novela y emoción
# Sense and Sensibility
pal.anger.sas = sas.words.l[sentimientos.df.sas$anger> 0]
length(pal.anger.sas) # Cantidad total de palabras utilizadas para ira
pal.antic.sas = sas.words.l[sentimientos.df.sas$anticipation> 0]
length(pal.antic.sas)
pal.disg.sas = sas.words.l[sentimientos.df.sas$disgust> 0]
length(pal.disg.sas)
pal.fear.sas = sas.words.l[sentimientos.df.sas$fear> 0]
length(pal.fear.sas)
pal.joy.sas = sas.words.l[sentimientos.df.sas$joy> 0]
length(pal.joy.sas)
pal.sad.sas = sas.words.l[sentimientos.df.sas$sadness> 0]
length(pal.sad.sas)
pal.surp.sas = sas.words.l[sentimientos.df.sas$surprise> 0]
length(pal.surp.sas)
pal.trust.sas = sas.words.l[sentimientos.df.sas$trust> 0]
length(pal.trust.sas)
# Pride and Prejudice
pal.anger.pap = pap.words.l[sentimientos.df.pap$anger> 0]
length(pal.anger.pap)
pal.antic.pap = pap.words.l[sentimientos.df.pap$anticipation> 0]
length(pal.antic.pap)
pal.disg.pap = pap.words.l[sentimientos.df.pap$disgust> 0]
length(pal.disg.pap)
pal.fear.pap = pap.words.l[sentimientos.df.pap$fear> 0]
length(pal.fear.pap)
pal.joy.pap = pap.words.l[sentimientos.df.pap$joy> 0]
length(pal.joy.pap)
pal.sad.pap = pap.words.l[sentimientos.df.pap$sadness> 0]
length(pal.sad.pap)
pal.surp.pap = pap.words.l[sentimientos.df.pap$surprise> 0]
length(pal.surp.pap)
pal.trust.pap = pap.words.l[sentimientos.df.pap$trust> 0]
length(pal.trust.pap)
# Mansfield Park
pal.anger.mp = mp.words.l[sentimientos.df.mp$anger> 0]
length(pal.anger.mp)
pal.antic.mp = mp.words.l[sentimientos.df.mp$anticipation> 0]
length(pal.antic.mp)
pal.disg.mp = mp.words.l[sentimientos.df.mp$disgust> 0]
length(pal.disg.mp)
pal.fear.mp = mp.words.l[sentimientos.df.mp$fear> 0]
length(pal.fear.mp)
pal.joy.mp = mp.words.l[sentimientos.df.mp$joy> 0]
length(pal.joy.mp)
pal.sad.mp = mp.words.l[sentimientos.df.mp$sadness> 0]
length(pal.sad.mp)
pal.surp.mp = mp.words.l[sentimientos.df.mp$surprise> 0]
length(pal.surp.mp)
pal.trust.mp = mp.words.l[sentimientos.df.mp$trust> 0]
length(pal.trust.mp)
# Emma
pal.anger.em = em.words.l[sentimientos.df.em$anger> 0]
length(pal.anger.em)
pal.antic.em = em.words.l[sentimientos.df.em$anticipation> 0]
length(pal.antic.em)
pal.disg.em = em.words.l[sentimientos.df.em$disgust> 0]
length(pal.disg.em)
pal.fear.em = em.words.l[sentimientos.df.em$fear> 0]
length(pal.fear.em)
pal.joy.em = em.words.l[sentimientos.df.em$joy> 0]
length(pal.joy.em)
pal.sad.em = em.words.l[sentimientos.df.em$sadness> 0]
length(pal.sad.em)
pal.surp.em = em.words.l[sentimientos.df.em$surprise> 0]
length(pal.surp.em)
pal.trust.em = em.words.l[sentimientos.df.em$trust> 0]
length(pal.trust.em)
# Northanger Abbey
pal.anger.nab = nab.words.l[sentimientos.df.nab$anger> 0]
length(pal.anger.nab)
pal.antic.nab = nab.words.l[sentimientos.df.nab$anticipation> 0]
length(pal.antic.nab)
pal.disg.nab = nab.words.l[sentimientos.df.nab$disgust> 0]
length(pal.disg.nab)
pal.fear.nab = nab.words.l[sentimientos.df.nab$fear> 0]
length(pal.fear.nab)
pal.joy.nab = nab.words.l[sentimientos.df.nab$joy> 0]
length(pal.joy.nab)
pal.sad.nab = nab.words.l[sentimientos.df.nab$sadness> 0]
length(pal.sad.nab)
pal.surp.nab = nab.words.l[sentimientos.df.nab$surprise> 0]
length(pal.surp.nab)
pal.trust.nab = nab.words.l[sentimientos.df.nab$trust> 0]
length(pal.trust.nab)
# Persuasion
pal.anger.per = per.words.l[sentimientos.df.per$anger> 0]
length(pal.anger.per)
pal.antic.per = per.words.l[sentimientos.df.per$anticipation> 0]
length(pal.antic.per)
pal.disg.per = per.words.l[sentimientos.df.per$disgust> 0]
length(pal.disg.per)
pal.fear.per = per.words.l[sentimientos.df.per$fear> 0]
length(pal.fear.per)
pal.joy.per = per.words.l[sentimientos.df.per$joy> 0]
length(pal.joy.per)
pal.sad.per = per.words.l[sentimientos.df.per$sadness> 0]
length(pal.sad.per)
pal.surp.per = per.words.l[sentimientos.df.per$surprise> 0]
length(pal.surp.per)
pal.trust.per = per.words.l[sentimientos.df.per$trust> 0]
length(pal.trust.per)

# Palabras más frecuentes por emoción y número de palabras únicas
# Ira
pal.anger.sas.orden = sort(table(unlist(pal.anger.sas)), decreasing = TRUE)
head(pal.anger.sas.orden, n=20)
length(pal.anger.sas.orden)
pal.anger.pap.orden = sort(table(unlist(pal.anger.pap)), decreasing = TRUE)
head(pal.anger.pap.orden, n=20)
length(pal.anger.pap.orden)
pal.anger.mp.orden = sort(table(unlist(pal.anger.mp)), decreasing = TRUE)
head(pal.anger.mp.orden, n=20)
length(pal.anger.mp.orden)
pal.anger.em.orden = sort(table(unlist(pal.anger.em)), decreasing = TRUE)
head(pal.anger.em.orden, n=20)
length(pal.anger.em.orden)
pal.anger.nab.orden = sort(table(unlist(pal.anger.nab)), decreasing = TRUE)
head(pal.anger.nab.orden, n=20)
length(pal.anger.ban.orden)
pal.anger.per.orden = sort(table(unlist(pal.anger.per)), decreasing = TRUE)
head(pal.anger.per.orden, n=20)
length(pal.anger.per.orden)
# Anticipación
pal.antic.sas.orden = sort(table(unlist(pal.antic.sas)), decreasing = TRUE)
head(pal.antic.sas.orden, n=20)
length(pal.antic.sas.orden)
pal.antic.pap.orden = sort(table(unlist(pal.antic.pap)), decreasing = TRUE)
head(pal.antic.pap.orden, n=20)
length(pal.antic.pap.orden)
pal.antic.mp.orden = sort(table(unlist(pal.antic.mp)), decreasing = TRUE)
head(pal.antic.mp.orden, n=20)
length(pal.antic.mp.orden)
pal.antic.em.orden = sort(table(unlist(pal.antic.em)), decreasing = TRUE)
head(pal.antic.em.orden, n=20)
length(pal.antic.em.orden)
pal.antic.nab.orden = sort(table(unlist(pal.antic.nab)), decreasing = TRUE)
head(pal.antic.nab.orden, n=20)
length(pal.antic.nab.orden)
pal.antic.per.orden = sort(table(unlist(pal.antic.per)), decreasing = TRUE)
head(pal.antic.per.orden, n=20)
length(pal.antic.per.orden)
# Disgusto
pal.disg.sas.orden = sort(table(unlist(pal.disg.sas)), decreasing = TRUE)
head(pal.disg.sas.orden, n=20)
length(pal.disg.sas.orden)
pal.disg.pap.orden = sort(table(unlist(pal.disg.pap)), decreasing = TRUE)
head(pal.disg.pap.orden, n=20)
length(pal.disg.pap.orden)
pal.disg.mp.orden = sort(table(unlist(pal.disg.mp)), decreasing = TRUE)
head(pal.disg.mp.orden, n=20)
length(pal.disg.mp.orden)
pal.disg.em.orden = sort(table(unlist(pal.disg.em)), decreasing = TRUE)
head(pal.disg.em.orden, n=20)
length(pal.disg.em.orden)
pal.disg.nab.orden = sort(table(unlist(pal.disg.nab)), decreasing = TRUE)
head(pal.disg.nab.orden, n=20)
length(pal.disg.nab.orden)
pal.disg.per.orden = sort(table(unlist(pal.disg.per)), decreasing = TRUE)
head(pal.disg.per.orden, n=20)
length(pal.disg.per.orden)
#Miedo
pal.fear.sas.orden = sort(table(unlist(pal.fear.sas)), decreasing = TRUE)
head(pal.fear.sas.orden, n=20)
length(pal.fear.sas.orden)
pal.fear.pap.orden = sort(table(unlist(pal.fear.pap)), decreasing = TRUE)
head(pal.fear.pap.orden, n=20)
length(pal.fear.pap.orden)
pal.fear.mp.orden = sort(table(unlist(pal.fear.mp)), decreasing = TRUE)
head(pal.fear.mp.orden, n=20)
length(pal.fear.mp.orden)
pal.fear.em.orden = sort(table(unlist(pal.fear.em)), decreasing = TRUE)
head(pal.fear.em.orden, n=20)
length(pal.fear.em.orden)
pal.fear.nab.orden = sort(table(unlist(pal.fear.nab)), decreasing = TRUE)
head(pal.fear.nab.orden, n=20)
length(pal.fear.nab.orden)
pal.fear.per.orden = sort(table(unlist(pal.fear.per)), decreasing = TRUE)
head(pal.fear.per.orden, n=20)
length(pal.fear.per.orden)
# Alegría
pal.joy.sas.orden = sort(table(unlist(pal.joy.sas)), decreasing = TRUE)
head(pal.joy.sas.orden, n=20)
length(pal.joy.sas.orden)
pal.joy.pap.orden = sort(table(unlist(pal.joy.pap)), decreasing = TRUE)
head(pal.joy.pap.orden, n=20)
length(pal.joy.pap.orden)
pal.joy.mp.orden = sort(table(unlist(pal.joy.mp)), decreasing = TRUE)
head(pal.joy.mp.orden, n=20)
length(pal.joy.mp.orden)
pal.joy.em.orden = sort(table(unlist(pal.joy.em)), decreasing = TRUE)
head(pal.joy.em.orden, n=20)
length(pal.joy.em.orden)
pal.joy.nab.orden = sort(table(unlist(pal.joy.nab)), decreasing = TRUE)
head(pal.joy.nab.orden, n=20)
length(pal.joy.nab.orden)
pal.joy.per.orden = sort(table(unlist(pal.joy.per)), decreasing = TRUE)
head(pal.joy.per.orden, n=20)
length(pal.joy.per.orden)
# Tristeza
pal.sad.sas.orden = sort(table(unlist(pal.sad.sas)), decreasing = TRUE)
head(pal.sad.sas.orden, n=20)
length(pal.sad.sas.orden)
pal.sad.pap.orden = sort(table(unlist(pal.sad.pap)), decreasing = TRUE)
head(pal.sad.pap.orden, n=20)
length(pal.sad.pap.orden)
pal.sad.mp.orden = sort(table(unlist(pal.sad.mp)), decreasing = TRUE)
head(pal.sad.mp.orden, n=20)
length(pal.sad.mp.orden)
pal.sad.em.orden = sort(table(unlist(pal.sad.em)), decreasing = TRUE)
head(pal.sad.em.orden, n=20)
length(pal.sad.em.orden)
pal.sad.nab.orden = sort(table(unlist(pal.sad.nab)), decreasing = TRUE)
head(pal.sad.nab.orden, n=20)
length(pal.sad.nab.orden)
pal.sad.per.orden = sort(table(unlist(pal.sad.per)), decreasing = TRUE)
head(pal.sad.per.orden, n=20)
length(pal.sad.per.orden)
# Sorpresa
pal.surp.sas.orden = sort(table(unlist(pal.surp.sas)), decreasing = TRUE)
head(pal.surp.sas.orden, n=20)
length(pal.surp.sas.orden)
pal.surp.pap.orden = sort(table(unlist(pal.surp.pap)), decreasing = TRUE)
head(pal.surp.pap.orden, n=20)
length(pal.surp.pap.orden)
pal.surp.mp.orden = sort(table(unlist(pal.surp.mp)), decreasing = TRUE)
head(pal.surp.mp.orden, n=20)
length(pal.surp.mp.orden)
pal.surp.em.orden = sort(table(unlist(pal.surp.em)), decreasing = TRUE)
head(pal.surp.em.orden, n=20)
length(pal.surp.em.orden)
pal.surp.nab.orden = sort(table(unlist(pal.surp.nab)), decreasing = TRUE)
head(pal.surp.nab.orden, n=20)
length(pal.surp.nab.orden)
pal.surp.per.orden = sort(table(unlist(pal.surp.per)), decreasing = TRUE)
head(pal.surp.per.orden, n=20)
length(pal.surp.per.orden)
# Confianza
pal.trust.sas.orden = sort(table(unlist(pal.trust.sas)), decreasing = TRUE)
head(pal.trust.sas.orden, n=20)
length(pal.trust.sas.orden)
pal.trust.pap.orden = sort(table(unlist(pal.trust.pap)), decreasing = TRUE)
head(pal.trust.pap.orden, n=20)
length(pal.trust.pap.orden)
pal.trust.mp.orden = sort(table(unlist(pal.trust.mp)), decreasing = TRUE)
head(pal.trust.mp.orden, n=20)
length(pal.trust.mp.orden)
pal.trust.em.orden = sort(table(unlist(pal.trust.em)), decreasing = TRUE)
head(pal.trust.em.orden, n=20)
length(pal.trust.em.orden)
pal.trust.nab.orden = sort(table(unlist(pal.trust.nab)), decreasing = TRUE)
head(pal.trust.nab.orden, n=20)
length(pal.trust.nab.orden)
pal.trust.per.orden = sort(table(unlist(pal.trust.per)), decreasing = TRUE)
head(pal.trust.per.orden, n=20)
length(pal.trust.per.orden)

# Palabras únicas por emoción
pal.anger = c(pal.anger.sas, pal.anger.pap, pal.anger.mp, pal.anger.em, 
              pal.anger.nab, pal.anger.per)
pal.anger.orden = sort(table(unlist(pal.anger)), decreasing = TRUE)
length(pal.anger.orden)
pal.antic = c(pal.antic.sas, pal.antic.pap, pal.antic.mp, pal.antic.em, 
              pal.antic.nab, pal.antic.per)
pal.antic.orden = sort(table(unlist(pal.antic)), decreasing = TRUE)
length(pal.antic.orden)
pal.disg = c(pal.disg.sas, pal.disg.pap, pal.disg.mp, pal.disg.em, pal.disg.nab, 
             pal.disg.per)
pal.disg.orden = sort(table(unlist(pal.disg)), decreasing = TRUE)
length(pal.disg.orden)
pal.fear = c(pal.fear.sas, pal.fear.pap, pal.fear.mp, pal.fear.em, pal.fear.nab, 
             pal.fear.per)
pal.fear.orden = sort(table(unlist(pal.fear)), decreasing = TRUE)
length(pal.fear.orden)
pal.joy = c(pal.joy.sas, pal.joy.pap, pal.joy.mp, pal.joy.em, pal.joy.nab, 
            pal.joy.per)
pal.joy.orden = sort(table(unlist(pal.joy)), decreasing = TRUE)
length(pal.joy.orden)
pal.sad = c(pal.sad.sas, pal.sad.pap, pal.sad.mp, pal.sad.em, pal.sad.nab, 
            pal.sad.per)
pal.sad.orden = sort(table(unlist(pal.sad)), decreasing = TRUE)
length(pal.sad.orden)
pal.surp = c(pal.surp.sas, pal.surp.pap, pal.surp.mp, pal.surp.em, pal.surp.nab, 
             pal.surp.per)
pal.surp.orden = sort(table(unlist(pal.surp)), decreasing = TRUE)
length(pal.surp.orden)
pal.trust = c(pal.trust.sas, pal.trust.pap, pal.trust.mp, pal.trust.em, 
              pal.trust.nab, pal.trust.per)
pal.trust.orden = sort(table(unlist(pal.trust)), decreasing = TRUE)
length(pal.trust.orden)

# Nubes de palabras para cada emoción
# Confianza
nube.trust.v = c(
  paste(sas.words.l[sentimientos.df.sas$trust> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$trust > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$trust > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$trust > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$trust > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$trust > 0], collapse = " "))
nube.trust.v = iconv(nube.trust.v, "latin1", "UTF-8")
nube.trust = Corpus(VectorSource(nube.trust.v))
nube.trust.tdm = TermDocumentMatrix(nube.trust)
nube.trust.tdm = as.matrix(nube.trust.tdm)
head(nube.trust.tdm)
colnames(nube.trust.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                             'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.trust.tdm)
set.seed(365)
comparison.cloud(nube.trust.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Ira
nube.anger.v = c(
  paste(sas.words.l[sentimientos.df.sas$anger> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$anger > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$anger > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$anger > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$anger > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$anger > 0], collapse = " "))
nube.anger.v = iconv(nube.anger.v, "latin1", "UTF-8")
nube.anger = Corpus(VectorSource(nube.anger.v))
nube.anger.tdm = TermDocumentMatrix(nube.anger)
nube.anger.tdm = as.matrix(nube.anger.tdm)
head(nube.anger.tdm)
colnames(nube.anger.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                             'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.anger.tdm)
set.seed(365)
comparison.cloud(nube.anger.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Anticipación
nube.antic.v = c(
  paste(sas.words.l[sentimientos.df.sas$anticipation> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$anticipation > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$anticipation > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$anticipation > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$anticipation > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$anticipation > 0], collapse = " "))
nube.antic.v = iconv(nube.antic.v, "latin1", "UTF-8")
nube.antic = Corpus(VectorSource(nube.antic.v))
nube.antic.tdm = TermDocumentMatrix(nube.antic)
nube.antic.tdm = as.matrix(nube.antic.tdm)
head(nube.antic.tdm)
colnames(nube.antic.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                             'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.antic.tdm)
set.seed(365)
comparison.cloud(nube.antic.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Disgusto
nube.disg.v = c(
  paste(sas.words.l[sentimientos.df.sas$disgust> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$disgust > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$disgust > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$disgust > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$disgust > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$disgust > 0], collapse = " "))
nube.disg.v = iconv(nube.disg.v, "latin1", "UTF-8")
nube.disg = Corpus(VectorSource(nube.disg.v))
nube.disg.tdm = TermDocumentMatrix(nube.disg)
nube.disg.tdm = as.matrix(nube.disg.tdm)
head(nube.disg.tdm)
colnames(nube.disg.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                            'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.disg.tdm)
set.seed(365)
comparison.cloud(nube.disg.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Miedo
nube.fear.v = c(
  paste(sas.words.l[sentimientos.df.sas$fear> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$fear > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$fear > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$fear > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$fear > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$fear > 0], collapse = " "))
nube.fear.v = iconv(nube.fear.v, "latin1", "UTF-8")
nube.fear = Corpus(VectorSource(nube.fear.v))
nube.fear.tdm = TermDocumentMatrix(nube.fear)
nube.fear.tdm = as.matrix(nube.fear.tdm)
head(nube.fear.tdm)
colnames(nube.fear.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                            'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.fear.tdm)
set.seed(365)
comparison.cloud(nube.fear.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Alegría
nube.joy.v = c(
  paste(sas.words.l[sentimientos.df.sas$joy> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$joy > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$joy > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$joy > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$joy > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$joy > 0], collapse = " "))
nube.joy.v = iconv(nube.joy.v, "latin1", "UTF-8")
nube.joy = Corpus(VectorSource(nube.joy.v))
nube.joy.tdm = TermDocumentMatrix(nube.joy)
nube.joy.tdm = as.matrix(nube.joy.tdm)
head(nube.joy.tdm)
colnames(nube.joy.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                           'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.joy.tdm)
set.seed(365)
comparison.cloud(nube.joy.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Tristeza
nube.sad.v = c(
  paste(sas.words.l[sentimientos.df.sas$sadness> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$sadness > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$sadness > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$sadness > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$sadness > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$sadness > 0], collapse = " "))
nube.sad.v = iconv(nube.sad.v, "latin1", "UTF-8")
nube.sad = Corpus(VectorSource(nube.sad.v))
nube.sad.tdm = TermDocumentMatrix(nube.sad)
nube.sad.tdm = as.matrix(nube.sad.tdm)
head(nube.sad.tdm)
colnames(nube.sad.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                           'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.sad.tdm)
set.seed(365)
comparison.cloud(nube.sad.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
# Sorpresa
nube.surp.v = c(
  paste(sas.words.l[sentimientos.df.sas$surprise> 0], collapse = " "),
  paste(pap.words.l[sentimientos.df.pap$surprise > 0], collapse = " "),
  paste(mp.words.l[sentimientos.df.mp$surprise > 0], collapse = " "),
  paste(em.words.l[sentimientos.df.em$surprise > 0], collapse = " "),
  paste(nab.words.l[sentimientos.df.nab$surprise > 0], collapse = " "),
  paste(per.words.l[sentimientos.df.per$surprise > 0], collapse = " "))
nube.surp.v = iconv(nube.surp.v, "latin1", "UTF-8")
nube.surp = Corpus(VectorSource(nube.surp.v))
nube.surp.tdm = TermDocumentMatrix(nube.surp)
nube.surp.tdm = as.matrix(nube.surp.tdm)
head(nube.surp.tdm)
colnames(nube.surp.tdm) = c('Sense and Sensibility', 'Pride and Prejudice', 
                            'Mansfield Park', 'Emma', 'Northanger Abbey', 'Persuasion')
head(nube.surp.tdm)
set.seed(365)
comparison.cloud(nube.surp.tdm, random.order = FALSE,
                 colors = c("deeppink3", "red", "purple", "aquamarine3", "blue", 
                            "green3"),
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)


