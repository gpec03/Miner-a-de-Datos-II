install.packages("rvest")
install.packages("syuzhet")
install.packages("stringr")
library(rvest)
library(stringr)
library(dplyr)
library(tm)
library(qdap)
library(wordcloud2)
library(igraph)
library(visNetwork)
library(lubridate)
library(syuzhet)

#ECONOMIA

economy <- "https://www.economy.com.bo/blog/section/economia/"
n_economy <- read_html(economy)
aux <- html_nodes(n_economy, ".col-sm-12.col-xs-12")
titular1 <- html_text2(html_nodes(aux, ".economia .title a"))

economy <- "https://www.economy.com.bo/blog/section/economia/?page="
for(i in 2:60) {
  n_diario <- read_html(paste0(economy, i))
  aux <- html_nodes(n_diario, ".col-sm-12.col-xs-12")
  x1 <- html_text2(html_nodes(aux, ".economia .title a"))
  titular1 <- append(titular1, x1)
}

eldiario <- "https://www.eldiario.net/portal/category/secciones/economia/"
n_diario <- read_html(eldiario)
aux <- html_nodes(n_diario, "#tdi_58 .td-cpt-post")
titular2 <- html_text2(html_nodes(aux, ".td-module-title"))

eldiario <- "https://www.eldiario.net/portal/category/secciones/economia/page/"
for(i in 2:100) {
  eldiario2 <- paste0(eldiario, i)
  n_diario <- read_html(paste0(eldiario2, "/"))
  aux <- html_nodes(n_diario, "#tdi_58 .td-cpt-post")
  x1 <- html_text2(html_nodes(aux, ".td-module-title"))
  titular2 <- append(titular2, x1)
}

elmundo <- "https://elmundo.com.bo/category/economia/"
n_elmundo <- read_html(elmundo)
aux <- html_nodes(n_elmundo, ".content-column")
titular3 <- html_text2(html_nodes(aux, ".bsw-7 .title"))

elmundo <- "https://elmundo.com.bo/category/economia/page/"
for(i in 2:120) {
  elmundo2 <- paste0(elmundo, i)
  n_elmundo <- read_html(paste0(elmundo2, "/"))
  aux <- html_nodes(n_elmundo, ".content-column")
  x1 <- html_text2(html_nodes(aux, ".bsw-7 .title"))
  titular3 <- append(titular3, x1)
}

elarazon <- "https://www.la-razon.com/economia/"
n_elarazon <- read_html(elarazon)
eaux <- html_nodes(n_elarazon, ".mobile")
etitular2 <- html_text2(html_nodes(eaux, ".title"))

p_elarazon <- "https://www.la-razon.com/economia/page/"
for(i in 2:134) {
  elarazon2 <- paste0(p_elarazon, i)
  n_elarazon <- read_html(paste0(elarazon2, "/"))
  eaux <- html_nodes(n_elarazon, ".mobile")
  ex1 <- html_text2(html_nodes(eaux, ".title"))
  etitular2 <- append(etitular2, ex1)
}

economy_teco <- data.frame(titular = titular1)
eldiario_teco <- data.frame(titular = titular2)
elmundo_teco <- data.frame(titular = titular3)
larazon_teco <- data.frame(titular = etitular2)
economia <- merge(economy_teco, eldiario_teco, by = "titular", all = TRUE)
economia <- merge(economia, elmundo_teco, by = "titular", all = TRUE)
economia <- merge(economia, larazon_teco, by = "titular", all = TRUE)

# POLITICA

eldiario <- "https://www.eldiario.net/portal/category/secciones/politica/"
n_diario <- read_html(eldiario)
aux <- html_nodes(n_diario, "#tdi_58 .td-module-meta-info")
titular2 <- html_text2(html_nodes(aux, ".td-module-title a"))

eldiario <- "https://www.eldiario.net/portal/category/secciones/politica/page/"
for(i in 2:100) {
  eldiario2 <- paste0(eldiario, i)
  n_diario <- read_html(paste0(eldiario2, "/"))
  aux <- html_nodes(n_diario, "#tdi_58 .td-module-meta-info")
  titular2 <- html_text2(html_nodes(aux, ".td-module-title a"))
  titular2 <- append(titular2, x1)
}

elmundo <- "https://elmundo.com.bo/category/politica/"
n_elmundo <- read_html(elmundo)
aux <- html_nodes(n_elmundo, ".bsw-7 .clearfix")
titular3 <- html_text2(html_nodes(aux, ".title"))

elmundo <- "https://elmundo.com.bo/category/politica/page/"
for(i in 2:100) {
  elmundo2 <- paste0(elmundo, i)
  n_elmundo <- read_html(paste0(elmundo2, "/"))
  aux <- html_nodes(n_elmundo, ".bsw-7 .clearfix")
  x1 <- html_text2(html_nodes(aux, ".title"))
  titular3 <- append(titular3, x1)
}

plarazon <- "https://www.la-razon.com/nacional/"
n_plarazon <- read_html(plarazon)
paux <- html_nodes(n_plarazon, ".mobile")
ptitular2 <- html_text2(html_nodes(paux, ".title"))

p_plarazon <- "https://www.la-razon.com/nacional/page/"
for(i in 2:134) {
  plarazon2 <- paste0(p_plarazon, i)
  n_plarazon <- read_html(paste0(plarazon2, "/"))
  paux <- html_nodes(n_plarazon, ".mobile")
  px1 <- html_text2(html_nodes(paux, ".title"))
  ptitular2 <- append(ptitular2, px1)
}

eldiario_tpoli <- data.frame(titular = titular2)
elmundo_tpoli <- data.frame(titular = titular3)
larazon_tpoli <- data.frame(titular = ptitular2)
politica <- eldiario_tpoli %>%
  full_join(elmundo_tpoli, by = "titular") %>%
  full_join(larazon_tpoli, by = "titular")

# INTERNACIONAL

eldiario <- "https://www.eldiario.net/portal/category/secciones/internacional/"
n_diario <- read_html(eldiario)
aux <- html_nodes(n_diario, "#tdi_58 .td-module-meta-info")
titular2 <- html_text2(html_nodes(aux, ".td-module-title a"))

eldiario <- "https://www.eldiario.net/portal/category/secciones/economia/page/"
for(i in 2:100) {
  eldiario2 <- paste0(eldiario, i)
  n_diario <- read_html(paste0(eldiario2, "/"))
  aux <- html_nodes(n_diario, "#tdi_58 .td-module-meta-info")
  x1 <- html_text2(html_nodes(aux, ".td-module-title"))
  titular2 <- append(titular2, x1)
}

elmundo <- "https://elmundo.com.bo/category/internacional/"
n_elmundo <- read_html(elmundo)
aux <- html_nodes(n_elmundo, ".bsw-7 .clearfix")
titular3 <- html_text2(html_nodes(aux, ".title"))

elmundo <- "https://elmundo.com.bo/category/internacional/page/"
for(i in 2:17) {
  elmundo2 <- paste0(elmundo, i)
  n_elmundo <- read_html(paste0(elmundo2, "/"))
  aux <- html_nodes(n_elmundo, ".bsw-7 .clearfix")
  x1 <- html_text2(html_nodes(aux, ".post-title"))
  titular3 <- append(titular3, x1)
}

ilarazon <- "https://www.la-razon.com/mundo/"
n_ilarazon <- read_html(ilarazon)
iaux <- html_nodes(n_ilarazon, ".mobile")
ititular2 <- html_text2(html_nodes(iaux, ".title"))

p_ilarazon <- "https://www.la-razon.com/mundo/page/"
for(i in 2:134) {
  ilarazon2 <- paste0(p_ilarazon, i)
  n_ilarazon <- read_html(paste0(ilarazon2, "/"))
  iaux <- html_nodes(n_ilarazon, ".mobile")
  ix1 <- html_text2(html_nodes(iaux, ".title"))
  ititular2 <- append(ititular2, ix1)
}

eldiario_tint <- data.frame(titular = titular2)
elmundo_tint <- data.frame(titular = titular3)
larazon_tint <- data.frame(titular = ititular2)

internacional <- eldiario_tint %>%
  full_join(elmundo_tint, by = "titular") %>%
  full_join(larazon_tint, by = "titular")

# Importar y cargar el corpus
bdt <- economia$titular
bdt2 <- politica$titular
bdt3 <- internacional$titular

bd_t <- VCorpus(VectorSource(bdt))
bd_t2 <- VCorpus(VectorSource(bdt2))
bd_t3 <- VCorpus(VectorSource(bdt3))

# Limpieza del corpus
limpieza <- function(cp, extra = c("")) {
  cpl <- tm_map(cp, content_transformer(tolower))
  cpl <- tm_map(cpl, removePunctuation)
  cpl <- tm_map(cpl, removeNumbers)
  cpl <- tm_map(cpl, removeWords, stopwords("es"))
  cpl <- tm_map(cpl, removeWords, extra)
  cpl <- tm_map(cpl, stripWhitespace)
  return(cpl)
}

tl <- limpieza(bd_t)
tl2 <- limpieza(bd_t2)
tl3 <- limpieza(bd_t3)

tl[[2]]$content

# Armar los TDM o DTM
dtm <- DocumentTermMatrix(tl)
dtm_m <- as.matrix(dtm)
tdm <- TermDocumentMatrix(tl)
tdm_m <- as.matrix(tdm)

dtm2 <- DocumentTermMatrix(tl2)
dtm_m2 <- as.matrix(dtm2)
tdm2 <- TermDocumentMatrix(tl2)
tdm_m2 <- as.matrix(tdm2)

dtm3 <- DocumentTermMatrix(tl3)
dtm_m3 <- as.matrix(dtm3)
tdm3 <- TermDocumentMatrix(tl3)
tdm_m3 <- as.matrix(tdm3)

#ANALISIS
tf <- rowSums(tdm_m)
bd1 <- data.frame(tx = names(tf), freq = tf)
bd1 <- bd1 %>% arrange(-freq)
bd1 %>% filter(freq >= 50)

tf2 <- rowSums(tdm_m2)
bd2 <- data.frame(tx = names(tf2), freq = tf2)
bd2 <- bd2 %>% arrange(-freq)
bd2 %>% filter(freq >= 100)

tf3 <- rowSums(tdm_m3)
bd3 <- data.frame(tx = names(tf3), freq = tf3)
bd3 <- bd3 %>% arrange(-freq)
bd3 %>% filter(freq >= 100)

comunes <- merge(bd1, bd2, by = "tx")
comunes <- merge(comunes, bd3, by = "tx")
palabras_comunes <- comunes %>% filter(freq.x >= 5, freq.y >= 5, freq >= 5) %>% select(tx)
palabras_comunes_vector <- as.vector(palabras_comunes$tx)

titular_a_binario <- function(titular, palabras_comunes_vector) {
  palabras_titular <- unlist(strsplit(titular, " "))
  binario <- sapply(palabras_comunes_vector, function(palabra) ifelse(palabra %in% palabras_titular, 1, 0))
  return(binario)
}

titulares_binarios <- t(apply(data.frame(titular = all_news$titular), 1, function(x) titular_a_binario(x, palabras_comunes_vector)))

titulares_binarios_df <- as.data.frame(titulares_binarios)
colnames(titulares_binarios_df) <- palabras_comunes_vector
titulares_binarios_df$category <- all_news$category
library(caret)
library(e1071)

titulares_binarios_df$category <- as.factor(titulares_binarios_df$category)

set.seed(123)
train_index <- createDataPartition(titulares_binarios_df$category, p = 0.7, list = FALSE)
train_data <- titulares_binarios_df[train_index, ]
test_data <- titulares_binarios_df[-train_index, ]

nb_model <- naiveBayes(category ~ ., data = train_data)

predictions <- predict(nb_model, test_data)

confusionMatrix(predictions, test_data$category)

library(caret)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

titulares_binarios_df$category <- as.factor(titulares_binarios_df$category)

set.seed(123)
train_index <- createDataPartition(titulares_binarios_df$category, p = 0.7, list = FALSE)
train_data <- titulares_binarios_df[train_index, ]
test_data <- titulares_binarios_df[-train_index, ]

cart_model <- rpart(category ~ ., data = train_data, method = "class")

rpart.plot(cart_model)

predictions <- predict(cart_model, test_data, type = "class")

confusionMatrix(predictions, test_data$category)