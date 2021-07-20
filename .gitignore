library(dplyr) #digunakan untuk analisis dan eksplorasi data
library(tidyr) #digunakan untuk import data file
library(NLP) #digunakan untuk preprocessing data
library(tm) #digunakan untuk cleansing data
library(stringr) #digunakan untuk integrasi karakter
library(caret) #digunakan untuk menguji berbagai prosedur pemodelan
library(ggplot2) #digunakan untuk membuat berbagai bentuk grafik
library(RColorBrewer) #digunakan untuk memberikan pewarnaan pada visualisasi
library(wordcloud) #pembuatan wordcloud
library(tokenizers) #proses number removal, puntuation removal (tanda baca)

#Input file SIGAP
library(readxl)
Tiket_Sigap_1_ <- read_excel("C:/Users/USER/Downloads/Tiket Sigap.xlsx")
View(Tiket_Sigap_1_)

#Mengubah data ke dalam corpus
corpusdata <- Corpus(VectorSource(Tiket_Sigap_1_$tiket_pertanyaan))
corpusdata
casefolding <- tm_map(corpusdata, content_transformer(tolower))

#Hanya mengambil 100 data
inspect(corpusdata[1:100])

#Data Cleaning Pra Processing
removeURL <- function(x)gsub("http[^[:space:]]*","",x)
data_URL <- tm_map(casefolding, content_transformer(removeURL))
inspect(data_URL[1:100])

remove.NL <- function(x)gsub("\n","",x)
data_NL <- tm_map(data_URL, remove.NL)
inspect(data_NL[1:100])

remove_r <- function(x)gsub("\r","",x)
data_r <- tm_map(data_NL, remove_r)
inspect(data_r[1:100])

remove_html <- function(x)gsub("<a href=' target='_blank'>","",x)
data_html <- tm_map(data_r, remove_html)
inspect(data_html[1:100])

replace.comma <- function(x)gsub(",","",x)
data_comma <- tm_map(data_html, replace.comma)
inspect(data_comma[1:100])

docs <- function(x)gsub("/","",x)
data_docs <- tm_map(data_comma, docs)
inspect(data_docs[1:100])

#Cleaning number
data_docs <- tm_map(data_docs, removeNumbers)
inspect(data_docs[1:100])

#Cleaning punctuation
data_punctuation <- tm_map(data_docs, content_transformer(removePunctuation))
inspect(data_punctuation[1:100])

stop_word <- readLines("C:/Users/USER/Desktop/magang/stoplist.csv")
data_stoplist <- tm_map(data_punctuation, removeWords, stop_word)
inspect(data_stoplist[1:100])

#Menghapus kata-kata tidak penting
tambahan_stopword <- tm_map(data_stoplist, removeWords,
                            c('yth', 'ythbapakibu', 'terima kasih', 'pd', 'ythkepala', 'assalamualaikum',
                              'selamat siang', 'bapakibu', 'selamat sore', 'selamat pagi', 'yg', 'wr',
                              'aa', 'dll', 'aslkm', 'an', 'maaf', 'terimakasih', 'mohon', 'nya', 'ucapkan',
                              'bantuan','bantuannya', 'kerjasamanya', 'timlldikti', 'ilmu', 'terima',
                              'ajuan', 'ajukan', 'selamat', 'aktif', 'dikti', 'sesuai', 'kasih', 'stikes',
                              'hormat', 'perhatiannya', 'politeknik', 'pindah', 'lampirkan', 'mengajukan',
                              'terkait', 'pendidikan', 'pengajuan', 'nama', 'pinadmin','tipe'))
inspect(tambahan_stopword[1:100])


#Menghapus spasi berlebih
data_whitespace <- tm_map(tambahan_stopword, stripWhitespace)
inspect(data_whitespace[1:100])

#Menyimpan data bersih
databersih <- data.frame(text=unlist(sapply(data_whitespace, '[')),stringsAsFactors = F)
write.csv(databersih, file ="C:/Users/USER/Desktop/magang/databersih.csv")

databersih1 <- read.csv(file.choose(), header = TRUE)
databersih1

#Tokenizing
corpustext <- Corpus(VectorSource(databersih1$text))
inspect(corpustext[1:100])

text = data_whitespace

strsplit_space_tokenizer <- function(x)
  unlist(strsplit(as.character(x), "[[:space:]]+"))
strsplit_space_tokenizer(text)

#Pembuatan wordcloud
library(wordcloud)
text <- as.character(text)
wordcloud(text, max.words = 300, random.color = TRUE,min.freq = 200,
          rot.per = 0.35,random.order = FALSE, colors = brewer.pal(8, "Dark2"))

tdm <- TermDocumentMatrix(corpustext)
inspect(tdm)

t <- removeSparseTerms(tdm, sparse = 0.95)

#Pembuatan matriks kata
library(factoextra)
m <- as.matrix(t)

freq <- rowSums(m)
freq

freq <- subset(freq, freq>=50)
barplot(freq, las=2, col = rainbow(10))
barplot







  







