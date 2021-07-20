#Import library
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)

library(readxl)
Data <- read_excel("C:/Users/USER/Desktop/Data.xlsx")
View(Data)
head(Data, n=10)

#Memilih variabel untuk divisualisasikan
vis1 <- Data %>% 
  select(Position, Company)
vis1 <- head(vis1, n=10)
vis1

#Visualisasi dalam bentuk geom_bar
ggplot(data = vis1, mapping = aes(x=reorder(Position, Company), y=Company)) + 
  geom_bar(stat = "identity", fill="#8B0000") +
  theme(axis.text = element_text(angle = 90))

ggplot(data = vis1, mapping = aes(x=reorder(Position, Company), y=Company)) +
  geom_bar(stat = "identity", fill="#8B0000") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Position in My Own Linkedin",
       x = "Position",
       y = "Company")


#Visualisasi dalam bentuk koordinat
ggplot(data = vis1, mapping = aes(x=reorder(Position, Company), y=Company)) + 
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text = element_text(angle = 90))

ggplot(data = vis1, mapping = aes(x=reorder(Position, Company), y=Company)) +
  geom_bar(stat = "identity", fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Top Position in My Own Linkedin",
       x = "Position",
       y = "Company") +
  coord_flip()
