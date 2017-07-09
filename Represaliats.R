library(tidyverse)
library(pdftools)
library(stringr)
library(WriteXLS)

#Author:
# Marc Belzunces (@marcbeldata)
# 8/July/2017

#Getting pdf from web and importing data
link <- "http://anc.gencat.cat/web/.content/anc/noticies/Documents/20170707_PC_Llei_11_reparacio_juridica_victimes_franquisme.pdf"
txt <- pdf_text(link)

#Building the data frame with all the data. It may take several minutes
final <- tibble()

for (i in 1:length(txt)) {
#Writing lines for every pdf page into a txt file
fileConn <- file("output.txt")
writeLines(txt[i], fileConn)
df <- read_csv("output.txt", col_names = F)

#Solving issues
index <- which(is.na(df$X2))
index2 <- index-1
df$X3 <- ""
df$X3[index2] <- df$X1[index]
df <- df[-index,]

#Spliting data to reconstruct pdf columns by patterns
df <- df %>% separate(X2, into = c("A","B"), sep = " - ")
df <- df %>% separate(B, into = c("B", "C"), sep = "\\s/\\s")

#Spliting data to reconstruct pdf columns by position, and solving issues
df2 <- df %>% 
  mutate(
    Sexe = str_extract(B, "....."),
    B = str_replace(B, ".....", ""),
    Tipus = str_extract(B, "........................."),
    B = str_replace(B, ".........................", ""),
    Procediment = str_extract(B, "........................"),
    B = str_replace(B, "........................", ""),
    Num_causa = str_extract(B, ".............."),
    B = str_replace(B, "..............", ""),
    Any_final = str_extract(C, "..........."),
    C = str_replace(C, "...........", ""),
    Pena = str_extract(C, "........................................."),
    C = str_replace(C, ".........................................", ""),
    Commutacio = str_extract(C, ".................................."),
    C = str_replace(C, "..................................", ""),
    X3 = str_replace_na(X3, ""),
    Commutacio = str_replace_na(Commutacio, ""),
    Commutacio = str_trim(Commutacio, side = "both"),
    X3 = str_trim(X3, side = "both"),
    Commutacio = str_c(Commutacio, " ", X3)
  ) %>% 
  select(-X3)

#Solving issues
index <- which(is.na(df2$Pena))
df2$Pena[index] <- df2$C[index]
df2$C[index] <- NA
df2$Commutacio[index] <- ""

#Final colnames
df2 <- df2 %>%  rename(
  Cognoms = X1,
  Nom = A,
  Any_inici = B,
  Ref = C
  ) %>% 
  select(
    Cognoms:Nom,
    Sexe:Num_causa,
    Any_inici,
    Any_final,
    Pena:Commutacio,
    Ref
  ) 

#Removing white spaces
df2 <- df2 %>% 
  mutate(
    Cognoms = str_trim(Cognoms, side = "both"),
    Nom = str_trim(Nom, side = "both"),
    Sexe = str_trim(Sexe, side = "both"),
    Tipus = str_replace(Tipus, "\\[\\]", ""),
    Tipus = str_trim(Tipus, side = "both"),
    Procediment = str_trim(Procediment, side = "both"),
    Num_causa = str_trim(Num_causa, side = "both"),
    Any_inici = str_trim(Any_inici, side = "both"),
    Any_final = str_trim(Any_final, side = "both"),
    Pena = str_trim(Pena, side = "both"),
    Commutacio = str_trim(Commutacio, side = "both"),
    Ref = str_trim(Ref, side = "both")
  ) %>% 
  filter(!is.na(Sexe))

#Merging data from every page
final <- rbind(final,df2)
}

#Solving issues
index2 <- which(startsWith(final$Cognoms, "\""))
final2 <- final[-index2,]
final2 <- final2 %>% 
  mutate(
    Num = row_number()
  )

#Some unsophisticated manual substitutions to solve issues
final2[39386,c(6:11)] <- c("004275,002653", "1939", "1939", "Reclusió perpètua", "", "3210")

final2$Commutacio[final2$Commutacio == "e"] <- "Executat"
final2$Commutacio[final2$Commutacio == "ex"] <- "Executat"
final2$Commutacio[final2$Commutacio == "exe"] <- "Executat"
final2$Commutacio[final2$Commutacio == "exec"] <- "Executat"
final2$Commutacio[final2$Commutacio == "execu"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executa"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executatc"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executatcu"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executatcut"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executatcuta"] <- "Executat"
final2$Commutacio[final2$Commutacio == "executatFrancisco"] <- "Executat"
final2$Commutacio[29740] <- "Executat"
final2$Commutacio[47093] <- "Executat"

index <- which(startsWith(final2$Ref, "executat/da "))
final2$Commutacio[index] <- "Executat"

final2 <- final2 %>% 
  mutate(
    Ref = str_replace(Ref, "executat/da ", ""),
    Ref = str_replace(Ref, "xecutat/da ", ""),
    Ref = str_replace(Ref, "ecutat/da ", ""),
    Ref = str_replace(Ref, "cutat/da ", ""),
    Ref = str_replace(Ref, "utat/da ", ""),
    Ref = str_replace(Ref, "tat/da ", ""),
    Ref = str_replace(Ref, "at/da ", ""),
    Ref = str_replace(Ref, "t/da ", "")
  )

final2$Commutacio[64223] <- str_c(final2$Ref[64223], " ", final2$Commutacio[64223])
final2$Ref[64223] <- "2554"

#Saving general data
write_csv(final2, "Repressaliats pel franquisme a Catalunya.csv")
WriteXLS(final2, "Repressaliats pel franquisme a Catalunya.xlsx")

#Executed
executed <- final2 %>% 
  filter(Commutacio == "Executat")

write_csv(executed, "Executats pel franquisme a Catalunya.csv")
WriteXLS(executed, "Executats pel franquisme a Catalunya.xlsx")



