
# Code for Quantifying stereotypes of Romanians in American print media using word embeddings
# MSc Computational Social Science Thesis
# Nicoleta Bobescu

## Setup

# Clearing workspace and freeing memory
rm(list = ls())
gc()

# Setting working directory
getwd()
setwd("C:/Users/Your/Directory/Here")

# Installing and loading dependencies
install.packages("remotes")
install.packages("devtools")
remotes::install_github("bmschmidt/wordVectors")
install.packages("magrittr")
install.packages("dplyr")
install.packages("word2vec")
install.packages("sweater")
install.packages("knitr")
install.packages("kableExtra")

library(magrittr)
library(dplyr)
library(word2vec)
library(wordVectors)
library(sweater)
library(knitr)
library(kableExtra)

## Loading and preparing data

# Google Books Ngram American English 2012 corpus (version 2)
vectors <- read.csv("US_Ngrams_2000_12.csv") # loading file (Warning: may take several minutes!)
rownames(vectors) <- vectors$X # moving vocabulary data to rownames
vectors <- vectors[, -1] # deleting extra vocabulary data left behind
vectors <- as.matrix(vectors) # converting to matrix
vectors <- as.VectorSpaceModel(vectors) # converting to vector space model

# Checking behavior with example from Machine Learning Lab 4
kingtowoman <- vectors[rownames(vectors)=="king",] -
  vectors[rownames(vectors)=="man",] +
  vectors[rownames(vectors)=="woman",]

word2vec::word2vec_similarity(x = matrix(kingtowoman,nrow = 1),
                              y = vectors,
                              top_n = 20,
                              type = "cosine")
# Good!

# Setting group words for target groups (as Garg et al. 2018)
# Romanian
surnames_romanian <- c("popescu", "radu", "dumitru", "stoica", "gheorghe", "matei", "rusu"," mihai", "ciobanu", "constantin", "ionescu", "florea", "ilie", "stanciu", "munteanu", "vasile", "oprea", "tudor", "sandu", "ungureanu", "dinu", "andrei", "serban", "neagu", "cristea", "anghel", "dragomir", "enache", "badea", "stefan", "mocanu", "iordache", "coman", "cojocaru", "grigore", "voicu", "dobre", "petre", "lupu", "lungu", "ionita", "iancu", "nicolae", "balan", "nistor", "stoian", "avram", "pavel", "simion", "iacob")

# Checking each surname's position in the vector space (missing words have integer(0))
positions_romanian <- sapply(surnames_romanian, function(word) which(rownames(vectors) == word))

# Unlisting for easier visualization, which also removes missing surnames
positions_romanian <- unlist(positions_romanian)

# Making into a data frame, arranging by descending frequency and taking the first 20 surnames
positions_romanian <- tibble(Name = names(positions_romanian), Position = positions_romanian) %>% 
  arrange(Position) %>%
  head(20)

# Overriding over previous list with most common Romanian surnames that are also most frequent in the vector space
surnames_romanian <- positions_romanian$Name 

###

# Albanian
surnames_albanian <- c("hoxha", "shehu", "dervishi", "prifti", "elezi", "muca", "gjoni", "kurti", "marku", "gjoka", "hasani", "lleshi", "mema", "osmani", "halili", "gega", "mehmeti", "toska", "beqiri", "murati", "hoxhaj", "deda", "gjini", "agolli", "hyseni", "tafa", "karaj", "sinani", "kraja", "sulaj", "ismaili", "basha", "cenaj", "shabani", "cani", "aliaj", "kodra", "spahiu", "domi", "hyka", "bardhi", "doda", "arapi", "tabaku", "malaj", "bushi", "bregu", "balliu", "ibrahimi", "zeneli", "biba", "doci")

# Checking each surname's position in the vector space (missing words have integer(0))
positions_albanian <- sapply(surnames_albanian, function(word) which(rownames(vectors) == word))

# Unlisting for easier visualization, which also removes missing surnames
positions_albanian <- unlist(positions_albanian)

# Making into a data frame, arranging by descending frequency and taking the first 20 surnames
positions_albanian <- tibble(Name = names(positions_albanian), Position = positions_albanian) %>% 
  arrange(Position) %>%
  head(20)

# Overriding over previous list with most common Albanian surnames that are also most frequent in the vector space
surnames_albanian <- positions_albanian$Name 

###

# Bulgarian
surnames_bulgarian <- c("ivanov", "ivanova", "georgieva", "georgiev", "dimitrova", "dimitrov", "petrova", "petrov", "nikolova", "nikolov", "stoyanova", "hristova", "stoyanov", "todorova", "hristov", "todorov", "ilieva", "angelova", "iliev", "angelov", "atanasova", "atanasov", "vasileva", "vasilev", "yordanova", "yordanov", "petkova", "petkov", "marinova", "koleva", "marinov", "kolev", "stefanova", "stefanov", "mihaylova", "mihaylov", "asenov", "asenova", "kostova", "krasteva", "dimova", "kostadinov", "kostadinova", "miteva", "borisov", "krastev", "kostov", "dimov", "aleksandrova", "borisova")

# Checking each surname's position in the vector space (missing words have integer(0))
positions_bulgarian <- sapply(surnames_bulgarian, function(word) which(rownames(vectors) == word))

# Unlisting for easier visualization, which also removes missing surnames
positions_bulgarian <- unlist(positions_bulgarian)

# Making into a data frame, arranging by descending frequency and taking the first 20 surnames
positions_bulgarian <- tibble(Name = names(positions_bulgarian), Position = positions_bulgarian) %>% 
  arrange(Position) %>%
  head(20)

# Overriding over previous list with most common Bulgarian surnames that are also most frequent in the vector space
surnames_bulgarian <- positions_bulgarian$Name 

###

# Czech
surnames_czech <- c("novakova", "novak", "svobodova", "svoboda", "novotna", "novotny", "dvorakova", "dvorak", "cerna", "prochazkova", "prochazka", "vesela", "horakova", "vesely", "horak", "marek", "pokorna", "markova", "pospisil", "hajek", "benesova", "kralova", "benes", "fialova", "zemanova", "kral", "zeman", "dolezalova", "hajkova", "dolezal", "navratil", "navratilova", "urbanova", "ruzicka", "nemec", "kopecka", "blazkova", "blazek", "musil", "bartosova", "musilova", "simkova", "machova", "nemcova", "kadlec", "holubova", "kopecky", "kadlecova", "blahova", "holub")

# Checking each surname's position in the vector space (missing words have integer(0))
positions_czech <- sapply(surnames_czech, function(word) which(rownames(vectors) == word))

# Unlisting for easier visualization, which also removes missing surnames
positions_czech <- unlist(positions_czech)

# Making into a data frame, arranging by descending frequency and taking the first 20 surnames
positions_czech <- tibble(Name = names(positions_czech), Position = positions_czech) %>% 
  arrange(Position) %>%
  head(20)

# Overriding over previous list with most common Czech surnames that are also most frequent in the vector space
surnames_czech <- positions_czech$Name

###

# Hungarian
surnames_hungarian <- c("toth", "nagy", "szabo", "kovacs", "varga", "horvath", "molnar", "nemeth", "farkas", "takacs", "balogh", "juhasz", "szilagyi", "meszaros", "szucs", "fekete", "torok", "racz", "olah", "szalai", "feher", "pinter", "balazs", "kocsis", "lakatos", "fodor", "vincze", "sandor", "veres", "magyar", "kis", "sipos", "katona", "kiraly", "hegedus", "lukacs", "voros", "szoke", "gulyas", "fazekas", "varadi", "somogyi", "kelemen", "fulop", "laszlo", "orosz", "szekely", "jakab", "balint", "deak")

# Checking each surname's position in the vector space (missing words have integer(0))
positions_hungarian <- sapply(surnames_hungarian, function(word) which(rownames(vectors) == word))

# Unlisting for easier visualization, which also removes missing surnames
positions_hungarian <- unlist(positions_hungarian)

# Making into a data frame, arranging by descending frequency and taking the first 20 surnames
positions_hungarian <- tibble(Name = names(positions_hungarian), Position = positions_hungarian) %>% 
  arrange(Position) %>%
  head(20)

# Overriding over previous list with most common Hungarian surnames that are also most frequent in the vector space
surnames_hungarian <- positions_hungarian$Name 

#

## Analysises: bias toward Romanians using Word Embedding Association Test (as in Caliskan et al. 2017)

# Top negative areas of stereotypes: criminality, morality, wealth + pleasantness (as in Caliskan et al. 2017)
# Area 1: Criminality

crime <- vectors %>% closest_to("crime", n = 20)
crime <- crime$word

vectors %>% closest_to("lawful", n = 25)
vectors %>% closest_to("lawfully", n = 25)
vectors %>% closest_to("legal", n = 25)
vectors %>% closest_to("legally", n = 25)

lawful <- c("lawful", "legitimate", "lawfully", "permissible", "justifiable", "rightful", "legal", "legally", "proper", "constitutionally",
            "reasonable", "peaceable", "constitutional", "ethical", "statutory", "moral", "duly", "legitimately", "morally", "ethically")

# Romanian vs. Albanian
weat_criminality_RO_ALB <- query(vectors, S_words = crime, T_words = lawful, A_words = surnames_romanian, B_words = surnames_albanian, method = "weat")
weat_criminality_RO_ALB
weat_resampling(weat_criminality_RO_ALB)

# Romanian vs. Bulgarian
weat_criminality_RO_BG <- query(vectors, S_words = crime, T_words = lawful, A_words = surnames_romanian, B_words = surnames_bulgarian, method = "weat")
weat_criminality_RO_BG
weat_resampling(weat_criminality_RO_BG)

# Romanian vs. Czech
weat_criminality_RO_CZE <- query(vectors, S_words = crime, T_words = lawful, A_words = surnames_romanian, B_words = surnames_czech, method = "weat")
weat_criminality_RO_CZE
weat_resampling(weat_criminality_RO_CZE)

# Romanian vs.Hungarian
weat_criminality_RO_HU <- query(vectors, S_words = crime, T_words = lawful, A_words = surnames_romanian, B_words = surnames_hungarian, method = "weat")
weat_criminality_RO_HU
weat_resampling(weat_criminality_RO_HU)

# Area 2: morality

vectors %>% closest_to("moral", n = 20)
vectors %>% closest_to("morality", n = 20)
vectors %>% closest_to("virtue", n = 20)
vectors %>% closest_to("probity", n = 20)
vectors %>% closest_to("righteous", n = 20)

# Checking that words from thesaurus are in the vector space
a <- c("moral", "goodness", "honor", "integrity", "righteousness", "uprightness", "virtue", "fairness", "rectitude", "probity")
a %in% rownames(vectors)

high_morality <- c("moral", "ethical", "morality", "morals", "ethics", "decency", "virtue", "benevolence", "virtues", "prudence", "nobleness", "rectitude", "beneficence", "probity", "honesty", "goodness", "honor", "integrity", "righteousness", "fairness")


vectors %>% closest_to("immoral", n = 25)

# Checking that words from thesaurus are in the vector space
b <- c("infamous", "unprincipled", "nefarious", "obscene", "corrupt", "heinous", "licentious", "evil", "profligate", "wrong")
b %in% rownames(vectors)

low_morality <- c("immoral", "unethical", "reprehensible", "undesirable", "indecent", "heinous", "dishonest", "impure", "depraved", "unjust", "sinful", "hurtful", "disgraceful", "improper", "dishonorable", "infamous", "unprincipled", "nefarious", "obscene", "corrupt")

# Romanian vs. Albanian
weat_morality_RO_ALB <- query(vectors, S_words = low_morality, T_words = high_morality, A_words = surnames_romanian, B_words = surnames_albanian, method = "weat")
weat_morality_RO_ALB
weat_resampling(weat_morality_RO_ALB)

# Romanian vs. Bulgarian
weat_morality_RO_BG <- query(vectors, S_words = low_morality, T_words = high_morality, A_words = surnames_romanian, B_words = surnames_bulgarian, method = "weat")
weat_morality_RO_BG
weat_resampling(weat_morality_RO_BG)

# Romanian vs. Czech
weat_morality_RO_CZE <- query(vectors, S_words = low_morality, T_words = high_morality, A_words = surnames_romanian, B_words = surnames_czech, method = "weat")
weat_morality_RO_CZE
weat_resampling(weat_morality_RO_CZE)

# Romanian vs.Hungarian
weat_morality_RO_HU <- query(vectors, S_words = low_morality, T_words = high_morality, A_words = surnames_romanian, B_words = surnames_hungarian, method = "weat")
weat_morality_RO_HU
weat_resampling(weat_morality_RO_HU)
```

# Area 3: wealth
vectors %>% closest_to("poor", n = 25)
vectors %>% closest_to("wretched", n = 25)
vectors %>% closest_to("penniless", n = 25)

# Checking that words from thesaurus are in the vector space
c <- c("penniless", "impoverished", "destitute", "disadvantaged", "broke", "deprived", "strapped", "poverty-stricken", "skint", "beggarly")
c %in% rownames(vectors)

poor_words <- c("poor", "poorer", "wretched", "needy", "poorest", "unfortunate", "downtrodden", "miserable", "penniless", "impoverished", "destitute", "disadvantaged", "broke", "deprived", "strapped", "skint", "beggarly", "famished", "helpless", "beggar")

vectors %>% closest_to("rich", n = 25)
vectors %>% closest_to("wealthy", n = 25)
vectors %>% closest_to("prosperous", n = 25)

# Checking that words from thesaurus are in the vector space
d <- c("loaded", "flush", "moneyed", "well-off", "successful", "thriving", "comfortable", "flourishing", "fortunate", "lucky")
d %in% rownames(vectors)

rich_words <- c("rich", "richer", "wealthy", "richest", "prosperous", "affluent", "wealthier", "opulent", "wealthiest", "profitable", "prosperity", "luxurious", "wealthier", "loaded", "flush", "moneyed", "successful", "thriving", "flourishing", "fortunate")

# Romanian vs. Albanian
weat_wealth_RO_ALB <- query(vectors, S_words = poor_words, T_words = rich_words, A_words = surnames_romanian, B_words = surnames_albanian, method = "weat")
weat_wealth_RO_ALB
weat_resampling(weat_wealth_RO_ALB)

# Romanian vs. Bulgarian
weat_wealth_RO_BG <- query(vectors, S_words = poor_words, T_words = rich_words, A_words = surnames_romanian, B_words = surnames_bulgarian, method = "weat")
weat_wealth_RO_BG
weat_resampling(weat_wealth_RO_BG)

# Romanian vs. Czech
weat_wealth_RO_CZE <- query(vectors, S_words = poor_words, T_words = rich_words, A_words = surnames_romanian, B_words = surnames_czech, method = "weat")
weat_wealth_RO_CZE
weat_resampling(weat_wealth_RO_CZE)

# Romanian vs.Hungarian
weat_wealth_RO_HU <- query(vectors, S_words = poor_words, T_words = rich_words, A_words = surnames_romanian, B_words = surnames_hungarian, method = "weat")
weat_wealth_RO_HU
weat_resampling(weat_wealth_RO_HU)

# Area 4: Pleasantness (as in Caliskan et al. 2017)

pleasant <- c("caress", "freedom", "health", "love", "peace", "cheer", "friend", "heaven", "loyal", "pleasure", "diamond", "gentle", "honest", "lucky", "rainbow", "diploma", "gift", "honor", "miracle", "sunrise", "family", "happy", "laughter", "paradise", "vacation")

unpleasant <- c("abuse", "crash", "filth", "murder", "sickness", "accident", "death", "grief", "poison", "stink", "assault", "disaster", "hatred", "pollute", "tragedy", "divorce", "jail", "poverty", "ugly", "cancer", "kill", "rotten", "vomit", "agony", "prison")

# Checking that words from paper are in the vector space
pleasant %in% rownames(vectors)
unpleasant %in% rownames(vectors)

# Romanian vs. Albanian
weat_pleasant_RO_ALB <- query(vectors, S_words = unpleasant, T_words = pleasant, A_words = surnames_romanian, B_words = surnames_albanian, method = "weat")
weat_pleasant_RO_ALB
weat_resampling(weat_pleasant_RO_ALB)

# Romanian vs. Bulgarian
weat_pleasant_RO_BG <- query(vectors, S_words = unpleasant, T_words = pleasant, A_words = surnames_romanian, B_words = surnames_bulgarian, method = "weat")
weat_pleasant_RO_BG
weat_resampling(weat_pleasant_RO_BG)

# Romanian vs. Czech
weat_pleasant_RO_CZE <- query(vectors, S_words = unpleasant, T_words = pleasant, A_words = surnames_romanian, B_words = surnames_czech, method = "weat")
weat_pleasant_RO_CZE
weat_resampling(weat_pleasant_RO_CZE)

# Romanian vs.Hungarian
weat_pleasant_RO_HU <- query(vectors, S_words = unpleasant, T_words = pleasant, A_words = surnames_romanian, B_words = surnames_hungarian, method = "weat")
weat_pleasant_RO_HU
weat_resampling(weat_pleasant_RO_HU)
