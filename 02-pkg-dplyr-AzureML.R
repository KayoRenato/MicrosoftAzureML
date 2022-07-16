# Variável que controla a execução do script
Azure = FALSE

if(Azure){
  restaurantes <- maml.mapInputPort(1)
  ratings <- maml.mapInputPort(2) 
}else{
  restaurantes  <- read.csv("Restaurant-features.csv", sep = ",", header = T, stringsAsFactors = F )
  ratings <- read.csv("Restaurant-ratings.csv", sep = ",", header = T, stringsAsFactors = F)
}

# Filtrando o dataset restaurantes
restaurantes <- restaurantes[restaurantes$franchise == 'f' & restaurantes$alcohol != 'No_Alcohol_Served', ]

require(dplyr)

# Combinando os datasets com base em regras
df <- as.data.frame(restaurantes %>%
                      inner_join(ratings, by = 'placeID') %>%
                      select(name, rating) %>%
                      group_by(name) %>%
                      summarize(ave_Rating = mean(rating)) %>%
                      arrange(desc(ave_Rating))) 
df

if(Azure) maml.mapOutputPort("df")


