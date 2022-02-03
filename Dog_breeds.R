pack <- c("tidytuesdayR", "tidyverse", "ggimage", "ggtext", "showtext", "ragg")
(function(pack){
  sapply(pack, function(pack) if(!pack %in% installed.packages()){
    install.packages(pack, dependencies = T)
  })
  sapply(pack, library, character.only=T)
})(pack)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

#The most important factor for the ranking of the dog

#There was some problem with Breed names and joint them directly was not working
Breeds <- breed_rank_all %>%
  arrange(Breed) %>%
  mutate(nrow=row_number()) %>% 
  right_join(
    breed_traits %>% 
      arrange(Breed) %>%
      mutate(nrow=row_number()),
    by="nrow") %>% 
  select(-c(Breed.y, nrow)) %>% 
  relocate(links, Image, .after=last_col()) %>% rename(Breed= 1)



Avg_top_10 <- Breeds %>%
  select(which(grepl("Rank",names(.))), Breed) %>% 
  pivot_longer(cols=which(grepl("Rank",names(.))),
               values_to = "Rank") %>% 
  group_by(Breed) %>% 
  summarise(Rank_mean=mean(Rank)) %>% 
  slice_min(Rank_mean, n=10)


#Which dogs grew most in popularity from each year? 
#and which lost more popularity? 
#What are the differences between them?
  
Breeds_variation <- Breeds %>%
  select(Breed, which(grepl("Rank",names(.)))) %>% 
  pivot_longer(cols=which(grepl("Rank",names(.))),
               values_to = "Rank",
               names_to = "Year") %>%
  mutate(Year= parse_number(Year)) %>% 
  group_by(Breed) %>% 
  mutate(Rank_var= c(NA,diff(Rank)))

pop_var <- Breeds_variation %>%
  ungroup() %>%
  group_by(Year) %>%
  slice_max(Rank_var, n=5, with_ties = F) %>%
  rbind(Breeds_variation %>%
          ungroup() %>%
          group_by(Year) %>% 
          slice_min(Rank_var, n=5, with_ties = F)) %>% 
  #Remove 2014 year para dar um facet com número par
  filter(Year !=2014) %>% 
  #Adc as imagens
  right_join(Breeds %>% select(Image, Breed), by="Breed") %>% 
  drop_na() %>% 
  #Adc cor
  mutate(cor= ifelse(Rank_var <0, "#C23664", "#0B666D"),
         #Adc uma coluna para o hjust dos nomes
         middle_coord=Rank_var/2)
  

font_add_google(name = "Roboto", family = "Roboto")

showtext_auto()



Dog_fn <- function(x){
  
  a <- pop_var %>% filter(Year==x)  
  a %>%  ggplot(aes(x=reorder(Breed, Rank_var), y=Rank_var))+
    geom_hline(yintercept=0, linetype=2, size=.5, color="#FBBF59")+
    geom_text(aes(y=middle_coord, label=Breed), vjust=-1, colour = "#0B5A99", size=10)+
    geom_segment(aes(x=reorder(Breed, Rank_var), xend=reorder(Breed, Rank_var), y=0, yend=Rank_var), 
                 color=a$cor)+
    geom_point(aes(x=reorder(Breed, Rank_var), y=Rank_var),
               size=15, pch=21, bg="white", color=a$cor)+
    labs(x=NULL, y=NULL,
         title= paste("Cachorros que <span style=color:#C23664>perderam</span> e 
       <span style= color:#0B666D>ganharam</span> maior popularidade em ", x),
         caption= "Source: American Kennel Club, courtesy of KKakey")+
    theme_minimal()+
    theme(
      axis.text.y = element_blank(),
      panel.grid.major.y =element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_line(linetype=2),
      plot.title=element_markdown(family = "Roboto", size = 52, hjust = 0.5, vjust=2, face = "bold"),
      axis.text.x= element_text(family = "Roboto", size = 28, hjust = 0.5, vjust=2, colour = "#0B5A99"),
      plot.caption = element_text(size=20),
      aspect.ratio = 1/1.61)+
    geom_image(aes(image=Image), size=.04)+
    coord_flip()

  
  
  ggsave(paste0("Dogs.",x, ".png"), width = 8, height = 6, units = "in", bg="white")
  

}
Dog_fn(2020)
  






