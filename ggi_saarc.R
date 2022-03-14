library(tidyverse)
library(ggbump)

df <- read.csv('data.csv')

df_rank <- df %>% 
  filter(Indicator == 'Overall Global Gender Gap Index' & Subindicator.Type == 'Rank')


saarc <- c('Bhutan', 'Bangladesh', 'India', 'Nepal', 'Maldives', 'Sri Lanka', 'Pakistan')

df_rank <- df_rank %>% 
  filter(Country.Name %in% saarc) %>% 
  select(country = Country.Name, 6:13) %>% 
  pivot_longer(X2013:X2021, names_to = 'year', values_to = 'rank' ) %>% 
  mutate(year=gsub('X', '', year)) %>%
  group_by(year) %>% 
  mutate(rank1 = rank (rank)) %>%
  drop_na(rank) %>%
  filter(year>2015, country %in% saarc)


df_rank %>% 
  drop_na(rank) %>%
  ggplot(aes(as.numeric(year), rank1, colour=country))+
  geom_bump(size=2,smooth = 50)+
  geom_point(size=7)+
  scale_y_reverse(breaks=c(7:1))+
  scale_x_continuous(limits = c(2015.5, 2022), breaks = seq(2016, 2021, 1))+
  geom_text(data = subset(df_rank, year==2016),
            aes(label=country), hjust=1.2, size=6)+
  geom_text(data = subset(df_rank, year==2021),
            aes(label=country), hjust=-.2, size=6, vjust=.5)+
  labs (title = 'Ranking of SAARC Countries by Gender Gap Index',
        subtitle = 'From 2016 to 2021',
        y = 'Rank',
        caption = 'Source: Word Economic Forum \n https://tcdata360.worldbank.org/indicators/af52ebe9?country=BRA&indicator=27962&viz=line_chart&years=2006,2021',
        x='')+
  theme(legend.position = 'none')+
  theme(plot.caption = element_text(size = 12, face = 'italic'),
        plot.subtitle = element_text(size = 22, face = 'italic', colour='Blue'))