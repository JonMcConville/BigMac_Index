# Setup ----

library(tidytuesdayR)
library(tidyverse)
bigmac <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-22/big-mac.csv')

write.csv(bigmac,"bigmac_index.csv",row.names=FALSE)

bigmac<-read.csv('bigmac_index.csv')

countries_by_regions <- read.csv("country_by_region.csv")

bigmac$name[bigmac$name == 'Britain'] <- 'United Kingdom'
countries_by_regions$Country[countries_by_regions$Country == 'Korea, Republic of'] <- 'South Korea'
countries_by_regions$Country[countries_by_regions$Country == "Korea, Democratic People's Republic of"] <- 'North Korea'
bigmac$name[bigmac$name == 'Moldova'] <- 'Moldova, Republic of'
countries_by_regions$Country[countries_by_regions$Country == 'Russian Federation'] <- 'Russia'
bigmac$name[bigmac$name == 'UAE'] <- 'United Arab Emirates'
bigmac$local_price <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2002-04-01', bigmac$local_price/1000000, bigmac$local_price)
bigmac$local_price <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2003-04-01', bigmac$local_price/1000000, bigmac$local_price)
bigmac$local_price <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2004-05-01', bigmac$local_price/1000000, bigmac$local_price)
bigmac$dollar_ex <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2002-04-01', bigmac$dollar_ex/1000000, bigmac$dollar_ex)
bigmac$dollar_ex <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2003-04-01', bigmac$dollar_ex/1000000, bigmac$dollar_ex)
bigmac$dollar_ex <- ifelse(bigmac$name =='Turkey' & bigmac$date == '2004-05-01', bigmac$dollar_ex/1000000, bigmac$dollar_ex)

bigmac <- cbind(bigmac, year = str_sub(bigmac$date,1,4))



cards_slim <- cbind(cards_slim, GreenWhitePhyrexian = str_count(cards_slim$manaCost,"\\{G/W/P\\}") + str_count(cards_slim$manaCost,"\\{W/G/P\\}"))


bigmac <- merge(bigmac, countries_by_regions, by.x = 'name', by.y = 'Country', all.x = TRUE)
bigmac$Region[bigmac$name == 'Euro area'] <- 'Europe'
bigmac$Global.South[bigmac$name == 'Euro area'] <- 'Global North'

# Play Area ----

test_check <- bigmac %>%
  select(year,name,dollar_price)%>%
  group_by(year, name) %>%
  summarise(Average_by_Year = mean(dollar_price))%>%
  arrange(name, year)

test_check <-  test_check %>%
  select(year,name,Average_by_Year)%>%
  group_by(name)%>%
  summarise(annual_inflation = (test_check$Average_by_Year/lag(test_check$Average_by_Year,n=1)-1))


ggplot(test_check, aes(x = year, y = annual_inflation)) +
  geom_point() +
  facet_wrap(~ Region, nrow = 3) +
  geom_smooth(mapping = aes(x = date, y = dollar_price, colour = Region)) #+
#  annotate("rect", xmin = '2008-01-01', xmax = '2012-01-01', ymin= 0, ymax = 9, alpha = .1, fill="blue")




unique(bigmac$date)

ggplot(bigmac, aes(x = date, y = dollar_price, colour = Region)) +
  geom_point() +
  facet_wrap(~ Region, nrow = 3) +
  geom_smooth(mapping = aes(x = date, y = dollar_price, colour = Region)) #+
#  annotate("rect", xmin = '2008-01-01', xmax = '2012-01-01', ymin= 0, ymax = 9, alpha = .1, fill="blue")

test_check %>%
  select(year,name)%>%
  group_by(year, name) %>%
  summarise(n=n())%>%
  arrange(desc(n))

rm(test_check)




Turkey <- bigmac %>%
  filter(name == 'Turkey')

na_rows <- bigmac[is.na(bigmac$Region), ]



ggplot(data = shape,
       aes(fill = BIR74)) +
  geom_sf() +
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) + 
  coord_sf(xlim = c(-90, NA),
           ylim = c(NA, 40))
  
