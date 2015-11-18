library(rgdal)
library(maptools)
library(rgeos)
library(dplyr)
library(ggplot2)

county <- read.table('zcta_county_rel_10.txt', sep = ',', header = TRUE,
                     stringsAsFactors = FALSE)

zip <- readOGR(dsn = '.', 'cb_2014_us_zcta510_500k')

state <- county %>%
  filter(STATE == 1) %>%
  select(ZCTA5) %>%
  t() %>%  # transpose
  as.vector()  # change to vector

alabama <- zip[zip$ZCTA5CE10 %in% state,]

#-------

alabama@data$id <- rownames(alabama@data)

alabama.points <- fortify(alabama, region = 'id') %>%
  mutate(extra = as.factor(round(rnorm(163394), 0)))  # adding some test data to fill

alabama.df <- join(alabama.points, alabama@data, by="id")

head(alabama.df)

ggplot(alabama.df) + 
  aes(long,lat,group=group, fill = extra) + 
  geom_polygon() +
  geom_path(color="white") +
  coord_equal() +
  scale_fill_brewer("Alabama")
