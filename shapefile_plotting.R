# dont use; library(maptools)
library(dplyr)
library(rgdal)  # use readOGR so that it preserves aspect ratio

# https://www.census.gov/geo/maps-data/data/zcta_rel_download.html has information about ZCTAs
# get shapefiles from https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html

zip <- readOGR('.', 'cb_2014_us_zcta510_500k')
# (reading with shapespatial) zip <- readShapeSpatial('cb_2014_us_zcta510_500k.shp')
cbsa <- read.table('zcta_cbsa_rel_10.txt', sep = ',', header = TRUE,
                   stringsAsFactors = FALSE)
county <- read.table('zcta_county_rel_10.txt', sep = ',', header = TRUE,
                   stringsAsFactors = FALSE)

# plotting country except for hawaii and AK
# note: alaska is 2, hawaii is 15

test_state <- county %>%
  filter(STATE != 2) %>%
  filter(STATE != 15) %>%
  select(ZCTA5) %>%
  t() %>%  # transpose
  as.vector()  # change to vector

test <- zip[zip$ZCTA5CE10 %in% test_state,]
plot(test)

# merge in some extra data

extra_data <- data.frame(ZCTA = zip$ZCTA5CE10,
                         info = round(runif(33144, 1, 5), 0))

# visualize

# create parallel data frame that matches records in shape file and assign class

classes <- cut(extra_data$info, c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))  # parallel, categorizes into factors
levels(classes) <- c('one', 'two', 'three', 'four', 'five')  # gives the factors above names
colors <- c('#73AC97', '#489074', '#267356', '#0E563B', '#003924')  # matches each factor with a color

state <- county %>%
  filter(STATE == 1) %>%
  select(ZCTA5) %>%
  t() %>%  # transpose
  as.vector()  # change to vector

alabama <- zip[zip$ZCTA5CE10 %in% state,]

# set plot area parameters

par(bg = 'lightgray')

plot(alabama, border = 'darkgray', col = colors[classes])
