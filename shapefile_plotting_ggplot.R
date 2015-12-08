library(rgdal)
library(dplyr)
library(ggplot2)

#------- Plot ZCTA Shapes (1 state)

zip <- readOGR(dsn = '.', 'cb_2014_us_zcta510_500k')  # zcta shapes

county <- read.table('zcta_county_rel_10.txt', sep = ',', header = TRUE,
                     stringsAsFactors = FALSE)

state <- county %>%
  filter(STATE == 1) %>%
  select(ZCTA5) %>%
  t() %>%  # transpose
  as.vector()  # change to vector

alabama <- zip[zip$ZCTA5CE10 %in% state,]

alabama@data$id <- rownames(alabama@data)

alabama.points <- fortify(alabama, region = 'id') %>%
  mutate(extra = as.factor(round(rnorm(163394), 0)))  # adding some test data to fill

alabama.df <- merge(alabama.points, alabama@data, by='id')

ggplot(alabama.df) + 
  aes(long,lat,group=group, fill = extra) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  scale_fill_brewer("Alabama")


#---------- Plot County Shapes

county_shp <- readOGR(dsn = '.', 'cb_2014_us_county_500k')  # county shapes
county_shp@data$id <- rownames(county_shp@data)
county_shp@data$STATE <- as.numeric(county_shp@data$STATEFP)

# one state

alabama_cnty <- county_shp[county_shp$STATE == 1,]

alabama_cnty@data$id <- rownames(alabama_cnty@data)

alabama_cnty.points <- fortify(alabama_cnty, region = 'id') %>%
  mutate(extra = as.factor(round(rnorm(21324), 0)))  # adding some test data to fill

alabama_cnty.df <- merge(alabama_cnty.points, alabama_cnty@data, by='id')

ggplot(alabama_cnty.df) + 
  aes(long,lat,group=group, fill = extra) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_equal() +
  scale_fill_brewer("Alabama")

# all states

us_cnty <- county_shp[county_shp$STATE < 60 & county_shp$STATE != 2,]  # remove AK and outlying

us_cnty.points <- fortify(us_cnty, region = 'id') %>%  # melts polygons into points; each row in us_cnty was a polygon; now each row is a point tagged with which polygon it used to be
  mutate(extra = as.factor(round(rnorm(921668), 0)))  # adding some test data to fill

us_cnty.df <- merge(us_cnty.points, us_cnty@data, by='id')  # joins point data to attributes

us_cnty.df2 <- us_cnty.df[us_cnty.df$long < 0 &
                          us_cnty.df$long > -140 &
                          us_cnty.df$lat > 20,]  # removing outlying territories and PR

ggplot(us_cnty.df2) + 
  aes(long,lat,group=group, fill = extra) + 
  geom_polygon() +
  geom_path(color="black") +
  coord_fixed(ratio = 69/53) +  # fixes distortion; 69 mi lat for 53 mi long if center kansas
  scale_fill_brewer("USA")


