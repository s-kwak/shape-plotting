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
county_shp@data$GEOID <- as.character(county_shp@data$GEOID)
county_shp@data$GEOID <- sprintf('%05d', as.numeric(county_shp@data$GEOID))
county_shp@data$STATE <- as.numeric(as.character(county_shp@data$STATEFP))  # need to go factor -> character -> numeric

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

county_shp.points <- fortify(county_shp, ID = 'id')  # melts polygons into points; each row in us_cnty was a polygon; now each row is a point tagged with which polygon it used to be
                              # NEEDED TO CHANGE "REGION" FROM TUT TO "ID"; SHOULD POINT TO WHATEVER THE IDENTIFIER IS; FIND BY USING str(county_shp@polygons)

county_shp.df <- merge(county_shp.points, county_shp@data, by='id')  # joins point data to attributes

county_shp.df2 <- merge(county_shp.df, merged,
                         by = 'GEOID', all.x = TRUE)  # merge in data to dislay

county_shp.df2$scaled_pop <- log(county_shp.df2$pop_b+1)

county_shp.df3 <- county_shp.df2[county_shp.df2$long < 0 &
                                  county_shp.df2$long > -127 &
                                  county_shp.df2$lat > 20,]  # removing outlying territories and PR

ggplot(county_shp.df3, aes(long,lat))+ 
  geom_polygon(aes(group = group, fill = scaled_pop)) +
  coord_fixed(ratio = 69/53) +   # fixes distortion; 69 mi lat for 53 mi long if center kansas
  scale_fill_gradient(low = 'white', high = 'red',
                      na.value = 'white')

#--- data - run before the all states code

data <- read.table('zcta_county_rel_10.txt', sep = ',', header = TRUE, stringsAsFactors = FALSE)
data$GEOID <- sprintf('%05d', data$GEOID)

county <- data %>%
  group_by(GEOID) %>%
  summarise(area = sum(AREALANDPT),
            pop = sum(POPPT)) %>%
  mutate(density = pop/area)

merged <- data.frame(GEOID = unique(county_shp@data$GEOID)) %>%
  merge(county, by = 'GEOID', all.x = TRUE) %>%
  mutate(density_b = ifelse(is.na(density) == TRUE, 0, density)) %>%
  mutate(pop_b = ifelse(is.na(pop) == TRUE, 0, pop)) %>%
  mutate(area_b = ifelse(is.na(area) == TRUE, 0, area)) %>%
  select(GEOID, density_b, pop_b, area_b)

merged$GEOID <- as.character(merged$GEOID)

# checks-----

ggplot(county_shp@data)+
  geom_histogram(aes(pop_b))

