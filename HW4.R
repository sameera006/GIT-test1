library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)

HDI <- read_csv(here::here("Week4", "HDR21-22_Composite_indices_complete_time_series.csv"),
                locale = locale(encoding = "latin1"),
                na = " ", skip=0)

World <- st_read("Week4/World_Countries_Generalized/World_Countries_Generalized.shp")

HDINEW <- HDI %>%
  clean_names() %>%
  select(iso3, country, gii_2019, gii_2010) %>%
  #filter(region != " ") %>%
  mutate(difference = gii_2019 - gii_2010 ) %>%
  mutate(iso_code = countrycode(country, origin = 'country.name', destination = 'iso2c' )) %>%
  mutate(iso_code2 = countrycode(iso3, origin= 'iso3c', destination = 'iso2c'))

join_HDI <- World %>%
  clean_names() %>%
  left_join(.,
            HDINEW,
            by = c ("iso" = "iso_code"))
## Plot a map
```{r}
tmap_mode("plot")
qtm(join_HDI), 
    fill = "difference")
```
## Making a map
```{r}
breaks=c(0.0,0.2,0.4,0.6,0.8,1.0)
diffbreaks=c(-0.4,-0.3,-0.2,-0.1,0,0.1)
# preserve size not direction like WGS84
joinshp = st_transform(join_HDI, crs = "+proj=moll")
# plot each map

tm1 <- tm_shape(joinshp) + 
  tm_polygons("gii_2019", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

tm2 <- tm_shape(joinshp) + 
  tm_polygons("gii_2010",
              breaks=breaks,
              palette="PuBu") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)

tm3 <- tm_shape(joinshp) + 
  tm_polygons("difference",
              #style="fixed",
              breaks=diffbreaks,
              palette=("Blues"),
               midpoint = NA)+ 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(c)", position=c(0,0.85), size=1.5)


legend <- tm_shape(joinshp) + 
  tm_polygons("hdi_2019", 
              breaks=breaks,
              palette="PuBu",
              title = "GII")+
  tm_legend(show=TRUE)+
  #asp is aspect ratio! 
  tm_layout(legend.only = TRUE, legend.position=c(0.3,0.25),asp=0.1)+
  tm_shape(joinshp) +
  tm_polygons("difference",
                 palette=("Blues"),
               midpoint = NA,
               title="HDI difference \n(2019-2010)") +
    #tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
    tm_compass(north=0, position=c(0.6,0.6))+
    tm_layout(legend.only = TRUE, legend.position=c(0.1,0.1),asp=0.1)+
    tm_credits("Mapped data:\nUN Human Development Index\nWorld outline:\nArcGIS Hub 
               ", position=c(0.35,0.2), just="left")

# in tmap can't make legends side by side unless use this workaround: 
#https://github.com/r-tmap/tmap/issues/476 

t=tmap_arrange(tm1, tm2, tm3, legend, ncol=2)

t

```

