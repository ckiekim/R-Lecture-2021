# 단계 구분도(Choropleth)
library(geojsonio)
library(leaflet)
library(stringr)

map <- geojson_read('Map/map_data/daejon.json', what='sp')
leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons()
class(map)   # SpatialPolygonsDataFrame
slotNames(map)
map@data

city <- read.csv('Map/대전자치단체.csv', fileEncoding='utf-8')[-1,]
city

# 연속적인 값의 팔레트
pal <- colorNumeric('RdPu', NULL)
leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(city$pop),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~city$place
    )

# map 데이터와 city 데이터를 통합하면 좋지 않을까
names(map@data)
names(city)
city$name <- lapply(city$place,
                    function(x) substring(x, 1, str_length(x)-1))

# city의 데이터 순서를 거꾸로 만들고 테스트
city <- city[c(5,4,3,2,1),]
city
pal <- colorNumeric('YlOrRd', NULL)
leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(city$pop),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~city$name
    )

# map@data와 city 를 merge해서 하나의 데이터로 처리
map@data <- merge(map@data, city, by='name', sort = F)

leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(pop),
        weight = 2,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~name
    )

# 단계를 내 맘대로 정하기
bins <- c(20, 25, 30, 40, 50) * 10000
pal <- colorBin('PuBuGn', domain = map@data$pop, bins=bins)
leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(pop), weight = 2, opacity = 1,
        color = 'white', dashArray = '3', fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5, color = '#999', dashArray = '',
            fillColor = 0.7, bringToFront = T
        ),
        label = ~name
    ) %>% 
    addLegend(pal=pal, values = ~pop, opacity = 0.7,
              title='인구수', position='bottomright')
