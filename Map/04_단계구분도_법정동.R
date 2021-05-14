# shape 파일로 부터 단계구분도 그리기
# 대전 법정동 단계구분도
library(raster)
library(rgdal)
library(leaflet)

map <- shapefile('Map/map_data/LSMD_ADM_SECT_UMD_대전/LSMD_ADM_SECT_UMD_30.shp')
map <- spTransform(map, CRSobj = CRS(
    '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'
))
slotNames(map)        # SpatialPolygonsDataFrame

leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = 'yellow',
        weight = 2,
        opacity = 1,
        color = 'blue',
        dashArray = '3',
        fillOpacity = 0.7
    )

# 동별로 단계 구분도 그리기
head(map@data)    # EMD_NM
rand <- sample(100:1000, 177)
map@data$rand <- rand

pal <- colorNumeric('RdPu', NULL)
leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(rand), weight = 2, opacity = 1,
        color = 'white', dashArray = '3', fillOpacity = 0.7,
        label = ~EMD_NM
    )

leaflet(map) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addProviderTiles('Stamen.TonerLite') %>% 
    addPolygons(
        fillColor = ~pal(rand), weight = 2, opacity = 1,
        color = 'white', dashArray = '3', fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 3, color = '#999', dashArray = '',
            fillColor = 0.7, bringToFront = T),
        label = ~EMD_NM
    ) %>% 
    addLegend(pal=pal, values = ~rand, opacity = 0.7,
              title='랜덤값', position='bottomright')
