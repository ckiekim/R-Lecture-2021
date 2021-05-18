library(leaflet)
library(stringr)

fest <- read.csv('project/대전축제.txt', sep='\t')
head(fest)
gu <- lapply(fest$소재지지번주소, function(x) unlist(str_split(x, ' '))[2])
fest$gu <- unlist(gu)

colors <- c('red','blue','green','purple','orange')
pal <- colorFactor(colors, domain = fest$gu) 
leaflet(fest) %>% 
    setView(lng=127.39, lat=36.35, zoom=11) %>% 
    addTiles() %>% 
    addCircles(lng=~경도, lat=~위도, label=~축제명,
               weight=1, fillColor=~pal(gu), radius=800,
               fillOpacity=0.7) %>% 
    addLegend(pal = pal, values = ~gu, position = 'bottomright', 
              title='자치구')

