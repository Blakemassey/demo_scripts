if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)

ggmap::register_google(key = "AIzaSyAbyeokGINThuEHxnaT1lEB9xWS54RvdlQ",
  account_type = "premium", day_limit = 1000)

ggmap_credentials()

gmap <- get_map(location = c(lon = -72, lat = 42), maptype = "hybrid",
  source = "google", zoom = 6)
ggmap(gmap)



