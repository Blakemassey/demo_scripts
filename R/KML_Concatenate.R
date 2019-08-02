suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tictoc))

outfile <- "Results/Tracks_Test.kml"
sf <- stamp("2018-05-15T14:34:35-05")
datetime_start <- dmy_hms("1 Jan 2018 04:59:00")

xml_header <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n',
'<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2">\n',
'<Document>\n<Style id="arrival">\n<IconStyle>\n<Icon>\n',
'<href>http://earth.google.com/images/kml-icons/track-directional/track-0.png</href>\n',
'</Icon>\n</IconStyle>\n<LineStyle>\n<color>ff0000ff</color>\n<width>3</width>',
'\n</LineStyle>\n<PolyStyle>\n<color>7fffffff</color>\n</PolyStyle>\n</Style>\n')

home_folder_name <- paste0('<name>Flight Tracks</name>\n')
start_inner_folder <- paste0('<Folder>\n<name>Arrivals</name>\n')

placemarks <- vector(mode = "character", length = 0)
long_start <- -81.5
lat_start <- 26.0

ids <- c(1:10)
timesteps <- 2000

tic()
for (i in 1:length(ids)){
  print(paste0("starting id# ", i))
  placemark_start <- paste0('<Placemark>\n<name>', ids[i], '</name>\n',
    '<adflag>A</adflag>\n<styleUrl>#arrival</styleUrl>\n')
  gxtrack_start <- paste0(
    '<gx:Track>\n\t<altitudeMode>absolute</altitudeMode>\n',
    '\t<extrude>1</extrude>\n\t<altitudeMode>absolute</altitudeMode>\n',
    '\t<extrude>1</extrude>\n')
  whens <- vector(mode = "character", length = 0)
  coords <- vector(mode = "character", length = 0)

  datetime <- datetime_start
  long <- long_start
  lat <- lat_start + (ids[i]*.4)

  for (i in (1:timesteps)){
      datetime <- datetime + duration(5, units = "minutes")
      # whens <- append(whens, paste0('\t<when>',sf(datetime),'</when>\n'))
      whens <- paste0(whens, collapse="")
  }
  whens <- paste0(whens, collapse = "")
  for (i in (1:timesteps)){
      long <- round(long + runif(1, .01, .10), 5)
      lat <- round(lat, 2)
      coords <- paste0(coords, '\t<gx:coord>',long, ' ', lat, ' 2000',
        '</gx:coord>\n')
  }
  coords <- paste0(coords, collapse="")
  track_end <- paste0('</gx:Track></Placemark>\n')
  placemarks <- append(placemarks, paste0(placemark_start, gxtrack_start, whens,
    coords, track_end))
}
toc()

document_end <-paste0('</Folder>\n</Document>\n</kml>')

full_kml <- paste0(xml_header, home_folder_name, start_inner_folder,
  paste0(placemarks, collapse="\n"), document_end)

cat(full_kml, file = outfile, sep = "")
