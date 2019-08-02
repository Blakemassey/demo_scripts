# --- ColorPalettes Script -----------------------------------------------------
# General functions to plot a series of color palettes. Specifically, most of
# palettes are from the 'RColorBrewer' Package
# ------------------------------------------------------------------------------
library(colorRamps)
library(RColorBrewer)
library(plotKML)
library(plot3D)
library(baear)
data(worldgrids_pal)
par(mfrow = c(1, 1))

### Collections of color palettes ----------------------------------------------

# Interactive color palette visualizer
tmaptools::palette_explorer

# Function for color ramp palettes
n = 20
PlotColorPie(colorRampPalette(c("red", "green"))(n))

# Functions for specific palettes
n = 20
PlotColorPie(rainbow(n))
PlotColorPie(heat.colors(n))
PlotColorPie(gg.col(n))
PlotColorPie(gg2.col(n))
PlotColorPie(jet.col(n))
PlotColorPie(jet2.col(n))
PlotColorPie(terrain.colors(n))
PlotColorPie(topo.colors(n))
PlotColorPie(cm.colors(n))

# Package 'plotKML' palettes
PlotColorPalette(R_pal, sel=1:10)
PlotColorPalette(R_pal, sel=11:12) #R_pal has 12 palettes
PlotColorPie(R_pal[[1]])

PlotColorPalette(SAGA_pal, sel=1:10)
PlotColorPalette(SAGA_pal, sel=11:20)
PlotColorPalette(SAGA_pal, sel=21:22) #SAGA_pal has 22 palettes
PlotColorPie(SAGA_pal[[1]])

PlotColorPalette(worldgrids_pal, sel=1:10) #Worldgrids has 10 palettes
PlotColorPie(worldgrids_pal[[4]])

# RColorBrewer palettes
display.brewer.all()

### RColorBrewer Palettes ------------------------------------------------------

display.brewer.all()
display.brewer.all(type="seq") #max n is 9
display.brewer.all(type="div") #max n is 11
display.brewer.all(type="qual") #max n 8-12
display.brewer.pal(9,"Set1")
display.brewer.pal(8,"Set2")
display.brewer.pal(12,"Set3")
display.brewer.pal(8,"Dark2")
display.brewer.pal(8,"Accent")
n = 20
colors = 9
b_pal = "Dark2"
PlotColorPie(colorRampPalette(brewer.pal(colors,b_pal))(n))
PlotColorPie(colorRampPalette(brewer.pal(colors,"Spectral"))(n))
### Difference between RColorBrewer palettes and others ------------------------

n = 18
PlotColorPie(colorRampPalette(R_pal[[1]])(n))
  # Rainbow blends together
PlotColorPie(colorRampPalette(brewer.pal(colors,"Set1"))(n))
  # Very distinctive
PlotColorPie(colorRampPalette(brewer.pal(9,"Set1"))(21))
colors <- colorRampPalette(brewer.pal(9,"Set1"))(21)
colors

### Plot of All Shapes  --------------------------------------------------------
d=data.frame(p=c(0:25,32:127))
ggplot() +
scale_y_continuous(name="") +
scale_x_continuous(name="") +
scale_shape_identity() +
geom_point(data=d, mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="red")+
geom_text(data=d, mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)


## Create PDF of R Colors
pdf("rcolorsheet.pdf", paper="a4r", width=11.6, height=8.2, onefile=TRUE)

### page 1

# grDevices::colors
m <- matrix(1:660, 60, 11)
kol <- colors()[m]
op <- par(mar=c(.1, .1, 2, .1))
image(1:11, 1:60, t(m), col=kol, axes=FALSE, ann=FALSE)
txtcol <- ifelse( apply(col2rgb(kol), 2, mean) < 70, "white", "black")
text( as.numeric(col(m)), as.numeric(row(m)), kol, cex=.8, col=txtcol)
mtext("grDevices::colors", 3, cex=2)
par(op)

### page 2
layout(matrix(1:2, 1, 2))

# RColorBrewer
op <- par(mar=c(.1, 5, 2, .1))
display.brewer.all()
mtext("RColorBrewer", 3, cex=2)
par(op)

# ramps
N <- 100 # ramp length
funnames <- c("grDevices::rainbow", "grDevices::heat.colors",
          "grDevices::terrain.colors", "grDevices::topo.colors",
          "grDevices::cm.colors", "colorRamps::blue2red",
          "colorRamps::blue2green", "colorRamps::green2red",
          "colorRamps::blue2yellow", "colorRamps::cyan2yellow",
          "colorRamps::magenta2green", "colorRamps::matlab.like",
          "colorRamps::matlab.like2", "colorRamps::primary.colors",
          "colorRamps::ygobb")
spl <- strsplit(funnames, "::")
pkgs <- sapply(spl, "[", 1)
funs <- sapply(spl, "[", 2)
kolmat <- sapply(funs, do.call, list(N))
mat <- matrix( seq(1, length(kolmat)), nrow(kolmat), ncol(kolmat))
op <- par(mar=c(.1, .1, 2, .1))
image(seq(1, nrow(mat)), seq(1, ncol(mat)), mat, col=kolmat,
      axes=FALSE, ann=FALSE)
text( nrow(mat)/2, seq(1, ncol(mat)), funnames)
mtext("Color ramps", 3, cex=2)
par(op)


dev.off()
