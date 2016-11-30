## ----setup---------------------------------------------------------------
library(EwEvis)
library(rgl)

## ---- echo=FALSE---------------------------------------------------------
library(knitr)
knit_hooks$set(webgl = hook_webgl)

## ------------------------------------------------------------------------
# Northern Humboldt Current Ecosystem (La Niña, Tam et al. 2008)
TEgm <- 0.1014 # mean transfer efficiency (trophic levels 2-4)
Ts <- c(27165, 22349, 1658, 266.3, 17.45, 0.607, 0.00515, 6e-06) # Throughout
Bs <- c(62.84, 147.1, 70.05, 20.61, 1.658, 0.084, 0.000657, 1e-06, 0) # Biomass

## ----testgl, webgl=TRUE, fig.width = 6, fig.height = 5-------------------
open3d()
# color palatte
pal <- colorRampPalette(c("green", "yellow", "red"), bias=3)
# Biomass
tmp1 <- Vbox3d(V=Bs[1:5], TL.len = 10, col=pal(5), add.scale = FALSE)
# Throughput
tmp2 <- Vbox3d(V=Ts[1:5], TL.len = 10, col=pal(5), add.scale = TRUE,  shift=c(dist(tmp1[[1]]$vb[1,1:2])*20,0,0), scale.len = 1000^(1/3)
)

## ----testgl2, webgl=TRUE, fig.width = 6, fig.height = 5------------------
open3d()
tmp <- Vpyramid3d(V=Ts[2:8], TEgm = 0.07, col=2:7, 
    add.scale = FALSE
)
tmp2 <- Vpyramid3d(V=Ts[2:8], TEgm = 0.1, col=2:7,
   shift=c(dist(tmp[[1]]$vb[1,1:2])*1.1,0,0),
   add.scale = FALSE
)
tmp3 <- Vpyramid3d(V=Ts[2:8], TEgm = 0.15, col=2:7,
  shift=c(dist(tmp[[1]]$vb[1,1:2])*1.1*2,0,0),
  add.scale = TRUE, scale.len = 1000^(1/3)
)

# add labels
pos <- data.frame(rbind(
  rowMeans(tmp[[7]]$vb),
  rowMeans(tmp2[[7]]$vb),
  rowMeans(tmp3[[7]]$vb)
))
names(pos) <- c("x", "y", "z", "w")

text3d(pos$x, y=pos$y, z = pos$z+3,
          text=paste("TE =", c("7%", "10%", "15%")),
       color="black", font=2
) 

