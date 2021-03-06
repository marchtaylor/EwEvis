#' 3d trophic level volume pyramid
#'
#' @param V vector of volumes (i.e. biomass, throughput) from lowest to 
#' highest trophic level (TL). 
#' Pyramid plots usually extend to highest TL (e.g. TL 8 in EwE output). 
#' Passed to \link[EwEvis]{V2Pdim}. 
#' @param TEgm geometric mean transfer efficiency (TLs 2-4). 
#' Passed to \link[EwEvis]{V2Pdim}.
#' @param col vector of colors for each volume (V). If shorter than V, colors 
#' will be cycled.
#' @param col.bases.only Logical. Color bases of trophic levels only?
#' @param alpha alpha of V (single value (0-1))
#' @param add.scale logical (TRUE / FALSE) adds scale (cube) 
#' to right of box plot.
#' @param scale.len length of side of scale cube (units of V). 
#' @param shift shift location of plot (x,y,z). Useful in comparitive plots.
#' @param draw Logical. Produce plot (Default: draw = TRUE)
#' 
#' @description Produces a trophic pyramid where the vertical seperations
#' are trophic levels, with volume being biomass or (more typically) 
#' throuput. The pyramid angle is inversely proportional to the mean 
#' transfer efficiency (TEgm). V and TEgm arguments are passed to 
#' the \link[EwEvis]{V2Pdim} function, which calculates the 
#' pyramid geometry. Output is a 3d model plotted with the rgl package.
#'
#' @return
#' rgl output
#' 
#' @export
#'
#' @examples
#' 
#' ### data from Tam et al (2008) - Northern Humboldt Current Ecosystem (1995-1996)
#' 
#' # data
#' TEgm <- 0.1014
#' Ts <- c(27165, 22349, 1658, 266.3, 17.45, 0.607, 0.00515, 6e-06)
#' Bs <- c(62.84, 147.1, 70.05, 20.61, 1.658, 0.084, 0.000657, 1e-06, 0)
#' res <- V2Pdim(V = Ts, TEgm=TEgm)
#' pal <- colorRampPalette(c("green", "yellow", "red"), bias=3)
#' 
#' 
#' \donttest{
#' # single plot of biomasses by TL
#' rgl::open3d()
#' res <- Vpyramid3d(V=Bs, TEgm = TEgm, col=pal(9), scale.len = 1)
#' 
#' # single plot of throughput by TL
#' rgl::open3d()
#' res <- Vpyramid3d(V=Ts[2:8], TEgm = TEgm, col=2:7, scale.len = 10)
#' 
#' # color bases only
#' rgl::open3d()
#' res <- Vpyramid3d(V=Ts[2:8], TEgm = TEgm, col=8, alpha=0.5, col.bases.only = TRUE, scale.len = 10)
#' 
#' 
#' # how to set-up a comparison plot (throughout example)
#' rgl::open3d()
#' tmp <- Vpyramid3d(V=Ts[2:8], TEgm = TEgm, alpha=0, add.scale = FALSE)
#' Vpyramid3d(V=Ts[2:8], TEgm = TEgm, col=2:7, 
#'   shift=c(dist(tmp[[1]]$vb[1,1:2])+5,0,0), scale.len = 10
#' )
#' 
#' # illustration of the effect of TEgm on top angle
#' rgl::open3d()
#' tmp <- Vpyramid3d(V=Ts[2:8], TEgm = 0.07, col=2:7, 
#'     add.scale = FALSE
#' )
#' tmp2 <- Vpyramid3d(V=Ts[2:8], TEgm = 0.1, col=2:7,
#'    shift=c(dist(tmp[[1]]$vb[1,1:2])*1.1,0,0),
#'    add.scale = FALSE
#' )
#' tmp3 <- Vpyramid3d(V=Ts[2:8], TEgm = 0.15, col=2:7,
#'   shift=c(dist(tmp[[1]]$vb[1,1:2])*1.1*2,0,0),
#'   add.scale = TRUE, scale.len = 1000^(1/3)
#' )
#' 
#' # add labels
#' pos <- data.frame(rbind(
#'   rowMeans(tmp[[7]]$vb),
#'   rowMeans(tmp2[[7]]$vb),
#'   rowMeans(tmp3[[7]]$vb)
#' ))
#' names(pos) <- c("x", "y", "z", "w")
#' 
#' rgl::text3d(pos$x, y=pos$y, z = pos$z+3,
#'           text=paste("TE =", c("7%", "10%", "15%")),
#'        color="black", font=2
#' ) 
#' 
#' } 
#' 
#' 
Vpyramid3d <- function(
  V, 
  TEgm,
  col = seq(V), 
  alpha = 0.2,
  col.bases.only = FALSE,
  add.scale = TRUE, 
  scale.len = 10, 
  shift = c(0,0,0),
  draw = TRUE
){
  # Make TL shapes
  cub <- rgl::cube3d()
  shape.obj <- vector(mode="list", length(V))
  res <- EwEvis::V2Pdim(V = V, TEgm = TEgm)
  for(i in seq(shape.obj)){
    cubi <- cub
    cubi$vb[1:2,c(1:4)] <- cubi$vb[1:2,c(1:4)]*(res$a[i]/2) # bottom
    if(i != length(shape.obj)){
      cubi$vb[1:2,c(5:8)] <- cubi$vb[1:2,c(5:8)]*(res$a[i+1]/2) # top
    } else {
      cubi$vb[1:2,c(5:8)] <- cubi$vb[1:2,c(5:8)]*0 # top
    }
    cubi$vb[3,] <- cubi$vb[3,]*c(rep(0,4),rep(res$h[i],4))
    shape.obj[[i]] <- rgl::translate3d( cubi, shift[1], shift[2], sum(res$h[seq(from=0,to=i-1)]) + shift[3])
  }
  
  # draw TL shapes
  if(draw){
    col <- rep_len(col, length.out = length(shape.obj))
    for(i in seq(shape.obj)){
      rgl::wire3d( shape.obj[[i]])
      if(col.bases.only){
        rgl::shade3d( rgl::qmesh3d(vertices=shape.obj[[i]]$vb, indices=shape.obj[[i]]$ib[,1]), col=col[i], alpha=alpha )
      } else {
        rgl::shade3d( shape.obj[[i]], col=col[i], alpha=alpha)
      }
    }
    
    # add scale
    if(add.scale){
      cub.scale <- cub
      cub.scale$vb[1:3,] <- cub.scale$vb[1:3,] * 0.5 * scale.len
      cub.scale <- rgl::translate3d(
        cub.scale, 
        max(unlist(lapply(shape.obj, function(x){max(x$vb[1,])}))) + scale.len*1.5, # x shift
        min(unlist(lapply(shape.obj, function(x){min(x$vb[2,])}))) + scale.len*0.5, # y shift
        min(unlist(lapply(shape.obj, function(x){min(x$vb[3,])}))) + scale.len*0.5 # z shift
      )
      rgl::wire3d( cub.scale )
    }
  }

  return(shape.obj)
}
