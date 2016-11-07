#' 3d trophic level volume pyramid
#'
#' @param V vector of volumes (i.e. throughput) from lowest to highers trophic level
#' @param TEgm geometric mean transfer efficiency (TLs 2-4) 
#' @param col colors of V (vector)
#' @param alpha alpha of V (single value (0-1))
#' @param add.scale logical add scale
#' @param scale.len length of side of scale (cube) 
#' @param shift shift values (x,y,z)
#'
#' @return
#' rgl output
#' 
#' @export
#'
#' @examples
#' 
#' ### data from Tam et al (2008) - Northern Humboldt Cureent Ecosystem (1995-1996)
#' 
#' # data
#' TEgm <- 0.1014
#' Ts <- c(27165, 22349, 1658, 266.3, 17.45, 0.607, 0.00515, 6e-06)
#' Bs <- c(62.84, 147.1, 70.05, 20.61, 1.658, 0.084, 0.000657, 1e-06, 0)
#' res <- V2Pdim(V = Ts, TEgm=TEgm)
#' pal <- colorRampPalette(c("green", "yellow", "red"), bias=3)
#' 
#' \donttest{
#' # single plot of biomasses by TL
#' rgl::open3d()
#' res <- V.pyramid3d(V=Bs[1:5], TEgm = TEgm, col=pal(5), scale.len = 1)
#' 
#' # single plot of throughput by TL
#' rgl::open3d()
#' res <- V.pyramid3d(V=Ts[2:5], TEgm = TEgm, col=pal(4), alpha=0.2, scale.len = 10)
#' 
#' # how to set-up a comparison plot (throughout example)
#' rgl::open3d()
#' tmp <- V.pyramid3d(V=Ts[2:5], TEgm = TEgm, col=pal(4), add.scale = FALSE)
#' V.pyramid3d(V=Ts[2:5], TEgm = TEgm, col=pal(4), 
#'   shift=c(dist(tmp[[1]]$vb[1,1:2])+5,0,0), scale.len = 10)
#' }
#' 
V.pyramid3d <- function(V, TEgm, col=seq(V), alpha=0.2, add.scale=TRUE, scale.len = 10, shift=c(0,0,0)){
  cub <- rgl::cube3d()
  shape.obj <- vector(mode="list", length(V))
  res <- V2Pdim(V = V, TEgm = TEgm)
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
    rgl::wire3d( shape.obj[[i]] )
    rgl::shade3d( shape.obj[[i]], col=col[i], alpha=alpha)
  }
  
  # add scale
  if(add.scale){
    cub.scale <- cub
    cub.scale$vb[1:3,] <- cub.scale$vb[1:3,] * 0.5 * scale.len
    cub.scale <- rgl::translate3d(
      cub.scale, 
      max(unlist(lapply(shape.obj, function(x){max(x$vb[1,])}))) + scale.len, # x shift
      min(unlist(lapply(shape.obj, function(x){min(x$vb[2,])}))) + scale.len, # y shift
      min(unlist(lapply(shape.obj, function(x){min(x$vb[3,])}))) + scale.len/2 # z shift
    )
    rgl::wire3d( cub.scale )
  }
  return(shape.obj)
}
