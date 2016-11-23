#' Trophic level volume to pyramid dimensions
#'
#' @param V vector of volumes (i.e. throughput) from lowest to highest trophic level (TL)
#' @param TEgm geometric mean transfer efficiency (TLs 2-4)
#' @param method Method to use for determining base to height ratio 
#' (i.e. pyramid angle). Choices include "proportional" (default) or 
#' "log". "proportional" is  
#' 
#' @description Used internally by \link[EwEvis]{Vpyramid3d}. The angle (i.e. steepness) 
#' of the pyramid is determined by the mean transfer efficiency (\code{TEgm}). 
#' In the case of method = "proportional", the top angle is proportional
#' to the inverse of the TEgm value. 
#' When method = "log", the ratio between the pyramid base length (a) 
#' and height (h) (i.e. "ah_ratio") is determined as 
#' \code{ah_ratio = -log10(TEgm)}. Supsequently the top angle is calculated.
#'
#' @return A list containing (from lowest to highest TL): base widths (a), heights (h), 
#' cumulative height (cum.h), a:h ratio (ah_ratio), top angle(angle), volumes (V), 
#' geometric mean transfer efficiency (TEgm), cumulative volume (cum.V).
#' 
#' 
#' @export
#'
#' @examples
#' # From Tam et al (2008) - Northern Humboldt Current Ecosystem (1995-1996)
#' TEgm <- 0.1014
#' Ts <- c(27165, 22349, 1658, 266.3, 17.45, 0.607, 0.00515, 6e-06)
#' V2Pdim(V = Ts, TEgm = TEgm)

V2Pdim <- function(V, TEgm, method = "proportional"){
  cum.ro.V = cumsum(rev(V)) # cumulative reverse-order volume
  if(method == "proportional"){
    angle <- (1/TEgm)*6
    ah_ratio <- tan(angle/2*(pi/180))*2 # a/h ratio
  }
  if(method == "log"){
    ah_ratio <- -log10(TEgm) # a/h ratio
    angle <- atan(ah_ratio/2)*(180/pi)*2
  }
  a <- rev((cum.ro.V * 3 * ah_ratio)^(1/3)) # base length
  cum.ro.h <- rev(a / ah_ratio) # cumulative reverse-order heights
  h <- rev(c(cum.ro.h[1], diff(cum.ro.h))) # heights
  r <- a / 2 # half base length
  cum.ro.s <- sqrt(cum.ro.h^2 + rev(r)^2) # cumulative reverse-order slant heights
  s <- rev(c(cum.ro.s[1], diff(cum.ro.s))) # slant heights
  rs_ratio <- (rev(r)/cum.ro.s)[1] # ratio of r to s
  theta <- asin(rs_ratio) # base angle
  cum.h <- cumsum(h) # cumulative height
  return(list(
    a=a, 
    h=h, 
    cum.h=cum.h, 
    ah_ratio=ah_ratio, 
    angle=angle, 
    V=V, 
    TEgm=TEgm, 
    cum.V=cumsum(V)
  ))
}
