#' Plot clusters
#'
#' Find clusters of snares based on connectivity (dilation; Steiner set) and the minimum number of snares that you want to be in that hotspot.
#' The default dilation distance is based on the 90% quantile of the nearest distance of snares for k=2 (the second nearest snare).
#'
#' This method was suggested by one of the authors of the spatstat package, Ege Rubak on my stackoverflow question:
#' https://stackoverflow.com/questions/55656101/isolate-singletons-in-groups-using-stienen-sets-and-dilation/55680507#55680507
#'
#' @param p input spatstat ppp object
#' @param distance the distance that you want to apply to the dilation (Steiner set), see ?connected.ppp
#' @param clusterpoints the number of objects (e.g. snares) that you want to have in one cluster. Default is 5.
#' @param add Whether you want to add the pattern to an existing map or not. Default is FALSE.
#' @return  A list of hotspot characteristics and a hotspot plot. hotspots A list of individual hotspots within the overall window. hotspot_centroids The centroids of the hotspots that you found as SpatialPoints class object. raster_hotspots The hotspots as a raster object
#' @examples
#' plotstuff(add=F, legend=F, cex=0.8, lwd=2)
#' plotclusters(snares, clusterpoints=5)
#' @export

plotclusters=function(p, distance, add=F, clusterpoints=5, main=NULL,plot=T) {

  require(spatstat)
  require(raster)
  require(rgeos)

  p=unmark(p)

  # identify hotspots ----------------------------------------------------

  cc=spatstat::connected(p, R=distance*2)                       # connection occurs when two discs of radius r touch each other
  s=split(cc)
  np=sapply(s,npoints)

  X=s[np < clusterpoints]                                     # rejects
  X=spatstat::unmark(spatstat::superimpose(X))                 # trick to glue the split object s together again
  Y=s[np >= clusterpoints]                                      # choose number of snares per hotspot
  Y=spatstat::unmark(spatstat::superimpose(Y))

  # split into separate hotspots
  sh=spatstat::connected(Y, R=distance)
  hotspots=split(sh)

  # plot -----------------------------------------------------------------

  if(plot){
    a=plot(spatstat::dilation(Y, distance), add=add, col="orange", main=main)
    plot(X, main="", pch=3, cex=0.5, add=T)                     # these are the points that were thrown away
    # spatstat::stienen(Y, add=T, bg="orange")
  }


  # identify hotspot centroids -------------------------------------------
  hs=spatstat::dilation(Y,distance)                             # see diameter above
  im=spatstat::as.im(hs)
  spatstat::Window(im)=spatstat::Window(snares)
  r=raster::raster(im, crs="+proj=utm +zone=36 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  clumps=raster::clump(r)
  pol = raster::rasterToPolygons(clumps, dissolve=T)
  hotspot_centroids=rgeos::gCentroid(pol, byid=T)

  # get area of hotspots ------------------------------------------------

  polsf=sf::st_as_sf(pol)
  area.m2=st_area(polsf)
  area.km2=area.m2/1e6

  # as percentage of Soysambu?
  soysambu.km2=spatstat::area(Window(p))/1e6
  perc = round(sum(area.km2/soysambu.km2)*100, 1) %>% as.integer

  message("hotspots are ",perc,"% of Soysambu.\n")

  # close out -----------------------------------------------------------
  return( list(
    hotspots=hotspots,
    hotspot_centroids=hotspot_centroids,
    raster_hotspots=r,
    X=X,
    Y=Y
  ))
}
