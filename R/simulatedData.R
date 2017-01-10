#' simulateSphereData
#'
#' simulate 2D or 3D image geometry and segmentation data. Return the single
#' channel image, the multi-channel image, center of mass and ground truth labels.
#'
#' @param referenceImage a reference image defining the image space
#' @param radius a two vector defining the radii of the inner and outer shells
#' @param noiseLevel a two vector defining the noise level
#' @param positionNoiseLevel a two vector defining the noise level controlling
#' how much the image moves randomly away from the center.
#' @return list containing the single channel image, the multi-channel image,
#' the ground truth labels and the center of mass stored in a data frame.
#' @author Avants BB
#' @import ANTsR RcppCNPy
#' @examples
#'
#' rimg = makeImage( c(25,25) )
#' sim = simulateSphereData( rimg, radius = c( 2, 3 )  )
#'
#' @export simulateSphereData
simulateSphereData <- function( referenceImage,
  radius = c( 5, 6 ),
  noiseLevel = c( 0, 0.1 ),
  positionNoiseLevel = c( 0, 2 ) )
{
  mydim = referenceImage@dimension
  idim = dim( referenceImage )
  msk = referenceImage * 0.0 + 1
  spat = imageDomainToSpatialMatrix( msk, msk )
  spatx = makeImage( dim(msk), spat[,1] )
  spaty = makeImage( dim(msk), spat[,2] )
  if ( mydim == 3 ) {
    spatz = makeImage( dim(msk), spat[,3] )
    spatlist = list( spatx, spaty, spatz )
  } else spatlist = list( spatx,spaty )
  baserad = radius[1]
  plusrad = radius[2]
  labels = data.frame( labels = rep( 0, sum( msk ) ) )
  pts = matrix( nrow = 1, ncol = mydim )
  ctr = idim / 2
  pts[1, ] = antsTransformIndexToPhysicalPoint( referenceImage, ctr ) +
        rnorm( mydim, positionNoiseLevel[1], positionNoiseLevel[2] )
  ptsi = makePointsImage( pts, msk, radius = baserad )
  ptsi = ptsi + makePointsImage( pts, msk, radius = baserad + plusrad )
  labels$labels = ptsi[ msk == 1 ]
  mynoise = rnorm( sum( msk == 1 ), noiseLevel[1], noiseLevel[2] )
  mycom = getCenterOfMass( ptsi )
  ptsi[ msk == 1 ] = ptsi[ msk == 1 ] + mynoise
  ptsm = mergeChannels( lappend( ptsi, spatlist ) )       # multichannel version
  return( list( image = ptsi, mcimage = ptsm,
    groundTruth = labels, centerOfMass = mycom ) )
}
