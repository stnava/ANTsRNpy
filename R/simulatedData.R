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
#' @param classByPosition boolean altering label by the sign of position change
#' @return list containing the single channel image, the multi-channel image,
#' the ground truth image and the ground truth labels and the center of mass
#' stored in a data frame.
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
  positionNoiseLevel = c( 0, 2 ),
  classByPosition = FALSE )
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
  posNoise = rnorm( mydim, positionNoiseLevel[1], positionNoiseLevel[2] )
  pts[1, ] = antsTransformIndexToPhysicalPoint( referenceImage, ctr ) + posNoise
  ptsi = makePointsImage( pts, msk, radius = baserad )
  ptsi = ptsi + makePointsImage( pts, msk, radius = plusrad )
  if ( classByPosition ) {
    templabs = ptsi[ msk == 1 ]
    templabs[ 1:floor(length( templabs )/2) ] =
      templabs[ 1:floor(length( templabs )/2) ] + max( templabs ) + 1
    ptsi[ msk == 1 ] = templabs
  }
  gtSeg = antsImageClone( ptsi )
  labels$labels = ptsi[ msk == 1 ]
  mynoise = rnorm( sum( msk == 1 ), noiseLevel[1], noiseLevel[2] )
  mycom = getCenterOfMass( ptsi )
  ptsi[ msk == 1 ] = ptsi[ msk == 1 ] + mynoise
  ptsm = mergeChannels( lappend( ptsi, spatlist ) )       # multichannel version
  return( list( image = ptsi, mcimage = ptsm, groundTruthImage = gtSeg,
    groundTruth = labels, centerOfMass = mycom ) )
}



#' imageToPatches
#'
#' Convert an input image to a set of patches of given radius.  Return the local
#' patch position for each patch. Optionally return the ground truth data
#' associated with each patch, if available.
#'
#' @param image a reference image defining the image space
#' @param mask mask for the image
#' @param radius a scalar defining the patch size
#' @param npatches number of patches to regular sample from the image
#' @param groundTruth single scalar or vector (size of mask) with groundTruth
#' @param randomize randomize the location of the patch sampling
#' @return list containing the image patches and separately the data frame
#' holding the patch coordinate and the ground truth.
#' @author Avants BB
#' @import ANTsR RcppCNPy
#' @examples
#'
#' rimg = makeImage( c(25,25) )
#' sim = simulateSphereData( rimg, radius = c( 2, 3 )  )
#' ptch = imageToPatches( sim$image, rimg * 0 + 1  )
#'
#' @export imageToPatches
imageToPatches <- function(
  image,
  mask = NA,
  radius = 5,
  npatches = NA,
  groundTruth = 0,
  randomize = FALSE )
{
mydim = image@dimension
mycomp = image@components
gtIsVector = TRUE
if ( length( groundTruth ) == 1 ) gtIsVector = FALSE
if ( is.na( mask ) ) mask = image * 0 + 1
nmat = getNeighborhoodInMask( image, mask, radius=rep( radius, mydim ),
  physical.coordinates=T, spatial.info=T, boundary.condition='image' )
nmat$values[  is.na( nmat$values )  ] = mean( nmat$values , na.rm = TRUE )
# write out each (or a sample of) patch along with its center coordinate and label
patches = list( )
if ( is.na( npatches ) ) npatches = ncol( nmat$values )
voxes = seq( from = 1, to = ncol( nmat$values ), by = floor(ncol( nmat$values )/npatches ) )
if ( randomize ) {
  voxes = sample(  1:ncol( nmat$values ), npatches )
}
if ( length( voxes ) > npatches ) voxes = voxes[ 1:npatches ]
mydf = data.frame(  matrix( nrow = npatches, ncol = mydim + 3 ) )
colnames( mydf ) = c( "index", "value", c("x","y","z","t")[1:mydim], 'groundTruth' )
centervalind = floor( nrow( nmat$values ) / 2 ) + 1
ct = 1
if ( !( gtIsVector ) ) locgt = groundTruth
for ( vox in voxes ) {
  rimg = makeImage( rep( radius*2 + 1, mydim ), nmat$values[,vox] )
  locvalue = nmat$values[ centervalind, vox ]
  position = nmat$indices[vox,]
  if ( gtIsVector ) locgt = groundTruth[ vox ]
  mydf[ ct, ] = c( vox, locvalue, position, locgt )
  patches[[ ct ]] = rimg
  ct = ct + 1
  }
 return( list( patches = patches, patchSummary = mydf ) )
}




#' multiChannelImageToPatches
#'
#' Convert an input image to a set of patches of given radius.  Return the local
#' patch position for each patch. Optionally return the ground truth data
#' associated with each patch, if available.
#'
#' @param image a reference image defining the image space
#' @param mask mask for the image
#' @param radius a scalar defining the patch size
#' @param npatches number of patches to regular sample from the image
#' @param groundTruth single scalar or vector (size of mask) with groundTruth
#' @param randomize randomize the location of the patch sampling
#' @return list containing the image patches and separately the data frame
#' holding the patch coordinate and the ground truth.
#' @author Avants BB
#' @import ANTsR RcppCNPy
#' @examples
#'
#' i1 = makeImage( c(25,25) )
#' i2 = makeImage( c(25,25) )
#' rimg = mergeChannels( list( i1, i2 ) )
#' ptch = multiChannelImageToPatches( rimg, i1 * 0 + 1, npatches = 3  )
#'
#' @export multiChannelImageToPatches
multiChannelImageToPatches <- function(
  image,
  mask = NA,
  radius = 5,
  npatches = NA,
  groundTruth = 0,
  randomize = FALSE )
{
mydim = image@dimension
mycomp = image@components
gtIsVector = TRUE
if ( length( groundTruth ) == 1 ) gtIsVector = FALSE
# get each component
mychan = splitChannels( image )
if ( is.na( mask ) ) mask = mychan[[ 1 ]] * 0 + 1
if ( length( mychan ) != mycomp )
  stop("Number of split channels does not equal number of components.")
mymats = list( )
for ( myk in 1:length( mychan ) ) {
  nmat = getNeighborhoodInMask( mychan[[myk]], mask, radius=rep( radius, mydim ),
    physical.coordinates=T, spatial.info=T, boundary.condition='image' )
  nmat$values[  is.na( nmat$values )  ] = mean( nmat$values , na.rm = TRUE )
  mymats[[ myk ]] = nmat
  }
# write out each (or a sample of) patch along with its center coordinate and label
patches = list( )
if ( is.na( npatches ) ) npatches = ncol( mymats[[1]]$values )
voxes = seq( from = 1, to = ncol( mymats[[1]]$values ), by = floor(ncol( mymats[[1]]$values )/npatches ) )
if ( randomize ) {
  voxes = sample(  1:ncol( mymats[[1]]$values ), npatches )
}
if ( length( voxes ) > npatches ) voxes = voxes[ 1:npatches ]
mydf = data.frame(  matrix( nrow = npatches, ncol = mydim + 3 ) )
colnames( mydf ) = c( "index", "value", c("x","y","z","t")[1:mydim], 'groundTruth' )
centervalind = floor( nrow( mymats[[1]]$values ) / 2 ) + 1
ct = 1
if ( !( gtIsVector ) ) locgt = groundTruth
for ( vox in voxes ) {
  rimgs = list( )
  for ( myk in 1:length( mychan ) ) {
    rimgs[[ myk ]] = makeImage( rep( radius*2 + 1, mydim ), mymats[[myk]]$values[,vox] )
  }
  rimg = mergeChannels( rimgs )
  locvalue = mymats[[1]]$values[ centervalind, vox ]
  position = mymats[[1]]$indices[vox,]
  if ( gtIsVector ) locgt = groundTruth[ vox ]
  mydf[ ct, ] = c( vox, locvalue, position, locgt )
  patches[[ ct ]] = rimg
  ct = ct + 1
  }
 return( list( patches = patches, patchSummary = mydf ) )
}
