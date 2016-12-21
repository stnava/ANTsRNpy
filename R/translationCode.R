#' writeANTsImageToNumpy
#'
#' Write antsImage type to numpy array.  This will produce a vector for a single
#' channel image and a matrix for a multi-channel image.  We reorder the data
#' such that the ordering of indices matches across the two systems.
#'
#' @param img input antsImage
#' @param fn output filename (probably a .npy extension)
#' @return NULL
#' @author Avants BB
#' @import ANTsR RcppCNPy
#' @examples
#'
#' img = ANTsR::antsImageRead( ANTsR::getANTsRData( "r16" ) )
#' ofn = tempfile( fileext=".npy" )
#' writeANTsImageToNumpy( img, ofn )
#' img = ANTsR::makeImage( c( 5, 6, 7 ) )
#' writeANTsImageToNumpy( img, ofn )
#'
#' # to read in python, do
#' # from PIL import Image
#' # import numpy as np
#' # n = 256
#' # data = np.load( ofn )
#' # array = np.reshape( data, [n, n] )
#' # from scipy.misc import toimage
#' # toimage(data).show()
#'
#' @export writeANTsImageToNumpy
writeANTsImageToNumpy <- function( img, fn  ) {
  idim = img@dimension
  if ( idim > 3 ) {
    stop( paste( "image dimension", idim, "not implemented." ) )
  }
  if ( img@components == 1 ) {
    if ( idim == 2 ) {
      msk = img * 0 + 1
      vec = img[ msk == 1 ]
      RcppCNPy::npySave( fn, vec )
    }
    if ( idim == 3 ) {
      msk = img * 0 + 1
      vec = img[ msk == 1 ]
      RcppCNPy::npySave( fn, vec )
    }
  }
  if ( img@components > 1 ) {
    ilist = ANTsR::splitChannels( img )
    msk = ilist[[ 1 ]] * 0 + 1
    matout = matrix( nrow = length( ilist ), ncol = sum( msk ) )
    ct = 1
    for ( x in ilist ) {
      if ( idim == 2 ) {
        vec = x[ msk == 1 ]
        matout[ ct, ] = vec
      }
      if ( idim == 3 ) {
        vec = x[ msk == 1 ]
        matout[ ct, ] = vec
      }
      ct = ct + 1
    }
    RcppCNPy::npySave( fn, matout )
  }
  return( NULL )
}


#' readNumpyToANTsImage
#'
#' Read numpy array to antsImage.  This inverts writeANTsImageToNumpy.
#'
#' @param fn input filename (probably a .npy extension)
#' @param img reference antsImage in which to put numpy data
#' @return antsImage is output
#' @author Avants BB
#' @import ANTsR RcppCNPy
#' @examples
#'
#' img = ANTsR::antsImageRead( ANTsR::getANTsRData( "r16" ) )
#' ofn = tempfile( fileext=".npy" )
#' writeANTsImageToNumpy( img, ofn )
#' img2 = readNumpyToANTsImage( ofn, img )
#' img = ANTsR::makeImage( c( 5, 6, 7 ) )
#' writeANTsImageToNumpy( img, ofn )
#' img3 = readNumpyToANTsImage( ofn, img )
#'
#' @export readNumpyToANTsImage
readNumpyToANTsImage <- function( fn, img ) {
  fmat <- RcppCNPy::npyLoad( fn )
  print( dim( fmat ) )
  return( img )
}
