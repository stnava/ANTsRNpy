#' writeANTsImageToNumpy
#'
#' Write antsImage type to numpy array.  This will produce a vector for a single
#' channel image and a matrix for a multi-channel image.  We do not reorder the
#' data to match R and numpy indexing.
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
#' img = ANTsR::makeImage( c( 5, 7 ), rnorm( 35 ) )
#' writeANTsImageToNumpy( img, ofn )
#' # to read in python, do
#' # from PIL import Image
#' # import numpy as np
#' # from scipy.misc import toimage
#' # data = np.load( ofn )
#' # array = np.reshape( data, [ 7,5] )
#' # toimage(array).show()
#' img = ANTsR::makeImage( c( 5, 6, 7 ), rnorm( 5*6*7) )
#' writeANTsImageToNumpy( img, ofn )
#' # to read in python, do
#' # from PIL import Image
#' # import numpy as np
#' # from scipy.misc import toimage
#' # data = np.load( ofn )
#' # array = np.reshape( data, [ 7, 6, 5 ] )
#' # then we have
#' # array[ 3, 1, 4 ] # index in python
#' # as.array( img )[ 5, 2, 4 ] # index in R
#' # so the python indices are in reverse order and minus one compared to R
#' # toimage(array[:,:,2]).show()
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
#' # now a multi-channel example
#' i1 = ANTsR::makeImage( c( 5, 6, 7 ), rnorm( 5*6*7) )
#' i2 = ANTsR::makeImage( c( 5, 6, 7 ), rnorm( 5*6*7) )
#' img = mergeChannels( list( i1, i2 ) )
#' writeANTsImageToNumpy( img, ofn )
#' # now to read this in python, follow the examples above but each channel
#' # corresponds to each row in the numpy matrix
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
#' img = antsImageRead( getANTsRData( "mni" ) )
#' writeANTsImageToNumpy( img, ofn )
#' img3 = readNumpyToANTsImage( ofn, img )
#' # # in python
#' # from PIL import Image
#' # import numpy as np
#' # from scipy.misc import toimage
#' # data = np.load( ofn )
#' # array = np.reshape( data, [ 182, 218, 182 ] )
#' # toimage(array[:,111,:]).show()
#'
#' @export readNumpyToANTsImage
readNumpyToANTsImage <- function( fn, img ) {
  if ( img@components == 1 ) msk = img * 0 + 1
  if ( img@components > 1 ) {
    msk = splitChannels( img )[[ 1 ]]  * 0 + 1
    }
  fmat <- RcppCNPy::npyLoad( fn )
  if ( class( fmat ) == "numeric" ) {
    msk[ msk == 1 ] = fmat
    return( msk )
  }
  if ( class( fmat ) == "matrix" ) {
    nchannel = nrow( fmat )
    ilist = list()
    for ( k in 1:nchannel ) {
      msk[ msk == 1 ] = fmat[k,]
      ilist[[ k ]] = antsImageClone( msk )
    }
    return( mergeChannels( ilist ) )
  } else stop("cannot identify this numpy type")
}
