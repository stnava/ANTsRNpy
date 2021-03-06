% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translationCode.R
\name{readNumpyToANTsImage}
\alias{readNumpyToANTsImage}
\title{readNumpyToANTsImage}
\usage{
readNumpyToANTsImage(fn, img)
}
\arguments{
\item{fn}{input filename (probably a .npy extension)}

\item{img}{reference antsImage in which to put numpy data}
}
\value{
antsImage is output
}
\description{
Read numpy array to antsImage.  This inverts writeANTsImageToNumpy.
}
\examples{

img = ANTsR::antsImageRead( ANTsR::getANTsRData( "r16" ) )
ofn = tempfile( fileext=".npy" )
writeANTsImageToNumpy( img, ofn )
img2 = readNumpyToANTsImage( ofn, img )
img = ANTsR::makeImage( c( 5, 6, 7 ) )
writeANTsImageToNumpy( img, ofn )
img3 = readNumpyToANTsImage( ofn, img )

img = antsImageRead( getANTsRData( "mni" ) )
writeANTsImageToNumpy( img, ofn )
img3 = readNumpyToANTsImage( ofn, img )
# # in python
# from PIL import Image
# import numpy as np
# from scipy.misc import toimage
# data = np.load( ofn )
# array = np.reshape( data, [ 182, 218, 182 ] )
# toimage(array[:,111,:]).show()

}
\author{
Avants BB
}

