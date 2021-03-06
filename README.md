# ANTsRNpy

Write a single or multi-channel antsImage to a numpy vector or matrix.  Read a
numpy vector or matrix back into a single or multi-channel antsImage.

Package dependencies: `ANTSR, RcppCNPy`

Installation:

```
devtools::install_github("stnava/ANTsRNpy")
```

See the help for the two functions:  

```
readNumpyToANTsImage
writeANTsImageToNumpy
```

The help has examples for both python and R.  Here are brief examples:


First, `R` (single-channel example):
```
library( ANTsRNpy )
ofn = "/tmp/temp.npy"
img = ANTsR::makeImage( c( 5, 6, 7 ), rnorm( 5*6*7) )
writeANTsImageToNumpy( img, ofn )
img3 = readNumpyToANTsImage( ofn, img )
as.array( img )[ 5, 2, 4 ]
```

Now `python` (corresponding to single-channel example from above):

```
# to read in python, do
from PIL import Image
import numpy as np
from scipy.misc import toimage
ofn = "/tmp/temp.npy"
data = np.load( ofn )
array = np.reshape( data, [ 7, 6, 5 ] )
# then we have
array[ 3, 1, 4 ] # index in python
# as.array( img )[ 5, 2, 4 ] # index in R
# so the python indices are in reverse order and minus one compared to R
toimage(array[:,:,2]).show()
```

So, from above, we can see that the necessary index reordering for 2D and 3D is:

  * 2D: if `R` is  `m * n`  then the `numpy` data in `python` is  `n * m`

  * 3D: if `R` is  `m * n * o`  then the `numpy` data in `python` is `o * n * m`

The examples illustrate this and the differences induced by the 1-based indexing
in `R` vs. 0-based indexing in `python`.

**why do we need this tool?** we want to create multichannel image ground truth
data in `R` and read into `python`.  however, `R` currently lacks the ability to
write `numpy` data with dimensionality greater than 2 (i.e. matrices are ok but
multi-dimensional arrays cannot be written).  this is due to limits in the
current implementation of `RcppCNPy` which may, in the future, be overcome.
In the meantime, the current approach allows us to write either single or multi
channel images into numpy vectors and matrices.  we then aggregate and pickle
the numpy data in python to create sharable training and testing datasets.
