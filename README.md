# ANTsRNpy

Write a single or multi-channel antsImage to a numpy vector or matrix.  Read a
numpy vector or matrix back into a single or multi-channel antsImage.

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
