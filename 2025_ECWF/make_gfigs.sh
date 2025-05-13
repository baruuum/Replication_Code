#!/bin/bash

## ---------------------------------------------
## Converting plots to gray scale
## ---------------------------------------------

PLOT_DIR=results/plots

mkdir -p $PLOT_DIR/gray

for f in $PLOT_DIR/*.pdf
do  
    gs \
    -sDEVICE=pdfwrite \
    -dProcessColorModel=/DeviceGray \
    -dColorConversionStrategy=/Gray \
    -dPDFUseOldCMS=false \
    -o $(dirname $f)/gray/$(basename $f) \
    -f ${f}
done

### END OF CODE ###