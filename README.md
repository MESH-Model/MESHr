# MESHr
This package contains functions for pre- and post- processing MESH models.
MESH is a Free Open Source (FOSS) community hydrological model. MESH is described at
http://www.usask.ca/hydrology/MESH.php.

This package is undergoing rapid development, so it is a good idea to watch this
repository to find out about each new version.


## Installation instructions

### Dependencies
MESHr depends on several other packages, which you need to install first, from CRAN, before installing MESHr.
These packages are:
- grid
- ggplot2
- stringr
- reshape2
- knitr
- hydroGOF
- raster
- rts
- readr
- sp
- hydroTSM
- plyr



To install the dependencies, you can use the menu command **Packages | Install** 
in Rstudio, or the command install.packages as in

	install.packages("ggplot2")

### Installing MESHr
You will be able to  download the complete package, as well as the manual .pdf by 
clicking on **releases**, when version 1.0 is released. However, you can 
download and install the most up-to-date version directly from this repository. 
The procedure is:
1. Install the package "devtools" - you only have to do this once. If you are using Windows, you must install the program Rtools, before installing the devtools package. You can install Rtools from https://cran.r-project.org/bin/windows/Rtools/
2. Load the devtools library
3. Install the package.

The commands are: 

	install.packages("devtools")
	library(devtools)
	install_github("CentreForHydrology/MESHr")



