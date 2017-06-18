# pcvmodr
Portable version of Oregon TLUMIP commercial vehicle (CV) model

This repository contains the code necessary to build and run the commercial vehicle (CV) module of Oregon's statewide integrated model (SWIM) system. It is an experimental replacement for the commercial transport (CT) module in SWIM version 2.5. The motivation for CV was partly to create a portable version of CT, using Idaho's statewide model as the crash test dummy. The final code will work in both places, eliminating code that makes it specific to a certain location. One big change that will be required is that synthetic firms must be exogenously created, rather than generated on the fly from zonal employment data.

The code is written in R, and configured as a formal R package. It uses the doParallel library to reduce the model run time for selected functions. Running them in parallel is not required, but results in substantial runtime reductions. Our initial tests reveal that the simulation scales well up to about 20 cores, but afterwards runtime starts increasing, due to the additional time required to set up and populate so many cores. Users will need to test different hardware configurations in order to determine optimal number of cores for their hardware.

The CV system is set up as a R package, which can be installed from this repository:
```r
install.packages("devtools")  # If not already installed
devtools::install_github("rickdonnelly/pcvmodr")
```

One can mix and match the included functions in any order to run the model, but the current implementation assumes that inter-regional demands will be synthesized from FHWA [Freight Analysis Framework](http://www.ops.fhwa.dot.gov/freight/freight_analysis/faf/) (FAF) or Global Insight's [Transearch data](https://www.ihs.com/products/transearch-freight-transportation-research.html) first, followed by a microsimulation of local truck trips that borrows model parameters and data from analyses of commercial vehicle and establishment surveys conducted by [COMPASS](http://www.compassidaho.org/) and the FHWA [Quick Response Freight Manual II](https://ops.fhwa.dot.gov/freight/publications/qrfm2/) (QRFM). 

Default model parameters and runtime properties will be bundled into the package, to include the inter-regional flow databases and truckload equivalency factors, generation probabilities, trip length and time-of-day distributions, etc. However, this is a work in progress, and for now these files (included in this distribution in the data-raw folder) should be placed in a local folder when running the model.
