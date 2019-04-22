This repository contains a commercial vehicle forecasting model for use in [statewide travel modeling](http://tfresource.org/Category:Statewide_models). It represents a slow evolution of the Oregon commercial travel (CT) model, part of their [Statewide Integrated Model](https://github.com/tlumip/tlumip) (SWIM). It is dubbed the "<u>p</u>ortable <u>c</u>ommercial <u>v</u>ehicle <u>mod</u>el in <u>R</u>" (`pcvmodr`). As its name implies it is implemented as a package in the [R software environment](https://www.r-project.org), making extensive use of the [tidyverse](https://www.tidyverse.org) family of packages for data science. It is portable in the sense that the model can be implemented easily elsewhere with minimal effort. In fact, the genesis of `pcvmodr` was the adaptation of the CT model to Idaho. It was thought that Idaho was similar enough to Oregon with respect to transportation choices and patterns that porting the model would be simple. That was wishful thinking, of course, for interfaces and features unique to the SWIM system required rewriting. And once down the path of refactoring one course was inevitably drawn to the newer `tidyverse` capabilities in R that did not exist when CT was first written, further drawing out the process. The current version is closer to truly portable than its predecessor, but much can still be done on the user experience side. 

The `pcvmodr` system actually encompasses three separate but complementary models:

+ A firm synthesizer within the internal model area (i.e., generation of synthetic firms whose key characteristics in aggregate match total employment and number of firms by size within each economic sector)
+ A long-distance truck model that connects the internal model area with markets in the rest of the continent
+ An urban truck tour model that handles "the last mile" of long-distance flows, typically through nearby distribution centers, as well as local truck travel within the internal model area

The technical details of each of these models need not concern us at this point. What is of note is that this functionality should probably be broken out into three different packages at some point. For the time being they are bundled in this package, which can be installed easily. We make good use of Greg Macfarlane's excellent [omxr package](https://github.com/gregmacfarlane/omxr) to read OMX files and convert them into tibbles. This, in turn, requires installation of the `rhdf5` package, which Greg explains the process for in his [README file](https://github.com/gregmacfarlane/omxr/blob/master/README.md).

```r
install.packages("devtools")   # If not already installed
# Install rhdf5 if not already installed, then run next two commands:
devtools::install_github("gregmacfarlane/omxr")
devtools::install_github("rickdonnelly/pcvmodr")
```

The current build of `pcvmodr` requires R version 3.5 or later, as well as the tidyverse and [doParallel](https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf) packages. Installing the `pcvmodr` package and its dependencies provides the parts necessary to build a fully-functional CV model, but doesn't stitch it all together for you. That is, there is no default model or setup. You can implement one, two, or all three of the major components described above. You might already have synthetic firms built using a different process, or wish to use only the long-distance truck model in conjunction with an urban or statewide model that already includes local truck trips. In general, however, one would run all three, and scripts that illustrate how such models are run in Oregon and Idaho will be included to demonstrate how to do so.