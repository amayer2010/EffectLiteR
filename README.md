effectliter
=========

This R package can be used to estimate average and conditional effects of a treatment variable on an outcome variable, taking into account any number of continuous and categorical covariates. It automatically generates `lavaan` (Rosseel, 2012) syntax for a multi-group structural equation model, runs the model in `lavaan`, and extracts various average and conditional effects of interest.

`effectliter` is a new program based on the ideas (not the code) of EffectLite (Steyer & Partchev, 2008; www.causal-effects.de) and many (former) researchers at the Department of Methodology and Evaluation Research at Friedrich Schiller University Jena.

Dependencies
=========

Running `effectliter` requires `lavaan` (>= 0.5-16), and `methods`. Calling the graphical user interface requires the additional packages `shiny`, `foreign`, and `ggplot2`. Installing the latest version of the package directly from GitHub requires Rtools (http://cran.r-project.org/bin/windows/Rtools/) and the additional package `devtools`.

For the impatient ones among us, start a new R session and copy and paste the following:

```
install.packages(c("lavaan","methods"))
install.packages(c("shiny","foreign","ggplot2"))
install.packages("devtools")
```

Installation
=========

`effectliter` can be installed directly from this GitHub repository (under Windows, please make sure Rtools are installed and no older version of `effectliter` is currently loaded): 

```
library(devtools)
install_github("amayer2010/effectliter")
```

Alternatively, the subfolder `/old` contains `tar.gz` files (for installing from source) and Windows binaries. However, these may be outdated.


Run effectliter
=========

The main function of the package is `effectLite()`. Type `example(effectLite)` for an example. The `shiny` interface can be called by `effectLiteGUI()` after having loaded the package `effectliter`:

```
library(effectliter)
example(effectLite)
effectLiteGUI()
```
