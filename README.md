EffectLiteR
=========

This R package can be used to estimate average and conditional effects of a treatment variable on an outcome variable, taking into account any number of continuous and categorical covariates. It automatically generates `lavaan` (Rosseel, 2012) syntax for a multi-group structural equation model, runs the model in `lavaan`, and extracts various average and conditional effects of interest.

`EffectLiteR` is a new program based on the ideas (not the code) of EffectLite (Steyer & Partchev, 2008; www.causal-effects.de) and many (former) researchers at the Department of Methodology and Evaluation Research at Friedrich Schiller University Jena.


Installation
=========

`EffectLiteR` can be installed directly from this GitHub repository using the additional package `devtools`. Under Windows, please make sure Rtools (http://cran.r-project.org/bin/windows/Rtools/) are installed and no older version of `EffectLiteR` is currently loaded): 

```
install.packages("devtools")
library(devtools)

install_github("amayer2010/EffectLiteR")
```

Alternatively, the subfolder `/old` contains `tar.gz` files (for installing from source) and Windows binaries. However, these may be outdated. When `devtools` is used, dependencies are installed automatically. Otherwise, you need to install the following packages yourself:

```
install.packages(c("lavaan","methods"))
install.packages(c("shiny","foreign","ggplot2"))

```


Run EffectLiteR
=========

The main function of the package is `effectLite()`. Type `example(effectLite)` for an example. The `shiny` interface can be called by `effectLiteGUI()` after having loaded the package `EffectLiteR`:

```
library(EffectLiteR)

example(effectLite)
effectLiteGUI()
```
