effectliter
=========

This package can be used to estimate average and conditional effects of a treatment variable on an outcome variable, taking into
account any number of continuous and categorical covariates. It automatically generates lavaan (Rosseel, 2012) syntax for a multi-group structural equation model, runs the model in lavaan, and extracts various average and conditional effects of interest.

effectliter is a new program based on the ideas (not the code) of EffectLite (Steyer & Partchev, 2008; www.causal-effects.de) and many (former) researchers at the Department of Methodology and Evaluation Research at Friedrich Schiller University Jena.

Installation
=========

effectliter can be installed from this Github repository. For those not familiar with installing packages from Github, the subfolder /old contains tar.gz files (for installing from source) and windows binaries. Please make sure all dependencies are installed (lavaan (>= 0.5-16), methods, plyr), and for the shiny interface, the additional packages shiny, Hmisc, and ggplot2 are required.


Run effectliter
=========

The main function of the package is effectLite(). Type example(effectLite) for an example. The shiny interface can be called by effectLiteGUI() after having loaded the package effectliter.