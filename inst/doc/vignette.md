yspec vignette
================

-   [Setup](#setup)
-   [Load some examples](#load-some-examples)
    -   [An example data specification](#an-example-data-specification)
    -   [An example project file](#an-example-project-file)
    -   [An example lookup file](#an-example-lookup-file)
-   [Work with a `yspec` object](#work-with-a-yspec-object)

Setup
=====

``` r
library(yspec)
```

Load some examples
==================

An example data specification
-----------------------------

``` r
spec <- load_spec_ex()
head(spec)
```

    .    col    name      type  unit                   short
    . 1    1       C character     . commented row indicator
    . 2    2 USUBJID character     .                 USUBJID
    . 3    3      ID   numeric     .                      ID
    . 4    4    EVID   numeric     .                    EVID
    . 5    5     MDV   numeric     .                     MDV
    . 6    6     SEQ   numeric     .                     SEQ
    . 7    7     AMT   numeric    mg                  Amount
    . 8    8      II   numeric hours                      II
    . 9    9     CMT   numeric     .             Compartment
    . 10  10    TAFD   numeric hours                    TAFD

An example project file
-----------------------

``` r
spec <- load_proj_ex()
spec
```

    .  name            description                       
    .  DEM104101F_PK   Population PK analysis data set   
    .  DEM104101F_PKPD Population PK/PD analysis data set
    .  DEM104101F_AE   AE analysis data set

An example lookup file
----------------------

``` r
spec <- load_spec_ex("lookup.yml")
head(spec)
```

    .   col  name    type          unit                   short
    . 1   1    WT numeric            kg                  Weight
    . 2   2  EGFR numeric ml/min/1.73m2                    eGFR
    . 3   3 EGFR2 numeric             .                   EGFR2
    . 4   4   SEX integer             .                     SEX
    . 5   5   CMT numeric             .             compartment
    . 6   6     C numeric             . commented row indicator

Work with a `yspec` object
==========================

``` r
spec <- load_spec_ex()

names(spec)
```

    .  [1] "C"       "USUBJID" "ID"      "EVID"    "MDV"     "SEQ"     "AMT"    
    .  [8] "II"      "CMT"     "TAFD"    "WT"      "EGFR"    "STUDY"   "SEX"    
    . [15] "BQL"     "DV"      "DV2"     "HAIR"    "CLCR"

``` r
spec$WT
```

    .  name  value    
    .  col   WT       
    .  type  numeric  
    .  short Weight   
    .  unit  kg       
    .  range 40 to 100
