
yspec
=====

About
-----

-   Write data set specification data in `yaml` format
    -   [About the format](./inst/doc/reference.md)
-   The `yspec` package reads the specification and further formats the data
    -   For example, [splitting data columns by other data items](./inst/doc/reference.md#split-data-column)
-   Information can be accessed or utilized by other functions in the ecosystem
-   `yspec` will render a [`Define.pdf`](./inst/doc/define.pdf) document or output in any other format supported by `rmarkdown`

Use
---

``` r
library(yspec)
```

We can load [this yaml file](inst/spec/spec.yml)

``` r
sp <- load_spec("inst/spec/spec.yml")
```

See [here](./inst/doc/reference.md) for `YAML` specification details

A lot like the current spec setup

``` r
names(sp)
```

    .  [1] "C"       "USUBJID" "ID"      "EVID"    "MDV"     "SEQ"     "AMT"    
    .  [8] "II"      "CMT"     "TAFD"    "WT"      "EGFR"    "STUDY"   "SEX"    
    . [15] "BQL"     "DV"      "DV2"     "HAIR"

``` r
sp[WT]$unit
```

    . [1] "kg"

``` r
knitr::kable(as.data.frame(sp))
```

|  col| name    | units          | long                                     |
|----:|:--------|:---------------|:-----------------------------------------|
|    1| C       | .              | commented rows                           |
|    2| USUBJID | .              | universal subject identifier             |
|    3| ID      | .              | NONMEM ID number                         |
|    4| EVID    | .              | event ID indicator                       |
|    5| MDV     | .              | missing DV indicator                     |
|    6| SEQ     | .              | record type indicators                   |
|    7| AMT     | mg             | dose amount                              |
|    8| II      | hours          | interdose interval                       |
|    9| CMT     | .              | compartment number                       |
|   10| TAFD    | hours          | time after first dose                    |
|   11| WT      | kg             | baseline weight                          |
|   12| EGFR    | ml/min/1.73 m2 | estimated glomerular filtration rate     |
|   13| STUDY   | .              | study identifier                         |
|   14| SEX     | .              | .                                        |
|   15| BQL     | .              | DV value is below the quantitation limit |
|   16| DV      | .              | dependent variable                       |
|   17| DV2     | .              | second dependent variable                |
|   18| HAIR    | .              | patient hair color                       |

Render in pdf format

``` r
render_spec(sp, "define", output_dir="inst/doc")
```

with output [here](./inst/doc/define.pdf)

Or render to any format

``` r
render_spec(sp, "define", output_format="md_document", output_dir="inst/doc")
```

Then we get [this output](./inst/doc/define.md)
