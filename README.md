yspec
================

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
head(sp)
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

See [here](./inst/doc/reference.md) for `YAML` specification details

A lot like the current spec setup

``` r
names(sp)
```

    .  [1] "C"       "USUBJID" "ID"      "EVID"    "MDV"     "SEQ"     "AMT"    
    .  [8] "II"      "CMT"     "TAFD"    "WT"      "EGFR"    "STUDY"   "SEX"    
    . [15] "BQL"     "DV"      "DV2"     "HAIR"    "CLCR"

``` r
sp$WT$unit
```

    . [1] "kg"

``` r
writeLines(yspec:::pander_table(sp))
```

    . Loading required namespace: pander

<table style="width:97%;">
<colgroup>
<col width="13%" />
<col width="16%" />
<col width="18%" />
<col width="48%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Column</th>
<th align="left">Type</th>
<th align="left">Field</th>
<th align="left">Value</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">C</td>
<td align="left">character</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">commented row indicator</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">commented rows</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">., C</td>
</tr>
<tr class="odd">
<td align="left">USUBJID</td>
<td align="left">character</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">USUBJID</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">universal subject identifier</td>
</tr>
<tr class="even">
<td align="left">ID</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">ID</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">NONMEM ID number</td>
</tr>
<tr class="odd">
<td align="left">EVID</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">EVID</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">event ID indicator</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">per NONMEM specifications</td>
</tr>
<tr class="odd">
<td align="left">MDV</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">MDV</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">missing DV indicator</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">per NONMEM specifications</td>
</tr>
<tr class="odd">
<td align="left">SEQ</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">SEQ</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">record type indicators</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">1 = concentration, 2 = response, 3 = other</td>
</tr>
<tr class="odd">
<td align="left">AMT</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">mg</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">Amount</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">dose amount</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">0 to 1000</td>
</tr>
<tr class="odd">
<td align="left">II</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">hours</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">II</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">interdose interval</td>
</tr>
<tr class="even">
<td align="left">CMT</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">Compartment</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">compartment number</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">per NONMEM specifications</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">1, 2, 3, 4</td>
</tr>
<tr class="odd">
<td align="left">TAFD</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">hours</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">TAFD</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">time after first dose</td>
</tr>
<tr class="even">
<td align="left">WT</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">kg</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">Weight</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">baseline weight</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">40 to 100</td>
</tr>
<tr class="even">
<td align="left">EGFR</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">ml/min/1.73 m2</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">eGFR</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">estimated glomerular filtration rate</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">10 to 300</td>
</tr>
<tr class="even">
<td align="left">STUDY</td>
<td align="left">character</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">Study</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">study identifier</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">100 = The first phase 1 study, 202 = The first phase 2 study conducted only at sites 1, 2, 3 when the formulation was still oral liquid only, 303 = The renal impairment study, 203 = The second phase 2 study</td>
</tr>
<tr class="even">
<td align="left">SEX</td>
<td align="left">integer</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">SEX</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">0 = male, 1 = female</td>
</tr>
<tr class="odd">
<td align="left">BQL</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">below limit of quantification</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">DV value is below the quantitation limit</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">0 = FALSE, 1 = TRUE</td>
</tr>
<tr class="odd">
<td align="left">DV</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">DV</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">dependent variable</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">see SEQ</td>
</tr>
<tr class="odd">
<td align="left">DV2</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">DV2</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">second dependent variable</td>
</tr>
<tr class="even">
<td align="left">HAIR</td>
<td align="left">character</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">HAIR</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">patient hair color</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">0 = brown, 1 = blonde, 2 = black</td>
</tr>
<tr class="even">
<td align="left">CLCR</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">ml/min</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">creatinine clearance</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">40 to 180</td>
</tr>
</tbody>
</table>

Render in pdf format

``` r
render_spec(sp, stem = "define", output_dir="inst/doc")
```

with output [here](./inst/doc/define.pdf)

Or render to any format

``` r
render_spec(sp, stem = "define_example", 
            envir  = new.env(),
            output_format="md_document",
            output_dir="inst/doc")
```

Then we get [this output](./inst/doc/define_example.md)

Build a `define.pdf` document
-----------------------------

To make a proper `define.pdf` document, create another yaml file that points to data set specific files.

For example [./inst/spec/project.yml](./inst/spec/project.yml)

Then render it:

``` r
render_define(file = "inst/spec/project.yml", 
              stem = "project", envir = new.env(),
              output_format = "pdf_document",
              output_dir = "inst/doc",
              title = "DEM104101 PK/PD Analysis Data Sets")
```

Output [here](./inst/doc/project.pdf)

Build a `define.pdf` document for sending to FDA
------------------------------------------------

``` r
render_fda_define(spec_ex_proj(), 
                  stem = "fda_define",
                  output_dir = "inst/doc")
```

    . 
    . 
    . processing file: fda_define.Rmd

    . output file: fda_define.knit.md

    . 
    . Output created: /Users/kyleb/ghe/software/yspec/inst/doc/fda_define.pdf
