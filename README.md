yspec
================

About
-----

-   Write data set specification data in `yaml` format
    -   [About the format](./inst/doc/reference.md)
-   The `yspec` package reads the specification and further formats the data
    -   For example, [splitting data columns by other data items](./inst/doc/reference.md#split-data-column)
-   Information can be accessed or utilized by other functions in the ecosystem
-   `yspec` will render a [`Define.pdf`](./inst/readme_docs/fda_define.pdf) document or output in any other format supported by `rmarkdown`

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

We can write output in different formats

``` r
sp2 <- sp[c("WT", "SEX", "STUDY")]
writeLines(yspec:::pander_table(sp2))
```

<table style="width:96%;">
<colgroup>
<col width="12%" />
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
<td align="left">WT</td>
<td align="left">numeric</td>
<td align="left">unit</td>
<td align="left">kg</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">Weight</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">baseline weight</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">40 to 100</td>
</tr>
<tr class="odd">
<td align="left">SEX</td>
<td align="left">integer</td>
<td align="left">unit</td>
<td align="left"></td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">short-name</td>
<td align="left">SEX</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">values</td>
<td align="left">0 = male, 1 = female</td>
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
</tbody>
</table>

Render in pdf format

``` r
render_spec(sp, 
            stem = "define", 
            output_format = "pdf_document",
            output_dir = "inst/readme_docs")
```

with output [here](./inst/readme_docs/define.pdf)

Or render to any format

``` r
render_spec(sp, stem = "define_example", 
            output_format = "md_document",
            output_dir = "inst/readme_docs")
```

Then we get [this output](./inst/readme_docs/define_example.md)

Build a `define.pdf` document
-----------------------------

To make a proper `define.pdf` document, create another yaml file that points to data set specific files.

For example [./inst/spec/project.yml](./inst/spec/project.yml)

Then render it:

``` r
render_define(file = "inst/spec/project.yml", 
              stem = "project", 
              output_format = "pdf_document",
              output_dir = "inst/readme_docs",
              title = "DEM104101 PK/PD Analysis Data Sets")
```

Output [here](./inst/readme_docs/project.pdf)

Build a `define.pdf` document for sending to FDA
------------------------------------------------

``` r
render_fda_define(spec_ex_proj(), 
                  stem = "fda_define",
                  build_dir = yspec:::mrgtemplate(),
                  output_dir = "inst/readme_docs")
```

Output [here](./inst/readme_docs/fda_define.pdf)
