
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
    . [15] "BQL"     "DV"      "DV2"     "HAIR"    "CLCR"

``` r
sp[WT]$unit
```

    . [1] "kg"

``` r
yspec:::pander_table(sp)
```

    ## Loading required namespace: pander

<table style="width:90%;">
<colgroup>
<col width="20%" />
<col width="13%" />
<col width="18%" />
<col width="37%" />
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
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">C</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">commented rows</td>
</tr>
<tr class="odd">
<td align="left">USUBJID</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">USUBJID</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">universal subject identifier</td>
</tr>
<tr class="odd">
<td align="left">ID</td>
<td align="left">numeric</td>
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
<td align="left">short-name</td>
<td align="left">EVID</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">event ID indicator</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">per NONMEM specifications</td>
</tr>
<tr class="even">
<td align="left">MDV</td>
<td align="left">numeric</td>
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
<td align="left">short-name</td>
<td align="left">SEQ</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">record type indicators</td>
</tr>
<tr class="odd">
<td align="left">AMT (mg)</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">Amount</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">dose amount</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">(0 - 1000)</td>
</tr>
<tr class="even">
<td align="left">II (hours)</td>
<td align="left">numeric</td>
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
<td align="left">short-name</td>
<td align="left">Compartment</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">compartment number</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">comment</td>
<td align="left">per NONMEM specifications</td>
</tr>
<tr class="odd">
<td align="left">TAFD (hours)</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">TAFD</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">time after first dose</td>
</tr>
<tr class="odd">
<td align="left">WT (kg)</td>
<td align="left">numeric</td>
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
<td align="left">(40 - 100)</td>
</tr>
<tr class="even">
<td align="left">EGFR (ml/min/1.73 m2)</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">eGFR</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">estimated glomerular filtration rate</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">range</td>
<td align="left">(10 - 300)</td>
</tr>
<tr class="odd">
<td align="left">STUDY</td>
<td align="left">numeric</td>
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
<td align="left">SEX</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">SEX</td>
</tr>
<tr class="even">
<td align="left">BQL</td>
<td align="left">numeric</td>
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
<td align="left">DV</td>
<td align="left">numeric</td>
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
<td align="left">short-name</td>
<td align="left">DV2</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left"></td>
<td align="left">long-name</td>
<td align="left">second dependent variable</td>
</tr>
<tr class="odd">
<td align="left">HAIR</td>
<td align="left">numeric</td>
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
<td align="left">CLCR (ml/min)</td>
<td align="left">numeric</td>
<td align="left">short-name</td>
<td align="left">creatinine clearance</td>
</tr>
</tbody>
</table>

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

Build a `define.pdf` document
-----------------------------

To make a proper `define.pdf` document, create another yaml file that points to data set specific files.

For example [./inst/spec/project.yml](./inst/spec/project.yml)

Then render it:

``` r
render_define(file = "./inst/spec/project.yml", 
              output = "project", 
              title = "DEM104101 PK/PD Analysis Data Sets")
```

Output [here](./inst/doc/project.pdf)
