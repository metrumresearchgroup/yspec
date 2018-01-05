
Introduction
============

Requirements
============

<table style="width:99%;">
<colgroup>
<col width="31%" />
<col width="8%" />
<col width="58%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Section</th>
<th align="right">RID</th>
<th align="left">Requirement</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><strong>Unit tests</strong></td>
<td align="right">1</td>
<td align="left">All unit tests pass</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">2</td>
<td align="left">Unit tests implemented in <code>testthat</code> framework</td>
</tr>
<tr class="odd">
<td align="left"><strong>Loading a specification object</strong></td>
<td align="right">3</td>
<td align="left">Elements of the data specification object are coded in yaml format in a specification file</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">4</td>
<td align="left">The specification file is parsed with <code>yaml::yaml.load_file</code></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">5</td>
<td align="left">On loading, an error is generted if foreign fields are found for a column or if no data is found for a column</td>
</tr>
<tr class="even">
<td align="left"><strong>Valid fields in the specification file</strong></td>
<td align="right">6</td>
<td align="left">Field <code>type</code> - character or numeric</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">7</td>
<td align="left">Field <code>short</code> - a short name for the column</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">8</td>
<td align="left">Field <code>long</code> - a (possibly) longer name for the column</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">9</td>
<td align="left">Field <code>unit</code> - the unit for the data item</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">10</td>
<td align="left">Field <code>range</code> - potential minimum and maximum value if continuous data</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">11</td>
<td align="left">Field <code>values</code> - unique values that the column could take if discrete data</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">12</td>
<td align="left">Field <code>decodes</code> - meaningful names when <code>values</code> is specified</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">13</td>
<td align="left">Field <code>longvalues</code> - if <code>true</code>, values are displayed in long format</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">14</td>
<td align="left">Field <code>source</code> - originating data domain or method for deriving the value in the column</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">15</td>
<td align="left">Field <code>comment</code> - other information relevant to the data column</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">16</td>
<td align="left">Field <code>lookup</code> - if <code>false</code>, the column will not be merged with the lookup table; if <code>true</code>, the column will be merged by the column name; if <code>character</code> name, the column will be merged by the name specified in this field</td>
</tr>
<tr class="odd">
<td align="left"><strong>Import specification details from a lookup document</strong></td>
<td align="right">17</td>
<td align="left">For each specification file, external specification files can be referenced and used to import column specification field data into the current specification object</td>
</tr>
<tr class="even">
<td align="left"><strong>Render single specification documents</strong></td>
<td align="right">18</td>
<td align="left">A data specification object can be rendered in a variety of formats using <code>rmarkdown::render</code></td>
</tr>
<tr class="odd">
<td align="left"><strong>Project-level listing document</strong></td>
<td align="right">19</td>
<td align="left">A separate yaml file can be created that lists all of the data specification documents across a single project</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">20</td>
<td align="left">The project-level specification listing can be loaded and used to direct content in a comprehensive definitions document for all data sets in a project</td>
</tr>
<tr class="odd">
<td align="left"><strong>Specification documents for FDA submission</strong></td>
<td align="right">21</td>
<td align="left">A data definition document meeting FDA requirements can be generated from a project-level listing file</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">22</td>
<td align="left">The columns for each data set are <code>VARIABLE</code>, <code>LABEL</code>, <code>TYPE</code>, and <code>CODES</code></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">23</td>
<td align="left">In addition to FDA requirements, the <code>unit</code> is included in the <code>LABEL</code> column when it is available</td>
</tr>
<tr class="even">
<td align="left"><strong>Specification documents in custom formats</strong></td>
<td align="right">24</td>
<td align="left">A project-wide data definition document can be rendered in a custom format using <code>rmarkdown::render</code></td>
</tr>
<tr class="odd">
<td align="left"><strong>Reconcile data set and specification</strong></td>
<td align="right">25</td>
<td align="left">The check procedure verifies that data set columns have the same names and same order as that found in the spec</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="right">26</td>
<td align="left">The check procedure verifies that all values in a continuous data column are greater than the minimum and less than maximum value given in the range field</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">27</td>
<td align="left">The check procedure verifies that all unique values in a discrete data column exactly overlap with the <code>values</code> field for that column in the specification object</td>
</tr>
<tr class="even">
<td align="left"><strong>R packages</strong></td>
<td align="right">28</td>
<td align="left">Imports: <code>yaml</code>, <code>dplyr</code>, <code>rmarkdown</code>, <code>knitr</code>, <code>xtable</code>, <code>rlang</code>, <code>assertthat</code>, <code>purrr</code>, <code>glue</code></td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="right">29</td>
<td align="left">Suggests: <code>pander</code>, <code>testthat</code></td>
</tr>
</tbody>
</table>
