# yspec (development version)

- Add `ys_col_note()` to generate a string of column names and definitions
  based on `short` or `label`; use this when column names appear in a table 
  and require explanation in a a footnote #119, #120.

# yspec 0.5.2

- Adds `ys_extend()` to extend a spec object with additional 
  columns with any external yaml file; adds `extend_file` 
  field in `SETUP__:` to identify a single file which can be 
  automatically called upon for extension #112, #113.

- `ys_add_factors()` refactored to only add factors for available 
  columns #88, #91.

- `ys_add_factors()` restores tidyselect semantics when specifying 
  a subset of columns for adding factors #109, #110. 

- Add `ys_table()` to render the spec in TeX table code to be included
  in a report appendix #98, #104.

- `ys_select()` returns an empty spec object when nothing is selected
  #100, #101.

- Add `ys_prune()` to select any data columns that appear in the spec
  as well as other optional columns #87, #97. #99.

- Add `ys_recode()` to convert a vector of data set column names to 
  `short` version, optionally adding `unit` #86, #114.

- `ys_load()` will now detect if `values` / `decode` are mis-coded as 
  a list of lists; all items in `values` and `decode` must be atomic 
  #106, #108.

- `ys_load()` will issue more informative error message when `short` 
  contains too many characters #102, 105. 

- `ys_document()` now accepts `sponsor` and `projectnumber` arguments
  which get passed through to the rendered define document #90, #92. 


# yspec 0.5.1

- Fix bug where dots in lookup file were not properly inherited by spec
  columns that didn't already have a dots list (#69)
  
- Fix bug in namespace creation when multiple fields include namespace info
  (#81)

- Fix bug where specification document didn't properly render when using 
  R 4.1 (#73)
  
- `ys_add_factors()` now substitutes missing values (`NA`) with a character 
  string supplied by the user prior to making the factor; the `decode` list 
  is also updated so that the "missing" data is the last level; the string is 
  supplied by the new argument `.missing` argument (#79)
  

# yspec 0.5.0

- `ys_select()` will now rename columns if new names are provided (#62)
- `ys_filter()` will subset a `yspec` object based on logical criteria (#65)
- `ys_rename()` will rename columns in a `yspec` object (#63)
- `ys_join()` will combine two or more `yspec` objects together (#64)
- New `SETUP__` field called `flags` that can be used to flag sets of 
  columns (#62)
- `ys_fill_dots()` will supplement the `dots` list with default values (#62)
- Add vignettes back to the package

# yspec 0.4.2
- Add `ys_select()` to select a subset of columns from a yspec object #24
- Refactor `axis_col_labs` to add vars argument; if missing, all col / labs
  will be created; otherwise, the user can select a subset to be created 
- Deprecate `mrgtemplate()`; added `definetemplate()` that is used as default
  for a more styled define document #37
- Add ability to include alternate representations of field information through
  namespacing #38
- Fix bug in `ys_check` where the return value was not correct when 
  `error_on_fail` was set to `FALSE` #31
- Add `nm_input` function to print nonmem input data text to the console #8
- Fix bug where some meta data (`data_stem`, `name`, `spec_file`) was not 
  updated when importing from another spec #5
- Fix bug where `ys_get_short_unit` was not returning a list #2
- Fix bug where `axis_col_labs` doesn't do title case or short max #26
- Better reporting of bad values in discrete data column #27


# yspec 0.4.1

- Add minimum version numbers for dependency packages
- Add `long` argument to `pander_table` to create longer tables
- Fix bug where `data_stem` meta field was not getting propagated into 
  data set name in the define document #35
- Add ability to update project number and sponsor information in a yspec
  object or a yproj object #36
- Fix bug where `unit` input was not properly validated #45
- Fix bug where `ys_check` didn't issue error when data set columns were not 
  in the right order #54
- New field for data set columns: `label`; this will be used when labeling 
  data sets for xpt export #60
- The label field can be made optional with `ys.require.label` option #99
- Add function (`ys_add_labels`) to label data set columns #63
- Add function (`update_short`) to change the value in the `short` field #82
- Add meta field (`import`) to enable import of contents of another spec #84
- Add meta field option (`character_last`) to push all character columns to 
  the back of the spec object #86
- Add meta field option (`comment_col`) to identify the column that is for 
  comments; character comment columns will not be pushed to back #86
- Add argument to `ys_check` so that an error is not generated when checking 
  the data, but rather `FALSE` is returned #88
- Add functionality to add all possible factors to data set with 
  `yspec_add_factors` #91
- Add new data column field `make_factor` that will tell `yspec_add_factors`
  to make that column into a factor #91
- The following fields will be collapsed into a single value when they are 
  entered as array: `source`, `comment`, `long` #93
- Add alias `ys_add_factors` to `yspec_add_factors` #97
- Add functions to form label, unit, and short; methods for `yspec` and 
  `ycol` #100

# yspec 0.3.0

- Released into production

# yspec 0.2.0.9000
- Fixed bug where column check error were assigned to the wrong column 
name #7
- Added `as_proj_spec` function that allows creation of a project
spec file from individual data set spec objects
- Added more flexibility for specifying project and data set
spec files in different locations #5
- Function `ys_load` preferred alias for `load_spec`
- Function `ys_load_file` preferred alias for `load_spec_file`
- Function `ys_project` preferred alias for `as_proj_spec`
- Function `ys_document` is a wrapper around both `render_define` and 
`render_fda_define`.  Use the `type` argument to request a "working" 
document or a "regulatory" document
- Added internal lookup source (`ysdb_internal.yml`)
- It is no longer an error to put no data in a column specification; 
the `lookup` attribute will be added and yspec will attempt to look up the 
column in formation in one of the lookup tables
- `ys_get_lookup` will recreate the lookup list for a yspec object
- `ys_lookup_source` will report on the source of each column in an yspec
object
- In general, we no longer write out a project file; use `ys_project` with 
a vector of files or yspec objects instead
- The table of contents for the regulatory document (`fda_content_table`)
now links to the corresponding `.xpt` file; to facilitate testing, arguments
`loc` and `ext` have been added so that the linking can be better tested 
(e.g. create a document that links to a `.csv` file, which should be easier
to open on computers that don't have SAS installed). #20
- Arguments `data_path` and `data_stem` have been added to `ys_load`
- Handler `!look` has been added to indicate that column data should be 
searched for in the lookup resources
- Handler `!value:decode` has been added so that maps can be created with the 
values on the left hand side (`- 0 : male`)
- Handler `!decode:value` has been added that recreates the default map 
handling, but also allows several values to be mapped to the same decode
- An error is generated if the number of characters in a column name 
is greater than 8.  Set the `ys.col.len` option to control the max allowable
characters.
- Added `verbose` argument to print information to the console while processing
a yaml file
- Added `ys_help` object that includes example / demo materials, including a 
function that will export a folder of assets that will work out of the box
- Revised package vignettes
- Expanded the `?yspec` help topic to outline the suggested workflow and 
point to other locations for help
- Added custom sanitizer `ys_sanitize` that can be changed through the 
`ys.sanitize` option
- Added a `glue` field for `SETUP__`, where a yaml map can be used to connect
value with a name; when the name is inserted in to yaml code as `<<name>>`, 
the value will be substituted in with `glue`; this is the proper way to evade
the sanitizer when including TeX code in the document
- Only "numeric" or "character" will be accepted in the `type` field; and error
is generated if anything else appears there
- Added `c` method to combine two yspec objects together

# yspec 0.1.0.9001

- `render_define` changed to generic function, with methods 
for `yproj` and `character
- Removed `projectnumber` and `sponsor` arguments to 
`render_define`; that information will be pulled 
from the `SETUP__` block as in `render_fda_define`
- The column widths for `x_table` were made smaller 
so that they fit on our standard page (8.5 x 11 with 7 cm 
margins on the left and the right)


# yspec 0.1.0
- Initial validated version
