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
handling, but also allows several values to be maped to the same decode
- An error is generated if the number of characters in a column name 
is greater than 8.  Set the `ys.col.len` option to control the max allowable
characters.

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