# yspec 0.2.0.9000
- Fixed bug where column check error were assigned to the wrong column 
name #7
- Added `as_proj_spec` function that allows creation of a project
spec file from individual data set spec objects
- Added more flexibility for specifying project and data set
spec files in different locations #5

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