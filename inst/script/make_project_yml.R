library(yspec)
proj <- ys_project( 
  ys_load("inst/spec/DEM104101F_PK.yml"), 
  ys_load("inst/spec/DEM104101F_PKPD.yml"), 
  ys_load("inst/spec/DEM104101F_AE.yml"), 
  output = "inst/spec/project.yml"
)
