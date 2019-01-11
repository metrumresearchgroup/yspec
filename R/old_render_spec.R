# ##' Render a data specification file
# ##'
# ##' @param x a data specification file name or a yspec object
# ##' @param stem for output file name
# ##' @param format function defining how to render the object
# ##' @param title used for yaml front matter
# ##' @param author used for yaml front matter
# ##' @param projectnumber project number; only used with mrgtemplate
# ##' @param sponsor project sponsor; only used with mrgtemplate
# ##' @param date used for yaml front matter
# ##' @param output_format passed to \code{rmarkdown::render}
# ##' @param output_dir passed to \code{rmarkdown::render}
# ##' @param build_dir where to build the document
# ##' @param ... passed to \code{rmarkdown::render}
# ##'
# ##' @examples
# ##'
# ##' file <- file_spec_ex()
# ##' render_spec(file)
# ##'
# ##' spec <- load_spec_ex()
# ##' 
# ##' \dontrun{
# ##'   render_spec(spec)
# ##' }
# ##' @export
# render_spec <- function(x, ...) UseMethod("render_spec")
# 
# ##' @rdname render_spec
# ##' @export
# render_spec.character <- function(x,
#                                   stem = basename(x),
#                                   format = c("x_table","pander_table","md_outline"),
#                                   title  = "Data Specification",
#                                   author =  "MetrumRG",
#                                   projectnumber = "", 
#                                   sponsor = "",
#                                   date = format(Sys.time()),
#                                   output_format="pdf_document",
#                                   output_dir = getwd(),
#                                   build_dir = tempdir(), ...) {
#   
#   return(render_define(as_proj_spec(load_spec(normalizePath(x)))))
#   
#   yamlfile <- normalizePath(x)
#   
#   output_dir <- normalizePath(output_dir)
#   
#   build_dir <- normalizePath(build_dir)
#   
#   copy_back <- FALSE
#   
#   cwd <- normalizePath(getwd())
#   if(cwd != build_dir) {
#     setwd(build_dir)
#     copy_back <- TRUE
#     on.exit(setwd(cwd))
#   }
#   
#   format <- match.arg(format)
#   
#   file <- paste0(stem, ".Rmd")
#   
#   rmd <- system.file("rmd", "spec.Rmd", package = "yspec")
#   
#   txt <- paste0(readLines(rmd), collapse = "\n")
#   
#   txt <- glue::glue(txt, .open = "<", .close = ">")
#   
#   writeLines(txt,file)
#   
#   ans <- rmarkdown::render(file, output_format = output_format, ...)
#   
#   if(copy_back) file.copy(ans, output_dir, overwrite = TRUE)
#   
#   return(invisible(ans))
# }
# 
# ##' @rdname render_spec
# ##' @export
# render_spec.yspec <- function(x, ...) {
#   render_spec.character(get_meta(x)[["yml_file"]], ...)
# }
# 
# ##' Generate code for a generic define document
# ##'
# ##' @param yamlfile a project file name
# ##' @param format a function or the name of a function to format the spec
# ##' contents
# ##' @export
# define_for_rmd <- function(yamlfile, format) {
#   
#   if(is.character(format)) {
#     format_fun <- get(format, mode = "function")
#   } else {
#     format_fun <- format
#   }
#   
#   assert_that(is.function(format_fun))
#   
#   proj <- load_spec_proj(yamlfile)
#   
#   specs <- imap(proj, .f = function(x,name) {
#     description <- proj[[name]][["description"]]
#     sp <- load_spec(x[["spec_file"]])
#     sp <- format_fun(sp)
#     c(paste0("# ", name),
#       "",
#       "__Description__: ",
#       description,"",
#       sp, " ")
#   })
#   
#   specs <- flatten_chr(specs)
#   
#   specs
# }
