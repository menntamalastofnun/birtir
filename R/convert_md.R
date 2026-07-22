#' Convert a Markdown file with Pandoc
#'
#' `convert_md()` converts an existing `.md` file to another output format by
#' calling the Pandoc executable available on the current system.
#'
#' @param path Path to an existing `.md` file.
#' @param to Output format. Currently supports `"docx"`, `"html"`, and `"pdf"`.
#' @param output Optional output path. Defaults to the same file stem as `path`
#'   with the extension implied by `to`.
#'
#' @return Invisibly returns the output file path.
#' @export
#'
#' @examples
#' \dontrun{
#' convert_md("report.md", to = "docx")
#' convert_md("report.md", to = "html")
#' }
convert_md <- function(path,
                       to = c("docx", "html", "pdf"),
                       output = NULL) {
  stopifnot(is.character(path), length(path) == 1, !is.na(path), nzchar(path))
  stopifnot(is.null(output) || (is.character(output) && length(output) == 1 && !is.na(output) && nzchar(output)))

  if (!file.exists(path)) {
    stop("`path` must point to an existing `.md` file.", call. = FALSE)
  }

  if (!identical(tolower(tools::file_ext(path)), "md")) {
    stop("`path` must point to an existing `.md` file.", call. = FALSE)
  }

  to <- match.arg(to)
  input_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  input_dir <- dirname(input_path)
  input_file <- basename(input_path)

  if (is.null(output)) {
    output <- fs::path(
      input_dir,
      paste0(tools::file_path_sans_ext(input_file), ".", to)
    )
  } else if (!fs::is_absolute_path(output)) {
    output <- fs::path(input_dir, output)
  }

  output <- fs::path_norm(output)
  fs::dir_create(dirname(output), recurse = TRUE)

  pandoc <- birtir_find_pandoc()

  if (!nzchar(pandoc)) {
    stop(
      "Pandoc is required for `convert_md()` but was not found on this system.",
      call. = FALSE
    )
  }

  pandoc_input <- birtir_prepare_md_for_conversion(input_path)

  if (!identical(pandoc_input, input_file)) {
    on.exit(unlink(fs::path(input_dir, pandoc_input)), add = TRUE)
  }

  status <- birtir_run_pandoc(
    pandoc = pandoc,
    input = pandoc_input,
    output = output,
    workdir = input_dir
  )

  if (!identical(status, 0L)) {
    stop(
      paste0(
        "Pandoc conversion failed for `",
        input_path,
        "` with exit status ",
        status,
        "."
      ),
      call. = FALSE
    )
  }

  invisible(output)
}

birtir_find_pandoc <- function() {
  Sys.which("pandoc")
}

birtir_run_pandoc <- function(pandoc, input, output, workdir) {
  old_wd <- setwd(workdir)
  on.exit(setwd(old_wd), add = TRUE)

  system2(
    command = pandoc,
    args = c(
      shQuote(input),
      "-o",
      shQuote(output)
    )
  )
}

birtir_prepare_md_for_conversion <- function(input_path) {
  input_dir <- dirname(input_path)
  input_file <- basename(input_path)
  lines <- readLines(input_path, warn = FALSE)
  cleaned_lines <- birtir_strip_export_only_lines(lines)

  if (identical(cleaned_lines, lines)) {
    return(input_file)
  }

  temp_path <- tempfile(
    pattern = paste0(tools::file_path_sans_ext(input_file), "-convert-"),
    tmpdir = input_dir,
    fileext = ".md"
  )
  writeLines(cleaned_lines, temp_path, useBytes = TRUE)

  basename(temp_path)
}

birtir_strip_export_only_lines <- function(lines) {
  keep <- !grepl(
    "^\\[Table:\\s+[^]]+_tbl-\\d+\\.md\\]\\([^)]*tables/[^)]+_tbl-\\d+\\.md\\)$",
    trimws(lines)
  )

  lines[keep]
}
