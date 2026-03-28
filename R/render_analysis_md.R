#' Render an analysis script to Markdown
#'
#' Runs a plain `.R` analysis script and writes a plain `.md` report.
#'
#' `render_analysis_md()` evaluates the script, captures visible console output,
#' saves tables and ggplot objects created with [md_table()] and [md_plot()],
#' and writes a structured Markdown file into a report directory.
#'
#' The script may contain special comment directives:
#' - `#| h1: Title`
#' - `#| h2: Subtitle`
#' - `#| text: Some markdown text`
#'
#' It may also call helper functions:
#' - `md_table()`
#' - `md_plot()`
#'
#' @param script Path to an `.R` script.
#' @param output_dir Root folder for rendered outputs. Defaults to `"outputs"`.
#' @param show_code Logical; if `TRUE`, include source code blocks. Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the generated Markdown file.
#'
#' @details
#' `birtir` currently assumes one render at a time in a normal interactive
#' R session. Scripts may use `library(...)` and can affect the current
#' session while rendering.
#' @export
render_analysis_md <- function(script,
                               output_dir = "outputs",
                               show_code = FALSE) {
  stopifnot(file.exists(script))
  stopifnot(is.character(script), length(script) == 1)
  stopifnot(is.character(output_dir), length(output_dir) == 1)
  stopifnot(is.logical(show_code), length(show_code) == 1, !is.na(show_code))

  script_name <- tools::file_path_sans_ext(basename(script))
  safe_name <- sanitize_name(script_name)

  report_dir <- fs::path(output_dir, safe_name)
  images_dir <- fs::path(report_dir, "images")
  tables_dir <- fs::path(report_dir, "tables")
  md_path <- fs::path(report_dir, glue::glue("{safe_name}.md"))

  fs::dir_create(report_dir, recurse = TRUE)
  fs::dir_create(images_dir, recurse = TRUE)
  fs::dir_create(tables_dir, recurse = TRUE)

  script_lines <- readLines(script, warn = FALSE)

  state <- new.env(parent = emptyenv())
  state$md <- character()
  state$plot_index <- 0L
  state$table_index <- 0L
  state$images_dir <- images_dir
  state$tables_dir <- tables_dir
  state$report_dir <- report_dir
  state$script <- script

  envir <- new.env(parent = globalenv())
  envir$.birtir_state <- state
  envir$md_table <- md_table
  envir$md_plot <- md_plot
  envir$text_md <- function(text) {
    add_md_line(state, text)
    invisible(NULL)
  }

  birtir_set_render_state(state)
  on.exit(birtir_clear_render_state(), add = TRUE)

  blocks <- parse_analysis_blocks(script_lines, script = script)

  for (block in blocks) {
    if (identical(block$type, "directive")) {
      emit_directive(state, block$value, block$text)
      next
    }

    result <- evaluate_analysis_block(
      block$lines,
      envir = envir,
      script = script
    )

    if (show_code && nzchar(trimws(result$code))) {
      add_md_line(state, "```r")
      add_md_line(state, result$code)
      add_md_line(state, "```")
      add_md_line(state, "")
    }

    add_md_text_block(state, result$lines)

    if (!is.null(result$error)) {
      writeLines(state$md, md_path)
      stop(result$error, call. = FALSE)
    }
  }

  writeLines(state$md, md_path)
  invisible(md_path)
}

birtir_runtime <- new.env(parent = emptyenv())

birtir_set_render_state <- function(state) {
  assign("state", state, envir = birtir_runtime)
  invisible(NULL)
}

birtir_get_render_state <- function() {
  if (!exists("state", envir = birtir_runtime, inherits = FALSE)) {
    return(NULL)
  }

  get("state", envir = birtir_runtime, inherits = FALSE)
}

birtir_clear_render_state <- function() {
  if (exists("state", envir = birtir_runtime, inherits = FALSE)) {
    rm("state", envir = birtir_runtime)
  }

  invisible(NULL)
}

#' Format a table for Markdown reports
#'
#' When called inside [render_analysis_md()], the table is written into the
#' active report and saved under the report's `tables/` directory.
#'
#' When called outside a render context, the table is printed as a pipe-style
#' Markdown preview in the console.
#'
#' @param x A data frame or table-like object.
#' @param caption Optional caption text.
#' @param filename Optional file stem used during rendering. The `.md`
#'   extension is added automatically.
#' @param digits Optional number of digits for numeric columns.
#'
#' @return Invisibly returns the saved table path during rendering, or the
#'   Markdown table text outside rendering.
#'
#' @details
#' During rendering, filenames are saved into the active report's `tables/`
#' directory. Outside rendering, `md_table()` is a lightweight preview helper.
#' @export
md_table <- function(x, caption = NULL, filename = NULL, digits = NULL) {
  out <- x

  if (!is.null(digits) && is.data.frame(out)) {
    out <- dplyr::mutate(
      out,
      dplyr::across(where(is.numeric), ~ round(.x, digits))
    )
  }

  table_md <- knitr::kable(out, format = "pipe")
  state <- birtir_get_render_state()

  if (is.null(state)) {
    preview <- character()

    if (!is.null(caption)) {
      preview <- c(preview, paste0("**", caption, "**"), "")
    }

    preview <- c(preview, table_md)
    cat(paste(preview, collapse = "\n"), "\n", sep = "")

    return(invisible(table_md))
  }

  invisible(write_md_table(
    state = state,
    table_md = table_md,
    caption = caption,
    filename = filename
  ))
}

#' Format a plot for Markdown reports
#'
#' When called inside [render_analysis_md()], the plot is saved and embedded in
#' the active report.
#'
#' When called outside a render context, the plot is printed normally for
#' interactive use.
#'
#' @param plot A ggplot object.
#' @param caption Optional caption text.
#' @param filename Optional file stem used during rendering. The `.png`
#'   extension is added automatically.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @param dpi Plot resolution.
#'
#' @return Invisibly returns the saved plot path during rendering, or the plot
#'   object outside rendering.
#'
#' @details
#' `md_plot()` saves ggplot objects into the active report during rendering and
#' behaves like a normal plot preview helper otherwise.
#' @export
md_plot <- function(plot, caption = NULL, filename = NULL,
                    width = 7, height = 5, dpi = 300) {
  state <- birtir_get_render_state()

  if (is.null(state)) {
    print(plot)
    return(invisible(plot))
  }

  invisible(write_md_plot(
    state = state,
    plot = plot,
    caption = caption,
    filename = filename,
    width = width,
    height = height,
    dpi = dpi
  ))
}

parse_analysis_blocks <- function(lines, script = NULL) {
  blocks <- list()
  current_code <- character()

  flush_code <- function() {
    parsed_code <- paste(current_code, collapse = "\n")
    if (!nzchar(trimws(parsed_code))) {
      current_code <<- character()
      return(invisible(NULL))
    }

    blocks[[length(blocks) + 1L]] <<- list(
      type = "code",
      lines = current_code
    )
    current_code <<- character()
    invisible(NULL)
  }

  for (i in seq_along(lines)) {
    line <- lines[[i]]
    trimmed <- trimws(line)

    if (grepl("^#\\|\\s*(h1|h2|text)\\s*:", trimmed)) {
      flush_code()

      directive <- sub("^#\\|\\s*([a-zA-Z0-9_]+)\\s*:.*$", "\\1", trimmed)
      text <- sub("^#\\|\\s*[a-zA-Z0-9_]+\\s*:\\s*", "", trimmed)

      blocks[[length(blocks) + 1L]] <- list(
        type = "directive",
        value = directive,
        text = text
      )
      next
    }

    if (grepl("^#\\|", trimmed)) {
      location <- if (is.null(script)) {
        paste0("line ", i)
      } else {
        paste0(script, " near line ", i)
      }

      stop(
        paste0(
          "Invalid birtir directive in ",
          location,
          ": `",
          trimmed,
          "`. Supported directives are `#| h1:`, `#| h2:`, and `#| text:`."
        ),
        call. = FALSE
      )
    }

    current_code <- c(current_code, line)
  }

  flush_code()
  blocks
}

emit_directive <- function(state, directive, text) {
  if (identical(directive, "h1")) {
    add_md_line(state, paste0("# ", text))
    add_md_line(state, "")
    return(invisible(NULL))
  }

  if (identical(directive, "h2")) {
    add_md_line(state, paste0("## ", text))
    add_md_line(state, "")
    return(invisible(NULL))
  }

  if (identical(directive, "text")) {
    add_md_line(state, text)
    add_md_line(state, "")
    return(invisible(NULL))
  }

  invisible(NULL)
}

write_md_table <- function(state, table_md, caption = NULL, filename = NULL) {
  state$table_index <- state$table_index + 1L

  if (is.null(filename)) {
    filename <- sprintf("tbl-%03d.md", state$table_index)
  } else {
    filename <- paste0(sanitize_name(filename), ".md")
  }

  table_path <- fs::path(state$tables_dir, filename)
  writeLines(table_md, table_path)

  if (!is.null(caption)) {
    add_md_line(state, paste0("**", caption, "**"))
  }

  rel_path <- fs::path_rel(table_path, start = state$report_dir)
  add_md_line(state, paste0("[Table: ", filename, "](", rel_path, ")"))
  add_md_line(state, "")
  add_md_line(state, table_md)
  add_md_line(state, "")

  invisible(table_path)
}

write_md_plot <- function(state, plot, caption = NULL, filename = NULL,
                          width = 7, height = 5, dpi = 300) {
  state$plot_index <- state$plot_index + 1L

  if (is.null(filename)) {
    filename <- sprintf("fig-%03d.png", state$plot_index)
  } else {
    filename <- paste0(sanitize_name(filename), ".png")
  }

  plot_path <- fs::path(state$images_dir, filename)

  ggplot2::ggsave(
    filename = plot_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )

  if (!is.null(caption)) {
    add_md_line(state, paste0("**Figure ", state$plot_index, ". ", caption, "**"))
  }

  rel_path <- fs::path_rel(plot_path, start = state$report_dir)
  add_md_line(state, paste0("![](", rel_path, ")"))
  add_md_line(state, "")

  invisible(plot_path)
}

evaluate_analysis_block <- function(lines, envir, script) {
  code <- paste(lines, collapse = "\n")
  srcfile <- srcfilecopy(script, lines)

  expressions <- tryCatch(
    parse(text = code, keep.source = TRUE, srcfile = srcfile),
    error = function(error) {
      list(
        code = code,
        lines = paste0("Error: ", conditionMessage(error)),
        error = paste0("Failed to parse ", script, ": ", conditionMessage(error))
      )
    }
  )

  if (is.list(expressions) && !is.expression(expressions)) {
    return(expressions)
  }

  output_lines <- character()
  srcrefs <- attr(expressions, "srcref", exact = TRUE)

  for (i in seq_along(expressions)) {
    result <- evaluate_parsed_expression(
      expressions[[i]],
      envir = envir,
      script = script,
      srcref = srcrefs[[i]]
    )
    output_lines <- c(output_lines, result$lines)

    if (!is.null(result$error)) {
      return(list(code = code, lines = output_lines, error = result$error))
    }
  }

  list(code = code, lines = output_lines, error = NULL)
}

evaluate_parsed_expression <- function(expr, envir, script, srcref = NULL) {
  lines <- character()

  append_condition <- function(prefix, condition) {
    lines <<- c(lines, paste0(prefix, conditionMessage(condition)))
  }

  output <- tryCatch(
    utils::capture.output(
      withCallingHandlers(
        {
          value <- withVisible(eval(expr, envir = envir))
          if (isTRUE(value$visible)) {
            print(value$value)
          }

          invisible(NULL)
        },
        message = function(condition) {
          append_condition("Message: ", condition)
          invokeRestart("muffleMessage")
        },
        warning = function(condition) {
          append_condition("Warning: ", condition)
          invokeRestart("muffleWarning")
        }
      ),
      type = "output"
    ),
    error = function(condition) {
      structure(
        list(
          message = format_eval_error(expr, script, condition, srcref = srcref),
          line = paste0("Error: ", conditionMessage(condition))
        ),
        class = "birtir_eval_error"
      )
    }
  )

  if (inherits(output, "birtir_eval_error")) {
    return(list(
      lines = c(lines, output$line),
      error = output$message
    ))
  }

  list(
    lines = c(lines, output),
    error = NULL
  )
}

format_eval_error <- function(expr, script, condition, srcref = attr(expr, "srcref", exact = TRUE)) {
  if (!is.null(srcref)) {
    line <- srcref[[1]]
    return(
      paste0(
        "Error while evaluating ",
        script,
        " near line ",
        line,
        ": ",
        conditionMessage(condition)
      )
    )
  }

  paste0("Error while evaluating ", script, ": ", conditionMessage(condition))
}
