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
#' - `md_text()`
#'
#' @param script Path to an `.R` script.
#' @param output_dir Root folder for rendered outputs. Defaults to `"outputs"`.
#' @param report_name Optional output/report name. Defaults to the script file
#'   name without extension.
#' @param params Optional named list of values exposed inside the script
#'   environment. Each entry is available by name, and the full list is also
#'   available as `params`.
#' @param show_code Logical; if `TRUE`, include source code blocks. Defaults to `FALSE`.
#' @param labels A label object created with [report_labels()]. Defaults to
#'   English `Table` / `Figure` labels.
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
                               report_name = NULL,
                               params = list(),
                               show_code = FALSE,
                               labels = report_labels()) {
  stopifnot(file.exists(script))
  stopifnot(is.character(script), length(script) == 1)
  stopifnot(is.character(output_dir), length(output_dir) == 1)
  stopifnot(
    is.null(report_name) ||
      (is.character(report_name) && length(report_name) == 1 && nzchar(report_name))
  )
  validate_render_params(params)
  stopifnot(is.logical(show_code), length(show_code) == 1, !is.na(show_code))
  validate_report_labels(labels)

  script_name <- tools::file_path_sans_ext(basename(script))
  output_name <- if (is.null(report_name)) script_name else report_name
  safe_name <- sanitize_name(output_name)

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
  state$safe_name <- safe_name
  state$labels <- labels

  envir <- new.env(parent = globalenv())
  list2env(params, envir = envir)
  envir$params <- params
  envir$.birtir_state <- state
  envir$md_table <- md_table
  envir$md_plot <- md_plot
  envir$md_text <- md_text
  envir$text_md <- md_text

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

validate_render_params <- function(params) {
  if (!is.list(params)) {
    stop("`params` must be a named list.", call. = FALSE)
  }

  if (length(params) == 0) {
    return(invisible(params))
  }

  param_names <- names(params)

  if (is.null(param_names) || anyNA(param_names) || any(!nzchar(param_names))) {
    stop("`params` must be a named list.", call. = FALSE)
  }

  if (anyDuplicated(param_names)) {
    stop("`params` names must be unique.", call. = FALSE)
  }

  reserved_names <- c(".birtir_state", "md_table", "md_plot", "md_text", "text_md", "params")
  conflicting_names <- intersect(param_names, reserved_names)

  if (length(conflicting_names) > 0) {
    stop(
      paste0(
        "`params` cannot use reserved names: ",
        paste(conflicting_names, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(params)
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

#' Create report labels for rendered captions
#'
#' Builds a small label object used by [render_analysis_md()] to name rendered
#' table and figure captions.
#'
#' @param table Label prefix used for rendered tables.
#' @param figure Label prefix used for rendered figures.
#'
#' @return A `birtir_report_labels` object.
#' @export
report_labels <- function(table = "Table", figure = "Figure") {
  labels <- list(
    table = table,
    figure = figure
  )

  validate_report_labels(labels)
  structure(labels, class = "birtir_report_labels")
}

validate_report_labels <- function(labels) {
  if (!is.list(labels)) {
    stop("`labels` must be created with `report_labels()`.", call. = FALSE)
  }

  required <- c("table", "figure")
  missing <- setdiff(required, names(labels))

  if (length(missing) > 0) {
    stop(
      paste0(
        "`labels` is missing required fields: ",
        paste(missing, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invalid <- !vapply(labels[required], function(x) {
    is.character(x) && length(x) == 1 && nzchar(x)
  }, logical(1))

  if (any(invalid)) {
    stop(
      "`labels$table` and `labels$figure` must be non-empty character strings.",
      call. = FALSE
    )
  }

  invisible(labels)
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

#' Format inline Markdown text with glue interpolation
#'
#' `md_text()` formats inline text with [glue::glue()] and applies lightweight
#' number formatting to values injected with `{...}` expressions.
#'
#' When called inside [render_analysis_md()], the formatted text is appended to
#' the active report as Markdown. Outside rendering, the text is printed as a
#' console preview and returned invisibly.
#'
#' @param text A glue-compatible character string.
#' @param ... Additional values passed to [glue::glue()].
#' @param digits Number of decimal places for numeric values. Defaults to `2`.
#' @param style Number formatting style. Use `"apa"` to suppress leading zeros
#'   for values between `-1` and `1`, or `"plain"` to keep standard decimals.
#' @param trailing_zeros Logical; if `TRUE`, keep a fixed number of decimals.
#'   Defaults to `TRUE`.
#' @param .envir Environment used to evaluate glue expressions. Defaults to the
#'   caller environment.
#'
#' @return Invisibly returns the formatted Markdown text.
#' @export
md_text <- function(text,
                    ...,
                    digits = 2,
                    style = c("apa", "plain"),
                    trailing_zeros = TRUE,
                    .envir = parent.frame()) {
  stopifnot(is.character(text), length(text) == 1)
  stopifnot(is.numeric(digits), length(digits) == 1, !is.na(digits), digits >= 0)
  stopifnot(is.logical(trailing_zeros), length(trailing_zeros) == 1, !is.na(trailing_zeros))

  digits <- as.integer(digits)
  style <- match.arg(style)

  rendered <- glue::glue(
    text,
    ...,
    .envir = .envir,
    .transformer = function(expr, envir) {
      value <- glue::identity_transformer(expr, envir)
      format_md_text_value(
        value = value,
        digits = digits,
        style = style,
        trailing_zeros = trailing_zeros
      )
    }
  )
  rendered <- as.character(rendered)

  state <- birtir_get_render_state()

  if (is.null(state)) {
    cat(rendered, "\n", sep = "")
    return(invisible(rendered))
  }

  add_md_line(state, rendered)
  add_md_line(state, "")

  invisible(rendered)
}

format_md_text_value <- function(value, digits, style, trailing_zeros) {
  if (length(value) == 0) {
    return("")
  }

  if (length(value) > 1) {
    return(paste(
      vapply(
        value,
        format_md_text_value,
        character(1),
        digits = digits,
        style = style,
        trailing_zeros = trailing_zeros
      ),
      collapse = ", "
    ))
  }

  if (is.numeric(value)) {
    return(format_md_number(
      value = value,
      digits = digits,
      style = style,
      trailing_zeros = trailing_zeros
    ))
  }

  as.character(value)
}

format_md_number <- function(value, digits, style, trailing_zeros) {
  if (is.na(value)) {
    return("NA")
  }

  rounded <- round(value, digits)

  if (trailing_zeros) {
    formatted <- formatC(rounded, format = "f", digits = digits)
  } else {
    formatted <- format(round(rounded, digits), nsmall = 0, trim = TRUE, scientific = FALSE)
  }

  if (identical(style, "apa") && abs(rounded) < 1 && !identical(rounded, 0)) {
    formatted <- sub("^(-?)0\\.", "\\1.", formatted)
  }

  formatted
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
    filename <- sprintf("%s_tbl-%03d.md", state$safe_name, state$table_index)
  } else {
    filename <- paste0(sanitize_name(filename), ".md")
  }

  table_path <- fs::path(state$tables_dir, filename)
  writeLines(table_md, table_path)

  if (!is.null(caption)) {
    add_md_line(
      state,
      paste0("**", state$labels$table, " ", state$table_index, ". ", caption, "**")
    )
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
    filename <- sprintf("%s_fig-%03d.png", state$safe_name, state$plot_index)
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
    add_md_line(
      state,
      paste0("**", state$labels$figure, " ", state$plot_index, ". ", caption, "**")
    )
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
