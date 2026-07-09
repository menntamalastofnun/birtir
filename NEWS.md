# birtir 0.0.2

* Merged formula-based data description methods from `reiknir` directly into `birtir::describe_data()`.
* Added inline terminal visualization helpers including `inline_hist()`, `inline_boxplot()`, `inline_bar()`, `inline_scatter()`, and `inline_forest()`.
* Removed line-based parser directives (`#| h1:`, `#| h2:`, `#| text:`) in favor of using standard `md_text()` to output Markdown text and headings directly.
* Updated `render_analysis_md()` to parse and execute scripts as a single code block with interleaved code and output formatting when `show_code = TRUE`.
* Cleaned up repository hygiene, untracked IDE configurations (`birtir.Rproj`), and enabled pre-commit git hooks.
