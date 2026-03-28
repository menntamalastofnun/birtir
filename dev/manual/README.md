# Manual Testing

These scripts are for quick interactive checks of the current `birtir`
workflow.

Current package story:

- write a plain analysis script
- use `birtir::md_table()` and `birtir::md_plot()` inside that script
- call `birtir::render_analysis_md()` to create the Markdown report
- use the same helpers interactively outside rendering when needed

Suggested flow:

1. Run `dev/manual/smoke-render.R` to render the example analysis script.
2. Run `dev/manual/preview-helpers.R` to confirm that `md_table()` and
   `md_plot()` behave naturally outside `render_analysis_md()`.

Files:

- `regression_example.R`: example analysis script using `birtir::md_table()`
  and `birtir::md_plot()`
- `smoke-render.R`: end-to-end render check
- `preview-helpers.R`: interactive preview check for helpers

Current assumptions:

- one render at a time in a normal interactive R session
- helpers are the public script API
- supported directives are `#| h1:`, `#| h2:`, and `#| text:`
