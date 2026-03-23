# Manual Testing

These scripts are for quick interactive checks while developing `birtir`.

Suggested flow:

1. Run `dev/manual/smoke-render.R` to render the example analysis script.
2. Run `dev/manual/preview-helpers.R` to confirm that `md_table()` and
   `md_plot()` behave naturally outside `render_analysis_md()`.

Files:

- `regression_example.R`: example analysis script using `birtir::md_table()`
  and `birtir::md_plot()`
- `smoke-render.R`: end-to-end render check
- `preview-helpers.R`: interactive preview check for helpers
