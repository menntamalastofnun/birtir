# Legacy Workflow Note

`birtir` is being rebuilt around `render_analysis_md()` and plain Markdown
reports.

The older plotting helpers are still kept in the package for now because they
contain useful ideas, examples, and visual patterns worth revisiting during the
transition.

Current intent:

- `render_analysis_md()` is the primary workflow.
- Older exported plotting helpers are considered legacy.
- Legacy functions remain callable, but they now emit deprecation warnings.
- New work should build on the Markdown renderer rather than extend the old
  plotting/report pipeline.

This lets the old package stay visible as reference material without making it
the public direction of the project.
