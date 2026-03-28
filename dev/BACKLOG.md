# birtir backlog

## Render context

- Revisit the current render state design.
- Current assumption: one render at a time in a normal interactive R session.
- Risk: a single package-global render state can still become fragile if future
  work adds nested renders, batch rendering, or helper calls that outlive a
  render.
- Explore replacing the single global state with a stack-based or token-based
  render context.

## Session side effects

- Decide how much a rendered script is allowed to affect the calling session.
- Current behavior allows `library(...)`, attached packages, options changes,
  random seed changes, and globals created during the analysis script.
- This is convenient for interactive use, but weakens reproducibility and
  isolation.
- Consider whether some cleanup or scoped execution should be added later.

## Directive parsing

- Current directives are line-based and only support:
  - `#| h1:`
  - `#| h2:`
  - `#| text:`
- Think about whether this remains intentionally minimal or if more structure
  will be needed later.
- If more directives are added, keep errors clear and parsing predictable.

## Helper API

- Keep `md_table()` and `md_plot()` as the shared public script API.
- Consider adding lightweight validation:
  - clearer error for non-ggplot input to `md_plot()`
  - clearer error if helpers are called with a broken render state

## Testing gaps

- Add tests for render-state cleanup after helper failures.
- Add tests for malformed helper inputs.
- Add tests for scripts that intentionally modify session state, so expected
  behavior is explicit.
