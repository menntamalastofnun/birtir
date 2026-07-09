---
name: knit-readme
description: Explains how to compile/knit README.Rmd into README.md and README.html in the birtir workspace, satisfying pre-commit staging requirements.
---

# Knitting README Documentation in birtir

Use this skill when modifying package documentation in `README.Rmd` to generate clean output files and pass pre-commit hook validation.

## Steps

1. **Modify README.Rmd**: Edit `README.Rmd` as needed.
2. **Render R Markdown**: Execute the following R script command in the workspace root to compile the document and generate `README.md` and `README.html`:
   ```bash
   Rscript -e 'rmarkdown::render("README.Rmd")'
   ```
3. **Stage Together**: Stage both files and the backlog together to pass git hook checks:
   ```bash
   git add README.Rmd README.md
   ```
