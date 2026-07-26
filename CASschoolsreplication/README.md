# CASchools Class-Size Replication

 This mini repo reproduces the standard "test scores and class size" example using
`AER::CASchools` from Stock and Watson's introductory econometrics material 
(Stock, J.H. and Watson, M.W. (2007) Introduction to Econometrics. 2nd edn. Boston, MA: Pearson Addison Wesley).

## What this does

1. Builds the common variables:
   - `testscr = (read + math) / 2`
   - `str = students / teachers`
2. Runs three regressions:
   - `testscr ~ str`
   - `testscr ~ str + english`
   - `testscr ~ str + english + lunch`
3. Creates two simple plots with `ggplot2`.
4. Checks the estimated STR coefficients against canonical rounded values
   (`-2.28`, `-1.10`, `-1.00`).

## How to run

This script needs two packages: `AER` and `tidyverse`.

From this folder:

```bash
Rscript analysis.R
```


## Outputs

- `outputs/str_coefficients_check.csv`
- `outputs/test_scores_vs_str.png`
- `outputs/str_coefficients.png`

If `matches_canonical` is `TRUE` for all models, the replication matches the
standard textbook result up to rounding (0.03).
