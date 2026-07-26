# Card & Krueger (1994) in base R

A compact reproduction of the restaurant-survey analysis in:

> Card, David, and Alan B. Krueger. 1994. “Minimum Wages and Employment: A Case Study of the Fast-Food Industry in New Jersey and Pennsylvania.” *American Economic Review* 84(4): 772–793.

The repository parses the released fixed-width `public.dat` file and constructs Tables 1–7 with base R. It also adds one small sensitivity check: the employment coefficient is recalculated while the FTE weight assigned to a part-time worker varies from 0 to 1.

## Run

From the repository root:

```r
source("analysis.R")
```

No packages are required. The script writes all results to `output/`.

The bundled `data/public.dat` contains the released 410 restaurant records in the fixed-width layout given by the authors’ codebook. If that file is deleted, the script downloads `njmin.zip` from David Card’s data page.

## Scope

Included:

- Table 1: every quantity identified by the 410-row released file;
- Tables 2–4: descriptive statistics and employment models;
- Table 5: every specification supported by released variables;
- Table 6: other employment-related outcomes;
- Table 7: full-meal price models;
- one continuous part-time-weight sensitivity check.

Not claimed as reproducible from `public.dat`:

- Table 1’s original wave-1 sampling frame, refusals, and response rates;
- Table 5 specifications 10–11, which require three-digit ZIP codes absent from the released file;
- Table 8, which uses separate national McDonald’s directories, CPS data, state minimum wages, population, and unemployment data.


## Main files

```text
analysis.R
data/
  public.dat
  published_targets.csv
output/
  table1.csv ... table7.csv
  table4_model5_check.csv
  table5_closed_store_convention_check.csv
  table7_summary.csv
  validation.csv
  validation_summary.csv
  part_time_weight_sensitivity.csv
  part_time_weight_sensitivity.png
```

`validation.csv` compares every encoded published cell with the independently calculated value at the precision printed in the article. It is the quickest way to distinguish exact matches, rounding-level disagreements, released-data disagreements, and cells that the archive cannot identify.

`table5_closed_store_convention_check.csv` reports specifications 3–5 under
both treatments of permanently closed stores. The main `table5.csv` retains the
convention that reproduces the printed table.

## Important findings

The headline balanced employment difference-in-differences estimate is approximately **+2.75 FTE workers per restaurant**, very close to the published +2.76.

The released data reproduce most headline and regression results, but not everything. Main differences are:

1. **Table 3, row 3 standard errors.** Row 3 subtracts the wave-1 average
   employment from the wave-2 average. The two averages are not from completely
   separate groups: many of the same restaurants appear in both waves, so the
   observations are related. That relationship affects the standard error, but
   the paper does not say how it was handled. The script estimates the
   relationship from restaurants observed in both waves rather than treating the
   two samples as independent.

2. **Table 4, model (v) controls.** The table note says this regression includes
   controls for restaurant chain, company ownership, and region. The supplied
   `check.sas` program includes chain and region but leaves out ownership, and
   that version is closer to the printed coefficient. The archive does not show
   whether the table note or the SAS program describes the regression actually
   used for publication, so the repository reports both versions.

3. **Table 5, specification 2 coefficient.** This specification treats four
   temporarily closed restaurants as having zero employment in wave 2. Applying
   that rule to the released file gives an NJ coefficient near 2.03, not the
   printed 2.20. The archive does not contain enough information to recover
   2.20 or determine whether it came from an earlier data version, different
   code, or a publication error. The repository therefore keeps the calculated
   2.03 and stores 2.20 only as the published comparison value.

4. **Table 5, specifications 3–5 closed-store convention.** The proportional
   change formula naturally gives -2 when a restaurant goes from positive
   employment to zero. Footnote 18 instead says closed restaurants were assigned
   -1. However, the printed coefficients in rows 3–5 match the released data only
   when the raw value of -2 is retained. I do not know the reason for mismatch, however, I applied both of the coefficients to show the divergence. The main `table5.csv` uses -2 to
   reproduce the printed table, while
   `table5_closed_store_convention_check.csv` shows results under both -2 and the
   paper-stated -1 convention.

5. **Table 6 wage-gap regressions.** The simple mean changes and regressions using
   the NJ indicator generally reproduce the article, which suggests that the
   outcome variables and broad samples were constructed correctly. Several
   regressions using the initial wage-gap measure do not reproduce the printed
   coefficients. The article and archive do not reveal an additional sample
   restriction or coding choice that resolves the difference, so the script
   follows the stated specification and leaves the disagreement as it is.

6. **Table 7 sample size.** Using the released file and the stated requirements
   produces 317 restaurants with complete data for the price regressions. The
   article reports 315 but does not identify two further exclusions. As such, given that I do not know which 2 to exclude, I used all 317 in the reproduction

The script never replaces calculated results with published values. Published targets are stored separately in `data/published_targets.csv`.

## Sources

- Paper: https://davidcard.berkeley.edu/papers/njmin-aer.pdf
- Authors’ data page: https://davidcard.berkeley.edu/data_sets.html
- Archive: https://davidcard.berkeley.edu/data_sets/njmin.zip
