suppressPackageStartupMessages({
  library(AER)
  library(tidyverse)
})

dir.create("outputs", showWarnings = FALSE)

data("CASchools", package = "AER")

schools <- CASchools |>
  mutate(
    testscr = (read + math) / 2,
    str = students / teachers,
    english_pct = english,
    lunch_pct = lunch
  )

# Canonical specifications used in the Stock & Watson example.

# Model 1: baseline — does class size alone predict test scores?
# We expect negative coefficient on str (larger classes → lower scores, ~-2.28).
m1 <- lm(testscr ~ str, data = schools)

# Model 2: adds English-learner share to address omitted variable bias.
# Districts with high STR tend to also have more English learners (who score lower),
# so Model 1 likely overstates the pure class-size effect. Controlling for
# english_pct absorbs that confound; the STR coefficient shrinks toward ~-1.10.
m2 <- lm(testscr ~ str + english_pct, data = schools)

# Model 3: here we add lunch share (free/reduced-price lunch = poverty proxy).
# Controls further for socioeconomic disadvantage across districts. STR shrinks
# again toward ~-1.00, better isolating the class-size effect from poverty.
m3 <- lm(testscr ~ str + english_pct + lunch_pct, data = schools)

# Collect the STR coefficient and its standard error from each model into one table.
# str_se is used to build 95% confidence intervals: estimate ± 1.96 × SE.
coef_tbl <- tibble(
  model = c(
    "Model 1: testscr ~ str",
    "Model 2: testscr ~ str + english",
    "Model 3: testscr ~ str + english + lunch"
  ),
  str_estimate = c(coef(m1)["str"], coef(m2)["str"], coef(m3)["str"]),
  str_se = c(
    sqrt(vcov(m1)["str", "str"]),
    sqrt(vcov(m2)["str", "str"]),
    sqrt(vcov(m3)["str", "str"])
  )
) |>
  mutate(
    lower_95 = str_estimate - 1.96 * str_se,  # lower bound of 95% CI
    upper_95 = str_estimate + 1.96 * str_se   # upper bound of 95% CI
  )

# Rounded values from Stock and Watson that our estimates should match.
canonical <- tibble(
  model = coef_tbl$model,
  canonical_str = c(-2.28, -1.10, -1.00)
)

# This part joins the estimates to the benchmark values to see whether the replication matches them.
# `gap` measures the difference, and `matches_canonical` is TRUE if the gap is within 0.03 (which is generous for possible rounding error).
check_tbl <- coef_tbl |>
  left_join(canonical, by = "model") |>
  mutate(
    gap = str_estimate - canonical_str,
    matches_canonical = abs(gap) < 0.03
  )

# Saving the comparison table and the two plots
write_csv(check_tbl, "outputs/str_coefficients_check.csv")

scatter_plot <- ggplot(schools, aes(x = str, y = testscr)) +
  geom_point(alpha = 0.45, color = "#2B6CB0") +
  geom_smooth(method = "lm", se = TRUE, color = "#C53030") +
  labs(
    title = "Test Scores and Class Size (CASchools)",
    subtitle = "Each point is a California school district, 1998-99",
    x = "Student-teacher ratio (STR)",
    y = "Average test score",
    caption = "Data: AER::CASchools"
  ) +
  theme_minimal(base_size = 12)

coef_plot <- ggplot(check_tbl, aes(x = model, y = str_estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(ymin = lower_95, ymax = upper_95), color = "#C53030", linewidth = 0.8) +
  geom_point(aes(y = canonical_str), color = "#2F855A", size = 2.2) +
  labs(
    title = "Estimated STR effect across models",
    subtitle = "Red = this replication (95% CI), Green = canonical rounded values",
    x = NULL,
    y = "Coefficient on STR"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 12, hjust = 1))

ggsave("outputs/test_scores_vs_str.png", scatter_plot, width = 8, height = 5, dpi = 160)
ggsave("outputs/str_coefficients.png", coef_plot, width = 8, height = 5, dpi = 160)

cat("\nReplication complete.\n")
cat("Saved:\n")
cat("- outputs/str_coefficients_check.csv\n")
cat("- outputs/test_scores_vs_str.png\n")
cat("- outputs/str_coefficients.png\n\n")
print(check_tbl)
