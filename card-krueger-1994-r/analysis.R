# Card & Krueger (1994): reproduction from the released restaurant file
# Run from the repository root with: source("analysis.R")
# Base R only.

options(stringsAsFactors = FALSE)
dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# 1. Read the released fixed width file

data_url <- "https://davidcard.berkeley.edu/data_sets/njmin.zip"
data_file <- file.path("data", "public.dat")

if (!file.exists(data_file)) {
  message("data/public.dat is missing; downloading the authors' archive.")
  tmp <- tempfile(fileext = ".zip")
  download.file(data_url, tmp, mode = "wb", quiet = TRUE)
  unzip(tmp, files = "public.dat", exdir = "data")
  unlink(tmp) }

var_names <- c(
  "SHEET", "CHAIN", "CO_OWNED", "STATE", "SOUTHJ", "CENTRALJ",
  "NORTHJ", "PA1", "PA2", "SHORE", "NCALLS", "EMPFT", "EMPPT",
  "NMGRS", "WAGE_ST", "INCTIME", "FIRSTINC", "BONUS", "PCTAFF",
  "MEALS", "OPEN", "HRSOPEN", "PSODA", "PFRY", "PENTREE", "NREGS",
  "NREGS11", "TYPE2", "STATUS2", "DATE2", "NCALLS2", "EMPFT2",
  "EMPPT2", "NMGRS2", "WAGE_ST2", "INCTIME2", "FIRSTIN2", "SPECIAL2",
  "MEALS2", "OPEN2R", "HRSOPEN2", "PSODA2", "PFRY2", "PENTREE2",
  "NREGS2", "NREGS112")

starts <- c(
  1, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 26, 32, 38, 44, 50,
  56, 62, 64, 70, 72, 78, 84, 90, 96, 102, 105, 108, 110, 112,
  119, 122, 128, 134, 140, 146, 152, 158, 160, 162, 168, 174,
  180, 186, 192, 195)

ends <- c(
  3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 24, 30, 36, 42, 48, 54,
  60, 62, 68, 70, 76, 82, 88, 94, 100, 103, 106, 108, 110, 117,
  120, 126, 132, 138, 144, 150, 156, 158, 160, 166, 172, 178,
  184, 190, 193, 196)

read_public <- function(path) {
  lines <- readLines(path, warn = FALSE)
  values <- list()
  for (j in seq_along(var_names)) {
    x <- substr(lines, starts[j], ends[j])
    x <- trimws(x)
    x[x == "" | x == "."] <- NA_character_
    values[[j]] <- as.numeric(x) }
  names(values) <- var_names
  as.data.frame(values, check.names = FALSE) }

d <- read_public(data_file)
stopifnot(nrow(d) == 410L, ncol(d) == 46L)
# The official archive reuses SHEET 407 for two stores in different
# chains and states, so SHEET alone is not a unique record identifier.
stopifnot(length(unique(d$SHEET)) == 409L, sum(d$SHEET == 407) == 2L)
stopifnot(anyDuplicated(d[c("SHEET", "CHAIN", "STATE")]) == 0L)

# 2. Variables used in the paper

d$NJ <- d$STATE
d$BK <- as.numeric(d$CHAIN == 1)
d$KFC <- as.numeric(d$CHAIN == 2)
d$ROYS <- as.numeric(d$CHAIN == 3)
d$WENDYS <- as.numeric(d$CHAIN == 4)

d$FTE1 <- d$EMPFT + d$NMGRS + 0.5 * d$EMPPT
d$FTE2 <- d$EMPFT2 + d$NMGRS2 + 0.5 * d$EMPPT2
d$DEMP <- d$FTE2 - d$FTE1

d$DWAGE <- d$WAGE_ST2 - d$WAGE_ST
below_minimum <- !is.na(d$WAGE_ST) & d$WAGE_ST < 5.05
at_or_above_minimum <- !is.na(d$WAGE_ST) & d$WAGE_ST >= 5.05
gap_to_minimum <- rep(NA_real_, nrow(d))
gap_to_minimum[below_minimum] <-
  (5.05 - d$WAGE_ST[below_minimum]) / d$WAGE_ST[below_minimum]
gap_to_minimum[at_or_above_minimum] <- 0

nj_rows <- !is.na(d$NJ) & d$NJ == 1
pa_rows <- !is.na(d$NJ) & d$NJ == 0
d$GAP <- NA_real_
d$GAP[nj_rows] <- gap_to_minimum[nj_rows]
d$GAP[pa_rows] <- 0

d$PMEAL1 <- d$PSODA + d$PFRY + d$PENTREE
d$PMEAL2 <- d$PSODA2 + d$PFRY2 + d$PENTREE2
d$DLOGMEAL <- log(d$PMEAL2) - log(d$PMEAL1)

d$CLOSED <- d$STATUS2 == 3
d$FRACFT1 <- d$EMPFT / d$FTE1
d$FRACFT2 <- ifelse(d$FTE2 > 0, d$EMPFT2 / d$FTE2, NA_real_)

# SAS comparisons treat missing wage values as false for these indicators.
d$ATMIN1 <- as.numeric(d$WAGE_ST == 4.25)
d$ATMIN1[is.na(d$WAGE_ST)] <- 0
d$ATMIN2 <- as.numeric(d$WAGE_ST2 == 4.25)
d$ATMIN2[is.na(d$WAGE_ST2)] <- 0
d$NEWMIN2 <- as.numeric(d$WAGE_ST2 == 5.05)
d$NEWMIN2[is.na(d$WAGE_ST2)] <- 0

d$WAGECAT <- NA_character_
d$WAGECAT[!is.na(d$WAGE_ST) & d$NJ == 1 & d$WAGE_ST == 4.25] <- "low"
d$WAGECAT[!is.na(d$WAGE_ST) & d$NJ == 1 & d$WAGE_ST > 4.25 & d$WAGE_ST < 5.00] <- "mid"
d$WAGECAT[!is.na(d$WAGE_ST) & d$NJ == 1 & d$WAGE_ST >= 5.00] <- "high"

# Three interview period dummies used in Table 5, specification 7.
# One released date is coded 111191; it is treated as 111192.
date2 <- d$DATE2
date2[!is.na(date2) & date2 == 111191] <- 111192
d$WEEK1 <- as.numeric(!is.na(date2) & date2 >= 111292 & date2 <= 111992)
d$WEEK2 <- as.numeric(!is.na(date2) & date2 >= 120992 & date2 <= 121592)
d$WEEK3 <- as.numeric(!is.na(date2) & date2 >= 121692)

# Table 6 outcomes.
d$DFRACFT <- 100 * (d$FRACFT2 - d$FRACFT1)
d$DHRSOPEN <- d$HRSOPEN2 - d$HRSOPEN
d$DNREGS <- d$NREGS2 - d$NREGS
d$DNREGS11 <- d$NREGS112 - d$NREGS11

d$LOWPRICE1 <- ifelse(is.na(d$MEALS), NA_real_, 100 * as.numeric(d$MEALS == 2))
d$LOWPRICE2 <- ifelse(is.na(d$MEALS2), NA_real_, 100 * as.numeric(d$MEALS2 == 2))
d$DLOWPRICE <- d$LOWPRICE2 - d$LOWPRICE1

d$FREEMEAL1 <- ifelse(is.na(d$MEALS), NA_real_, 100 * as.numeric(d$MEALS == 1))
d$FREEMEAL2 <- ifelse(is.na(d$MEALS2), NA_real_, 100 * as.numeric(d$MEALS2 == 1))
d$DFREEMEAL <- d$FREEMEAL2 - d$FREEMEAL1

d$COMBO1 <- ifelse(is.na(d$MEALS), NA_real_, 100 * as.numeric(d$MEALS == 3))
d$COMBO2 <- ifelse(is.na(d$MEALS2), NA_real_, 100 * as.numeric(d$MEALS2 == 3))
d$DCOMBO <- d$COMBO2 - d$COMBO1

d$DINCTIME <- d$INCTIME2 - d$INCTIME
d$DFIRSTINC <- d$FIRSTIN2 - d$FIRSTINC
d$SLOPE1 <- 100 * d$FIRSTINC / (d$WAGE_ST * d$INCTIME)
d$SLOPE2 <- 100 * d$FIRSTIN2 / (d$WAGE_ST2 * d$INCTIME2)
d$DSLOPE <- d$SLOPE2 - d$SLOPE1

# 3. Small helpers

mean_se <- function(x) {
  x <- x[!is.na(x)]
  c(mean = mean(x), se = sd(x) / sqrt(length(x)), n = length(x)) }

welch_t <- function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  (mean(x) - mean(y)) / sqrt(var(x) / length(x) + var(y) / length(y)) }

difference_se <- function(se1, se2) sqrt(se1^2 + se2^2)

overlap_change <- function(x1, x2) {
  n1 <- sum(!is.na(x1))
  n2 <- sum(!is.na(x2))
  common <- !is.na(x1) & !is.na(x2)
  covariance <- cov(x1[common], x2[common])
  variance <- var(x1, na.rm = TRUE) / n1 +
    var(x2, na.rm = TRUE) / n2 -
    2 * sum(common) * covariance / (n1 * n2)
  c(
    mean = mean(x2, na.rm = TRUE) - mean(x1, na.rm = TRUE),
    se = sqrt(variance),
    n = min(n1, n2)) }

joint_p <- function(restricted, full) {
  unname(anova(restricted, full)$`Pr(>F)`[2]) }

write_out <- function(x, name) {
  write.csv(x, file.path("output", name), row.names = FALSE, na = "") }

# 4. Table 1: sample design

count_state <- function(mask, state = NULL) {
  if (!is.null(state)) mask <- mask & d$NJ == state
  sum(mask, na.rm = TRUE) }

table1 <- data.frame(
  wave = c(rep("Wave 1", 4), rep("Wave 2", 6)),
  item = c(
    "Number of stores in sample frame",
    "Number of refusals",
    "Number interviewed",
    "Response rate (percentage)",
    "Number of stores in sample frame",
    "Number closed",
    "Number under renovation",
    "Number temporarily closed",
    "Number of refusals",
    "Number interviewed"),
  published_all = c(473, 63, 410, 86.7, 410, 6, 2, 2, 1, 399),
  published_NJ = c(364, 33, 331, 90.9, 331, 5, 2, 2, 1, 321),
  published_PA = c(109, 30, 79, 72.5, 79, 1, 0, 0, 0, 78),
  reproduced_all = c(
    NA, NA, nrow(d), NA, nrow(d),
    count_state(d$STATUS2 == 3),
    count_state(d$STATUS2 == 2),
    count_state(d$STATUS2 %in% c(4, 5)),
    count_state(d$STATUS2 == 0),
    count_state(d$STATUS2 == 1)),
  reproduced_NJ = c(
    NA, NA, sum(d$NJ == 1), NA, sum(d$NJ == 1),
    count_state(d$STATUS2 == 3, 1),
    count_state(d$STATUS2 == 2, 1),
    count_state(d$STATUS2 %in% c(4, 5), 1),
    count_state(d$STATUS2 == 0, 1),
    count_state(d$STATUS2 == 1, 1)),
  reproduced_PA = c(
    NA, NA, sum(d$NJ == 0), NA, sum(d$NJ == 0),
    count_state(d$STATUS2 == 3, 0),
    count_state(d$STATUS2 == 2, 0),
    count_state(d$STATUS2 %in% c(4, 5), 0),
    count_state(d$STATUS2 == 0, 0),
    count_state(d$STATUS2 == 1, 0)),
  note = c(
    "not identifiable from 410-row respondent file",
    "not identifiable from 410-row respondent file",
    "identified by row/state counts",
    "requires original sample frame",
    "", "", "", "", "", ""))
write_out(table1, "table1.csv")

# 5. Table 2: descriptive statistics

table2_spec <- data.frame(
  section = c(
    rep("Store types", 5),
    rep("Wave 1", 7),
    rep("Wave 2", 8)),
  variable = c(
    "Burger King", "KFC", "Roy Rogers", "Wendy's", "Company-owned",
    "FTE employment", "Percentage full-time employees", "Starting wage",
    "Wage = $4.25 (percentage)", "Price of full meal",
    "Hours open (weekday)", "Recruiting bonus (percentage)",
    "FTE employment", "Percentage full-time employees", "Starting wage",
    "Wage = $4.25 (percentage)", "Wage = $5.05 (percentage)",
    "Price of full meal", "Hours open (weekday)",
    "Recruiting bonus (percentage)"),
  source = c(
    "BK", "KFC", "ROYS", "WENDYS", "CO_OWNED",
    "FTE1", "FRACFT1", "WAGE_ST", "ATMIN1", "PMEAL1", "HRSOPEN", "BONUS",
    "FTE2", "FRACFT2", "WAGE_ST2", "ATMIN2", "NEWMIN2",
    "PMEAL2", "HRSOPEN2", "SPECIAL2"),
  scale = c(
    rep(100, 5), 1, 100, 1, 100, 1, 1, 100,
    1, 100, 1, 100, 100, 1, 1, 100),
  paper_mean_digits = c(
    rep(1, 5), 1, 1, 2, 1, 2, 1, 1,
    1, 1, 2, 1, 1, 2, 1, 1))

table2 <- data.frame()
for (i in seq_len(nrow(table2_spec))) {
  s <- table2_spec[i, ]
  values <- d[[s$source]] * s$scale
  x_nj <- values[d$NJ == 1]
  x_pa <- values[d$NJ == 0]
  nj <- mean_se(x_nj)
  pa <- mean_se(x_pa)
  result_row <- data.frame(
    section = s$section,
    variable = s$variable,
    NJ_mean = nj["mean"], NJ_se = nj["se"], NJ_n = nj["n"],
    PA_mean = pa["mean"], PA_se = pa["se"], PA_n = pa["n"],
    t_stat = welch_t(x_nj, x_pa),
    paper_mean_digits = s$paper_mean_digits)
  table2 <- rbind(table2, result_row) }
write_out(table2, "table2.csv")

# 6. Table 3: employment before and after

group_masks <- list(
  PA = d$NJ == 0,
  NJ = d$NJ == 1,
  NJ_low = d$WAGECAT == "low",
  NJ_mid = d$WAGECAT == "mid",
  NJ_high = d$WAGECAT == "high")
for (group_name in names(group_masks)) {
  rows_to_keep <- group_masks[[group_name]]
  rows_to_keep[is.na(rows_to_keep)] <- FALSE
  group_masks[[group_name]] <- rows_to_keep }

make_table3_row <- function(row, description, stats, note = "") {
  pa <- stats$PA; nj <- stats$NJ
  low <- stats$NJ_low; mid <- stats$NJ_mid; high <- stats$NJ_high
  data.frame(
    row = row,
    description = description,
    PA = pa["mean"], PA_se = pa["se"], PA_n = pa["n"],
    NJ = nj["mean"], NJ_se = nj["se"], NJ_n = nj["n"],
    NJ_minus_PA = nj["mean"] - pa["mean"],
    NJ_minus_PA_se = difference_se(nj["se"], pa["se"]),
    NJ_low = low["mean"], NJ_low_se = low["se"], NJ_low_n = low["n"],
    NJ_mid = mid["mean"], NJ_mid_se = mid["se"], NJ_mid_n = mid["n"],
    NJ_high = high["mean"], NJ_high_se = high["se"], NJ_high_n = high["n"],
    low_minus_high = low["mean"] - high["mean"],
    low_minus_high_se = difference_se(low["se"], high["se"]),
    mid_minus_high = mid["mean"] - high["mean"],
    mid_minus_high_se = difference_se(mid["se"], high["se"]),
    note = note) }

fte2_temp_zero <- d$FTE2
fte2_temp_zero[d$STATUS2 %in% c(2, 4, 5)] <- 0
demp_temp_zero <- fte2_temp_zero - d$FTE1

row1_stats <- list()
row2_stats <- list()
row3_stats <- list()
row4_stats <- list()
row5_stats <- list()

for (group_name in names(group_masks)) {
  rows_to_keep <- group_masks[[group_name]]
  employment_before <- d$FTE1[rows_to_keep]
  employment_after <- d$FTE2[rows_to_keep]
  employment_change <- d$DEMP[rows_to_keep]
  temporary_zero_change <- demp_temp_zero[rows_to_keep]

  row1_stats[[group_name]] <- mean_se(employment_before)
  row2_stats[[group_name]] <- mean_se(employment_after)
  row3_stats[[group_name]] <- overlap_change(employment_before, employment_after)
  row4_stats[[group_name]] <- mean_se(employment_change)
  row5_stats[[group_name]] <- mean_se(temporary_zero_change) }

table3 <- rbind(
  make_table3_row(
    1, "FTE employment before, all available observations", row1_stats),
  make_table3_row(
    2, "FTE employment after, all available observations", row2_stats),
  make_table3_row(
    3, "Change in mean FTE employment", row3_stats,
    "row 3 SE uses overlap-aware variance of two wave means; paper does not document its row-3 SE formula"),
  make_table3_row(
    4, "Change in mean FTE employment, balanced sample", row4_stats),
  make_table3_row(
    5, "Change in mean FTE employment, temporary closures set to zero", row5_stats))
write_out(table3, "table3.csv")

# 7. Table 4: reduced form employment models

base_keep <- !is.na(d$DEMP) &
  (d$CLOSED | (!d$CLOSED & !is.na(d$DWAGE)))
base <- d[base_keep, , drop = FALSE]
stopifnot(nrow(base) == 357L)

m4_i <- lm(
  DEMP ~ NJ,
  data = base, na.action = na.omit)
m4_ii <- lm(
  DEMP ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = base, na.action = na.omit)
m4_iii <- lm(
  DEMP ~ GAP,
  data = base, na.action = na.omit)
m4_iv <- lm(
  DEMP ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = base, na.action = na.omit)
m4_v <- lm(
  DEMP ~ GAP + BK + KFC + ROYS + CO_OWNED +
    CENTRALJ + SOUTHJ + PA1 + PA2,
  data = base, na.action = na.omit)

s4_i <- summary(m4_i)
s4_ii <- summary(m4_ii)
s4_iii <- summary(m4_iii)
s4_iv <- summary(m4_iv)
s4_v <- summary(m4_v)

table4 <- data.frame(
  model = c("i", "ii", "iii", "iv", "v"),
  term = c("NJ", "NJ", "GAP", "GAP", "GAP"),
  estimate = c(
    s4_i$coefficients["NJ", "Estimate"],
    s4_ii$coefficients["NJ", "Estimate"],
    s4_iii$coefficients["GAP", "Estimate"],
    s4_iv$coefficients["GAP", "Estimate"],
    s4_v$coefficients["GAP", "Estimate"]),
  std_error = c(
    s4_i$coefficients["NJ", "Std. Error"],
    s4_ii$coefficients["NJ", "Std. Error"],
    s4_iii$coefficients["GAP", "Std. Error"],
    s4_iv$coefficients["GAP", "Std. Error"],
    s4_v$coefficients["GAP", "Std. Error"]),
  chain_ownership_controls = c(FALSE, TRUE, FALSE, TRUE, TRUE),
  region_controls = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  regression_SE = c(
    s4_i$sigma, s4_ii$sigma, s4_iii$sigma,
    s4_iv$sigma, s4_v$sigma),
  joint_p_controls = c(
    NA,
    joint_p(m4_i, m4_ii),
    NA,
    joint_p(m4_iii, m4_iv),
    joint_p(m4_iii, m4_v)),
  n = c(
    nrow(model.frame(m4_i)), nrow(model.frame(m4_ii)),
    nrow(model.frame(m4_iii)), nrow(model.frame(m4_iv)),
    nrow(model.frame(m4_v))))
write_out(table4, "table4.csv")

# The printed model v coefficient follows the supplied SAS program, which
# omits ownership despite the published table note saying it is included.
m4_v_sas <- lm(
  DEMP ~ GAP + BK + KFC + ROYS + CENTRALJ + SOUTHJ + PA1 + PA2,
  data = base, na.action = na.omit)
s4_v_sas <- summary(m4_v_sas)
table4_model5_check <- data.frame(
  variant = c("published table note", "supplied check.sas"),
  formula = c(
    "GAP + chain + ownership + region",
    "GAP + chain + region (ownership omitted)"),
  estimate = c(
    s4_v$coefficients["GAP", "Estimate"],
    s4_v_sas$coefficients["GAP", "Estimate"]),
  std_error = c(
    s4_v$coefficients["GAP", "Std. Error"],
    s4_v_sas$coefficients["GAP", "Std. Error"]),
  regression_SE = c(s4_v$sigma, s4_v_sas$sigma),
  joint_p_all_controls = c(
    joint_p(m4_iii, m4_v),
    joint_p(m4_iii, m4_v_sas)),
  n = c(nrow(model.frame(m4_v)), nrow(model.frame(m4_v_sas))))
write_out(table4_model5_check, "table4_model5_check.csv")

# 8. Table 5: specification tests

four_table5_models <- function(index, change, proportional,
                               interview_controls = FALSE) {
  model_data <- d[index, , drop = FALSE]
  model_data$Y_CHANGE <- change[index]
  model_data$Y_PROP <- proportional[index]

  if (interview_controls) {
    change_nj <- lm(
      Y_CHANGE ~ NJ + BK + KFC + ROYS + CO_OWNED + WEEK1 + WEEK2 + WEEK3,
      data = model_data, na.action = na.omit)
    change_gap <- lm(
      Y_CHANGE ~ GAP + BK + KFC + ROYS + CO_OWNED + WEEK1 + WEEK2 + WEEK3,
      data = model_data, na.action = na.omit)
    prop_nj <- lm(
      Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED + WEEK1 + WEEK2 + WEEK3,
      data = model_data, na.action = na.omit)
    prop_gap <- lm(
      Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED + WEEK1 + WEEK2 + WEEK3,
      data = model_data, na.action = na.omit) } else {
    change_nj <- lm(
      Y_CHANGE ~ NJ + BK + KFC + ROYS + CO_OWNED,
      data = model_data, na.action = na.omit)
    change_gap <- lm(
      Y_CHANGE ~ GAP + BK + KFC + ROYS + CO_OWNED,
      data = model_data, na.action = na.omit)
    prop_nj <- lm(
      Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED,
      data = model_data, na.action = na.omit)
    prop_gap <- lm(
      Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED,
      data = model_data, na.action = na.omit) }

  change_nj_summary <- summary(change_nj)
  change_gap_summary <- summary(change_gap)
  prop_nj_summary <- summary(prop_nj)
  prop_gap_summary <- summary(prop_gap)

  c(
    change_NJ = change_nj_summary$coefficients["NJ", "Estimate"],
    change_NJ_se = change_nj_summary$coefficients["NJ", "Std. Error"],
    change_GAP = change_gap_summary$coefficients["GAP", "Estimate"],
    change_GAP_se = change_gap_summary$coefficients["GAP", "Std. Error"],
    proportional_NJ = prop_nj_summary$coefficients["NJ", "Estimate"],
    proportional_NJ_se = prop_nj_summary$coefficients["NJ", "Std. Error"],
    proportional_GAP = prop_gap_summary$coefficients["GAP", "Estimate"],
    proportional_GAP_se = prop_gap_summary$coefficients["GAP", "Std. Error"],
    n = nrow(model.frame(change_nj))) }

# Specification 1: base specification.
base_proportional <- 2 * d$DEMP / (d$FTE2 + d$FTE1)
base_proportional[!is.na(d$FTE2) & d$FTE2 == 0] <- -1
spec1 <- four_table5_models(
  index = which(base_keep),
  change = d$DEMP,
  proportional = base_proportional)

# Specification 2: treat temporary closures as permanently closed.
temp_proportional <- 2 * demp_temp_zero / (fte2_temp_zero + d$FTE1)
temp_proportional[!is.na(fte2_temp_zero) & fte2_temp_zero == 0] <- -1
temp_keep <- !is.na(demp_temp_zero) &
  (d$STATUS2 %in% c(2, 3, 4, 5) |
     (!(d$STATUS2 %in% c(2, 3, 4, 5)) & !is.na(d$DWAGE)))
spec2 <- four_table5_models(
  index = which(temp_keep),
  change = demp_temp_zero,
  proportional = temp_proportional)

# The printed Table 5 results for specifications 3 to 5 retain the raw symmetric
# proportional change of negative 2 for closed stores. Footnote 18 instead says that
# closed stores are assigned negative 1. I think this is a mistype

# Specification 3: exclude managers from employment.
no_manager_fte1 <- d$EMPFT + 0.5 * d$EMPPT
no_manager_fte2 <- d$EMPFT2 + 0.5 * d$EMPPT2
no_manager_change <- no_manager_fte2 - no_manager_fte1
no_manager_proportional <- 2 * no_manager_change / (no_manager_fte2 + no_manager_fte1)
spec3 <- four_table5_models(
  index = which(base_keep),
  change = no_manager_change,
  proportional = no_manager_proportional)

no_manager_paper_proportional <- no_manager_proportional
no_manager_paper_proportional[!is.na(no_manager_fte2) & no_manager_fte2 == 0] <- -1
spec3_paper_data <- d[base_keep, , drop = FALSE]
spec3_paper_data$Y_PROP <- no_manager_paper_proportional[base_keep]
spec3_paper_nj <- lm(
  Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = spec3_paper_data, na.action = na.omit)
spec3_paper_gap <- lm(
  Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = spec3_paper_data, na.action = na.omit)
spec3_paper_nj_summary <- summary(spec3_paper_nj)
spec3_paper_gap_summary <- summary(spec3_paper_gap)
spec3_paper <- c(
  proportional_NJ = spec3_paper_nj_summary$coefficients["NJ", "Estimate"],
  proportional_NJ_se = spec3_paper_nj_summary$coefficients["NJ", "Std. Error"],
  proportional_GAP = spec3_paper_gap_summary$coefficients["GAP", "Estimate"],
  proportional_GAP_se = spec3_paper_gap_summary$coefficients["GAP", "Std. Error"],
  n = unname(spec3["n"]))

# Specification 4: count each part time worker as 0.4 full time workers.
weight04_fte1 <- d$EMPFT + d$NMGRS + 0.4 * d$EMPPT
weight04_fte2 <- d$EMPFT2 + d$NMGRS2 + 0.4 * d$EMPPT2
weight04_change <- weight04_fte2 - weight04_fte1
weight04_proportional <- 2 * weight04_change / (weight04_fte2 + weight04_fte1)
spec4 <- four_table5_models(
  index = which(base_keep),
  change = weight04_change,
  proportional = weight04_proportional)

weight04_paper_proportional <- weight04_proportional
weight04_paper_proportional[!is.na(weight04_fte2) & weight04_fte2 == 0] <- -1
spec4_paper_data <- d[base_keep, , drop = FALSE]
spec4_paper_data$Y_PROP <- weight04_paper_proportional[base_keep]
spec4_paper_nj <- lm(
  Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = spec4_paper_data, na.action = na.omit)
spec4_paper_gap <- lm(
  Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = spec4_paper_data, na.action = na.omit)
spec4_paper_nj_summary <- summary(spec4_paper_nj)
spec4_paper_gap_summary <- summary(spec4_paper_gap)
spec4_paper <- c(
  proportional_NJ = spec4_paper_nj_summary$coefficients["NJ", "Estimate"],
  proportional_NJ_se = spec4_paper_nj_summary$coefficients["NJ", "Std. Error"],
  proportional_GAP = spec4_paper_gap_summary$coefficients["GAP", "Estimate"],
  proportional_GAP_se = spec4_paper_gap_summary$coefficients["GAP", "Std. Error"],
  n = unname(spec4["n"]))

# Specification 5: count each part time worker as 0.6 full time workers.
weight06_fte1 <- d$EMPFT + d$NMGRS + 0.6 * d$EMPPT
weight06_fte2 <- d$EMPFT2 + d$NMGRS2 + 0.6 * d$EMPPT2
weight06_change <- weight06_fte2 - weight06_fte1
weight06_proportional <- 2 * weight06_change / (weight06_fte2 + weight06_fte1)
spec5 <- four_table5_models(
  index = which(base_keep),
  change = weight06_change,
  proportional = weight06_proportional)

weight06_paper_proportional <- weight06_proportional
weight06_paper_proportional[!is.na(weight06_fte2) & weight06_fte2 == 0] <- -1
spec5_paper_data <- d[base_keep, , drop = FALSE]
spec5_paper_data$Y_PROP <- weight06_paper_proportional[base_keep]
spec5_paper_nj <- lm(
  Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = spec5_paper_data, na.action = na.omit)
spec5_paper_gap <- lm(
  Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = spec5_paper_data, na.action = na.omit)
spec5_paper_nj_summary <- summary(spec5_paper_nj)
spec5_paper_gap_summary <- summary(spec5_paper_gap)
spec5_paper <- c(
  proportional_NJ = spec5_paper_nj_summary$coefficients["NJ", "Estimate"],
  proportional_NJ_se = spec5_paper_nj_summary$coefficients["NJ", "Std. Error"],
  proportional_GAP = spec5_paper_gap_summary$coefficients["GAP", "Estimate"],
  proportional_GAP_se = spec5_paper_gap_summary$coefficients["GAP", "Std. Error"],
  n = unname(spec5["n"]))

# Specification 6: exclude stores in the NJ shore area.
spec6 <- four_table5_models(
  index = which(base_keep & d$SHORE != 1),
  change = d$DEMP,
  proportional = base_proportional)

# Specification 7: control for the second interview period.
spec7 <- four_table5_models(
  index = which(base_keep),
  change = d$DEMP,
  proportional = base_proportional,
  interview_controls = TRUE)

# Specification 8: exclude stores called more than twice in wave 1.
spec8 <- four_table5_models(
  index = which(base_keep & d$NCALLS <= 2),
  change = d$DEMP,
  proportional = base_proportional)

# Specification 9: weight proportional change by initial employment.
weighted <- d[base_keep, , drop = FALSE]
weighted$Y_PROP <- base_proportional[base_keep]
spec9_nj <- lm(
  Y_PROP ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = weighted, weights = FTE1, na.action = na.omit)
spec9_nj_summary <- summary(spec9_nj)
spec9_gap <- lm(
  Y_PROP ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = weighted, weights = FTE1, na.action = na.omit)
spec9_gap_summary <- summary(spec9_gap)

# Specification 12: Pennsylvania stores only.
pa_keep <- base_keep & d$NJ == 0
pa <- d[pa_keep, , drop = FALSE]
pa$Y_CHANGE <- d$DEMP[pa_keep]
pa$Y_PROP <- base_proportional[pa_keep]
pa$PA_GAP <- gap_to_minimum[pa_keep]
spec12_change <- lm(
  Y_CHANGE ~ PA_GAP + BK + KFC + ROYS + CO_OWNED,
  data = pa, na.action = na.omit)
spec12_change_summary <- summary(spec12_change)
spec12_prop <- lm(
  Y_PROP ~ PA_GAP + BK + KFC + ROYS + CO_OWNED,
  data = pa, na.action = na.omit)
spec12_prop_summary <- summary(spec12_prop)

table5_row <- function(number, description, values, note = "") {
  data.frame(
    specification = number,
    description = description,
    change_NJ = unname(values["change_NJ"]),
    change_NJ_se = unname(values["change_NJ_se"]),
    change_GAP = unname(values["change_GAP"]),
    change_GAP_se = unname(values["change_GAP_se"]),
    proportional_NJ = unname(values["proportional_NJ"]),
    proportional_NJ_se = unname(values["proportional_NJ_se"]),
    proportional_GAP = unname(values["proportional_GAP"]),
    proportional_GAP_se = unname(values["proportional_GAP_se"]),
    n = unname(values["n"]),
    note = note) }

table5 <- rbind(
  table5_row(1, "Base specification", spec1),
  table5_row(
    2, "Treat four temporarily closed stores as permanently closed", spec2,
    "released data produce 2.03 rather than printed 2.20 for the NJ coefficient"),
  table5_row(
    3, "Exclude managers from employment count", spec3,
    "printed-table reproduction uses -2 for closed stores; paper-stated -1 results exported separately"),
  table5_row(
    4, "Weight part-time as 0.4 x full-time", spec4,
    "printed-table reproduction uses -2 for closed stores; paper-stated -1 results exported separately"),
  table5_row(
    5, "Weight part-time as 0.6 x full-time", spec5,
    "printed-table reproduction uses -2 for closed stores; paper-stated -1 results exported separately"),
  table5_row(6, "Exclude stores in NJ shore area", spec6),
  table5_row(
    7, "Add controls for wave-2 interview period", spec7,
    "period bins: Nov 5-11, Nov 12-19, Dec 9-15, Dec 16-31; one 1991 date typo corrected to 1992"),
  table5_row(8, "Exclude stores called more than twice in wave 1", spec8),
  data.frame(
    specification = 9,
    description = "Weight proportional-change model by initial employment",
    change_NJ = NA, change_NJ_se = NA, change_GAP = NA, change_GAP_se = NA,
    proportional_NJ = spec9_nj_summary$coefficients["NJ", "Estimate"],
    proportional_NJ_se = spec9_nj_summary$coefficients["NJ", "Std. Error"],
    proportional_GAP = spec9_gap_summary$coefficients["GAP", "Estimate"],
    proportional_GAP_se = spec9_gap_summary$coefficients["GAP", "Std. Error"],
    n = nrow(model.frame(spec9_nj)), note = ""),
  data.frame(
    specification = 10,
    description = "Stores in towns around Newark",
    change_NJ = NA, change_NJ_se = NA, change_GAP = NA, change_GAP_se = NA,
    proportional_NJ = NA, proportional_NJ_se = NA,
    proportional_GAP = NA, proportional_GAP_se = NA, n = NA,
    note = "not reproducible: three-digit ZIP codes used for this subsample are absent from public.dat"),
  data.frame(
    specification = 11,
    description = "Stores in towns around Camden",
    change_NJ = NA, change_NJ_se = NA, change_GAP = NA, change_GAP_se = NA,
    proportional_NJ = NA, proportional_NJ_se = NA,
    proportional_GAP = NA, proportional_GAP_se = NA, n = NA,
    note = "not reproducible: three-digit ZIP codes used for this subsample are absent from public.dat"),
  data.frame(
    specification = 12,
    description = "Pennsylvania stores only",
    change_NJ = NA, change_NJ_se = NA,
    change_GAP = spec12_change_summary$coefficients["PA_GAP", "Estimate"],
    change_GAP_se = spec12_change_summary$coefficients["PA_GAP", "Std. Error"],
    proportional_NJ = NA, proportional_NJ_se = NA,
    proportional_GAP = spec12_prop_summary$coefficients["PA_GAP", "Estimate"],
    proportional_GAP_se = spec12_prop_summary$coefficients["PA_GAP", "Std. Error"],
    n = nrow(model.frame(spec12_change)),
    note = "PA wage gap redefined as increase required to reach $5.05"))
write_out(table5, "table5.csv")

table5_convention_row <- function(number, description, convention,
                                  closed_store_value, values, note) {
  data.frame(
    specification = number,
    description = description,
    convention = convention,
    closed_store_proportional_change = closed_store_value,
    proportional_NJ = unname(values["proportional_NJ"]),
    proportional_NJ_se = unname(values["proportional_NJ_se"]),
    proportional_GAP = unname(values["proportional_GAP"]),
    proportional_GAP_se = unname(values["proportional_GAP_se"]),
    n = unname(values["n"]),
    note = note) }

table5_closed_store_convention_check <- rbind(
  table5_convention_row(
    3, "Exclude managers from employment count",
    "printed-table reproduction", -2, spec3,
    "raw symmetric proportional-change formula; matches printed Table 5"),
  table5_convention_row(
    3, "Exclude managers from employment count",
    "paper-stated method", -1, spec3_paper,
    "closed stores assigned -1 as stated in footnote 18"),
  table5_convention_row(
    4, "Weight part-time as 0.4 x full-time",
    "printed-table reproduction", -2, spec4,
    "raw symmetric proportional-change formula; matches printed Table 5"),
  table5_convention_row(
    4, "Weight part-time as 0.4 x full-time",
    "paper-stated method", -1, spec4_paper,
    "closed stores assigned -1 as stated in footnote 18"),
  table5_convention_row(
    5, "Weight part-time as 0.6 x full-time",
    "printed-table reproduction", -2, spec5,
    "raw symmetric proportional-change formula; matches printed Table 5"),
  table5_convention_row(
    5, "Weight part-time as 0.6 x full-time",
    "paper-stated method", -1, spec5_paper,
    "closed stores assigned -1 as stated in footnote 18"))
write_out(
  table5_closed_store_convention_check,
  "table5_closed_store_convention_check.csv")

# 9. Table 6: other outcomes

table6_spec <- data.frame(
  outcome = c(
    "Fraction full-time workers (percentage)",
    "Number of hours open per weekday",
    "Number of cash registers",
    "Number of cash registers open at 11:00 A.M.",
    "Low-price meal program (percentage)",
    "Free meal program (percentage)",
    "Combination of low-price and free meals (percentage)",
    "Time to first raise (weeks)",
    "Usual amount of first raise (dollars)",
    "Slope of wage profile (percent per week)"),
  source = c(
    "DFRACFT", "DHRSOPEN", "DNREGS", "DNREGS11",
    "DLOWPRICE", "DFREEMEAL", "DCOMBO",
    "DINCTIME", "DFIRSTINC", "DSLOPE"))

table6 <- data.frame()
for (i in seq_len(nrow(table6_spec))) {
  outcome <- table6_spec$source[i]
  values <- d[[outcome]]
  x_nj <- values[d$NJ == 1]
  x_pa <- values[d$NJ == 0]
  nj <- mean_se(x_nj)
  pa <- mean_se(x_pa)

  model_data <- d
  model_data$Y_CHANGE <- values
  nj_model <- lm(
    Y_CHANGE ~ NJ + BK + KFC + ROYS + CO_OWNED,
    data = model_data, na.action = na.omit)
  gap_model <- lm(
    Y_CHANGE ~ GAP + BK + KFC + ROYS + CO_OWNED,
    data = model_data, na.action = na.omit)
  gap_region_model <- lm(
    Y_CHANGE ~ GAP + BK + KFC + ROYS + CO_OWNED + CENTRALJ + SOUTHJ + PA1 + PA2,
    data = model_data, na.action = na.omit)
  nj_summary <- summary(nj_model)
  gap_summary <- summary(gap_model)
  gap_region_summary <- summary(gap_region_model)

  result_row <- data.frame(
    outcome = table6_spec$outcome[i],
    NJ_mean_change = nj["mean"], NJ_mean_se = nj["se"], NJ_n = nj["n"],
    PA_mean_change = pa["mean"], PA_mean_se = pa["se"], PA_n = pa["n"],
    NJ_minus_PA = nj["mean"] - pa["mean"],
    NJ_minus_PA_se = difference_se(nj["se"], pa["se"]),
    NJ_dummy = nj_summary$coefficients["NJ", "Estimate"],
    NJ_dummy_se = nj_summary$coefficients["NJ", "Std. Error"],
    NJ_reg_n = nrow(model.frame(nj_model)),
    GAP = gap_summary$coefficients["GAP", "Estimate"],
    GAP_se = gap_summary$coefficients["GAP", "Std. Error"],
    GAP_reg_n = nrow(model.frame(gap_model)),
    GAP_with_regions = gap_region_summary$coefficients["GAP", "Estimate"],
    GAP_with_regions_se = gap_region_summary$coefficients["GAP", "Std. Error"],
    GAP_region_n = nrow(model.frame(gap_region_model)))
  table6 <- rbind(table6, result_row) }
write_out(table6, "table6.csv")

# 10. Table 7: full meal price models

table7_sample <- d[base_keep & !is.na(d$DLOGMEAL), , drop = FALSE]

m7_i <- lm(
  DLOGMEAL ~ NJ,
  data = table7_sample, na.action = na.omit)
m7_ii <- lm(
  DLOGMEAL ~ NJ + BK + KFC + ROYS + CO_OWNED,
  data = table7_sample, na.action = na.omit)
m7_iii <- lm(
  DLOGMEAL ~ GAP,
  data = table7_sample, na.action = na.omit)
m7_iv <- lm(
  DLOGMEAL ~ GAP + BK + KFC + ROYS + CO_OWNED,
  data = table7_sample, na.action = na.omit)
m7_v <- lm(
  DLOGMEAL ~ GAP + BK + KFC + ROYS + CO_OWNED +
    CENTRALJ + SOUTHJ + PA1 + PA2,
  data = table7_sample, na.action = na.omit)

s7_i <- summary(m7_i)
s7_ii <- summary(m7_ii)
s7_iii <- summary(m7_iii)
s7_iv <- summary(m7_iv)
s7_v <- summary(m7_v)

table7 <- data.frame(
  model = c("i", "ii", "iii", "iv", "v"),
  term = c("NJ", "NJ", "GAP", "GAP", "GAP"),
  estimate = c(
    s7_i$coefficients["NJ", "Estimate"],
    s7_ii$coefficients["NJ", "Estimate"],
    s7_iii$coefficients["GAP", "Estimate"],
    s7_iv$coefficients["GAP", "Estimate"],
    s7_v$coefficients["GAP", "Estimate"]),
  std_error = c(
    s7_i$coefficients["NJ", "Std. Error"],
    s7_ii$coefficients["NJ", "Std. Error"],
    s7_iii$coefficients["GAP", "Std. Error"],
    s7_iv$coefficients["GAP", "Std. Error"],
    s7_v$coefficients["GAP", "Std. Error"]),
  chain_ownership_controls = c(FALSE, TRUE, FALSE, TRUE, TRUE),
  region_controls = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  regression_SE = c(
    s7_i$sigma, s7_ii$sigma, s7_iii$sigma,
    s7_iv$sigma, s7_v$sigma),
  n = c(
    nrow(model.frame(m7_i)), nrow(model.frame(m7_ii)),
    nrow(model.frame(m7_iii)), nrow(model.frame(m7_iv)),
    nrow(model.frame(m7_v))))
write_out(table7, "table7.csv")

table7_summary <- data.frame(
  n = nrow(table7_sample),
  dependent_mean = mean(table7_sample$DLOGMEAL),
  dependent_sd = sd(table7_sample$DLOGMEAL),
  note = "released public.dat yields 317 observations; paper reports 315")
write_out(table7_summary, "table7_summary.csv")

# 11. One simple addition: sensitivity to the part time FTE weight

weights_grid <- seq(0, 1, by = 0.02)
sensitivity <- data.frame()
for (w in weights_grid) {
  fte1 <- d$EMPFT + d$NMGRS + w * d$EMPPT
  fte2 <- d$EMPFT2 + d$NMGRS2 + w * d$EMPPT2
  employment_change <- fte2 - fte1

  model_data <- d[base_keep, , drop = FALSE]
  model_data$Y_CHANGE <- employment_change[base_keep]
  model <- lm(
    Y_CHANGE ~ NJ + BK + KFC + ROYS + CO_OWNED,
    data = model_data, na.action = na.omit)
  model_summary <- summary(model)
  result_row <- data.frame(
    part_time_weight = w,
    NJ_coefficient = model_summary$coefficients["NJ", "Estimate"],
    std_error = model_summary$coefficients["NJ", "Std. Error"],
    n = nrow(model.frame(model)))
  sensitivity <- rbind(sensitivity, result_row) }
write_out(sensitivity, "part_time_weight_sensitivity.csv")

png(
  file.path("output", "part_time_weight_sensitivity.png"),
  width = 1000, height = 650, res = 120)
plot(
  sensitivity$part_time_weight,
  sensitivity$NJ_coefficient,
  type = "l", lwd = 2,
  xlab = "Weight assigned to one part-time worker",
  ylab = "Estimated New Jersey coefficient (FTE workers)",
  main = "Sensitivity of the employment estimate to the FTE convention")
abline(v = 0.5, lty = 2)
points(0.5, sensitivity$NJ_coefficient[which.min(abs(sensitivity$part_time_weight - 0.5))], pch = 19)
dev.off()

# 12. Compare every published cell encoded in data/published_targets.csv

targets_file <- file.path("data", "published_targets.csv")
if (file.exists(targets_file)) {
  targets <- read.csv(targets_file, check.names = FALSE)
  targets$reproduced <- NA_real_
  for (i in seq_len(nrow(targets))) {
    file <- targets$file[i]
    row <- as.integer(targets$row[i])
    column <- targets$column[i]
    results <- read.csv(
      file.path("output", file),
      check.names = FALSE,
      na.strings = c("", "NA"))
    value <- results[row, column]
    targets$reproduced[i] <- as.numeric(value) }
  targets$difference <- targets$reproduced - targets$published
  tolerance <- 0.5000001 * 10^(-targets$digits)
  targets$status <- ifelse(
    is.na(targets$reproduced), "not_reproducible",
    ifelse(abs(targets$difference) <= tolerance, "match", "differs"))
  write_out(targets, "validation.csv")

  summary_rows <- aggregate(
    list(cells = targets$status),
    list(table = targets$table, status = targets$status),
    length)
  write_out(summary_rows, "validation_summary.csv") }

message("YAY, all done. Results are in output/.")
