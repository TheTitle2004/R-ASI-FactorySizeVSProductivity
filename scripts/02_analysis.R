# ASI Term Paper — analysis, tables, regressions, and figures
# Run from the ASI_Term_Paper RStudio project root, after 01_build_dataset.R.
# This script reads clean_data/asi_all_records.csv and writes to outputs/.

library(data.table)
library(ggplot2)
library(fixest)

root <- getwd()
clean_file <- file.path(root, "clean_data", "asi_all_records.csv")
out_dir <- file.path(root, "outputs")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(clean_file)) stop("Cannot find ", clean_file, ". Run 01_build_dataset.R first.")

# ---------- 1. Load data and reproduce the sample filters ----------
all_records <- fread(clean_file)
required <- c("year", "status", "mult", "persons", "output", "gva", "capital", "machinery",
              "emoluments", "nic3", "nic4", "state")
missing_vars <- setdiff(required, names(all_records))
if (length(missing_vars)) stop("Missing required variables: ", paste(missing_vars, collapse = ", "))

# Print counts step by step; do not silently force the sample to match.
filter_counts <- all_records[, .(
  all_records = .N,
  open_status_1 = sum(status == 1 & !is.na(status)),
  persons_positive = sum(status == 1 & !is.na(persons) & persons >= 1),
  output_positive = sum(status == 1 & !is.na(persons) & persons >= 1 & !is.na(output) & output > 0),
  final_sample = sum(status == 1 & !is.na(mult) & mult > 0 &
                       !is.na(persons) & persons >= 1 &
                       !is.na(output) & output > 0 &
                       !is.na(capital) & capital > 0)
), by = year]
setorder(filter_counts, year)
print(filter_counts)
fwrite(filter_counts, file.path(out_dir, "sample_counts_analysis.csv"))

expected_final <- c("2021-22" = 54843L, "2022-23" = 55999L, "2023-24" = 57044L)
for (yr in names(expected_final)) {
  got <- filter_counts[year == yr, final_sample]
  if (length(got) != 1L || got != expected_final[[yr]]) {
    stop("Final-sample checkpoint failed for ", yr, ": expected ", expected_final[[yr]], ", got ", got)
  }
}

# Analysis sample: open, positive weight, persons engaged >= 1, output > 0, net capital > 0.
d <- copy(all_records[status == 1 & !is.na(mult) & mult > 0 &
                        !is.na(persons) & persons >= 1 &
                        !is.na(output) & output > 0 &
                        !is.na(capital) & capital > 0])

# ---------- 2. Construct ratios and analysis variables ----------
d[, `:=`(
  lp = output / persons,
  vap = gva / persons,
  ci = capital / persons,
  mi = machinery / persons,
  emol_per_person = emoluments / persons,
  year = factor(year, levels = c("2021-22", "2022-23", "2023-24")),
  state_fe = factor(state),
  nic3_fe = factor(nic3),
  nic4_fe = factor(nic4),
  size_class = factor(fifelse(persons < 50, "Small",
                      fifelse(persons < 200, "Medium", "Large")),
                      levels = c("Small", "Medium", "Large"))
)]

# Create logs only where the underlying value is valid. GVA can be zero/negative,
# and some factories report zero machinery; those observations are left NA for log models.
d[, `:=`(
  ln_lp = log(lp),
  ln_ci = log(ci),
  ln_L = log(persons),
  ln_output = log(output),
  ln_capital = log(capital),
  ln_vap = NA_real_,
  ln_mi = NA_real_
)]
d[!is.na(vap) & vap > 0, ln_vap := log(vap)]
d[!is.na(mi) & mi > 0, ln_mi := log(mi)]

# ---------- 3. Table 1: medians by year and size class ----------
table1 <- d[, .(
  N = .N,
  median_L = as.numeric(median(persons, na.rm = TRUE)),
  output_per_person_lakh = median(lp, na.rm = TRUE) / 1e5,
  gva_per_person_lakh = median(vap, na.rm = TRUE) / 1e5,
  capital_per_person_lakh = median(ci, na.rm = TRUE) / 1e5,
  machinery_per_person_lakh = median(mi, na.rm = TRUE) / 1e5,
  emoluments_per_person_lakh = median(emol_per_person, na.rm = TRUE) / 1e5
), by = .(year, size_class)]
setorder(table1, year, size_class)
fwrite(table1, file.path(out_dir, "table1_by_size_year.csv"))
print(table1)

# ---------- 4. Table 2: multiplier-weighted pooled aggregates by size ----------
weighted_sum <- function(x, w) sum(x * w, na.rm = TRUE)
weighted_by_size <- d[, .(
  weighted_workers = weighted_sum(persons, mult),
  weighted_output = weighted_sum(output, mult),
  weighted_gva = weighted_sum(gva, mult),
  weighted_capital = weighted_sum(capital, mult),
  weighted_machinery = weighted_sum(machinery, mult),
  weighted_emoluments = weighted_sum(emoluments, mult)
), by = size_class]
weighted_by_size[, `:=`(
  share_workforce_pct = 100 * weighted_workers / sum(weighted_workers),
  share_output_pct = 100 * weighted_output / sum(weighted_output),
  gva_per_person_lakh = weighted_gva / weighted_workers / 1e5,
  capital_per_person_lakh = weighted_capital / weighted_workers / 1e5,
  machinery_per_person_lakh = weighted_machinery / weighted_workers / 1e5,
  emoluments_per_person_lakh = weighted_emoluments / weighted_workers / 1e5,
  emoluments_share_gva_pct = 100 * weighted_emoluments / weighted_gva
)]
table2 <- weighted_by_size[, .(size_class, share_workforce_pct, share_output_pct,
                               gva_per_person_lakh, capital_per_person_lakh,
                               emoluments_per_person_lakh, emoluments_share_gva_pct)]
setorder(table2, size_class)
fwrite(table2, file.path(out_dir, "table2_pooled_by_size.csv"))
print(table2)

# ---------- 5. Figure 1–3: medians by size and year ----------
plot_median <- function(data, y, y_label, title, filename) {
  p <- ggplot(data, aes(x = size_class, y = .data[[y]], group = year, colour = year)) +
    geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
    labs(title = title, x = "Factory size class", y = y_label, colour = "Survey year") +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_dir, filename), p, width = 8, height = 5, dpi = 300)
}
plot_median(table1, "output_per_person_lakh", "Rs lakh per person", "Median output per person, by factory size and year", "figure1_median_output.png")
plot_median(table1, "gva_per_person_lakh", "Rs lakh per person", "Median value added per person, by factory size and year", "figure2_median_gva.png")
plot_median(table1, "capital_per_person_lakh", "Rs lakh per person", "Median net fixed capital per person, by factory size and year", "figure3_median_capital.png")

# ---------- 6. Figure 4: annual median and multiplier-weighted output per person ----------
fig4_median <- d[, .(median_output_lakh = median(lp, na.rm = TRUE) / 1e5), by = year]
fig4_weighted <- d[, .(weighted_output_lakh = weighted_sum(output, mult) /
                          weighted_sum(persons, mult) / 1e5), by = year]
fig4 <- merge(fig4_median, fig4_weighted, by = "year")
fig4_long <- melt(fig4, id.vars = "year", variable.name = "measure", value.name = "output_lakh")
fig4_long[, measure := fifelse(measure == "median_output_lakh", "Median factory", "Weighted aggregate")]
p4 <- ggplot(fig4_long, aes(x = year, y = output_lakh, group = measure, colour = measure)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
  labs(title = "Output per person by year", x = "Survey year", y = "Rs lakh per person (nominal)", colour = "Measure") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "figure4_output_by_year.png"), p4, width = 8, height = 5, dpi = 300)
fwrite(fig4, file.path(out_dir, "figure4_output_by_year.csv"))

# ---------- 7. Figure 5: output per person across capital-intensity deciles ----------
d[, ci_decile := pmin(10L, pmax(1L, ceiling(data.table::frank(ci, ties.method = "average") / .N * 10)))]
fig5 <- d[, .(
  factories = .N,
  median_capital_lakh = median(ci, na.rm = TRUE) / 1e5,
  median_output_lakh = median(lp, na.rm = TRUE) / 1e5,
  mean_output_lakh = mean(lp, na.rm = TRUE) / 1e5
), by = ci_decile]
setorder(fig5, ci_decile)
p5 <- ggplot(fig5, aes(x = ci_decile, y = median_output_lakh)) +
  geom_line(linewidth = 0.8) + geom_point(size = 2.5) +
  labs(title = "Output per person by capital-intensity decile", x = "Capital-intensity decile (1 = lowest)",
       y = "Median output per person (Rs lakh, nominal)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "figure5_output_by_capital_decile.png"), p5, width = 8, height = 5, dpi = 300)
fwrite(fig5, file.path(out_dir, "figure5_capital_deciles.csv"))

# ---------- 8. Main regressions: paper Table 3 ----------
vc <- ~nic4_fe
m1 <- feols(ln_lp ~ ln_L | year, data = d, vcov = vc)
m2 <- feols(ln_lp ~ ln_L + ln_ci | year, data = d, vcov = vc)
m3 <- feols(ln_lp ~ ln_L + ln_ci | year + nic3_fe, data = d, vcov = vc)
m4 <- feols(ln_lp ~ ln_L + ln_ci | year + nic3_fe + state_fe, data = d, vcov = vc)
m5 <- feols(ln_lp ~ ln_L + ln_ci | year + nic3_fe, data = d, weights = ~mult, vcov = vc)

d_mach <- d[!is.na(ln_mi) & mi > 0]
m6 <- feols(ln_lp ~ ln_L + ln_mi | year + nic3_fe, data = d_mach, vcov = vc)
m6b <- feols(ln_lp ~ ln_L + ln_ci + ln_mi | year + nic3_fe, data = d_mach, vcov = vc)

d_gva <- d[!is.na(ln_vap) & vap > 0]
m7 <- feols(ln_vap ~ ln_L + ln_ci | year + nic3_fe, data = d_gva, vcov = vc)

main_models <- list(
  "1 Size only" = m1,
  "2 + capital intensity" = m2,
  "3 Baseline + industry FE" = m3,
  "4 + state FE" = m4,
  "5 Multiplier weighted" = m5,
  "6 Machinery replaces capital" = m6,
  "6b Capital and machinery" = m6b,
  "7 Value added per person" = m7
)
main_table_text <- capture.output(etable(m1, m2, m3, m4, m5, m6, m6b, m7,
  headers = c("1 Size only", "2 + capital intensity", "3 Baseline + industry FE",
              "4 + state FE", "5 Weighted", "6 Machinery", "6b Both", "7 GVA outcome"),
  dict = c(ln_L = "Log persons engaged", ln_ci = "Log capital per person",
           ln_mi = "Log machinery per person"), tex = FALSE))
writeLines(main_table_text, file.path(out_dir, "table3_regressions.txt"))

# A tidy coefficient file makes it easy to check target coefficients and standard errors.
extract_model <- function(model, model_name) {
  ct <- coeftable(model)
  data.table(model = model_name, term = rownames(ct), estimate = ct[, 1], std_error = ct[, 2],
             statistic = ct[, 3], p_value = ct[, 4], observations = nobs(model))
}
coef_table <- rbindlist(Map(extract_model, main_models, names(main_models)), fill = TRUE)
fwrite(coef_table, file.path(out_dir, "table3_regression_coefficients.csv"))

# ---------- 9. Additional specifications: paper Table 5 ----------
m_sizeclass <- feols(ln_lp ~ size_class + ln_ci | year + nic3_fe, data = d, vcov = vc)
m_interaction <- feols(ln_lp ~ ln_L * ln_ci | year + nic3_fe, data = d, vcov = vc)
d[, ln_ci_c := ln_ci - mean(ln_ci, na.rm = TRUE)]
m_squared <- feols(ln_lp ~ ln_L + ln_ci_c + I(ln_ci_c^2) | year + nic3_fe, data = d, vcov = vc)
d[, ln_L_below200 := ln_L * as.integer(persons < 200)]
d[, ln_L_200plus := ln_L * as.integer(persons >= 200)]
d[, large200 := as.integer(persons >= 200)]

m_kink <- feols(
  ln_lp ~ ln_L_below200 + ln_L_200plus +
    large200 + ln_ci | year + nic3_fe,
  data = d,
  vcov = vc
)
m_levels <- feols(ln_output ~ ln_capital + ln_L | year + nic3_fe, data = d, vcov = vc)

# Ratios for the distributional test. Drop the upper 1% of each ratio as in the paper.
d_share <- d[gva > 0]
d_share[, emol_share_gva := emoluments / gva]
d_share <- d_share[emol_share_gva <= quantile(emol_share_gva, 0.99, na.rm = TRUE)]
m_emol_gva <- feols(emol_share_gva ~ size_class | year + nic3_fe, data = d_share, vcov = vc)

d_share_output <- copy(d)
d_share_output[, emol_share_output := emoluments / output]
d_share_output <- d_share_output[emol_share_output <= quantile(emol_share_output, 0.99, na.rm = TRUE)]
m_emol_output <- feols(emol_share_output ~ size_class | year + nic3_fe, data = d_share_output, vcov = vc)

extra_models <- list(
  "Size classes" = m_sizeclass,
  "Capital x size" = m_interaction,
  "Kink at 200 persons" = m_kink,
  "Levels: output on capital and labour" = m_levels,
  "Squared capital intensity" = m_squared,
  "Emoluments / GVA share" = m_emol_gva,
  "Emoluments / output share" = m_emol_output
)
extra_table_text <- capture.output(etable(m_sizeclass, m_interaction, m_kink, m_levels, m_squared,
  m_emol_gva, m_emol_output,
  headers = c("Size classes", "Capital x size", "Kink at 200", "Levels",
              "Squared capital", "Emoluments/GVA", "Emoluments/output"), tex = FALSE))
writeLines(extra_table_text, file.path(out_dir, "table5_additional_regressions.txt"))
extra_coef <- rbindlist(Map(extract_model, extra_models, names(extra_models)), fill = TRUE)
fwrite(extra_coef, file.path(out_dir, "table5_regression_coefficients.csv"))

# ---------- 10. Robustness by year and by size class ----------
by_year_models <- list()
for (yr in levels(d$year)) {
  dd <- d[year == yr]
  by_year_models[[yr]] <- feols(ln_lp ~ ln_L + ln_ci | nic3_fe, data = dd, vcov = vc)
}
by_year_coef <- rbindlist(Map(extract_model, by_year_models, names(by_year_models)), fill = TRUE)
fwrite(by_year_coef, file.path(out_dir, "robustness_by_year.csv"))

by_size_models <- list()
for (sc in levels(d$size_class)) {
  dd <- d[size_class == sc]
  by_size_models[[sc]] <- feols(ln_lp ~ ln_L + ln_ci | year + nic3_fe, data = dd, vcov = vc)
}
by_size_coef <- rbindlist(Map(extract_model, by_size_models, names(by_size_models)), fill = TRUE)
fwrite(by_size_coef, file.path(out_dir, "table4_splits.csv"))

# ---------- 11. Figure 6: size elasticity by size class ----------
fig6 <- by_size_coef[term == "ln_L", .(size_class = model, estimate, std_error)]
fig6[, `:=`(lower = estimate - 1.96 * std_error, upper = estimate + 1.96 * std_error)]
p6 <- ggplot(fig6, aes(x = size_class, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  labs(title = "Size elasticity of productivity within each size class", x = "Factory size class",
       y = "Coefficient on log persons engaged (95% CI)") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "figure6_size_elasticity.png"), p6, width = 8, height = 5, dpi = 300)
fwrite(fig6, file.path(out_dir, "figure6_size_elasticity.csv"))

# Save the selected analysis sample separately so the raw merged file remains intact.
fwrite(d, file.path(root, "clean_data", "asi_analysis_sample.csv"))

message("\nAnalysis script complete. Outputs saved in: ", out_dir)
message("Next, compare table1_by_size_year.csv with the paper's medians, then check the baseline rows in table3_regression_coefficients.csv.")
