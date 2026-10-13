# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# Domain Comparison: All Pairwise Contrasts
# ==============================================================================
#  Does the Off-Topic/Decorative ("poor") rate persist
#         across models, regardless of size?
#
# Motivation: We found that 27.6% of all citations
# are Off-Topic/Irrelevant or Decorative (i.e. not "appropriate").
# This script examines whether that rate is concentrated in one
# model (e.g. the smallest) or holds across all three tested
# models, and gives the full 4-category breakdown by model for
# reference.
#
# ============================================================

library(dplyr)
library(tidyr)

df <- readRDS("df_recoded.rds")

cat("N:", nrow(df), "\n")
stopifnot(nrow(df) == 6900)  # sanity check against the confirmatory model's N

# --- Overall poor rate (sanity check against the 27.6% figure) -
overall_poor <- mean(df$poor)
cat(sprintf("Overall poor rate: %.1f%%\n", 100 * overall_poor))

# --- Define "poor" = Off-Topic/Irrelevant or Decorative -------
# Note: judgement may be an ordered factor at this point (from
# Step 2's releveling for clmm) rather than a plain character
# column. %in% works the same either way, but if this errors,
# try as.character(df$judgement) %in% c(...) instead.
df <- df %>%
  mutate(poor = judgement %in% c("Off-Topic / Irrelevant", "Decorative"))

# --- Poor rate by model ---------------------------------------
by_model <- df %>%
  group_by(llm_model) %>%
  summarise(
    n = n(),
    poor_rate = mean(poor),
    .groups = "drop"
  ) %>%
  mutate(poor_pct = round(100 * poor_rate, 1))

cat("\nPoor rate by model:\n")
print(by_model)

# --- Poor rate by model_condition (model x retriever) ---------
by_condition <- df %>%
  group_by(model_condition) %>%
  summarise(
    n = n(),
    poor_rate = mean(poor),
    .groups = "drop"
  ) %>%
  mutate(poor_pct = round(100 * poor_rate, 1))

cat("\nPoor rate by model_condition (model x retriever):\n")
print(by_condition)

# --- Full 4-category breakdown by model (%) --------------------
category_breakdown <- df %>%
  count(llm_model, judgement) %>%
  group_by(llm_model) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = judgement, values_from = pct)

cat("\nFull faithfulness category breakdown by model (%):\n")
print(category_breakdown)

# --- Optional: chi-square test of independence -----------------
# Tests whether the (poor vs. not-poor) split differs by model.
# Note: this treats each citation as independent, which ignores
# the clustering (by query/response) that the main CLMM accounts
# for via random effects. Table 3's model_condition coefficients
# are the more rigorous test of the same underlying question;
# this is a simple descriptive/confirmatory cross-check only.
tab <- table(df$llm_model, df$poor)
print(tab)
print(chisq.test(tab))
