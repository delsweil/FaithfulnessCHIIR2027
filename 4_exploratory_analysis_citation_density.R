# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# Exploratory Analysis: Citation Density
# ==============================================================================
#
# Citation count is measured at a RESPONSE-level (i.e. how many citations did this one
# generated response contain?), unlike faithfulness, which is a CITATION-level
# quantity. This script therefore aggregates df_recoded.rds up to one row per
# TRUE response (query x model_condition x run_no) before analysing.
#
# An IMPORTANT LIMITATION is that a response with a citation count of
# zero would produce ZERO rows in the source data, which is indistinguishable
# from a response that was never generated at all (e.g. a pipeline failure,
# as with llama-4-scout_ddg run 1 -- see reanalysis_steps1-3.R). This script
# can therefore only describe citation density among responses that actually
# produced at least one extracted citation; true zero-citation responses, if
# any exist, cannot be identified from this dataset and are not represented.
# We need to REMEMBER to include this as a limitation in the paper.
#

# ==============================================================================

library(dplyr)
library(tidyr)
# install.packages("glmmTMB")  # uncomment if needed
library(glmmTMB)

df <- readRDS("df_recoded.rds")

## ---- Chunk 1: Aggregate to true response level ---------------------------------

resp <- df %>%
  group_by(response_id, memory_axis, task_axis, domain, model_condition, query_id) %>%
  summarise(n_citations = n(), .groups = "drop")

cat("True responses with >=1 extracted citation:", nrow(resp), "\n")
cat("(of a possible 1440 = 80 queries x 6 conditions x 3 runs")


## ---- Chunk 2: Descriptive stats by model_condition (recomputed, TRUE responses) -


cat("=== Citation density by model_condition (TRUE response counts) ===\n")
resp %>%
  group_by(model_condition) %>%
  summarise(n_responses = n(), mean = round(mean(n_citations), 2),
            sd = round(sd(n_citations), 2), median = median(n_citations),
            min = min(n_citations), max = max(n_citations)) %>%
  as.data.frame() %>% print()

## ---- Chunk 3: Descriptive stats by quadrant (new -- ties back to Introduction) --
# This is the more theoretically interesting breakdown: does citation COUNT
# track faithfulness (fewer, better citations under Interpretive) or move in
# the opposite direction (more citations despite lower faithfulness, which
# would echo the paratextual-citation motivation in the Introduction -- citation
# density as a substitute for, rather than a signal of, genuine grounding)?

cat("=== Citation density by quadrant ===\n")
resp %>%
  group_by(memory_axis, task_axis) %>%
  summarise(n_responses = n(), mean = round(mean(n_citations), 2),
            sd = round(sd(n_citations), 2), median = median(n_citations),
            .groups = "drop") %>%
  as.data.frame() %>% print()

## ---- Chunk 4: Formal model -------------------------------------------------------
# Same fixed-effects structure as the faithfulness model, but at the RESPONSE
# level, so only ONE random effect (Query) is needed 
#
# I tried a negative binomial GLMM first (as pre-registered). It converged with a "false
# convergence" warning, and its estimated dispersion parameter was large
# (~1.3e7). 
# I then switched to a Poisson model: it converges cleanly with no warnings, gives
# essentially identical coefficients to the (unstable) negative binomial fit,
# has a marginally better AIC, and a post-hoc Pearson dispersion ratio of
# 0.808 (mild UNDERdispersion, not over-), confirming Poisson is the
# appropriate choice.

cat("Fitting Poisson GLMM: n_citations ~ memory_axis * task_axis + domain +\n")
cat("  model_condition + (1 | query_id)\n\n")

count_fit <- glmmTMB(
  n_citations ~ memory_axis * task_axis + domain + model_condition + (1 | query_id),
  data = resp, family = poisson()
)

print(summary(count_fit))

pearson_resid <- residuals(count_fit, type = "pearson")
disp_ratio <- sum(pearson_resid^2) / df.residual(count_fit)
cat("\nPost-hoc Pearson dispersion ratio (should be near 1.0):", round(disp_ratio, 3), "\n")

saveRDS(count_fit, "count_fit.rds")
cat("\nSaved model to count_fit.rds\n\n")

cat("=== Quick read ===\n")
cat("Positive coefficients = MORE citations; recall the faithfulness model found\n")
cat("Interpretive queries have LOWER faithfulness (H2). If task_axis is positive\n")
cat("and significant here, that means Interpretive responses contain MORE\n")
cat("citations despite being less faithful -- consistent with citations serving\n")
cat("a paratextual/quantity-signalling role rather than tracking genuine grounding.\n")
cat("If it is negative or null, citation count does not compensate this way.\n")

write.csv(resp, "response_level_citation_counts.csv", row.names = FALSE)
cat("\nSaved response_level_citation_counts.csv\n")

# A late addition. I felt the paper needed some more basic descriptive stats for citation density

cat("=== Overall citation density (all responses) ===\n")
resp %>%
  summarise(n = n(), mean = round(mean(n_citations), 2), sd = round(sd(n_citations), 2),
            median = median(n_citations), min = min(n_citations), max = max(n_citations)) %>%
  print()

qr <- read.csv("full_data_analysis/all_query_results_unified.csv", stringsAsFactors = FALSE)
judg <- read.csv("full_data_analysis/all_judgements_complete.csv", stringsAsFactors = FALSE)

qr$true_key <- paste(qr$query, qr$model_condition, qr$run_no, sep = "_")
judg_valid <- judg %>% filter(judgement != "Error")
judg_valid$true_key <- paste(judg_valid$query, judg_valid$model_condition, judg_valid$run_no, sep = "_")

responses_with_citations <- unique(judg_valid$true_key)

cat("Total generated responses:", nrow(qr), "\n")
cat("Responses with >=1 valid (non-Error) citation:", length(responses_with_citations), "\n")
cat("Responses with ZERO valid citations:", nrow(qr) - length(responses_with_citations), "\n\n")

zero_cite <- qr %>% filter(!true_key %in% responses_with_citations)
zero_cite$n_urls <- sapply(strsplit(as.character(zero_cite$cited_urls), "\\|"), function(x) sum(nchar(x) > 0))

cat("Of these zero-citation responses:\n")
cat("  Empty cited_urls (model cited nothing at all):", sum(zero_cite$n_urls == 0), "\n")
cat("  Non-empty cited_urls, but 0 valid citations survived:", sum(zero_cite$n_urls > 0), "\n\n")

cat("=== Zero-citation responses by model_condition ===\n")
print(table(zero_cite$model_condition))
cat("\n=== Zero-citation responses by quadrant ===\n")
print(table(zero_cite$quadrant))
