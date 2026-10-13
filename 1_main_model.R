# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# ==============================================================================
#
# This first script has three steps. 
# Step 1: Run some sanity checks to validate the full-scale judgements dataset.
# Step 2: Wrangle the data to get it into a format appropriate for modelling. 
# Step 3: Fit the pre-registered CLMM 
#
# I will put the exploratory analyses in separate scripts to keep things tidy
# ==============================================================================

# install.packages(c("dplyr", "tidyr", "ordinal"))  # uncomment if needed
library(dplyr)
library(tidyr)
library(ordinal)

DATA_PATH <- "full_data_analysis/all_judgements_complete.csv"

## ---- Step 1: Load and structurally validate ---------------------------------

raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE)
cat("Total rows loaded:", nrow(raw), "\n\n")

cat("=== Unique queries (expect 80) ===\n")
cat(length(unique(raw$query)), "\n\n")

cat("=== Domain labels (check for typos) ===\n")
print(sort(unique(raw$domain)))
cat("\n")

cat("=== memory_axis / task_axis vs quadrant consistency (should be a clean diagonal) ===\n")
print(table(raw$quadrant, raw$memory_axis))
print(table(raw$quadrant, raw$task_axis))

cat("=== judgement categories (check for stray/unexpected values, e.g. 'Error') ===\n")
print(table(raw$judgement, useNA = "ifany"))

cat("=== status field (should be uniformly 'N/A (snippet)' given the snippet-only design) ===\n")
print(table(raw$status, useNA = "ifany"))

cat("=== rows per model_condition x run_no (checks generation completeness) ===\n")
raw %>% count(model_condition, run_no) %>%
  pivot_wider(names_from = run_no, values_from = n, values_fill = 0) %>%
  as.data.frame() %>% print()

cat("=== KEY CHECK: does response_id (as given) collide across model_condition? ===\n")
cat("Given response_id is built as query + run_no only (no model/retriever), it is\n")
cat("NOT expected to uniquely identify one generated response. This check confirms that.\n")
collision_check <- raw %>%
  group_by(response_id) %>%
  summarise(n_model_conditions = n_distinct(model_condition), .groups = "drop")
print(table(collision_check$n_model_conditions))
cat("(a value > 1 anywhere above means response_id is shared across multiple distinct\n")
cat(" LLM responses -- I will fix this in Step 2 below.)\n\n")

df <- raw %>%
  filter(judgement != "Error") %>%
  mutate(
    # Fix known domain-label typo
    domain = ifelse(domain == "Environment abd Climate Science",
                    "Environment and Climate Science", domain),
    # Build a  unique response identifier: query x model_condition x run_no.
    # The original `response_id` column omits model_condition, so the same
    # response_id string is shared across up to 6 different LLM
    # responses (one per model/retriever combination) -- see Step 1 check above.
    # This matters because the pre-registered random effect (1 | response_id)
    # is meant to capture correlation among citations WITHIN one response; using
    # the uncorrected column would incorrectly pool citations from several
    # unrelated responses under one random-effect level.
    response_id_true = paste(query, model_condition, run_no, sep = "_"),
    query_id     = factor(query),
    response_id  = factor(response_id_true),
    domain       = factor(domain),
    memory_axis  = factor(memory_axis, levels = c("In-memory", "Out-of-memory")),
    task_axis    = factor(task_axis,   levels = c("Factual", "Interpretive")),
    model_condition = factor(model_condition),
    judgement = factor(judgement,
                       levels = c("Off-Topic / Irrelevant", "Decorative",
                                  "Functional", "Fully-Supported"),
                       ordered = TRUE)
  )

cat("Post-recode: ", nrow(df), "rows, ",
    nlevels(df$query_id), "queries, ",
    nlevels(df$response_id), "TRUE unique responses ",
    "(expect up to 1440 = 80 queries x 6 conditions x 3 runs; fewer indicates\n",
    " missing generation cells -- see Step 1's model_condition x run_no table above)\n\n")

cat("Reference levels used in the model below:\n")
cat("  memory_axis:    ", levels(df$memory_axis)[1], "\n")
cat("  task_axis:      ", levels(df$task_axis)[1], "\n")
cat("  domain:         ", levels(df$domain)[1], "\n")
cat("  model_condition:", levels(df$model_condition)[1], "\n\n")

cat("=== Sanity check: judgement distribution by model_condition ===\n")
cat("(looking for any degenerate cell -- all citations in one category -- which\n")
cat(" would risk convergence problems in the model below)\n")
print(table(df$model_condition, df$judgement))
cat("looks good!")

# Save the recoded data
saveRDS(df, "df_recoded.rds")
cat("Saved recoded data to df_recoded.rds\n\n")

## ---- Step 3: Fit the pre-registered CLMM (primary, confirmatory analysis) -----
# NOTE: the interaction term (memory_axis * task_axis) is included even though
# the pre-registration's Model Specification prose lists Query Quadrant as a
# single fixed effect -- a main-effects-only model cannot produce the specific
# quadrant-level contrasts H3 and H4 require, an interaction term
# is needed to test them, which is equivalent to
# treating Quadrant as a 4-level factor.

cat("Fitting: judgement ~ memory_axis * task_axis + domain + model_condition +\n")
cat("           (1 | query_id) + (1 | response_id)\n")

fit <- clmm(
  judgement ~ memory_axis * task_axis + domain + model_condition +
    (1 | query_id) + (1 | response_id),
  data = df, link = "logit"
)

print(summary(fit))

# saving the model to rds so it can be picked up again in future scripts
saveRDS(fit, "clmm_fit.rds")
cat("\nSaved model fit to clmm_fit.rds\n")


## ---- Step 4: Quadrant Contrasts (H3 / H4)
#
# Here we calcuulate the four quadrant-level estimates and all six pairwise comparisons
# between them, with properly propagated standard errors from the fitted
# model's variance-covariance matrix (Wald tests, Holm-corrected for the 6
# simultaneous comparisons).
#
# This tests H3 ("In-memory-Factual is the highest of
# the four quadrants") and H4 ("Out-of-memory-Interpretive is the lowest of
# the four quadrants"). Summing POSTERIOR/POINT MEANS across coefficients
# (like we suggested in the pre-reg) gives  a point estimate for
# each quadrant but no valid uncertainty for the difference between two
# quadrants, since it ignores the covariance between the coefficients being
# combined. The contrasts below use the model's actual covariance structure,
# via L' %*% V %*% L for each linear combination L of the fixed effects.
#


fit <- readRDS("clmm_fit.rds")

## ---- Build the coefficient vector and matching covariance submatrix ----------
# vcov(fit) includes extra structural parameters (ST1, ST2) not in coef(fit);
# we subset to exactly the named coefficients so indices line up correctly.

beta_names <- names(coef(fit))
beta <- coef(fit)
V <- vcov(fit)[beta_names, beta_names]

i_mem  <- which(beta_names == "memory_axisOut-of-memory")
i_task <- which(beta_names == "task_axisInterpretive")
i_int  <- which(beta_names == "memory_axisOut-of-memory:task_axisInterpretive")

# A linear-combination "recipe" for each quadrant: which of the memory_axis,
# task_axis, and interaction coefficients contribute, and with what sign.
# Domain and model_condition coefficients are left at zero throughout: because
# this is an additive model with no domain/model x quadrant interaction, the
# RELATIVE differences between quadrants are the same regardless of which
# domain/model level you'd otherwise hold fixed, so this does not bias the
# contrasts. It just evaluates them at the reference domain/model level.

# A helper function that generates a  vector (L) by assigning specific weights to memory, task, and interaction effects.
# We will use the vector is to test specific linear combinations of coefficients.

make_L <- function(mem = 0, task = 0, inter = 0) {
  L <- rep(0, length(beta_names))
  L[i_mem] <- mem; L[i_task] <- task; L[i_int] <- inter
  L
}


quadrants <- list(
  "In-memory+Factual"          = make_L(0, 0, 0),  # reference quadrant
  "Out-of-memory+Factual"      = make_L(1, 0, 0),
  "In-memory+Interpretive"     = make_L(0, 1, 0),
  "Out-of-memory+Interpretive" = make_L(1, 1, 1)
)

# Another helper function that calculates the point estimate and standard error for a specified linear contrast (L).
# It uses the model's coefficients (beta) and variance-covariance matrix (V) to test custom linear combinations.
get_est <- function(L) {
  est <- sum(L * beta)
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  c(estimate = est, se = se)
}

## ---- Quadrant-level estimates (relative to In-memory+Factual = 0) ------------

cat("=== Quadrant-level log-odds estimates (relative to In-memory+Factual) ===\n")
quad_tab <- as.data.frame(t(sapply(quadrants, get_est)))
quad_tab$OR <- exp(quad_tab$estimate)
quad_tab$lower95 <- quad_tab$estimate - 1.96 * quad_tab$se
quad_tab$upper95 <- quad_tab$estimate + 1.96 * quad_tab$se
print(round(quad_tab, 3))
cat("\nNote: In-memory+Factual is fixed at 0 by construction (it's the reference\n")
cat("level for both axes), so its 'se' and CI are zero. Makes sense!\n")


## ---- All 6 pairwise contrasts, Holm-corrected ---------------------------------

cat("=== All 6 pairwise quadrant contrasts (Wald z-tests, Holm-adjusted p) ===\n")
pairs <- combn(names(quadrants), 2, simplify = FALSE)
pairwise <- lapply(pairs, function(p) {
  Ldiff <- quadrants[[p[1]]] - quadrants[[p[2]]]
  est <- sum(Ldiff * beta)
  se  <- sqrt(as.numeric(t(Ldiff) %*% V %*% Ldiff))
  z <- est / se
  pval <- 2 * pnorm(-abs(z))
  data.frame(comparison = paste(p[1], "-", p[2]), estimate = est, se = se,
             z = z, p_raw = pval, OR = exp(est))
})
pairwise_df <- do.call(rbind, pairwise)
pairwise_df$p_holm <- p.adjust(pairwise_df$p_raw, method = "holm")
print(pairwise_df, row.names = FALSE, digits = 4)


write.csv(quad_tab, "quadrant_estimates.csv", row.names = TRUE)
write.csv(pairwise_df, "quadrant_pairwise_contrasts.csv", row.names = FALSE)
cat("\nSaved quadrant_estimates.csv and quadrant_pairwise_contrasts.csv\n")
