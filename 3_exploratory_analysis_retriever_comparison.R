# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# Exploratory Analysis: Retriever Comparison (Tavily vs DDG)
# ==============================================================================
#
# The main model's fixed effects only give 5 model_condition coefficients,
# each relative to the reference level (gpt-4.1-mini_ddg). None of these
# directly answer "does Tavily beat DDG?". To do that we need to combine pairs of
# coefficients (as we did with the  H3/H4 quadrant comparisons). The reason is that DDG is the
# reference for gpt-4.1-mini but NOT for llama-4-scout or qwen-3.8b.
#
# This script computes, using the same covariance-propagation approach as the
# quadrant contrasts:
#   - Tavily minus DDG, separately for each of the 3 models
#   - A pooled Tavily-vs-DDG contrast, averaged equally across the 3 models
#     (i.e. testing "does retriever matter on average", not dominated by
#     whichever model happens to have more citations)
#
# The 3 per-model contrasts are Holm-corrected as a family (same question,
# asked 3 times). The pooled contrast is a single, separate test and is not
# part of that correction.
#
# ==============================================================================

library(ordinal)

#load in the main model again
fit <- readRDS("clmm_fit.rds")

beta_names <- names(coef(fit))
beta <- coef(fit)
V <- vcov(fit)[beta_names, beta_names]

## ---- Locate the model_condition coefficients ----------------------------------
# Reference level is gpt-4.1-mini_ddg (coefficient = 0, not a named row).

i_gpt_tavily   <- which(beta_names == "model_conditiongpt-4.1-mini_tavily")
i_llama_ddg    <- which(beta_names == "model_conditionllama-4-scout_ddg")
i_llama_tavily <- which(beta_names == "model_conditionllama-4-scout_tavily")
i_qwen_ddg     <- which(beta_names == "model_conditionqwen-3.8b_ddg")
i_qwen_tavily  <- which(beta_names == "model_conditionqwen-3.8b_tavily")

#sanity check
stopifnot(length(i_gpt_tavily) == 1, length(i_llama_ddg) == 1,
          length(i_llama_tavily) == 1, length(i_qwen_ddg) == 1,
          length(i_qwen_tavily) == 1)

# create a vector of zeros with the correct length
zero_L <- function() rep(0, length(beta_names))

## ---- Build per-model Tavily-minus-DDG contrast vectors ------------------------
# gpt-4.1-mini: DDG is the reference (0), so this is just the Tavily coefficient.
# llama-4-scout / qwen-3.8b: neither is the reference, so this is the
# DIFFERENCE between their two named coefficients.

L_gpt <- zero_L(); L_gpt[i_gpt_tavily] <- 1
L_llama <- zero_L(); L_llama[i_llama_tavily] <- 1; L_llama[i_llama_ddg] <- -1
L_qwen  <- zero_L(); L_qwen[i_qwen_tavily]  <- 1; L_qwen[i_qwen_ddg]  <- -1

# Pooled: equal-weighted average of the three per-model contrasts.
L_pooled <- (L_gpt + L_llama + L_qwen) / 3

get_contrast <- function(L) {
  est <- sum(L * beta)
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  z <- est / se
  p <- 2 * pnorm(-abs(z))
  c(estimate = est, se = se, z = z, p_raw = p, OR = exp(est))
}

## ---- Per-model contrasts (Holm-corrected as a family) --------------------------

cat("=== Tavily minus DDG, per model (Holm-corrected across the 3 models) ===\n")
per_model <- rbind(
  "GPT-4.1-mini"    = get_contrast(L_gpt),
  "Llama-4-Scout"   = get_contrast(L_llama),
  "Qwen-3.8B"       = get_contrast(L_qwen)
)
per_model <- as.data.frame(per_model)
per_model$p_holm <- p.adjust(per_model$p_raw, method = "holm")
print(round(per_model, 4))
cat("\n")

## ---- Pooled contrast (single test, not part of the correction above) ----------

cat("=== Pooled Tavily minus DDG (equal-weighted across models; single test) ===\n")
pooled <- get_contrast(L_pooled)
print(round(pooled, 4))
cat("\n")

## ---- Save for the write-up ------------------------------------------------------

per_model$model <- rownames(per_model)
out <- rbind(
  per_model[, c("model", "estimate", "se", "z", "p_raw", "p_holm", "OR")],
  data.frame(model = "Pooled", estimate = pooled["estimate"], se = pooled["se"],
             z = pooled["z"], p_raw = pooled["p_raw"], p_holm = NA, OR = pooled["OR"])
)
write.csv(out, "retriever_comparison.csv", row.names = FALSE)
cat("Saved retriever_comparison.csv\n")
