# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# Domain Comparison: All Pairwise Contrasts
# ==============================================================================
#
# The main model's domain coefficients only tell us how each domain compares
# to the reference level (Environment and Climate Science). It does not
# directly say how Global Finance compares to Medicine and Public
# Health, or whether Global Finance is the worst of all four domains overall.
# This script computes domain-level estimates and all six pairwise contrasts,
# using the same covariance-propagated approach as the quadrant contrasts
# and retriever comparison, rather than reading pairwise
# domain differences off two separate vs-reference coefficients.
#
# ==============================================================================

suppressMessages(library(ordinal))

#load the saved clmm_fit.rds
fit <- readRDS("clmm_fit.rds")

beta_names <- names(coef(fit))
beta <- coef(fit)
V <- vcov(fit)[beta_names, beta_names]

## ---- Get the domain coefficients --------------------------------------------
# Reference level is Environment and Climate Science (coefficient = 0, not a
# named row, since it's alphabetically first and R's default factor coding
# uses the first level as baseline).

domain_coef_names <- grep("^domain", beta_names, value = TRUE)
cat("Domain coefficients found in model:\n")
print(domain_coef_names)

i_finance   <- which(beta_names == "domainGlobal Finance")
i_law       <- which(beta_names == "domainLaw and Regulations")
i_medicine  <- which(beta_names == "domainMedicine and Public Health")

# sanity check 
stopifnot(length(i_finance) == 1, length(i_law) == 1, length(i_medicine) == 1)

zero_L <- function() rep(0, length(beta_names))

domains <- list(
  "Environment and Climate Science" = zero_L(),  # reference domain
  "Global Finance"                  = { L <- zero_L(); L[i_finance]  <- 1; L },
  "Law and Regulations"             = { L <- zero_L(); L[i_law]      <- 1; L },
  "Medicine and Public Health"      = { L <- zero_L(); L[i_medicine] <- 1; L }
)

get_est <- function(L) {
  est <- sum(L * beta)
  se  <- sqrt(as.numeric(t(L) %*% V %*% L))
  c(estimate = est, se = se)
}

## ---- Domain-level estimates (relative to Environment and Climate Science) -----

cat("=== Domain-level log-odds estimates (relative to Environment and Climate Science) ===\n")
domain_tab <- as.data.frame(t(sapply(domains, get_est)))
domain_tab$OR <- exp(domain_tab$estimate)
domain_tab$lower95 <- domain_tab$estimate - 1.96 * domain_tab$se
domain_tab$upper95 <- domain_tab$estimate + 1.96 * domain_tab$se
print(round(domain_tab, 3))

## ---- All 6 pairwise domain contrasts, Holm-corrected --------------------------

cat("=== All 6 pairwise domain contrasts (Wald z-tests, Holm-adjusted p) ===\n")
pairs <- combn(names(domains), 2, simplify = FALSE)
pairwise <- lapply(pairs, function(p) {
  Ldiff <- domains[[p[1]]] - domains[[p[2]]]
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

write.csv(domain_tab, "domain_estimates.csv", row.names = TRUE)
write.csv(pairwise_df, "domain_pairwise_contrasts.csv", row.names = FALSE)
cat("\nSaved domain_estimates.csv and domain_pairwise_contrasts.csv\n")

cat("\n=== Interpretation guide ===\n")
cat("To check whether Global Finance is significantly the WORST of all four domains,\n")
cat("look at the 3 rows involving 'Global Finance': it needs to be significantly\n")
cat("lower than all three others (p_holm < 0.05, negative estimate when Global\n")
cat("Finance is the second-named domain, or positive when it is first-named and\n")
cat("the sign is flipped) for that claim to hold cleanly, the same logic used for\n")
cat("the quadrant ceiling/floor checks in Step 4.\n")