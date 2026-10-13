# ==============================================================================
# CHIIR 2026 - Citation Faithfulness Study
# Exploratory Analysis: Faithfulness Category Breakdown by Quadrant
# ==============================================================================
#
# We pre-registered these analyses as descriptive only  because a
# chi-square test of independence would assume each citation is an independent
# observation, which is no the case here: citations are clustered within responses
# and queries (we handle this with the CLMM's random effects). 
#
#
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# load in the recoded dataframe
df <- readRDS("df_recoded.rds")

## ---- Chunk 1: Overall judgement distribution ----------------------------------
# Baseline, before splitting by anything.

cat("=== Overall judgement distribution ===\n")
overall <- df %>%
  count(judgement) %>%
  mutate(proportion = round(n / sum(n), 3))
print(overall)

## ---- Chunk 2: By memory_axis alone (marginal) ----------------------------------
# Recall H1 was not supported -- expect these two rows to look similar.

cat("=== judgement distribution by memory_axis ===\n")
by_memory <- df %>%
  count(memory_axis, judgement) %>%
  group_by(memory_axis) %>%
  mutate(proportion = round(n / sum(n), 3)) %>%
  ungroup()
by_memory %>%
  select(-n) %>%
  pivot_wider(names_from = judgement, values_from = proportion) %>%
  as.data.frame() %>% print()

## ---- Chunk 3: By task_axis alone (marginal) ------------------------------------
# Recall H2 was strongly supported -- expect a visible shift here.

cat("=== judgement distribution by task ===\n")
by_task <- df %>%
  count(task_axis, judgement) %>%
  group_by(task_axis) %>%
  mutate(proportion = round(n / sum(n), 3)) %>%
  ungroup()
by_task %>%
  select(-n) %>%
  pivot_wider(names_from = judgement, values_from = proportion) %>%
  as.data.frame() %>% print()
cat("Here we see a change. Far fewer fully-supported and much higher percentage of functional(+13.8%) and decorative (+4.2%), and slightly more off-topic citations(+1.2%)\n")

## ---- Chunk 4: Full quadrant breakdown  ------------------
# We want to understand which categories absorb the H2 effect. 
#

cat("=== judgement distribution by full quadrant (counts) ===\n")
quad_counts <- df %>%
  count(memory_axis, task_axis, judgement) %>%
  pivot_wider(names_from = judgement, values_from = n, values_fill = 0)
quad_counts %>% as.data.frame() %>% print()


cat("\n=== judgement distribution by full quadrant (row proportions) ===\n")
cat("(each row sums to 1.0 -- shows the SHAPE of the distribution per quadrant)\n")
quad_props <- df %>%
  count(memory_axis, task_axis, judgement) %>%
  group_by(memory_axis, task_axis) %>%
  mutate(proportion = round(n / sum(n), 3)) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(names_from = judgement, values_from = proportion, values_fill = 0)
quad_props %>% as.data.frame() %>% print()


## ---- Chunk 5: Save tables for use in the paper ------------------------------

write.csv(quad_counts, "quadrant_judgement_counts.csv", row.names = FALSE)
write.csv(quad_props, "quadrant_judgement_proportions.csv", row.names = FALSE)
cat("Saved quadrant_judgement_counts.csv and quadrant_judgement_proportions.csv\n\n")

## ---- Chunk 6: Stacked bar chart (visual version of the same thing) ------------

df$quadrant_label <- factor(
  paste(df$memory_axis, df$task_axis, sep = " + "),
  levels = c("In-memory + Factual", "Out-of-memory + Factual",
             "In-memory + Interpretive", "Out-of-memory + Interpretive")
)

p <- ggplot(df, aes(x = quadrant_label, fill = judgement)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Quadrant", y = "Proportion of citations",
       fill = "Faithfulness category",
       title = "Faithfulness category distribution by quadrant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("quadrant_judgement_stackedbar.png", p, width = 7, height = 5, dpi = 150)
cat("Saved quadrant_judgement_stackedbar.png\n")
