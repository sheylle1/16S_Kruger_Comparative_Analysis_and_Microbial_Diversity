library(readxl)
library(tidyverse)
library(rstatix)
library(openxlsx)  

# Read Excel file
df <- read_excel("Entropydata.xlsx", sheet = 1) %>%
  mutate(Diet_type = factor(Diet_type))

# Run tests

## Shannon
kruskal_shannon <- kruskal_test(df, shannon_entropy ~ Diet_type) %>%
  mutate(Metric = "Shannon_entropy")

dunn_shannon <- dunn_test(df, shannon_entropy ~ Diet_type,
                          p.adjust.method = "holm", detailed = TRUE) %>%
  mutate(Metric = "Shannon_entropy")


## Faith's PD
kruskal_faith <- kruskal_test(df, faith_pd ~ Diet_type) %>%
  mutate(Metric = "Faith_PD")

dunn_faith <- dunn_test(df, faith_pd ~ Diet_type,
                        p.adjust.method = "holm", detailed = TRUE) %>%
  mutate(Metric = "Faith_PD")


## Observed feature
kruskal_observed <- kruskal_test(df, observed_features ~ Diet_type) %>%
  mutate(Metric = "Observed_features")

dunn_observed <- dunn_test(df, observed_features ~ Diet_type,
                           p.adjust.method = "holm", detailed = TRUE) %>%
  mutate(Metric = "Observed_features")


#  Combine results into tables for export

# Kruskal–Wallis summary table
kw_table <- bind_rows(
  kruskal_shannon,
  kruskal_faith,
  kruskal_observed
) %>%
  select(Metric, everything())

# Dunn post-hoc table
dunn_table <- bind_rows(
  dunn_shannon,
  dunn_faith,
  dunn_observed
) %>%
  select(Metric, group1, group2, n1, n2, estimate, statistic, p, p.adj, p.adj.signif, method)


# supplementary table
wb <- createWorkbook()

addWorksheet(wb, "Kruskal-Wallis Results")
addWorksheet(wb, "Dunn Post-hoc Results")

writeData(wb, "Kruskal-Wallis Results", kw_table)
writeData(wb, "Dunn Post-hoc Results", dunn_table)

saveWorkbook(wb, "Supplementary_Table_AlphaDiversity.xlsx", overwrite = TRUE)
