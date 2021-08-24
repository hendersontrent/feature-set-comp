#------------------------------------
# This script sets out to investigate
# why tsfresh is so unique using
# hctsa as a benchmark
#------------------------------------

#----------------------------------------
# Author: Trent Henderson, 24 August 2021
#----------------------------------------

# Load in data

load("data/corMats/20.Rda")

# Compute maximum absolute correlation per tsfresh feature

corMat2 <- corMat %>%
  mutate(correlation = abs(correlation)) %>%
  group_by(V1) %>%
  summarise(max = max(correlation, na.rm = TRUE)) %>%
  ungroup() %>%
  filter_all(all_vars(!is.infinite(.))) %>%
  mutate(V1 = gsub("tsfresh_values__", "\\1", V1))

# Draw plot

p <- corMat2 %>%
  ggplot(aes(x = reorder(V1, -max), y = max)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of tsfresh max abs correlation by feature with hctsa",
       x = "tsfresh Feature",
       y = "Max. Abs. Correlation") +
  theme_bw() +
theme(axis.text.x = element_blank())

print(p)

# Do just unique features

p1 <- corMat2 %>%
  filter(max < 0.20) %>%
  ggplot(aes(x = reorder(V1, -max), y = max)) +
  geom_bar(stat = "identity") +
  labs(title = "tsfresh maximum absolute correlation by feature with hctsa",
       subtitle = "Only features with < 0.2 max. abs. correlation",
       x = "tsfresh Feature",
       y = "Max. Abs. Correlation") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

print(p1)

# Save outputs

ggsave("output/tsfresh-hctsa-dist.png", p)
ggsave("output/tsfresh-hctsa-unique.png", p1, units = "in", width = 15, height = 11)
