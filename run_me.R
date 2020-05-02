# Load raw data from file -------------------------------------------------
# This function loads the original data if available,
# and a synthetic dataset if they are not available.

library(worcs)
library(lavaan)
library(tidySEM)
library(ggplot2)
library(dplyr)
data <- load_data()

model <- "
depression ~ personal_threat + social_isolation + personal_threat:social_isolation
anxiety ~ personal_threat + sense_of_control + personal_threat:sense_of_control
depression ~~ anxiety
"
res <- sem(model, data, meanstructure = TRUE)
table_results(res, columns = NULL) %>%
  filter(!se == "") %>%
  select(label:confint_std) -> tab
write.csv(tab, "model.csv", row.names = F)
saveRDS(res, "res.RData")

data$days2 <- data$days_isolation*data$days_isolation
model_full <- "
personal_threat ~ income + local_diagnoses + perceived_risk
social_isolation ~ social_actions + days_isolation + days2
sense_of_control ~ days_isolation + days2

depression ~ personal_threat + social_isolation + personal_threat:social_isolation
anxiety ~ personal_threat + sense_of_control + personal_threat:sense_of_control
depression ~~ anxiety
"
res_full <- sem(model_full, data, meanstructure = TRUE)
table_results(res_full, columns = NULL) %>%
  filter(!se == "") %>%
  select(label:confint_std) -> tab
write.csv(tab, "model_full.csv", row.names = F)

saveRDS(res_full, "res_full.RData")
pt <- parTable(res_full)

plot_curve <- function(model, data, dv, s, q){
  a <- pt$est[pt$lhs == dv & pt$op == "~1"]  
  b <- pt$est[pt$lhs == dv & pt$op == "~" & pt$rhs == s]
  c <- pt$est[pt$lhs == dv & pt$op == "~" & pt$rhs == q]
  x <- seq(from = min(data[[s]], na.rm = TRUE), to = max(data[[s]], na.rm = TRUE), length.out = 100)
  y <- a + (b*x) + (c*x^2)
  df_point <- data[, c(s, dv)]
  ggplot(NULL)+ geom_path(data = data.frame(x=x, y=y), aes(x=x, y=y))+
    geom_point(data=df_point, aes_string(x = s, y = dv)) + labs(x = s, y = dv)+theme_bw()
}
p <- plot_curve(res_full, data, dv = "sense_of_control", s = "days_isolation", q = "days2")
ggsave("curvilinearl.png", p)
ggsave("curvilinearl.svg", p)
ggsave("curvilinearl.eps", p)



model_conspiracy <- "
sense_of_control ~ avoidance_actions + freqcomcov + coping_actions + government_actions + institutional_trust + government_actions:institutional_trust"
res_control <- sem(model_control, data)
summary(res_control)


# library(tidyLPA)
# 
# mix <- estimate_profiles(data[, c("misinformedness", "trust_local", "trust_global", "trust_scient")],
#                          1:4, variances = "varying")
# 
# sapply(data[, c("misinformedness", "trust_local", "trust_global", "trust_scient")], hist)
# 
# psych::principal(data[, c("misinformedness", "trust_local", "trust_global", "trust_scient")])
# )])

source("plot_models.R")


# Latent variable model with interacting indicators -----------------------
vars <- table(gsub("^(trust|freq)_", "", c(grep("^trus", names(data), value = TRUE), 
                                   grep("^freq", names(data), value = TRUE))))

vars <- names(vars)[vars == 2]
df <- data[, c(paste0("freq_", vars), paste0("trust_", vars))]
df <- data.frame(scale(df, scale = FALSE))
ints <- data.frame(sapply(vars, function(x){
  df[[paste0("freq_", x)]] * df[[paste0("trust_", x)]]
}))
names(ints) <- paste0("int_", names(ints))

df_control <- cbind(data, ints)

model_control <- paste0("media =~ ", paste0(grep("int_", names(df_control), value = T), collapse = " + "), "\n", "sense_of_control ~ avoidance_actions + media + freqcomcov + coping_actions + government_actions + institutional_trust + government_actions:institutional_trust")

res_control <- sem(model_control, df_control)
summary(res_control, standardized=TRUE)

