# Load raw data from file -------------------------------------------------
# This function loads the original data if available,
# and a synthetic dataset if they are not available.

library(worcs)
library(lavaan)
library(tidySEM)
library(ggplot2)
library(dplyr)
library(tidyr)

est_model <- function(name, model){
  res <- sem(model, data, meanstructure = TRUE)
  table_results(res, columns = NULL) %>%
    dplyr::select(label:confint_std) -> tab
  write.csv(tab, paste0(name, ".csv"), row.names = F)
  saveRDS(res, paste0("res_", name, ".RData"))
  fitmeasures(res)
}
proper=function(x){
  x <- gsub("_", " ", x)
  x <- gsub(":", "\nx\n", x)
  x <- paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  x <- gsub("Depression", "Depressive\nsymptoms", x)
  x <- gsub("Anxiety", "Anxiety\nsymptoms", x)
  x
}

load_data()

desc <- descriptives(data)
# Center all variables except excl_vars
excl_vars <- c("countryres", "countryhome", "gender", "conspiracy")
data[which(!names(data) %in% excl_vars)] <- scale(data[which(!names(data) %in% excl_vars)], scale = FALSE)
data$conspiracy <- ordered(data$conspiracy > 0, labels = c("No", "Yes"))
data$days2 <- data$days_isolation*data$days_isolation


model <- "
depression ~ perceived_threat + social_isolation + perceived_threat:social_isolation
anxiety ~ perceived_threat + sense_of_control + perceived_threat:sense_of_control
depression ~~ anxiety
"
res <- sem(model, data, meanstructure = TRUE)
model_fits <- c(model = "model_basic", fitmeasures(res))
table_results(res, columns = NULL) %>%
  #filter(!se == "") %>%
  select(label:confint_std) -> tab
write.csv(tab, "model.csv", row.names = F)
saveRDS(res, "res.RData")

model_full <- "
perceived_threat ~ income + local_diagnoses + perceived_risk
social_isolation ~ social_actions + days_isolation + days2
sense_of_control ~ days_isolation + days2

depression ~ perceived_threat + social_isolation + perceived_threat:social_isolation
anxiety ~ perceived_threat + sense_of_control + perceived_threat:sense_of_control
depression ~~ anxiety
"
res_full <- sem(model_full, data, meanstructure = TRUE)
model_fits <- rbind(model_fits, c(model = "model_full", fitmeasures(res_full)))
summary(res_full, fit.measures = TRUE, estimates = FALSE)
table_results(res_full, columns = NULL) %>%
  #filter(!se == "") %>%
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
    geom_point(data=df_point, aes_string(x = s, y = dv), alpha = .2) + labs(x = s, y = dv)+theme_bw()
}
p <- plot_curve(res_full, data, dv = "sense_of_control", s = "days_isolation", q = "days2")
ggsave("curvilinearl.png", p)
ggsave("curvilinearl.svg", p)
ggsave("curvilinearl.eps", p)


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
model_fits <- rbind(model_fits, c(model = "model_control", fitmeasures(res_control)))
summary(res_control, standardized=TRUE, fit.measures=T)

table_results(res_control, columns = NULL) %>%
  #filter(!se == "") %>%
  select(label:confint_std) -> tab
write.csv(tab, "model_control.csv", row.names = F)

saveRDS(res_control, "res_control.RData")

# Conspiracy --------------------------------------------------------------

model_conspiracy <- "
admit_confusion =~ fake_news + confusion
sense_of_control ~ conspiracy
conspiracy ~ social_isolation + trust_fb + admit_confusion + education
"
res_conspiracy <- sem(model_conspiracy, data)
model_fits <- rbind(model_fits, c(model = "model_conspiracy", fitmeasures(res_conspiracy)[colnames(model_fits)][-1]))
summary(res_conspiracy, fit.measures = TRUE)
#write.table(get_layout(res_conspiracy), "clipboard", sep = "\t")

table_results(res_conspiracy, columns = NULL) %>%
  #filter(!se == "") %>%
  select(label:confint_std) -> tab
write.csv(tab, "model_conspiracy.csv", row.names = F)

source("plot_models.R")


# Cope CFA ----------------------------------------------------------------
# tmp <- read.table("clipboard", sep = "\t")
# mod_subscales <- apply(tmp, 1, function(i){tolower(paste0(i[1], " =~ ", i[4], " + ", i[5]))})
# mod_posneg <- tolower(c(
#   sapply(unique(tmp$V2), function(i){
#   paste0(i[1], " =~ ", paste0(unlist(tmp[tmp$V2 == i, 4:5]), collapse = " + "))
#   }, USE.NAMES = FALSE),
#   apply(tmp[, 4:5], 1, function(i){paste0(i[1], " ~~ ", i[2])})
#   ))
# 
# res_subscales <- sem(mod_subscales, data, std.lv = TRUE)
# summary(res_subscales, standardized = TRUE, fit.measures = T)
# 
# res_posneg <- sem(mod_posneg, data, std.lv = TRUE)
# summary(res_posneg, standardized = TRUE, fit.measures = T)
# 
# 
# df_cope <- data[, grep("^cope\\d{1,2}$", names(data), value = TRUE)]
# model_cope <- paste0("cope =~ ", paste0(grep("^cope\\d{1,2}$", names(data), value = TRUE), collapse = " + "))
# 
# res_cope <- sem(model_cope, data, std.lv = TRUE)
# summary(res_cope, standardized = TRUE)
# fa.parallel(df_cope)



# Simple model with social isolation->anxiety and sense of control->dep --------
mod_name = "model_26_5"
fits <- est_model(mod_name, 
"
depression ~ perceived_threat  + sense_of_control + social_isolation + perceived_threat:sense_of_control + perceived_threat:social_isolation
anxiety ~ perceived_threat  + sense_of_control + social_isolation + perceived_threat:sense_of_control + perceived_threat:social_isolation
depression ~~ anxiety
")
model_fits <- rbind(model_fits, c(model = "basic_26-5", fits))
res <- readRDS(paste0("res_", mod_name, ".RData"))
lo <- get_layout("social_isolation", "",
                 "perceived_threat:social_isolation", "depression",
                 "perceived_threat", "",
                 "perceived_threat:sense_of_control", "anxiety",
                 "sense_of_control", "", rows = 5)

p <- prepare_graph(res, layout = lo, nodes = get_nodes(res, label = NULL), angle = 1, rect_width = 3, rect_height = 1.4, spacing_x = 6, curvature = 50)
p <- edit_graph(p, {curvature <- sign(curvature)*40})
edges(p) %>%
  filter(!label == "1.00") %>%
  filter(!(arrow == "none" & !(to == "anxiety" & from == "depression"))) %>%
  filter(!from == to) %>%
  within({
    label_location <- .5
    label_location[arrow == "none"] <- .5
    label_location[!arrow == "none"] <- .2
    label_colour <- "black"
    label_colour[!grepl("\\*", label)] <- "grey"
    label_colour[grepl("\\*", label)] <- "black"
    colour <- "black"
    colour[!grepl("\\*", label)] <- "grey"
    colour[grepl("\\*", label)] <- "black"
    })-> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave(paste0(mod_name, ".png"), p)
ggsave(paste0(mod_name, ".svg"), p)
ggsave(paste0(mod_name, ".eps"), p)



# Full model revised ------------------------------------------------------
mod_name <- "model_full_26_5"

fits <- est_model(mod_name, 
"
perceived_risk ~ local_diagnoses
perceived_threat ~ income + perceived_risk
social_isolation ~ social_actions + days_isolation + days2
sense_of_control ~ days_isolation + days2
social_isolation ~~ perceived_threat
social_isolation ~~ sense_of_control
perceived_threat ~~ sense_of_control

depression ~ social_actions + days_isolation + local_diagnoses+ perceived_risk
depression ~ perceived_threat:social_isolation + social_isolation +  perceived_threat + sense_of_control + perceived_threat:sense_of_control

anxiety ~ social_actions + days_isolation + local_diagnoses+ perceived_risk
anxiety ~ perceived_threat:social_isolation + social_isolation +  perceived_threat + sense_of_control + perceived_threat:sense_of_control
")
model_fits <- rbind(model_fits, c(model = "full_26-5", fits))
res <- readRDS(paste0("res_", mod_name, ".RData"))
summary(res,fit.measures=TRUE, estimates = FALSE)

tmp <- table_results(res,columns = NULL)
tmp <- modindices(res)
tmp <- tmp[order(tmp$mi, decreasing = TRUE), ]

lo <- eval(parse(text = 'structure(list(V1 = c("social_actions", "days_isolation", "", 
"days2", "local_diagnoses", "", "perceived_risk", "income"), V2 = c("social_isolation", 
"", "sense_of_control", "", "", "", "", "perceived_threat"
), V3 = c("", "", "depression", "", "", "", "anxiety", ""), V4 = c("", 
"", "perceived_threat:social_isolation", "", "", "", "perceived_threat:sense_of_control", 
"")), class = "data.frame", row.names = c(NA, -8L))'))

p <- prepare_graph(res, edges = get_edges(res, label = "est_sig_std"), nodes = get_nodes(res, label = NULL), layout = lo, angle = 1, rect_width = 3, rect_height = 1.5, spacing_y = 2, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  mutate(label_location = .2) %>%
  filter(!from == to) %>%
  filter(!label == "1.00") %>%
  filter(!(arrow == "none" & !(to == "anxiety" & from == "depression"))) %>%
  within({
    connect_from[arrow == "none"] <- "bottom"
    connect_to[arrow == "none"] <- "top"
    label_colour <- "black"
    label_colour[!grepl("\\*", label)] <- "grey"
    label_colour[grepl("\\*", label)] <- "black"
    colour <- "black"
    colour[!grepl("\\*", label)] <- "grey"
    colour[grepl("\\*", label)] <- "black"
  }) -> edges(p)
edges(p)$label_location[edges(p)$arrow == "none"] <- .5


nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave(paste0(mod_name, ".png"), p)
ggsave(paste0(mod_name, ".svg"), p)
ggsave(paste0(mod_name, ".eps"), p)


# Feeling informed as mediator -----------------------

vars <- table(gsub("^(trust|freq)_", "", c(grep("^trus", names(data), value = TRUE), 
                                           grep("^freq", names(data), value = TRUE))))

vars <- names(vars)[vars == 2]
df <- data[, c(paste0("freq_", vars), paste0("trust_", vars))]
df <- data.frame(scale(df, scale = FALSE))
ints <- data.frame(sapply(vars, function(x){
  df[[paste0("freq_", x)]] * df[[paste0("trust_", x)]]
}))
names(ints) <- paste0("int_", names(ints))

data <- cbind(data, ints)

model_control <- paste0("media =~ ", paste0(grep("int_", names(df_control), value = T), collapse = " + "), "\n", "sense_of_control ~ c*feelinginformed + avoidance_actions + a*media + freqcomcov + coping_actions + government_actions + institutional_trust + government_actions:institutional_trust
feelinginformed ~ b*media
ind := b*c
tot := a+(b*c)")

mod_name <- "model_control_26_5"

fits <- est_model(mod_name, 
          model_control)
model_fits <- rbind(model_fits, c(model = "control_26-5", fits))
res <- readRDS(paste0("res_", mod_name, ".RData"))
summary(res,fit.measures=TRUE, estimates = FALSE)

lo <- eval(parse(text = 'structure(list(V1 = c("freqcomcov", "avoidance_actions", "", 
"int_fb", "int_google", "int_gov"), V2 = c("coping_actions", 
"", "", "", "", "int_ig"), V3 = c("", "sense_of_control", "", 
"", "media", "int_maps"), V4 = c("government_actions", "", "feelinginformed", 
"", "", "int_newspaper"), V5 = c("institutional_trust", "government_actions:institutional_trust", 
"", "int_who", "int_tw", "int_nhs")), class = "data.frame", row.names = c(NA, 
-6L))'))

p <- prepare_graph(res, edges = get_edges(res, label = "est_sig_std"), nodes = get_nodes(res, label = NULL), layout = lo, rect_width = 4, rect_height = 2, ellipses_width = 1.5, spacing_y = 3, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  within({
    connect_from[arrow == "none"] <- "bottom"
    connect_to[arrow == "none"] <- "top"
    label_colour <- "black"
    label_colour[!grepl("\\*", label)] <- "grey"
    label_colour[grepl("\\*", label)] <- "black"
    colour <- "black"
    colour[!grepl("\\*", label)] <- "grey"
    colour[grepl("\\*", label)] <- "black"
  })  %>%
  filter(!arrow %in% c("none", "both")) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave(paste0(mod_name, ".png"), p)
ggsave(paste0(mod_name, ".svg"), p)
ggsave(paste0(mod_name, ".eps"), p)

# Feeling informed as mediator, anxiety as outcome -----------------------

model_anx <- gsub("sense_of_control", "anxiety", model_control)
mod_name <- "model_anxiety_26_5"
fits <- est_model(mod_name, 
          model_anx)
model_fits <- rbind(model_fits, c(model = "anxiety_26-5", fits))
res <- readRDS(paste0("res_", mod_name, ".RData"))
summary(res,fit.measures=TRUE, estimates = FALSE)

lo[lo =="sense_of_control"] <- "anxiety"

p <- prepare_graph(res, edges = get_edges(res, label = "est_sig_std"), nodes = get_nodes(res, label = NULL), layout = lo, rect_width = 4, rect_height = 2, ellipses_width = 1.5, spacing_y = 3, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  within({
    connect_from[arrow == "none"] <- "bottom"
    connect_to[arrow == "none"] <- "top"
    label_colour <- "black"
    label_colour[!grepl("\\*", label)] <- "grey"
    label_colour[grepl("\\*", label)] <- "black"
    colour <- "black"
    colour[!grepl("\\*", label)] <- "grey"
    colour[grepl("\\*", label)] <- "black"
  })  %>%
  filter(!arrow %in% c("none", "both")) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave(paste0(mod_name, ".png"), p)
ggsave(paste0(mod_name, ".svg"), p)
ggsave(paste0(mod_name, ".eps"), p)

write.csv(model_fits, "model_fits.csv", row.names = FALSE)