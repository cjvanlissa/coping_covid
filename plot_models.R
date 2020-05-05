library(dplyr)
library(ggplot2)
library(tidySEM)
proper=function(x){
  x <- gsub("_", " ", x)
  x <- gsub(":", "\nx\n", x)
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
}

# write.table(get_layout("social_isolation", "",
#                        "personal_threat:social_isolation", "depression",
#                        "personal_threat", "",
#                        "personal_threat:sense_of_control", "anxiety",
#                        "sense_of_control", "", rows = 5)
# , "clipboard", sep = "\t", row.names = F, col.names = F)


# Basic model -------------------------------------------------------------

res <- readRDS("res.RData")

lo <- get_layout("social_isolation", "",
           "personal_threat:social_isolation", "depression",
           "personal_threat", "",
           "personal_threat:sense_of_control", "anxiety",
           "sense_of_control", "", rows = 5)

p <- prepare_graph(res, layout = lo, nodes = get_nodes(res, label = NULL), angle = 1, rect_width = 3, rect_height = 1.4, spacing_x = 6, curvature = 50)
p <- edit_graph(p, {curvature <- sign(curvature)*40})
edges(p) %>%
  filter(!label == "1.00") %>%
  filter(!(arrow == "none" & !(to == "anxiety" & from == "depression"))) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave("model.png", p)
ggsave("model.svg", p)
ggsave("model.eps", p)


# Full model --------------------------------------------------------------

res_full <- readRDS("res_full.RData")

lo <- eval(parse(text = 'structure(list(V1 = c("social_actions", "days_isolation", "local_diagnoses",
"income", "perceived_risk", "days2", ""), V2 = c("personal_threat:social_isolation",
"social_isolation", "", "personal_threat", "", "sense_of_control",
"personal_threat:sense_of_control"), V3 = c("", "", "depression",
"", "anxiety", "", "")), class = "data.frame", row.names = c(NA,
-7L))'))

p <- prepare_graph(res_full, edges = get_edges(res_full, label = "est_sig_std"), nodes = get_nodes(res_full, label = NULL), layout = lo, angle = 1, rect_width = 3, rect_height = 1.5, spacing_y = 2, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  mutate(label_location = .2) %>%
  within({
    connect_from[from == to & from == "social_isolation"] <- "bottom"
    connect_to[from == to & from == "social_isolation"] <- "bottom"
    connect_from[from == to & from == "sense_of_control"] <- "top"
    connect_to[from == to & from == "sense_of_control"] <- "top"
  }) -> edges(p)
edges(p)$label_location[edges(p)$arrow == "none"] <- .5
#  filter(!label == "1.00") %>%
#  filter(!(arrow == "none" & !(to == "anxiety" & from == "depression"))) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave("model_full.png", p)
ggsave("model_full.svg", p)
ggsave("model_full.eps", p)

# Plot control ------------------------------------------------------------

res_control <- readRDS("res_control.RData")

lo <- eval(parse(text = 'structure(list(V1 = c("", "", "", "int_fb"), V2 = c("avoidance_actions", 
"", "", "int_google"), V3 = c("freqcomcov", "", "", "int_gov"
), V4 = c("coping_actions", "", "", "int_ig"), V5 = c("", "sense_of_control", 
"media", "int_maps"), V6 = c("government_actions", "", "", "int_newspaper"
), V7 = c("institutional_trust", "", "", "int_nhs"), V8 = c("government_actions:institutional_trust", 
"", "", "int_tw"), V9 = c("", "", "", "int_who")), class = "data.frame", row.names = c(NA, 
-4L))'))

lo <- eval(parse(text = 'structure(list(V1 = c("freqcomcov", "avoidance_actions", "", 
"int_fb", "int_google", "int_gov"), V2 = c("coping_actions", 
"", "", "", "", "int_ig"), V3 = c("", "sense_of_control", "", 
"", "media", "int_maps"), V4 = c("government_actions", "", "", 
"", "", "int_newspaper"), V5 = c("institutional_trust", "government_actions:institutional_trust", 
"", "int_who", "int_tw", "int_nhs")), class = "data.frame", row.names = c(NA, 
-6L))'))

p <- prepare_graph(res_control, edges = get_edges(res_control, label = "est_sig_std"), nodes = get_nodes(res_control, label = NULL), layout = lo, rect_width = 4, rect_height = 2, ellipses_width = 1.5, spacing_y = 3, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  #filter(!(from == to & from == "media")) %>%
  filter(!arrow %in% c("none", "both")) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()
ggsave("model_control.png", p)
ggsave("model_control.svg", p)
ggsave("model_control.eps", p)



# Plot conspiracy ---------------------------------------------------------

lo <- eval(parse(text = 'structure(list(V1 = c("", "", "social_isolation", ""), V2 = c("", 
"", "trust_fb", ""), V3 = c("sense_of_control", "conspiracy", 
"", "fake_news"), V4 = c("", "", "admit_confusion", ""), V5 = c("", 
"", "education", "confusion")), class = "data.frame", row.names = c(NA, 
-4L))'))

lo <- eval(parse(text = 'structure(list(V1 = c("", "", "social_isolation", ""), V2 = c("", 
"", "trust_fb", ""), V3 = c("sense_of_control", "conspiracy", 
"admit_confusion", "fake_news"), V4 = c("", "", "education", 
"confusion")), class = "data.frame", row.names = c(NA, -4L))'))

p <- prepare_graph(res_conspiracy, edges = get_edges(res_conspiracy, label = "est_sig_std"), nodes = get_nodes(res_conspiracy, label = NULL), layout = lo, angle = 180, rect_width = 4, rect_height = 2, ellipses_width = 4, ellipses_height = 2, spacing_y = 4, spacing_x = 6, curvature = 50, text_size = 3)

edges(p) %>%
  #filter(!(from == to & from == "media")) %>%
  filter(!arrow %in% c("none", "both")) -> edges(p)

nodes(p) %>%
  mutate(label = proper(label)) -> nodes(p)

p <- plot(p)+coord_fixed()

ggsave("model_conspiracy.png", p)
ggsave("model_conspiracy.svg", p)
ggsave("model_conspiracy.eps", p)
