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