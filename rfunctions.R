doReliability <- function (data, keys.list, name =  "scales", missing = TRUE, impute = "none", 
          omega = FALSE, omega.factors = NULL, write_files = FALSE, 
          digits = 2) 
{
  require(psych)
  require(pastecs)
  scoredatanames <- as.vector(gsub("-", "", unlist(keys.list)))
  scoredatanames <- unique(scoredatanames)
  data <- subset(data, select = (names(data) %in% scoredatanames))
  keys <- make.keys(length(scoredatanames), keys.list = keys.list, 
                    item.labels = scoredatanames)
  scores <- scoreItems(keys, data, missing = missing, impute = impute)
  if (omega) {
    if (!is.null(omega.factors)) {
      sapply(1:length(keys.list), function(x) {
        if (omega.factors[x] == "pa") {
          return(fa.parallel(data[keys.list[[x]]], fa = "fa", 
                             use = ifelse(missing == TRUE, "pairwise.complete.obs", 
                                          "complete.obs"), plot = FALSE)$nfact)
        }
        else {
          return(x)
        }
      })
    }
    else {
      omega.factors <- rep(3, length(keys.list))
    }
    omegas <- unlist(sapply(1:length(keys.list), function(x) {
      omega(data[keys.list[[x]]], nfactors = omega.factors[x])$omega.tot
    }))
  }
  interpret <- function(reliability = NULL) {
    interpretation <- rep("Unacceptable", length(reliability))
    interpretation[reliability >= 0.5] <- "Poor"
    interpretation[reliability >= 0.6] <- "Questionable"
    interpretation[reliability >= 0.7] <- "Acceptable"
    interpretation[reliability >= 0.8] <- "Good"
    interpretation[reliability >= 0.9] <- "Excellent"
    return(interpretation)
  }
  
  table_descriptives <- data.frame(Subscale = colnames(scores$scores), 
                                   Items = unlist(lapply(keys.list, length)), as.matrix(describe(scores$scores))[, c(2, 3, 4, 8, 9)])
  if(nrow(scores$scores) > 3 & nrow(scores$scores) < 5000){
    table_descriptives <- cbind(table_descriptives, t(stat.desc(scores$scores, basic = FALSE, norm = TRUE)[c(8, 9, 10, 11), ]))
  }
  table_descriptives$Alpha <- as.vector(scores$alpha)
  table_descriptives$Interpret.a = interpret(as.vector(scores$alpha))
  
  table_descriptives[, sapply(table_descriptives, is.numeric)] <- lapply(table_descriptives[, 
                                                                                            sapply(table_descriptives, is.numeric)], formatC, digits = digits, 
                                                                         format = "f")
  if (omega) {
    table_descriptives <- data.frame(table_descriptives, 
                                     Omega = omegas, Interpret.O = interpret(omegas))
  }
  if (write_files) 
    write.csv(table_descriptives, paste0(name, " scale table.csv"), 
              row.names = F)
  cordat <- data.frame(scores$scores)
  if (missing == FALSE) {
    cordat <- cordat[complete.cases(cordat), ]
  }
  combos <- expand.grid(names(cordat), names(cordat))
  cortab <- matrix(mapply(function(x, y) {
    tmp <- cor.test(cordat[[x]], cordat[[y]])
    paste0(formatC(tmp$estimate, digits = digits, format = "f"), 
           ifelse(tmp$p.value < 0.05, "*", ""), ifelse(tmp$p.value < 
                                                         0.01, "*", ""), ifelse(tmp$p.value < 0.001, 
                                                                                "*", ""))
  }, x = combos$Var1, y = combos$Var2), nrow = ncol(cordat))
  colnames(cortab) <- rownames(cortab) <- names(cordat)
  cortab[upper.tri(cortab)] <- ""
  if (write_files) 
    write.csv(cortab, paste0(name, " correlation table.csv"))
  return(list(table_descriptives = table_descriptives, Correlations = cortab, 
              scores = scores$scores))
}
