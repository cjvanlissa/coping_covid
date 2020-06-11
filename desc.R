nms <- c("anxiety", "depression", "sense_of_control", "social_isolation", "personal_threat", "perceived_risk", "freqcom1fam", "freqcom2fr", "freqcom3col", "freqcom4fam", "freqcom5fr", "freqcom6col", "freqcom7fam", "freqcom8fr", "freqcom9col", "freqcomcov_fam", "freqcomcov_fr", "freqcomcov_col", "activ1_wash", "activ2_wash20", "activ3_stayhome", "activ4_disinfecthome", "activ5_antibactprod", "activ6_noface", "activ7_nohands", "activ8_nophys", "activ9_mask", "activ10_maskn95", "activ12_othersfood", "cope_active", "cope_plan", "cope_posrefr", "cope_accept", "cope_humour", "cope_relspir", "cope_emosup", "cope_instrumsup", "cope_distract", "cope_denial", "cope_vent", "cope_substan", "cope_giveup", "cope_selfblame", "govac1", "govac2", "govac3", "govac4", "govac5", "govac6", "feelinginformed_avg", "freq_who", "freq_nhs", "freq_gov", "freq_newspaper", "freq_fb", "freq_tw", "freq_ig", "freq_maps", "freq_google")

nms[!nms %in% names(data)]
install.packages("tidyr")
library(tidyr)
df_plot <- data
df_plot$conspiracy <- NULL
df_plot <- pivot_longer(df_plot, cols = names(df_plot))
p <- ggplot(df_plot, aes(x = value))+geom_histogram()+facet_wrap(~name, scales = "free")+theme_bw()