##Psychological Impact of Threat and Lockdown - Figures script##

library(foreign)
library(ggplot2)
library(sm)
library(patchwork) 
library(ggplot2)
library(reshape2)

##Preparation##

data <- read.spss("DATA_PsychologicalImpact_COVID19.sav",
                  use.value.labels = FALSE,
                  to.data.frame = TRUE)
names(data) <- tolower(names(data))

data_usa <- data[data$countryres==187,]
data_uk <-data[data$countryres==185,]
data_ita <-data[data$countryres==84,]
data_bra <-data[data$countryres==24,]
data_aus <-data[data$countryres==9,]
data_nld <-data[data$countryres==122,]
data_por <-data[data$countryres==138,]
data_ger <-data[data$countryres==65,]
data_fra <-data[data$countryres==61,]
data_fin <-data[data$countryres==60,]
data_cro <-data[data$countryres==42,]
data_nz <-data[data$countryres==123,]

######MANUSCRIPT########

result.mean_usa <-  mean(data_usa$depression_sum,na.rm = TRUE)
result.mean_uk <-  mean(data_uk$depression_sum,na.rm = TRUE)
result.mean_ita <-  mean(data_ita$depression_sum,na.rm = TRUE)
result.mean_bra <-  mean(data_bra$depression_sum,na.rm = TRUE)
result.mean_aus <-  mean(data_aus$depression_sum,na.rm = TRUE)
result.mean_nld <-  mean(data_nld$depression_sum,na.rm = TRUE)
result.mean_por <-  mean(data_por$depression_sum,na.rm = TRUE)
result.mean_ger <-  mean(data_ger$depression_sum,na.rm = TRUE)
result.mean_fra <-  mean(data_fra$depression_sum,na.rm = TRUE)
result.mean_fin <-  mean(data_fin$depression_sum,na.rm = TRUE)
result.mean_cro <-  mean(data_cro$depression_sum,na.rm = TRUE)
result.mean_nz <-  mean(data_nz$depression_sum,na.rm = TRUE)

anx.mean_usa <-  mean(data_usa$anxiety_sum,na.rm = TRUE)
anx.mean_uk <-  mean(data_uk$anxiety_sum,na.rm = TRUE)
anx.mean_ita <-  mean(data_ita$anxiety_sum,na.rm = TRUE)
anx.mean_bra <-  mean(data_bra$anxiety_sum,na.rm = TRUE)
anx.mean_aus <-  mean(data_aus$anxiety_sum,na.rm = TRUE)
anx.mean_nld <-  mean(data_nld$anxiety_sum,na.rm = TRUE)
anx.mean_por <-  mean(data_por$anxiety_sum,na.rm = TRUE)
anx.mean_ger <-  mean(data_ger$anxiety_sum,na.rm = TRUE)
anx.mean_fra <-  mean(data_fra$anxiety_sum,na.rm = TRUE)
anx.mean_fin <-  mean(data_fin$anxiety_sum,na.rm = TRUE)
anx.mean_cro <-  mean(data_cro$anxiety_sum,na.rm = TRUE)
anx.mean_nz <-  mean(data_nz$anxiety_sum,na.rm = TRUE)

govac.mean_usa <-  mean(data_usa$actionsgov_avg,na.rm = TRUE)
govac.mean_uk <-  mean(data_uk$actionsgov_avg,na.rm = TRUE)
govac.mean_ita <-  mean(data_ita$actionsgov_avg,na.rm = TRUE)
govac.mean_bra <-  mean(data_bra$actionsgov_avg,na.rm = TRUE)
govac.mean_aus <-  mean(data_aus$actionsgov_avg,na.rm = TRUE)
govac.mean_nld <-  mean(data_nld$actionsgov_avg,na.rm = TRUE)
govac.mean_por <-  mean(data_por$actionsgov_avg,na.rm = TRUE)
govac.mean_ger <-  mean(data_ger$actionsgov_avg,na.rm = TRUE)
govac.mean_fra <-  mean(data_fra$actionsgov_avg,na.rm = TRUE)
govac.mean_fin <-  mean(data_fin$actionsgov_avg,na.rm = TRUE)
govac.mean_cro <-  mean(data_cro$actionsgov_avg,na.rm = TRUE)
govac.mean_nz <-  mean(data_nz$actionsgov_avg,na.rm = TRUE)

instit.mean_usa <-  mean(data_usa$instittrust_avg,na.rm = TRUE)
instit.mean_uk <-  mean(data_uk$instittrust_avg,na.rm = TRUE)
instit.mean_ita <-  mean(data_ita$instittrust_avg,na.rm = TRUE)
instit.mean_bra <-  mean(data_bra$instittrust_avg,na.rm = TRUE)
instit.mean_aus <-  mean(data_aus$instittrust_avg,na.rm = TRUE)
instit.mean_nld <-  mean(data_nld$instittrust_avg,na.rm = TRUE)
instit.mean_por <-  mean(data_por$instittrust_avg,na.rm = TRUE)
instit.mean_ger <-  mean(data_ger$instittrust_avg,na.rm = TRUE)
instit.mean_fra <-  mean(data_fra$instittrust_avg,na.rm = TRUE)
instit.mean_fin <-  mean(data_fin$instittrust_avg,na.rm = TRUE)
instit.mean_cro <-  mean(data_cro$instittrust_avg,na.rm = TRUE)
instit.mean_nz <-  mean(data_nz$instittrust_avg,na.rm = TRUE)


##Figure 1##

mytheme =  theme_bw() + theme(panel.grid.major=element_blank(),
                              panel.grid.minor=element_blank(),
                              panel.background = element_blank(),
                              panel.border=element_blank(),
                              axis.line=element_line(colour = "black"),
                              axis.text=element_text(size=12),
                              axis.title=element_text(size=12),
                              legend.title=element_blank(),
                              legend.position = "none"
                              #legend.text = element_text(size = 12),
                              #legend.position = "bottom"
)

anxdepr_usa <- data.frame(data_usa$anxiety_sum,data_usa$depression_sum)
m_anxdepr_usa<- melt(anxdepr_usa)
p_anxdepr_usa <- ggplot(m_anxdepr_usa,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_usa), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_usa), color="turquoise1", linetype="dashed", size=1) 
usa_anxdepr <- p_anxdepr_usa + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("United States") + scale_y_continuous(limits = c(0, 0.15))

m_anxdepr_uk<- melt(anxdepr_uk)
p_anxdepr_uk <- ggplot(m_anxdepr_uk,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_uk), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_uk), color="turquoise1", linetype="dashed", size=1)
uk_anxdepr <- p_anxdepr_uk + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) +  xlab("score") + ggtitle("United Kingdom") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_aus <- data.frame(data_aus$anxiety_sum,data_aus$depression_sum)
m_anxdepr_aus<- melt(anxdepr_aus)
p_anxdepr_aus <- ggplot(m_anxdepr_aus,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_aus), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_aus), color="turquoise1", linetype="dashed", size=1)
aus_anxdepr <- p_anxdepr_aus + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Australia") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_bra <- data.frame(data_bra$anxiety_sum,data_bra$depression_sum)
m_anxdepr_bra<- melt(anxdepr_bra)
p_anxdepr_bra <- ggplot(m_anxdepr_bra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_bra), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_bra), color="turquoise1", linetype="dashed", size=1)
bra_anxdepr <- p_anxdepr_bra + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Brazil") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_ita <- data.frame(data_ita$anxiety_sum,data_ita$depression_sum)
m_anxdepr_ita<- melt(anxdepr_ita)
p_anxdepr_ita <- ggplot(m_anxdepr_ita,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_ita), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_ita), color="turquoise1", linetype="dashed", size=1)
ita_anxdepr <- p_anxdepr_ita + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Italy") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_fin <- data.frame(data_fin$anxiety_sum,data_fin$depression_sum)
m_anxdepr_fin<- melt(anxdepr_fin)
p_anxdepr_fin <- ggplot(m_anxdepr_fin,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_fin), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_fin), color="turquoise1", linetype="dashed", size=1)
fin_anxdepr <- p_anxdepr_fin + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Finland") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_fra <- data.frame(data_fra$anxiety_sum,data_fra$depression_sum)
m_anxdepr_fra<- melt(anxdepr_fra)
p_anxdepr_fra <- ggplot(m_anxdepr_fra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_fra), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_fra), color="turquoise1", linetype="dashed", size=1)
fra_anxdepr <- p_anxdepr_fra + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("France") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_ger <- data.frame(data_ger$anxiety_sum,data_ger$depression_sum)
m_anxdepr_ger<- melt(anxdepr_ger)
p_anxdepr_ger <- ggplot(m_anxdepr_ger,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_ger), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_ger), color="turquoise1", linetype="dashed", size=1)
ger_anxdepr <- p_anxdepr_ger + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Germany") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_nld <- data.frame(data_nld$anxiety_sum,data_nld$depression_sum)
m_anxdepr_nld<- melt(anxdepr_nld)
p_anxdepr_nld <- ggplot(m_anxdepr_nld,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_nld), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_nld), color="turquoise1", linetype="dashed", size=1)
nld_anxdepr <- p_anxdepr_nld + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Netherlands") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_nz <- data.frame(data_nz$anxiety_sum,data_nz$depression_sum)
m_anxdepr_nz<- melt(anxdepr_nz)
p_anxdepr_nz <- ggplot(m_anxdepr_nz,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_nz), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_nz), color="turquoise1", linetype="dashed", size=1)
nz_anxdepr <- p_anxdepr_nz + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("New Zealand") + scale_x_continuous(limits = c(0, 20)) + scale_y_continuous(limits = c(0, 0.15))

anxdepr_por <- data.frame(data_por$anxiety_sum,data_por$depression_sum)
m_anxdepr_por<- melt(anxdepr_por)
p_anxdepr_por <- ggplot(m_anxdepr_por,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_por), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_por), color="turquoise1", linetype="dashed", size=1)
por_anxdepr <- p_anxdepr_por + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Portugal") + scale_y_continuous(limits = c(0, 0.15))

anxdepr_cro <- data.frame(data_cro$anxiety_sum,data_cro$depression_sum)
m_anxdepr_cro<- melt(anxdepr_cro)
p_anxdepr_cro <- ggplot(m_anxdepr_cro,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept=anx.mean_cro), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=result.mean_cro), color="turquoise1", linetype="dashed", size=1)
cro_anxdepr <- p_anxdepr_cro + mytheme + scale_fill_discrete(labels = c("anxiety", "depression")) + xlab("score") + ggtitle("Croatia") + scale_y_continuous(limits = c(0, 0.15))


(aus_anxdepr + bra_anxdepr + cro_anxdepr + fin_anxdepr + fra_anxdepr + ger_anxdepr + ita_anxdepr + nld_anxdepr + nz_anxdepr + por_anxdepr + uk_anxdepr + usa_anxdepr)


##Figure 2 & 3 see SEM script##

##Figure 4##

doubleregression <- plot_summs(tableSM_9_1, tableSM_9_2, omit.coefs = c("(Intercept)", "Intercept"),
                               model.names = c("Sense of Control Regression", "Reduce Anxiety Regression"))

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.y=element_blank(),
        legend.title=element_blank(), 
        legend.text = element_text(size = 12),
        legend.position = "top"
  )

drplot <- doubleregression  + apatheme 

drplot + scale_y_discrete(labels=c("Interaction Govt Actions * Institutional Trust",
                                   "Institutional Trust", 
                                   "Government Actions", 
                                   "Maladaptive Coping", 
                                   "Adaptive Coping", 
                                   "Avoidance Actions", 
                                   "Frequency COVID-19 Communication", 
                                   "Feeling Informed"
)) 



##Figure 5###

haveppe_all_hist <- qplot(sit_occup_ppe, data=data, alpha=I(.5), binwidth=1,
                          xlab="Access to sufficient PPE", ylab="Frequency", 
                          ylim=c(0,100), colour = I("black"))

haveppe_new <- haveppe_all_hist + geom_vline(aes(xintercept= ppe.mean_all), color="black", linetype="dashed", size=1)

p_haveppe_all_hist <- haveppe_new + theme(legend.position = "none", axis.text=element_text(size=15),
                                          axis.title=element_text(size=20, face="plain"), 
                                          title=element_text(size=25, face="bold"),
                                          panel.background = element_rect(fill = "transparent"),
                                          axis.line = element_line(colour = "black"),
                                          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
                                          axis.title.x = element_text(vjust=-1))


par(mar=c(5,4,5,1)+.1)
plot(p_haveppe_all_hist)



##Figure 6##

govacinst_usa <- data.frame(data_usa$governmentactions_avg,data_usa$instittrust_avg)
m_govacinst_usa<- melt(govacinst_usa)
p_govacinst_usa <- ggplot(m_govacinst_usa,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_usa), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_usa), color="turquoise1", linetype="dashed", size=1)
usa_govacinst <- p_govacinst_usa + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("United States") + scale_y_continuous(limits = c(0, 0.8))

govacinst_uk <- data.frame(data_uk$governmentactions_avg,data_uk$instittrust_avg)
m_govacinst_uk<- melt(govacinst_uk)
p_govacinst_uk <- ggplot(m_govacinst_uk,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_uk), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_uk), color="turquoise1", linetype="dashed", size=1)
uk_govacinst <- p_govacinst_uk + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) +  xlab("score") + ggtitle("United Kingdom") + scale_y_continuous(limits = c(0, 0.8))

govacinst_aus <- data.frame(data_aus$governmentactions_avg,data_aus$instittrust_avg)
m_govacinst_aus<- melt(govacinst_aus)
p_govacinst_aus <- ggplot(m_govacinst_aus,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_aus), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_aus), color="turquoise1", linetype="dashed", size=1)
aus_govacinst <- p_govacinst_aus + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Australia") + scale_y_continuous(limits = c(0, 0.8))

govacinst_bra <- data.frame(data_bra$governmentactions_avg,data_bra$instittrust_avg)
m_govacinst_bra<- melt(govacinst_bra)
p_govacinst_bra <- ggplot(m_govacinst_bra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_bra), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_bra), color="turquoise1", linetype="dashed", size=1)
bra_govacinst <- p_govacinst_bra + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Brazil") + scale_y_continuous(limits = c(0, 0.8))

govacinst_ita <- data.frame(data_ita$governmentactions_avg,data_ita$instittrust_avg)
m_govacinst_ita<- melt(govacinst_ita)
p_govacinst_ita <- ggplot(m_govacinst_ita,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_ita), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_ita), color="turquoise1", linetype="dashed", size=1)
ita_govacinst <- p_govacinst_ita + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Italy") + scale_y_continuous(limits = c(0, 0.8))

govacinst_fin <- data.frame(data_fin$governmentactions_avg,data_fin$instittrust_avg)
m_govacinst_fin<- melt(govacinst_fin)
p_govacinst_fin <- ggplot(m_govacinst_fin,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_fin), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_fin), color="turquoise1", linetype="dashed", size=1)
fin_govacinst <- p_govacinst_fin + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Finland") + scale_y_continuous(limits = c(0, 0.8))

govacinst_fra <- data.frame(data_fra$governmentactions_avg,data_fra$instittrust_avg)
m_govacinst_fra<- melt(govacinst_fra)
p_govacinst_fra <- ggplot(m_govacinst_fra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_fra), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_fra), color="turquoise1", linetype="dashed", size=1)
fra_govacinst <- p_govacinst_fra + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("France") + scale_y_continuous(limits = c(0, 0.8))

govacinst_ger <- data.frame(data_ger$governmentactions_avg,data_ger$instittrust_avg)
m_govacinst_ger<- melt(govacinst_ger)
p_govacinst_ger <- ggplot(m_govacinst_ger,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_ger), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_ger), color="turquoise1", linetype="dashed", size=1)
ger_govacinst <- p_govacinst_ger + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Germany") + scale_y_continuous(limits = c(0, 0.8))

govacinst_nld <- data.frame(data_nld$governmentactions_avg,data_nld$instittrust_avg)
m_govacinst_nld<- melt(govacinst_nld)
p_govacinst_nld <- ggplot(m_govacinst_nld,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_nld), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_nld), color="turquoise1", linetype="dashed", size=1)
nld_govacinst <- p_govacinst_nld + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Netherlands") + scale_y_continuous(limits = c(0, 0.8))

govacinst_nz <- data.frame(data_nz$governmentactions_avg,data_nz$instittrust_avg)
m_govacinst_nz<- melt(govacinst_nz)
p_govacinst_nz <- ggplot(m_govacinst_nz,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_nz), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_nz), color="turquoise1", linetype="dashed", size=1)
nz_govacinst <- p_govacinst_nz + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("New Zealand")

govacinst_por <- data.frame(data_por$governmentactions_avg,data_por$instittrust_avg)
m_govacinst_por<- melt(govacinst_por)
p_govacinst_por <- ggplot(m_govacinst_por,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_por), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_por), color="turquoise1", linetype="dashed", size=1)
por_govacinst <- p_govacinst_por + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Portugal") + scale_y_continuous(limits = c(0, 0.8))

govacinst_cro <- data.frame(data_cro$governmentactions_avg,data_cro$instittrust_avg)
m_govacinst_cro<- melt(govacinst_cro)
p_govacinst_cro <- ggplot(m_govacinst_cro,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govac.mean_cro), color="pink", linetype="dashed", size=1) + geom_vline(aes(xintercept=instit.mean_cro), color="turquoise1", linetype="dashed", size=1)
cro_govacinst <- p_govacinst_cro + mytheme + scale_fill_discrete(labels = c("government actions", "institutional trust")) + xlab("score") + ggtitle("Croatia") + scale_y_continuous(limits = c(0, 0.8))


(aus_govacinst + bra_govacinst + cro_govacinst + fin_govacinst + fra_govacinst + ger_govacinst + ita_govacinst + nld_govacinst + nz_govacinst + por_govacinst + uk_govacinst + usa_govacinst)

##Figure 7##

column1 <- c("Australia","Brazil","Croatia","Finland","France","Germany","Italy","Netherlands", "New Zealand","Portugal","United Kingdom","United States")
column2 <- c(govac.mean_aus,govac.mean_bra,govac.mean_cro,govac.mean_fin,govac.mean_fra,govac.mean_ger,govac.mean_ita,govac.mean_nld, govac.mean_nz, govac.mean_por,govac.mean_uk,govac.mean_usa)
column3 <- c(instit.mean_aus,instit.mean_bra,instit.mean_cro,instit.mean_fin,instit.mean_fra,instit.mean_ger,instit.mean_ita,instit.mean_nld, instit.mean_nz, instit.mean_por,instit.mean_uk,instit.mean_usa)

data_scatter <- cbind(column1,column2,column3)
colnames(data_scatter) <- c("country", "govact", "instittrust")

write.csv(data_scatter,'data_scatter.csv')
data_scatterplot <- read.csv("data_scatter.csv")

par(pty = "s")
plot(govact~instittrust, col="lightblue", pch=19, cex=2, xlim=c(0,4), ylim=c(0,4), xlab="institutional trust", ylab="government actions", data=data_scatterplot)
text(govact~instittrust, labels=country,data=data_scatterplot, cex=0.9, font=2)




#####SUPPLEMENTARY MATERIALS#########

thr.mean_usa <-  mean(data_usa$severe_life,na.rm = TRUE)
thr.mean_uk <-  mean(data_uk$severe_life,na.rm = TRUE)
thr.mean_ita <-  mean(data_ita$severe_life,na.rm = TRUE)
thr.mean_bra <-  mean(data_bra$severe_life,na.rm = TRUE)
thr.mean_aus <-  mean(data_aus$severe_life,na.rm = TRUE)
thr.mean_nld <-  mean(data_nld$severe_life,na.rm = TRUE)
thr.mean_por <-  mean(data_por$severe_life,na.rm = TRUE)
thr.mean_ger <-  mean(data_ger$severe_life,na.rm = TRUE)
thr.mean_fra <-  mean(data_fra$severe_life,na.rm = TRUE)
thr.mean_fin <-  mean(data_fin$severe_life,na.rm = TRUE)
thr.mean_cro <-  mean(data_cro$severe_life,na.rm = TRUE)
thr.mean_nz <-  mean(data_nz$severe_life,na.rm = TRUE)

con.mean_usa <-  mean(data_usa$controlsense_avg,na.rm = TRUE)
con.mean_uk <-  mean(data_uk$controlsense_avg,na.rm = TRUE)
con.mean_ita <-  mean(data_ita$controlsense_avg,na.rm = TRUE)
con.mean_bra <-  mean(data_bra$controlsense_avg,na.rm = TRUE)
con.mean_aus <-  mean(data_aus$controlsense_avg,na.rm = TRUE)
con.mean_nld <-  mean(data_nld$controlsense_avg,na.rm = TRUE)
con.mean_por <-  mean(data_por$controlsense_avg,na.rm = TRUE)
con.mean_ger <-  mean(data_ger$controlsense_avg,na.rm = TRUE)
con.mean_fra <-  mean(data_fra$controlsense_avg,na.rm = TRUE)
con.mean_fin <-  mean(data_fin$controlsense_avg,na.rm = TRUE)
con.mean_cro <-  mean(data_cro$controlsense_avg,na.rm = TRUE)
con.mean_nz <-  mean(data_nz$controlsense_avg,na.rm = TRUE)

iso.mean_usa <-  mean(data_usa$socisol_avg,na.rm = TRUE)
iso.mean_uk <-  mean(data_uk$socisol_avg,na.rm = TRUE)
iso.mean_ita <-  mean(data_ita$socisol_avg,na.rm = TRUE)
iso.mean_bra <-  mean(data_bra$socisol_avg,na.rm = TRUE)
iso.mean_aus <-  mean(data_aus$socisol_avg,na.rm = TRUE)
iso.mean_nld <-  mean(data_nld$socisol_avg,na.rm = TRUE)
iso.mean_por <-  mean(data_por$socisol_avg,na.rm = TRUE)
iso.mean_ger <-  mean(data_ger$socisol_avg,na.rm = TRUE)
iso.mean_fra <-  mean(data_fra$socisol_avg,na.rm = TRUE)
iso.mean_fin <-  mean(data_fin$socisol_avg,na.rm = TRUE)
iso.mean_cro <-  mean(data_cro$socisol_avg,na.rm = TRUE)
iso.mean_nz <-  mean(data_nz$socisol_avg,na.rm = TRUE)

govcap.mean_usa <-  mean(data_usa$gov_capab,na.rm = TRUE)
govcap.mean_uk <-  mean(data_uk$gov_capab,na.rm = TRUE)
govcap.mean_ita <-  mean(data_ita$gov_capab,na.rm = TRUE)
govcap.mean_bra <-  mean(data_bra$gov_capab,na.rm = TRUE)
govcap.mean_aus <-  mean(data_aus$gov_capab,na.rm = TRUE)
govcap.mean_nld <-  mean(data_nld$gov_capab,na.rm = TRUE)
govcap.mean_por <-  mean(data_por$gov_capab,na.rm = TRUE)
govcap.mean_ger <-  mean(data_ger$gov_capab,na.rm = TRUE)
govcap.mean_fra <-  mean(data_fra$gov_capab,na.rm = TRUE)
govcap.mean_fin <-  mean(data_fin$gov_capab,na.rm = TRUE)
govcap.mean_cro <-  mean(data_cro$gov_capab,na.rm = TRUE)
govcap.mean_nz <-  mean(data_nz$gov_capab,na.rm = TRUE)

govben.mean_usa <-  mean(data_usa$gov_benevol,na.rm = TRUE)
govben.mean_uk <-  mean(data_uk$gov_benevol,na.rm = TRUE)
govben.mean_ita <-  mean(data_ita$gov_benevol,na.rm = TRUE)
govben.mean_bra <-  mean(data_bra$gov_benevol,na.rm = TRUE)
govben.mean_aus <-  mean(data_aus$gov_benevol,na.rm = TRUE)
govben.mean_nld <-  mean(data_nld$gov_benevol,na.rm = TRUE)
govben.mean_por <-  mean(data_por$gov_benevol,na.rm = TRUE)
govben.mean_ger <-  mean(data_ger$gov_benevol,na.rm = TRUE)
govben.mean_fra <-  mean(data_fra$gov_benevol,na.rm = TRUE)
govben.mean_fin <-  mean(data_fin$gov_benevol,na.rm = TRUE)
govben.mean_cro <-  mean(data_cro$gov_benevol,na.rm = TRUE)
govben.mean_nz <-  mean(data_nz$gov_benevol,na.rm = TRUE)

govint.mean_usa <-  mean(data_usa$gov_trust,na.rm = TRUE)
govint.mean_uk <-  mean(data_uk$gov_trust,na.rm = TRUE)
govint.mean_ita <-  mean(data_ita$gov_trust,na.rm = TRUE)
govint.mean_bra <-  mean(data_bra$gov_trust,na.rm = TRUE)
govint.mean_aus <-  mean(data_aus$gov_trust,na.rm = TRUE)
govint.mean_nld <-  mean(data_nld$gov_trust,na.rm = TRUE)
govint.mean_por <-  mean(data_por$gov_trust,na.rm = TRUE)
govint.mean_ger <-  mean(data_ger$gov_trust,na.rm = TRUE)
govint.mean_fra <-  mean(data_fra$gov_trust,na.rm = TRUE)
govint.mean_fin <-  mean(data_fin$gov_trust,na.rm = TRUE)
govint.mean_cro <-  mean(data_cro$gov_trust,na.rm = TRUE)
govint.mean_nz <-  mean(data_nz$gov_trust,na.rm = TRUE)

gov.mean_capab <-  mean(data_usa$gov_capab,na.rm = TRUE)
gov.mean_benev <-  mean(data_usa$gov_benevol,na.rm = TRUE)
gov.mean_trust <-  mean(data_usa$gov_trust,na.rm = TRUE)
state.mean_capab <-  mean(data_usa$govstate_capab,na.rm = TRUE)
state.mean_benev <-  mean(data_usa$govstate_benevol,na.rm = TRUE)
state.mean_trust <-  mean(data_usa$govstate_trust,na.rm = TRUE)

##Figure SM3.1##


pthr_usa <- qplot(severe_life, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                  main="United States", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))
pthr_usa <- pthr_usa + geom_vline(aes(xintercept=thr.mean_usa),
                                  color="blue", linetype="dashed", size=1)
data_usa_thr <- pthr_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_usa <- data_usa_thr + xlim("1","2","3","4","5")
png(file="thr_usa.png", width=932, height=582)
plot(plotthr_usa)
dev.off()



pthr_uk <- qplot(severe_life, data=data_uk, fill=countryres, alpha=I(.5), binwidth=1,
                 main="United Kingdom", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_uk <- pthr_uk + geom_vline(aes(xintercept=thr.mean_uk),
                                color="blue", linetype="dashed", size=1)
data_uk_thr <- pthr_uk + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20, face="plain"), 
                               title=element_text(size=25, face="bold"),
                               panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_uk <- data_uk_thr + xlim("1","2","3","4","5")
png(file="thr_uk.png", width=932, height=582)
plot(plotthr_uk)
dev.off()

pthr_ita <- qplot(severe_life, data=data_ita, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Italy", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_ita <- pthr_ita + geom_vline(aes(xintercept=thr.mean_ita),
                                  color="blue", linetype="dashed", size=1)
data_ita_thr <- pthr_ita + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_ita <- data_ita_thr + xlim("1","2","3","4","5")
png(file="thr_ita.png", width=932, height=582)
plot(plotthr_ita)
dev.off()



pthr_bra <- qplot(severe_life, data=data_bra, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Brazil", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_bra <- pthr_bra + geom_vline(aes(xintercept=thr.mean_bra),
                                  color="blue", linetype="dashed", size=1)
data_bra_thr <- pthr_bra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_bra <- data_bra_thr + xlim("1","2","3","4","5")
png(file="thr_bra.png", width=932, height=582)
plot(plotthr_bra)
dev.off()


pthr_aus <- qplot(severe_life, data=data_aus, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Australia", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_aus <- pthr_aus + geom_vline(aes(xintercept=thr.mean_aus),
                                  color="blue", linetype="dashed", size=1)
data_aus_thr <- pthr_aus + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_aus <- data_aus_thr + xlim("1","2","3","4","5")
png(file="thr_aus.png", width=932, height=582)
plot(plotthr_aus)
dev.off()


pthr_nld <- qplot(severe_life, data=data_nld, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Netherlands", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_nld <- pthr_nld + geom_vline(aes(xintercept=thr.mean_nld),
                                  color="blue", linetype="dashed", size=1)
data_nld_thr <- pthr_nld + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_nld <- data_nld_thr + xlim("1","2","3","4","5")
png(file="thr_nld.png", width=932, height=582)
plot(plotthr_nld)
dev.off()



pthr_por <- qplot(severe_life, data=data_por, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Portugal", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_por <- pthr_por + geom_vline(aes(xintercept=thr.mean_por),
                                  color="blue", linetype="dashed", size=1)
data_por_thr <- pthr_por + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_por <- data_por_thr + xlim("1","2","3","4","5")
png(file="thr_por.png", width=932, height=582)
plot(plotthr_por)
dev.off()


pthr_ger <- qplot(severe_life, data=data_ger, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Germany", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_ger <- pthr_ger + geom_vline(aes(xintercept=thr.mean_ger),
                                  color="blue", linetype="dashed", size=1)
data_ger_thr <- pthr_ger + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_ger <- data_ger_thr + xlim("1","2","3","4","5")
png(file="thr_ger.png", width=932, height=582)
plot(plotthr_ger)
dev.off()


pthr_fra <- qplot(severe_life, data=data_fra, fill=countryres, alpha=I(.5), binwidth=1,
                  main="France", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_fra <- pthr_fra + geom_vline(aes(xintercept=thr.mean_fra),
                                  color="blue", linetype="dashed", size=1)
data_fra_thr <- pthr_fra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_fra <- data_fra_thr + xlim("1","2","3","4","5")
png(file="thr_fra.png", width=932, height=582)
plot(plotthr_fra)
dev.off()


pthr_fin <- qplot(severe_life, data=data_fin, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Finland", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_fin <- pthr_fin + geom_vline(aes(xintercept=thr.mean_fin),
                                  color="blue", linetype="dashed", size=1)
data_fin_thr <- pthr_fin + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_fin <- data_fin_thr + xlim("1","2","3","4","5")
png(file="thr_fin.png", width=932, height=582)
plot(plotthr_fin)
dev.off()


pthr_cro <- qplot(severe_life, data=data_cro, fill=countryres, alpha=I(.5), binwidth=1,
                  main="Croatia", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_cro <- pthr_cro + geom_vline(aes(xintercept=thr.mean_cro),
                                  color="blue", linetype="dashed", size=1)
data_cro_thr <- pthr_cro + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20, face="plain"), 
                                 title=element_text(size=25, face="bold"),
                                 panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_cro <- data_cro_thr + xlim("1","2","3","4","5")
png(file="thr_cro.png", width=932, height=582)
plot(plotthr_cro)
dev.off()


pthr_nz <- qplot(severe_life, data=data_nz, fill=countryres, alpha=I(.5), binwidth=1,
                 main="New Zealand", xlab="Perceived Threat", ylab="Frequency", colour = I("black"))

pthr_nz <- pthr_nz + geom_vline(aes(xintercept=thr.mean_nz),
                                color="blue", linetype="dashed", size=1)
data_nz_thr <- pthr_nz + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20, face="plain"), 
                               title=element_text(size=25, face="bold"),
                               panel.grid.minor = element_blank(), panel.grid.major = element_blank())

plotthr_nz <- data_nz_thr + xlim("1","2","3","4","5")
png(file="thr_nz.png", width=932, height=582)
plot(plotthr_nz)
dev.off()


(pthr_aus + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_bra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_cro + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_fin  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_fra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_ger + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_ita + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_nld  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_nz + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_por + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_uk + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5")) + 
  (pthr_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4","5"))


##Figure SM3.2##


pcon_usa <- qplot(controlsense_avg, data=data_usa, fill=countryres, geom="density", alpha=I(.5),
                  main="United States", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_usa <- pcon_usa + geom_vline(aes(xintercept=con.mean_usa),
                                  color="blue", linetype="dashed", size=1)
data_usa_con <- pcon_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="con_usa.png", width=932, height=582)
plot(data_usa_con)
dev.off()


pcon_uk <- qplot(controlsense_avg, data=data_uk, fill=countryres, geom="density", alpha=I(.5),
                 main="United Kingdom", xlab="Sense of Control", ylab="Density",  
                 xlim=c(-10,10), ylim=c(0,0.15))
pcon_uk <- pcon_uk + geom_vline(aes(xintercept=con.mean_uk),
                                color="blue", linetype="dashed", size=1)
data_uk_con <- pcon_uk + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="con_uk.png", width=932, height=582)
plot(data_uk_con)
dev.off()


pcon_ita <- qplot(controlsense_avg, data=data_ita, fill=countryres, geom="density", alpha=I(.5),
                  main="Italy", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_ita <- pcon_ita + geom_vline(aes(xintercept=con.mean_ita),
                                  color="blue", linetype="dashed", size=1)
data_ita_con <- pcon_ita + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_ita.png", width=932, height=582)
plot(data_ita_con)
dev.off()

pcon_bra <- qplot(controlsense_avg, data=data_bra, fill=countryres, geom="density", alpha=I(.5),
                  main="Brazil", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_bra <- pcon_bra + geom_vline(aes(xintercept=con.mean_bra),
                                  color="blue", linetype="dashed", size=1)
data_bra_con <- pcon_bra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_bra.png", width=932, height=582)
plot(data_bra_con)
dev.off()

pcon_aus <- qplot(controlsense_avg, data=data_aus, fill=countryres, geom="density", alpha=I(.5),
                  main="Australia", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_aus <- pcon_aus + geom_vline(aes(xintercept=con.mean_aus),
                                  color="blue", linetype="dashed", size=1)
data_aus_con <- pcon_aus + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_aus.png", width=932, height=582)
plot(data_aus_con)
dev.off()

pcon_nld <- qplot(controlsense_avg, data=data_nld, fill=countryres, geom="density", alpha=I(.5),
                  main="Netherlands", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_nld <- pcon_nld + geom_vline(aes(xintercept=con.mean_nld),
                                  color="blue", linetype="dashed", size=1)
data_nld_con <- pcon_nld + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_nld.png", width=932, height=582)
plot(data_nld_con)
dev.off()

pcon_por <- qplot(controlsense_avg, data=data_por, fill=countryres, geom="density", alpha=I(.5),
                  main="Portugal", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_por <- pcon_por + geom_vline(aes(xintercept=con.mean_por),
                                  color="blue", linetype="dashed", size=1)
data_por_con <- pcon_por + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_por.png", width=932, height=582)
plot(data_por_con)
dev.off()

pcon_ger <- qplot(controlsense_avg, data=data_ger, fill=countryres, geom="density", alpha=I(.5),
                  main="Germany", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_ger <- pcon_ger + geom_vline(aes(xintercept=con.mean_ger),
                                  color="blue", linetype="dashed", size=1)
data_ger_con <- pcon_ger + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_ger.png", width=932, height=582)
plot(data_ger_con)
dev.off()

pcon_fra <- qplot(controlsense_avg, data=data_fra, fill=countryres, geom="density", alpha=I(.5),
                  main="France", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_fra <- pcon_fra + geom_vline(aes(xintercept=con.mean_fra),
                                  color="blue", linetype="dashed", size=1)
data_fra_con <- pcon_fra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_fra.png", width=932, height=582)
plot(data_fra_con)
dev.off()

pcon_fin <- qplot(controlsense_avg, data=data_fin, fill=countryres, geom="density", alpha=I(.5),
                  main="Finland", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_fin <- pcon_fin + geom_vline(aes(xintercept=con.mean_fin),
                                  color="blue", linetype="dashed", size=1)
data_fin_con <- pcon_fin + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_fin.png", width=932, height=582)
plot(data_fin_con)
dev.off()


pcon_cro <- qplot(controlsense_avg, data=data_cro, fill=countryres, geom="density", alpha=I(.5),
                  main="Croatia", xlab="Sense of Control", ylab="Density",  
                  xlim=c(-10,10), ylim=c(0,0.15))
pcon_cro <- pcon_cro + geom_vline(aes(xintercept=con.mean_cro),
                                  color="blue", linetype="dashed", size=1)
data_cro_con <- pcon_cro + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_cro.png", width=932, height=582)
plot(data_cro_con)
dev.off()

pcon_nz <- qplot(controlsense_avg, data=data_nz, fill=countryres, geom="density", alpha=I(.5),
                 main="New Zealand", xlab="Sense of Control", ylab="Density",  
                 xlim=c(-10,10), ylim=c(0,0.15))
pcon_nz <- pcon_nz + geom_vline(aes(xintercept=con.mean_nz),
                                color="blue", linetype="dashed", size=1)
data_nz_con <- pcon_nz + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20, face="plain"), title=element_text(size=25,face="bold"))
#png(file="con_NZ.png", width=932, height=582)
plot(data_nz_con)
dev.off()


(pcon_aus + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_bra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_cro + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_fin  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_fra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_ger + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_ita + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_nld  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_nz + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_por + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_uk + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (pcon_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()))

##Figure SM3.3###



piso_usa <- qplot(socisol_avg, data=data_usa, fill=countryres, geom="density", alpha=I(.5),
                  main="United States", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_usa <- piso_usa + geom_vline(aes(xintercept=iso.mean_usa),
                                  color="blue", linetype="dashed", size=1)
data_usa_iso <- piso_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_usa.png", width=932, height=582)
plot(data_usa_iso)
dev.off()


piso_uk <- qplot(socisol_avg, data=data_uk, fill=countryres, geom="density", alpha=I(.5),
                 main="United Kingdom", xlab="Social Isolation", ylab="Density",  
                 xlim=c(0,4))
piso_uk <- piso_uk + geom_vline(aes(xintercept=iso.mean_uk),
                                color="blue", linetype="dashed", size=1)
data_uk_iso <- piso_uk + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_uk.png", width=932, height=582)
plot(data_uk_iso)
dev.off()


piso_ita <- qplot(socisol_avg, data=data_ita, fill=countryres, geom="density", alpha=I(.5),
                  main="Italy", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_ita <- piso_ita + geom_vline(aes(xintercept=iso.mean_ita),
                                  color="blue", linetype="dashed", size=1)
data_ita_iso <- piso_ita + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_ita.png", width=932, height=582)
plot(data_ita_iso)
dev.off()

piso_bra <- qplot(socisol_avg, data=data_bra, fill=countryres, geom="density", alpha=I(.5),
                  main="Brazil", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_bra <- piso_bra + geom_vline(aes(xintercept=iso.mean_bra),
                                  color="blue", linetype="dashed", size=1)
data_bra_iso <- piso_bra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_bra.png", width=932, height=582)
plot(data_bra_iso)
dev.off()

piso_aus <- qplot(socisol_avg, data=data_aus, fill=countryres, geom="density", alpha=I(.5),
                  main="Australia", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_aus <- piso_aus + geom_vline(aes(xintercept=iso.mean_aus),
                                  color="blue", linetype="dashed", size=1)
data_aus_iso <- piso_aus + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_aus.png", width=932, height=582)
plot(data_aus_iso)
dev.off()

piso_nld <- qplot(socisol_avg, data=data_nld, fill=countryres, geom="density", alpha=I(.5),
                  main="Netherlands", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_nld <- piso_nld + geom_vline(aes(xintercept=iso.mean_nld),
                                  color="blue", linetype="dashed", size=1)
data_nld_iso <- piso_nld + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_nld.png", width=932, height=582)
plot(data_nld_iso)
dev.off()

piso_por <- qplot(socisol_avg, data=data_por, fill=countryres, geom="density", alpha=I(.5),
                  main="Portugal", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_por <- piso_por + geom_vline(aes(xintercept=iso.mean_por),
                                  color="blue", linetype="dashed", size=1)
data_por_iso <- piso_por + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_por.png", width=932, height=582)
plot(data_por_iso)
dev.off()

piso_ger <- qplot(socisol_avg, data=data_ger, fill=countryres, geom="density", alpha=I(.5),
                  main="Germany", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_ger <- piso_ger + geom_vline(aes(xintercept=iso.mean_ger),
                                  color="blue", linetype="dashed", size=1)
data_ger_iso <- piso_ger + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_ger.png", width=932, height=582)
plot(data_ger_iso)
dev.off()

piso_fra <- qplot(socisol_avg, data=data_fra, fill=countryres, geom="density", alpha=I(.5),
                  main="France", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_fra <- piso_fra + geom_vline(aes(xintercept=iso.mean_fra),
                                  color="blue", linetype="dashed", size=1)
data_fra_iso <- piso_fra + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_fra.png", width=932, height=582)
plot(data_fra_iso)
dev.off()

piso_fin <- qplot(socisol_avg, data=data_fin, fill=countryres, geom="density", alpha=I(.5),
                  main="Finland", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_fin <- piso_fin + geom_vline(aes(xintercept=iso.mean_fin),
                                  color="blue", linetype="dashed", size=1)
data_fin_iso <- piso_fin + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_fin.png", width=932, height=582)
plot(data_fin_iso)
dev.off()


piso_cro <- qplot(socisol_avg, data=data_cro, fill=countryres, geom="density", alpha=I(.5),
                  main="Croatia", xlab="Social Isolation", ylab="Density",  
                  xlim=c(0,4))
piso_cro <- piso_cro + geom_vline(aes(xintercept=iso.mean_cro),
                                  color="blue", linetype="dashed", size=1)
data_cro_iso <- piso_cro + theme(legend.position = "none", axis.text=element_text(size=15),
                                 axis.title=element_text(size=20 , face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_cro.png", width=932, height=582)
plot(data_cro_iso)
dev.off()

piso_nz <- qplot(socisol_avg, data=data_nz, fill=countryres, geom="density", alpha=I(.5),
                 main="New Zealand", xlab="Social Isolation", ylab="Density",  
                 xlim=c(0,4))
piso_nz <- piso_nz + geom_vline(aes(xintercept=iso.mean_nz),
                                color="blue", linetype="dashed", size=1)
data_nz_iso <- piso_nz + theme(legend.position = "none", axis.text=element_text(size=15),
                               axis.title=element_text(size=20, face="plain"), title=element_text(size=25,face="bold"))
png(file="iso_NZ.png", width=932, height=582)
plot(data_nz_iso)
dev.off()


(piso_aus + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_bra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_cro + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_fin  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_fra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_ger + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_ita + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_nld  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_nz + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_por + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_uk + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank())) + 
  (piso_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()))

##Figure SM10.1##



p_ppe_usa <- qplot(sit_occup_ppe, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                   main="United States", xlab="Access to PPE", ylab="Frequency", colour = I("black"))
p_ppe_usa <- p_ppe_usa + geom_vline(aes(xintercept=ppe.mean_usa),
                                    color="blue", linetype="dashed", size=1)
data_usa_ppe <- p_ppe_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_ppe_uk <- qplot(sit_occup_ppe, data=data_uk, fill=countryres, alpha=I(.5), binwidth=1,
                  main="United Kingdom", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_uk <- p_ppe_uk + geom_vline(aes(xintercept=ppe.mean_uk),
                                  color="blue", linetype="dashed", size=1)
data_uk_ppe <- p_ppe_uk + theme(legend.position = "none", axis.text=element_text(size=15),
                                axis.title=element_text(size=20, face="plain"), 
                                title=element_text(size=25, face="bold"),
                                panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_ppe_ita <- qplot(sit_occup_ppe, data=data_ita, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Italy", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_ita <- p_ppe_ita + geom_vline(aes(xintercept=ppe.mean_ita),
                                    color="blue", linetype="dashed", size=1)
data_ita_ppe <- p_ppe_ita + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())


p_ppe_bra <- qplot(sit_occup_ppe, data=data_bra, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Brazil", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_bra <- p_ppe_bra + geom_vline(aes(xintercept=ppe.mean_bra),
                                    color="blue", linetype="dashed", size=1)
data_bra_ppe <- p_ppe_bra + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())


p_ppe_aus <- qplot(sit_occup_ppe, data=data_aus, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Australia", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_aus <- p_ppe_aus + geom_vline(aes(xintercept=ppe.mean_aus),
                                    color="blue", linetype="dashed", size=1)
data_aus_ppe <- p_ppe_aus + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())



p_ppe_nld <- qplot(sit_occup_ppe, data=data_nld, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Netherlands", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_nld <- p_ppe_nld + geom_vline(aes(xintercept=ppe.mean_nld),
                                    color="blue", linetype="dashed", size=1)
data_nld_ppe <- p_ppe_nld + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())



p_ppe_por <- qplot(sit_occup_ppe, data=data_por, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Portugal", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_por <- p_ppe_por + geom_vline(aes(xintercept=ppe.mean_por),
                                    color="blue", linetype="dashed", size=1)
data_por_ppe <- p_ppe_por + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())



p_ppe_ger <- qplot(sit_occup_ppe, data=data_ger, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Germany", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_ger <- p_ppe_ger + geom_vline(aes(xintercept=ppe.mean_ger),
                                    color="blue", linetype="dashed", size=1)
data_ger_ppe <- p_ppe_ger + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_ppe_fra <- qplot(sit_occup_ppe, data=data_fra, fill=countryres, alpha=I(.5), binwidth=1,
                   main="France", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_fra <- p_ppe_fra + geom_vline(aes(xintercept=ppe.mean_fra),
                                    color="blue", linetype="dashed", size=1)
data_fra_ppe <- p_ppe_fra + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_ppe_fin <- qplot(sit_occup_ppe, data=data_fin, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Finland", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_fin <- p_ppe_fin + geom_vline(aes(xintercept=ppe.mean_fin),
                                    color="blue", linetype="dashed", size=1)
data_fin_ppe <- p_ppe_fin + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())


p_ppe_cro <- qplot(sit_occup_ppe, data=data_cro, fill=countryres, alpha=I(.5), binwidth=1,
                   main="Croatia", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_cro <- p_ppe_cro + geom_vline(aes(xintercept=ppe.mean_cro),
                                    color="blue", linetype="dashed", size=1)
data_cro_ppe <- p_ppe_cro + theme(legend.position = "none", axis.text=element_text(size=15),
                                  axis.title=element_text(size=20, face="plain"), 
                                  title=element_text(size=25, face="bold"),
                                  panel.grid.minor = element_blank(), panel.grid.major = element_blank())


p_ppe_nz <- qplot(sit_occup_ppe, data=data_nz, fill=countryres, alpha=I(.5), binwidth=1,
                  main="New Zealand", xlab="Access to PPE", ylab="Frequency", colour = I("black"))

p_ppe_nz <- p_ppe_nz + geom_vline(aes(xintercept=ppe.mean_nz),
                                  color="blue", linetype="dashed", size=1)
data_nz_ppe <- p_ppe_nz + theme(legend.position = "none", axis.text=element_text(size=15),
                                axis.title=element_text(size=20, face="plain"), 
                                title=element_text(size=25, face="bold"),
                                panel.grid.minor = element_blank(), panel.grid.major = element_blank())


(p_ppe_aus + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_bra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_cro + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_fin  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_fra + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_ger + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_ita + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_nld  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_nz + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_por + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_uk + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) ) + 
  (p_ppe_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) )

##Figure SM12.1##


govcab_usa <- qplot(gov_capab, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                    main="Federal Government", xlab="Capability", ylab="Frequency", colour = I("black"))
govcab_usa <- govcab_usa + geom_vline(aes(xintercept=gov.mean_capab),
                                      color="blue", linetype="dashed", size=1)
p_govcab_usa <- govcab_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                   axis.title=element_text(size=20, face="plain"), 
                                   title=element_text(size=23, face="bold"),
                                   panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_govcab_usa <- p_govcab_usa + xlim("1","2","3","4")

#png(file="govfed_capab_usa.png", width=932, height=582)
plot(p_govcab_usa)
#dev.off()


govbenev_usa <- qplot(gov_benevol, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                      main="Federal Government", xlab="Benevolence", ylab="Frequency", colour = I("black"))
govbenev_usa <- govbenev_usa + geom_vline(aes(xintercept=gov.mean_benev),
                                          color="blue", linetype="dashed", size=1)
p_govbenev_usa <- govbenev_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                       axis.title=element_text(size=20, face="plain"), 
                                       title=element_text(size=23, face="bold"),
                                       panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_govbenev_usa <- p_govbenev_usa + xlim("1","2","3","4")

#png(file="govfed_benev_usa.png", width=932, height=582)
plot(p_govbenev_usa)
#dev.off()


govtrust_usa <- qplot(gov_trust, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                      main="Federal Government", xlab="Integrity", ylab="Frequency", colour = I("black"))
govtrust_usa <- govtrust_usa + geom_vline(aes(xintercept=gov.mean_trust),
                                          color="blue", linetype="dashed", size=1)
p_govtrust_usa <- govtrust_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                       axis.title=element_text(size=20, face="plain"), 
                                       title=element_text(size=23, face="bold"),
                                       panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_govtrust_usa <- p_govtrust_usa + xlim("1","2","3","4")

#png(file="govfed_trust_usa.png", width=932, height=582)
plot(p_govtrust_usa)
#dev.off()



statecab_usa <- qplot(govstate_capab, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                      main="State Government", xlab="Capability", ylab="Frequency", colour = I("black"))
statecab_usa <- statecab_usa + geom_vline(aes(xintercept=state.mean_capab),
                                          color="blue", linetype="dashed", size=1)
p_statecab_usa <- statecab_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                       axis.title=element_text(size=20, face="plain"), 
                                       title=element_text(size=23, face="bold"),
                                       panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_statecab_usa <- p_statecab_usa + xlim("1","2","3","4")

#png(file="govfed_capab_usa.png", width=932, height=582)
plot(p_statecab_usa)
#dev.off()


statebenev_usa <- qplot(govstate_benevol, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                        main="State Government", xlab="Benevolence", ylab="Frequency", colour = I("black"))
statebenev_usa <- statebenev_usa + geom_vline(aes(xintercept=state.mean_benev),
                                              color="blue", linetype="dashed", size=1)
p_statebenev_usa <- statebenev_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                           axis.title=element_text(size=20, face="plain"), 
                                           title=element_text(size=23, face="bold"),
                                           panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_statebenev_usa <- p_statebenev_usa + xlim("1","2","3","4")

#png(file="govfed_benev_usa.png", width=932, height=582)
plot(p_statebenev_usa)
#dev.off()


statetrust_usa <- qplot(govstate_trust, data=data_usa, fill=countryres, alpha=I(.5), binwidth=1,
                        main="State Government", xlab="Integrity", ylab="Frequency", colour = I("black"))
statetrust_usa <- statetrust_usa + geom_vline(aes(xintercept=state.mean_trust),
                                              color="blue", linetype="dashed", size=1)
p_statetrust_usa <- statetrust_usa + theme(legend.position = "none", axis.text=element_text(size=15),
                                           axis.title=element_text(size=20, face="plain"), 
                                           title=element_text(size=23, face="bold"),
                                           panel.grid.minor = element_blank(), panel.grid.major = element_blank())

p_statetrust_usa <- p_statetrust_usa + xlim("1","2","3","4")

#png(file="govfed_trust_usa.png", width=932, height=582)
plot(p_statetrust_usa)
#dev.off()

(govcab_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4")) + 
  (govbenev_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4")) + 
  (govtrust_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4")) + 
  (statecab_usa  + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4")) + 
  (statebenev_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4")) + 
  (statetrust_usa + theme(legend.position = "none", panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + xlim("1","2","3","4"))


##Figure SM13.1##



compinstit_usa <- data.frame(data_usa$gov_capab, data_usa$gov_benevol, data_usa$gov_trust)
m_compinstit_usa<- melt(compinstit_usa)
p_compinstit_usa <- ggplot(m_compinstit_usa,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_usa), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_usa), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_usa), color=" lightsteelblue3", linetype="dashed", size=1)
usa_compinstit <- p_compinstit_usa + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("United States") 

compinstit_uk <- data.frame(data_uk$gov_capab, data_uk$gov_benevol, data_uk$gov_trust)
m_compinstit_uk<- melt(compinstit_uk)
p_compinstit_uk <- ggplot(m_compinstit_uk,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_uk), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_uk), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_uk), color=" lightsteelblue3", linetype="dashed", size=1)
uk_compinstit <- p_compinstit_uk + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("United Kingdom") 

compinstit_aus <- data.frame(data_aus$gov_capab, data_aus$gov_benevol, data_aus$gov_trust)
m_compinstit_aus<- melt(compinstit_aus)
p_compinstit_aus <- ggplot(m_compinstit_aus,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_aus), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_aus), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_aus), color=" lightsteelblue3", linetype="dashed", size=1)
aus_compinstit <- p_compinstit_aus + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Australia") 

compinstit_bra <- data.frame(data_bra$gov_capab, data_bra$gov_benevol, data_bra$gov_trust)
m_compinstit_bra<- melt(compinstit_bra)
p_compinstit_bra <- ggplot(m_compinstit_bra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_bra), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_bra), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_bra), color=" lightsteelblue3", linetype="dashed", size=1)
bra_compinstit <- p_compinstit_bra + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Brazil") 

compinstit_cro <- data.frame(data_cro$gov_capab, data_cro$gov_benevol, data_cro$gov_trust)
m_compinstit_cro<- melt(compinstit_cro)
p_compinstit_cro <- ggplot(m_compinstit_cro,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_cro), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_cro), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_cro), color=" lightsteelblue3", linetype="dashed", size=1)
cro_compinstit <- p_compinstit_cro + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Croatia") 

compinstit_fin <- data.frame(data_fin$gov_capab, data_fin$gov_benevol, data_fin$gov_trust)
m_compinstit_fin<- melt(compinstit_fin)
p_compinstit_fin <- ggplot(m_compinstit_fin,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_fin), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_fin), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_fin), color=" lightsteelblue3", linetype="dashed", size=1)
fin_compinstit <- p_compinstit_fin + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Finland") 

compinstit_fra <- data.frame(data_fra$gov_capab, data_fra$gov_benevol, data_fra$gov_trust)
m_compinstit_fra<- melt(compinstit_fra)
p_compinstit_fra <- ggplot(m_compinstit_fra,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_fra), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_fra), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_fra), color=" lightsteelblue3", linetype="dashed", size=1)
fra_compinstit <- p_compinstit_fra + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("France") 

compinstit_ger <- data.frame(data_ger$gov_capab, data_ger$gov_benevol, data_ger$gov_trust)
m_compinstit_ger<- melt(compinstit_ger)
p_compinstit_ger <- ggplot(m_compinstit_ger,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_ger), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_ger), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_ger), color=" lightsteelblue3", linetype="dashed", size=1)
ger_compinstit <- p_compinstit_ger + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Germany") 

compinstit_ita <- data.frame(data_ita$gov_capab, data_ita$gov_benevol, data_ita$gov_trust)
m_compinstit_ita<- melt(compinstit_ita)
p_compinstit_ita <- ggplot(m_compinstit_ita,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_ita), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_ita), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_ita), color=" lightsteelblue3", linetype="dashed", size=1)
ita_compinstit <- p_compinstit_ita + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Italy") 

compinstit_nld <- data.frame(data_nld$gov_capab, data_nld$gov_benevol, data_nld$gov_trust)
m_compinstit_nld<- melt(compinstit_nld)
p_compinstit_nld <- ggplot(m_compinstit_nld,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_nld), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_nld), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_nld), color=" lightsteelblue3", linetype="dashed", size=1)
nld_compinstit <- p_compinstit_nld + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Netherlands") 

compinstit_nz <- data.frame(data_nz$gov_capab, data_nz$gov_benevol, data_nz$gov_trust)
m_compinstit_nz<- melt(compinstit_nz)
p_compinstit_nz <- ggplot(m_compinstit_nz,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_nz), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_nz), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_nz), color=" lightsteelblue3", linetype="dashed", size=1)
nz_compinstit <- p_compinstit_nz + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("New Zealand") 

compinstit_por <- data.frame(data_por$gov_capab, data_por$gov_benevol, data_por$gov_trust)
m_compinstit_por<- melt(compinstit_por)
p_compinstit_por <- ggplot(m_compinstit_por,aes(x=value, fill=variable)) + geom_density(alpha=0.25) + geom_vline(aes(xintercept= govcap.mean_por), color="pink2", linetype="dashed", size=1) + geom_vline(aes(xintercept= govben.mean_por), color=" palegreen3", linetype="dashed", size=1) + geom_vline(aes(xintercept= govint.mean_por), color=" lightsteelblue3", linetype="dashed", size=1)
por_compinstit <- p_compinstit_por + mytheme + scale_fill_discrete(labels = c("capability", "benevolence", "integrity")) + xlab("score") + ggtitle("Portugal") 


por_compinstit_test <- p_compinstit_por + themelegend + scale_fill_discrete(labels = c("government capability", "government benevolence", "government integrity")) + xlab("score") + ggtitle("Portugal") 

(aus_compinstit + bra_compinstit + cro_compinstit + fin_compinstit + fra_compinstit + ger_compinstit + ita_compinstit + nld_compinstit + nz_compinstit + por_compinstit + uk_compinstit + usa_compinstit)

