####code for warming trial experiment####
setwd("/Users/syuan-jyunsun/Library/CloudStorage/Dropbox/paper/new lab paper/Sun Lab dropbox/data/warming exp")
setwd("/Users/sun/Library/CloudStorage/Dropbox/paper/new lab paper/Sun Lab dropbox/data/warming exp")
library(lme4)
library(car)
library(dplyr)
library(emmeans)
library(ggplot2)
library(ggpubr)

data=read.csv("warming trial data.csv")
data=data[data$Notes!="relatives",]
data$tr <- factor(data$tr, levels=c("control", " +2°C", " +4°C"))

####probability of brood ball success####
model=glmer(ball_success~tr+carc.wt+(1|male)+(1|female),family=binomial,data=data)
Anova(model,type=3)
a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_success = mean(ball_success),
    se_success = sd(ball_success)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

success <- ggplot(data, aes(x = tr, y = ball_success,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_success, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_success - se_success, ymax = mean_success + se_success, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Probability of carcass preparation") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,1))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(success)

####carcass preparation time####
data_ball=data[data$ball_success=="1",]
model=glmer(log(ball+1)~tr+carc.wt+(1|male)+(1|female),family=gaussian,data=data_ball)
Anova(model,type=3)

a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

summary_data <- data_ball %>%
  group_by(tr) %>%
  summarise(
    mean_ball= mean(ball),
    se_ball = sd(ball)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

ball <- ggplot(data_ball, aes(x = tr, y = ball,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_ball, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_ball - se_ball, ymax = mean_ball + se_ball, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Time until carcass preparation (days)") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,4))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(ball)

ggarrange(success,ball, labels= c("(a)", "(b)"),ncol = 2, nrow = 1,widths=c(1,1),common.legend = F)

####egg laying time####
data_egg=data[data$clutchsize!="0",]
model=glmer(log(egg+1)~tr+carc.wt+(1|male)+(1|female),family=gaussian,data=data_egg)
Anova(model,type=3)
a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

summary_data <- data_egg %>%
  group_by(tr) %>%
  summarise(
    mean_egg= mean(egg),
    se_egg = sd(egg)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

egg <- ggplot(data_egg, aes(x = tr, y = egg,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_egg, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_egg - se_egg, ymax = mean_egg + se_egg, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Time until first egg (days)") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,4))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(egg)

####clutch size####
model=glmer(clutchsize~tr+carc.wt+(1|male)+(1|female),family=poisson,data=data)
Anova(model,type=3)

#test for overdispersion
sum(resid(model, type = "pearson")^2)/model$df.resid

a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

#plot
summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_clutchsize = mean(clutchsize),
    se_clutchsize = sd(clutchsize)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

clutchsize <- ggplot(data, aes(x = tr, y = clutchsize,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_clutchsize, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_clutchsize - se_clutchsize, ymax = mean_clutchsize + se_clutchsize, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Clutch size") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,50))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(clutchsize)

####probability of egg hatching success####
#data not including +4C and include only those laid eggs
data1=data[data$tr!=" +4°C",]
data2=data1[data1$clutchsize!="0",]#those have laid eggs

model=glmer(hatching_success~tr+carc.wt+(1|male)+(1|female),family=binomial,data=data2)
Anova(model,type=3)

data3=data[data$clutchsize!="0",]#those have laid eggs
summary_data <- data3 %>%
  group_by(tr) %>%
  summarise(
    mean_success = mean(hatching_success),
    se_success = sd(hatching_success)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

hatching_success <- ggplot(data3, aes(x = tr, y = hatching_success,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_success, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_success - se_success, ymax = mean_success + se_success, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Probability of egg hatching") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,1))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(hatching_success)

####larva hatching time####
data_larva=data[data$hatching_success!="0",]
model=glmer(log(larva+1)~tr+carc.wt+(1|male)+(1|female),family=gaussian,data=data_larva)
Anova(model,type=3)
a=emmeans (model,  ~ tr, adjust="tukey")
pairs(a)

summary_data <- data_larva %>%
  group_by(tr) %>%
  summarise(
    mean_larva= mean(larva),
    se_larva = sd(larva)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61")

larva <- ggplot(data_larva, aes(x = tr, y = larva,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_larva, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_larva - se_larva, ymax = mean_larva + se_larva, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Time until the first larvae hatched (days)") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(4,6.5))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(larva)


####probability of breeding success####
data1=data[data$tr!=" +4°C",]
model=glmer(success~tr+carc.wt+(1|male)+(1|female),family=binomial,data=data1)
Anova(model,type=3)

#success
summary_data <- data %>%
  group_by(tr) %>%
  summarise(
    mean_success = mean(success),
    se_success = sd(success)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61", "#E4191C")

success <- ggplot(data, aes(x = tr, y = success,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_success, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_success - se_success, ymax = mean_success + se_success, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Probability of breeding success") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,1))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(success)

####brood size####
data_brood=data1[data1$hatching_success!="0",]

model=glmer(broodsize~tr+carc.wt+(1|male)+(1|female),family=poisson,data=data_brood)
Anova(model,type=3)

#broodsize
summary_data <- data_brood %>%
  group_by(tr) %>%
  summarise(
    mean_broodsize = mean(broodsize),
    se_broodsize = sd(broodsize)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61")

broodsize <- ggplot(data_brood, aes(x = tr, y = broodsize,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_broodsize, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_broodsize - se_broodsize, ymax = mean_broodsize + se_broodsize, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Brood size") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,50))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")


print(broodsize)

####brood mass####
model=glmer(log(broodmass+1)~tr+carc.wt+(1|male)+(1|female),family=gaussian,data=data_brood)
Anova(model,type=3)

summary_data <- data_brood %>%
  group_by(tr) %>%
  summarise(
    mean_broodmass = mean(broodmass),
    se_broodmass = sd(broodmass)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61")

broodmass <- ggplot(data_brood, aes(x = tr, y = broodmass,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_broodmass, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_broodmass - se_broodmass, ymax = mean_broodmass + se_broodmass, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Brood mass (g)") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(0,10))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(broodmass)

####avgmass by broodsize####
data_avg=data1[data1$success=="1",]
model=glmer(avgmass~tr+broodsize+(1|male)+(1|female),family=gaussian,data=data_avg)
Anova(model,type=3)

avgmass <- ggplot(data_avg, aes(x=broodsize, y=avgmass)) +
  geom_point(aes(color=tr), size=2, alpha=0.4) +
  geom_smooth(data=subset(data_avg, tr=="control"),
              aes(fill="control", color="control", y=avgmass, x=broodsize),
              method="glm",
              method.args=list(family="gaussian"),
              se=T,
              alpha=0.3,
              size=1) +
  geom_smooth(data=subset(data_avg, tr==" +2°C"),
              aes(fill=" +2°C", color=" +2°C", y=avgmass, x=broodsize),
              method="glm",
              method.args=list(family="gaussian"),
              se=T,
              alpha=0.3,
              size=1) +
  scale_color_manual(values=c(" +2°C"="#fdae61", "control"="#3288bd")) +
  scale_fill_manual(values=c(" +2°C"="#fdae61", "control"="#3288bd")) +
  guides(color=guide_legend(override.aes=list(fill=NA)), fill=FALSE) +
  theme_classic() +
  scale_x_continuous(limits=c(0, 45)) +
  scale_y_continuous(limits=c(0.15, 0.4)) +
  labs(y = "Averaged larval mass (g)") +
  labs(x = "Brood size")+ 
  theme(
    axis.text=element_text(size=12),
    axis.title=element_text(size=14),
    legend.position="none"
   )
avgmass

ggarrange(clutchsize,broodsize,broodmass,avgmass,labels= c("(a)", "(b)","(c)","(d)"),ncol = 2, nrow = 2,widths=c(1,1),common.legend = F)


####time to dispersal####
model=glmer(log(dispersal+1)~tr+carc.wt+(1|male)+(1|female),family=gaussian,data=data_avg)
Anova(model,type=3)

summary_data <- data_avg %>%
  group_by(tr) %>%
  summarise(
    mean_dispersal= mean(dispersal),
    se_dispersal = sd(dispersal)/sqrt(n())
  ) 

treatment_colors <- c("#479BD5","#fdae61")

dispersal <- ggplot(data_larva, aes(x = tr, y = dispersal,fill=tr)) +
  geom_point(aes(color = tr), position = position_jitterdodge(jitter.width = 0.4, dodge.width = 0.8), size = 2, alpha = 0.4) + # Jittered data points
  geom_point(data = summary_data, aes(x = tr, y = mean_dispersal, color = tr), shape = 16, size = 5) +  # Mean points
  geom_errorbar(data = summary_data, aes(y = NULL, ymin = mean_dispersal - se_dispersal, ymax = mean_dispersal + se_dispersal, color = tr), width = 0.1) + 
  theme_classic() +
  scale_fill_manual(values = treatment_colors) +  # Specifying fill colors
  scale_color_manual(values = treatment_colors) + 
  labs(y = "Time until larval dispersal (days)") +
  labs(x = "Temperature treatments")+ 
  scale_y_continuous(limits=c(8,12))+
  theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),
    plot.title=element_text(face = "bold", size = 16),
    legend.position="none")

print(dispersal)

ggarrange(egg,larva,dispersal,labels= c("(a)", "(b)","(c)"),ncol = 3, nrow = 1,widths=c(1,1),common.legend = F)




