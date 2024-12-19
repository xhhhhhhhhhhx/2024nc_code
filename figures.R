
library(ggplot2)
library(ggpmisc)
library(readxl)
library(modelr)
library(lme4)
library(LCFdata)
library(LMERConvenienceFunctions)

fit<- read_excel("data_figures.xlsx",1)
head(fit)

##########
##########
#####1111#####
names(fit)
m2 <- lmer(formula = Y ~ A+Vegetation + (1 | Site),
           data = fit)            
summary(m2)

fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g1=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = A, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "Precipitation of Warmest Quarter (mm)", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(color = 'black', fill = 'transparent'),
    legend.key = element_rect(fill = "transparent", color = "transparent"), 
    legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g1

p1=g1+geom_smooth(data = fit, method = "lm", formula = y ~ x,
            se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +

  stat_poly_eq(
  data = fit, 
  aes(x = A, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
  formula = y ~ x,
  parse = TRUE,
  label.y.npc = 0.9, 
  label.x.npc = 0.9, 
  color = "black",
  inherit.aes = FALSE)
p1  




####2222######

names(fit)
m2 <- lmer(formula = Y ~ B+Vegetation + (1 | Site),
           data = fit)            
summary(m2)


fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g2=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = B, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "Temperature Seasonality (Unitless)", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g2

p2=g2+geom_smooth(data = fit, method = "lm", formula = y ~ x,
                  se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +
  
  stat_poly_eq(
    data = fit, 
    aes(x = B, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.y.npc = 0.9, 
    label.x.npc = 0.9, 
    color = "black",
    inherit.aes = FALSE)
p2  

  


#####3333#####

names(fit)
m2 <- lmer(formula = Y ~ C+Vegetation + (1 | Site),
                 
           data = fit)            
summary(m2)
coef(m2)

fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g3=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = C, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "Mean Temperature of Wettest Quarter (℃)", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g3

p3=g3+geom_smooth(data = fit, method = "lm", formula = y ~ x,
                  se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +
  
  stat_poly_eq(
    data = fit, 
    aes(x = C, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.y.npc = 0.9, 
    label.x.npc = 0.9, 
    color = "black",
    inherit.aes = FALSE)
p3  



#####4444#####
##########
##########
names(fit)
m2 <- lmer(formula = Y ~ PC1+Vegetation + (1 | Site),
                      
           data = fit)            
summary(m2)
coef(m2)

fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g4=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = PC1, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "PC1_score", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g4

p4=g4+geom_smooth(data = fit, method = "lm", formula = y ~ x,
                  se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +
  
  stat_poly_eq(
    data = fit, 
    aes(x = PC1, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.y.npc = 0.9, 
    label.x.npc = 0.9, 
    color = "black",
    inherit.aes = FALSE)
p4 



#####5555#####
names(fit)
m2 <- lmer(formula = Y ~ PC2+Vegetation + (1 | Site),
                      
           data = fit)            
summary(m2)
coef(m2)

fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g5=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = PC2, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +  
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "PC2_score", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g5

p5=g5+geom_smooth(data = fit, method = "lm", formula = y ~ x,
                  se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +
  
  stat_poly_eq(
    data = fit, 
    aes(x = PC2, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.y.npc = 0.9, 
    label.x.npc = 0.9, 
    color = "black",
    inherit.aes = FALSE)
p5  




#####6666#####
names(fit)
m2 <- lmer(formula = Y ~ PC3+Vegetation + (1 | Site),
                     
           data = fit)            
summary(m2)
coef(m2)

fit$site=factor(fit$Site)
fit$Vegetation=factor(fit$Vegetation,levels = c("Forests", "Grasslands"))
#####
g6=fit %>%            
  add_predictions(m2) %>%            
  ggplot(aes(            
    x = PC3, y = Y,
    color = Vegetation,shape = Vegetation  )) +            
  geom_point(size=6)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) + 
  scale_color_manual(values = c("#006400","#F46C1B"))+ scale_shape_manual(values = c(1,2))+
  
  labs(x = "PC3_score", y = "log10(SL_mean)") +       
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme_bw()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = "transparent", color = "transparent"), 
        legend.background = element_rect(fill = "transparent", color = "transparent") 
  )
g6

p6=g6+geom_smooth(data = fit, method = "lm", formula = y ~ x,
                  se = TRUE, color = "black", linetype = "solid", size = 1, aes(group = 1)) +
  
  stat_poly_eq(
    data = fit, 
    aes(x = PC3, y = Y, label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    formula = y ~ x,
    parse = TRUE,
    label.y.npc = 0.9, 
    label.x.npc = 0.9, 
    color = "black",
    inherit.aes = FALSE)
p6  


library(patchwork) 
P=(p1 + p2 + p3) / (p4 + p5+ p6)
P

