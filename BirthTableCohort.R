################################################################################################
# Birth table example
# Code to reproduce results in Baudisch and Alvarez (2020)
################################################################################################ 


library(tidyverse)
library(data.table)
library(hrbrthemes)

lab_years10 <- c("1940", "'45", "'50", "'55", "1960")
brk_years10 <- c(1940, 1945,  1950, 1955, 1960)


Bx <- read_table2("birthsVH.txt",skip = 2)
Bx$Age[Bx$Age== "12-"] <- 12
Bx$Age[Bx$Age== "55+"] <- 55
Bx$Age <- as.integer(Bx$Age)
names(Bx) <- c("Country","Year","Age","bx")
Bx$bx <- as.numeric(Bx$bx)
Bx<- data.table(subset(Bx, Year>=1940 & Year<= 1960
                       & !(Country == c("DEUTE", "DEUTW"))
                       & !(Country == c("DEUTW"))
                       & !(Country == c("GBR_NP"))
                       ))
Bx<- Bx[complete.cases(Bx)]

# Rates for Sweden
Fx <- read_table2("SWEasfrVH.txt", skip = 2)
Fx <- data.frame(lapply(Fx, as.character), stringsAsFactors=FALSE)
Fx$Age[Fx$Age == "12-"] <-12
Fx$Age[Fx$Age == "55+"] <-55

Fx[, c(1:3)] <- sapply(Fx[, c(1:3)], as.numeric)
Fx           <-data.table(arrange(Fx,Year, Age))
names(Fx)[3] <- "m"




# Function to calculate Birth Tables
BT <- function(Age, bx){
B  <- sum(bx)  
Bx <- cumsum(bx)
lx <- B-Bx # two ways of calculating it
nbx <- c(-diff(lx),0)

nqx <- nbx / lx
npx <- 1- nqx

Lx <- c((lx[-1] +lx[-length(lx)])*0.5,lx[length(lx)]) # Wachter approx.
Lx[is.na(Lx)] <- 0 ## in case of NA values
Lx[is.infinite(Lx)] <- 0 ## in case of Inf values

Tx <- rev(cumsum(rev(Lx)))
ex  <- Tx/lx 
ex[is.na(ex)] <- 0 # check

# Triplet
hx <- bx/(B-Bx)
fx <- bx/B
Sx <- (B - Bx) / B

# Variance, standard deviation and coefficient of variation

V <- rev( cumsum( rev(((Age -Age[1L] + 0.5 - ex[1L])^2)* fx))) # alpha has to be substracted
SD <- sqrt(V)
CV <-SD/ex 

# e-dagger and entropy
ex.nbx <- (ex * nbx)/ lx[1]
e.dag <- rev(cumsum(rev(ex.nbx)))
H <- e.dag / ex

H[is.na(H)] <- 0 ## in case of NA values
H[is.infinite(H)] <- 0 ## in case of Inf values

# gini coefficient
G.lx <- lx^2/(lx[1]^2)
G <- 1- rev(cumsum(rev(G.lx)))/ex
G[is.na(G)] <- 0 ## in case of Inf values


out <- data.frame(Age, bx, Bx, lx, nbx, nqx, npx, Lx, Tx, ex,V,SD,CV,e.dag, H,G, hx, fx, Sx)

return(out)

}

# Compute Birth Tables for all Years and Countries
BirthTable <- Bx[,BT(Age, bx), by = list(Year, Country)]


# Figues ------------------------------------------------------------------


# qx

BirthTable %>% filter(Country %in% c( "USA") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_point(aes(Age, nqx, colour = Country), size = 0.3)+
  geom_line(aes(Age, nqx, colour = Country), size = 0.2)+
  coord_cartesian(xlim = c(12,55), ylim = c(0,1.01))+
  scale_y_continuous(breaks = seq(0,1,by = 0.25), expand = c(0,0))+
  scale_x_continuous(breaks = c(seq(15,55,by = 5)), expand = c(0,0))+
  scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
  theme_minimal()+
  xlab("Age")+
  ylab("nqx")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 9,font_rc))+
  ylab("Probability of being born, q(x)")


#ggsave("fig/qx.pdf", width = 2, height = 2, device = cairo_pdf)



# Birth expectancy
BirthTable %>% filter(Country %in% c( "USA", "DEUTNP", "DNK") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_point(aes(Age, ex, colour = Country), size = 0.3)+
  geom_line(aes(Age, ex, colour = Country), size = 0.2)+
  coord_cartesian(xlim = c(12,55), ylim = c(0,16))+
  scale_y_continuous(breaks = seq(0,16,by = 4), expand = c(0,0))+
  scale_x_continuous(breaks = c(seq(15,55,by = 5)), expand = c(0,0))+
  scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
  theme_minimal()+
  xlab("Age")+
  ylab("nqx")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 9,font_rc))+
  ylab("Birth expectancy, e(x)")


#ggsave("fig/ex.pdf", width = 2, height = 2, device = cairo_pdf)


# lx 


BirthTable %>% filter(Country %in% c("USA", "DEUTNP", "DNK") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_point(aes(Age, nbx, colour = Country), size = 0.5)+
  geom_line(aes(Age, nbx, colour = Country), size = 0.2)+
 # coord_cartesian(xlim = c(10,55), ylim = c(0,1.01))+
  #scale_y_continuous(breaks = seq(0,1,by = 0.25), expand = c(0,0))+
  scale_x_continuous(breaks = seq(10,55,by = 5), expand = c(0,0))+
  scale_colour_manual(values = c("darkgoldenrod2", "forestgreen", "blue", "red"))+
  theme_minimal()+
  xlab("Age")+
  ylab("nqx")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))






# Survivorship function
BirthTable %>% filter(Country %in% c("USA", "DEUTNP", "DNK") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_line(aes(Age, Sx, colour = Country), size = 0.2)+
  geom_point(aes(Age, Sx, colour = Country), size = 0.2)+
  coord_cartesian(xlim = c(12,50), ylim = c(0,1.01))+
  scale_y_continuous(breaks = seq(0,1,by = 0.25), expand = c(0,0))+
  scale_x_continuous(breaks = seq(10,55,by = 5), expand = c(0,0))+
  scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
  theme_minimal()+
  xlab("Age")+
  ylab("Survivorship function")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

ggsave("fig/Sx.pdf", width = 2, height = 2, device = cairo_pdf)





# Density
BirthTable %>% filter(Country %in% c("SWE") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_line(data = subset(Fx, Cohort == 1960), 
            aes(Age, m), colour = "black", size = 0.2)+
  geom_point(data = subset(Fx, Cohort == 1960), 
            aes(Age, m), colour = "black", size = 0.2)+
  geom_line(aes(Age, fx, colour = Country), size = 0.2)+
  geom_point(aes(Age, fx, colour = Country), size = 0.2)+
  coord_cartesian(xlim = c(12,50), ylim = c(0,0.2))+
  scale_y_continuous(breaks = seq(0,.2,by = 0.02), expand = c(0,0))+
  scale_x_continuous(breaks = seq(10,55,by = 5), expand = c(0,0))+
  scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
  xlab("Age")+
  ylab("Density function")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

ggsave("fig/fx_vs_mx.pdf", width = 2, height = 2, device = cairo_pdf)


# Hazard
BirthTable %>% filter(Country %in% c("USA", "DEUTNP", "DNK") 
                      & Year == 1960) %>% 
  ggplot()+
  geom_point(aes(Age, (hx), colour = Country), size = 0.2)+
  geom_line(aes(Age, (hx), colour = Country), size = 0.2)+
  coord_cartesian(xlim = c(12,50), ylim = c(0.0001,1))+
  scale_y_continuous(breaks = c(0.01,0.1,0.5,1), trans= "log")+
  scale_x_continuous(breaks = seq(10,55,by = 5), expand = c(0,0))+
  scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
  xlab("Age")+
  ylab("Hazard (log)")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

ggsave("fig/hx.pdf", width = 2, height = 2, device = cairo_pdf)





# # Birth Expectancy
# 
# BirthTable %>% filter(Country %in% c("USA", "DEUTNP", "DNK") 
#                       & Age == 12) %>% 
#   ggplot()+
#   geom_point(aes(Year, ex, colour = Country), size = 0.2)+
#   geom_line(aes(Year, ex, colour = Country), size = 0.2)+
#   coord_cartesian(xlim = c(1940,1960), ylim = c(11,16))+
#   scale_y_continuous(breaks = seq(8,20,by = 1), expand = c(0,0))+
#   scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
#   scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
#   xlab("Year")+
#   ylab("Hazard (log)")+
#   theme_bw()+
#   theme(aspect.ratio = 0.8, 
#         strip.background = element_rect(fill="none"),
#         legend.position = "none",
#         panel.spacing = unit(2, "lines"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "grey"),
#         axis.line = element_blank(),
#         axis.ticks = element_line(size = 0.3, colour = "gray70"),
#         axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = 2),
#         text = element_text(size = 8,font_rc))
# 
# 
# 
# 
# #ggsave("fig/ex.pdf", width = 2, height = 2, device = cairo_pdf)
# 
# # Standard Deviation
# 
# 
# 
# BirthTable %>% filter(Country %in% c("USA", "DEUTNP", "DNK") 
#                       & Age == 12) %>% 
#   ggplot()+
#   geom_point(aes(Year, SD, colour = Country), size = 0.2)+
#   geom_line(aes(Year, SD, colour = Country), size = 0.2)+
#   coord_cartesian(xlim = c(1940,1960), ylim = c(4.5,6.5))+
#   scale_y_continuous(breaks = seq(3,10,by = 0.5), expand = c(0,0))+
#   scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
#   scale_colour_manual(values = c("#ff4d1c", "#601cff", "#6bcfa9"))+
#   xlab("Year")+
#   ylab("Hazard (log)")+
#   theme_bw()+
#   theme(aspect.ratio = 0.8, 
#         strip.background = element_rect(fill="none"),
#         legend.position = "none",
#         panel.spacing = unit(2, "lines"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "grey"),
#         axis.line = element_blank(),
#         axis.ticks = element_line(size = 0.3, colour = "gray70"),
#         axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = 2),
#         text = element_text(size = 8,font_rc))
# 
# 
# ggplot(subset(BirthTable, Age == 12))+
#   geom_line(aes(Year, SD, group = Country), colour = "grey80")+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "ESP"),
#             aes(Year, SD), colour = "darkgoldenrod2", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "JPN"),
#             aes(Year, SD), colour = "forestgreen", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "SWE"),
#             aes(Year, SD), colour = "blue", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "USA"),
#             aes(Year, SD), colour = "red", size = .5)+
#   coord_cartesian(xlim = c(1940,1960), ylim = c(3.5,6.5))+
#   scale_y_continuous(breaks = seq(3,10,by = 0.5), expand = c(0,0))+
#   scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
#   xlab("Cohort")+
#   ylab("Standard deviation")+
#   theme_bw()+
#   theme(aspect.ratio = 1, 
#         strip.background = element_rect(fill="none"),
#         legend.position = "none",
#         panel.spacing = unit(2, "lines"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "grey"),
#         axis.line = element_blank(),
#         axis.ticks = element_line(size = 0.3, colour = "gray70"),
#         axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = 2),
#         text = element_text(size = 8,font_rc))
# 
# 
# #ggsave("fig/SD.pdf", width = 2, height = 2, device = cairo_pdf)
# 
# # Coefficient of variation
# 
# ggplot(subset(BirthTable, Age == 12))+
#   geom_line(aes(Year, CV, group = Country), colour = "grey80")+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "ESP"),
#             aes(Year, CV), colour = "darkgoldenrod2", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "JPN"),
#             aes(Year, CV), colour = "forestgreen", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "SWE"),
#             aes(Year, CV), colour = "blue", size = .5)+
#   geom_line(data = subset(BirthTable, Age == 12 & Country == "USA"),
#             aes(Year, CV), colour = "red", size = .5)+
#   coord_cartesian(xlim = c(1940,1960), ylim = c(0.25,0.5000001))+
#   scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
#   scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
#   xlab("Cohort")+
#   ylab("Coefficient of variation")+
#   theme_bw()+
#   theme(aspect.ratio = 1, 
#         strip.background = element_rect(fill="none"),
#         legend.position = "none",
#         panel.spacing = unit(2, "lines"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(colour = "grey"),
#         axis.line = element_blank(),
#         axis.ticks = element_line(size = 0.3, colour = "gray70"),
#         axis.title.y = element_text(vjust = 2),
#         axis.title.x = element_text(vjust = 2),
#         text = element_text(size = 8,font_rc))

#ggsave("fig/CV.pdf", width = 2, height = 2, device = cairo_pdf)



# SD vs ex



ggplot(subset(BirthTable, Age == 12))+
  geom_point(aes(ex, SD, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
             aes(ex, SD), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
             aes(ex, SD), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
             aes(ex, SD), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
             aes(ex, SD), colour = "red", size = 0.2)+
  coord_cartesian(xlim = c(11,16), ylim = c(3.5,6.5000001))+
  scale_y_continuous(breaks = seq(0,16,by = 0.5), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Standard deviation")+
  theme_bw()+
  theme(aspect.ratio = 0.8, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

ggsave("fig/SDex.pdf", width = 2, height = 2, device = cairo_pdf)

# CV vs ex

ggplot(subset(BirthTable, Age == 12))+
  geom_point(aes(ex, CV, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
             aes(ex, CV), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
             aes(ex, CV), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
             aes(ex, CV), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
             aes(ex, CV), colour = "red", size = 0.2)+
  coord_cartesian(xlim = c(11,16), ylim = c(0.25,.50000001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Coefficient of variation")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))





#ggsave("fig/CVex.pdf", width = 2, height = 2, device = cairo_pdf)


# e-dagger

ggplot(subset(BirthTable, Age == 12))+
  geom_line(aes(Year, e.dag, group = Country), colour = "grey80")+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "ESP"),
            aes(Year, e.dag), colour = "darkgoldenrod2", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "JPN"),
            aes(Year, e.dag), colour = "forestgreen", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "SWE"),
            aes(Year, e.dag), colour = "blue", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "USA"),
            aes(Year, e.dag), colour = "red", size = 1.3)+
  #coord_cartesian(xlim = c(1940,1960), ylim = c(0.75,0.9))+
  #scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = seq(1940,1960,by = 5), expand = c(0,0))+
  xlab("Cohort")+
  ylab("e-dagger")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 18,font_rc))

#ggsave("fig/e.dag.pdf", width = 6, height = 4, device = cairo_pdf)


# Entropy

ggplot(subset(BirthTable, Age == 12))+
  geom_line(aes(Year, H, group = Country), colour = "grey80")+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "ESP"),
            aes(Year, H), colour = "darkgoldenrod2", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "JPN"),
            aes(Year, H), colour = "forestgreen", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "SWE"),
            aes(Year, H), colour = "blue", size = 1.3)+
  geom_line(data = subset(BirthTable, Age == 12 & Country == "USA"),
            aes(Year, H), colour = "red", size = 1.3)+
 # coord_cartesian(xlim = c(1940,1960), ylim = c(0.75,0.95000001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
  xlab("Cohort")+
  ylab("Entropy")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 18,font_rc))


# CV vs ex

ggplot(subset(BirthTable, Age == 12))+
  geom_point(aes(ex, CV, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
             aes(ex, CV), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
             aes(ex, CV), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
             aes(ex, CV), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
             aes(ex, CV), colour = "red", size = 0.2)+
  #coord_cartesian(xlim = c(11,16), ylim = c(0.75,.90000001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Coefficient of variation")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))


#ggsave("fig/CVex.pdf", width = 6, height = 4, device = cairo_pdf)

# e.dag vs ex

ggplot(subset(BirthTable, Age == 12))+
  geom_point(aes(ex, e.dag, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
             aes(ex, e.dag), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
             aes(ex, e.dag), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
             aes(ex, e.dag), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
             aes(ex, e.dag), colour = "red", size = 0.2)+
 # coord_cartesian(xlim = c(11,16), ylim = c(0.25,.450000001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Birthtable entropy (H)")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

#ggsave("fig/edagex.pdf", width = 6, height = 4, device = cairo_pdf)
# Entropy vs ex

ggplot(subset(BirthTable, Age == 12))+
 geom_point(aes(ex, H, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
            aes(ex, H), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
            aes(ex, H), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
            aes(ex, H), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
            aes(ex, H), colour = "red", size = 0.2)+
  coord_cartesian(xlim = c(11,16), ylim = c(0.25,.450000001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.05), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Birthtable entropy (H)")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))


#ggsave("fig/Hex.pdf", width = 2, height = 2, device = cairo_pdf)

# Gini

ggplot(subset(BirthTable, Age == 12))+
  geom_point(aes(ex, G, group = Country), colour = "grey70", size = 0.1)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "ESP"),
             aes(ex, G), colour = "darkgoldenrod2", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "JPN"),
             aes(ex, G), colour = "forestgreen", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "SWE"),
             aes(ex, G), colour = "blue", size = 0.2)+
  geom_point(data = subset(BirthTable, Age == 12 & Country == "USA"),
             aes(ex, G), colour = "red", size = 0.2)+
  coord_cartesian(xlim = c(11,16), ylim = c(0.10,0.22001))+
  scale_y_continuous(breaks = seq(0,1,by = 0.02), expand = c(0,0))+
  scale_x_continuous(breaks = seq(5,20,by = 1), expand = c(0,0))+
  xlab("Birth expectancy")+
  ylab("Gini coefficient (G)")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))


#ggsave("fig/Gex.pdf", width = 2, height = 2, device = cairo_pdf)


# Mean age at birth and Standard deviation from the HFD -------------------

mean <- read_table2("mabVH.txt", skip = 2)
sd <- read_table2("sdmabVH.txt", skip = 2)

dat <- merge(mean,sd)

dat[,c(2:6)]<- apply(FUN = as.numeric,X = dat[,c(2:6)],MARGIN = 2)

dat$CV <-  dat$sdCMAB/ dat$CMAB 

BT <- subset(BirthTable[,c(1,2,3,12,14)], Age == 12)

names(dat) <- c("Country", "Year", "MAB", "MAB40", "sdMAB", "sdMAB40", "CVMAB")

dat <- merge(dat, BT, by = c("Country", "Year"))
dat$exx <- dat$ex +13


ggplot(dat)+
  geom_line(aes(Year, MAB/exx, group = Country ), alpha = 0.5, colour = "grey50")+
  
  
  geom_line(data = subset(dat,Country == "ESP"),
            aes(Year, MAB/exx), colour = "darkgoldenrod2", size = .5)+
  geom_line(data = subset(dat, Country == "JPN"),
            aes(Year, MAB/exx), colour = "forestgreen", size = .5)+
  geom_line(data = subset(dat, Country == "SWE"),
            aes(Year, MAB/exx), colour = "blue", size = .5)+
  geom_line(data = subset(dat, Country == "USA"),
            aes(Year, MAB/exx), colour = "red", size = .5)+
  
  coord_cartesian(xlim = c(1940,1960), ylim = c(0.96,1.04))+
  scale_y_continuous(breaks = seq(0.9,1.1,by = 0.02), expand = c(0,0))+
  scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
  xlab("Cohort")+
  ylab("Mean age at birth / Birth expectancy")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))

ggsave("fig/MAD.pdf", width = 2, height = 2, device = cairo_pdf)

ggplot(dat)+
  geom_line(aes(Year, sdMAB/SD, group = Country ), alpha = 0.5, colour = "grey50")+
  geom_line(data = subset(dat,Country == "ESP"),
            aes(Year, sdMAB/SD), colour = "darkgoldenrod2", size = .5)+
  geom_line(data = subset(dat, Country == "JPN"),
            aes(Year, sdMAB/SD), colour = "forestgreen", size = .5)+
  geom_line(data = subset(dat, Country == "SWE"),
            aes(Year, sdMAB/SD), colour = "blue", size = .5)+
  geom_line(data = subset(dat, Country == "USA"),
            aes(Year, sdMAB/SD), colour = "red", size = .5)+
  
  coord_cartesian(xlim = c(1940,1960), ylim = c(0.96,1.04))+
  scale_y_continuous(breaks = seq(0.9,1.1,by = 0.01), expand = c(0,0))+
  scale_x_continuous(breaks = brk_years10, labels = lab_years10, expand = c(0,0))+
  xlab("Cohort")+
  ylab("SD(Mean age at death) / SD(Birth expectancy) ")+
  theme_bw()+
  theme(aspect.ratio = 1, 
        strip.background = element_rect(fill="none"),
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "grey"),
        axis.line = element_blank(),
        axis.ticks = element_line(size = 0.3, colour = "gray70"),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = 2),
        text = element_text(size = 8,font_rc))
ggsave("fig/sdMAD.pdf", width = 2, height = 2, device = cairo_pdf)
