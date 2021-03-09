#Data Visualization Challenge

happy = read.csv('Data/2019.csv')


library("MASS")
library(reshape2)
library(ggplot2)

happy$Country.or.region = NULL
happy$Overall.rank = NULL

happy_base = lm(happy$Score~., data = happy)
happy_final = stepAIC(happy_base,trace = F,direction = c("both"))

summary(happy_final)

happy_select = happy[,c("Score","GDP.per.capita","Social.support",
                        "Healthy.life.expectancy","Freedom.to.make.life.choices","Perceptions.of.corruption")]

diacormat <- round(cor(happy_select),2)


get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(diacormat)

melted_dia <- melt(upper_tri,na.rm = TRUE)
head(melted_dia)


ggplot(data = melted_dia, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.3, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

