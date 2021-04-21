library(RColorBrewer)

set.seed(124)
df <- data.frame(
  distribution=factor(rep(c("Non-rain clouds", "Rain clouds"), each=10000)),
  location=round(c(rnorm(10000, mean=500, sd=80),
                 rnorm(10000, mean=700, sd=150)))
)
head(df)


a <- ggplot(data=df, aes(location))
# Change y axis to count instead of density
a + geom_density(aes(color = distribution)) +
  scale_color_manual(values = c("#868686FF", "grey10"))
# Change fill color by distribution and add mean line
mu <- df %>% 
  group_by(distribution) %>%
  summarise(grp.mean = mean(location))
# Use semi-transparent fill: alpha = 0.4
a + geom_density(aes(fill = distribution), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = distribution),
             data = mu, linetype = "dashed") +
  scale_color_manual(values = c("#868686FF", "grey10"))+
  scale_fill_manual(values = c("#868686FF", "grey10")) +
  labs(title="Overlapping distributions of rain cloud categories", x="Rain cloud density", y="")




############################################
set.seed(124)
df <- data.frame(
  location=round(c(rnorm(1000, mean=300, sd=80),
                   rnorm(1000, mean=500, sd=150))),
  hue = round(c(rnorm(1000, mean=500, sd=100),
                rnorm(1000, mean=200, sd=100)))
)
head(df)

v<-ggplot(data = df, aes(x=location, y=hue) ) + 
  geom_density_2d_filled() + 
  xlim(0,800) + ylim(0,800)
v + labs(title="Heatmap of 2D Categories", x="Location on X-Axis", y="Hue")
#v + theme(legend.title = element_blank())
