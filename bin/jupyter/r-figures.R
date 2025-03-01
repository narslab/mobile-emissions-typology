library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggrepel)
library(tidyverse)
library(ggpubr)
library(reshape2)
library(data.table)

df10 = read.csv('cluster_variables_emissions.csv')
df8 = read.csv('emisisons_msa.csv')
names(df8)[1] = "msa"
df8_merge = subset(df8, select = -c(Cluster, Cluster.Name))
df = merge(df10, df8_merge, by=("msa"))
df = subset(df, select = -c(greenhouse.gas.emissions..MMTCO2e., Emissions.per.capita..MTCO2e..y) )


df$central.city[df$central.city=="New York"]<-"NYC"
df$central.city[df$central.city=="Los Angeles"]<-"LA"
df$central.city[df$central.city=="San Francisco"]<-"SF"

head(df8)

par(mar = c(1,1,1,1))
par(cex=1.4)
p <- ggplot(df, aes(Value, Emissions.per.capita..MTCO2e..x, color = Cluster.Name)) + 
  geom_point(alpha=1) +
  scale_x_log10() +
  geom_smooth(method = 'loess', color = 'blue', alpha=.1, span = 3) + #, se=F) + #formula = y ~ s(x, bs = "cs")
  scale_color_manual(
    breaks=c('Auto Large', 'Auto Small', 'Heavyweight Eco-Transit',
             'Heavyweight Transit', 'Sparse Active', 'Sparse Auto',
             'Dense Active'),
    values=c('#FDBF6F','#FF7F00','#B2DF8A',  '#33A02C',  '#FB9A99', '#E31A1C',
             '#1F78B4'
    )) +  
  theme_classic2() + #+
  theme(
    axis.text = element_text(color="black"),
    axis.ticks = element_line(color = "black")
  ) +
  theme(text = element_text(size=16), strip.text = element_text(size=16)) + #Font size
  theme(axis.text = element_text(size = 16), legend.text = element_text(size=16)) +   #All tick size  
  labs(title="", color = 'Type') +
  xlab("") +
  ylab(bquote(Per*-capita~Emissions~(MTCO[2]*e))) +
  coord_cartesian(ylim=c(2.4,13)) +
  geom_text_repel(
    aes(label = central.city), size = 3,
    force= .1, #.2
    force_pull = 2, #2
    angle = 0,
    fontface = 2,
    min.segment.length = 0.4, #.5
    segment.linetype = 1,
    max.overlaps = 6,show.legend=FALSE) +
  guides(fill=guide_legend(nrow=1), color = guide_legend(override.aes = list(size = 4))) +
    # theme(axis.title = element_text(size = 18)) +  # Adjusting Axis Title  
  theme(legend.position="bottom") 
    
p + facet_wrap(~Variable, nrow=2, scales = 'free_x',  strip.position = 'bottom') +
  theme(strip.background = element_blank(), strip.placement = "outside")

ggsave("figure10.png", dpi = 360,
    width     = 12,
    height    =12 )

#labeller = labeller(Factor = as_labeller(factor_names), Variable = as_labeller(variable_names)))

#dev.off()

## figure 8

p2 <- ggplot(df8, aes(greenhouse.gas.emissions..MMTCO2e., Emissions.per.capita..MTCO2e., color = Cluster.Name)) +
  geom_point(alpha=1) +
  scale_x_log10() +
  scale_y_log10() +
  #geom_smooth(method = 'loess', color = 'steelblue', span = 1.1, se=F ) + #formula = y ~ s(x, bs = "cs")
  scale_color_manual(
    breaks=c('Auto Large', 'Auto Small', 'Heavyweight Eco-Transit',
             'Heavyweight Transit', 'Sparse Active', 'Sparse Auto',
             'Dense Active'),
    values=c('#FDBF6F','#FF7F00','#B2DF8A',  '#33A02C',  '#FB9A99', '#E31A1C',
             '#1F78B4'
             )) +
  theme_classic2() + #+
  theme(
    axis.text = element_text(color="black"),
    axis.ticks = element_line(color = "black")
  ) +
  labs(title="",color = 'Type') +
  xlab(bquote(GHG~Emissions~(MMTCO[2]*e))) +
  ylab(bquote(Per*-capita~Emissions~(MTCO[2]*e))) +
  geom_text_repel(
    aes(label = central.city), size = 4, #3.5,
    force= .1, #.2
    force_pull = 2.5, #2
    angle = 0,
    fontface = 2,
    min.segment.length = 0.4, #.5
    segment.linetype = 1,
    max.overlaps = 7,show.legend=FALSE) + 
  theme(text = element_text(size=18), strip.text = element_text(size=18)) + #Font size
  theme(axis.text = element_text(size = 15), legend.text = element_text(size=15)) +# + #All tick size
# theme(axis.title = element_text(size = 18)) +  # Adjusting Axis Title
  theme(legend.position="bottom") +
  guides( color = guide_legend(override.aes = list(size = 4))) 
          #, title.position="top", title.hjust=.5))  #fill=guide_legend(nrow=1),

ggsave("figure8.png", plot=p2, dpi = 360,
       width     = 12,
       height    =10 )

