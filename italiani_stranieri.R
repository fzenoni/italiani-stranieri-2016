library(ggplot2)
library(dplyr)
library(viridis)
library(ggthemes)
library(RColorBrewer)


# Build the data
data <- data.frame(name = c("Italiani (88.4 %)", "Stranieri con cittadinanza italiana (2.1 %)",
                            "Immigrati regolari (8.2 %)", "Immigrati clandestini (0.8 %)",
                            "In attesa di documenti o rimpatrio (0.3 %)", "Rifugiati (0.2 %)"),
                   pc = c(88.4, 2.1, 8.2, 0.8, 0.3, 0.2))

# Waffle chart
names <- data$name
var <- rep(names, data$pc*10) 

ncol <- 50
nrow <- 20
df <- expand.grid(y=1:nrow, x=1:ncol)
categ_table <- round(sort(table(var), decreasing=TRUE) * ((ncol*nrow)/(length(var))))
df$category <- factor(rep(names(categ_table), categ_table))  

# The default order is alphabetical so it must be fixed
rightOrd <- sort(table(var), decreasing=TRUE) %>% names

# Define colors
colors <- plasma(6)
# customPalette <- c(colors[4], colors[2], colors[5], colors[1], colors[6], colors[3])
colors <- c(grey.colors(n=5, start = 0.1, end = 0.9), '#5794b2')
colors <- c(colors[1], colors[2], colors[3], colors[6], colors[4], colors[5])

# Plot
ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "white", size = 0.5) +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  # scale_fill_brewer(breaks = rightOrd, palette = 'Paired') +
  scale_fill_manual(breaks = rightOrd, values = colors) +
  labs(title = 'Italiani e stranieri in Italia (2016)',
       caption = 'Fonti: ilpost.it, ismu.org, ilsole24ore.com, lenius.it, unhcr.it\nrielaborato da un post di @maurovanetti') +
  theme(panel.border = element_rect(color = "white", size = 1, fill = NA),
        plot.title = element_text(size = rel(1.2)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
