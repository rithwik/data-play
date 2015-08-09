
# Libraries for data manipulation
library(dplyr, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(tidyr, warn.conflicts = F)

# Libraries for graphics
library(ggplot2, warn.conflicts = F)
library(scales)
library(grid)
library(RColorBrewer)

df_data = read.csv("indpakfinal.csv")
df_data[1:5,]

# splitting values like '8 wickets' into '8' and 'wickets'
# thereby getting two columns of margin and whether the winning team batted first or not
df_data_final <-
 df_data %>%
 separate(Margin, into=c("margin", "category"), sep=" ", remove=T, convert=T, extra="drop") %>%
 spread(category, margin)

df_data_final <-
  df_data_final %>%
  select(-NA) %>%
  mutate(Date=as.Date(Date, "%d/%m/%y"), FirstBatScore=as.numeric(FirstBatScore), 
    BR=as.numeric(BR), runs=as.numeric(runs), wickets=as.numeric(wickets)) %>%
  mutate(WinMarPerc = ifelse(!is.na(runs), (runs*100/FirstBatScore),  
    (BR*100/300)), Year=year(Date), Day=wday(Date, label=TRUE)) %>%
  select(Date, Year, Day, Result, Bat, FirstBatScore, BR, runs, wickets, WinMarPerc) %>%
  arrange(Date)

# helper function to set significant digits for percentages
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

df_data_final$WinMarPerc = specify_decimal(df_data_final$WinMarPerc, 2)

indpak <- df_data_final %>%
  select(Year, Day, Result, WinMarPerc) %>%
  mutate(WinMarPerc = as.numeric(WinMarPerc)) %>%
  filter(is.na(WinMarPerc)==F)

### fivethirtyeight.com theme for pretty plot from the interwebz
fte_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  # Begin construction of chart
  theme_bw(base_size=9) +
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

indwin <- filter(indpak, Result=="won")
pakwin <- filter(indpak, Result=="lost")

# final plotting
ggplot(indpak, aes(x=Year, y=Day)) +
  geom_point(size=indwin$WinMarPerc/3, data=indwin, color="#0029A3", alpha=0.7) +
  geom_point(size=pakwin$WinMarPerc/3, data=pakwin, color="#c0392b", alpha=0.7) +
  fte_theme() +
  labs(title="Ind vs Pak, '78-'14", x="Year", y="Day of the Week") +
  scale_x_continuous(breaks=c(1975,1980,1985,1990,1995,2000,2005,2010,2015)) +
  scale_y_discrete(breaks=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")) +
  geom_hline(yintercept=0, size=0.4, color="black") +
  theme(plot.title = element_text(color="#666666", face="bold", size=25, hjust=0)) +
  theme(axis.title = element_text(color="#666666", face="bold", size=22))

# saving the image to disk
ggsave("~/Desktop/indpak.jpg", dpi=500, width=6, height=3)





