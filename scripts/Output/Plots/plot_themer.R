###----  Plot output script

# normal axes
ggplot_theming <- function(...) {
     theme_minimal() +
          theme(
               axis.title = element_text(
                    color = 'gray100',
                    margin = margin(10, 10, 10, 10, "pt")
               ),
               axis.title.x = element_text(margin = margin(10, 10, 10, 10, "pt"), face = "bold"),
               axis.title.y = element_text(
                    face = "bold",
                    size = rel(1),
                    margin = margin(10, 10, 10, 10, "pt")
               ),
               axis.text = element_text(color = 'gray', margin = margin(5, 5, 5, 5, "pt")),
               axis.text.x = element_text(),
               axis.text.y = element_text(margin = margin(0, 5, 0, 5, "pt")),
               axis.text.x.top = element_text(vjust = 0.5),
               legend.background = element_rect(fill = '#222'),
               legend.position = "bottom",
               legend.text = element_text(color = 'gray'),
               legend.title = element_text(color = 'white'),
               panel.background = element_rect(fill = '#222'),
               panel.grid.major.x = element_line(linetype = 'solid', color = 'black'),
               panel.grid.minor.x = element_line(linetype = "dotted", color = 'black'),
               panel.grid.major.y = element_line(
                    linetype = 'solid',
                    color = 'black',
                    linewidth = .2
               ),
               panel.grid.minor.y = element_line(linetype = 'dotted', color = 'black'),
               plot.title = element_text(
                    face = "bold",
                    color = 'white',
                    size = rel(1.5)
               ),
               plot.background = element_rect(fill = '#222'),
               plot.caption = element_text(
                    size = 10,
                    color = "gray80",
                    margin = margin(5, 2, 5, 2),
                    hjust = 0
               ),
               #plot.margin = margin(0, 0, 0, 0, "pt"),
               strip.text.y = element_text(color = "gray"),
               strip.text.x = element_text(color = "gray", face = "bold"),
               text = element_text(size = 12)
          )
}

# flipped axes
ggplot_theming_flipped_axes <- function(...) {
     theme_minimal() +
          theme(
               axis.title = element_text(color = 'gray100'),
               axis.text = element_text(color = 'gray'),
               panel.background = element_rect(fill = '#222'),
               panel.grid.major.x = element_line(linetype = 'dashed'),
               panel.grid.minor.x = element_line(linetype = "dotted"),
               panel.grid.major.y = element_line(linetype = 'solid'),
               panel.grid.minor.y = element_line(linetype = 'dotted'),
               plot.title = element_text(color = 'white', size = rel(2)),
               plot.background = element_rect(fill = '#222'),
               legend.background = element_rect(fill = '#222'),
               legend.text = element_text(color = 'gray'),
               legend.title = element_text(color = 'white')
          )
}
