###----  Plot output script

# normal axes
ggplot_theming <- function() {
     theme_minimal() +
          theme(
               axis.title = element_text(color = 'gray100'),
               axis.text = element_text(color = 'gray'),
               legend.background = element_rect(fill = '#222'),
               legend.text = element_text(color = 'gray'),
               legend.title = element_text(color = 'white'),
               panel.background = element_rect(fill = '#222'),
               panel.grid.major.x = element_line(
                    linetype = 'solid', color = 'black'),
               panel.grid.minor.x = element_line(
                    linetype = "dotted", color = 'gray'),
               panel.grid.major.y = element_line(
                    linetype = 'dashed', color = 'black'),
               panel.grid.minor.y = element_line(
                    linetype = 'dotted', color = 'gray'),
               plot.title = element_text(color = 'white', size = rel(1.5)),
               plot.background = element_rect(fill = '#222')
          )
}

# flipped axes
ggplot_theming_flipped_axes <- function() {
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
