theme_doj <- function(){
  return(
    theme(
      panel.background = element_rect(fill = 'grey50', colour = NA),
      plot.background = element_rect(fill = 'transparent', color = NA),
      strip.background = element_rect(fill = 'transparent', colour = NA),
      legend.background = element_rect(fill = 'transparent'),
      legend.box.background = element_rect(fill = 'transparent'),
      plot.title = element_text(color='grey42'),
      legend.title = element_text(color='grey42'),
      legend.text = element_text(color='grey42'),
      panel.grid = element_line(colour = 'grey42'),
      panel.grid.major = element_line(size = rel(0.5)),
      panel.grid.minor = element_line(size = rel(0.5))
      )
  )
}
