library(ggplot2)
library(ggthemes)
library(scales)
library(extrafont)

theme_ga_raw = function() {
  bg_color = rgb(245, 245, 245, maxColorValue = 255)

  theme_bw(base_size=12, base_family="Avenir") + #%+replace%
    theme(
      panel.background  = element_blank(),
      plot.background = element_rect(fill=bg_color, colour=NA),
      legend.background = element_rect(fill=bg_color, colour=NA),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank()
    )
}

# Main theme
theme_ga = list(
  theme_ga_raw(),
  scale_color_pander(),
  scale_fill_pander(),
  labs(caption='gospelanalysis.com')
  #scale_y_continuous(expand = c(0,0))
)

# Theme for date-related
theme_ga_date = list(
  theme_ga,
  scale_x_date(breaks=pretty_breaks(6))
)

clean_theme = theme(
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  legend.position = "none",
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.grid = element_blank(),
  plot.caption = element_blank()
)

save_gg <- function(title){
  # "Save two copies.
  folder_name = basename(getwd())

  dir_out = 'output'
  dir.create(dir_out, showWarnings = F)
  fp_output = file.path(dir_out, title)

  # Save a copy in ~/gitlab/ldstxt/static/mypost/folder_name
  dir_static = file.path('~/gitlab/ldstxt/static/mypost', folder_name)
  dir.create(dir_static, showWarnings = F)
  fp_static = file.path(dir_static, title)

  # save two copies, one in "output", another in "static"
  ggsave(paste0(fp_output, '.png'))
  ggsave(paste0(fp_static, '.png'))
  # save a clean version for the title
  ggsave(paste0(fp_static, '-clean.png'),
         # height = 10, width = 8*2.5,
         plot = ggplot2::last_plot() + clean_theme)
}

