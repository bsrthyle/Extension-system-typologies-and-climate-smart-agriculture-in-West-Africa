
# title: Utility functions
# author: Bisrat Haile
# date: 20/05/2021


# Global variables, Data and Functions

# define font
my_font <- "Segoe Pro Display"

# Define color pallates

my_colors_1 <- c("#ffae49", "#44b7c2", "#024b7a", "#ee4144", "#1f5e77", "#c792e9", "#5eeda0", "#019d9c", "#83329b")
my_colors_2 <- c("#086fa1", "#40c2d3", "#c1dae6", "#5eeda0", "#e3542a", "#935fa7")
colors_two <- c("#383e56", "#fb743e", "#f2a154", "#314e52", "#1687a7")
color_three <- c("#BEBADA", "#95ABCE", "#679DBB", "#388EA0", "#067D7D", "#056B55")
color_four <- c("#019d9c", "#83329b", "#002454", "#30469c", "#f7a35c", "#067D7D", "#056B55")

likert_colors <- c(
  "#a3336d",
  "#b55b8a",
  "#7a88bf",
  "#596baf",
  "#e7e7e7"
)

box_color <- c(
  "#00A08A", "#F2AD00", "#F98400", "#5BBCD6" ,"#ECCBAE" ,"#046C9A" ,"#D69C4E", "#ABDDDE", "#000000"
)
# default theme

my_size <- 12
myTheme <- function() {
  ggstatsplot::theme_ggstatsplot() +
    theme(
      axis.text =    element_text(family = my_font, size = my_size),
      axis.title = element_text(family = my_font, size = my_size, face = 'plain'),
      plot.title =        element_text(size = my_size ,face = 'plain', family = my_font),
      legend.title = element_blank(),
      legend.text =  element_text(family = my_font, size = my_size),
      legend.position = "none",
      panel.background = element_rect(fill = "#ECF0F1"),
      plot.background = element_rect(fill = "#ECF0F1"),
      panel.grid = element_line(colour = "white")
    )
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, class) == "numeric"
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}


axis_limits_and_ticks <- function(axis.lim, min.val, max.val, grid.breaks, exponentiate, min.est, max.est) {

  # factor to multiply the axis limits. for exponentiated scales,
  # these need to be large enough to find appropriate pretty numbers

  fac.ll <- dplyr::if_else(exponentiate, .3, .95)
  fac.ul <- dplyr::if_else(exponentiate, 3.3, 1.05)


  # check for correct boundaries

  if (is.infinite(min.val) || is.na(min.val)) min.val <- min.est
  if (is.infinite(max.val) || is.na(max.val)) max.val <- max.est


  # for negative signs, need to change multiplier

  if (min.val < 0) fac.ll <- 1 / fac.ll
  if (max.val < 0) fac.ul <- 1 / fac.ul


  # axis limits

  if (is.null(axis.lim)) {
    lower_lim <- min.val * fac.ll
    upper_lim <- max.val * fac.ul
  } else {
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }


  # determine gridbreaks

  if (is.null(grid.breaks)) {
    if (exponentiate) {

      # make sure we have nice x-positions for breaks
      lower_lim <- round(lower_lim, 2)
      upper_lim <- round(upper_lim, 2)

      # for *very* small values, lower_lim might be zero, so
      # correct value here. else we have Inf as limit
      if (lower_lim == 0) lower_lim <- min.val * fac.ll / 10

      # use pretty distances for log-scale
      ls <- log10(c(lower_lim, upper_lim))
      ticks <- grDevices::axisTicks(c(floor(ls[1]), ceiling(ls[2])), log = TRUE)

      # truncate ticks to highest value below lower lim and
      # lowest value above upper lim

      ll <- which(ticks < lower_lim)
      if (!sjmisc::is_empty(ll) && length(ll) > 1) ticks <- ticks[ll[length(ll)]:length(ticks)]

      ul <- which(ticks > upper_lim)
      if (!sjmisc::is_empty(ul) && length(ul) > 1) ticks <- ticks[1:ul[1]]
    } else {
      ticks <- pretty(c(floor(lower_lim), ceiling(upper_lim)))
    }
  } else {
    if (length(grid.breaks) == 1) {
      ticks <- seq(floor(lower_lim), ceiling(upper_lim), by = grid.breaks)
    } else {
      ticks <- grid.breaks
    }
  }

  # save proper axis limits
  list(axis.lim = c(min(ticks), max(ticks)), ticks = ticks)
}


nulldef <- function(x, y, z = NULL) {
  if (is.null(x)) {
    if (is.null(y)) {
      z
    } else {
      y
    }
  } else {
    x
  }
}


geom_intercept_line <- function(yintercept, axis.scaling, vline.color) {
  if (yintercept > axis.scaling$axis.lim[1] && yintercept < axis.scaling$axis.lim[2]) {
    t <- theme_get()
    if (is.null(t$panel.grid.major)) t$panel.grid.major <- t$panel.grid
    color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
    minor_size <- nulldef(t$panel.grid.minor$size, .125)
    major_size <- nulldef(t$panel.grid.major$size, minor_size * 1.5)
    size <- major_size * 1.5
    geom_hline(yintercept = yintercept, color = color, size = size)
  } else {
    NULL
  }
}





# A function to extend stargazer latex out put to long table format

long_stargazer <- function(...,
                           table_caption,
                           table_label,
                           threeparttable = TRUE,
                           landscape = FALSE,
                           font_size = "small") {

  # Capturing stargazer to hack it
  x <- utils::capture.output(
    stargazer::stargazer(...)
  )

  # Changing tabulare environment for longtable
  # x <- gsub("table", "", x)
  x <- gsub("tabular", "longtable", x)

  x <- c(
    x[1:which(x == "\\hline \\\\[-1.8ex] ")[1] - 1],
    "\\endhead",
    x[which(x == "\\hline \\\\[-1.8ex] ")[1]:length(x)]
  )

  x <- c(
    x[2],
    paste0("\\", font_size),
    x[3:4],
    paste0("\\caption{\\textbf{", table_caption, "}}\\\\", "\\endfirsthead"),
    # paste0("\\label{", table_label, "}"),
    x[5:length(x)]
  )

  if (threeparttable) {
    x <- c("\\begin{ThreePartTable}", x, "\\end{ThreePartTable}")
  }

  if (landscape) {
    x <- c("\\begin{landscape}", x, "\\end{landscape}")
  }

  cat(x, sep = "\n")
}

