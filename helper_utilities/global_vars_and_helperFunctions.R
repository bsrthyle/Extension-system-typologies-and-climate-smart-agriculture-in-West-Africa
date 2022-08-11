


# Global variables, Data and Functions

# define font
my_font <- "Segoe Pro Display"

# Define color pallates

my_colors_9 <-
  c(
    "#ffae49",
    "#44b7c2",
    "#024b7a",
    "#ee4144",
    "#1f5e77",
    "#c792e9",
    "#5eeda0",
    "#019d9c",
    "#83329b"
  )
my_colors_6 <-
  c("#086fa1",
    "#40c2d3",
    "#c1dae6",
    "#5eeda0",
    "#e3542a",
    "#935fa7")
my_colors_5 <-
  c("#383e56",
    "#fb743e",
    "#f2a154",
    "#314e52",
    "#1687a7")
my_colors2_6 <-
  c("#BEBADA",
    "#95ABCE",
    "#679DBB",
    "#388EA0",
    "#067D7D",
    "#056B55")
my_colors_7 <-
  c("#019d9c",
    "#83329b",
    "#002454",
    "#30469c",
    "#f7a35c",
    "#067D7D",
    "#056B55")

likert_colors <-
  c("#a3336d",
    "#b55b8a",
    "#7a88bf",
    "#596baf",
    "#e7e7e7")

box_color <- 
  c("#00A08A",
  "#F2AD00",
  "#F98400",
  "#5BBCD6" ,
  "#ECCBAE" ,
  "#046C9A" ,
  "#D69C4E",
  "#ABDDDE",
  "#000000"
)


# custom theme

my_size <- 12
myTheme <- function() {
  ggstatsplot::theme_ggstatsplot() +
    theme(
      axis.text =    element_text(family = my_font, size = my_size),
      axis.title = element_text(
        family = my_font,
        size = my_size,
        face = 'plain'
      ),
      plot.title =        element_text(
        size = my_size ,
        face = 'plain',
        family = my_font
      ),
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


# A function to extend stargazer latex out put to long table format

long_stargazer <- function(...,
                           table_caption,
                           table_label,
                           threeparttable = TRUE,
                           landscape = FALSE,
                           font_size = "small") {
  # Capturing stargazer to hack it
  x <- utils::capture.output(stargazer::stargazer(...))
  
  # Changing tabulare environment for longtable
  # x <- gsub("table", "", x)
  x <- gsub("tabular", "longtable", x)
  
  x <- c(x[1:which(x == "\\hline \\\\[-1.8ex] ")[1] - 1],
         "\\endhead",
         x[which(x == "\\hline \\\\[-1.8ex] ")[1]:length(x)])
  
  x <- c(
    x[2],
    paste0("\\", font_size),
    x[3:4],
    paste0(
      "\\caption{\\textbf{",
      table_caption,
      "}}\\\\",
      "\\endfirsthead"
    ),
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
