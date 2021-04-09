#' Configure a simple plot theme
#'
#' \code{theme_stwd_base} provides a default plot theme inspired by the basic
#' principles explained in Edward Tufte's books as well as plot types showcased
#' in C. Knaflic's book "Storytelling with data".
#'
#' It is possible to apply this to any type of plot.
#'
#' @param baseSize the font base size. All text elements are sized relative to
#'   this size. Default is \emph{12}.
#'
#' @param baseFamily the font family that should be used for text elements.
#'   Default is \emph{sans} (e.g. Helvetica, Arial - depending on which is the
#'   default on the local system).
#'
#' @examples
#'
#' \dontrun{
#' theme_stwd_base(baseSize = 12, baseFamily = "serif")
#' }
#'
#' @family visualizations
#'
#' @export
theme_stwd_base <- function(baseSize = 12, baseFamily = "sans") {
  # setup some properties for reuse
  # all font sizes will be defined relative to the base size
  font_base_size <- baseSize

  font_size_axis_title <- 0.9*font_base_size
  font_size_axis_text <- 0.9*font_base_size
  font_size_legend_text <- font_base_size

  # colour set for reuse
  col_white <- "#ffffff"
  col_gray1 <- "#f0f0f0" #col_lightgray<-"#f0f0f0"
  col_gray2 <- "#d9d9d9"
  col_gray3 <- "#bdbdbd" #col_mediumgray<-"#bdbdbd" palette_mediumgray<-"#bdbdbd"
  col_gray4 <- "#969696"
  col_gray5 <- "#737373"
  col_gray6 <- "#636363" #palette_darkgray<-"#636363" #col_darkgray<-"#636363"
  col_gray7 <- "#525252"
  col_gray8 <- "#252525"
  col_black <- "#000000"

  col_brightblue <- "#1342C4"
  col_blue <- "#4964A6"
  col_darkblue <- "#4964A6"

  # colours to be used for the different chart elements
  color_axis_title <- col_gray4
  color_axis_labels <- col_gray4
  color_axis_lines <- col_gray4
  color_plot_title <- col_gray6 #col_gray3
  color_plot_title_faceted <- col_gray6

  # define all line sizes here
  line_elements_size <- 0.5

  # configure the theme
  ggplot2::theme(
    text =
      ggplot2::element_text(family = baseFamily,
                            colour = col_gray6,
                            size = font_base_size),

    panel.background =
      ggplot2::element_blank(),

    panel.grid.major.y =
      ggplot2::element_line(colour = color_axis_lines,
                            size = line_elements_size),

    panel.grid.minor =
      ggplot2::element_blank(),

    panel.grid.major.x =
      ggplot2::element_blank(),

    axis.line =
      ggplot2::element_line(colour = color_axis_lines,
                            size = line_elements_size),

    axis.line.y =
      ggplot2::element_blank(),

    axis.ticks =
      ggplot2::element_line(colour = color_axis_lines,
                            size = line_elements_size),
    axis.title.x =
      ggplot2::element_text(face = "bold",
                            colour = color_axis_title,
                            hjust = 0.0,
                            vjust = 0.0,
                            size = font_size_axis_title),

    axis.text.x =
      ggplot2::element_text(colour = color_axis_labels,
                            size = font_size_axis_text),

    axis.title.y  =
      ggplot2::element_text(face = "bold",
                            colour = color_axis_title,
                            hjust = 1.0,
                            vjust = 1.0,
                            size = font_size_axis_text),

    axis.text.y =
      ggplot2::element_text(colour = color_axis_labels,
                            vjust = 0.35,
                            size = font_size_axis_text),

    legend.position = "top",

    #legend.justification = c(-0.2,1),

    legend.margin = ggplot2::margin(10,0,10,0),

    legend.justification = c(0,0),

    legend.direction = "horizontal",

    legend.title =
      ggplot2::element_text(face = "bold",
                            size = 1.0 * font_size_legend_text),

    legend.text =
      ggplot2::element_text(face = "bold",
                            size = 1.0 * font_size_legend_text),

    plot.title =
      ggplot2::element_text(colour = color_plot_title,
                            hjust = 0.0,
                            size = 1.5 * font_base_size)
  )
}



#' Applies custom fonts and sizes to chart axis
#'
#' @family visualizations
#'
#' @export
#'
theme_fte_fonts <- function(sizeAxisText = 12) {
  # explanatory text elements could use different fonts
  # (such as Source Sans Pro or Cooper Hewitt)
  font_family_main_plot_text <- "Source Sans Pro"
  # all data elements should use the same font family
  font_family_axis_text <- "Roboto Condensed"

  ggplot2::theme(
    text = ggplot2::element_text(family = font_family_main_plot_text),
    axis.title.x = ggplot2::element_text(
      family = font_family_main_plot_text, #font_family_axis_text,
      size = sizeAxisText),
    axis.text.x = ggplot2::element_text(
      family = font_family_axis_text,
      size = sizeAxisText),
    axis.title.y  = ggplot2::element_text(
      family = font_family_main_plot_text, #font_family_axis_text,
      size = sizeAxisText),
    axis.text.y = ggplot2::element_text(
      family = font_family_axis_text,
      size = sizeAxisText))
}



#' Default palette applied to term trend lines in plotting functions
#'
#' @keywords internal
#'
.term_trend_palette <- function() {
  c("increasing" = "#4964A6",
    "decreasing" = "#ec7014",
    "constant" = "#969696")
}



#' Creates some custom month/year labels for date axes
#'
#' @keywords internal
#'
.custom_timeseries_month_label <- function(x) {
  month_index <- as.numeric(format(x, "%m"))
  #min_x <- min(subset(x, !is.na(x) & month_index %in% c(1,4,7,10)))
  min_x <- min(subset(x, !is.na(x)))# & month_index %in% c(1,4,7,10)))

  paste(ifelse((length(subset(x, !is.na(x))) < 6
                | (length(subset(x, !is.na(x))) <= 24 & month_index %in% c(1,4,7,10)))
               | ((length(subset(x, !is.na(x))) > 24 & month_index %in% c(1,7))),
               month.abb[month_index], ""),
        "\n",
        ifelse(month_index == 1 | x == min_x , format(x, "%Y"), ""))
}
