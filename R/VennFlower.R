#' Flower venn plot
#'
#' @importFrom dplyr mutate
#' @importFrom plotrix draw.ellipse
#' @importFrom plotrix draw.circle
#' @param df A data frame, each column typically represents a different sample or group, and the rows represent the observations or features.
#' @param start A numeric value that specifies the starting angle (in degrees) for the first ellipse in the flower plot (default: 90).
#' @param a A numeric value representing the length of the semi-major axis of the ellipses (default: 0.5).
#' @param b A numeric value representing the length of the semi-minor axis of the ellipses (default: 2).
#' @param r A numeric value that specifies the radius of the central circle in the flower plot (default: 1).
#' @param ellipse_col A vector of colors for the ellipses.
#' @param circle_col A character string specifying the color of the central circle in the plot.
#' @param ... Additional graphical parameters that can be passed to the plotting functions. This allows for further customization of the plot, such as adjusting text size, font, or other graphical parameters.
#'
#' @return Venn flower plot
#' @export
#'
#' @examples
#' library(vennFlower)
#' library(dplyr)
#' library(plotrix)
#' my_col <- c(
#'   "#6181BD4E", "#F348004E", "#64A10E4E", "#9300264E",
#'   "#464E044E", "#049a0b4E", "#4E0C664E", "#D000004E",
#'   "#FF6C004E", "#FF00FF4E", "#c7475b4E", "#00F5FF4E",
#'   "#BDA5004E", "#A5CFED4E", "#f0301c4E", "#2B8BC34E",
#'   "#FDA1004E", "#54adf54E", "#CDD7E24E", "#9295C14E",
#'   "#6181BD4E", "#FDA1004E")
#' data("gene_absence")
#' p <- venn.flower(df = gene_absence, ellipse_col = my_col)
#' # If you want to save the venn flower results, use:
#' png("venn_flower.png", width = 10, height = 10, units = "in", res = 300)
#' p
#' dev.off()
venn.flower <- function(df,
                        start = 90,
                        a = 0.5,
                        b = 2,
                        r = 1,
                        ellipse_col = c("#6181BD4E", "#F348004E", "#64A10E4E", "#9300264E",
                                        "#464E044E", "#049a0b4E", "#4E0C664E", "#D000004E",
                                        "#FF6C004E", "#FF00FF4E", "#c7475b4E", "#00F5FF4E",
                                        "#BDA5004E", "#A5CFED4E", "#f0301c4E", "#2B8BC34E",
                                        "#FDA1004E", "#54adf54E", "#CDD7E24E", "#9295C14E",
                                        "#6181BD4E", "#FDA1004E"),
                        circle_col = "white",
                        ...) {
  df_binary <- df %>%
    mutate(across(everything(), ~ ifelse(. > 0, 1, 0)))
  sample <- colnames(df_binary)
  df_binary <- df_binary %>%
    mutate(total = rowSums(.))
  uniq_num <- sapply(sample, function(i) {
    nrow(df_binary %>% filter(df_binary[[i]] == 1 & df_binary$total == 1))
  })
  df_core <- df_binary %>% filter(df_binary$total == length(sample))
  core_num <- nrow(df_core)
  n <- length(sample)
  deg <- 360 / n
  par(bty = "n", ann = F, xaxt = "n", yaxt = "n", mar = c(1, 1, 1, 1))
  plot(c(0, 10), c(0, 10), type = "n")

  lapply(1:n, function(t) {
    # ellipse draw
    draw.ellipse(
      x = 5 + cos((start + deg * (t - 1)) * pi / 180),
      y = 5 + sin((start + deg * (t - 1)) * pi / 180),
      col = ellipse_col[t],
      border = ellipse_col[t],
      a = a, b = b, angle = deg * (t - 1),
      ...
    )

    text(
      x = 5 + 2.5 * cos((start + deg * (t - 1)) * pi / 180),
      y = 5 + 2.5 * sin((start + deg * (t - 1)) * pi / 180),
      uniq_num[t],
      ...
    )

    # text label
    text_angle <- ifelse(deg * (t - 1) < 180 && deg * (t - 1) > 0,
                         deg * (t - 1) - start,
                         deg * (t - 1) + start
    )

    text(
      x = 5 + 3.3 * cos((start + deg * (t - 1)) * pi / 180),
      y = 5 + 3.3 * sin((start + deg * (t - 1)) * pi / 180),
      sample[t],
      srt = text_angle,
      adj = ifelse(deg * (t - 1) < 180 && deg * (t - 1) > 0, 1, 0),
      cex = 1,
      ...
    )
  })

  draw.circle(x = 5, y = 5, r = r, col = circle_col, border = NA, ...)
  text(x = 5, y = 5, core_num, ...)

  # return
  return(recordPlot())
}

