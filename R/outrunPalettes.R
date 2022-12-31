# code adapted from the `wesanderson` package: https://github.com/karthik/wesanderson
#
# palette source can be found here: https://old.reddit.com/r/outrun/comments/8yvo26/outrun_colour_palette/
#   or a direct link here: https://i.redd.it/aepphltiqy911.png


#' Complete list of palettes
#'
#' Use \code{\link{outrunPalette}} to construct palettes of desired length.
#'
#' @export
outrunPalettes <- list(
  pal1 = c("#FF6C11", "#FF3864", "#2DE2E6", "#261447", "#0D0221"),
  pal2 = c("#023788", "#650D89", "#920075", "#F6019D", "#D40078"),
  pal3 = c("#241734", "#2E2157", "#FD3777", "#F706CF", "#FD1D53"),
  pal4 = c("#F9C80E", "#FF4365", "#540D6#", "#791E94", "#541388")
)

#' @title An outrun themed palette generator
#'
#' @description A selection of 4 outrun themed palettes taken from this
#'
#' @param name The name of a color palette. Options are \code{pal1},
#'      \code{pal2}, \code{pal3}, \code{pal4}.
#' @param n Number of desired colors. All palettes are 5 colors.
#' @param type Either "continuous" or "discrete". Use "continuous" if you want
#'      to automatically interpolate between colors.
#'
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#'
#' @return A vector of colors
#'
#' @export
#'
#' @examples
#' outrunPalette("pal1")
#' outrunPalette("pal12", n = 3)
outrunPalette <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)

  pal <- outrunPalettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
