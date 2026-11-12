## Render a MAESPA stand as a 3D image (crowns as spheres, trunks as lines, focal
## tree in red), for the manuscript figure showing stand representations.
## Uses a NULL rgl device + headless Chrome (chromote), so no display is needed.
##
##   render_stand("simulations2026_fla_0.1/H15_V0_L0.44_F1_fla0.1_S1", "stand.png")

render_stand <- function(path, out_png, inner = c(28, 168), width = 1000, height = 800,
                         spin = 45, tilt = -60, zoom = 0.9) {
  options(rgl.useNULL = TRUE)
  tr <- Maeswrap::parseFile(file.path(path, "Trees.dat"))
  focal <- Maeswrap::parseFile(file.path(path, "confile.dat"))$treescon$itargets
  xy <- tr$xy$xycoords
  d <- data.frame(id = seq_along(tr$indivlarea$values),
                  x = xy[c(TRUE, FALSE)], y = xy[c(FALSE, TRUE)],
                  trunk = tr$indivhttrunk$values, crown = tr$indivhtcrown$values,
                  r = tr$indivradx$values)
  d <- subset(d, x > inner[1] & x < inner[2] & y > inner[1] & y < inner[2])
  d$z <- d$trunk + d$crown / 2
  d$col <- ifelse(d$id == focal, "red", "forestgreen")

  rgl::open3d(windowRect = c(0, 0, width, height))
  rgl::bg3d("white")
  rgl::spheres3d(d$x, d$y, d$z, radius = d$r, color = d$col, lit = TRUE)
  trunks <- as.vector(t(cbind(d$x, d$y, 0, d$x, d$y, d$trunk)))
  rgl::segments3d(matrix(trunks, ncol = 3, byrow = TRUE), color = "grey60")
  box <- rbind(c(inner[1], inner[1]), c(inner[2], inner[1]), c(inner[2], inner[2]),
               c(inner[1], inner[2]), c(inner[1], inner[1]))
  rgl::lines3d(box[, 1], box[, 2], 0, color = "grey70")
  rgl::aspect3d("iso")
  ## rotate the stand about the vertical (spin), then tilt it towards the viewer
  rgl::par3d(userMatrix = rgl::rotationMatrix(tilt * pi / 180, 1, 0, 0) %*%
                          rgl::rotationMatrix(spin * pi / 180, 0, 0, 1),
             zoom = zoom)

  html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(rgl::rglwidget(width = width, height = height), html,
                          selfcontained = TRUE)
  rgl::close3d()
  b <- chromote::ChromoteSession$new(width = width, height = height)
  on.exit(b$close())
  b$Page$navigate(paste0("file://", html))
  Sys.sleep(5)
  shot <- b$Page$captureScreenshot(format = "png")
  writeBin(jsonlite::base64_dec(shot$data), out_png)
  invisible(out_png)
}
