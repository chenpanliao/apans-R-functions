quartz.tiff <- 
  function(
    filename, # no filename extension; automatically added
    dpi = 1200,
    color = c("bw", "gray", "rgb", "cmyk")
  ) {
    inputname <- paste0(filename, ".png")
    outputname <- paste0(filename, ".tiff")

    if (color == "bw") {
      channel <- "Black"
      colorspace <- "Gray"
    } else if (color == "gray") {
      channel <- "Gray"
      colorspace <- "Gray"
    } else if (color == "rgb") {
      channel <- "RGB"
      colorspace <- "RGB"
    } else if (color == "cmyk") {
      channel <- "CMYK"
      colorspace <- "CMYK"
    } else {
      channel <- "Black"
      colorspace <- "Gray"
    }

    quartz.save(inputname, type = "png", dpi = dpi)
    
    str <-paste(
      "/usr/local/bin/convert",
      "-density", dpi,
      inputname,
      "-compress Group4",
      "-quality 9",
      "-flatten",
      "-density", dpi,
      "-channel", channel,
      "-colorspace", colorspace,
      outputname,
      sep = " ", collapse = " "
    )
    system(str)
    unlink(inputname)
  }
