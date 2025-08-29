# img_process3_fixed.R
library(shiny)
library(magick)
library(ggplot2)
library(shinyjs)
library(shinythemes)
library(imager)     # used for Canny edge detection
library(base64enc)  # used to produce download link for exported crop

# Helper: convert magick image to native raster for rasterImage
img_to_raster <- function(img) {
  as.raster(image_scale(img, "100%"))
}

# Small CSS to reduce spacing so plots sit closer together
app_css <- "
.row { margin-bottom: 6px; }
h4 { margin-bottom: 6px; margin-top: 6px; }
.small-note { margin-top: 2px; margin-bottom: 2px; font-size: 90%; color: #333; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),
  theme = shinytheme("flatly"),
  useShinyjs(),
  titlePanel("CT Tumor Measurement — Interactive Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Load image (or keep default 'img.jpg')", accept = c('image/jpeg','image/png')),
      numericInput("mm_per_pixel", "Millimetres per pixel (mm/px)", value = 0.5, step = 0.01),
      checkboxInput("native_display", "Display cropped at native pixel size (may be large)", value = FALSE),
      hr(),
      h4("Selection / Tools"),
      helpText("1) Drag a rectangle on the ORIGINAL image to select area (brush).\n2) Click two points on the CROPPED image or on the EDGE image to measure distance."),
      actionButton("clear_lines", "Clear drawn lines", icon = icon("trash")),
      actionButton("clear_crop", "Clear Cropped Area", icon = icon("ban")),
      actionButton("export_crop", "Download cropped image"),
      hr(),
      h4("Edge Detection Controls"),
      sliderInput("edge_sigma", "Sigma (blur for Canny)", min = 0.5, max = 5, value = 1, step = 0.1),
      sliderInput("edge_thr", "Edge threshold (0-1, alpha)", min = 0, max = 1, value = 0.3, step = 0.01),
      hr(),
      h4("Measurements"),
      verbatimTextOutput("measure_text"),
      hr(),
      h5("Tips"),
      tags$ul(
        tags$li("If you have DICOM-derived spacing use that value for mm/px."),
        tags$li("If native display is off, the app will scale images to fit-screen but measurements still use mm/px.")
      )
    ),
    mainPanel(
      fluidRow(
        column(7,
               h4("Original Image"),
               plotOutput("orig_plot",
                          brush = brushOpts(id = "orig_brush", direction = "xy", fill = "#0F0", clip = TRUE),
                          click = "orig_click",
                          height = "1050px"),
               tags$small("Brush to select an area. Click on image to add endpoints of overlay lines.", class = "small-note")
        ),
        column(5,
               h4("Cropped Image"),
               uiOutput("cropped_ui"),
               tags$small("Click two points in the cropped image to measure distance. Click additional pairs to measure more.", class = "small-note")
        )
      ),
      fluidRow(
        column(7,
               h4("Canny Edge View (of cropped area)"),
               plotOutput("edge_plot", click = "edge_click", height = "800px"),
               tags$small("Adjust sigma & threshold in the sidebar. Lines you draw here also appear on the original image.", class = "small-note")
        ),
        column(5,
               h4("Computed Polar Overlay & Ruler"),
               plotOutput("polar_plot", height = "420px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load default image 'img.jpg' (user indicated name)
  img_path <- reactiveVal('img.jpg')
  observeEvent(input$file, {
    req(input$file)
    img_path(input$file$datapath)
  })
  
  image_magick <- reactive({
    path <- img_path()
    if (!file.exists(path)) stop(sprintf("Image '%s' not found in working directory.", path))
    im <- image_read(path)
    if (image_info(im)$colorspace != 'sRGB') im <- image_convert(im, colorspace = 'sRGB')
    im
  })
  
  # image dims
  dims <- reactive({
    im <- image_magick()
    info <- image_info(im)
    list(width = info$width, height = info$height)
  })
  
  # reactive raster object
  raster_img <- reactive({
    im <- image_magick()
    img_to_raster(im)
  })
  
  # store lines (store coordinates in ORIGINAL image coordinate space)
  # each line: list(x1, y1, x2, y2, plot) ; x2/y2 may be NA while drawing
  drawn_lines <- reactiveVal(list())
  
  # Helper: find incomplete line index for a given plot source ('orig' or 'crop')
  find_incomplete_line_index <- function(lines, src) {
    idx <- NULL
    if (length(lines) > 0) {
      for (i in seq_along(lines)) {
        ln <- lines[[i]]
        if (!is.null(ln$plot) && ln$plot == src && is.na(ln$x2)) {
          idx <- i
          break
        }
      }
    }
    idx
  }
  
  # when user clicks on original: store original coordinates directly
  observeEvent(input$orig_click, {
    click <- input$orig_click
    if (is.null(click)) return()
    w <- dims()$width; h <- dims()$height
    x <- click$x; y <- click$y
    x <- max(0, min(w, x)); y <- max(0, min(h, y))
    lines <- drawn_lines()
    lines <- append(lines, list(list(x1 = x, y1 = y, x2 = NA, y2 = NA, plot = 'orig')))
    drawn_lines(lines)
  })
  
  # Cropped point clicks for measuring or drawing (convert to original image coords)
  observeEvent(input$crop_click, {
    click <- input$crop_click
    cr <- current_crop()
    if (is.null(click) || is.null(cr)) return()
    orig_x <- cr$x1 - 1 + click$x
    orig_y <- cr$y1 - 1 + click$y
    orig_x <- max(0, min(dims()$width, orig_x))
    orig_y <- max(0, min(dims()$height, orig_y))
    lines <- drawn_lines()
    idx <- find_incomplete_line_index(lines, 'crop')
    if (is.null(idx)) {
      lines <- append(lines, list(list(x1 = orig_x, y1 = orig_y, x2 = NA, y2 = NA, plot = 'crop')))
    } else {
      lines[[idx]]$x2 <- orig_x
      lines[[idx]]$y2 <- orig_y
    }
    drawn_lines(lines)
  })
  
  # Edge view click (same as crop click) - lines added as 'crop' so they map onto original
  observeEvent(input$edge_click, {
    click <- input$edge_click
    cr <- current_crop()
    if (is.null(click) || is.null(cr)) return()
    orig_x <- cr$x1 - 1 + click$x
    orig_y <- cr$y1 - 1 + click$y
    orig_x <- max(0, min(dims()$width, orig_x))
    orig_y <- max(0, min(dims()$height, orig_y))
    lines <- drawn_lines()
    idx <- find_incomplete_line_index(lines, 'crop')
    if (is.null(idx)) {
      lines <- append(lines, list(list(x1 = orig_x, y1 = orig_y, x2 = NA, y2 = NA, plot = 'crop')))
    } else {
      lines[[idx]]$x2 <- orig_x
      lines[[idx]]$y2 <- orig_y
    }
    drawn_lines(lines)
  })
  
  # If user brushes selection, create crop
  current_crop <- reactive({
    b <- input$orig_brush
    if (is.null(b)) return(NULL)
    w <- dims()$width; h <- dims()$height
    x1 <- max(1, floor(b$xmin))
    x2 <- min(w, ceiling(b$xmax))
    y1 <- max(1, floor(b$ymin))
    y2 <- min(h, ceiling(b$ymax))
    if (x2 <= x1 || y2 <= y1) return(NULL)
    list(x1 = x1, x2 = x2, y1 = y1, y2 = y2)
  })
  
  # clear crop button - reset brush; keep same approach as earlier
  observeEvent(input$clear_crop, {
    try({
      session$resetBrush("orig_brush")
    }, silent = TRUE)
    session$sendCustomMessage(type = "resetBrush", message = list(id = "orig_brush"))
  })
  
  # produce cropped magick image
  cropped_magick <- reactive({
    cr <- current_crop()
    if (is.null(cr)) return(NULL)
    im <- image_magick()
    crop_w <- cr$x2 - cr$x1 + 1
    crop_h <- cr$y2 - cr$y1 + 1
    geom <- sprintf('%dx%d+%d+%d', crop_w, crop_h, cr$x1 - 1, dims()$height - cr$y2)
    cropped <- image_crop(im, geometry = geom)
    cropped
  })
  
  # Canny edges for cropped image (using imager)
  cropped_edges <- reactive({
    req(cropped_magick())
    tmp <- tempfile(fileext = ".png")
    image_write(cropped_magick(), tmp, format = "png")
    im <- tryCatch({
      load.image(tmp)
    }, error = function(e) {
      im_tmp <- magick::image_read(tmp)
      magick::image_write(magick::image_convert(im_tmp, colorspace = 'gray'), tmp, format = "png")
      load.image(tmp)
    })
    # convert to grayscale if needed
    if (spectrum(im) > 1) img <- grayscale(im) else img <- im
    # Corrected Canny call: pass image positionally
    edges <- tryCatch({
      cannyEdges(img, sigma = input$edge_sigma, alpha = input$edge_thr)
    }, error = function(e) {
      # fallback: approximate using isoblur + gradient magnitude threshold
      g <- isoblur(img, sigma = input$edge_sigma)
      gx <- imgradient(g, "x")
      gy <- imgradient(g, "y")
      mag <- sqrt(gx^2 + gy^2)
      th <- max(mag) * input$edge_thr
      mag > th
    })
    # produce raster for plotting: white background with dark edges
    if (is.cimg(edges)) {
      edges_cimg <- edges
      if (spectrum(edges_cimg) > 1) edges_cimg <- grayscale(edges_cimg)
      raster_obj <- as.raster(1 - as.array(edges_cimg)[,,1,1])
    } else {
      mat <- as.matrix(edges)
      raster_obj <- as.raster(mat * 1)
    }
    raster_obj
  })
  
  # Render original image with overlays and ruler
  output$orig_plot <- renderPlot({
    im_r <- raster_img()
    w <- dims()$width; h <- dims()$height
    par(mar = c(2, 2, 2, 2))
    plot(0, 0, type = 'n', xlim = c(0, w), ylim = c(0, h), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', asp = 1)
    rasterImage(im_r, 0, 0, w, h, interpolate = FALSE)
    # draw ruler ticks (in mm)
    mmpp <- input$mm_per_pixel
    if (mmpp > 0) {
      tick_mm <- 10
      tick_px <- tick_mm / mmpp
      xs <- seq(0, w, by = tick_px)
      ys <- seq(0, h, by = tick_px)
      axis(1, at = xs, labels = round(xs * mmpp), las = 2)
      axis(2, at = ys, labels = round(ys * mmpp), las = 2)
    }
    # draw crop rectangle
    cr <- current_crop()
    if (!is.null(cr)) {
      rect(cr$x1, cr$y1, cr$x2, cr$y2, border = 'yellow', lwd = 2)
      cx <- (cr$x1 + cr$x2) / 2; cy <- (cr$y1 + cr$y2) / 2
      points(cx, cy, pch = 16, col = 'yellow')
    }
    # draw center of image
    points(w / 2, h / 2, pch = 3, col = 'cyan')
    
    # draw drawn lines (all lines are in original coords) - changed lime -> steelblue
    lines_list <- drawn_lines()
    if (length(lines_list) > 0) {
      for (ln in lines_list) {
        if (!is.na(ln$x2)) {
          segments(ln$x1, ln$y1, ln$x2, ln$y2, col = 'steelblue', lwd = 2)
        } else {
          points(ln$x1, ln$y1, col = ifelse(ln$plot == 'orig', 'steelblue', 'orange'), pch = 16)
        }
      }
    }
  }, width = function() {
    min(dims()$width, 1400)
  }, height = function() {
    w <- min(dims()$width, 1400)
    round(w * dims()$height / dims()$width)
  })
  
  # Cropped UI: either show plotOutput or message
  output$cropped_ui <- renderUI({
    if (is.null(current_crop())) {
      tags$div(tags$em('No crop selected. Use the brush on the original image to select an area.'))
    } else {
      cr <- current_crop()
      crop_w <- cr$x2 - cr$x1 + 1
      crop_h <- cr$y2 - cr$y1 + 1
      if (input$native_display && crop_w <= 2000 && crop_h <= 2000) {
        plotOutput("crop_plot", click = "crop_click", height = paste0(crop_h, 'px'), width = paste0(crop_w, 'px'))
      } else {
        plotOutput("crop_plot", click = "crop_click", height = "800px")
      }
    }
  })
  
  # Render cropped image and overlays & ruler (note: lines converted to crop coords for display)
  output$crop_plot <- renderPlot({
    req(current_crop())
    crop_im <- cropped_magick()
    cr <- current_crop()
    crop_r <- as.raster(image_scale(crop_im, "100%"))
    w <- cr$x2 - cr$x1 + 1; h <- cr$y2 - cr$y1 + 1
    par(mar = c(2, 2, 2, 2))
    plot(0, 0, type = 'n', xlim = c(0, w), ylim = c(0, h), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', asp = 1)
    rasterImage(crop_r, 0, 0, w, h, interpolate = FALSE)
    mmpp <- input$mm_per_pixel
    if (mmpp > 0) {
      tick_mm <- 10
      tick_px <- tick_mm / mmpp
      xs <- seq(0, w, by = tick_px)
      ys <- seq(0, h, by = tick_px)
      axis(1, at = xs, labels = round(xs * mmpp), las = 2)
      axis(2, at = ys, labels = round(ys * mmpp), las = 2)
    }
    lines_list <- drawn_lines()
    if (length(lines_list) > 0) {
      for (ln in lines_list) {
        cx1 <- ln$x1 - cr$x1 + 1
        cy1 <- ln$y1 - cr$y1 + 1
        if (!is.na(ln$x2)) {
          cx2 <- ln$x2 - cr$x1 + 1
          cy2 <- ln$y2 - cr$y1 + 1
          if (cx1 >= 0 && cx1 <= w && cy1 >= 0 && cy1 <= h && cx2 >= 0 && cx2 <= w && cy2 >= 0 && cy2 <= h) {
            segments(cx1, cy1, cx2, cy2, col = 'orange', lwd = 2)
            pxdist <- sqrt((ln$x2 - ln$x1)^2 + (ln$y2 - ln$y1)^2)
            mm <- pxdist * input$mm_per_pixel
            midx <- (cx1 + cx2) / 2; midy <- (cy1 + cy2) / 2
            text(midx, midy, labels = paste0(round(mm, 1), ' mm'), pos = 3)
          }
        } else {
          if (cx1 >= 0 && cx1 <= w && cy1 >= 0 && cy1 <= h) {
            points(cx1, cy1, col = 'orange', pch = 16)
          }
        }
      }
    }
  })
  
  # Edge plot - draw produced edge raster and overlay same crop lines
  output$edge_plot <- renderPlot({
    req(current_crop())
    req(cropped_edges())
    cr <- current_crop()
    w <- cr$x2 - cr$x1 + 1; h <- cr$y2 - cr$y1 + 1
    par(mar = c(2, 2, 2, 2))
    plot(0, 0, type = 'n', xlim = c(0, w), ylim = c(0, h), xaxt = 'n', yaxt = 'n', asp = 1)
    rasterImage(cropped_edges(), 0, 0, w, h, interpolate = FALSE)
    lines_list <- drawn_lines()
    if (length(lines_list) > 0) {
      for (ln in lines_list) {
        cx1 <- ln$x1 - cr$x1 + 1
        cy1 <- ln$y1 - cr$y1 + 1
        if (!is.na(ln$x2)) {
          cx2 <- ln$x2 - cr$x1 + 1
          cy2 <- ln$y2 - cr$y1 + 1
          if (cx1 >= 0 && cx1 <= w && cy1 >= 0 && cy1 <= h && cx2 >= 0 && cx2 <= w && cy2 >= 0 && cy2 <= h) {
            segments(cx1, cy1, cx2, cy2, col = 'red', lwd = 2)
          }
        } else {
          if (cx1 >= 0 && cx1 <= w && cy1 >= 0 && cy1 <= h) {
            points(cx1, cy1, col = 'red', pch = 16)
          }
        }
      }
    }
  })
  
  # Polar overlay plot (show original with polar rays and indicate crop center and angle)
  output$polar_plot <- renderPlot({
    im_r <- raster_img()
    w <- dims()$width; h <- dims()$height
    par(mar = c(2, 2, 2, 2))
    plot(0, 0, type = 'n', xlim = c(0, w), ylim = c(0, h), xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', asp = 1)
    rasterImage(im_r, 0, 0, w, h, interpolate = FALSE)
    cx <- w / 2; cy <- h / 2
    points(cx, cy, pch = 3, col = 'cyan', cex = 1.5)
    maxr <- min(cx, cy)
    angles_deg <- seq(0, 330, by = 30)
    for (a in angles_deg) {
      rad <- a * pi / 180
      xend <- cx + maxr * cos(rad)
      yend <- cy + maxr * sin(rad)
      segments(cx, cy, xend, yend, col = 'red', lty = 2)
      text(cx + (maxr + 20) * cos(rad), cy + (maxr + 20) * sin(rad), labels = paste0(a, '°'), col = 'red')
    }
    cr <- current_crop()
    if (!is.null(cr)) {
      ccx <- (cr$x1 + cr$x2) / 2; ccy <- (cr$y1 + cr$y2) / 2
      points(ccx, ccy, pch = 16, col = 'yellow')
      ang <- atan2(ccy - cy, ccx - cx) * 180 / pi
      if (ang < 0) ang <- ang + 360
      segments(cx, cy, ccx, ccy, col = 'yellow', lwd = 2)
      text(ccx, ccy, labels = paste0('Angle: ', round(ang, 1), '°'), pos = 4, col = 'yellow')
    }
    if (input$mm_per_pixel > 0) {
      mmpp <- input$mm_per_pixel
      tick_mm <- 10
      tick_px <- tick_mm / mmpp
      xs <- seq(0, w, by = tick_px)
      axis(1, at = xs, labels = round(xs * mmpp), las = 2)
    }
  }, height = 420)
  
  # measurement summary
  output$measure_text <- renderPrint({
    cr <- current_crop()
    if (is.null(cr)) {
      cat('No crop selected yet.')
      return()
    }
    cx <- (cr$x1 + cr$x2) / 2; cy <- (cr$y1 + cr$y2) / 2
    w <- dims()$width; h <- dims()$height
    ang <- atan2(cy - h / 2, cx - w / 2) * 180 / pi
    if (ang < 0) ang <- ang + 360
    cat(sprintf('Crop center (px): x=%.1f, y=%.1f\n', cx, cy))
    cat(sprintf('Angle from image center: %.2f°\n', ang))
    cat(sprintf('Crop size (px): width=%d, height=%d\n', cr$x2 - cr$x1 + 1, cr$y2 - cr$y1 + 1))
    cat(sprintf('Crop size (mm): width=%.2f mm, height=%.2f mm\n', (cr$x2 - cr$x1 + 1) * input$mm_per_pixel, (cr$y2 - cr$y1 + 1) * input$mm_per_pixel))
    
    lines_list <- drawn_lines()
    if (length(lines_list) > 0) {
      cat('\nLines measured (completed):\n')
      i <- 1
      for (ln in lines_list) {
        if (!is.na(ln$x2)) {
          px <- sqrt((ln$x2 - ln$x1)^2 + (ln$y2 - ln$y1)^2)
          mm <- px * input$mm_per_pixel
          cat(sprintf(' %d) %.1f px = %.2f mm   (source: %s)\n', i, px, mm, ln$plot))
          i <- i + 1
        }
      }
      if (i == 1) cat(" (No completed lines yet)\n")
    }
  })
  
  # clear lines
  observeEvent(input$clear_lines, {
    drawn_lines(list())
  })
  
  # Export cropped image
  observeEvent(input$export_crop, {
    req(cropped_magick())
    tmp <- tempfile(fileext = '.png')
    image_write(cropped_magick(), path = tmp, format = 'png')
    b64 <- base64enc::base64encode(tmp)
    showModal(modalDialog(
      title = 'Cropped Image Ready',
      'Click the link below to download the cropped image (PNG).',
      tags$a(href = paste0('data:;base64,', b64), 'Download cropped image', target = '_blank'),
      easyClose = TRUE
    ))
  })
  
  # graceful error if default image missing
  observe({
    if (!file.exists(img_path())) {
      showModal(modalDialog(
        title = 'Image not found',
        paste0("Default image '", img_path(), "' not found. Please upload or place 'img.jpg' in the app working directory."),
        easyClose = TRUE
      ))
    }
  })
}

# JS handler fallback for clearing brush (only used if session$resetBrush isn't available)
js <- "
Shiny.addCustomMessageHandler('resetBrush', function(message) {
  var id = message.id;
  var el = document.getElementById(id);
  if (el) {
    var evt = new Event('mouseup');
    el.dispatchEvent(evt);
  }
});
"
ui_with_js <- tagList(
  tags$head(tags$script(HTML(js))),
  ui
)

shinyApp(ui = ui_with_js, server = server)
