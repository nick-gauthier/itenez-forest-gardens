# Clean Moondream → Ultralytics SAM Pipeline
# This script:
# 1. Detects points using moondream API (returns normalized coordinates 0-1)
# 2. Converts normalized coordinates to pixel coordinates
# 3. Segments using ultralytics SAM via reticulate
# 4. Visualizes results with overlaid masks

library(httr2)
library(openssl)
library(tibble)
library(dplyr)
library(magick)
library(reticulate)
py_require('ultralytics')

# ============================================================================
# MOONDREAM POINT DETECTION
# ============================================================================

# Encode image to base64
encode_image <- function(image_path) {
  if (!file.exists(image_path)) stop("Image not found: ", image_path)
  raw <- readBin(image_path, "raw", file.info(image_path)$size)
  gsub("\n", "", openssl::base64_encode(raw), fixed = TRUE)
}

# Detect points using moondream API
# Returns: tibble with object_id, x, y (normalized 0-1)
detect_points_moondream <- function(image_path,
                                   object_prompt,
                                   api_key = Sys.getenv("MOONDREAM_API_KEY"),
                                   use_local = FALSE) {

  # Determine endpoint
  base_url <- if (use_local) {
    "http://localhost:2020/v1/point"
  } else {
    "https://api.moondream.ai/v1/point"
  }

  # Get file extension and create data URL
  ext <- tolower(tools::file_ext(image_path))
  mime <- switch(ext,
    png = "data:image/png;base64,",
    jpg = "data:image/jpeg;base64,",
    jpeg = "data:image/jpeg;base64,",
    stop("Unsupported format. Use PNG or JPEG.")
  )

  image_url <- paste0(mime, encode_image(image_path))

  # Make API request
  req <- httr2::request(base_url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(list(
      image_url = image_url,
      object = object_prompt
    ))

  # Add auth header if using cloud API
  if (!use_local) {
    req <- req |> httr2::req_headers("X-Moondream-Auth" = api_key)
  }

  resp <- req |>
    httr2::req_error(is_error = ~ httr2::resp_status(.x) >= 400) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  # Parse response
  points <- resp$points
  if (is.null(points) || length(points) == 0) {
    return(tibble(object_id = integer(), x = numeric(), y = numeric()))
  }

  tibble(
    object_id = seq_along(points),
    x = purrr::map_dbl(points, ~ .x$x),  # normalized 0-1
    y = purrr::map_dbl(points, ~ .x$y)   # normalized 0-1
  )
}

# ============================================================================
# COORDINATE CONVERSION
# ============================================================================

# Convert normalized moondream coordinates (0-1) to pixel coordinates
# Moondream: x,y are normalized (0-1)
# SAM: needs pixel coordinates (x,y in actual image dimensions)
normalized_to_pixel <- function(points_df, image_path) {

  # Get image dimensions
  img_info <- magick::image_info(magick::image_read(image_path))
  width <- img_info$width
  height <- img_info$height

  # Convert to pixel coordinates
  points_df |>
    dplyr::mutate(
      x_pixel = x * width,
      y_pixel = y * height
    )
}

# ============================================================================
# ULTRALYTICS SAM SEGMENTATION
# ============================================================================

# Initialize ultralytics SAM model
init_sam <- function(model_type = "sam_b.pt") {
  # Check if ultralytics is available
  if (!reticulate::py_module_available("ultralytics")) {
    stop("ultralytics not found. Install with: pip install ultralytics")
  }

  # Import ultralytics
  ul <- reticulate::import("ultralytics")

  # Load SAM model
  message("Loading SAM model: ", model_type)
  model <- ul$SAM(model_type)

  model
}

# Segment using point prompts
# points_df should have columns: x_pixel, y_pixel
# Segments each point individually (one mask per point)
# Uses other points as negative prompts to help distinguish objects
segment_with_sam <- function(model, image_path, points_df,
                             one_at_a_time = TRUE,
                             use_negative_prompts = TRUE) {

  if (nrow(points_df) == 0) {
    message("No points to segment")
    return(list())
  }

  np <- reticulate::import("numpy")
  cv2 <- reticulate::import("cv2")

  if (one_at_a_time) {
    # Segment each point individually -> N points = N masks
    # Use other points as negative prompts to help SAM distinguish objects

    results_list <- list()
    for (i in 1:nrow(points_df)) {

      if (use_negative_prompts && nrow(points_df) > 1) {
        # Current point as positive, all other points as negative
        positive_idx <- i
        negative_idx <- setdiff(1:nrow(points_df), i)

        # Combine positive and negative points
        # Format: [[x1, y1], [x2, y2], ...] with labels [1, 0, 0, ...]
        all_coords <- rbind(
          points_df[positive_idx, c("x_pixel", "y_pixel")],
          points_df[negative_idx, c("x_pixel", "y_pixel")]
        )

        # Labels: 1 for positive (foreground), 0 for negative (background)
        all_labels <- c(1L, rep(0L, length(negative_idx)))

      } else {
        # Just the single point as positive
        all_coords <- points_df[i, c("x_pixel", "y_pixel")]
        all_labels <- 1L
      }

      result <- model(
        source = image_path,
        points = np$array(as.matrix(all_coords)),
        labels = np$array(all_labels),
        verbose = FALSE
      )

      results_list[[i]] <- result
    }

    return(results_list)

  } else {
    # Segment all points together -> 1 mask for all points
    coords <- as.matrix(points_df[, c("x_pixel", "y_pixel")])
    labels <- rep(1L, nrow(coords))

    results <- model(
      source = image_path,
      points = np$array(coords),
      labels = np$array(labels),
      verbose = FALSE
    )

    return(results)
  }
}

# ============================================================================
# VISUALIZATION
# ============================================================================

# Plot image with SAM masks overlaid
# Can be called with either:
#   plot_sam_results(result)  # using result object from pipeline
#   plot_sam_results(segments, image_path, points_df)  # using individual components
plot_sam_results <- function(sam_results, image_path = NULL, points_df = NULL,
                             colors = c("#1E88E5", "#FFC107", "#D81B60",
                                       "#004D40", "#7CB342"),
                             alpha = 0.5) {

  # Check if first argument is a result object from the pipeline
  if (is.list(sam_results) && !is.null(sam_results$segments) &&
      !is.null(sam_results$image_path)) {
    # Extract components from result object
    result <- sam_results
    sam_results <- result$segments
    image_path <- result$image_path
    points_df <- result$points_pixel
  }

  # Validate arguments
  if (is.null(image_path)) {
    stop("image_path must be provided or sam_results must be a result object from moondream_sam_pipeline()")
  }

  # Read base image
  img <- magick::image_read(image_path)
  img_info <- magick::image_info(img)

  # Extract masks from ultralytics results
  # Handle both cases: list of results (one per point) or single result (all points)
  if (length(sam_results) > 0) {

    # Process each result
    mask_counter <- 0
    for (i in seq_along(sam_results)) {
      # Each result is a list containing a Python Results object at position [[1]]
      result_obj <- if (is.list(sam_results[[i]]) && length(sam_results[[i]]) > 0) {
        sam_results[[i]][[1]]  # Extract the actual Results object
      } else {
        sam_results[[i]]
      }

      # Check if it's a Python Results object with masks
      if (!inherits(result_obj, "python.builtin.object")) next
      if (!reticulate::py_has_attr(result_obj, "masks")) next
      if (is.null(result_obj$masks)) next

      # Get mask data - ultralytics stores as torch tensor
      masks_data <- result_obj$masks$data

      # Convert to R array
      np <- reticulate::import("numpy")
      if (inherits(masks_data, "python.builtin.object")) {
        masks_array <- reticulate::py_to_r(masks_data$cpu()$numpy())
      } else {
        masks_array <- masks_data
      }

      # masks_array shape is [n_masks, height, width]
      # For single point prompts, typically n_masks=3 (SAM returns 3 candidates)
      # We'll use the first/best mask
      n_masks_in_result <- dim(masks_array)[1]

      # Use only the best mask (first one) from each result
      mask <- masks_array[1, , , drop = TRUE]
      mask_counter <- mask_counter + 1

      # Ensure binary mask
      mask <- ifelse(mask > 0.5, 1, 0)

      # Resize mask to match original image if needed
      if (nrow(mask) != img_info$height || ncol(mask) != img_info$width) {
        mask_img <- magick::image_read(as.raster(mask))
        mask_img <- magick::image_resize(
          mask_img,
          paste0(img_info$width, "x", img_info$height, "!")
        )
        mask <- as.numeric(magick::image_data(mask_img, "gray")) > 0.5
        dim(mask) <- c(img_info$height, img_info$width)
      }

      # Create colored overlay [height, width, 4]
      col <- colors[((mask_counter - 1) %% length(colors)) + 1]
      rgb_vals <- grDevices::col2rgb(col)

      overlay <- array(0, dim = c(nrow(mask), ncol(mask), 4))
      overlay[, , 1] <- mask * rgb_vals[1, 1]
      overlay[, , 2] <- mask * rgb_vals[2, 1]
      overlay[, , 3] <- mask * rgb_vals[3, 1]
      overlay[, , 4] <- mask * 255 * alpha

      # Composite onto image
      img <- magick::image_composite(
        img,
        magick::image_read(overlay / 255),
        operator = "over"
      )
    }
  }

  # Add points if provided
  if (!is.null(points_df) && nrow(points_df) > 0) {
    img <- magick::image_draw(img)
    graphics::points(points_df$x_pixel, points_df$y_pixel,
                    pch = 19, cex = 2, col = "cyan")
    graphics::text(points_df$x_pixel, points_df$y_pixel,
                  labels = points_df$object_id,
                  pos = 3, col = "cyan", cex = 1)
    grDevices::dev.off()
  }

  plot(img)
  invisible(img)
}

# Plot a single mask by index
plot_single_mask <- function(result, mask_index,
                             color = "#1E88E5",
                             alpha = 0.5,
                             show_point = TRUE) {

  if (mask_index < 1 || mask_index > length(result$segments)) {
    stop("mask_index must be between 1 and ", length(result$segments))
  }

  # Read base image
  img <- magick::image_read(result$image_path)
  img_info <- magick::image_info(img)

  # Get the specific result
  result_obj <- if (is.list(result$segments[[mask_index]]) &&
                    length(result$segments[[mask_index]]) > 0) {
    result$segments[[mask_index]][[1]]
  } else {
    result$segments[[mask_index]]
  }

  # Check if it has masks
  if (!inherits(result_obj, "python.builtin.object") ||
      !reticulate::py_has_attr(result_obj, "masks") ||
      is.null(result_obj$masks)) {
    stop("No masks found for mask_index ", mask_index)
  }

  # Get mask data
  masks_data <- result_obj$masks$data
  np <- reticulate::import("numpy")

  if (inherits(masks_data, "python.builtin.object")) {
    masks_array <- reticulate::py_to_r(masks_data$cpu()$numpy())
  } else {
    masks_array <- masks_data
  }

  # Use the first/best mask
  mask <- masks_array[1, , , drop = TRUE]

  # Ensure binary mask
  mask <- ifelse(mask > 0.5, 1, 0)

  # Resize mask to match original image if needed
  if (nrow(mask) != img_info$height || ncol(mask) != img_info$width) {
    mask_img <- magick::image_read(as.raster(mask))
    mask_img <- magick::image_resize(
      mask_img,
      paste0(img_info$width, "x", img_info$height, "!")
    )
    mask <- as.numeric(magick::image_data(mask_img, "gray")) > 0.5
    dim(mask) <- c(img_info$height, img_info$width)
  }

  # Create colored overlay
  rgb_vals <- grDevices::col2rgb(color)

  overlay <- array(0, dim = c(nrow(mask), ncol(mask), 4))
  overlay[, , 1] <- mask * rgb_vals[1, 1]
  overlay[, , 2] <- mask * rgb_vals[2, 1]
  overlay[, , 3] <- mask * rgb_vals[3, 1]
  overlay[, , 4] <- mask * 255 * alpha

  # Composite onto image
  img <- magick::image_composite(
    img,
    magick::image_read(overlay / 255),
    operator = "over"
  )

  # Add point if requested
  if (show_point && !is.null(result$points_pixel)) {
    point_data <- result$points_pixel[mask_index, ]

    img <- magick::image_draw(img)
    graphics::points(point_data$x_pixel, point_data$y_pixel,
                    pch = 19, cex = 2, col = "cyan")
    graphics::text(point_data$x_pixel, point_data$y_pixel,
                  labels = point_data$object_id,
                  pos = 3, col = "cyan", cex = 1)
    grDevices::dev.off()
  }

  plot(img)
  invisible(img)
}

# ============================================================================
# MAIN PIPELINE
# ============================================================================

# Complete moondream → SAM pipeline
moondream_sam_pipeline <- function(image_path,
                                   object_prompt,
                                   sam_model = NULL,
                                   model_type = "sam2.1_b.pt",
                                   api_key = Sys.getenv("MOONDREAM_API_KEY"),
                                   use_local = FALSE,
                                   use_negative_prompts = TRUE,
                                   plot = TRUE) {

  message("Step 1: Detecting points with moondream...")
  points <- detect_points_moondream(
    image_path = image_path,
    object_prompt = object_prompt,
    api_key = api_key,
    use_local = use_local
  )

  message("Found ", nrow(points), " points")

  if (nrow(points) == 0) {
    message("No points detected, stopping pipeline")
    return(invisible(list(
      image_path = image_path,
      prompt = object_prompt,
      points = points,
      segments = NULL
    )))
  }

  message("Step 2: Converting coordinates to pixels...")
  points_pixel <- normalized_to_pixel(points, image_path)

  message("Step 3: Initializing SAM model...")
  if (is.null(sam_model)) {
    sam_model <- init_sam(model_type)
  }

  message("Step 4: Running SAM segmentation...")
  if (use_negative_prompts) {
    message("  Using other points as negative prompts...")
  }
  segments <- segment_with_sam(sam_model, image_path, points_pixel,
                               use_negative_prompts = use_negative_prompts)

  result <- list(
    image_path = image_path,
    prompt = object_prompt,
    points = points,
    points_pixel = points_pixel,
    segments = segments,
    model = sam_model
  )

  if (plot) {
    message("Step 5: Plotting results...")
    plot_sam_results(result)
  }

  invisible(result)
}

# ============================================================================
# USAGE EXAMPLE
# ============================================================================

# Example usage:
result <- moondream_sam_pipeline(
 image_path = ('../amazon/data/derived/itenez_tiles/tile_768/tile_768_58/geology_chip.png'),
 object_prompt = "magenta patches",
 use_negative_prompts = TRUE  # Use other points as negative prompts (default)
)
#
# To disable negative prompts:
# result <- moondream_sam_pipeline(
#   image_path = "...",
#   object_prompt = "...",
#   use_negative_prompts = FALSE
# )
#
# Access components:
# result$points         # Original normalized coordinates
# result$points_pixel   # Pixel coordinates
# result$segments       # SAM segmentation results
#
# Plot all masks together:
plot_sam_results(result)                       # All masks with default colors
#
# Plot individual masks:
#plot_single_mask(result, 1)                    # Plot first mask
#plot_single_mask(result, 3, color = "red")     # Plot third mask in red
#plot_single_mask(result, 2, alpha = 0.7)       # Plot second mask more opaque

