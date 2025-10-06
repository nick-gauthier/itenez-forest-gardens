library(ellmer)
library(tidyverse)
library(httr2)
library(jsonlite)
library(openssl)

moondream_api <- function(image_b64, prompt, endpoint = c("query", "point"),
                          use_local = FALSE, api_key = NULL) {
  endpoint <- match.arg(endpoint)

  # Get API key from env if not provided
  api_key <- api_key %||% Sys.getenv("MOONDREAM_API_KEY", unset = NA)

  config <- list(
    base_url = c("https://api.moondream.ai/v1/", "http://localhost:2020/v1/")[use_local + 1],
    param_name = c(query = "question", point = "object")[[endpoint]],
    needs_auth = !use_local && !is.na(api_key)
  )

  req <- request(str_c(config$base_url, endpoint)) |>
    req_body_json(set_names(list(image_b64, prompt), c("image_url", config$param_name)))

  req <- if(config$needs_auth) {
    req |> req_headers(`X-Moondream-Auth` = api_key)
  } else {
    req
  }

  req |> req_perform() |> resp_body_json()
}


# Set up once at the start of detect_anthrosols()
setup_moondream_tools <- function(ellmer_content) {

  # Extract base64 for moondream (need to check the actual slot name)
  image_b64 <- paste0("data:", ellmer_content@type,
                      ";base64,",
                      ellmer_content@data)  # or whatever the slot is called

  # Return bound functions that don't need image parameter
  list(
    moondream_point = function(prompt) moondream_api(image_b64, prompt, 'point')$points,
    moondream_query = function(question) moondream_api(image_b64, question, "query")$answer
  )
}

# Create anthrosol detector function
create_anthrosol_detector <- function(image, model = "gpt-4.1-mini", echo = FALSE) {
  # Create chat detector
  prompt_moondream <- read_file(here::here('prompts/prompt_moondream.md'))
  chat <- chat_openai(model = model,
                      system_prompt = prompt_moondream,
                      params = list(temperature = 0.1, seed = 42),
                      echo = echo,
                      base_url = "https://api.ai.it.ufl.edu")

  moondream_tools <- setup_moondream_tools(image)

  # Register Moondream point detection tool
  chat$register_tool(
   tool(
     moondream_tools$moondream_point,
     name = 'moondream_point',
    description = "Detect objects in the C1 image using Moondream API. Returns center points.",
     arguments = list(
       prompt = type_string("Simple description of what to detect as single character string, e.g. 'red spots', 'pink patches', 'bright blobs'")
     )
   )
  )

  # Register Moondream query tool
  # chat$register_tool(tool(
  #   moondream_tools$moondream_query,
  #   "Ask Moondream to describe the C1 image in simple terms",
  #   question = type_string("Question about the image, e.g. 'What shapes and colors do you see?'")
  # ))

  chat
}

# Extract mask from SAM result
extract_sam_mask <- function(result_obj) {
  if (!inherits(result_obj, "python.builtin.object") ||
      !reticulate::py_has_attr(result_obj, "masks") ||
      is.null(result_obj$masks)) {
    return(NULL)
  }

  np <- reticulate::import("numpy")
  masks_data <- result_obj$masks$data

  if (inherits(masks_data, "python.builtin.object")) {
    masks_array <- reticulate::py_to_r(masks_data$cpu()$numpy())
  } else {
    masks_array <- masks_data
  }

  # Use first/best mask
  mask <- masks_array[1, , , drop = TRUE]
  mask <- ifelse(mask > 0.5, 1, 0)

  mask
}

# Segment detected anthrosols with SAM
segment_anthrosols <- function(results,
                               sam_model = NULL,
                               model_type = "sam2.1_b.pt",
                               use_negative_prompts = TRUE) {

  # Source SAM pipeline functions if not already loaded
  if (!exists("init_sam", mode = "function")) {
    source(here::here("moondream_ultralytics_sam.R"))
  }

  # Check if we have points to segment
  if (is.null(results$points) || length(results$points) == 0) {
    message("No points to segment")
    return(results)
  }

  # Convert points list to tibble for SAM pipeline
  # Handle both list of lists and already-flattened format
  points_df <- if (is.data.frame(results$points)) {
    # Already a data frame
    results$points
  } else if (is.list(results$points) && !is.null(names(results$points)) && "x" %in% names(results$points)) {
    # Single point as named list
    tibble::tibble(
      object_id = 1,
      x = results$points$x,
      y = results$points$y
    )
  } else {
    # List of point objects
    tibble::tibble(
      object_id = seq_along(results$points),
      x = purrr::map_dbl(results$points, ~ if(is.list(.x)) .x$x else .x["x"]),
      y = purrr::map_dbl(results$points, ~ if(is.list(.x)) .x$y else .x["y"])
    )
  }

  message("Segmenting ", nrow(points_df), " detected anthrosols with SAM...")

  # Initialize SAM if needed
  if (is.null(sam_model)) {
    sam_model <- init_sam(model_type)
  }

  # Convert to pixel coordinates
  points_pixel <- normalized_to_pixel(points_df, results$image_path)

  # Run SAM segmentation
  segments <- segment_with_sam(
    sam_model,
    results$image_path,
    points_pixel,
    use_negative_prompts = use_negative_prompts
  )

  # Extract masks from each segment
  masks <- list()
  for (i in seq_along(segments)) {
    # Extract mask from ultralytics result
    result_obj <- if (is.list(segments[[i]]) && length(segments[[i]]) > 0) {
      segments[[i]][[1]]
    } else {
      segments[[i]]
    }

    masks[[i]] <- extract_sam_mask(result_obj)
  }

  # Add to results
  results$segments <- segments
  results$masks <- masks
  results$sam_model <- sam_model

  message("Extracted ", sum(!purrr::map_lgl(masks, is.null)), " masks")

  results
}

# Create quick visualization of points on image for review
create_review_viz <- function(points, image_path,
                               point_color = "cyan", point_cex = 2) {
  if (is.null(points) || length(points) == 0) return(NULL)

  # Read image
  img <- magick::image_read(image_path)
  img_info <- magick::image_info(img)
  width <- img_info$width
  height <- img_info$height

  # Draw points
  img <- magick::image_draw(img)
  x_coords <- purrr::map_dbl(points, ~ if(is.list(.x)) .x$x else .x["x"]) * width
  y_coords <- purrr::map_dbl(points, ~ if(is.list(.x)) .x$y else .x["y"]) * height
  graphics::points(x_coords, y_coords, pch = 19, cex = point_cex, col = point_color)

  # Add numbering
  graphics::text(x_coords, y_coords, labels = seq_along(points),
                pos = 3, col = point_color, cex = 1.2, font = 2)
  grDevices::dev.off()

  # Save to temp file
  temp_path <- tempfile(fileext = ".png")
  magick::image_write(img, temp_path)
  temp_path
}

# Main detection function
detect_anthrosols <- function(imgs, bbox = NULL, model = "gpt-5-mini", echo = FALSE,
                              segment = TRUE, sam_model = NULL) {

  # Encode images
  c1_ellmer <- content_image_file(imgs$c1, resize = 'none')
  c2_ellmer <- content_image_file(imgs$c2, resize = 'none')
  chm_ellmer <- content_image_file(imgs$chm, resize = 'none')

  # Create detector
  detector <- create_anthrosol_detector(c1_ellmer, model, echo = echo)

  # Initialize detection
  detector$chat('C1:', c1_ellmer, '\nC2:', c2_ellmer, '\nCHM:', chm_ellmer)

  results_json <- detector$chat_structured('Generate outputs',
    type = type_detection_results
  )

  # Extract successful prompts from chat history and save them
#if (length(results_json$detection_summary$prompts) > 0) {
#   walk(results_json$detection_summary$prompts, ~remember_prompts(.x, "anthrosol detection"))
#}

#Add GeoJSON if bbox provided and points found
if (!is.null(bbox) && length(results_json$points) > 0) {
 results_json$geojson <- create_geojson(results_json$points, bbox)
}

  results_json$image_path <- imgs$c1

  # Segment with SAM if requested
  if (segment && !is.null(results_json$points) && length(results_json$points) > 0) {
    results_json <- segment_anthrosols(results_json, sam_model = sam_model)
  }

  return(results_json)
}



# result <- fromJSON(result_json)
#
# # Add GeoJSON if bbox provided
# if (!is.null(bbox) && length(result$points) > 0) {
#   features <- result$points %>%
#     map(~list(
#       type = "Feature",
#       geometry = list(
#         type = "Point",
#         coordinates = c(
#           bbox[1] + (bbox[3] - bbox[1]) * .x$x,
#           bbox[4] - (bbox[4] - bbox[2]) * .x$y
#         )
#       )
#     ))
#
#   result$geojson <- toJSON(list(
#     type = "FeatureCollection",
#     features = features
#   ), auto_unbox = TRUE)
# }


type_detection_results <- type_object(
  count = type_number('patch count'),
  points = type_array(
    items = type_object(
      x = type_number(),
      y = type_number()),
    'normalized patch coordinates')#,
 # detection_summary = type_object(
#    prompts = type_array(items = type_string(), 'most successful prompt(s)', required = FALSE),
#    notes = type_string("≤5 concise sentences summarizing reasoning", required = FALSE))
)


#' Append successful prompt to memory file
#'
#' @param prompt The prompt that worked well
#' @param context Brief description of what it detected
#' @param success_rate Estimated success rate (0-1)
#' @return Success message
remember_prompts <- function(prompt, context = "", success_rate = NA) {
  memory_file <- "anthrosol_prompt_memory.md"

  # Create file if doesn't exist
  if (!file.exists(memory_file)) {
    writeLines("# Anthrosol Detection Prompt Memory\n", memory_file)
  }

  # Append new entry
  entry <- sprintf(
    "\n## %s\n- **Prompt**: `%s`\n- **Context**: %s\n- **Success Rate**: %s\n",
    Sys.time(),
    prompt,
    context,
    ifelse(is.na(success_rate), "Unknown", paste0(round(success_rate * 100), "%"))
  )

  write(entry, file = memory_file, append = TRUE)
  paste("Added prompt to memory:", prompt)
}

plot_detections <- function(result, title = "Detections",
                            show_masks = TRUE,
                            show_points = TRUE,
                            mask_colors = c("#1E88E5", "#FFC107", "#D81B60",
                                           "#004D40", "#7CB342"),
                            mask_alpha = 0.5,
                            point_color = "cyan",
                            point_cex = 1) {
  image_path <- result$image_path

  # Read image with magick for easier compositing
  img <- magick::image_read(image_path)
  img_info <- magick::image_info(img)
  width <- img_info$width
  height <- img_info$height

  # Draw SAM masks if available
  if (show_masks && !is.null(result$masks) && length(result$masks) > 0) {
    for (i in seq_along(result$masks)) {
      mask <- result$masks[[i]]
      if (is.null(mask)) next

      # Get color for this mask
      col <- mask_colors[((i - 1) %% length(mask_colors)) + 1]
      rgb_vals <- grDevices::col2rgb(col)

      # Create colored overlay [height, width, 4]
      overlay <- array(0, dim = c(nrow(mask), ncol(mask), 4))
      overlay[, , 1] <- mask * rgb_vals[1, 1]
      overlay[, , 2] <- mask * rgb_vals[2, 1]
      overlay[, , 3] <- mask * rgb_vals[3, 1]
      overlay[, , 4] <- mask * 255 * mask_alpha

      # Composite onto image
      img <- magick::image_composite(
        img,
        magick::image_read(overlay / 255),
        operator = "over"
      )
    }
  }

  # Add points if requested
  if (show_points && !is.null(result$points) && length(result$points) > 0) {
    img <- magick::image_draw(img)
    x_coords <- purrr::map_dbl(result$points, ~ if(is.list(.x)) .x$x else .x["x"]) * width
    y_coords <- purrr::map_dbl(result$points, ~ if(is.list(.x)) .x$y else .x["y"]) * height
    graphics::points(x_coords, y_coords, pch = 19, cex = point_cex, col = point_color)
    grDevices::dev.off()
  }

  # Plot
  plot(img, main = title)
  invisible(img)
}

# Load prompt memory helper
load_prompt_memory <- function() {
  memory_file <- "~/.anthrosol_prompt_memory.md"
  if (file.exists(memory_file)) {
    readLines(memory_file)
  } else {
    "No prompt memory found."
  }
}

# GeoJSON creator
create_geojson <- function(points, bbox) {
  features <- points %>%
    map(~{
      list(
        type = "Feature",
        geometry = list(
          type = "Point",
          coordinates = c(
            bbox[1] + (bbox[3] - bbox[1]) * .x$x,
            bbox[4] - (bbox[4] - bbox[2]) * .x$y
          )
        ),
        properties = list(
          normalized = c(.x$x, .x$y)
        )
      )
    })

  toJSON(list(
    type = "FeatureCollection",
    features = features
  ), auto_unbox = TRUE, pretty = TRUE)
}
