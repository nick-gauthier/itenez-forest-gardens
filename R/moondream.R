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
  chat <- chat_openai(model = model, system_prompt = prompt_moondream, params = list(temperature = 0.1), echo = echo)

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

# Main detection function
detect_anthrosols <- function(imgs, bbox = NULL, model = "gpt-4.1-mini", echo = FALSE) {

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
  points = type_array('normalized patch coordinates',
    items = type_object(
      x = type_number(),
      y = type_number())),
  detection_summary = type_object(
    prompts = type_array('most successful prompt(s)', items = type_string(), required = FALSE),
    notes = type_string("≤5 concise sentences summarizing reasoning", required = FALSE),
  )
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

plot_detections <- function(result, title = "Detections") {
  image_path <- result$image_path

  # Read image
  if (grepl("\\.png$", image_path, ignore.case = TRUE)) {
    img <- png::readPNG(image_path)
  } else {
    img <- jpeg::readJPEG(image_path)
  }

  # Get dimensions
  dims <- dim(img)
  height <- dims[1]
  width <- dims[2]

  # Plot image
  par(mar = c(0, 0, 2, 0))
  plot(0, 0, type = "n", xlim = c(0, width), ylim = c(0, height),
       xlab = "", ylab = "", axes = FALSE, main = title, asp = 1)
  rasterImage(img, 0, 0, width, height)

  # Add points (convert normalized coords to pixels)
  if (length(result$points) > 0) {
    x_coords <- map_dbl(result$points$x, ~.x * width)
    y_coords <- map_dbl(result$points$y, ~(1 - .x) * height)  # Flip Y

    #points(x_coords, y_coords, col = rgb(0, 0, 0.5, alpha = 0.6), pch = 19, cex = 4)
    points(x_coords, y_coords, col = "cyan", pch = 19, cex = 1)
  }
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
