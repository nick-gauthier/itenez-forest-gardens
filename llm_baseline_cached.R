library(tidyverse)
library(here)
library(ellmer)
library(furrr)
library(sf)

tiles <- readRDS(here('data/derived/tiles_768_sf.rds'))

get_sorted_files <- function(pattern, dir = here('data/derived/tile_768')) {
  list.files(dir, recursive = TRUE, pattern = pattern, full.names = TRUE) |>
    gtools::mixedsort()
}

create_image_triplets <- function() {
  img_paths <- c('geology', 'moisture', 'canopy') |>
    paste0('_chip.jpg') |>
    map(get_sorted_files)
  # Create list of triplets
  n_images <- length(img_paths[[1]])
  map(1:n_images, ~list(
    c1 = img_paths[[1]][.x],
    c2 = img_paths[[2]][.x],
    chm = img_paths[[3]][.x]
  ))
}

triplets <- create_image_triplets()

img_prompts <- triplets |>
  map(~{
    list(
      "C1:", content_image_file(.x$c1, resize = 'none'),
      "---",
      "C2:", content_image_file(.x$c2, resize = 'none'),
      "---",
      "CHM:", content_image_file(.x$chm, resize = 'none')
    )
  })

system_prompt <- read_file(here('prompts/prompt_anthrosol_counter.md'))

ade_counter <- function(imgs, prompt, model = 'o4-mini', seed = 12345){
  withr::local_options(ellmer_connecttimeout_s = 5 * 60,
                       ellmer_timeout_s = 5 * 60,
                       ellmer_max_tries = 20)
  chat <- ellmer::chat_openai(model = model, system_prompt = prompt,
                              params = list(seed = seed),
                              base_url = "https://api.ai.it.ufl.edu")
  ellmer::parallel_chat_structured(chat, imgs, type_number(), max_active = 5)
}

# Define model mapping
model_mapping <- c(
 # nano = 'gpt-4.1-nano',
  mini = 'gpt-4.1-mini',
  gpt_41 = 'gpt-4.1',
  o4_mini = 'o4-mini',
  gpt5_nano = 'gpt-5-nano',
  gpt5_mini = 'gpt-5-mini',
  gpt5 = 'gpt-5',
  claude = 'claude-4-sonnet'
)

# Cached version of ade_counter
ade_counter_cached <- function(model_name, model_id, imgs, prompt) {
  cache_file <- here(glue::glue('data/derived/cache_{model_name}.rds'))

  # Return cached if exists
  if (file.exists(cache_file)) {
    message(glue::glue("Loading cached results for {model_name}"))
    return(readRDS(cache_file))
  }

  # Otherwise compute
  message(glue::glue("Computing results for {model_name}"))
  result <- ade_counter(imgs, prompt, model = model_id)

  # Save to cache
  saveRDS(result, cache_file)
  message(glue::glue("Saved cache for {model_name}"))

  return(result)
}

# Process models (with caching)
plan(multisession, workers = 2)

model_results <- future_map(
  names(model_mapping),
  ~ade_counter_cached(.x, model_mapping[[.x]], img_prompts, system_prompt),
  .options = furrr_options(seed = TRUE)
)
names(model_results) <- names(model_mapping)

plan(sequential)

# Combine into final results
results <- tiles |>
  mutate(
    id = 1:n(),
    !!!model_results
  )

saveRDS(results, here('data/derived/results_new.rds'))
