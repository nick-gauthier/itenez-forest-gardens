library(tidyverse)
library(furrr)
library(here)
library(ellmer)

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
  withr::local_options(ellmer_connecttimeout_s = 50 * 60,
                       ellmer_timeout_s = 50 * 60)

  chat <- chat_openai(model = model, system_prompt = prompt, params = list(seed = seed))

  parallel_chat_structured(chat, imgs, type_number(), max_active = 30)
}


# Define model mapping
model_mapping <- c(
  nano = 'gpt-4.1-nano',
  mini = 'gpt-4.1-mini',
  gpt_4.1 = 'gpt-4.1',
  o4_mini = 'o4-mini',
  o3 = 'o3'
)

plan(multisession, workers = 2)
results <- tiles |>
  mutate(
    id = 1:n(),
    !!!future_map(model_mapping, ~ade_counter(img_prompts, system_prompt, .x))
  )
plan(sequential)

saveRDS(results, here('data/derived/results.rds'))

source(here('R/moondream.R'))

plan(multisession, workers = 3)
safe_detect_anthrosols <- safely(detect_anthrosols)
moon_41_test <- future_map(triplets, ~safe_detect_anthrosols(.x, model = 'gpt-4.1'), .progress = TRUE)
plan(sequential)
saveRDS(moon_41_test, 'moon_41.rds')
