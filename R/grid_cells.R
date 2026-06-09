library(palaeoverse)
library(deeptime)
library(tidyverse)

load("Data/Phanerozoic_clean_final.RData")
load("Data/marine_ecosystem_engineer_occs.RData")

all_data_grid <- bin_space(all_data, spacing = 500)

all_data_grid_stage <- bin_time(all_data_grid, bins = time_bins(), method = "mid") # majority?

cols <- all_data_grid_stage |>
  # filter to time-space combinations with greater than or equal to 10 occurrences
  group_by(cell_ID, bin_assignment) |>
  filter(n() >= 10) |>
  # filter to grid cells with more than one time slice
  group_by(cell_ID) |>
  filter(n() > 1) |>
  # get a single row for each time-space cell
  group_by(cell_ID, bin_assignment) |>
  slice(1) |>
  select(cell_ID, cell_centroid_lng, cell_centroid_lat, bin_assignment, bin_midpoint) |>
  # calculate the # of stages between time slices within each column
  group_by(cell_ID) |>
  arrange(bin_assignment) |>
  mutate(bin_diff = c(0, diff(bin_assignment))) |>
  ungroup() |>
  arrange(cell_ID, bin_assignment)

# split by 
cols_split <- split.data.frame(cols, ~ cell_ID)
cols_spans <- lapply(cols_split, function(item) {
  diffs <- item$bin_diff
  diffs[diffs > 1] <- 2
  diffs_paste <- paste0(diffs, collapse = "")

  sapply(strsplit(diffs_paste, "2")[[1]], nchar)
})

spans <- do.call(c, cols_spans)
spans |> hist(breaks = seq(0, 30))
table(cut(spans, c(0, 4, 9, 10000)))
