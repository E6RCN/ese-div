library(palaeoverse)
library(deeptime)
library(tidyverse)

load("Data/Phanerozoic_clean_final.RData")
load("Data/marine_ecosystem_engineer_occs.RData")

all_data_grid_100 <- bin_space(all_data, spacing = 100)
all_data_grid_100$cell_size <- 100
all_data_grid_275 <- bin_space(all_data, spacing = 275)
all_data_grid_275$cell_size <- 275
all_data_grid_725 <- bin_space(all_data, spacing = 725)
all_data_grid_725$cell_size <- 725

all_data_grid_long <- rbind(all_data_grid_100,
                            all_data_grid_275,
                            all_data_grid_725)
all_data_grid <- pivot_wider(
    all_data_grid_long,
    names_from  = cell_size,
    values_from = c(cell_ID, cell_centroid_lng, cell_centroid_lat)
  )

all_data_grid_stage <- bin_time(all_data_grid, bins = time_bins(), method = "mid") # majority?
all_data_grid_long_stage <- bin_time(all_data_grid_long, bins = time_bins(), method = "mid") # majority?

save(all_data_grid_stage, file = "Data/all_data_grid_stage.RData")
saveRDS(all_data_grid_long_stage, file = "Data/all_data_grid_long_stage.RDS", compress = "bzip2")

cols <- all_data_grid_long_stage |>
  # filter to time-space combinations with greater than or equal to 10 occurrences
  group_by(cell_size, cell_ID, bin_assignment) |>
  filter(n() >= 10) |>
  # filter to grid cells with more than one time slice
  group_by(cell_size, cell_ID) |>
  filter(n() > 1) |>
  # get a single row for each time-space cell
  group_by(cell_size, cell_ID, bin_assignment) |>
  slice(1) |>
  select(cell_size, cell_ID, cell_centroid_lng, cell_centroid_lat, bin_assignment, bin_midpoint) |>
  # calculate the # of stages between time slices within each column
  group_by(cell_size, cell_ID) |>
  arrange(bin_assignment) |>
  mutate(bin_diff = c(diff(bin_assignment), 0)) |>
  ungroup() |>
  arrange(cell_ID, bin_assignment)

cols_spans <- cols |>
  group_by(cell_size, cell_ID) |>
  group_split() |>
  map(\(item) {
    diffs <- item$bin_diff
    # collapse any gap larger than one bin into a single break marker
    diffs[diffs > 1] <- 2
    diffs_paste <- paste0(diffs, collapse = "")

    # split on the break marker; nchar of each remaining run = number of consecutive bins
    data.frame(cell_size = unique(item$cell_size), cell_ID = unique(item$cell_ID),
               span = nchar(strsplit(diffs_paste, "2")[[1]]) + 1)
  }) |>
  list_rbind()

cols_spans |>
  ggplot() +
  geom_histogram(aes(x = span), binwidth = 1) +
  facet_wrap(vars(cell_size), ncol = 1, scales = "free_y") +
  theme_classic(base_size = 18)

ggsave("Plots/span_hists.png")

cols_spans |>
  group_by(cell_size) |>
  reframe({
    tab <- table(cut(span, c(0, 4, 9, 10000)))
    t(tab)
  })

