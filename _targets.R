

library(targets)
library(tarchetypes)

tar_option_set(packages = c("data.table", "geosphere", "lubridate", "robustbase", "rstatix",
                            "viridisLite", "ggplot2", "ggmap", "bit64", "quarto"))

options(clustermq.scheduler = "multiprocess")
tar_source()

tarchetypes::tar_plan(
  url = "https://divvy-tripdata.s3.amazonaws.com/",
  tarchetypes::tar_fst_dt(raw.tot_data, get.raw_data(url, Timeout = 200)),
  tarchetypes::tar_fst_dt(tot_data, raw.tot_data |> rm.outliers() |> rm.missing()),
  eda_plots = get_box(tot_data),
  eda_stats = get_stats(tot_data),
  cols = get_cols(eda_stats),
  maps = get_maps(tot_data),
  tarchetypes::tar_quarto(report, "report.qmd")
)
