# remotes::install_packages("bergant/datamodelr")
library(datamodelr)
library(rlang)
devtools::load_all(".")

ds <- read_pprof("tests/testthat/proto/1.out.prof.pb.gz")

dm <- rlang::invoke(dm_from_data_frames, keep(ds, tibble::is_tibble))
dm_r <- dm_add_references(
  dm,

  samples$locations == locations$location_id,
  locations$function_id == functions$function_id
)
graph <- dm_create_graph(dm_r, rankdir = "LR", col_attr = c("column", "type"))
dm_render_graph(graph)
dm_export_graph(graph, "man/figures/dm.png")
