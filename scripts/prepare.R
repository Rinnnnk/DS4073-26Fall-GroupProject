# Usage from the project root: Rscript scripts/prepare.R RAW_EVENT_DIR SOURCE_DIR
# SOURCE_DIR contains the Census summary tables, DCP tracts and MTA snapshot.
suppressPackageStartupMessages({
  library(data.table)
  library(sf)
})
projection_data <- Sys.getenv("NYC_PROJ_DIR")
if (nzchar(projection_data)) sf_proj_search_paths(projection_data)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
raw <- args[1L]
source_dir <- args[2L]
out <- "data"
dir.create(out, showWarnings = FALSE)
boroughs <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")

read_acs <- function(file, fields) {
  name <- paste0("acs-", file)
  path <- file.path(source_dir, paste0(name, ".dat"))
  archive <- file.path(source_dir, paste0(name, ".zip"))
  if (!file.exists(path) && file.exists(archive)) {
    scratch <- tempfile("nyc-acs-")
    dir.create(scratch)
    on.exit(unlink(scratch, recursive = TRUE))
    path <- unzip(archive, files = paste0(name, ".dat"), exdir = scratch)
  }
  tbl <- fread(path, sep = "|", select = c("GEO_ID", fields))
  tbl <- tbl[grepl("^1400000US36[0-9]{9}$", GEO_ID)]
  tbl[, GEOID := sub("^1400000US", "", GEO_ID)]
  tbl[, GEO_ID := NULL]
  # Census special negative values convey suppression or other unavailability.
  tbl[, (fields) := lapply(.SD, function(x) fifelse(x >= 0, as.numeric(x), NA_real_)), .SDcols = fields]
  stopifnot(!anyDuplicated(tbl$GEOID))
  tbl
}

shapes <- st_read(file.path(source_dir, "tracts.geojson"), quiet = TRUE)
stopifnot(nrow(shapes) == 2325L, !anyDuplicated(shapes$geoid))
shapes <- st_make_valid(st_transform(shapes, 2263))
tracts <- data.table(
  GEOID = shapes$geoid, borough = shapes$boroname, tract = shapes$ct2020,
  NTA = shapes$ntaname, area_km2 = as.numeric(units::set_units(st_area(shapes), km^2))
)
tracts[, label := paste(borough, "tract", tract)]
for (spec in list(c("b01003", "B01003_E001", "population"), c("b19013", "B19013_E001", "income"),
                  c("b25064", "B25064_E001", "rent"))) {
  counts <- read_acs(spec[1], spec[2])
  setnames(counts, spec[2], spec[3])
  tracts[counts, (spec[3]) := get(paste0("i.", spec[3])), on = "GEOID"]
}
burden_fields <- sprintf("B25070_E%03d", c(1L, 7:11))
burden <- read_acs("b25070", burden_fields)
burden[, computable := B25070_E001 - B25070_E011]
burden[, burden_30 := rowSums(.SD), .SDcols = sprintf("B25070_E%03d", 7:10)]
burden[, burden_rate := fifelse(computable > 0, burden_30 / computable, NA_real_)]
tracts[burden, `:=`(computable = i.computable, burden_rate = i.burden_rate), on = "GEOID"]
stopifnot(all(tracts$burden_rate >= 0 & tracts$burden_rate <= 1, na.rm = TRUE),
          all(tracts$borough %in% boroughs))

read_events <- function(type, date_field, id_field) {
  months <- sprintf("%s-2024-%02d.csv", type, 1:12)
  result <- rbindlist(lapply(seq_along(months), function(i) {
    input <- fread(file.path(raw, months[i]), colClasses = list(character = c(id_field, date_field)))
    stopifnot(nrow(input) > 0L, all(substr(input[[date_field]], 1L, 7L) == sprintf("2024-%02d", i)))
    input[, month := i]
  }))
  stopifnot(!anyNA(result[[id_field]]), !anyDuplicated(result[[id_field]]))
  result
}

assign_tract <- function(tbl) {
  # Count missing or out-of-region coordinates separately from point-in-polygon failures.
  has_xy <- is.finite(tbl$longitude) & is.finite(tbl$latitude)
  near_nyc <- has_xy & tbl$longitude >= -74.35 & tbl$longitude <= -73.65 &
    tbl$latitude >= 40.47 & tbl$latitude <= 40.97
  xy <- tbl[near_nyc, .(longitude, latitude)]
  pts <- st_transform(st_as_sf(xy, coords = c("longitude", "latitude"), crs = 4326), 2263)
  idx <- st_within(pts, shapes)
  one <- lengths(idx) == 1L
  tbl[, GEOID := NA_character_]
  tbl[which(near_nyc)[one], GEOID := shapes$geoid[unlist(idx[one])]]
  data.table(raw_rows = nrow(tbl), coordinate_pairs = sum(has_xy),
             within_bbox = sum(near_nyc), unique_tract = sum(one), ambiguous = sum(lengths(idx) > 1L))
}

noise <- read_events("noise", "created_date", "unique_key")
nypd <- read_events("nypd", "cmplnt_fr_dt", "cmplnt_num")
noise_qa <- assign_tract(noise)
nypd_qa <- assign_tract(nypd)
noise[, date := as.IDate(substr(created_date, 1L, 10L))]
noise[, hour := as.integer(substr(created_date, 12L, 13L))]
stopifnot(!anyNA(noise$date), !anyNA(noise$hour), all(noise$hour %in% 0:23))
noise[, weekday := (as.POSIXlt(date)$wday + 6L) %% 7L + 1L]
noise[, late := hour >= 22L | hour < 6L]
noise[, type := fcase(
  complaint_type == "Noise - Residential", "Residential",
  complaint_type == "Noise - Street/Sidewalk", "Street/sidewalk",
  complaint_type == "Noise - Commercial", "Commercial",
  complaint_type == "Noise - Vehicle", "Vehicle",
  default = "Other noise"
)]
boro_lookup <- setNames(boroughs, toupper(boroughs))
noise[, reported_borough := unname(boro_lookup[toupper(trimws(borough))])]
noise[tracts, mapped_borough := i.borough, on = "GEOID"]
noise[, time_borough := fcoalesce(mapped_borough, reported_borough)]
# City time totals keep every selected request. By-borough views exclude requests
# without a declared or spatially assigned borough.
time_summary <- rbindlist(list(
  noise[, .N, by = .(type, month, weekday, hour)][, borough := "All NYC"],
  noise[!is.na(time_borough), .N, by = .(borough = time_borough, type, month, weekday, hour)]
), use.names = TRUE)
stopifnot(time_summary[borough == "All NYC", sum(N)] == nrow(noise))
stopifnot(all(time_summary$borough %in% c("All NYC", boroughs)))
noise_hour <- noise[!is.na(GEOID), .N, by = .(GEOID, hour)]
tracts[noise[!is.na(GEOID), .(noise_count = .N, night_count = sum(late)), by = GEOID],
       `:=`(noise_count = i.noise_count, night_count = i.night_count), on = "GEOID"]
nypd[, law := fcase(law_cat_cd == "FELONY", "Felony", law_cat_cd == "MISDEMEANOR", "Misdemeanor",
                   law_cat_cd == "VIOLATION", "Violation", default = "Other/unknown")]
tracts[nypd[!is.na(GEOID), .(nypd_count = .N), by = GEOID], nypd_count := i.nypd_count, on = "GEOID"]
nypd[tracts, mapped_borough := i.borough, on = "GEOID"]
law_summary <- nypd[!is.na(mapped_borough), .N, by = .(borough = mapped_borough, law)]
setnafill(tracts, fill = 0, cols = c("noise_count", "night_count", "nypd_count"))
stopifnot(sum(tracts$noise_count) == noise_qa$unique_tract,
          sum(tracts$nypd_count) == nypd_qa$unique_tract, sum(law_summary$N) == nypd_qa$unique_tract)

stations <- as.data.table(jsonlite::fromJSON(file.path(source_dir, "mta.json")))
stations <- stations[, .(complex_id, display_name, daytime_routes, borough,
                         latitude = as.numeric(latitude), longitude = as.numeric(longitude))]
stopifnot(!anyDuplicated(stations$complex_id), all(is.finite(stations$latitude)), all(is.finite(stations$longitude)))
station_sf <- st_transform(st_as_sf(stations, coords = c("longitude", "latitude"), crs = 4326), 2263)
representative <- suppressWarnings(st_point_on_surface(shapes))
nearest <- st_nearest_feature(representative, station_sf)
distance <- st_distance(representative, station_sf[nearest, ], by_element = TRUE)
tracts[, station_m := as.numeric(units::set_units(distance, m))]
tracts[, station_name := stations$display_name[nearest]]
tracts[, density := fifelse(area_km2 > 0, population / area_km2, NA_real_)]
for (k in c("noise", "night", "nypd")) {
  tracts[, (paste0(k, "_rate")) := fifelse(population >= 500, 1000 * get(paste0(k, "_count")) / population, NA_real_)]
}
stopifnot(all(tracts$station_m >= 0), all(tracts$night_count <= tracts$noise_count))
tracts[, profile := complete.cases(rent, night_rate, nypd_rate, station_m, density)]
stopifnot(tracts[profile == TRUE, .N] >= 1000L)

months <- rbindlist(list(
  noise[, .N, by = month][, source := "311 noise"],
  nypd[, .N, by = month][, source := "NYPD"]
))
qa <- rbindlist(list(cbind(source = "311 noise", noise_qa), cbind(source = "NYPD", nypd_qa)))
fwrite(tracts, file.path(out, "tracts.csv"))
fwrite(stations, file.path(out, "stations.csv"))
fwrite(time_summary, file.path(out, "noise-time.csv"))
fwrite(noise_hour, file.path(out, "tract-noise-hour.csv"))
fwrite(law_summary, file.path(out, "nypd-law.csv"))
fwrite(months, file.path(out, "source-months.csv"))
fwrite(qa, file.path(out, "join-audit.csv"))
display_shapes <- st_transform(st_simplify(shapes[, "geoid"], dTolerance = 65, preserveTopology = TRUE), 4326)
st_write(display_shapes, file.path(out, "tract-boundaries.geojson"), delete_dsn = TRUE, quiet = TRUE)
print(qa)
print(tracts[, .(mapped = .N, rent = sum(!is.na(rent)), burden = sum(computable >= 50 & !is.na(burden_rate), na.rm=TRUE),
                 rate = sum(population >= 500, na.rm=TRUE), profile = sum(profile))])
