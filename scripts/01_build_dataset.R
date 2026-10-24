# ASI Term Paper — Build factory-level dataset
# Run from the ASI_Term_Paper RStudio project root.
# This script reads the raw CSVs and writes clean_data/asi_all_records.csv.
# It does not overwrite or modify the original files.

library(data.table)

# ---------- 1. Project folders and survey years ----------
root <- getwd()
raw_dir <- file.path(root, "raw_data")
out_dir <- file.path(root, "clean_data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years <- list(
  list(year = "2021-22", folder = "ASI_DATA_2021_22_CSV", suffix = "202122", e_total = 9L),
  list(year = "2022-23", folder = "ASI_DATA_2022_23_CSV", suffix = "202223", e_total = 9L),
  list(year = "2023-24", folder = "ASI_DATA_2023_24_CSV", suffix = "202324", e_total = 10L)
)

# Use the first available name because some block column names changed slightly across years.
first_col <- function(DT, candidates, description) {
  found <- candidates[candidates %in% names(DT)]
  if (length(found) == 0L) {
    stop("Could not find column for ", description, ". Tried: ", paste(candidates, collapse = ", "))
  }
  found[1L]
}

read_block <- function(folder, filename) {
  path <- file.path(raw_dir, folder, filename)
  if (!file.exists(path)) stop("File not found: ", path,
                               "\nCheck that each ZIP was extracted into raw_data/.")
  fread(path, na.strings = c("", "NA"), showProgress = TRUE)
}

check_unique_id <- function(DT, block_name) {
  if (anyDuplicated(DT$id)) {
    stop(block_name, " did not reduce to one row per factory. Do not merge; inspect duplicate IDs first.")
  }
  invisible(TRUE)
}

merge_one <- function(master, piece, block_name) {
  check_unique_id(piece, block_name)
  n_before <- nrow(master)
  joined <- merge(master, piece, by = "id", all.x = TRUE, sort = FALSE)
  if (nrow(joined) != n_before) {
    stop("Row count changed after joining ", block_name, ": ", n_before, " -> ", nrow(joined))
  }
  joined
}

build_year <- function(info) {
  message("\n--- Building ", info$year, " ---")
  s <- info$suffix
  folder <- info$folder

  # Block A: one record per factory, plus identifiers, status, weight and payments.
  A <- read_block(folder, paste0("blkA", s, ".csv"))
  if (anyDuplicated(as.character(A$a1))) stop("Duplicate factory IDs in Block A for ", info$year)
  expected_a <- c("2021-22" = 68548L, "2022-23" = 68462L, "2023-24" = 68641L)
  if (nrow(A) != expected_a[[info$year]]) {
    stop("Block A count does not match checkpoint for ", info$year,
         ". Expected ", expected_a[[info$year]], ", got ", nrow(A))
  }

  d <- data.table(
    id = as.character(A$a1),
    year = info$year,
    scheme = as.integer(A$a3),
    nic5 = as.integer(A$a5),
    state = as.integer(A$a7),
    status = as.integer(A$a12),
    mult = as.numeric(A$mult),
    bonus = as.numeric(A$bonus),
    pf = as.numeric(A$pf),
    welfare = as.numeric(A$welfare)
  )
  message("Block A: ", nrow(d), " factory records")

  # Block C: item 10 = total fixed assets; item 3 = plant and machinery.
  C <- read_block(folder, paste0("blkC", s, ".csv"))
  c_item <- first_col(C, "c_11", "Block C item number")
  C[, id := as.character(ac01)]
  cap <- C[get(c_item) == 10, .(id, capital = as.numeric(c_113))]
  mach <- C[get(c_item) == 3, .(id, machinery = as.numeric(c_113))]
  check_unique_id(cap, "Block C item 10 (total assets)")
  check_unique_id(mach, "Block C item 3 (machinery)")
  d <- merge_one(d, cap, "Block C total assets")
  d <- merge_one(d, mach, "Block C machinery")
  rm(C, cap, mach)

  # Block D: item 6 = finished goods; change is closing minus opening stock.
  D <- read_block(folder, paste0("blkD", s, ".csv"))
  d_item <- first_col(D, c("d11", "dI1"), "Block D item number")
  d_open <- first_col(D, c("d13", "dI3"), "Block D opening stock value")
  d_close <- first_col(D, c("d14", "dI4"), "Block D closing stock value")
  D[, id := as.character(ad01)]
  delta <- D[get(d_item) == 6,
             .(id, finished_goods_change = as.numeric(get(d_close)) - as.numeric(get(d_open)))]
  check_unique_id(delta, "Block D item 6 (finished goods)")
  d <- merge_one(d, delta, "Block D finished goods")
  rm(D, delta)

  # Block E: total employees row; row number is 9 in the first two years, 10 in 2023-24.
  E <- read_block(folder, paste0("blkE", s, ".csv"))
  e_item <- first_col(E, c("E11", "EI1"), "Block E item number")
  e_persons <- first_col(E, c("E16", "EI6"), "Block E average persons worked")
  e_wages <- first_col(E, c("E18", "EI8"), "Block E wages/salaries")
  e_id <- first_col(E, c("AE01"), "Block E factory ID")
  E[, id := as.character(get(e_id))]
  labour <- E[get(e_item) == info$e_total,
              .(id, persons = as.numeric(get(e_persons)), wages = as.numeric(get(e_wages)))]
  check_unique_id(labour, "Block E total-employees row")
  d <- merge_one(d, labour, "Block E employment")
  rm(E, labour)

  # Block F: one row per factory. The selected expenses follow the ASI scrutiny formula.
  F <- read_block(folder, paste0("blkF", s, ".csv"))
  F[, id := as.character(AF01)]
  f_exp_cols <- c("F1", "F2A", "F2B", "F3", "F4", "F6", "F7", "F8", "F11")
  missing_f <- setdiff(f_exp_cols, names(F))
  if (length(missing_f)) stop("Missing Block F columns: ", paste(missing_f, collapse = ", "))
  F[, other_expenses := rowSums(.SD, na.rm = TRUE), .SDcols = f_exp_cols]
  F[, rd_expense := as.numeric(F7)]
  f_piece <- F[, .(id, other_expenses, rd_expense)]
  check_unique_id(f_piece, "Block F")
  d <- merge_one(d, f_piece, "Block F expenses")
  rm(F, f_piece)

  # Block G: one record per factory; add output categories specified in the schedule.
  G <- read_block(folder, paste0("blkG", s, ".csv"))
  G[, id := as.character(AG01)]
  g_cols <- c("G1", "G2", "G3", "G4", "G6", "G7", "G8", "G11")
  missing_g <- setdiff(g_cols, names(G))
  if (length(missing_g)) stop("Missing Block G columns: ", paste(missing_g, collapse = ", "))
  G[, other_output := rowSums(.SD, na.rm = TRUE), .SDcols = g_cols]
  g_piece <- G[, .(id, other_output)]
  check_unique_id(g_piece, "Block G")
  d <- merge_one(d, g_piece, "Block G other output")
  rm(G, g_piece)

  # Block H: item 23 is total indigenous inputs; H16/HI6 is its purchase value.
  H <- read_block(folder, paste0("blkH", s, ".csv"))
  h_item <- first_col(H, c("H11", "HI1"), "Block H serial/item number")
  h_value <- first_col(H, c("H16", "HI6"), "Block H purchase value")
  h_id <- first_col(H, c("AH01"), "Block H factory ID")
  H[, id := as.character(get(h_id))]
  indigenous <- H[get(h_item) == 23, .(id, indigenous_inputs = as.numeric(get(h_value)))]
  check_unique_id(indigenous, "Block H item 23 (total indigenous inputs)")
  d <- merge_one(d, indigenous, "Block H indigenous inputs")
  rm(H, indigenous)

  # Block I: item 7 is total imports; I16/II6 is its purchase value.
  I <- read_block(folder, paste0("blkI", s, ".csv"))
  i_item <- first_col(I, c("I11", "II1"), "Block I serial/item number")
  i_value <- first_col(I, c("I16", "II6"), "Block I purchase value")
  i_id <- first_col(I, c("AI01"), "Block I factory ID")
  I[, id := as.character(get(i_id))]
  imports <- I[get(i_item) == 7, .(id, imported_inputs = as.numeric(get(i_value)))]
  check_unique_id(imports, "Block I item 7 (total imports)")
  d <- merge_one(d, imports, "Block I imported inputs")
  rm(I, imports)

  # Block J: item 12 = total products. Compute net sales from sale value, taxes and subsidy.
  J <- read_block(folder, paste0("blkJ", s, ".csv"))
  J[, id := as.character(AJ01)]
  sales <- J[as.integer(J11) == 12,
             .(id,
               net_sales = as.numeric(J17) - as.numeric(J18) - as.numeric(J19) -
                 as.numeric(J110) + as.numeric(J111))]
  check_unique_id(sales, "Block J item 12 (total products)")
  d <- merge_one(d, sales, "Block J net sales")
  rm(J, sales)

  # Treat absent component rows as zero when summing the ASI formula components.
  # Some factories have no Block J total row; in that case there is no recorded net-sales
  # component, but inventory changes or other output can still make total output positive.
  # Do NOT zero-fill the constructed total output, persons, capital, or survey multiplier.
  zero_if_missing <- c("net_sales", "finished_goods_change", "other_expenses", "rd_expense",
                       "other_output", "indigenous_inputs", "imported_inputs")
  for (v in zero_if_missing) set(d, which(is.na(d[[v]])), v, 0)

  # Construct the paper variables. F7 is intentionally in both output and input per the ASI formula.
  d[, output := net_sales + finished_goods_change + other_output + rd_expense]
  d[, total_input := other_expenses + indigenous_inputs + imported_inputs]
  d[, gva := output - total_input]
  d[, emoluments := wages + bonus + pf + welfare]
  d[, nic2 := as.integer(nic5 %/% 1000)]
  d[, nic3 := as.integer(nic5 %/% 100)]
  d[, nic4 := as.integer(nic5 %/% 10)]

  message("Finished ", info$year, ": ", nrow(d), " rows after left-joining selected variables")
  d
}

# ---------- 2. Build and append all survey years ----------
all_records <- rbindlist(lapply(years, build_year), use.names = TRUE, fill = TRUE)

expected_total <- 205651L
if (nrow(all_records) != expected_total) {
  stop("Raw merged total did not match checkpoint. Expected ", expected_total,
       ", got ", nrow(all_records), ". Review the error/checks before proceeding.")
}

# Save raw-merge output before any analysis-sample filters.
out_file <- file.path(out_dir, "asi_all_records.csv")
fwrite(all_records, out_file)
message("\nSaved: ", out_file)

# ---------- 3. Report the paper's sample-selection checkpoints ----------
# The expected counts are checkpoints, not values to force the data to match.
checkpoints <- data.table(
  year = c("2021-22", "2022-23", "2023-24"),
  expected_A = c(68548L, 68462L, 68641L),
  expected_open = c(55087L, 56230L, 57274L),
  expected_L_positive = c(55086L, 56229L, 57274L),
  expected_output_positive = c(55053L, 56198L, 57231L),
  expected_final = c(54843L, 55999L, 57044L)
)

sample_counts <- all_records[, .(
  all_records = .N,
  open_status_1 = sum(status == 1L, na.rm = TRUE),
  persons_positive = sum(status == 1L & !is.na(persons) & persons >= 1, na.rm = TRUE),
  output_positive = sum(status == 1L & !is.na(persons) & persons >= 1 & !is.na(output) & output > 0, na.rm = TRUE),
  final_sample = sum(status == 1L & !is.na(mult) & mult > 0 & !is.na(persons) & persons >= 1 &
                       !is.na(output) & output > 0 & !is.na(capital) & capital > 0, na.rm = TRUE)
), by = year]

setorder(sample_counts, year)
print(sample_counts)
fwrite(sample_counts, file.path(out_dir, "sample_count_checkpoints.csv"))

message("\nCompare sample_counts with the expected numbers in the workflow.")
message("If any filter count differs, stop before estimating regressions and send me the printed table.")
message("Build script complete.")
