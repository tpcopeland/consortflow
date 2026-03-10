# ==============================================================================
# consortflow demo
# ==============================================================================
#
# Demonstrates the full consortflow pipeline: init -> exclude -> save
# Produces three example diagrams with different styling options.

library(consortflow)

# output directory
outdir <- file.path(tempdir(), "consortflow_demo")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
cat("Output directory:", outdir, "\n\n")


# ==============================================================================
# Generate example cohort data
# ==============================================================================

set.seed(2026)
n <- 15000

cohort <- data.frame(
  patient_id   = seq_len(n),
  age          = sample(12:95, n, replace = TRUE),
  sex          = sample(c("Male", "Female"), n, replace = TRUE),
  index_date   = as.Date("2010-01-01") + sample(0:3650, n, replace = TRUE),
  exit_date    = as.Date("2010-01-01") + sample(30:4000, n, replace = TRUE),
  has_baseline = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.88, 0.12)),
  education    = sample(c("Primary", "Secondary", "Tertiary", NA), n,
                        replace = TRUE, prob = c(0.25, 0.35, 0.30, 0.10)),
  prior_event  = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.05, 0.95))
)

# derive follow-up
cohort$followup_days <- as.integer(cohort$exit_date - cohort$index_date)
cohort$followup_days[cohort$followup_days < 0] <- 0L

cat("Generated cohort:", format(nrow(cohort), big.mark = ","), "patients\n\n")


# ==============================================================================
# Example 1: Basic exclusion flowchart (no shading)
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Example 1: Basic flowchart\n")
cat(strrep("=", 60), "\n")

flow1 <- cf_init(cohort, label = "Persons with antidepressant dispensing")
flow1 <- cf_exclude(flow1, age < 18, label = "Age < 18 years")
flow1 <- cf_exclude(flow1, followup_days < 30, label = "Follow-up < 30 days")
flow1 <- cf_exclude(flow1, !has_baseline, label = "Missing baseline data")
flow1 <- cf_exclude(flow1, is.na(education), label = "Missing education")

out1 <- file.path(outdir, "example1_basic.png")
cf_save(flow1, output = out1, final = "Final Analytic Cohort", dpi = 300)
cat("\n")


# ==============================================================================
# Example 2: Shaded boxes with milestone labels
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Example 2: Shaded with milestone labels\n")
cat(strrep("=", 60), "\n")

flow2 <- cf_init(cohort, label = "All antidepressant dispensings 2010-2020")
flow2 <- cf_exclude(flow2, age < 18,
                    label = "Age < 18 years",
                    remaining = "Adult cohort")
flow2 <- cf_exclude(flow2, prior_event,
                    label = "Prior event before index date",
                    remaining = "Incident users")
flow2 <- cf_exclude(flow2, followup_days < 30,
                    label = "Follow-up < 30 days",
                    remaining = "Minimum follow-up met")
flow2 <- cf_exclude(flow2, !has_baseline,
                    label = "Missing baseline laboratory values")
flow2 <- cf_exclude(flow2, is.na(education),
                    label = "Missing education data")

out2 <- file.path(outdir, "example2_shaded.png")
cf_save(flow2, output = out2, final = "Study Population",
        shading = TRUE, dpi = 300)
cat("\n")


# ==============================================================================
# Example 3: PDF output
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Example 3: PDF output\n")
cat(strrep("=", 60), "\n")

flow3 <- cf_init(cohort, label = "Source population")
flow3 <- cf_exclude(flow3, age < 18 | age > 85,
                    label = "Age outside 18-85 years",
                    remaining = "Age-eligible cohort")
flow3 <- cf_exclude(flow3, followup_days < 90,
                    label = "Follow-up < 90 days")
flow3 <- cf_exclude(flow3, !has_baseline | is.na(education),
                    label = "Missing baseline data or education")

out3 <- file.path(outdir, "example3_output.pdf")
cf_save(flow3, output = out3, final = "Final Cohort")
cat("\n")


# ==============================================================================
# Summary
# ==============================================================================

cat(strrep("=", 60), "\n")
cat("Demo complete. Output files:\n")
cat("  ", out1, "\n")
cat("  ", out2, "\n")
cat("  ", out3, "\n")
