## General settings

# General directories
root.dir <- here::here()
# Custom R function directory
functions.dir <- file.path(root.dir, "functions")
# Reports directory
reports.dir <- file.path(root.dir, "reports")
# Data directory
data.dir <- file.path(root.dir, "data")
data.raw.dir  <- file.path(data.dir, "raw")
data.processed.dir  <- file.path(data.dir, "processed")
# Output directory
output.dir <- file.path(root.dir, "data")
output.precision <- 4

## Growth Rates

# method for estimating growth rates
regression.method <- "lsq" # use fast least-squares method

# found growth rates must have R squared above
# r.squared.cutoff <- 0.9
r.squared.threshold <- 0.9
# found growth rates must be below
slope.cutoff <- 0.2
slope.threshold <- 0
# when calculating sliding windows
w_size <- 12

# Test Settings
p.value.adj.method <- "fdr"
test.alpha <- 0.05

## GGplot Settings

mmp_theme <- theme_bw(
    base_size = 26,
  ) +
  # theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  # theme(panel.spacing.x = unit(0.75, "cm")) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    # axis.text = element_text(size=22, color = "black"), 
    # axis.title.y = element_text(size=26, margin=margin(r=16), color = "black"),
    # axis.title.x = element_text(size=26, margin=margin(t=16), color = "black"),
    # legend.text = element_text(size=22, color = "black"),
    # legend.title = element_text(size=26, color = "black"),
    # legend.margin = margin(0.5, 0.5, 0.5, 0.5, unit='cm')
  )

update_geom_defaults("point", list(size = 2.5))

### GGplot labels

p.label.mu <- bquote(mu~("h"^-1))
p.label.dt <- bquote(Delta*"t"~(h))
p.label.condition <- "Condition"
p.label.time.h <- bquote("Time"~(h))
p.label.r.sq <- bquote("R"^2)
p.label.pbr <- "PBR"
p.label.channel <- "Channel"
p.label.dO2 <- bquote("dO"[2]~("%"))
p.label.intensity <- bquote("LI"~(mu*"E"^-1*"m"^-2*"s"^-1))
p.label.red.intensity <- bquote("red" ~ .(p.label.intensity))
p.label.lac.conc <- "[Na-L-lactate]"
p.label.dataset <- "Dataset"
p.label.n.points <- "# OD measurements"
p.lalel.n.generations <- "# Generations"
p.label.dt <- bquote("Doubling time"~(h))
p.label.method <- "Method"
p.label.od <- function(.w) { bquote("OD"[.(.w)]) }
p.label.od.720 <- p.label.od(720)
p.label.od.730 <- p.label.od(730)
p.label.od.ln <- function(.w) { bquote("ln"(.(p.label.od(.w)))) }
p.label.od.ln.720 <- p.label.od.ln(720)
p.label.cdr <- ~ bquote("Cumulative d"~("h"^-1))
p.label.temperature <-  bquote("Temperature"~(degree*"C"))
p.label.size <-  bquote("Size"~(mu*"m"))
p.label.counts <- "# Counts"

p.label.dna.conc <- bquote("[DNA]"~("ng"%.%mu*"L"^-1))
p.label.conc.phs.mM <- bquote("Phosphate (mM)")
