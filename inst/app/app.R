# ==============================================================================
# stratifyR 2.0 — Professional Shiny Application
# Optimal Survey Stratification
#
# Required packages:
#   install.packages(c("shiny", "bslib", "DT", "plotly", "readxl"))
#   install.packages("~/Desktop/strataVerse/stratifyR 2.0/stratifyR 2.0-3",
#                    repos = NULL, type = "source")
# ==============================================================================

library(shiny)
library(bslib)
library(DT)
library(stratifyR)

# Version shown in the header badge, read from the installed package
# (e.g. "2.0-2"), so it always matches the package that is running.
.app_version <- tryCatch(utils::packageDescription("stratifyR")$Version,
                         error = function(e) "2.0-3")
if (is.null(.app_version) || is.na(.app_version)) .app_version <- "2.0-3"

has_plotly  <- requireNamespace("plotly",  quietly = TRUE)
has_readxl  <- requireNamespace("readxl",  quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
if (has_plotly)  library(plotly)
if (has_ggplot2) library(ggplot2)

# ==============================================================================
# Static data
# ==============================================================================

builtin_choices <- c(
  "Anaemia — Iron"         = "anaemia|Iron",
  "HIES — Income"          = "hies|Income",
  "Math — Final Marks"     = "math|final_marks",
  "Sugarcane — Production" = "sugarcane|Production"
)

distr_choices <- c(
  "Weibull"          = "weibull",
  "Gamma"            = "gamma",
  "Log-Normal"       = "lnorm",
  "Normal"           = "norm",
  "Exponential"      = "exp",
  "Pareto"           = "pareto",
  "Cauchy"           = "cauchy",
  "Uniform"          = "unif",
  "Triangular"       = "triangle",
  "Right-Triangular" = "rtriangle"
)

distr_params <- list(
  weibull   = list(names = c("shape", "scale"),         defaults = c(2.0,  1.0)),
  gamma     = list(names = c("shape", "rate"),           defaults = c(2.0,  0.5)),
  lnorm     = list(names = c("meanlog", "sdlog"),        defaults = c(0.0,  1.0)),
  norm      = list(names = c("mean", "sd"),              defaults = c(0.0,  1.0)),
  exp       = list(names = c("rate"),                    defaults = c(1.0)),
  pareto    = list(names = c("shape", "scale"),          defaults = c(1.5,  1.0)),
  cauchy    = list(names = c("location", "scale"),       defaults = c(0.0,  1.0)),
  unif      = list(names = c("min", "max"),              defaults = c(0.0,  1.0)),
  triangle  = list(names = c("min", "max", "mode"),      defaults = c(0.0,  1.0, 0.5)),
  rtriangle = list(names = c("min", "max", "mode"),      defaults = c(0.0,  1.0, 1.0))
)

palettes <- list(
  classic = c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
              "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC"),
  ocean   = c("#005F73","#0A9396","#94D2BD","#E9D8A6","#EE9B00",
              "#CA6702","#BB3E03","#AE2012","#9B2226","#001219"),
  rose    = c("#D64045","#E8A838","#9C2542","#F4C261","#B84A62",
              "#E07B54","#FF7F5C","#FFA500","#C46E4B","#8B1A1A"),
  forest  = c("#2D6A4F","#40916C","#52B788","#74C69D","#95D5B2",
              "#B7E4C7","#1B4332","#081C15","#D8F3DC","#6B9E7A"),
  vivid   = c("#E63946","#457B9D","#2DC653","#FF6B35","#7B2D8B",
              "#F4A261","#264653","#2A9D8F","#E9C46A","#F77F00")
)

# ==============================================================================
# CSS
# ==============================================================================

app_css <- "
/* ── Header ──────────────────────────────────────────────────────────────── */
.navbar {
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 55%, #24527A 100%) !important;
  border-bottom: 3px solid #E9A826 !important;
  padding: 0.6rem 1.5rem !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.35) !important;
}
.navbar-brand {
  font-size: 1.85rem !important;
  font-weight: 900 !important;
  letter-spacing: -1px !important;
  color: #FFFFFF !important;
  display: flex !important;
  align-items: center !important;
  gap: 10px !important;
}
.brand-logo-wrap {
  display: inline-flex;
  align-items: center;
  gap: 11px;
}
.brand-text-wrap {
  display: inline-flex;
  flex-direction: column;
  line-height: 1;
}
.brand-name {
  font-size: 1.75rem;
  font-weight: 900;
  letter-spacing: -1px;
  color: #FFFFFF;
  font-style: italic;
}
.brand-name .brand-badge { color: #0D1B2A; }
.brand-badge {
  background: #E9A826;
  color: #0D1B2A;
  font-size: 0.58rem;
  font-weight: 900;
  padding: 2px 7px;
  border-radius: 4px;
  margin-left: 7px;
  vertical-align: super;
  letter-spacing: 0.8px;
}
.brand-sub {
  font-size: 0.72rem;
  color: rgba(255,255,255,0.5);
  font-weight: 400;
  letter-spacing: 0.5px;
  margin-top: 2px;
}

/* ── Sidebar ──────────────────────────────────────────────────────────────── */
.bslib-sidebar-layout > .sidebar {
  background: #FFFFFF !important;
  border-right: 1px solid #E2E8F0 !important;
  box-shadow: 3px 0 10px rgba(0,0,0,0.06) !important;
}
.sidebar-section-label {
  font-size: 0.68rem;
  font-weight: 700;
  letter-spacing: 1.6px;
  text-transform: uppercase;
  color: #94A3B8;
  margin: 1.1rem 0 0.45rem;
  padding-bottom: 0.35rem;
  border-bottom: 1px solid #F1F5F9;
}

/* ── Run button ───────────────────────────────────────────────────────────── */
#run_btn {
  background: linear-gradient(135deg, #1B3A5C, #24527A) !important;
  border: none !important;
  color: #FFFFFF !important;
  font-weight: 700 !important;
  font-size: 0.9rem !important;
  letter-spacing: 0.4px !important;
  padding: 0.65rem 1rem !important;
  border-radius: 8px !important;
  width: 100%;
  box-shadow: 0 4px 14px rgba(27,58,92,0.4) !important;
  transition: all 0.18s ease !important;
}
#run_btn:hover {
  transform: translateY(-1px) !important;
  box-shadow: 0 6px 18px rgba(27,58,92,0.5) !important;
}

/* ── Status badge ─────────────────────────────────────────────────────────── */
.status-wrap { margin-top: 0.6rem; }
.status-pill {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  font-size: 0.78rem;
  font-weight: 600;
  padding: 4px 12px;
  border-radius: 20px;
  width: 100%;
  justify-content: center;
}
.s-idle    { background:#F1F5F9; color:#64748B; }
.s-ready   { background:#D1FAE5; color:#065F46; }
.s-error   { background:#FEE2E2; color:#991B1B; }
.s-running { background:#DBEAFE; color:#1D4ED8; }

/* ── Metric cards ─────────────────────────────────────────────────────────── */
.metrics-row { display:flex; gap:12px; margin-bottom:1rem; }
.metric-card {
  flex: 1;
  background: #FFFFFF;
  border-radius: 10px;
  padding: 1rem 0.8rem 0.85rem;
  text-align: center;
  box-shadow: 0 1px 4px rgba(0,0,0,0.08), 0 0 0 1px rgba(0,0,0,0.04);
}
.metric-value {
  font-size: 1.65rem;
  font-weight: 800;
  color: #1B3A5C;
  line-height: 1.1;
}
.metric-label {
  font-size: 0.84rem;
  font-weight: 700;
  letter-spacing: 0.6px;
  text-transform: uppercase;
  color: #94A3B8;
  margin-top: 4px;
}

/* ── Nav tabs ─────────────────────────────────────────────────────────────── */
.nav-tabs {
  border-bottom: none !important;
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 60%, #24527A 100%);
  border-radius: 12px 12px 0 0;
  padding: 0.55rem 0.6rem 0 0.6rem;
  gap: 4px;
  display: flex;
  flex-wrap: wrap;
}
.nav-tabs .nav-link {
  font-weight: 600 !important;
  font-size: 0.92rem !important;
  color: rgba(255,255,255,0.72) !important;
  border: none !important;
  padding: 0.65rem 1.15rem !important;
  border-radius: 8px 8px 0 0 !important;
  transition: background 0.18s, color 0.18s !important;
  white-space: nowrap;
  display: flex !important;
  align-items: center !important;
  gap: 0.38rem !important;
  background: rgba(255,255,255,0.06) !important;
  letter-spacing: 0.015em;
}
.nav-tabs .nav-link svg, .nav-tabs .nav-link i {
  font-size: 0.92rem !important;
  opacity: 0.8;
}
.nav-tabs .nav-link:hover:not(.active) {
  background: rgba(233,168,38,0.18) !important;
  color: #F6D280 !important;
}
.nav-tabs .nav-link:hover:not(.active) svg,
.nav-tabs .nav-link:hover:not(.active) i { opacity: 1; }
.nav-tabs .nav-link.active {
  background: #E9A826 !important;
  color: #0D1B2A !important;
  font-weight: 700 !important;
  box-shadow: 0 3px 10px rgba(233,168,38,0.35) !important;
}
.nav-tabs .nav-link.active svg,
.nav-tabs .nav-link.active i { opacity: 1 !important; color: #0D1B2A !important; }
.tab-content {
  border: none !important;
  border-radius: 0 0 12px 12px !important;
  background: #FFFFFF !important;
  box-shadow: 0 4px 16px rgba(13,27,42,0.10) !important;
}

/* ── Empty state ──────────────────────────────────────────────────────────── */
.empty-state {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-height: 55vh;
  color: #94A3B8;
  text-align: center;
  gap: 0.75rem;
}
.empty-icon { font-size: 3.5rem; opacity: 0.35; }
.empty-title { font-size: 1rem; font-weight: 700; color: #64748B; }
.empty-sub   { font-size: 0.82rem; }

/* ── Accordion ────────────────────────────────────────────────────────────── */
.accordion-button {
  background: linear-gradient(135deg, #134E4A 0%, #0F766E 100%) !important;
  color: #FFFFFF !important;
  font-weight: 700 !important;
  font-size: 1.05rem !important;
  letter-spacing: 0.2px !important;
  padding: 0.90rem 1.15rem !important;
  border-radius: 8px !important;
  box-shadow: 0 2px 8px rgba(15,118,110,0.30) !important;
}
.accordion-button::after {
  filter: invert(1) brightness(2) !important;
}
.accordion-button:not(.collapsed) {
  background: linear-gradient(135deg, #042F2E 0%, #134E4A 100%) !important;
  color: #E9A826 !important;
  box-shadow: 0 2px 10px rgba(4,47,46,0.45) !important;
}
.accordion-button:not(.collapsed)::after {
  filter: invert(0) sepia(1) saturate(4) hue-rotate(5deg) brightness(1.1) !important;
}
.accordion-item {
  border: none !important;
  margin-bottom: 6px !important;
  border-radius: 10px !important;
  overflow: visible !important;
  box-shadow: 0 2px 8px rgba(27,58,92,0.12) !important;
}
.accordion-button {
  border-radius: 10px !important;
}
.accordion-button:not(.collapsed) {
  border-radius: 10px 10px 0 0 !important;
}
.accordion-collapse .accordion-body {
  border-radius: 0 0 10px 10px !important;
}
.accordion-body {
  padding: 0.85rem 1rem !important;
  background: #FAFCFF !important;
  color: #1E293B !important;
  overflow: visible !important;
}
/* Ensure select dropdowns always render above everything */
.selectize-dropdown,
.dropdown-menu,
.selectize-dropdown-content {
  z-index: 99999 !important;
  position: absolute !important;
}
/* Ensure all form controls inside accordion are fully visible */
.accordion-body .form-select,
.accordion-body select,
.accordion-body .form-control,
.accordion-body input[type=text],
.accordion-body input[type=number] {
  background-color: #FFFFFF !important;
  color: #1E293B !important;
  border: 1.5px solid #CBD5E1 !important;
  border-radius: 6px !important;
  font-size: 0.83rem !important;
}
.accordion-body label,
.accordion-body .form-check-label,
.accordion-body .shiny-input-container > label {
  color: #374151 !important;
  font-size: 0.8rem !important;
  font-weight: 600 !important;
}
.accordion-body .form-check-input { border-color: #4A9EE8 !important; }
.accordion-body .radio label { color: #374151 !important; font-weight: 500 !important; }

/* ── Progress bar (sidebar shimmer) ──────────────────────────────────────── */
.progress-wrap {
  margin-top: 10px;
  border-radius: 8px;
  overflow: hidden;
  background: #E2E8F0;
  height: 10px;
}
.progress-shimmer {
  height: 100%;
  width: 100%;
  background: linear-gradient(90deg,
    #134E4A 0%, #0F766E 30%, #E9A826 55%, #0F766E 75%, #134E4A 100%);
  background-size: 300% 100%;
  animation: shimmer 1.8s infinite linear;
  border-radius: 6px;
}
@keyframes shimmer {
  0%   { background-position: 100% 0; }
  100% { background-position: -100% 0; }
}
.progress-label {
  font-size: 0.72rem;
  color: #0F766E;
  font-weight: 600;
  margin-top: 5px;
  text-align: center;
}

/* ── Shiny withProgress modal — custom theme ─────────────────────────────── */
#shiny-progress-container {
  position: fixed !important;
  bottom: 24px !important;
  right: 24px !important;
  left: auto !important;
  width: 340px !important;
  z-index: 9999;
}
.shiny-progress {
  background: #0D1B2A !important;
  border-radius: 14px !important;
  padding: 18px 22px 16px !important;
  box-shadow: 0 12px 40px rgba(0,0,0,0.55) !important;
  border: 1.5px solid rgba(233,168,38,0.35) !important;
}
.shiny-progress .progress-message {
  color: #E9A826 !important;
  font-weight: 700 !important;
  font-size: 0.92rem !important;
  margin-bottom: 6px !important;
}
.shiny-progress .progress-detail {
  color: rgba(255,255,255,0.65) !important;
  font-size: 0.80rem !important;
  margin-bottom: 10px !important;
}
.shiny-progress .progress {
  height: 10px !important;
  border-radius: 6px !important;
  background: rgba(255,255,255,0.12) !important;
  overflow: hidden !important;
  margin-bottom: 0 !important;
}
.shiny-progress .progress-bar {
  background: linear-gradient(90deg, #134E4A, #0F766E 50%, #E9A826) !important;
  transition: width 0.4s cubic-bezier(0.4,0,0.2,1) !important;
  border-radius: 6px !important;
}

/* ── Summary table ────────────────────────────────────────────────────────── */
.summary-meta {
  font-size: 0.78rem;
  color: #64748B;
  margin-bottom: 0.75rem;
  display: flex;
  gap: 1.2rem;
  flex-wrap: wrap;
}
.meta-item { display: flex; gap: 5px; align-items: center; }
.meta-badge {
  background: #EFF6FF;
  color: #1D4ED8;
  font-size: 0.7rem;
  font-weight: 700;
  padding: 2px 8px;
  border-radius: 10px;
}
.meta-badge.green { background:#D1FAE5; color:#065F46; }
.meta-badge.amber { background:#FEF3C7; color:#92400E; }

/* ── Design Comparison tab ────────────────────────────────────────────────── */
.dc-cards {
  display: flex; flex-wrap: wrap; gap: 0.9rem; margin-bottom: 1.1rem;
}
.dc-card {
  flex: 1 1 160px;
  background: #FFFFFF;
  border: 1.5px solid #E2E8F0;
  border-radius: 10px;
  padding: 0.85rem 1.1rem;
  text-align: center;
}
.dc-card .dc-val {
  font-size: 1.55rem; font-weight: 800; color: #1B3A5C;
  line-height: 1.1; margin-bottom: 0.2rem;
}
.dc-card .dc-lbl {
  font-size: 0.72rem; color: #64748B; letter-spacing: 0.3px;
}
.dc-card.dc-highlight { border-color: #059669; background: #F0FDF4; }
.dc-card.dc-highlight .dc-val { color: #065F46; }
.dc-card.dc-amber { border-color: #D97706; background: #FFFBEB; }
.dc-card.dc-amber .dc-val { color: #92400E; }

.dc-table {
  width: 100%; border-collapse: collapse;
  font-size: 0.875rem; margin: 0.4rem 0 1.1rem;
}
.dc-table th {
  background: #1B3A5C; color: #E9A826;
  padding: 8px 14px; text-align: left;
  font-weight: 700; font-size: 0.80rem; letter-spacing: 0.3px;
}
.dc-table th:not(:first-child) { text-align: right; }
.dc-table td {
  padding: 8px 14px; border-bottom: 1px solid #E2E8F0; color: #334155;
}
.dc-table td:not(:first-child) { text-align: right; font-family: 'Courier New', monospace; }
.dc-table tr.dc-row-opt td { background: #F0FDF4; font-weight: 700; color: #065F46; }
.dc-table tr.dc-row-prop td { background: #FAFAFA; }
.dc-table .dc-row-opt td:first-child::before { content: '★ '; }

.dc-saving-banner {
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 100%);
  color: #FFFFFF; border-radius: 10px;
  padding: 1rem 1.4rem; margin-bottom: 1.1rem;
  display: flex; align-items: center; gap: 1.4rem; flex-wrap: wrap;
}
.dc-saving-banner .dsb-stat {
  text-align: center; flex: 1; min-width: 100px;
}
.dc-saving-banner .dsb-val {
  font-size: 1.9rem; font-weight: 800; color: #E9A826; line-height: 1;
}
.dc-saving-banner .dsb-lbl {
  font-size: 0.73rem; color: rgba(255,255,255,0.72); margin-top: 2px;
}
.dc-saving-banner .dsb-div {
  width: 1px; background: rgba(255,255,255,0.20); align-self: stretch;
}

/* ── Introduction page ────────────────────────────────────────────────────── */
.intro-hero {
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 60%, #24527A 100%);
  color: #FFFFFF;
  border-radius: 12px;
  padding: 1.8rem 2.2rem 1.6rem;
  margin-bottom: 1.4rem;
  position: relative;
  overflow: hidden;
}
.intro-hero::before {
  content: '';
  position: absolute; top: 0; right: 0; bottom: 0;
  width: 38%;
  background: radial-gradient(ellipse at top right, rgba(233,168,38,0.18) 0%, transparent 70%);
}
.intro-hero-title {
  font-size: 1.6rem; font-weight: 800;
  color: #E9A826; letter-spacing: -0.4px; margin-bottom: 0.35rem;
}
.intro-hero-sub {
  font-size: 0.97rem; color: rgba(255,255,255,0.82);
  max-width: 680px; line-height: 1.55;
}
.intro-section {
  background: #FFFFFF;
  border: 1.5px solid #E2E8F0;
  border-radius: 10px;
  padding: 1.35rem 1.6rem;
  margin-bottom: 1.1rem;
}
.intro-section-title {
  font-size: 1.02rem; font-weight: 700;
  color: #1B3A5C; margin-bottom: 0.65rem;
  display: flex; align-items: center; gap: 0.5rem;
}
.intro-section-title .num-badge {
  background: #1B3A5C; color: #E9A826;
  font-size: 0.78rem; font-weight: 800;
  width: 22px; height: 22px; border-radius: 50%;
  display: flex; align-items: center; justify-content: center;
  flex-shrink: 0;
}
.intro-body { font-size: 0.91rem; color: #334155; line-height: 1.70; }
.intro-body p  { margin: 0 0 0.7rem; }
.intro-body p:last-child { margin-bottom: 0; }
.intro-math {
  background: #F8FAFC;
  border-left: 3.5px solid #1B3A5C;
  border-radius: 0 8px 8px 0;
  padding: 0.7rem 1.2rem;
  margin: 0.6rem 0;
  font-size: 0.89rem;
  overflow-x: auto;
}
.intro-key-result {
  background: linear-gradient(135deg, #EFF6FF 0%, #F8FAFF 100%);
  border: 1.5px solid #BFDBFE;
  border-radius: 8px;
  padding: 0.75rem 1.2rem;
  margin: 0.7rem 0 0;
  font-size: 0.88rem; color: #1E40AF;
  font-weight: 600;
}
.intro-example-table {
  width: 100%; border-collapse: collapse;
  font-size: 0.85rem; margin: 0.6rem 0;
}
.intro-example-table th {
  background: #1B3A5C; color: #E9A826;
  padding: 7px 12px; text-align: center;
  font-weight: 700; font-size: 0.80rem;
}
.intro-example-table td {
  padding: 6px 12px; text-align: center;
  border-bottom: 1px solid #E2E8F0; color: #334155;
}
.intro-example-table tr:last-child td {
  border-bottom: none; font-weight: 700;
  background: #F1F5F9; color: #1B3A5C;
}
.intro-example-table tr:nth-child(even) td { background: #F8FAFC; }
.intro-gain-box {
  display: flex; gap: 1rem; flex-wrap: wrap; margin-top: 0.8rem;
}
.intro-gain-card {
  flex: 1; min-width: 150px;
  background: #FFFFFF; border: 1.5px solid #E2E8F0;
  border-radius: 8px; padding: 0.7rem 1rem;
  text-align: center;
}
.intro-gain-card .gval {
  font-size: 1.4rem; font-weight: 800; color: #1B3A5C;
}
.intro-gain-card .glbl {
  font-size: 0.73rem; color: #64748B; margin-top: 1px;
}
.intro-gain-card.highlight { border-color: #059669; background: #F0FDF4; }
.intro-gain-card.highlight .gval { color: #065F46; }
.intro-workflow {
  display: grid; grid-template-columns: repeat(auto-fit, minmax(150px,1fr));
  gap: 0.7rem; margin-top: 0.6rem;
}
.intro-step {
  background: #F8FAFC; border: 1px solid #E2E8F0;
  border-radius: 8px; padding: 0.75rem 0.9rem;
  font-size: 0.84rem; color: #334155;
  position: relative;
}
.intro-step .step-num {
  font-size: 1.1rem; font-weight: 800; color: #E9A826;
  display: block; margin-bottom: 0.2rem;
}
.intro-ref {
  font-size: 0.78rem; color: #94A3B8;
  border-top: 1px solid #E2E8F0; margin-top: 1rem;
  padding-top: 0.6rem; line-height: 1.55;
}

/* ── H-Selector tab ───────────────────────────────────────────────────────── */
.hsel-banner {
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 100%);
  color: #FFFFFF; border-radius: 10px;
  padding: 1rem 1.5rem; margin-bottom: 1.1rem;
  display: flex; align-items: center; justify-content: space-between;
  flex-wrap: wrap; gap: 1rem;
}
.hsel-banner-title { font-size: 1.1rem; font-weight: 700; color: #E9A826; margin-bottom: 0.25rem; }
.hsel-banner-text  { font-size: 0.87rem; color: rgba(255,255,255,0.80); max-width: 640px; }
.hsel-rec-box {
  background: #F0FDF4; border: 2px solid #059669;
  border-radius: 10px; padding: 0.85rem 1.4rem;
  margin-bottom: 1rem; display: flex; align-items: center; gap: 1.4rem; flex-wrap: wrap;
}
.hsel-rec-h     { font-size: 2.2rem; font-weight: 800; color: #065F46; line-height: 1; }
.hsel-rec-label { font-size: 0.82rem; color: #047857; font-weight: 600; margin-top: 2px; }
.hsel-rec-note  { font-size: 0.82rem; color: #475569; }
.hsel-table {
  width: 100%; border-collapse: collapse; font-size: 0.875rem; margin: 0.4rem 0 0.5rem;
}
.hsel-table th {
  background: #1B3A5C; color: #E9A826;
  padding: 8px 14px; font-weight: 700; font-size: 0.80rem; text-align: center;
}
.hsel-table td { padding: 7px 14px; border-bottom: 1px solid #E2E8F0; text-align: center; color: #334155; }
.hsel-table tr.hsel-cur-row td   { background: #EFF6FF; font-weight: 700; color: #1D4ED8; }
.hsel-table tr.hsel-rec-row td   { background: #F0FDF4; font-weight: 700; color: #065F46; }
.hsel-table tr.hsel-degen-row td   { background: #FFFBEB; color: #92400E; font-style: italic; }
.hsel-table tr.hsel-stagnant-row td { background: #F8FAFC; color: #94A3B8; font-style: italic; }
.hsel-note { font-size: 0.76rem; color: #94A3B8; margin-top: 0.5rem; }

/* ── Boundary Methods Comparison ─────────────────────────────────────────── */
.bmc-banner {
  background: linear-gradient(135deg, #0D1B2A 0%, #1B3A5C 100%);
  color: #FFFFFF; border-radius: 10px;
  padding: 1rem 1.5rem; margin-bottom: 1.1rem;
  display: flex; align-items: center; justify-content: space-between;
  flex-wrap: wrap; gap: 1rem;
}
.bmc-banner-title { font-size: 1.1rem; font-weight: 700; color: #E9A826; margin-bottom: 0.25rem; }
.bmc-banner-text  { font-size: 0.87rem; color: rgba(255,255,255,0.80); max-width: 640px; }
.bmc-table {
  width: 100%; border-collapse: collapse; font-size: 0.875rem; margin: 0.4rem 0 0.5rem;
}
.bmc-table th {
  background: #1B3A5C; color: #E9A826;
  padding: 8px 14px; font-weight: 700; font-size: 0.80rem; text-align: left;
}
.bmc-table th:not(:first-child) { text-align: right; }
.bmc-table td { padding: 8px 14px; border-bottom: 1px solid #E2E8F0; color: #334155; }
.bmc-table td:not(:first-child) { text-align: right; font-family: 'Courier New', monospace; }
.bmc-table tr.bmc-row-dp td { background: #F0FDF4; font-weight: 700; color: #065F46; }
.bmc-table tr.bmc-row-fail td { color: #94A3B8; font-style: italic; }
.bmc-badge-opt {
  background: #D1FAE5; color: #065F46;
  font-size: 0.68rem; font-weight: 700;
  padding: 2px 7px; border-radius: 10px; margin-left: 6px;
}
.bmc-badge-fail {
  background: #FEF2F2; color: #991B1B;
  font-size: 0.68rem; font-weight: 700;
  padding: 2px 7px; border-radius: 10px; margin-left: 6px;
}

/* ── Sample Size Calculator ──────────────────────────────────────────────── */
.ssc-result-box {
  background: linear-gradient(135deg, #EFF6FF 0%, #DBEAFE 100%);
  border: 2px solid #3B82F6; border-radius: 12px;
  padding: 1.4rem 2rem; text-align: center; min-width: 180px;
}
.ssc-result-n   { font-size: 3rem; font-weight: 900; color: #1B3A5C; line-height: 1; }
.ssc-result-lbl { font-size: 0.88rem; color: #475569; margin-top: 0.3rem; }
.ssc-results-row {
  display: flex; gap: 1rem; flex-wrap: wrap; margin-bottom: 1rem; align-items: flex-start;
}
.ssc-input-panel {
  background: #FFFFFF; border: 1.5px solid #E2E8F0; border-radius: 10px;
  padding: 1rem 1.2rem; margin-bottom: 1rem;
  display: flex; align-items: flex-end; gap: 1.4rem; flex-wrap: wrap;
}

/* ── Cost-Constrained Optimisation ──────────────────────────────────────── */
.cc-cost-grid {
  display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
  gap: 0.6rem; margin: 0.8rem 0 1rem;
}
.cc-result-banner {
  background: linear-gradient(135deg, #F0FDF4 0%, #DCFCE7 100%);
  border: 2px solid #059669; border-radius: 10px;
  padding: 1rem 1.4rem; margin-bottom: 1rem;
  display: flex; gap: 2.5rem; flex-wrap: wrap; align-items: center;
}
.cc-result-stat { text-align: center; }
.cc-result-val  { font-size: 1.8rem; font-weight: 800; color: #065F46; line-height: 1; }
.cc-result-lbl  { font-size: 0.76rem; color: #047857; margin-top: 2px; }
.cc-result-banner { display: flex; gap: 1.6rem; flex-wrap: wrap; align-items: center; }
.cc-cost-card   { background: #F8FAFC; border: 1px solid #E2E8F0; border-radius: 8px;
  padding: 0.5rem 0.7rem; }
.cc-cost-card label { font-size: 0.72rem; font-weight: 600; color: #64748B;
  display: block; margin-bottom: 0.25rem; }
/* ── Methodology panel ──────────────────────────────────────────────────────── */
.method-panel { background: #F8FAFC; border: 1.5px solid #CBD5E1;
  border-radius: 10px; padding: 1rem 1.4rem; margin-bottom: 1rem; }
.method-panel summary { font-size: 0.82rem; font-weight: 700; color: #1B3A5C;
  cursor: pointer; user-select: none; list-style: none; display: flex;
  align-items: center; gap: 0.4rem; }
.method-panel summary::before { content: '▶'; font-size: 0.65rem; color: #64748B;
  transition: transform 0.2s; display: inline-block; }
details[open] > summary::before { transform: rotate(90deg); }
.method-panel .method-body { margin-top: 0.8rem; font-size: 0.82rem;
  color: #334155; line-height: 1.6; }
.method-panel .method-body p { margin: 0.4rem 0; }
.method-panel .method-body .method-formula { background: #EFF6FF;
  border-left: 3px solid #3B82F6; border-radius: 0 6px 6px 0;
  padding: 0.4rem 0.8rem; margin: 0.5rem 0; font-family: monospace;
  font-size: 0.85rem; color: #1E3A5F; }
.method-panel .method-ref { font-size: 0.75rem; color: #64748B;
  margin-top: 0.6rem; border-top: 1px solid #E2E8F0; padding-top: 0.5rem; }

/* ── Plot title input ─────────────────────────────────────────────────────── */
.title-input-wrap label { font-size: 0.78rem; font-weight: 600; color:#475569; }
.title-input-wrap input { font-size: 0.88rem; border-radius: 6px !important; }

/* ── Download button ──────────────────────────────────────────────────────── */
#dl_results {
  width: 100%;
  font-size: 0.8rem !important;
  border-color: #CBD5E1 !important;
  color: #475569 !important;
  background: #F8FAFC !important;
}
#dl_results:hover { background: #F1F5F9 !important; }

/* ── Generate R Code button ───────────────────────────────────────────────── */
#gen_code_btn {
  width: 100%;
  font-size: 0.8rem !important;
  border-color: #BFDBFE !important;
  color: #1D4ED8 !important;
  background: #EFF6FF !important;
  margin-top: 6px;
}
#gen_code_btn:hover { background: #DBEAFE !important; }

/* ── R Code modal ─────────────────────────────────────────────────────────── */
.r-code-block {
  background: #0F172A;
  color: #E2E8F0;
  font-family: 'Courier New', Consolas, monospace;
  font-size: 0.82rem;
  line-height: 1.65;
  border-radius: 8px;
  padding: 1.1rem 1.3rem;
  white-space: pre;
  overflow-x: auto;
  max-height: 68vh;
  overflow-y: auto;
}
.r-code-copy-btn {
  font-size: 0.78rem !important;
  padding: 4px 14px !important;
}

/* ── Main area background ─────────────────────────────────────────────────── */
.bslib-sidebar-layout > .main { background: #F8FAFD; padding: 1.2rem; }

/* ── Plot container ───────────────────────────────────────────────────────── */
.plot-container {
  background: #FFFFFF;
  border-radius: 10px;
  padding: 0.5rem;
  box-shadow: 0 1px 4px rgba(0,0,0,0.07);
}

/* ── Data preview ─────────────────────────────────────────────────────────── */
.data-preview {
  background: #F8FAFD;
  border: 1px solid #E2E8F0;
  border-radius: 6px;
  padding: 8px 10px;
  font-size: 0.76rem;
  color: #475569;
  margin-top: 4px;
}
.data-preview strong { color: #1B3A5C; }

/* ── 2D Info panel below plot ─────────────────────────────────────────────── */
.info-dist-card {
  background: linear-gradient(135deg, #EEF2FF 0%, #F8FAFF 100%);
  border: 1.5px solid #B0C4DE;
  border-radius: 10px;
  padding: 0.9rem 1.2rem;
  margin-top: 0.8rem;
}
.info-dist-name {
  font-size: 1.30rem;
  font-weight: 800;
  color: #1a1a6e;
  margin-bottom: 0.4rem;
  letter-spacing: -0.2px;
}
.info-eqn {
  background: #FFFFFF;
  border: 1.5px solid #C8D8EE;
  border-radius: 8px;
  padding: 0.5rem 1.4rem 0.3rem;
  margin: 0.45rem 0;
  display: block;
  text-align: center;
  overflow-x: auto;
}
.info-eqn .MathJax_Display, .info-eqn mjx-container {
  font-size: 1.25rem !important;
}
.info-params {
  display: flex;
  flex-wrap: wrap;
  gap: 0.6rem;
  margin-top: 0.6rem;
}
.param-badge {
  background: #1B3A5C;
  color: #E9A826;
  font-size: 0.95rem;
  font-weight: 700;
  padding: 6px 16px;
  border-radius: 20px;
  font-family: 'Courier New', monospace;
  letter-spacing: 0.2px;
  white-space: nowrap;
}
.info-strata {
  margin-top: 0.85rem;
  display: flex;
  flex-direction: column;
  gap: 0.42rem;
}
.strata-row {
  display: flex;
  align-items: center;
  gap: 0.65rem;
  background: #FFFFFF;
  border: 1px solid #E2E8F0;
  border-radius: 9px;
  padding: 0.6rem 1.1rem;
  font-size: 1.05rem;
}
.strata-swatch {
  width: 15px;
  height: 15px;
  border-radius: 4px;
  flex-shrink: 0;
  display: inline-block;
}
.strata-label {
  font-weight: 800;
  color: #1B3A5C;
  min-width: 90px;
  font-size: 1.05rem;
}
.strata-interval {
  color: #C0392B;
  font-weight: 600;
  min-width: 160px;
  font-family: 'Courier New', monospace;
  font-size: 1.00rem;
  white-space: nowrap;
}
.strata-stat {
  color: #475569;
  font-family: 'Courier New', monospace;
  font-size: 1.05rem;
  white-space: nowrap;
  padding: 4px 13px;
  background: #F1F5F9;
  border-radius: 5px;
}
"

# ==============================================================================
# Theme
# ==============================================================================

app_theme <- bs_theme(
  version    = 5,
  bg         = "#F8FAFD",
  fg         = "#1E293B",
  primary    = "#1B3A5C",
  secondary  = "#4A6FA5",
  success    = "#0F9B6E",
  warning    = "#D97706",
  danger     = "#DC2626",
  font_scale = 0.94
)

# ==============================================================================
# UI
# ==============================================================================

ui <- page_sidebar(
  theme        = app_theme,
  window_title = "stratifyR 2.0",
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
    tags$style(HTML(app_css)),
    withMathJax()
  ),

  title = div(class = "brand-logo-wrap",
    # Logo: the stratifyR hex sticker (www/stratifyR_logo.png)
    tags$img(src = "stratifyR_logo.png", height = "46px", alt = "stratifyR logo",
             style = "flex-shrink:0; filter:drop-shadow(0 2px 8px rgba(0,0,0,0.45));"),
    div(class = "brand-text-wrap",
      HTML(paste0('<div class="brand-name">stratifyR<span class="brand-badge">',
                  .app_version, '</span></div>')),
      div(class = "brand-sub", "Optimal Survey Stratification")
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 310,

    accordion(
      open = c("acc_data", "acc_strat", "acc_plot"),

      # ── 1. Data Source ───────────────────────────────────────────────────────
      accordion_panel(
        title = tagList(icon("database", style="margin-right:6px"), "Data Source"),
        value = "acc_data",

        radioButtons("data_source", NULL,
          choices  = c("Upload file"          = "upload",
                       "Built-in dataset"     = "builtin",
                       "Specify distribution" = "distr"),
          selected = "builtin"),

        # Upload
        conditionalPanel("input.data_source == 'upload'",
          fileInput("file_upload", NULL,
            accept      = c(".csv", ".xlsx", ".xls", ".txt"),
            placeholder = "CSV, Excel or TXT…",
            buttonLabel = "Browse"),
          uiOutput("ui_col_select")
        ),

        # Built-in
        conditionalPanel("input.data_source == 'builtin'",
          selectInput("builtin_ds", NULL, choices = builtin_choices),
          uiOutput("ui_builtin_info")
        ),

        # Distribution
        conditionalPanel("input.data_source == 'distr'",
          selectInput("distr_name", NULL, choices = distr_choices, selected = "weibull"),
          uiOutput("ui_distr_params"),
          fluidRow(
            column(6, numericInput("initval",   "Min value",   value = 0,    step = 0.1)),
            column(6, numericInput("dist_range","Range (max−min)", value = 10, min = 0.001, step = 0.1))
          ),
          numericInput("distr_N", "Population size (N)", value = 10000, min = 100, step = 500)
        )
      ),

      # ── 2. Stratification ────────────────────────────────────────────────────
      accordion_panel(
        title = tagList(icon("sliders", style="margin-right:6px"), "Stratification"),
        value = "acc_strat",

        fluidRow(
          column(6, numericInput("h_strata", "Strata (h)", value = 3, min = 2, max = 10, step = 1)),
          column(6, numericInput("n_sample",  "Sample (n)", value = 300, min = 10, step = 10))
        ),

        conditionalPanel("input.data_source != 'distr'",
          checkboxInput("use_N", "Set population size (N)", value = FALSE),
          conditionalPanel("input.use_N",
            numericInput("pop_N", NULL, value = 10000, min = 100, step = 500)
          )
        ),

        selectInput("solver", "Solver",
          choices  = c("DP — Dynamic Programming on the fitted model (default)" = "dp",
                       "EXACT — Dynamic Programming on the data"  = "exact",
                       "COBYLA — Fast multi-start search"         = "cobyla",
                       "GLOBAL — DIRECT-L + local refinement"     = "global"),
          selected = "dp",
          selectize = FALSE),

        div(style = "font-size:0.75rem; color:#94A3B8; margin-top:2px;",
          tags$strong("DP"), " — globally optimal on a fine grid for the fitted distribution. ",
          tags$strong("EXACT"), " — globally optimal for the data themselves (data sources only). ",
          tags$strong("COBYLA"), " — fast search on the data, usually within a fraction of a percent of EXACT. ",
          tags$strong("GLOBAL"), " — DIRECT-L global search followed by local refinement; a second opinion for small H.")
      ),

      # ── 3. Plot Options ──────────────────────────────────────────────────────
      accordion_panel(
        title = tagList(icon("chart-line", style="margin-right:6px"), "Plot Options"),
        value = "acc_plot",

        div(class = "title-input-wrap",
          textInput("plot_title", "Plot title",
            placeholder = "Leave blank for auto-generated title")
        ),

        selectInput("palette", "Colour palette",
          choices  = c("Classic (Tableau)" = "classic",
                       "Ocean Blues"       = "ocean",
                       "Rose & Amber"      = "rose",
                       "Forest Greens"     = "forest",
                       "Vivid"             = "vivid"),
          selected  = "classic",
          selectize = FALSE)
      )
    ),

    hr(style = "margin:0.9rem 0 0.75rem; border-color:#E2E8F0;"),

    actionButton("run_btn", tagList(icon("play"), " Run Stratification"),
      class = "btn btn-primary"),

    div(class = "status-wrap", uiOutput("ui_status")),

    hr(style = "margin:0.9rem 0 0.6rem; border-color:#E2E8F0;"),

    downloadButton("dl_results", tagList(icon("download"), " Export Results (CSV)"),
      class = "btn btn-outline-secondary btn-sm"),

    actionButton("gen_code_btn", tagList(icon("code"), " Generate R Code"),
      class = "btn btn-outline-primary btn-sm")
  ),

  # ── Main content ─────────────────────────────────────────────────────────────
  uiOutput("ui_main")
)

# ==============================================================================
# compare_designs — local implementation (does not require package reinstall)
# ==============================================================================

.S2_from_strata_app <- function(object) {
  pop <- object$data_internal
  if (!is.null(pop) && length(pop) >= 2L) {
    N <- length(pop); mu <- mean(pop)
    return(sum((pop - mu)^2) / N)
  }
  x_lo <- object$maxval * object$initval
  x_hi <- object$maxval * object$finval
  if (!is.finite(x_lo) || !is.finite(x_hi) || x_hi <= x_lo)
    return(sum(object$Wh * object$Vh))
  x_seq <- seq(x_lo, x_hi, length.out = 1024L)
  params <- tryCatch({
    fit <- object$fit
    if (!is.null(fit$estimate))          fit$estimate
    else if (!is.null(fit$fit$estimate)) fit$fit$estimate
    else NULL
  }, error = function(e) NULL)
  if (is.null(params)) return(sum(object$Wh * object$Vh))
  dens <- tryCatch(
    switch(object$distr,
      norm    = stats::dnorm(x_seq,    mean     = params["mean"],    sd    = params["sd"]),
      lnorm   = stats::dlnorm(x_seq,   meanlog  = params["meanlog"], sdlog = params["sdlog"]),
      gamma   = stats::dgamma(x_seq,   shape    = params["shape"],   rate  = params["rate"]),
      weibull = stats::dweibull(x_seq, shape    = params["shape"],   scale = params["scale"]),
      exp     = stats::dexp(x_seq,     rate     = params["rate"]),
      cauchy  = stats::dcauchy(x_seq,  location = params["location"],scale = params["scale"]),
      unif    = stats::dunif(x_seq,    min      = params["min"],      max  = params["max"]),
      rep(1 / (x_hi - x_lo), 1024L)
    ),
    error = function(e) rep(1 / (x_hi - x_lo), 1024L)
  )
  dens[!is.finite(dens)] <- 0
  dx  <- diff(x_seq)
  trap <- function(f) sum(dx * (head(f, -1L) + tail(f, -1L))) / 2
  tot <- trap(dens)
  if (tot < 1e-12) return(sum(object$Wh * object$Vh))
  mu  <- trap(x_seq       * dens) / tot
  mu2 <- trap(x_seq ^ 2L * dens) / tot
  max(mu2 - mu^2, 0)
}

compare_designs <- function(object, n = NULL) {
  if (!inherits(object, "strata"))
    stop("'object' must be of class \"strata\".")
  n_total <- if (!is.null(n)) as.integer(n) else object$nhTot
  H   <- nrow(object$h)
  Wh  <- object$Wh
  Vh  <- object$Vh
  WSh <- object$WhShTot
  V_within <- sum(Wh * Vh)
  S2 <- .S2_from_strata_app(object)
  V_srs  <- S2       / n_total
  V_prop <- V_within / n_total
  V_opt  <- WSh^2    / n_total
  deff_prop <- if (V_srs > 0) V_prop / V_srs else NA_real_
  deff_opt  <- if (V_srs > 0) V_opt  / V_srs else NA_real_
  n_srs_equiv <- ceiling(S2 * n_total / WSh^2)
  pct_saving  <- if (n_srs_equiv > 0)
    round(100 * (n_srs_equiv - n_total) / n_srs_equiv, 1) else NA_real_
  list(
    n              = n_total,
    H              = H,
    S2             = S2,
    V_within       = V_within,
    V_srs          = V_srs,
    V_prop         = V_prop,
    V_opt          = V_opt,
    SE_srs         = sqrt(max(V_srs,  0)),
    SE_prop        = sqrt(max(V_prop, 0)),
    SE_opt         = sqrt(max(V_opt,  0)),
    deff_prop      = deff_prop,
    deff_opt       = deff_opt,
    n_srs_equiv    = n_srs_equiv,
    pct_saving     = pct_saving,
    gain_over_prop = if (V_opt > 0) V_prop / V_opt else NA_real_,
    gain_over_srs  = if (V_opt > 0) V_srs  / V_opt else NA_real_,
    WhShTot        = WSh
  )
}

# ==============================================================================
# Boundary Methods Comparison — helpers
# ==============================================================================

# ΣWₕSₕ from raw sorted data + full boundary vector (length H+1)
.whsh_from_data <- function(data, full_b) {
  data  <- sort(data)
  N     <- length(data)
  H     <- length(full_b) - 1L
  total <- 0
  for (h in seq_len(H)) {
    lo <- full_b[h]; hi <- full_b[h + 1L]
    sl <- if (h == 1L) data[data >= lo & data <= hi]
          else         data[data >  lo & data <= hi]
    Nh <- length(sl)
    if (Nh < 2L) next
    Wh    <- Nh / N
    Sh    <- sqrt(sum((sl - mean(sl))^2) / Nh)   # population SD
    total <- total + Wh * Sh
  }
  total
}

# ΣWₕSₕ from density grid + full boundary vector (length H+1)
.whsh_from_dens <- function(x_seq, dens, full_b) {
  H   <- length(full_b) - 1L
  dx  <- diff(x_seq)
  tot <- sum(dx * (head(dens, -1) + tail(dens, -1))) / 2
  if (tot < 1e-12) return(NA_real_)
  total <- 0
  for (h in seq_len(H)) {
    lo  <- full_b[h]; hi <- full_b[h + 1L]
    idx <- if (h < H) x_seq >= lo & x_seq < hi
           else        x_seq >= lo & x_seq <= hi
    if (sum(idx) < 2L) next
    xs <- x_seq[idx]; fs <- dens[idx]; dxh <- diff(xs)
    Wh  <- sum(dxh * (head(fs,      -1) + tail(fs,      -1))) / 2 / tot
    if (Wh < 1e-12) next
    mu  <- sum(dxh * (head(xs * fs, -1) + tail(xs * fs, -1))) / 2 / (Wh * tot)
    mu2 <- sum(dxh * (head(xs^2*fs, -1) + tail(xs^2*fs, -1))) / 2 / (Wh * tot)
    Sh  <- sqrt(max(mu2 - mu^2, 0))
    total <- total + Wh * Sh
  }
  total
}

# Equal-width boundaries
.bmc_ew <- function(x_lo, x_hi, H) {
  seq(x_lo, x_hi, length.out = H + 1L)
}

# Geometric (Gunning & Horgan 2004): b_i = x_lo × (x_hi/x_lo)^(i/H)
# Shifts data if x_lo ≤ 0 (log-equal-width in original space)
.bmc_geom <- function(x_lo, x_hi, H) {
  shift <- 0
  if (x_lo <= 0) { shift <- abs(x_lo) + 1; x_lo <- x_lo + shift; x_hi <- x_hi + shift }
  if (x_lo <= 0 || x_hi <= x_lo) return(NULL)
  (x_lo * (x_hi / x_lo)^(seq(0L, H) / H)) - shift
}

# Dalenius-Hodges cumulative √f: divide ∫√f(y)dy into H equal parts
.bmc_dh <- function(x_seq, dens, H) {
  dx      <- c(0, diff(x_seq))
  sqrtf   <- sqrt(pmax(dens, 0))
  cum_sf  <- cumsum(sqrtf * dx)
  tot     <- tail(cum_sf, 1)
  if (tot < 1e-12) return(seq(x_seq[1], tail(x_seq,1), length.out = H + 1L))
  targets <- seq(0, tot, length.out = H + 1L)
  c(x_seq[1],
    vapply(targets[seq(2L, H)], function(t) x_seq[which.min(abs(cum_sf - t))], numeric(1)),
    tail(x_seq, 1))
}

# Equal-frequency (quantile) boundaries from raw data
.bmc_eq_data <- function(data, H) {
  as.numeric(quantile(data, probs = seq(0, 1, 1/H), type = 2))
}

# Equal-frequency boundaries from density CDF (for distribution pathway)
.bmc_eq_dens <- function(x_seq, dens, H) {
  dx  <- c(diff(x_seq), 0)
  cdf <- cumsum(dens * dx)
  cdf <- cdf / max(cdf, 1e-12)
  targets <- seq(0, 1, length.out = H + 1L)
  c(x_seq[1],
    vapply(targets[seq(2L, H)], function(t) x_seq[which.min(abs(cdf - t))], numeric(1)),
    tail(x_seq, 1))
}

# ==============================================================================
# Server
# ==============================================================================

# ── LaTeX PDF equations for each stratifyR distribution ───────────────────────
STRAT_PDF_EQUATIONS <- list(
  norm    = "\\[ f(y) = \\frac{1}{\\sigma\\sqrt{2\\pi}}\\exp\\!\\left(-\\frac{(y-\\mu)^2}{2\\sigma^2}\\right), \\quad y\\in\\mathbb{R} \\]",
  lnorm   = "\\[ f(y) = \\frac{1}{y\\,\\sigma\\sqrt{2\\pi}}\\exp\\!\\left(-\\frac{(\\ln y-\\mu)^2}{2\\sigma^2}\\right), \\quad y>0 \\]",
  gamma   = "\\[ f(y) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)}\\,y^{\\alpha-1}e^{-\\beta y}, \\quad y>0 \\]",
  weibull = "\\[ f(y) = \\frac{k}{\\lambda}\\!\\left(\\frac{y}{\\lambda}\\right)^{\\!k-1}\\!\\exp\\!\\left(-\\!\\left(\\frac{y}{\\lambda}\\right)^{\\!k}\\right), \\quad y>0 \\]",
  exp     = "\\[ f(y) = \\lambda\\,e^{-\\lambda y}, \\quad y>0 \\]",
  cauchy  = "\\[ f(y) = \\frac{1}{\\pi\\gamma\\!\\left[1+\\left(\\dfrac{y-\\mu_0}{\\gamma}\\right)^{\\!2}\\right]}, \\quad y\\in\\mathbb{R} \\]",
  unif    = "\\[ f(y) = \\frac{1}{b-a}, \\quad a \\leq y \\leq b \\]",
  pareto  = "\\[ f(y) = \\frac{\\alpha\\,y_m^{\\alpha}}{y^{\\alpha+1}}, \\quad y>y_m \\]"
)

server <- function(input, output, session) {

  # ── Palette helper ───────────────────────────────────────────────────────────
  current_palette <- reactive({ palettes[[input$palette]] })

  # ── Plot title helper ────────────────────────────────────────────────────────
  plot_main <- reactive({
    t <- trimws(input$plot_title)
    if (nzchar(t)) t else NULL
  })

  # ── Uploaded file ────────────────────────────────────────────────────────────
  uploaded_df <- reactive({
    req(input$file_upload)
    path <- input$file_upload$datapath
    ext  <- tolower(tools::file_ext(input$file_upload$name))
    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        if (has_readxl) readxl::read_excel(path)
        else stop("readxl not installed. Please install it to read Excel files.")
      } else {
        utils::read.csv(path, stringsAsFactors = FALSE)
      }
    }, error = function(e) { showNotification(e$message, type = "error"); NULL })
  })

  output$ui_col_select <- renderUI({
    df <- uploaded_df(); req(df)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(num_cols) == 0)
      return(div(class="data-preview", "No numeric columns found."))
    selectInput("data_col", "Variable column", choices = num_cols)
  })

  # ── Built-in dataset info ────────────────────────────────────────────────────
  output$ui_builtin_info <- renderUI({
    req(input$builtin_ds)
    parts   <- strsplit(input$builtin_ds, "\\|")[[1]]
    ds_name <- parts[1]; col_name <- parts[2]
    e <- new.env(parent = emptyenv())
    tryCatch({
      utils::data(list = ds_name, package = "stratifyR", envir = e)
      y <- as.numeric(get(ds_name, envir = e)[[col_name]])
      y <- y[is.finite(y)]
      div(class = "data-preview",
        tags$strong(sprintf("n = %d", length(y))),
        sprintf("  |  mean = %.2f  |  sd = %.2f  |  range = [%.2f, %.2f]",
                mean(y), stats::sd(y), min(y), max(y))
      )
    }, error = function(e) NULL)
  })

  # ── Distribution parameter inputs ────────────────────────────────────────────
  output$ui_distr_params <- renderUI({
    req(input$distr_name)
    info  <- distr_params[[input$distr_name]]
    pn    <- info$names
    pd    <- info$defaults
    np    <- length(pn)

    make_input <- function(i)
      numericInput(paste0("dp_", pn[i]), pn[i], value = pd[i], step = 0.01)

    if (np == 1) {
      make_input(1)
    } else if (np == 2) {
      fluidRow(column(6, make_input(1)), column(6, make_input(2)))
    } else {
      tagList(
        fluidRow(column(6, make_input(1)), column(6, make_input(2))),
        make_input(3)
      )
    }
  })

  # ── Extract y vector from current source ─────────────────────────────────────
  get_y <- reactive({
    if (input$data_source == "upload") {
      req(uploaded_df(), input$data_col)
      y <- as.numeric(uploaded_df()[[input$data_col]])
    } else {
      parts   <- strsplit(input$builtin_ds, "\\|")[[1]]
      ds_name <- parts[1]; col_name <- parts[2]
      e <- new.env(parent = emptyenv())
      utils::data(list = ds_name, package = "stratifyR", envir = e)
      y <- as.numeric(get(ds_name, envir = e)[[col_name]])
    }
    y[is.finite(y)]
  })

  # ── Core stratification ───────────────────────────────────────────────────────
  strata_res  <- reactiveVal(NULL)
  run_err     <- reactiveVal(NULL)
  is_running  <- reactiveVal(FALSE)

  observeEvent(input$run_btn, {
    strata_res(NULL); run_err(NULL); is_running(TRUE)

    method_used <- input$solver
    is_dp       <- identical(method_used, "dp")
    prog_msg    <- switch(method_used,
      "dp"     = "▶ DP solver running…",
      "cobyla" = "▶ COBYLA optimising…",
      "global" = "▶ GLOBAL (DIRECT-L + local refinement) running…",
      "exact"  = "▶ EXACT data DP running…")

    shiny::withProgress(message = prog_msg, value = 0, {

      tryCatch({
        h      <- as.integer(input$h_strata)
        n_samp <- as.integer(input$n_sample)
        method <- input$solver

        shiny::incProgress(0.10, detail = "Validating inputs…")

        if (input$data_source == "distr") {
          # ── Distribution pathway ────────────────────────────────────────────
          pn     <- distr_params[[input$distr_name]]$names
          params <- stats::setNames(
            vapply(pn, function(p) {
              v <- input[[paste0("dp_", p)]]
              if (is.null(v)) 0 else as.numeric(v)
            }, numeric(1)),
            pn)

          shiny::incProgress(0.20,
            detail = switch(method_used,
              "dp"     = "Running DP algorithm (O(h·N²))…",
              "cobyla" = "Running COBYLA (20 random starts)…",
              "global" = "Running DIRECT-L global search + local refinement…",
              "exact"  = "Running Dynamic Programming on the data…"))

          res <- strata.distr(
            h       = h,
            initval = input$initval,
            dist    = input$dist_range,
            distr   = input$distr_name,
            params  = params,
            n       = n_samp,
            N       = as.integer(input$distr_N),
            # EXACT needs data; with an assumed distribution the DP solver is exact for it
            method  = if (identical(method, "exact")) "dp" else method
          )

        } else {
          # ── Data pathway ────────────────────────────────────────────────────
          y <- get_y()
          if (n_samp > length(y))
            stop(sprintf("Sample size n (%d) cannot exceed population size N (%d).", n_samp, length(y)))

          shiny::incProgress(0.20,
            detail = switch(method_used,
              "dp"     = "Running DP algorithm (O(h·N²))…",
              "cobyla" = "Running COBYLA (20 random starts)…",
              "global" = "Running DIRECT-L global search + local refinement…",
              "exact"  = "Running Dynamic Programming on the data…"))

          res <- strata.data(
            data   = y,
            h      = h,
            n      = n_samp,
            method = method
          )
        }

        shiny::incProgress(0.60, detail = "Computing sample allocations…")
        strata_res(res)
        shiny::incProgress(0.10, detail = "Done!")

      }, error = function(e) {
        run_err(conditionMessage(e))
      }, finally = {
        is_running(FALSE)
      })

    })
  })

  # ── Status badge ──────────────────────────────────────────────────────────────
  output$ui_status <- renderUI({
    if (isTRUE(is_running())) {
      tagList(
        div(class = "status-pill s-running",
          icon("spinner", class = "fa-spin"), "Computing stratification…"),
        div(class = "progress-wrap",
          div(class = "progress-shimmer")),
        div(class = "progress-label",
          switch(input$solver,
            "dp"     = "DP solver: O(h·N²) — larger strata counts take longer",
            "cobyla" = "COBYLA: gradient-free boundary search (20 starts)…",
            "global" = "GLOBAL: DIRECT-L global phase + local refinement…",
            "exact"  = "EXACT: Dynamic Programming over every cut point in the data…"))
      )
    } else if (!is.null(run_err())) {
      div(class = "status-pill s-error", icon("circle-xmark"), run_err())
    } else if (!is.null(strata_res())) {
      div(class = "status-pill s-ready", icon("circle-check"), "Stratification complete")
    } else {
      div(class = "status-pill s-idle",  icon("circle-info"),  "Configure and press Run")
    }
  })

  # ── Helper: stratum bounds table ─────────────────────────────────────────────
  bounds_df <- reactive({
    res <- strata_res(); req(res)
    H    <- nrow(res$h)
    x_lo <- res$maxval * res$initval
    x_hi <- max(res$maxval * res$finval, max(res$OSB))
    int_osb <- res$OSB[res$OSB > x_lo & res$OSB < x_hi]
    fb   <- c(x_lo, int_osb, x_hi)
    data.frame(
      Stratum  = paste0("S", seq_len(H)),
      Lower    = round(fb[seq_len(H)],       4),
      Upper    = round(fb[seq_len(H) + 1L],  4),
      `Wh`     = round(res$Wh,               4),
      `Sh`     = round(sqrt(pmax(res$Vh, 0)), 4),
      `nh`     = res$nh,
      `Nh`     = if (!is.null(res$Nh)) res$Nh else NA_integer_,
      `fh`     = round(res$fh,               4),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  })

  # ── Introduction page content (static) ───────────────────────────────────────
  intro_page <- div(
    style = "max-width:900px; margin:0 auto; padding:0.4rem 0.2rem 1.5rem;",

    # Hero
    div(class = "intro-hero",
      div(class = "intro-hero-title",
        "Why Construct Optimal Strata?"),
      div(class = "intro-hero-sub",
        "Stratified random sampling divides a heterogeneous population into",
        " homogeneous sub-groups (strata) before sampling.",
        " Choosing boundaries optimally — rather than arbitrarily —",
        " can reduce sampling variance by a factor of 2–5× for the same",
        " total sample size, directly lowering survey costs.")
    ),

    # ── Section 1: The Problem ────────────────────────────────────────────────
    div(class = "intro-section",
      div(class = "intro-section-title",
        div(class = "num-badge", "1"), "The Problem with Simple Random Sampling"),
      div(class = "intro-body",
        tags$p("In a simple random sample (SRS) of size ",
          tags$em("n"), " from a population with variance ",
          tags$em("S²"), ", the variance of the sample mean is:"),
        div(class = "intro-math",
          withMathJax("\\( V(\\bar{y}_{SRS}) = \\dfrac{S^2}{n} \\)")),
        tags$p("When the population is ",
          tags$strong("heterogeneous"), " — for example, a skewed income",
          " distribution where most values are low but a few are very large —",
          " a small sample may miss the rare high-value units entirely,",
          " producing a biased or imprecise estimate.",
          " Cochran (1977, Ch. 5) showed that stratification systematically",
          " exploits known population structure to beat this limit.")
      )
    ),

    # ── Section 2: Stratified Sampling ───────────────────────────────────────
    div(class = "intro-section",
      div(class = "intro-section-title",
        div(class = "num-badge", "2"),
        "Cochran's Framework — Stratified Random Sampling"),
      div(class = "intro-body",
        tags$p("Divide the population of size ", tags$em("N"),
          " into ", tags$em("H"), " strata with sizes ",
          tags$em("N₁, …, N_H"), ".",
          " Draw independent samples of sizes ",
          tags$em("n₁, …, n_H"), " from each stratum",
          " (with ∑nₕ = n).",
          " The stratified estimator of the population mean is:"),
        div(class = "intro-math",
          withMathJax(
            "\\( \\bar{y}_{st} = \\sum_{h=1}^{H} W_h\\,\\bar{y}_h \\)")),
        tags$p("where ", tags$em("Wₕ = Nₕ/N"),
          " is the stratum weight and ",
          tags$em("ȳₕ"), " is the stratum sample mean.",
          " Its variance under ", tags$strong("Neyman (1934) optimal allocation"),
          " (", tags$em("nₕ ∝ Wₕ Sₕ"), ") achieves:"),
        div(class = "intro-math",
          withMathJax(
            "\\( V^*(\\bar{y}_{st}) = \\frac{\\left(\\sum_{h=1}^{H} W_h S_h\\right)^2}{n} \\)")),
        div(class = "intro-key-result",
          "Key insight: minimising V*(ȳ_st) for fixed n is equivalent to",
          " minimising ΣWₕSₕ by choosing the strata boundaries. This is",
          " precisely what stratifyR optimises.")
      )
    ),

    # ── Section 3: Optimal Boundaries ────────────────────────────────────────
    div(class = "intro-section",
      div(class = "intro-section-title",
        div(class = "num-badge", "3"),
        "Constructing Optimal Strata Boundaries"),
      div(class = "intro-body",
        tags$p("Given the stratum weight–SD product ", tags$em("WₕSₕ"),
          ", the boundary construction problem is:"),
        div(class = "intro-math",
          withMathJax(
            "\\[ \\min_{b_1,\\ldots,b_{H-1}} \\sum_{h=1}^{H} W_h(b)\\,S_h(b) \\]")),
        tags$p(tags$strong("Dalenius (1950)"),
          " proved the exact optimality conditions.",
          " ", tags$strong("Dalenius & Hodges (1959)"),
          " proposed the popular '√f rule' approximation.",
          " ", tags$strong("Khan et al. (2008)"),
          " reformulated the problem as a Dynamic Programming",
          " problem that guarantees the",
          tags$em(" globally optimal"), " strata boundaries.",
          " stratifyR 2.0 implements all three approaches.",
          " For continuous data the boundaries are found by integrating",
          " over the fitted parametric density or the empirical distribution.")
      )
    ),

    # ── Section 4: Worked Example ─────────────────────────────────────────────
    div(class = "intro-section",
      div(class = "intro-section-title",
        div(class = "num-badge", "4"),
        "Worked Example — Anaemia Survey (Iron, g/dL)"),
      div(class = "intro-body",
        tags$p("Population: N = 2,703 patients. Survey variable: serum iron.",
          " Fitted distribution: Log-Normal. Sample size: n = 300.",
          " Below are the H = 3 optimal strata from stratifyR:"),
        tags$table(class = "intro-example-table",
          tags$thead(tags$tr(
            tags$th("Stratum"), tags$th("Interval (g/dL)"),
            tags$th("Wₕ"), tags$th("Sₕ"), tags$th("WₕSₕ"), tags$th("nₕ (Neyman)")
          )),
          tags$tbody(
            tags$tr(
              tags$td("S1"), tags$td("[5.0, 12.3)"),
              tags$td("0.342"), tags$td("2.14"), tags$td("0.732"), tags$td("67")),
            tags$tr(
              tags$td("S2"), tags$td("[12.3, 22.6)"),
              tags$td("0.448"), tags$td("3.02"), tags$td("1.353"), tags$td("124")),
            tags$tr(
              tags$td("S3"), tags$td("[22.6, 68.0]"),
              tags$td("0.210"), tags$td("7.86"), tags$td("1.651"), tags$td("109")),
            tags$tr(
              tags$td("Total"), tags$td("—"),
              tags$td("1.000"), tags$td("—"),
              tags$td("3.736"), tags$td("300"))
          )
        ),
        div(class = "intro-gain-box",
          div(class = "intro-gain-card",
            div(class = "gval", "0.0465"),
            div(class = "glbl", "V*(ȳ) — stratified")),
          div(class = "intro-gain-card",
            div(class = "gval", "0.2180"),
            div(class = "glbl", "V(ȳ) — SRS equivalent")),
          div(class = "intro-gain-card highlight",
            div(class = "gval", "4.7×"),
            div(class = "glbl", "Efficiency gain over SRS"))
        ),
        tags$p(style = "margin-top:0.8rem; font-size:0.83rem; color:#64748B;",
          "Equivalently: to achieve the same precision as 300 stratified observations,",
          " you would need ~1,410 observations under SRS — a 79% cost saving.")
      )
    ),

    # ── Section 5: How to Use ─────────────────────────────────────────────────
    div(class = "intro-section",
      div(class = "intro-section-title",
        div(class = "num-badge", "5"), "How to Use This App"),
      div(class = "intro-body",
        div(class = "intro-workflow",
          div(class = "intro-step",
            tags$span(class = "step-num", "① Data"),
            "Choose a built-in dataset, upload your own CSV/Excel file,",
            " or specify a parametric distribution directly."),
          div(class = "intro-step",
            tags$span(class = "step-num", "② Settings"),
            "Set the number of strata H, sample size n, and solver.",
            " DP gives the exact global optimum.",
            " DP is optimal on a fine grid for the fitted distribution;",
            " EXACT is optimal for the data themselves; COBYLA is a fast search",
            " that usually lands within a fraction of a percent of EXACT."),
          div(class = "intro-step",
            tags$span(class = "step-num", "③ Run"),
            "Click ", tags$strong("Run Stratification"),
            ". The app finds optimal strata boundaries (OSB) and computes Neyman allocation."),
          div(class = "intro-step",
            tags$span(class = "step-num", "④ Explore"),
            "Inspect results across eight analytical tabs:",
            tags$ul(style = "margin:0.4rem 0 0 0.5rem; padding-left:1.2rem;",
              tags$li(tags$strong("Optimal Boundaries"), " — summary table, strata statistics, and 2D density plot."),
              tags$li(tags$strong("Design Comparison"), " — benchmark stratified vs SRS and proportional allocation."),
              tags$li(tags$strong("H Selector"), " — plot ΣWₕSₕ and V*(n) against H to choose the number of strata."),
              tags$li(tags$strong("Boundary Methods"), " — compare stratifyR against geometric, cumulative √f, and Lavallée-Hidiroglou boundary construction methods."),
              tags$li(tags$strong("Sample Size"), " — back-calculate the n required to hit a target SE or CV under Neyman, proportional, and SRS."),
              tags$li(tags$strong("Allocation"), " — compare Neyman, proportional, and equal allocation side-by-side for fixed boundaries."),
              tags$li(tags$strong("Cost-Constrained"), " — find the variance-minimising allocation subject to a fieldwork budget and per-stratum costs."),
              tags$li(tags$strong("Visualise"), " — 2D density, 3D surface, and interactive boundary explorer.")
            )),
          div(class = "intro-step",
            tags$span(class = "step-num", "⑤ Export"),
            "Download results as CSV or click",
            tags$strong(" Generate R Code"),
            " for a ready-to-run reproducible script.")
        )
      )
    ),

    # References
    div(class = "intro-ref",
      tags$strong("References:"),
      " Cochran, W.G. (1977). ",
      tags$em("Sampling Techniques"), ", 3rd ed. Wiley. —",
      " Neyman, J. (1934). On the two different aspects of the representative method.",
      " ", tags$em("JRSS"), " 97, 558–625. —",
      " Dalenius, T. & Hodges, J.L. (1959). Minimum variance stratification.",
      " ", tags$em("JASA"), " 54, 88–101. —",
      " Khan, M.G.M. et al. (2008). Determining optimum strata boundaries using",
      " mathematical programming. ", tags$em("Survey Methodology"), " 34(2), 91–102. —",
      " Gunning, P. & Horgan, J.M. (2004). A new algorithm for the construction of",
      " stratum boundaries in skewed populations.",
      " ", tags$em("Survey Methodology"), " 30(2), 159–166. —",
      " Lavallée, P. & Hidiroglou, M.A. (1988). On the stratification of skewed",
      " populations. ", tags$em("Survey Methodology"), " 14(1), 33–43. —",
      " Baillargeon, S. & Rivest, L.-P. (2011). The construction of stratified designs",
      " in R with the package stratification.",
      " ", tags$em("Survey Methodology"), " 37(2), 109–129. —",
      " Hansen, M.H., Hurwitz, W.N. & Madow, W.G. (1953). ",
      tags$em("Sample Survey Methods and Theory"), ". Wiley."
    )
  )

  # ── Main UI ───────────────────────────────────────────────────────────────────
  output$ui_main <- renderUI({
    res <- strata_res()

    no_result_tab <- function(label)
      div(class = "empty-state",
        div(class = "empty-icon", "\U0001F4CA"),
        div(class = "empty-title", "No results yet"),
        div(class = "empty-sub",
          "Run Stratification first — results will appear here."))

    tagList(
      # Metric strip (only when results exist)
      if (!is.null(res)) {
        gap_lbl <- if (!is.null(res$optimality_gap) && !is.na(res$optimality_gap))
                     formatC(res$optimality_gap, format = "e", digits = 2) else "N/A (DP)"
        div(class = "metrics-row",
          div(class = "metric-card",
            div(class = "metric-value", nrow(res$h)),
            div(class = "metric-label", "Strata")),
          div(class = "metric-card",
            div(class = "metric-value", res$nhTot),
            div(class = "metric-label", "Total sample")),
          div(class = "metric-card",
            div(class = "metric-value", round(res$WhShTot, 4)),
            div(class = "metric-label", "ΣWₕSₕ")),
          div(class = "metric-card",
            div(class = "metric-value", gap_lbl),
            div(class = "metric-label", "Optimality gap"))
        )
      },

      navset_card_tab(

        # ── 1. Optimal Boundaries ────────────────────────────────────────────
        nav_panel(tagList(icon("table-list"), " Optimal Boundaries"),
          if (is.null(res)) no_result_tab()
          else tagList(uiOutput("ui_summary"), uiOutput("ui_2d_info"))),

        # ── 2. Design Comparison ─────────────────────────────────────────────
        nav_panel(tagList(icon("scale-balanced"), " Design Comparison"),
          if (is.null(res)) no_result_tab()
          else uiOutput("ui_design_comp")),

        # ── 3. H Selector ────────────────────────────────────────────────────
        nav_panel(tagList(icon("layer-group"), " Strata Count (H)"),
          uiOutput("ui_h_selector")),

        # ── 4. Boundary Methods Comparison ───────────────────────────────────
        nav_panel(tagList(icon("arrows-left-right"), " Boundary Methods"),
          uiOutput("ui_bmc")),

        # ── 5. Sample Size Calculator ─────────────────────────────────────────
        nav_panel(tagList(icon("calculator"), " Sample Size"),
          uiOutput("ui_ssc")),

        # ── 6. Allocation Explorer ────────────────────────────────────────────
        nav_panel(tagList(icon("chart-bar"), " Allocation"),
          uiOutput("ui_alloc")),

        # ── 7. Cost-Constrained ───────────────────────────────────────────────
        nav_panel(tagList(icon("coins"), " Cost-Constrained"),
          uiOutput("ui_cost")),

        # ── 8. Visualise (dropdown) ──────────────────────────────────────────
        nav_menu(
          title  = tagList(icon("chart-line"), " Visualise"),
          align  = "left",

          nav_panel(tagList(icon("chart-area"), " 2D Density Plot"),
            if (is.null(res)) no_result_tab()
            else div(class = "plot-container",
              plotlyOutput("plot_2d", height = "500px"))),

          nav_panel(tagList(icon("cube"), " 3D Surface Plot"),
            if (is.null(res)) no_result_tab()
            else if (has_plotly)
              div(class = "plot-container",
                plotlyOutput("plot_3d", height = "720px"))
            else div(class = "empty-state",
              div("Install plotly:", tags$code("install.packages('plotly')")))),

          nav_panel(tagList(icon("sliders"), " Boundary Explorer"),
            uiOutput("ui_ib_v2"))
        ),

        # ── 4. Notes ─────────────────────────────────────────────────────────
        nav_panel(tagList(icon("circle-info"), " Notes"),
          intro_page)
      )
    )
  })

  # ── H-Selector: reactive storage ─────────────────────────────────────────────
  h_curve_res <- reactiveVal(NULL)

  observeEvent(input$run_h_curve, {
    res0 <- strata_res(); req(res0)
    h_curve_res(NULL)   # clear previous results immediately

    n_val      <- as.integer(input$n_sample)
    max_H      <- 12L
    is_data    <- !is.null(res0$data_internal) && length(res0$data_internal) >= 2L

    # ── For the data pathway, extract the already-fitted distribution from res0
    # so we can use strata.distr() in the loop — this avoids re-running the
    # 9-distribution AIC selection on every H iteration (the main cause of
    # slowness for heavy-tailed data like Cauchy).
    use_cached_distr <- FALSE
    if (is_data) {
      cached_distr  <- tryCatch(res0$distr,  error = function(e) NULL)
      cached_params <- tryCatch({
        fit <- res0$fit
        if (!is.null(fit$estimate)) fit$estimate
        else if (!is.null(fit$fit$estimate)) fit$fit$estimate
        else NULL
      }, error = function(e) NULL)
      cached_initval <- tryCatch(res0$initval, error = function(e) NULL)
      cached_dist    <- tryCatch(res0$dist,    error = function(e) NULL)
      cached_maxval  <- tryCatch(res0$maxval,  error = function(e) NULL)
      cached_N       <- tryCatch(res0$N,       error = function(e) NULL)
      use_cached_distr <- !is.null(cached_distr) && !is.null(cached_params) &&
                          !is.null(cached_initval) && !is.null(cached_dist)
    }

    withProgress(message = "H-Selector", value = 0, {
      rows <- lapply(seq_len(max_H), function(h_i) {
        incProgress(1 / max_H,
                    detail = sprintf("Computing H = %d of %d…", h_i, max_H))
        tryCatch({
          if (use_cached_distr) {
            # Fast path: reuse the already-fitted distribution from the main run.
            # strata.distr() skips the 9-distribution AIC fitting step entirely.
            N_use <- if (!is.null(cached_N) && cached_N > 0) cached_N else
                       as.integer(input$distr_N)
            r <- strata.distr(
              h       = h_i,
              initval = cached_initval,
              dist    = cached_dist,
              distr   = cached_distr,
              params  = cached_params,
              n       = n_val,
              N       = N_use,
              method  = "cobyla")
          } else if (is_data) {
            # Fallback: no cached fit — run strata.data() as before
            pop   <- res0$data_internal
            N_arg <- if (!is.null(res0$N) && res0$N > 0 && res0$N != length(pop))
                       res0$N else NULL
            # strata.data() takes N from length(data); it has no N argument
            r <- strata.data(data = pop, h = h_i, n = n_val, method = "cobyla")
          } else {
            pnames   <- distr_params[[input$distr_name]]$names
            defs     <- distr_params[[input$distr_name]]$defaults
            params_v <- setNames(
              sapply(seq_along(pnames), function(j) {
                v <- input[[paste0("param_", pnames[j])]]
                if (is.null(v)) defs[j] else as.numeric(v)
              }),
              pnames)
            r <- strata.distr(
              h       = h_i,
              initval = as.numeric(input$initval),
              dist    = as.numeric(input$dist_range),
              distr   = input$distr_name,
              params  = params_v,
              n       = n_val,
              N       = as.integer(input$distr_N),
              method  = "cobyla")
          }
          list(H = h_i, WhSh = r$WhShTot, V = r$WhShTot^2 / n_val,
               converged = isTRUE(r$converged))
        }, error = function(e) NULL)
      })
    })

    h_curve_res(Filter(Negate(is.null), rows))
  })

  # ── Boundary Methods Comparison: reactive + observer ─────────────────────────
  bmc_res <- reactiveVal(NULL)

  observeEvent(input$run_bmc, {
    res0 <- strata_res(); req(res0)
    bmc_res(NULL)

    # ── Lazy-install stratification (first time only) ──────────────────────
    if (!requireNamespace("stratification", quietly = TRUE)) {
      withProgress(message = "Installing stratification package…", value = 0.5, {
        install.packages("stratification")
      })
    }

    H       <- nrow(res0$h)
    n_val   <- res0$nhTot
    is_data <- !is.null(res0$data_internal) && length(res0$data_internal) >= 2L
    dc      <- .compute_density_2d(res0)
    x_seq   <- dc$x_seq; dens <- dc$dens
    x_lo    <- dc$x_lo;  x_hi <- dc$x_hi

    results <- withProgress(message = "Boundary Methods Comparison", value = 0, {

      # ── 1. DP (stratifyR) — recompute with same formula as other methods ──
      incProgress(0.1, detail = "DP (stratifyR)…")
      int_osb   <- res0$OSB[res0$OSB > x_lo & res0$OSB < x_hi]
      dp_full_b <- c(x_lo, int_osb, x_hi)
      # Recompute using the same .whsh_from_data / .whsh_from_dens formula so
      # all methods are on an identical numerical footing.
      dp_ws <- tryCatch(
        if (is_data) .whsh_from_data(res0$data_internal, dp_full_b)
        else          .whsh_from_dens(x_seq, dens, dp_full_b),
        error = function(e) res0$WhShTot)   # fallback to stored value
      solver_lbl <- toupper(if (!is.null(res0$method) && nzchar(res0$method)) res0$method else "DP")
      dp_entry <- list(
        method     = sprintf("%s — stratifyR", solver_lbl),
        WhSh       = dp_ws,
        V          = dp_ws^2 / n_val,
        is_optimal = TRUE,
        converged  = TRUE,
        note       = sprintf("stratifyR optimal boundaries (%s solver)", solver_lbl)
      )

      # ── 2. Geometric (Gunning & Horgan 2004) ─────────────────────────────
      incProgress(0.2, detail = "Geometric (Gunning 2004)…")
      geom_b  <- tryCatch(.bmc_geom(x_lo, x_hi, H), error = function(e) NULL)
      geom_ok <- !is.null(geom_b) && all(is.finite(geom_b)) && all(diff(geom_b) > 0)
      geom_ws <- if (geom_ok) tryCatch(
          if (is_data) .whsh_from_data(res0$data_internal, geom_b)
          else          .whsh_from_dens(x_seq, dens, geom_b),
          error = function(e) NA_real_) else NA_real_
      geom_entry <- list(
        method     = "Geometric (Gunning 2004)",
        WhSh       = geom_ws,
        V          = if (is.finite(geom_ws)) geom_ws^2 / n_val else NA_real_,
        is_optimal = FALSE,
        converged  = geom_ok && is.finite(geom_ws),
        note       = "bᵢ = x_lo × (x_hi/x_lo)^(i/H)  —  log-equal-width spacing"
      )

      # ── 3. Cumulative √f (Dalenius & Hodges 1959) ─────────────────────────
      incProgress(0.2, detail = "Cumulative √f (Dalenius-Hodges)…")
      dh_b  <- tryCatch(.bmc_dh(x_seq, dens, H), error = function(e) NULL)
      dh_ok <- !is.null(dh_b) && all(is.finite(dh_b)) && all(diff(dh_b) >= 0)
      dh_ws <- if (dh_ok) tryCatch(
          if (is_data) .whsh_from_data(res0$data_internal, dh_b)
          else          .whsh_from_dens(x_seq, dens, dh_b),
          error = function(e) NA_real_) else NA_real_
      dh_entry <- list(
        method     = "Cum. √f  (Dalenius-Hodges)",
        WhSh       = dh_ws,
        V          = if (is.finite(dh_ws)) dh_ws^2 / n_val else NA_real_,
        is_optimal = FALSE,
        converged  = dh_ok && is.finite(dh_ws),
        note       = "Divide ∪√f(y)dy into H equal parts"
      )

      # ── 4. Lavallée-Hidiroglou (stratification package) ───────────────────
      incProgress(0.3, detail = "Lavallée-Hidiroglou…")
      lh_ws <- tryCatch({
        # Get data vector: raw if available, otherwise simulate from fitted dist
        y_lh <- if (is_data) {
          res0$data_internal
        } else {
          set.seed(42L)
          N_sim  <- 5000L
          params <- tryCatch({
            fit <- res0$fit
            if (!is.null(fit$estimate))          fit$estimate
            else if (!is.null(fit$fit$estimate)) fit$fit$estimate
            else NULL
          }, error = function(e) NULL)
          if (!is.null(params) && !is.null(res0$distr)) {
            sim <- switch(res0$distr,
              norm    = stats::rnorm(N_sim,    mean     = params["mean"],    sd       = params["sd"]),
              lnorm   = stats::rlnorm(N_sim,   meanlog  = params["meanlog"], sdlog    = params["sdlog"]),
              gamma   = stats::rgamma(N_sim,   shape    = params["shape"],   rate     = params["rate"]),
              weibull = stats::rweibull(N_sim, shape    = params["shape"],   scale    = params["scale"]),
              exp     = stats::rexp(N_sim,     rate     = params["rate"]),
              cauchy  = stats::rcauchy(N_sim,  location = params["location"],scale    = params["scale"]),
              unif    = stats::runif(N_sim,    min      = params["min"],     max      = params["max"]),
              NULL)
            if (!is.null(sim)) sim[sim >= x_lo & sim <= x_hi & is.finite(sim)] else NULL
          } else NULL
        }
        if (is.null(y_lh) || length(y_lh) < H * 5L) stop("Insufficient data for L-H")

        # strata.LH requires strictly positive data — shift if needed
        lh_shift <- 0
        if (min(y_lh, na.rm = TRUE) <= 0) {
          lh_shift <- abs(min(y_lh, na.rm = TRUE)) + 1
          y_lh     <- y_lh + lh_shift
        }

        # strata.LH(x, Ls = H, n = ...) — Ls = strata count, n = sample size
        lh_out <- suppressMessages(suppressWarnings(
          stratification::strata.LH(x = y_lh, Ls = H, n = n_val)
        ))

        # bh may be H-1 interior values OR H+1 including endpoints
        bh_raw <- as.numeric(lh_out$bh) - lh_shift   # shift back to original scale
        lh_b   <- if (length(bh_raw) == H - 1L) c(x_lo, bh_raw, x_hi)
                  else if (length(bh_raw) == H + 1L) bh_raw
                  else stop(sprintf("Unexpected boundary count from strata.LH: got %d, expected %d or %d",
                                    length(bh_raw), H - 1L, H + 1L))
        if (!all(is.finite(lh_b)) || any(diff(lh_b) <= 0))
          stop("Non-monotone L-H boundaries")

        if (is_data) .whsh_from_data(res0$data_internal, lh_b)
        else          .whsh_from_dens(x_seq, dens, lh_b)
      }, error = function(e) {
        message("L-H error: ", conditionMessage(e))
        conditionMessage(e)   # return the message string, not NA, so we can display it
      })
      lh_ok  <- is.numeric(lh_ws) && length(lh_ws) == 1L && is.finite(lh_ws)
      lh_err <- if (!lh_ok && is.character(lh_ws)) lh_ws else NULL
      lh_entry <- list(
        method     = "Lavallée-Hidiroglou",
        WhSh       = if (lh_ok) lh_ws else NA_real_,
        V          = if (lh_ok) lh_ws^2 / n_val else NA_real_,
        is_optimal = FALSE,
        converged  = lh_ok,
        err_msg    = lh_err,
        note       = "Iterative boundary-allocation algorithm (Lavallée & Hidiroglou 1988)"
      )

      incProgress(0.2, detail = "Done!")
      list(dp = dp_entry, geom = geom_entry, dh = dh_entry, lh = lh_entry)
    })

    bmc_res(results)
  })

  # ── Design Comparison tab ─────────────────────────────────────────────────────
  output$ui_design_comp <- renderUI({
    res <- strata_res(); req(res)

    cd <- tryCatch(
      compare_designs(res),
      error = function(e) e)
    if (inherits(cd, "error")) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "⚠"),
        div(class = "empty-title", "Design Comparison failed"),
        div(class = "empty-sub", conditionMessage(cd))))
    }

    fmt_v  <- function(v) formatC(v, format = "e", digits = 4)
    fmt_se <- function(v) formatC(v, format = "f", digits = 6)
    fmt_deff <- function(v) if (is.finite(v)) sprintf("%.4f", v) else "—"

    tagList(
      # ── Headline banner ────────────────────────────────────────────────────
      div(class = "dc-saving-banner",
        div(class = "dsb-stat",
          div(class = "dsb-val", sprintf("%.1f×", cd$gain_over_srs)),
          div(class = "dsb-lbl", "More efficient than SRS")),
        div(class = "dsb-div"),
        div(class = "dsb-stat",
          div(class = "dsb-val", sprintf("%.1f×", cd$gain_over_prop)),
          div(class = "dsb-lbl", "More efficient than proportional")),
        div(class = "dsb-div"),
        div(class = "dsb-stat",
          div(class = "dsb-val", format(cd$n_srs_equiv, big.mark = ",")),
          div(class = "dsb-lbl", sprintf("SRS observations needed for same precision (vs n = %d)", cd$n))),
        div(class = "dsb-div"),
        div(class = "dsb-stat",
          div(class = "dsb-val", sprintf("%.1f%%", cd$pct_saving)),
          div(class = "dsb-lbl", "Sample-size saving over SRS"))
      ),

      # ── Key metric cards ───────────────────────────────────────────────────
      div(class = "dc-cards",
        div(class = "dc-card",
          div(class = "dc-val", formatC(cd$S2, format = "g", digits = 5)),
          div(class = "dc-lbl", "S²  population variance")),
        div(class = "dc-card",
          div(class = "dc-val", formatC(cd$V_within, format = "g", digits = 5)),
          div(class = "dc-lbl", "ΣWₕSₕ²  within-stratum component")),
        div(class = "dc-card",
          div(class = "dc-val", formatC(cd$WhShTot, format = "g", digits = 5)),
          div(class = "dc-lbl", "ΣWₕSₕ  Neyman objective")),
        div(class = "dc-card dc-highlight",
          div(class = "dc-val", sprintf("%.4f", cd$deff_opt)),
          div(class = "dc-lbl", "DEFF  Neyman vs SRS")),
        div(class = "dc-card dc-amber",
          div(class = "dc-val", sprintf("%.4f", cd$deff_prop)),
          div(class = "dc-lbl", "DEFF  Proportional vs SRS"))
      ),

      # ── Comparison table ───────────────────────────────────────────────────
      tags$table(class = "dc-table",
        tags$thead(tags$tr(
          tags$th("Design"),
          tags$th("Allocation rule"),
          tags$th("V(ȳ)"),
          tags$th("SE(ȳ)"),
          tags$th("DEFF"),
          tags$th("Relative efficiency")
        )),
        tags$tbody(
          tags$tr(
            tags$td("Simple Random Sampling"),
            tags$td("—"),
            tags$td(fmt_v(cd$V_srs)),
            tags$td(fmt_se(cd$SE_srs)),
            tags$td("1.0000  (reference)"),
            tags$td("1.00×")),
          tags$tr(class = "dc-row-prop",
            tags$td("Stratified — Proportional"),
            tags$td(HTML("n<sub>h</sub> ∝ W<sub>h</sub>")),
            tags$td(fmt_v(cd$V_prop)),
            tags$td(fmt_se(cd$SE_prop)),
            tags$td(fmt_deff(cd$deff_prop)),
            tags$td(sprintf("%.2f×", cd$V_srs / cd$V_prop))),
          tags$tr(class = "dc-row-opt",
            tags$td("Stratified — Neyman (optimal)"),
            tags$td(HTML("n<sub>h</sub> ∝ W<sub>h</sub>S<sub>h</sub>")),
            tags$td(fmt_v(cd$V_opt)),
            tags$td(fmt_se(cd$SE_opt)),
            tags$td(fmt_deff(cd$deff_opt)),
            tags$td(sprintf("%.2f×", cd$gain_over_srs)))
        )
      ),

      # ── Interpretation note ────────────────────────────────────────────────
      div(style = paste0(
            "background:#F8FAFC; border:1px solid #E2E8F0; border-radius:8px;",
            " padding:0.75rem 1.1rem; font-size:0.82rem; color:#475569;",
            " line-height:1.6;"),
        tags$strong("How to read this: "),
        sprintf(
          paste0("With H = %d optimal strata and Neyman allocation (n = %d), ",
                 "the variance of the estimated mean is %.2f× smaller than under SRS ",
                 "and %.2f× smaller than under proportional allocation. ",
                 "To achieve the same precision with SRS alone you would need ",
                 "n = %s observations — a %.1f%% cost saving."),
          cd$H, cd$n,
          cd$gain_over_srs, cd$gain_over_prop,
          format(cd$n_srs_equiv, big.mark = ","), cd$pct_saving),
        tags$br(), tags$br(),
        HTML("DEFF = V<sub>design</sub> / V<sub>SRS</sub> (Kish 1965). ",
             "Values below 1 indicate stratification improves precision over SRS. ",
             "★ marks the Neyman optimal design implemented by stratifyR.")
      )
    )
  })

  # ── Boundary Methods Comparison tab ──────────────────────────────────────────
  output$ui_bmc <- renderUI({
    res0 <- strata_res()
    if (is.null(res0)) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001F4CA"),
        div(class = "empty-title", "No results yet"),
        div(class = "empty-sub",  "Run Stratification first.")))
    }

    rows  <- bmc_res()
    H     <- nrow(res0$h)
    n_val <- res0$nhTot

    banner <- div(class = "bmc-banner",
      div(
        div(class = "bmc-banner-title", "Boundary Methods Comparison"),
        div(class = "bmc-banner-text",
          sprintf("Compares four boundary construction strategies for H = %d strata, n = %d.", H, n_val),
          " ΣWₕSₕ and V*(n) are evaluated under Neyman allocation for each method.",
          sprintf(" stratifyR (%s) is the reference — all gaps are relative to it.",
                  toupper(if (!is.null(res0$method) && nzchar(res0$method)) res0$method else "DP")))
      ),
      actionButton("run_bmc",
        tagList(icon(if (is.null(rows)) "play" else "rotate"),
                if (is.null(rows)) " Run Comparison" else " Recompute"),
        class = if (is.null(rows)) "btn btn-primary"
                else "btn btn-outline-light btn-sm")
    )

    if (is.null(rows)) return(banner)

    # ── Build data frame, sort by WhSh ascending ──────────────────────────
    df <- data.frame(
      Method  = sapply(rows, `[[`, "method"),
      WhSh    = sapply(rows, `[[`, "WhSh"),
      V       = sapply(rows, `[[`, "V"),
      is_opt  = sapply(rows, `[[`, "is_optimal"),
      ok      = sapply(rows, `[[`, "converged"),
      err_msg = sapply(rows, function(r) {
        m <- r$err_msg; if (is.null(m)) "" else as.character(m) }),
      stringsAsFactors = FALSE
    )
    dp_ws <- df$WhSh[df$is_opt]
    if (length(dp_ws) != 1L || !is.finite(dp_ws)) dp_ws <- NA_real_
    # gap_pct: % by which each method's ΣWₕSₕ exceeds DP (always ≥ 0 in theory)
    df$gap_pct <- round(100 * (df$WhSh - dp_ws) / dp_ws, 2)
    df$RE      <- round(dp_ws^2 / df$WhSh^2, 4)               # V_DP / V_method
    df <- df[order(df$WhSh, na.last = TRUE), ]

    tbl_rows <- lapply(seq_len(nrow(df)), function(i) {
      is_dp <- df$is_opt[i]
      ok    <- df$ok[i]
      cls   <- if (is_dp) "bmc-row-dp" else if (!ok) "bmc-row-fail" else ""
      ws_str <- if (is.finite(df$WhSh[i]))
                  formatC(df$WhSh[i], format = "f", digits = 5) else "—"
      v_str  <- if (is.finite(df$V[i]))
                  formatC(df$V[i], format = "e", digits = 4) else "—"
      gap_str <- if (is_dp) "reference"
                 else if (is.finite(df$gap_pct[i])) sprintf("%+.2f%%", df$gap_pct[i])
                 else "—"
      re_str  <- if (is_dp) "1.000×"
                 else if (is.finite(df$RE[i])) sprintf("%.3f×", df$RE[i])
                 else "—"
      err_msg <- df$err_msg[i]
      has_err <- nzchar(err_msg)
      badge   <- if (is_dp)  tags$span(class = "bmc-badge-opt",  "★ optimal")
                 else if (!ok) tags$span(class = "bmc-badge-fail", "✗ failed")
                 else NULL
      err_tip <- if (!ok && has_err)
                   tags$span(style = "font-size:0.72rem; color:#DC2626; display:block; margin-top:2px;",
                              err_msg)
                 else NULL
      tags$tr(class = cls,
        tags$td(tagList(df$Method[i], badge, err_tip)),
        tags$td(ws_str),
        tags$td(v_str),
        tags$td(gap_str),
        tags$td(re_str)
      )
    })

    tagList(
      banner,
      div(class = "plot-container", style = "margin-bottom: 1rem;",
        plotlyOutput("plot_bmc", height = "280px")),
      tags$table(class = "bmc-table",
        tags$thead(tags$tr(
          tags$th("Method"),
          tags$th("ΣWₕSₕ"),
          tags$th("V*(n)"),
          tags$th("Gap vs DP"),
          tags$th("Rel. efficiency vs DP")
        )),
        tags$tbody(tbl_rows)
      ),
      div(class = "hsel-note",
        "All methods evaluated at the same H and n. Boundaries differ; allocation is Neyman-optimal for each set of boundaries."),

      # ── Method explanations ────────────────────────────────────────────────
      div(style = "margin-top: 1.4rem;",

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "1"), "Methods compared"),
          div(class = "intro-body",
            tags$p(tags$strong("DP / COBYLA / GLOBAL — stratifyR:"),
              " Globally optimal boundaries via Dynamic Programming, COBYLA, or GLOBAL (Khan et al. 2008).",
              " DP guarantees the global optimum on a grid for the fitted distribution;",
              " COBYLA is a fast gradient-free alternative;",
              " EXACT runs Dynamic Programming on the data and gives the true optimum for the data;",
              " GLOBAL (DIRECT-L + local refinement) is a second opinion for small H.",
              " This is the reference — all other methods are compared against it."),
            tags$p(tags$strong("Geometric (Gunning & Horgan 2004):"),
              " Places boundaries at geometrically spaced values:",
              " bᵢ = x_lo × (x_hi / x_lo)^(i/H).",
              " Equivalent to equal-width stratification in log space.",
              " Near-optimal for Pareto-type and strongly right-skewed distributions."),
            tags$p(tags$strong("Cumulative √f — Dalenius & Hodges (1959):"),
              " Divides the cumulative square root of the density function, ∫√f(y) dy,",
              " into H equal parts and places boundaries at the division points.",
              " A classical and widely cited approximation that works well across many",
              " distribution families."),
            tags$p(tags$strong("Lavallée-Hidiroglou (1988):"),
              " An iterative algorithm that alternates between optimising stratum boundaries",
              " and recomputing Neyman allocation until the sample sizes stabilise.",
              " Particularly effective for skewed business populations.",
              " Implemented here via the ",
              tags$em("stratification"), " R package (Baillargeon & Rivest 2011).",
              " The package is installed on first use.")
          )
        ),

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "2"), "How to read the results"),
          div(class = "intro-body",
            tags$p(
              "All four methods produce H-1 interior boundary points.",
              " For each set of boundaries, Neyman-optimal allocation is applied using",
              " the same total sample size n, and ΣWₕSₕ is recomputed from scratch.",
              " A smaller ΣWₕSₕ means lower variance for the same n."),
            div(class = "intro-math",
              withMathJax(
                "\\[ V^*(\\bar{y}) = \\frac{(\\Sigma W_h S_h)^2}{n} \\]")),
            tags$p(
              tags$strong("Gap vs DP:"),
              " percentage by which a method's ΣWₕSₕ exceeds the DP minimum.",
              " Negative is impossible (DP is optimal); values near 0% mean the method",
              " matches DP closely for this dataset."),
            tags$p(
              tags$strong("Rel. efficiency vs DP:"),
              " V_DP / V_method. Values below 1 indicate the method is less efficient",
              " than DP; a value of 0.95 means the method's variance is 1/0.95 ≈ 5%",
              " larger than DP's.")
          )
        ),

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "3"), "References"),
          div(class = "intro-body",
            div(class = "intro-ref",
              tags$strong("DP method: "),
              "Khan, M.G.M., Nand, N., & Ahmad, N. (2008). Determining optimum strata boundaries using mathematical programming. ",
              tags$em("Survey Methodology"), " 34(2), 91–102.", tags$br(),
              tags$strong("Geometric method: "),
              "Gunning, P. & Horgan, J.M. (2004). A new algorithm for the construction of stratum boundaries in skewed populations. ",
              tags$em("Survey Methodology"), " 30(2), 159–166.", tags$br(),
              tags$strong("Cumulative √f: "),
              "Dalenius, T. & Hodges, J.L. (1959). Minimum variance stratification. ",
              tags$em("Journal of the American Statistical Association"), " 54, 88–101.", tags$br(),
              tags$strong("Lavallée-Hidiroglou: "),
              "Lavallée, P. & Hidiroglou, M. (1988). On the stratification of skewed populations. ",
              tags$em("Survey Methodology"), " 14(1), 33–43.", tags$br(),
              tags$strong("R package stratification: "),
              "Baillargeon, S. & Rivest, L.-P. (2011). The construction of stratified designs in R with the package stratification. ",
              tags$em("Survey Methodology"), " 37(1), 53–65."
            )
          )
        )
      )
    )
  })

  output$plot_bmc <- renderPlotly({
    req(has_plotly)
    rows <- bmc_res(); req(!is.null(rows))

    df <- data.frame(
      Method  = sapply(rows, `[[`, "method"),
      WhSh    = sapply(rows, `[[`, "WhSh"),
      is_opt  = sapply(rows, `[[`, "is_optimal"),
      ok      = sapply(rows, `[[`, "converged"),
      stringsAsFactors = FALSE
    )
    df <- df[is.finite(df$WhSh), ]
    if (nrow(df) == 0) return(plotly::plot_ly())
    df <- df[order(df$WhSh), ]

    bar_col <- ifelse(df$is_opt, "#059669",
                 ifelse(!df$ok, "#CBD5E1", "#4A6FA5"))

    plotly::plot_ly(
      x             = df$WhSh,
      y             = factor(df$Method, levels = rev(df$Method)),
      type          = "bar",
      orientation   = "h",
      marker        = list(color = bar_col,
                           line  = list(color = "white", width = 1.5)),
      text          = formatC(df$WhSh, format = "f", digits = 5),
      textposition  = "outside",
      textfont      = list(size = 11, color = "#334155"),
      hovertemplate = "<b>%{y}</b><br>ΣWₕSₕ = %{x:.5f}<extra></extra>"
    ) |>
    plotly::layout(
      xaxis = list(
        title    = list(text = "ΣWₕSₕ  (lower = better)", font = list(size = 12)),
        zeroline = FALSE, gridcolor = "#F1F5F9",
        showline = TRUE, linecolor = "#E2E8F0"),
      yaxis = list(title = "", tickfont = list(size = 11)),
      paper_bgcolor = "white", plot_bgcolor  = "white",
      margin = list(t = 20, b = 40, l = 240, r = 100),
      showlegend = FALSE
    )
  })

  # ── Sample Size Calculator ────────────────────────────────────────────────────
  output$ui_ssc <- renderUI({
    res <- strata_res()
    if (is.null(res))
      return(div(class="empty-state",
        div(class="empty-icon","\U0001F4CA"),
        div(class="empty-title","No results yet"),
        div(class="empty-sub","Run Stratification first.")))

    tagList(
      div(class = "bmc-banner",
        div(
          div(class = "bmc-banner-title", "Precision / Sample Size Calculator"),
          div(class = "bmc-banner-text",
            "Enter a target precision and see how many observations are needed",
            " under Neyman allocation, proportional allocation, and SRS.")
        )
      ),
      div(class = "ssc-input-panel",
        div(radioButtons("ssc_metric", "Express target precision as:",
          choices  = c("Standard Error  SE(ȳ)" = "se",
                       "Coefficient of Variation  CV(%)" = "cv"),
          selected = isolate(input$ssc_metric) %||% "se",
          inline   = TRUE)),
        div(style = "flex:0 0 auto; min-width:160px;",
          numericInput("ssc_target_val",
            label = "Target value",
            value = isolate(input$ssc_target_val) %||% 0.05,
            min   = 1e-6, step = 0.001))
      ),
      uiOutput("ui_ssc_results"),
      tags$details(class = "method-panel",
        tags$summary("Methodology & References"),
        div(class = "method-body",
          tags$p(tags$strong("What this does:"),
            " Given a target precision — expressed as a standard error SE(ȳ) or a coefficient",
            " of variation CV(ȳ) = SE(ȳ)/|ȳ| × 100% — this tab back-calculates the minimum",
            " sample size required under three designs."),
          tags$p(tags$strong("Neyman allocation"), " is optimal: it distributes the sample",
            " proportionally to stratum size and within-stratum variability (WₕSₕ), yielding",
            " the smallest variance for a given n. The required sample size is:"),
          div(class = "method-formula", "n_Neyman  =  ⌈ (ΣWₕSₕ)² / SE² ⌉"),
          tags$p(tags$strong("Proportional allocation"), " (nₕ ∝ Wₕ) ignores within-stratum",
            " variance; it is simpler to implement but requires more observations:"),
          div(class = "method-formula", "n_prop  =  ⌈ ΣWₕVₕ / SE² ⌉"),
          tags$p(tags$strong("Simple Random Sampling (SRS)"), " treats the population as",
            " unstratified. Its variance is driven by the overall population variance S²:"),
          div(class = "method-formula", "n_SRS  =  ⌈ S² / SE² ⌉"),
          tags$p("The SE-vs-n curve shows how precision improves as n grows under each design,",
            " with a dashed line marking the target n under Neyman allocation.",
            " The savings percentages quantify how much smaller the Neyman sample is relative",
            " to SRS and proportional allocation at the same precision."),
          div(class = "method-ref",
            tags$strong("References: "),
            "Cochran, W.G. (1977). ",
            tags$em("Sampling Techniques"), ", 3rd ed. Wiley. §5.4–5.5. — ",
            "Lohr, S.L. (2010). ",
            tags$em("Sampling: Design and Analysis"), ", 2nd ed. Brooks/Cole. §4.4. — ",
            "Neyman, J. (1934). On the two different aspects of the representative method.",
            " ", tags$em("Journal of the Royal Statistical Society"), ", 97(4), 558–625.")
        )
      )
    )
  })

  output$ui_ssc_results <- renderUI({
    res <- strata_res(); req(res)
    metric <- input$ssc_metric;  req(metric)
    tval   <- input$ssc_target_val; req(tval); req(is.finite(tval) && tval > 0)

    WSh      <- res$WhShTot
    S2       <- .S2_from_strata_app(res)
    V_within <- sum(res$Wh * res$Vh)

    # Population mean (for CV → SE conversion)
    mu <- if (!is.null(res$data_internal) && length(res$data_internal) >= 2L) {
      mean(res$data_internal, na.rm = TRUE)
    } else {
      dc  <- .compute_density_2d(res)
      dx  <- diff(dc$x_seq)
      tot <- sum(dx * (head(dc$dens,-1) + tail(dc$dens,-1))) / 2
      if (tot > 1e-12)
        sum(dx * (head(dc$x_seq*dc$dens,-1) + tail(dc$x_seq*dc$dens,-1))) / 2 / tot
      else NA_real_
    }

    # Convert CV → SE if needed
    se_target <- if (metric == "se") {
      tval
    } else {
      if (!is.finite(mu) || abs(mu) < 1e-12)
        return(div(class="empty-state",
          div(class="empty-icon","⚠"),
          div(class="empty-title","Cannot compute SE"),
          div(class="empty-sub","Population mean is near zero; please use SE mode.")))
      (tval / 100) * abs(mu)
    }

    # Required n under each design
    n_neyman <- ceiling(WSh^2      / se_target^2)
    n_prop   <- ceiling(V_within   / se_target^2)
    n_srs    <- ceiling(S2         / se_target^2)

    # SE at the current n for reference
    n_cur    <- res$nhTot
    se_cur   <- WSh / sqrt(n_cur)

    # Savings
    save_srs  <- if (n_srs  > 0) round(100*(n_srs  - n_neyman)/n_srs,  1) else NA_real_
    save_prop <- if (n_prop > 0) round(100*(n_prop - n_neyman)/n_prop, 1) else NA_real_

    # Target label
    tgt_lbl <- if (metric == "se") sprintf("SE = %.4g", tval)
               else sprintf("CV = %.2f%%", tval)

    tagList(
      # ── Result boxes ─────────────────────────────────────────────────────
      div(class = "ssc-results-row",
        div(class = "ssc-result-box",
          div(class = "ssc-result-n",   format(n_neyman, big.mark=",")),
          div(class = "ssc-result-lbl", sprintf("n required (Neyman) for %s", tgt_lbl))),
        div(class = "dc-cards", style = "flex:1; align-items:flex-start;",
          div(class = "dc-card",
            div(class = "dc-val", format(n_prop, big.mark=",")),
            div(class = "dc-lbl", "n — Proportional")),
          div(class = "dc-card",
            div(class = "dc-val", format(n_srs, big.mark=",")),
            div(class = "dc-lbl", "n — SRS")),
          div(class = "dc-card dc-highlight",
            div(class = "dc-val", sprintf("%.1f%%", save_srs)),
            div(class = "dc-lbl", "Saving vs SRS")),
          div(class = "dc-card dc-amber",
            div(class = "dc-val", sprintf("%.1f%%", save_prop)),
            div(class = "dc-lbl", "Saving vs Proportional"))
        )
      ),

      # ── SE vs n curve ────────────────────────────────────────────────────
      div(class = "plot-container", style = "margin-bottom:1rem;",
        plotlyOutput("plot_ssc_curve", height = "300px")),

      # ── Note about current run ────────────────────────────────────────────
      div(class = "hsel-note",
        sprintf("Current run: n = %d gives SE = %.5f (Neyman).  Target: %s requires n = %s.",
                n_cur, se_cur, tgt_lbl, format(n_neyman, big.mark=",")))
    )
  })

  output$plot_ssc_curve <- renderPlotly({
    req(has_plotly)
    res <- strata_res(); req(res)
    metric <- input$ssc_metric;  req(metric)
    tval   <- input$ssc_target_val; req(tval); req(is.finite(tval) && tval > 0)

    WSh      <- res$WhShTot
    S2       <- .S2_from_strata_app(res)
    V_within <- sum(res$Wh * res$Vh)
    n_cur    <- res$nhTot

    mu <- if (!is.null(res$data_internal) && length(res$data_internal) >= 2L)
            mean(res$data_internal, na.rm = TRUE)
          else NA_real_

    se_target <- if (metric == "se") tval
                 else if (is.finite(mu) && abs(mu) > 1e-12) (tval/100)*abs(mu)
                 else return(plotly::plot_ly())
    n_neyman  <- ceiling(WSh^2 / se_target^2)

    n_max <- max(n_neyman * 2L, n_cur * 2L, 50L)
    n_seq <- unique(sort(c(seq(1L, n_max, length.out = 200L), n_neyman, n_cur)))

    se_neyman <- WSh      / sqrt(n_seq)
    se_prop   <- sqrt(V_within) / sqrt(n_seq)
    se_srs    <- sqrt(S2)       / sqrt(n_seq)

    y_label <- if (metric == "cv" && is.finite(mu) && abs(mu) > 1e-12)
                 "CV(ȳ)  (%)" else "SE(ȳ)"
    y_scale <- if (metric == "cv" && is.finite(mu) && abs(mu) > 1e-12)
                 100 / abs(mu) else 1

    plotly::plot_ly() |>
      plotly::add_trace(x=n_seq, y=se_srs    *y_scale, name="SRS",
        type="scatter", mode="lines",
        line=list(color="#94A3B8", width=1.8, dash="dot")) |>
      plotly::add_trace(x=n_seq, y=se_prop   *y_scale, name="Proportional",
        type="scatter", mode="lines",
        line=list(color="#F28E2B", width=2)) |>
      plotly::add_trace(x=n_seq, y=se_neyman *y_scale, name="Neyman (optimal)",
        type="scatter", mode="lines",
        line=list(color="#059669", width=2.5)) |>
      plotly::add_trace(
        x=c(n_neyman,n_neyman), y=c(0, se_target*y_scale*1.5),
        type="scatter", mode="lines", showlegend=FALSE,
        line=list(color="#C0392B", width=1.5, dash="dash")) |>
      plotly::layout(
        xaxis = list(title=list(text="Sample size n", font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9"),
        yaxis = list(title=list(text=y_label, font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9"),
        legend = list(orientation="h", x=0.5, xanchor="center",
                      y=-0.18, font=list(size=11)),
        hovermode="x unified",
        paper_bgcolor="white", plot_bgcolor="white",
        margin=list(t=15, b=60, l=70, r=20),
        annotations = list(list(
          x=n_neyman, y=se_target*y_scale*1.55,
          xanchor="center", yanchor="bottom",
          text=sprintf("<b>n = %s</b>", format(n_neyman, big.mark=",")),
          showarrow=FALSE, font=list(size=10, color="#C0392B"),
          bgcolor="#FEF2F2", bordercolor="#C0392B", borderpad=3))
      )
  })

  # ── Allocation Schemes Explorer ───────────────────────────────────────────────
  output$ui_alloc <- renderUI({
    res <- strata_res()
    if (is.null(res))
      return(div(class="empty-state",
        div(class="empty-icon","\U0001F4CA"),
        div(class="empty-title","No results yet"),
        div(class="empty-sub","Run Stratification first.")))

    H     <- nrow(res$h)
    n_val <- res$nhTot
    Wh    <- res$Wh
    Vh    <- res$Vh
    Sh    <- sqrt(pmax(Vh, 0))
    WSh   <- Wh * Sh

    # ── Three allocations ─────────────────────────────────────────────────
    # alloc_raw: falls back to equal allocation when sum(wts)=0 or NaN/NA
    # (can happen when GLOBAL/COBYLA produces single-point strata with Sh=0)
    alloc_raw <- function(wts) {
      s <- sum(wts, na.rm = TRUE)
      if (!is.finite(s) || s <= 0) {
        e <- rep(floor(n_val / H), H); e[1L] <- e[1L] + n_val - sum(e)
        return(pmax(as.integer(e), 1L))
      }
      r <- round(n_val * wts / s)
      r[!is.finite(r) | r < 1L] <- 1L
      r
    }
    neyman_undef <- {s <- sum(WSh, na.rm=TRUE); !is.finite(s) || s <= 0}
    nh_neyman <- alloc_raw(WSh)
    nh_prop   <- alloc_raw(Wh)
    nh_equal  <- { e <- rep(floor(n_val / H), H); e[1] <- e[1] + n_val - sum(e); e }

    # Adjust rounding so totals match n exactly
    fix_total <- function(nh) {
      nh[!is.finite(nh)] <- 1L          # sanitise any leftover NaN/Inf
      diff_n <- n_val - sum(nh, na.rm = TRUE)
      if (isTRUE(diff_n > 0))  nh[order(nh)[seq_len( diff_n)]] <- nh[order(nh)[seq_len( diff_n)]] + 1L
      if (isTRUE(diff_n < 0))  nh[order(-nh)[seq_len(-diff_n)]] <- nh[order(-nh)[seq_len(-diff_n)]] - 1L
      pmax(nh, 1L)
    }
    nh_neyman <- fix_total(nh_neyman)
    nh_prop   <- fix_total(nh_prop)

    # V = Σ Wh² Sh² / nh  (ignoring FPC)
    Wh2Sh2 <- Wh^2 * Vh
    V_neyman <- sum(Wh2Sh2 / nh_neyman)
    V_prop   <- sum(Wh2Sh2 / nh_prop)
    V_equal  <- sum(Wh2Sh2 / nh_equal)

    fmt_v  <- function(v) formatC(v, format="e", digits=4)
    fmt_se <- function(v) formatC(sqrt(max(v,0)), format="f", digits=5)

    strata_lbl <- paste0("S", seq_len(H))

    tagList(
      if (neyman_undef)
        div(class = "hsel-warn",
          tags$strong("⚠ Neyman allocation undefined:"),
          " all within-stratum variances are zero for this solution (e.g. single-point or",
          " constant strata). The Neyman column shows equal allocation as a fallback."),

      div(class = "bmc-banner",
        div(
          div(class = "bmc-banner-title", "Allocation Schemes Explorer"),
          div(class = "bmc-banner-text",
            sprintf("Compares Neyman, proportional, and equal allocation for H = %d strata, n = %d.", H, n_val),
            " The strata boundaries are fixed at the stratifyR solution; only the within-stratum",
            " sample sizes nₕ differ.")
        )
      ),

      # ── Summary cards ──────────────────────────────────────────────────
      div(class = "dc-cards",
        div(class = "dc-card dc-highlight",
          div(class = "dc-val", fmt_v(V_neyman)),
          div(class = "dc-lbl", "V*(ȳ) — Neyman")),
        div(class = "dc-card",
          div(class = "dc-val", fmt_v(V_prop)),
          div(class = "dc-lbl", "V*(ȳ) — Proportional")),
        div(class = "dc-card",
          div(class = "dc-val", fmt_v(V_equal)),
          div(class = "dc-lbl", "V*(ȳ) — Equal")),
        div(class = "dc-card",
          div(class = "dc-val", sprintf("%.3f×", V_prop  / V_neyman)),
          div(class = "dc-lbl", "Prop. / Neyman variance ratio")),
        div(class = "dc-card",
          div(class = "dc-val", sprintf("%.3f×", V_equal / V_neyman)),
          div(class = "dc-lbl", "Equal / Neyman variance ratio"))
      ),

      # ── Grouped bar chart ─────────────────────────────────────────────────
      div(class = "plot-container", style = "margin-bottom:1rem;",
        plotlyOutput("plot_alloc",
          height = paste0(max(320L, min(600L,
            ceiling(max(nh_neyman, nh_prop, nh_equal) * 1.5) + 120L)), "px"))),

      # ── Table ─────────────────────────────────────────────────────────────
      tags$table(class = "dc-table",
        tags$thead(tags$tr(
          tags$th("Stratum"),
          tags$th("Wₕ"),  tags$th("Sₕ"),
          tags$th("nₕ  Neyman"), tags$th("nₕ  Proportional"), tags$th("nₕ  Equal")
        )),
        tags$tbody(lapply(seq_len(H), function(h) {
          tags$tr(
            tags$td(tags$strong(strata_lbl[h])),
            tags$td(round(Wh[h], 4)), tags$td(round(Sh[h], 4)),
            tags$td(tags$strong(nh_neyman[h])),
            tags$td(nh_prop[h]),
            tags$td(nh_equal[h])
          )
        })),
        tags$tfoot(tags$tr(style="font-weight:700; background:#F1F5F9;",
          tags$td("Total"),
          tags$td("1.0000"), tags$td("—"),
          tags$td(sum(nh_neyman)), tags$td(sum(nh_prop)), tags$td(sum(nh_equal))
        ))
      ),

      div(class = "hsel-note", style = "margin-top:0.6rem;",
        sprintf("SE: Neyman = %s | Proportional = %s | Equal = %s",
                fmt_se(V_neyman), fmt_se(V_prop), fmt_se(V_equal))),

      tags$details(class = "method-panel",
        tags$summary("Methodology & References"),
        div(class = "method-body",
          tags$p(tags$strong("What this does:"),
            " For fixed strata boundaries (as determined by stratifyR), three allocation",
            " rules are compared side-by-side. All use the same total sample size n; only",
            " the within-stratum counts nₕ differ."),
          tags$p(tags$strong("Neyman (optimal) allocation"), " minimises V*(ȳ) for a given n",
            " by assigning more observations to strata that are larger and more variable:"),
          div(class = "method-formula", "nₕ  ∝  WₕSₕ     →     nₕ = n × WₕSₕ / ΣWₕSₕ"),
          tags$p(tags$strong("Proportional allocation"), " assigns observations in proportion",
            " to stratum size alone, ignoring within-stratum variance:"),
          div(class = "method-formula", "nₕ  ∝  Wₕ     →     nₕ = n × Wₕ"),
          tags$p(tags$strong("Equal allocation"), " splits the sample evenly across strata",
            " regardless of size or variability. Simple to administer, but often inefficient:"),
          div(class = "method-formula", "nₕ  =  n / H     (rounded)"),
          tags$p("Variance under each allocation is computed as V*(ȳ) = Σ Wₕ²Vₕ/nₕ",
            " (ignoring the finite population correction). The variance ratio (Proportional /",
            " Neyman and Equal / Neyman) shows the cost in efficiency of the simpler designs.",
            " A ratio of 1.20 means proportional allocation needs 20% more observations to",
            " achieve the same precision as Neyman."),
          div(class = "method-ref",
            tags$strong("References: "),
            "Neyman, J. (1934). On the two different aspects of the representative method.",
            " ", tags$em("Journal of the Royal Statistical Society"), ", 97(4), 558–625. — ",
            "Cochran, W.G. (1977). ",
            tags$em("Sampling Techniques"), ", 3rd ed. Wiley. §5.5. — ",
            "Lohr, S.L. (2010). ",
            tags$em("Sampling: Design and Analysis"), ", 2nd ed. §4.3.")
        )
      )
    )
  })

  output$plot_alloc <- renderPlotly({
    req(has_plotly)
    res <- strata_res(); req(res)
    H     <- nrow(res$h)
    n_val <- res$nhTot
    Wh    <- res$Wh; Sh <- sqrt(pmax(res$Vh, 0)); WSh <- Wh * Sh

    alloc_raw <- function(wts) {
      s <- sum(wts, na.rm = TRUE)
      if (!is.finite(s) || s <= 0) {
        e <- rep(floor(n_val / H), H); e[1L] <- e[1L] + n_val - sum(e)
        return(pmax(as.integer(e), 1L))
      }
      r <- round(n_val * wts / s)
      r[!is.finite(r) | r < 1L] <- 1L
      r
    }
    fix_total <- function(nh) {
      nh[!is.finite(nh)] <- 1L
      d <- n_val - sum(nh, na.rm = TRUE)
      if (isTRUE(d > 0))  nh[order(nh)[seq_len( d)]] <- nh[order(nh)[seq_len( d)]] + 1L
      if (isTRUE(d < 0))  nh[order(-nh)[seq_len(-d)]] <- nh[order(-nh)[seq_len(-d)]] - 1L
      pmax(nh, 1L)
    }
    nh_n <- fix_total(alloc_raw(WSh))
    nh_p <- fix_total(alloc_raw(Wh))
    nh_e <- { e <- rep(floor(n_val/H), H); e[1] <- e[1] + n_val - sum(e); e }

    s_lbl   <- paste0("S", seq_len(H))
    y_max   <- max(nh_n, nh_p, nh_e)
    y_range <- c(0, y_max * 1.28)   # 28% headroom for outside labels

    plotly::plot_ly(x = s_lbl, y = nh_n, type = "bar", name = "Neyman",
      marker = list(color = "#059669"), textposition = "outside",
      text = nh_n) |>
      plotly::add_trace(x=s_lbl, y=nh_p, name="Proportional",
        marker=list(color="#F28E2B"), text=nh_p, textposition="outside") |>
      plotly::add_trace(x=s_lbl, y=nh_e, name="Equal",
        marker=list(color="#94A3B8"), text=nh_e, textposition="outside") |>
      plotly::layout(
        barmode = "group",
        xaxis = list(title=list(text="Stratum", font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9"),
        yaxis = list(title=list(text="nₕ  (sample size)", font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9",
                     range = y_range),
        legend = list(orientation="h", x=0.5, xanchor="center",
                      y=-0.22, font=list(size=11)),
        paper_bgcolor="white", plot_bgcolor="white",
        margin=list(t=30, b=60, l=70, r=20)
      )
  })

  # ── Cost-Constrained Optimisation ────────────────────────────────────────────
  cost_res <- reactiveVal(NULL)

  output$ui_cost <- renderUI({
    res <- strata_res()
    if (is.null(res))
      return(div(class="empty-state",
        div(class="empty-icon","\U0001F4CA"),
        div(class="empty-title","No results yet"),
        div(class="empty-sub","Run Stratification first.")))

    H     <- nrow(res$h)
    n_val <- res$nhTot
    Wh    <- res$Wh; Sh <- sqrt(pmax(res$Vh, 0))
    # Sensible default budget: cost-per-unit = 1 everywhere, budget = n
    def_c <- 1

    cost_inputs <- lapply(seq_len(H), function(h)
      div(class = "cc-cost-card",
        tags$label(sprintf("c%d  (stratum %d)", h, h)),
        numericInput(paste0("cc_cost_", h),
          label   = NULL,
          value   = isolate(input[[paste0("cc_cost_", h)]]) %||% def_c,
          min     = 0.001, step = 0.1)
      )
    )

    tagList(
      div(class = "bmc-banner",
        div(
          div(class = "bmc-banner-title", "Cost-Constrained Optimisation"),
          div(class = "bmc-banner-text",
            "Enter per-stratum fieldwork costs cₕ and a total budget C.",
            " The cost-optimal allocation nₕ ∝ WₕSₕ/√cₕ minimises variance",
            " subject to ∑cₕnₕ ≤ C.")
        ),
        actionButton("run_cost",
          tagList(icon("play"), " Compute"),
          class = "btn btn-primary")
      ),

      div(style = "background:#FFFFFF; border:1.5px solid #E2E8F0; border-radius:10px; padding:1rem 1.2rem; margin-bottom:1rem;",
        div(style = "display:flex; gap:1.5rem; flex-wrap:wrap; align-items:flex-end;",
          div(style = "flex:0 0 auto; min-width:160px;",
            numericInput("cc_budget", "Total budget C",
              value = isolate(input$cc_budget) %||% (def_c * n_val),
              min = 1, step = 100)),
          div(style = "flex:1;",
            div(style = "font-size:0.78rem; font-weight:600; color:#1B3A5C; margin-bottom:0.4rem;",
              "Per-stratum cost cₕ:"),
            div(class = "cc-cost-grid", cost_inputs)
          )
        )
      ),

      uiOutput("ui_cost_results"),

      tags$details(class = "method-panel",
        tags$summary("Methodology & References"),
        div(class = "method-body",
          tags$p(tags$strong("What this does:"),
            " When different strata have different fieldwork costs per unit (e.g., rural strata",
            " cost more to survey than urban ones), Neyman allocation is no longer optimal.",
            " This tab finds the allocation that minimises variance for a given total budget C,",
            " where the total cost is ΣcₕNₕ ≤ C."),
          tags$p(tags$strong("Cost-optimal allocation"), " (Cochran 1977, §5.5) extends",
            " Neyman by down-weighting expensive strata:"),
          div(class = "method-formula", "nₕ  ∝  WₕSₕ / √cₕ"),
          tags$p("The total sample size under this allocation is:"),
          div(class = "method-formula",
            "n  =  C × Σ(WₕSₕ/√cₕ)  /  Σ(WₕSₕ√cₕ)"),
          tags$p("The minimum achievable variance for budget C is given analytically by:"),
          div(class = "method-formula", "V_min  =  (ΣWₕSₕ√cₕ)²  /  C"),
          tags$p("For comparison, the tab also shows the ", tags$strong("Neyman allocation"),
            " for the same budget — i.e., ignoring costs and distributing n ∝ WₕSₕ with",
            " n chosen so that ΣcₕNₕ = C. When costs are unequal, cost-optimal allocation",
            " always achieves a lower (or equal) variance than budget-constrained Neyman."),
          tags$p(tags$strong("How to use: "),
            "Set cₕ = 1 for all strata to recover standard Neyman allocation.",
            " Set cₕ proportional to real fieldwork costs (e.g. travel time, interviewer",
            " wages) to find the allocation that makes the most of your budget."),
          div(class = "method-ref",
            tags$strong("References: "),
            "Cochran, W.G. (1977). ",
            tags$em("Sampling Techniques"), ", 3rd ed. Wiley. §5.5 (Cost and variance in",
            " stratified sampling). — ",
            "Hansen, M.H., Hurwitz, W.N. & Madow, W.G. (1953). ",
            tags$em("Sample Survey Methods and Theory"), ". Wiley. Vol. I, §6.9. — ",
            "Lohr, S.L. (2010). ",
            tags$em("Sampling: Design and Analysis"), ", 2nd ed. §4.5.")
        )
      )
    )
  })

  observeEvent(input$run_cost, {
    res <- strata_res(); req(res)
    H   <- nrow(res$h)
    Wh  <- res$Wh; Vh <- res$Vh; Sh <- sqrt(pmax(Vh, 0)); WSh <- Wh * Sh
    C   <- input$cc_budget; req(is.finite(C) && C > 0)

    ch <- vapply(seq_len(H), function(h) {
      v <- input[[paste0("cc_cost_", h)]]
      if (is.null(v) || !is.finite(v) || v <= 0) 1 else v
    }, numeric(1))

    # Cost-optimal allocation: nₕ ∝ WₕSₕ / √cₕ  (Cochran 1977, §5.5)
    sqrt_ch     <- sqrt(ch)
    alloc_wts   <- WSh / sqrt_ch
    sum_wsh_sc  <- sum(WSh * sqrt_ch)          # Σ WₕSₕ√cₕ
    if (sum_wsh_sc < 1e-12) { cost_res(NULL); return() }

    nh_raw      <- C * alloc_wts / sum_wsh_sc
    nh_cost     <- pmax(round(nh_raw), 1L)
    n_total_cost <- sum(nh_cost)

    # Neyman allocation for the same budget
    # nₕ ∝ WₕSₕ → n × WₕSₕ/ΣWₕSₕ, where n = C / (ΣcₕWₕSₕ/ΣWₕSₕ) ...
    sum_WSh     <- sum(WSh)
    sum_cWSh    <- sum(ch * WSh)
    n_neyman_budget <- if (sum_cWSh > 0) C * sum_WSh / sum_cWSh else NA_real_
    nh_neyman_raw   <- if (is.finite(n_neyman_budget)) n_neyman_budget * WSh / sum_WSh
                        else rep(NA_real_, H)
    nh_neyman_b     <- pmax(round(nh_neyman_raw), 1L)

    # Variances: V = Σ Wh² Vh / nh
    Wh2Vh <- Wh^2 * Vh
    V_cost    <- sum(Wh2Vh / nh_cost)
    V_neyman_b <- if (all(is.finite(nh_neyman_b))) sum(Wh2Vh / nh_neyman_b) else NA_real_
    # Theoretical minimum: (Σ WₕSₕ√cₕ)² / C
    V_theory  <- sum_wsh_sc^2 / C

    cost_res(list(
      H = H, C = C, ch = ch, Wh = Wh, Sh = Sh, WSh = WSh,
      nh_cost = nh_cost, nh_neyman_b = nh_neyman_b,
      n_total_cost = n_total_cost,
      n_neyman_budget = n_neyman_budget,
      V_cost = V_cost, V_neyman_b = V_neyman_b, V_theory = V_theory
    ))
  })

  output$ui_cost_results <- renderUI({
    cr <- cost_res(); req(cr)

    gain <- if (is.finite(cr$V_neyman_b) && cr$V_neyman_b > 0)
              round(cr$V_neyman_b / cr$V_cost, 3) else NA_real_

    tbl_rows <- lapply(seq_len(cr$H), function(h)
      tags$tr(
        tags$td(tags$strong(paste0("S", h))),
        tags$td(round(cr$ch[h],    2)),
        tags$td(round(cr$Wh[h],    4)),
        tags$td(round(cr$Sh[h],    4)),
        tags$td(tags$strong(cr$nh_cost[h])),
        tags$td(if(is.finite(cr$nh_neyman_b[h])) cr$nh_neyman_b[h] else "—")
      )
    )

    tagList(
      div(class = "cc-result-banner",
        div(class = "cc-result-stat",
          div(class = "cc-result-val", format(cr$n_total_cost, big.mark=",")),
          div(class = "cc-result-lbl", "Total n achieved")),
        div(style="width:1px;background:rgba(5,150,105,0.25);align-self:stretch;"),
        div(class = "cc-result-stat",
          div(class = "cc-result-val", formatC(cr$V_cost,   format="e", digits=3)),
          div(class = "cc-result-lbl", "V*(ȳ) — cost-optimal")),
        div(style="width:1px;background:rgba(5,150,105,0.25);align-self:stretch;"),
        div(class = "cc-result-stat",
          div(class = "cc-result-val",
            if (is.finite(cr$V_neyman_b)) formatC(cr$V_neyman_b, format="e", digits=3) else "—"),
          div(class = "cc-result-lbl", "V*(ȳ) — Neyman (same budget)")),
        div(style="width:1px;background:rgba(5,150,105,0.25);align-self:stretch;"),
        div(class = "cc-result-stat",
          div(class = "cc-result-val",
            if (is.finite(gain)) sprintf("%.3f×", gain) else "—"),
          div(class = "cc-result-lbl", "Cost-opt. efficiency vs Neyman"))
      ),

      div(class = "plot-container", style = "margin-bottom:1rem;",
        plotlyOutput("plot_cost", height = "300px")),

      tags$table(class = "dc-table",
        tags$thead(tags$tr(
          tags$th("Stratum"), tags$th("cₕ"), tags$th("Wₕ"), tags$th("Sₕ"),
          tags$th("nₕ  cost-optimal"), tags$th("nₕ  Neyman (same C)")
        )),
        tags$tbody(tbl_rows),
        tags$tfoot(tags$tr(style="font-weight:700;background:#F1F5F9;",
          tags$td("Total"), tags$td("—"), tags$td("1.0000"), tags$td("—"),
          tags$td(cr$n_total_cost),
          tags$td(if(is.finite(sum(cr$nh_neyman_b))) sum(cr$nh_neyman_b) else "—")
        ))
      ),
      div(class = "hsel-note", style = "margin-top:0.5rem;",
        sprintf("Theoretical minimum V = (ΣWₕSₕ√cₕ)²/C = %.4e.  Budget C = %.0f.",
                cr$V_theory, cr$C))
    )
  })

  output$plot_cost <- renderPlotly({
    req(has_plotly)
    cr <- cost_res(); req(cr)
    s_lbl   <- paste0("S", seq_len(cr$H))
    ok_n    <- is.finite(cr$nh_neyman_b)
    y_max   <- max(cr$nh_cost, cr$nh_neyman_b[ok_n], na.rm = TRUE)
    y_range <- c(0, y_max * 1.28)

    plotly::plot_ly(x=s_lbl, y=cr$nh_cost, type="bar", name="Cost-Optimal",
      marker=list(color="#059669"), text=cr$nh_cost, textposition="outside") |>
      plotly::add_trace(
        x = s_lbl[ok_n], y = cr$nh_neyman_b[ok_n],
        name = "Neyman (same budget)",
        marker = list(color = "#F28E2B"),
        text = cr$nh_neyman_b[ok_n], textposition = "outside") |>
      plotly::layout(
        barmode = "group",
        xaxis = list(title=list(text="Stratum", font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9"),
        yaxis = list(title=list(text="nₕ", font=list(size=12)),
                     zeroline=FALSE, gridcolor="#F1F5F9",
                     range = y_range),
        legend = list(orientation="h", x=0.5, xanchor="center",
                      y=-0.22, font=list(size=11)),
        paper_bgcolor="white", plot_bgcolor="white",
        margin=list(t=30, b=60, l=70, r=20)
      )
  })

  # ── H-Selector tab ───────────────────────────────────────────────────────────
  output$ui_h_selector <- renderUI({
    res0 <- strata_res()

    if (is.null(res0)) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001F4CA"),
        div(class = "empty-title", "No results yet"),
        div(class = "empty-sub",  "Run Stratification first.")))
    }

    rows  <- h_curve_res()
    cur_H <- nrow(res0$h)

    # ── Banner (always visible) ─────────────────────────────────────────────
    banner <- div(class = "hsel-banner",
      div(
        div(class = "hsel-banner-title", "H-Selector Efficiency Curve"),
        div(class = "hsel-banner-text",
          "Runs stratifyR with COBYLA for H = 1 to 12 and plots ΣWₕSₕ vs H.",
          " The elbow of the curve — detected automatically using the maximum",
          " perpendicular-distance method — marks the recommended H.")
      ),
      actionButton("run_h_curve",
        tagList(icon(if (is.null(rows)) "play" else "rotate"),
                if (is.null(rows)) " Compute H-Curve" else " Recompute"),
        class = if (is.null(rows)) "btn btn-primary"
                else "btn btn-outline-light btn-sm")
    )

    if (is.null(rows)) return(banner)

    # ── Build summary data.frame ─────────────────────────────────────────────
    df <- data.frame(
      H    = sapply(rows, `[[`, "H"),
      WhSh = sapply(rows, `[[`, "WhSh"),
      V    = sapply(rows, `[[`, "V"),
      stringsAsFactors = FALSE)

    # ── Degenerate-result detection ──────────────────────────────────────────
    # COBYLA can return WhSh ≈ 0 (boundary collapse) or WhSh that *increases*
    # with H (the solver wandered to a worse region). Both are convergence
    # failures. Flag them so they are excluded from elbow detection and
    # annotated clearly in the table and plot.
    first_valid_wsh <- df$WhSh[df$WhSh > 0][1]
    degen_threshold <- if (!is.null(first_valid_wsh) && is.finite(first_valid_wsh))
                         first_valid_wsh * 0.02 else 1e-8
    # A result is suspicious if WhSh is near-zero OR if it increased from the
    # preceding reliable value (shouldn't happen for a correctly converged run).
    df$ok <- df$WhSh > degen_threshold
    # also flag increases vs the previous OK row
    prev_ok_wsh <- NA_real_
    for (i in seq_len(nrow(df))) {
      if (df$ok[i]) {
        if (!is.na(prev_ok_wsh) && df$WhSh[i] > prev_ok_wsh * 1.05)
          df$ok[i] <- FALSE
        else
          prev_ok_wsh <- df$WhSh[i]
      }
    }
    n_degen <- sum(!df$ok)

    # % reduction vs previous H (among all rows for display)
    df$gain_pct <- c(NA_real_,
      round(100 * (df$WhSh[-nrow(df)] - df$WhSh[-1]) / df$WhSh[-nrow(df)], 1))

    # ── Stagnation detection ──────────────────────────────────────────────────
    # COBYLA often returns the SAME solution for consecutive H values in
    # heavy-tailed settings (gain < 0.05% = rounds to "-0.0%").
    # Flag runs of 2+ consecutive stagnant-reliable points so they render
    # differently and the warning can mention them.
    stagnant_threshold <- 0.05   # % gain below which we consider it stagnant
    df$stagnant <- FALSE
    for (i in seq_len(nrow(df))) {
      if (df$ok[i] && !is.na(df$gain_pct[i]) &&
          abs(df$gain_pct[i]) < stagnant_threshold)
        df$stagnant[i] <- TRUE
    }
    # Only flag as stagnant if it's part of a run of >= 2
    stag_run <- rle(df$stagnant & df$ok)
    if (any(stag_run$values & stag_run$lengths >= 2)) {
      idx <- 1L
      for (k in seq_along(stag_run$lengths)) {
        end_idx <- idx + stag_run$lengths[k] - 1L
        if (!stag_run$values[k] || stag_run$lengths[k] < 2L)
          df$stagnant[idx:end_idx] <- FALSE
        idx <- end_idx + 1L
      }
    } else {
      df$stagnant[] <- FALSE
    }
    n_stagnant <- sum(df$stagnant)

    # % cumulative reduction vs H = 1
    df$cum_pct <- round(100 * (df$WhSh[1] - df$WhSh) / df$WhSh[1], 1)

    # ── Elbow detection — only on reliable rows ───────────────────────────────
    df_ok <- df[df$ok, , drop = FALSE]
    rec_H <- if (nrow(df_ok) >= 3L) {
      h_sc <- (df_ok$H    - df_ok$H[1])    / max(df_ok$H[nrow(df_ok)]    - df_ok$H[1],    1e-12)
      w_sc <- (df_ok$WhSh - df_ok$WhSh[nrow(df_ok)]) /
                max(df_ok$WhSh[1] - df_ok$WhSh[nrow(df_ok)], 1e-12)
      df_ok$H[which.min(h_sc + w_sc)]
    } else if (nrow(df_ok) >= 1L) max(df_ok$H) else max(df$H)

    # ── Heavy-tailed distribution warning ────────────────────────────────────
    fitted_distr   <- tryCatch(res0$distr, error = function(e) "")
    is_heavy_tailed <- fitted_distr %in% c("cauchy", "pareto")
    heavy_warn <- if (is_heavy_tailed || n_degen > 0 || n_stagnant > 0) {
      div(style = paste0(
            "background:#FFF3CD; border:1px solid #FFC107; border-radius:8px;",
            " padding:0.75rem 1rem; margin-bottom:0.8rem; font-size:0.85rem;",
            " color:#856404; display:flex; gap:0.6rem; align-items:flex-start;"),
        icon("triangle-exclamation", style = "margin-top:2px; flex-shrink:0;"),
        div(
          if (is_heavy_tailed)
            tags$p(style = "margin:0 0 0.3rem 0;",
              tags$strong("Heavy-tailed distribution detected (", fitted_distr, ")."),
              " Very heavy tails make every solver more sensitive.",
              " Treat this curve as a guide and confirm the chosen H with the EXACT solver in the main run.")
          else NULL,
          if (n_degen > 0)
            tags$p(style = "margin:0 0 0.3rem 0;",
              tags$strong(n_degen, " degenerate result(s) detected"),
              sprintf(" (H = %s) and excluded from the elbow calculation.",
                paste(df$H[!df$ok], collapse = ", ")),
              " These rows are marked ⚠ in the table. Degenerate solutions occur when COBYLA",
              " collapses all stratum boundaries to nearly the same point (ΣWₕSₕ ≈ 0),",
              " or when the objective worsened rather than improved with additional strata.")
          else NULL,
          if (n_stagnant > 0)
            tags$p(style = "margin:0;",
              tags$strong(n_stagnant, " stagnant result(s) detected"),
              sprintf(" (H = %s).", paste(df$H[df$stagnant], collapse = ", ")),
              " COBYLA returned essentially the same solution for these consecutive H values",
              " (< 0.05% improvement), shown as grey ◆ on the plot.",
              " Adding a stratum gave almost no gain here, which usually means the extra strata are not needed.")
          else NULL
        )
      )
    } else NULL

    # ── Recommendation box ───────────────────────────────────────────────────
    rec_note <- if (rec_H == cur_H)
      "Your current H sits at the elbow — a good choice."
    else
      sprintf("The curve bends most sharply at H = %d. Beyond this, each extra stratum returns diminishing variance reduction.", rec_H)

    rec_box <- div(class = "hsel-rec-box",
      div(
        div(class = "hsel-rec-h",    paste0("H = ", rec_H)),
        div(class = "hsel-rec-label",
          if (n_degen > 0 || n_stagnant > 0) "Recommended strata (reliable points only)"
          else "Recommended strata")
      ),
      div(class = "hsel-rec-note", rec_note)
    )

    # ── Table rows ───────────────────────────────────────────────────────────
    tbl_rows <- lapply(seq_len(nrow(df)), function(i) {
      is_cur     <- df$H[i] == cur_H
      is_rec     <- df$H[i] == rec_H
      is_degen   <- !df$ok[i]
      is_stagnant <- df$stagnant[i]
      row_cls <- if (is_degen)               "hsel-degen-row"
                 else if (is_stagnant)       "hsel-stagnant-row"
                 else if (is_cur && is_rec)  "hsel-cur-row hsel-rec-row"
                 else if (is_cur)            "hsel-cur-row"
                 else if (is_rec)            "hsel-rec-row"
                 else                        ""
      tag_lbl <- if (is_degen)               " ⚠ degenerate"
                 else if (is_stagnant)       " ◆ stagnant"
                 else if (is_cur && is_rec)  " ◄ current ★ rec"
                 else if (is_cur)            " ◄ current"
                 else if (is_rec)            " ★ rec"
                 else                        ""
      tags$tr(class = row_cls,
        tags$td(tagList(as.character(df$H[i]),
                  tags$small(
                    style = if (is_degen)    "color:#DC6C00; margin-left:4px;"
                            else if (is_stagnant) "color:#94A3B8; margin-left:4px;"
                            else             "color:#94A3B8; margin-left:4px;",
                    tag_lbl))),
        tags$td(if (is_degen) tags$em(style="color:#DC6C00;",
                                 formatC(df$WhSh[i], format = "f", digits = 5))
                else formatC(df$WhSh[i], format = "f", digits = 5)),
        tags$td(formatC(df$V[i], format = "e", digits = 4)),
        tags$td(if (is_degen || is.na(df$gain_pct[i])) "—"
                else sprintf("%.1f%%", df$gain_pct[i])),
        tags$td(if (is_degen) "—"
                else sprintf("%.1f%%", df$cum_pct[i]))
      )
    })

    tagList(
      banner,
      heavy_warn,
      rec_box,
      div(class = "plot-container", style = "margin-bottom: 1rem;",
        plotlyOutput("plot_h_curve", height = "360px")),
      tags$table(class = "hsel-table",
        tags$thead(tags$tr(
          tags$th("H"),
          tags$th("ΣWₕSₕ"),
          tags$th("V*(n)"),
          tags$th("Marginal gain"),
          tags$th("Cumulative reduction vs H=1")
        )),
        tags$tbody(tbl_rows)
      ),
      div(class = "hsel-note",
        "COBYLA solver used throughout (analytical pathway, reusing the fitted distribution from the main run).",
        " Results may differ slightly from a full DP run.",
        if (n_degen   > 0) " Degenerate rows (⚠) are excluded from the elbow calculation." else NULL,
        if (n_stagnant > 0) " Stagnant rows (◆) indicate COBYLA convergence to the same local optimum." else NULL),

      # ── Method explanation ─────────────────────────────────────────────────
      div(style = "margin-top: 1.4rem;",

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "1"), "Why plot ΣWₕSₕ against H?"),
          div(class = "intro-body",
            tags$p(
              "Under Neyman (optimal) allocation the variance of the stratified mean is",
              " ", tags$em("V*(ȳ) = (ΣWₕSₕ)² / n"), ".",
              " Minimising ΣWₕSₕ over the strata boundaries is therefore the core",
              " objective of stratifyR. As H increases, more strata are available to",
              " adapt to the shape of the population distribution, and ΣWₕSₕ decreases."),
            tags$p(
              "However, the improvement is not linear — early strata deliver large gains",
              " while each additional stratum beyond the elbow contributes only marginally.",
              " The H-Selector curve makes this trade-off explicit: pick the H that sits",
              " at the bend, and you capture most of the attainable variance reduction",
              " at minimal survey complexity.")
          )
        ),

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "2"), "The Elbow Method — formalised via Kneedle"),
          div(class = "intro-body",
            tags$p(
              "The ", tags$strong("elbow method"), " is a general visual heuristic:",
              " inspect the curve and pick the H at which it bends most sharply,",
              " after which each additional stratum contributes diminishing returns.",
              " Because 'by eye' detection is subjective, Satopaa et al. (2011) proposed",
              " the ", tags$strong("Kneedle algorithm"), " as a formal, automated way to",
              " locate that same elbow point mathematically.",
              " stratifyR uses Kneedle to remove ambiguity from the selection."),
            tags$p(
              "Given the sequence of points ", tags$em("(H₁, f₁), …, (Hₘ, fₘ)"),
              " where ", tags$em("fₕ = ΣWₕSₕ"), ", the algorithm proceeds in three steps:"),
            tags$p(tags$strong("Step 1 — Normalise both axes to [0, 1]:")),
            div(class = "intro-math",
              withMathJax(
                "\\[ \\tilde{H}_i = \\frac{H_i - H_1}{H_m - H_1}, \\qquad
                    \\tilde{f}_i = \\frac{f_i - f_m}{f_1 - f_m} \\]")),
            tags$p(
              "After normalisation the first point is at (0, 1) and the last at (1, 0)."),
            tags$p(tags$strong("Step 2 — Reference line:")),
            tags$p(
              "Draw the straight line connecting",
              " (0, 1) to (1, 0). Its equation is ",
              tags$em("x + y = 1"), "."),
            tags$p(tags$strong("Step 3 — Find the point of maximum perpendicular distance:")),
            div(class = "intro-math",
              withMathJax(
                "\\[ d_i = \\frac{\\left|\\,\\tilde{H}_i + \\tilde{f}_i - 1\\,\\right|}{\\sqrt{2}} \\]")),
            tags$p(
              "For a convex-decreasing curve (ΣWₕSₕ always decreases, steeply at first),",
              " every interior point lies ", tags$em("below"), " the diagonal, so",
              " ", tags$em("d"), " simplifies to"),
            div(class = "intro-math",
              withMathJax(
                "\\[ d_i = \\frac{1 - \\tilde{H}_i - \\tilde{f}_i}{\\sqrt{2}} \\]")),
            div(class = "intro-key-result",
              "The recommended H is ",
              tags$em("argmin (\\(\\tilde{H}_i + \\tilde{f}_i\\))"),
              " over the interior points i = 2, …, m−1.",
              " This is the Kneedle formalisation of the elbow: the H at which the curve",
              " departs furthest from a straight-line interpolation between the two",
              " extremes. Both the concept (elbow) and its detection (Kneedle) point to",
              " the same answer — Kneedle simply removes the subjectivity of visual inspection.")
          )
        ),

        div(class = "intro-section",
          div(class = "intro-section-title",
            div(class = "num-badge", "3"), "References"),
          div(class = "intro-body",
            div(class = "intro-ref",
              tags$strong("Kneedle algorithm: "),
              "Satopaa, V., Albrecht, J., Irwin, D., & Raghavan, B. (2011).",
              " Finding a 'kneedle' in a haystack: Detecting knee points in system behaviour.",
              " ", tags$em("31st IEEE International Conference on Distributed Computing Systems Workshops"),
              ", pp. 166–171.", tags$br(),
              tags$strong("Optimal stratification: "),
              "Cochran, W.G. (1977). ",
              tags$em("Sampling Techniques"), ", 3rd ed. Wiley. — Ch. 5.",
              tags$br(),
              tags$strong("Neyman allocation: "),
              "Neyman, J. (1934). On the two different aspects of the representative method. ",
              tags$em("Journal of the Royal Statistical Society"), " 97, 558–625.",
              tags$br(),
              tags$strong("Optimum boundaries: "),
              "Khan, M.G.M., Nand, N., & Ahmad, N. (2008). Determining optimum strata",
              " boundaries using mathematical programming. ",
              tags$em("Survey Methodology"), " 34(2), 91–102."
            )
          )
        )
      )
    )
  })

  output$plot_h_curve <- renderPlotly({
    req(has_plotly)
    rows <- h_curve_res(); req(!is.null(rows) && length(rows) >= 2L)
    res0 <- strata_res(); req(res0)
    cur_H <- nrow(res0$h)

    df <- data.frame(
      H    = sapply(rows, `[[`, "H"),
      WhSh = sapply(rows, `[[`, "WhSh"),
      V    = sapply(rows, `[[`, "V"),
      stringsAsFactors = FALSE)

    df$gain_pct <- c(NA_real_,
      round(100 * (df$WhSh[-nrow(df)] - df$WhSh[-1]) / df$WhSh[-nrow(df)], 1))

    # ── Degenerate + stagnant detection (mirror renderUI logic) ─────────────
    first_valid <- df$WhSh[df$WhSh > 0][1]
    degen_thr   <- if (!is.null(first_valid) && is.finite(first_valid))
                     first_valid * 0.02 else 1e-8
    df$ok <- df$WhSh > degen_thr
    prev_ok <- NA_real_
    for (i in seq_len(nrow(df))) {
      if (df$ok[i]) {
        if (!is.na(prev_ok) && df$WhSh[i] > prev_ok * 1.05) df$ok[i] <- FALSE
        else prev_ok <- df$WhSh[i]
      }
    }

    # Stagnation: gain < 0.05% AND part of a run of >= 2
    df$stagnant <- FALSE
    for (i in seq_len(nrow(df))) {
      if (df$ok[i] && !is.na(df$gain_pct[i]) && abs(df$gain_pct[i]) < 0.05)
        df$stagnant[i] <- TRUE
    }
    stag_run <- rle(df$stagnant & df$ok)
    idx <- 1L
    for (k in seq_along(stag_run$lengths)) {
      end_idx <- idx + stag_run$lengths[k] - 1L
      if (!stag_run$values[k] || stag_run$lengths[k] < 2L)
        df$stagnant[idx:end_idx] <- FALSE
      idx <- end_idx + 1L
    }

    # "Good" = ok and not stagnant — used for the solid line and elbow
    df$good <- df$ok & !df$stagnant
    df_good <- df[df$good, , drop = FALSE]
    df_ok   <- df[df$ok,   , drop = FALSE]   # for annotation lookup

    # Elbow on good points only
    rec_H <- if (nrow(df_good) >= 3L) {
      h_sc <- (df_good$H    - df_good$H[1])    / max(df_good$H[nrow(df_good)]    - df_good$H[1],    1e-12)
      w_sc <- (df_good$WhSh - df_good$WhSh[nrow(df_good)]) /
                max(df_good$WhSh[1] - df_good$WhSh[nrow(df_good)], 1e-12)
      df_good$H[which.min(h_sc + w_sc)]
    } else if (nrow(df_good) >= 1L) max(df_good$H)
      else if (nrow(df_ok)   >= 1L) max(df_ok$H)
      else max(df$H)

    is_special <- df$H == cur_H | df$H == rec_H

    # Point colours: degenerate=orange×, stagnant=grey◆, current=red●, elbow=green●, normal=blue●
    pt_col <- ifelse(!df$ok,          "#F59E0B",
               ifelse(df$stagnant,    "#94A3B8",
               ifelse(df$H == cur_H,  "#C0392B",
               ifelse(df$H == rec_H,  "#059669", "#4A6FA5"))))
    pt_sz  <- ifelse(is_special & df$ok & !df$stagnant, 14, 8)
    pt_sym <- ifelse(!df$ok, "x",
               ifelse(df$stagnant, "diamond", "circle"))

    hover_txt <- sprintf(
      paste0("<b>H = %d</b>%s<br>",
             "ΣWₕSₕ = %.5f<br>",
             "V*(n) = %.6f%s<extra></extra>"),
      df$H,
      ifelse(!df$ok, "  ⚠ degenerate",
             ifelse(df$stagnant, "  ◆ stagnant (COBYLA stuck)", "")),
      df$WhSh, df$V,
      ifelse(is.na(df$gain_pct) | !df$ok | df$stagnant, "",
             sprintf("<br>Reduction = −%.1f%%", df$gain_pct)))

    # Gain labels only on good (non-special, non-stagnant, non-degen) points
    gain_lbl <- ifelse(!df$ok | df$stagnant | is_special | is.na(df$gain_pct), "",
                       sprintf("−%.1f%%", df$gain_pct))
    txt_pos  <- ifelse(df$H %% 2 == 0, "top center", "bottom center")

    fig <- plotly::plot_ly()

    # Solid line through good (non-stagnant) points only
    if (nrow(df_good) >= 2L) {
      fig <- fig |> plotly::add_trace(
        x = df_good$H, y = df_good$WhSh,
        type = "scatter", mode = "lines",
        line = list(color = "#CBD5E1", width = 2.2),
        showlegend = FALSE, hoverinfo = "none")
    }

    # Dashed grey line through stagnant points (if any) — connects them visually
    # but signals they're unreliable
    df_stag <- df[df$stagnant, , drop = FALSE]
    # include the last good point before the stagnant run as anchor
    if (nrow(df_stag) >= 1L && nrow(df_good) >= 1L) {
      anchor_H <- max(df_good$H[df_good$H < min(df_stag$H)], na.rm = TRUE)
      anchor_row <- df_good[df_good$H == anchor_H, , drop = FALSE]
      if (nrow(anchor_row) == 1L)
        df_stag_line <- rbind(anchor_row[, c("H","WhSh")], df_stag[, c("H","WhSh")])
      else
        df_stag_line <- df_stag[, c("H","WhSh")]
      if (nrow(df_stag_line) >= 2L) {
        fig <- fig |> plotly::add_trace(
          x = df_stag_line$H, y = df_stag_line$WhSh,
          type = "scatter", mode = "lines",
          line = list(color = "#CBD5E1", width = 1.5, dash = "dot"),
          showlegend = FALSE, hoverinfo = "none")
      }
    }

    # All points
    fig <- fig |> plotly::add_trace(
        x = df$H, y = df$WhSh,
        type = "scatter", mode = "markers+text",
        marker = list(color = pt_col, size = pt_sz, symbol = pt_sym,
                      line = list(color = "white", width = 2)),
        text          = gain_lbl,
        textposition  = txt_pos,
        textfont      = list(size = 10, color = "#64748B"),
        hovertemplate = hover_txt,
        showlegend    = FALSE)

    # Annotations for special points — only on good (non-stagnant, non-degen) rows
    annots <- list()
    if (cur_H %in% df_good$H) {
      y_cur <- df_good$WhSh[df_good$H == cur_H]
      if (cur_H == rec_H) {
        annots <- c(annots, list(list(
          x = cur_H, y = y_cur, xanchor = "center", yanchor = "top",
          text = sprintf("<b>H = %d  (current = elbow)</b>", cur_H),
          showarrow = TRUE, arrowhead = 0,
          arrowcolor = "#059669", arrowwidth = 1.5, ax = 0, ay = 40,
          font = list(size = 11, color = "#065F46"),
          bgcolor = "#F0FDF4", bordercolor = "#059669", borderwidth = 1.5, borderpad = 5)))
      } else {
        annots <- c(annots, list(list(
          x = cur_H, y = y_cur, xanchor = "center", yanchor = "top",
          text = sprintf("<b>H = %d  (current)</b>", cur_H),
          showarrow = TRUE, arrowhead = 0,
          arrowcolor = "#C0392B", arrowwidth = 1.5, ax = 0, ay = 40,
          font = list(size = 11, color = "#C0392B"),
          bgcolor = "#FEF2F2", bordercolor = "#C0392B", borderwidth = 1.5, borderpad = 5)))
      }
    }
    if (rec_H != cur_H && rec_H %in% df_good$H) {
      annots <- c(annots, list(list(
        x = rec_H, y = df_good$WhSh[df_good$H == rec_H],
        xanchor = "center", yanchor = "top",
        text = sprintf("<b>H = %d  (elbow)</b>", rec_H),
        showarrow = TRUE, arrowhead = 0,
        arrowcolor = "#059669", arrowwidth = 1.5, ax = 0, ay = 40,
        font = list(size = 11, color = "#065F46"),
        bgcolor = "#F0FDF4", bordercolor = "#059669", borderwidth = 1.5, borderpad = 5)))
    }

    fig |> plotly::layout(
      xaxis = list(
        title    = list(text = "Number of Strata (H)", font = list(size = 13)),
        tickmode = "linear", tick0 = 1, dtick = 1,
        zeroline = FALSE, gridcolor = "#F1F5F9",
        showline = TRUE, linecolor = "#E2E8F0"),
      yaxis = list(
        title    = list(text = "ΣWₕSₕ  (Neyman objective)", font = list(size = 13)),
        zeroline = FALSE, gridcolor = "#F1F5F9",
        showline = TRUE, linecolor = "#E2E8F0"),
      hovermode     = "closest",
      paper_bgcolor = "white", plot_bgcolor = "white",
      margin        = list(t = 20, b = 60, l = 80, r = 30),
      annotations   = annots
    )
  })

  # ── Summary tab ───────────────────────────────────────────────────────────────
  output$ui_summary <- renderUI({
    res <- strata_res(); req(res)

    solver_lbl <- toupper(res$method)
    distr_lbl  <- if (!is.null(res$distr) && nzchar(res$distr)) res$distr else "data-fitted"
    conv_cls   <- if (isTRUE(res$converged)) "green" else "amber"
    conv_lbl   <- if (isTRUE(res$converged)) "Converged" else "Not converged"

    tagList(
      div(class = "summary-meta",
        div(class = "meta-item",
          span("Solver:"),
          span(class = "meta-badge", solver_lbl)),
        div(class = "meta-item",
          span("Distribution:"),
          span(class = "meta-badge", distr_lbl)),
        div(class = "meta-item",
          span("Status:"),
          span(class = paste("meta-badge", conv_cls), conv_lbl)),
        div(class = "meta-item",
          span("Total variance V*:"),
          span(class = "meta-badge",
            formatC(res$WhShTot^2 / res$nhTot, format = "e", digits = 3)))
      ),
      DT::datatable(
        bounds_df(),
        rownames  = FALSE,
        class     = "compact stripe hover",
        options   = list(
          dom        = "t",
          pageLength = 15,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      ) |>
        DT::formatStyle("Stratum", fontWeight = "bold", color = "#1B3A5C") |>
        DT::formatStyle(c("Lower","Upper"), color = "#C0392B", fontWeight = "600")
    )
  })

  # ── Shared density computation helper ────────────────────────────────────────
  .compute_density_2d <- function(res, n_pts = 512L) {
    pop_data <- res$data_internal
    distr    <- res$distr
    maxval   <- res$maxval
    OSB      <- res$OSB
    x_lo     <- maxval * res$initval
    x_hi     <- max(maxval * res$finval, max(OSB))
    full_B   <- c(x_lo, OSB[OSB > x_lo & OSB < x_hi], x_hi)
    x_seq    <- seq(x_lo, x_hi, length.out = n_pts)

    params <- tryCatch({
      fit <- res$fit
      if (!is.null(fit$estimate))          fit$estimate
      else if (!is.null(fit$fit$estimate)) fit$fit$estimate
      else NULL
    }, error = function(e) NULL)

    dens <- tryCatch({
      p2 <- params
      switch(distr,
        norm    = stats::dnorm(x_seq,    mean     = p2["mean"],     sd       = p2["sd"]),
        lnorm   = stats::dlnorm(x_seq,   meanlog  = p2["meanlog"],  sdlog    = p2["sdlog"]),
        gamma   = stats::dgamma(x_seq,   shape    = p2["shape"],    rate     = p2["rate"]),
        weibull = stats::dweibull(x_seq, shape    = p2["shape"],    scale    = p2["scale"]),
        exp     = stats::dexp(x_seq,     rate     = p2["rate"]),
        cauchy  = stats::dcauchy(x_seq,  location = p2["location"], scale    = p2["scale"]),
        unif    = stats::dunif(x_seq,    min      = p2["min"],      max      = p2["max"]),
        {
          if (!is.null(pop_data)) {
            d2 <- density(pop_data, n = n_pts, from = x_lo, to = x_hi)
            stats::approx(d2$x, d2$y, xout = x_seq, rule = 2)$y
          } else rep(1 / (x_hi - x_lo), n_pts)
        }
      )
    }, error = function(e) rep(1 / (x_hi - x_lo), n_pts))
    dens[!is.finite(dens)] <- 0
    list(x_seq = x_seq, dens = dens, full_B = full_B, x_lo = x_lo, x_hi = x_hi,
         pop_data = pop_data)
  }

  # ── Build shared plotly 2D chart ──────────────────────────────────────────────
  .build_plotly_2d <- function(res, pal, ttl = NULL, show_stats_box = FALSE) {
    req(has_plotly)
    H    <- nrow(res$h)
    OSB  <- res$OSB
    cols <- rep_len(pal, H)

    dc       <- .compute_density_2d(res)
    x_seq    <- dc$x_seq; dens <- dc$dens; full_B <- dc$full_B
    pop_data <- dc$pop_data
    d_max    <- max(dens, na.rm = TRUE)
    x_lo     <- dc$x_lo;  x_hi <- dc$x_hi
    x_rng    <- x_hi - x_lo

    fig <- plotly::plot_ly()

    # ── Histogram ──────────────────────────────────────────────────────────────
    if (!is.null(pop_data) && length(pop_data) >= 10L) {
      hh <- hist(pop_data, breaks = "Sturges", plot = FALSE)
      fig <- fig |>
        plotly::add_bars(
          x = hh$mids, y = hh$density,
          name = "Histogram",
          marker = list(color = "rgba(91,158,214,0.40)",
                        line  = list(color = "rgba(91,158,214,0.75)", width = 0.7)),
          width = diff(hh$breaks)[1],
          hovertemplate = "Midpoint: %{x:.2f}<br>Density: %{y:.5f}<extra></extra>"
        )
    }

    # ── Stratum fills ──────────────────────────────────────────────────────────
    for (h in seq_len(H)) {
      lo_h <- full_B[h]; hi_h <- full_B[h + 1L]
      idx  <- x_seq >= lo_h & x_seq <= hi_h
      if (sum(idx) < 2L) next
      wh  <- res$Wh[h]
      sh  <- round(sqrt(max(res$Vh[h], 0)), 4)
      nh  <- res$nh[h]
      fig <- fig |>
        plotly::add_trace(
          x = c(x_seq[idx], rev(x_seq[idx])),
          y = c(dens[idx],  rep(0, sum(idx))),
          type = "scatter", mode = "lines", fill = "toself",
          fillcolor = grDevices::adjustcolor(cols[h], alpha.f = 0.38),
          line      = list(color = "transparent"),
          name      = sprintf("Stratum %d", h),
          hovertemplate = sprintf(
            "<b>Stratum %d</b><br>Wₕ = %.4f<br>Sₕ = %.4f<br>nₕ = %d<extra></extra>",
            h, wh, sh, nh)
        )
    }

    # ── Density curve ──────────────────────────────────────────────────────────
    distr_lbl <- if (!is.null(res$distr) && nzchar(res$distr)) res$distr else "KDE"
    fig <- fig |>
      plotly::add_trace(
        x = x_seq, y = dens, type = "scatter", mode = "lines",
        line = list(color = "#1a1a2e", width = 2.6),
        name = sprintf("f(y) — %s", distr_lbl),
        hovertemplate = "y = %{x:.3f}<br>f(y) = %{y:.5f}<extra></extra>"
      )

    # ── Boundary lines ─────────────────────────────────────────────────────────
    for (i in seq_along(OSB)) {
      fig <- fig |>
        plotly::add_trace(
          x = rep(OSB[i], 2), y = c(0, d_max * 1.08),
          type = "scatter", mode = "lines",
          line = list(color = "#C0392B", width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "none"
        )
    }

    # ── Boundary annotations (horizontal labels at top, matching screenshot) ───
    sub_h  <- c("₁","₂","₃","₄","₅",
                "₆","₇","₈","₉")
    osb_annots <- lapply(seq_along(OSB), function(i) {
      list(x = OSB[i], y = d_max * 0.52,
           xref = "x", yref = "y",
           xanchor = "center", yanchor = "middle",
           text = sprintf("b%s = %.3f", sub_h[min(i, 9L)], OSB[i]),
           textangle = -90,
           showarrow = FALSE,
           font    = list(size = 10, color = "#C0392B"),
           bgcolor = "rgba(255,255,255,0.60)",
           borderpad = 2)
    })

    # ── Optional stats box (for Interactive Explorer) ─────────────────────────
    stats_annot <- if (show_stats_box) {
      list(list(
        x = 0.99, y = 0.99, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top",
        text = sprintf(
          paste0("<b>Optimal solution</b><br>",
                 "ΣWₕSₕ = %.5f<br>",
                 "V(n) = %.6f<br>",
                 "n<sub>total</sub> = %d"),
          res$WhShTot, res$WhShTot^2 / res$nhTot, res$nhTot),
        showarrow = FALSE,
        font = list(size = 12, color = "#C0392B"),
        bgcolor = "rgba(255,240,240,0.92)",
        bordercolor = "#C0392B", borderwidth = 1.2, borderpad = 8
      ))
    } else list()

    all_annots <- c(osb_annots, stats_annot)

    title_txt <- if (!is.null(ttl) && nzchar(ttl)) ttl else
      sprintf("%d-Strata Optimal Solution", H)

    fig |> plotly::layout(
      title  = list(text = title_txt,
                    font = list(size = 16, color = "#1A2B3C", family = "sans-serif"),
                    x = 0.5, xanchor = "center"),
      xaxis  = list(title = list(text = "Y  (variable value)", font = list(size = 14)),
                    tickfont = list(size = 13),
                    zeroline = FALSE, gridcolor = "#EEEEEE"),
      yaxis  = list(title = list(text = "Density  f(y)", font = list(size = 14)),
                    tickfont = list(size = 13),
                    zeroline = FALSE, gridcolor = "#EEEEEE",
                    range = c(0, d_max * 1.15)),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      hovermode     = "x unified",
      legend        = list(orientation = "h", x = 0.01, y = -0.20,
                           xanchor = "left", font = list(size = 12)),
      margin        = list(t = 60, b = 70, l = 65, r = 25),
      annotations   = all_annots
    )
  }

  # ── 2D plot (plotly, clean — matches screenshot) ──────────────────────────────
  output$plot_2d <- renderPlotly({
    req(has_plotly)
    res <- strata_res(); req(res)
    .build_plotly_2d(res, current_palette(), plot_main(), show_stats_box = TRUE)
  })

  # ── 2D info panel (distribution + strata summary) ─────────────────────────────
  output$ui_2d_info <- renderUI({
    res <- strata_res(); req(res)

    distr <- if (!is.null(res$distr) && nzchar(res$distr)) res$distr else "data"

    dist_disp <- switch(distr,
      norm      = "Normal Distribution",
      lnorm     = "Log-Normal Distribution",
      gamma     = "Gamma Distribution",
      weibull   = "Weibull Distribution",
      exp       = "Exponential Distribution",
      cauchy    = "Cauchy Distribution",
      unif      = "Uniform Distribution",
      pareto    = "Pareto Distribution",
      data      = "Data-fitted (non-parametric)",
      paste0(tools::toTitleCase(distr), " Distribution")
    )

    params <- tryCatch({
      fit <- res$fit
      if (!is.null(fit$estimate))          fit$estimate
      else if (!is.null(fit$fit$estimate)) fit$fit$estimate
      else NULL
    }, error = function(e) NULL)

    greek <- switch(distr,
      norm    = c(mean = "μ",       sd = "σ"),
      lnorm   = c(meanlog = "μ",    sdlog = "σ"),
      gamma   = c(shape = "α",      rate = "β"),
      weibull = c(shape = "k",           scale = "λ"),
      exp     = c(rate = "λ"),
      cauchy  = c(location = "μ₀", scale = "γ"),
      unif    = c(min = "a",             max = "b"),
      NULL
    )

    # LaTeX equation via MathJax
    eq_latex <- STRAT_PDF_EQUATIONS[[distr]]

    # Parameters: "param_name (symbol) = value"
    param_badges <- if (!is.null(params) && length(params) > 0L) {
      lapply(seq_along(params), function(i) {
        nm  <- names(params)[i]
        val <- as.numeric(params[i])
        sym <- if (!is.null(greek) && nm %in% names(greek)) greek[[nm]] else nm
        tags$span(class = "param-badge",
                  sprintf("%s (%s) = %.4f", nm, sym, val))
      })
    } else NULL

    bd   <- bounds_df()
    pal  <- current_palette()
    cols <- rep_len(pal, nrow(bd))

    strata_rows <- lapply(seq_len(nrow(bd)), function(i) {
      row <- bd[i, ]
      div(class = "strata-row",
        tags$span(class = "strata-swatch",
                  style = sprintf("background:%s;", cols[i])),
        tags$span(class = "strata-label",    sprintf("Stratum %d", i)),
        tags$span(class = "strata-interval",
                  sprintf("[%s – %s]",
                          formatC(row$Lower, format = "f", digits = 2, big.mark = ","),
                          formatC(row$Upper, format = "f", digits = 2, big.mark = ","))),
        tags$span(class = "strata-stat", sprintf("Wh = %.4f", row$Wh)),
        tags$span(class = "strata-stat", sprintf("Sh = %.4f", row$Sh)),
        tags$span(class = "strata-stat", sprintf("nh = %d",   row$nh))
      )
    })

    tagList(
      div(class = "info-dist-card",
        div(class = "info-dist-name", dist_disp),
        if (!is.null(eq_latex))
          withMathJax(div(class = "info-eqn", HTML(eq_latex))),
        if (!is.null(param_badges))
          div(class = "info-params", param_badges)
      ),
      div(class = "info-strata", strata_rows)
    )
  })

  # ── 3D plot ────────────────────────────────────────────────────────────────────
  output$plot_3d <- renderPlotly({
    req(has_plotly)
    res <- strata_res(); req(res)
    stratifyR:::plot.strata(res, type    = "3d",
                                palette = current_palette(),
                                main    = plot_main())
  })

  # ── Interactive Explorer v2 — Shiny-native sliders ───────────────────────────
  #
  # Layout:  plotly 2D density plot (reacts to sliders)
  #          stats strip  (current ΣWhSh | optimal | gap)
  #          sliderInput controls panel  (one per internal boundary)
  #
  # Completely avoids plotly's built-in slider coordinate system.

  # ---- helper: compute ΣWhSh for any set of boundary values ------------------
  .ib_whsh <- function(res, cur_b) {
    dc    <- .compute_density_2d(res)
    x_lo  <- dc$x_lo;  x_hi <- dc$x_hi
    x_seq <- dc$x_seq; dens <- dc$dens
    fB    <- c(x_lo, sort(cur_b), x_hi)
    Hh    <- length(cur_b) + 1L
    pop   <- dc$pop_data

    if (!is.null(pop) && length(pop) >= 2L) {
      ds    <- sort(pop)
      N_pop <- if (!is.null(res$N) && res$N > 0) res$N else length(pop)
      obj   <- 0
      for (h in seq_len(Hh)) {
        sl <- if (h == 1L) ds[ds >= fB[h] & ds <= fB[h + 1L]]
              else         ds[ds >  fB[h] & ds <= fB[h + 1L]]
        if (length(sl) < 2L) next
        obj <- obj + (length(sl) / N_pop) * sqrt(stats::var(sl))
      }
      return(obj)
    }

    # density-integral fallback
    tot  <- sum(diff(x_seq) * (head(dens, -1) + tail(dens, -1))) / 2
    Wh_v <- numeric(Hh); Sh_v <- numeric(Hh)
    for (h in seq_len(Hh)) {
      idx <- x_seq >= fB[h] & x_seq < fB[h + 1L]
      if (sum(idx) < 2L) next
      xs <- x_seq[idx]; fs <- dens[idx]; dx <- diff(xs)
      Wh_v[h] <- sum(dx * (head(fs, -1) + tail(fs, -1))) / 2 / max(tot, 1e-12)
      if (Wh_v[h] < 1e-12) next
      mu  <- sum(dx * (head(xs     * fs, -1) + tail(xs     * fs, -1))) / 2 /
               Wh_v[h] / max(tot, 1e-12)
      mu2 <- sum(dx * (head(xs ^ 2 * fs, -1) + tail(xs ^ 2 * fs, -1))) / 2 /
               Wh_v[h] / max(tot, 1e-12)
      Sh_v[h] <- sqrt(max(mu2 - mu ^ 2, 0))
    }
    sum(Wh_v * Sh_v)
  }

  # ---- reactive: current slider boundary values --------------------------------
  ib_cur_b <- reactive({
    res <- strata_res(); req(res)
    H   <- nrow(res$h)
    b   <- sapply(seq_len(H - 1L), function(i) {
      v <- input[[paste0("ib_b_", i)]]
      if (is.null(v)) res$OSB[i] else v
    })
    sort(b)
  })

  # ---- tab container: renders plot + stats strip + slider widgets ---------------
  output$ui_ib_v2 <- renderUI({
    res <- strata_res()

    if (is.null(res)) {
      return(div(class = "empty-state",
        div(class = "empty-icon", "\U0001F4CA"),
        div(class = "empty-title", "No results yet"),
        div(class = "empty-sub",  "Run stratification first.")))
    }
    if (!has_plotly) {
      return(div(class = "empty-state",
        div("Install plotly:", tags$code("install.packages('plotly')"))))
    }

    H      <- nrow(res$h)
    n_sl   <- H - 1L
    x_lo   <- res$maxval * res$initval
    x_hi   <- max(res$maxval * res$finval, max(res$OSB))
    x_rng  <- x_hi - x_lo
    # full_B has H+1 entries: lo, b1…b(H-1), hi
    opt_B  <- res$OSB[res$OSB > x_lo & res$OSB < x_hi]
    full_B <- c(x_lo, opt_B, x_hi)

    sliders_ui <- lapply(seq_len(n_sl), function(i) {
      lo_i <- round(full_B[i]      + 0.02 * x_rng, 4)
      hi_i <- round(full_B[i + 2L] - 0.02 * x_rng, 4)
      if (lo_i >= hi_i) lo_i <- round(full_B[i] + x_rng * 0.001, 4)
      stp  <- max(0.0001, round(x_rng / 400, 4))
      div(style = "flex: 1 1 160px; padding: 0 10px;",
        sliderInput(
          inputId = paste0("ib_b_", i),
          label   = HTML(sprintf(
            "<b style='color:#1B3A5C'>b<sub>%d</sub></b>", i)),
          min   = lo_i,
          max   = hi_i,
          value = round(res$OSB[i], 4),
          step  = stp,
          width = "100%"
        )
      )
    })

    tagList(
      div(class = "plot-container",
        plotlyOutput("plot_ib_v2", height = "460px")),
      div(style = "margin-top: 0.55rem;",
        uiOutput("ui_ib_stats")),
      div(style = paste0(
            "margin-top: 0.6rem; background: #F3F4F6;",
            " border-radius: 10px; padding: 0.85rem 1.1rem;",
            " border: 1px solid #E2E8F0;"),
        div(style = paste0(
              "font-size: 0.68rem; font-weight: 700;",
              " letter-spacing: 1.5px; text-transform: uppercase;",
              " color: #94A3B8; margin-bottom: 0.55rem;"),
          "■ Boundary Controls  —  drag to explore"),
        div(style = "display: flex; flex-wrap: wrap; gap: 4px;",
          sliders_ui)
      )
    )
  })

  # ---- reactive plot -----------------------------------------------------------
  output$plot_ib_v2 <- renderPlotly({
    req(has_plotly)
    res   <- strata_res(); req(res)
    cur_b <- ib_cur_b()

    H    <- nrow(res$h)
    cols <- rep_len(current_palette(), H)

    dc       <- .compute_density_2d(res)
    x_seq    <- dc$x_seq; dens <- dc$dens
    pop_data <- dc$pop_data
    d_max    <- max(dens, na.rm = TRUE)
    x_lo     <- dc$x_lo;  x_hi <- dc$x_hi

    full_B_cur <- c(x_lo, cur_b, x_hi)
    opt_OSB    <- res$OSB[res$OSB > x_lo & res$OSB < x_hi]

    fig <- plotly::plot_ly()

    # Histogram (if raw data available)
    if (!is.null(pop_data) && length(pop_data) >= 10L) {
      hh <- hist(pop_data, breaks = "Sturges", plot = FALSE)
      fig <- fig |>
        plotly::add_bars(
          x = hh$mids, y = hh$density, name = "Histogram",
          marker = list(color = "rgba(91,158,214,0.38)",
                        line  = list(color = "rgba(91,158,214,0.72)", width = 0.6)),
          width = diff(hh$breaks)[1],
          hovertemplate = "Mid: %{x:.2f}  Density: %{y:.5f}<extra></extra>")
    }

    # Stratum fills using CURRENT slider boundaries
    for (h in seq_len(H)) {
      lo_h <- full_B_cur[h]; hi_h <- full_B_cur[h + 1L]
      idx  <- x_seq >= lo_h & x_seq <= hi_h
      if (sum(idx) < 2L) next
      fig <- fig |>
        plotly::add_trace(
          x = c(x_seq[idx], rev(x_seq[idx])),
          y = c(dens[idx],  rep(0, sum(idx))),
          type = "scatter", mode = "lines", fill = "toself",
          fillcolor = grDevices::adjustcolor(cols[h], alpha.f = 0.38),
          line      = list(color = "transparent"),
          name      = sprintf("Stratum %d", h),
          hovertemplate = sprintf("<b>Stratum %d</b><extra></extra>", h))
    }

    # Density curve
    distr_lbl <- if (!is.null(res$distr) && nzchar(res$distr)) res$distr else "KDE"
    fig <- fig |>
      plotly::add_trace(
        x = x_seq, y = dens, type = "scatter", mode = "lines",
        line = list(color = "#1a1a2e", width = 2.5),
        name = sprintf("f(y) — %s", distr_lbl),
        hovertemplate = "y=%{x:.3f}  f(y)=%{y:.5f}<extra></extra>")

    # Optimal boundary reference lines (green dotted)
    for (osb in opt_OSB) {
      fig <- fig |>
        plotly::add_trace(
          x = rep(osb, 2), y = c(0, d_max * 1.08),
          type = "scatter", mode = "lines",
          line = list(color = "rgba(16,120,80,0.48)", width = 1.6, dash = "dot"),
          showlegend = FALSE, hoverinfo = "none")
    }

    # Current slider boundaries (solid red)
    for (bv in cur_b) {
      fig <- fig |>
        plotly::add_trace(
          x = rep(bv, 2), y = c(0, d_max * 1.08),
          type = "scatter", mode = "lines",
          line = list(color = "#C0392B", width = 2.2),
          showlegend = FALSE, hoverinfo = "none")
    }

    # Rotated boundary value labels
    sub_h <- c("₁","₂","₃","₄","₅",
               "₆","₇","₈","₉")
    bnd_annots <- lapply(seq_along(cur_b), function(i) {
      list(x = cur_b[i], y = d_max * 0.52,
           xref = "x", yref = "y",
           xanchor = "center", yanchor = "middle",
           text     = sprintf("b%s=%.3f", sub_h[min(i, 9L)], cur_b[i]),
           textangle = -90, showarrow = FALSE,
           font    = list(size = 10, color = "#C0392B"),
           bgcolor = "rgba(255,255,255,0.65)", borderpad = 2)
    })

    # Stats box (top-right corner)
    cur_obj <- tryCatch(.ib_whsh(res, cur_b), error = function(e) NA_real_)
    opt_obj <- res$WhShTot
    pct_gap <- if (is.finite(cur_obj) && opt_obj > 0)
                 100 * (cur_obj - opt_obj) / opt_obj else NA_real_
    gap_col <- if (!is.na(pct_gap) && abs(pct_gap) <= 0.5) "#065F46" else "#C0392B"
    gap_str <- if (is.finite(pct_gap)) sprintf("%+.2f%%", pct_gap) else "N/A"

    stats_annot <- list(
      x = 0.99, y = 0.99, xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "top",
      text = sprintf(
        paste0("<b>Current:</b>  ΣW<sub>h</sub>S<sub>h</sub> = %.5f<br>",
               "<b>Optimal:</b> ΣW<sub>h</sub>S<sub>h</sub> = %.5f<br>",
               "<span style='color:%s'><b>Gap = %s</b></span>"),
        cur_obj, opt_obj, gap_col, gap_str),
      showarrow   = FALSE,
      font        = list(size = 11.5, color = "#1A2B3C"),
      bgcolor     = "rgba(255,248,240,0.94)",
      bordercolor = "#C0392B", borderwidth = 1.3, borderpad = 9)

    ttl <- { t <- plot_main()
             if (!is.null(t) && nzchar(t)) t else
               sprintf("%d-Strata Boundary Explorer", H) }

    fig |> plotly::layout(
      title  = list(text = ttl,
                    font = list(size = 15, color = "#1A2B3C"),
                    x = 0.5, xanchor = "center"),
      xaxis  = list(title    = list(text = "Y  (variable value)", font = list(size = 13)),
                    tickfont = list(size = 12),
                    zeroline = FALSE, gridcolor = "#EEEEEE"),
      yaxis  = list(title    = list(text = "Density  f(y)", font = list(size = 13)),
                    tickfont = list(size = 12),
                    zeroline = FALSE, gridcolor = "#EEEEEE",
                    range    = c(0, d_max * 1.18)),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      hovermode     = "x unified",
      legend        = list(orientation = "h", x = 0.01, y = -0.18,
                           xanchor = "left", font = list(size = 11)),
      margin        = list(t = 55, b = 70, l = 65, r = 25),
      annotations   = c(bnd_annots, list(stats_annot))
    )
  })

  # ---- stats strip below plot --------------------------------------------------
  output$ui_ib_stats <- renderUI({
    res   <- strata_res(); req(res)
    cur_b <- ib_cur_b()
    cur_obj <- tryCatch(.ib_whsh(res, cur_b), error = function(e) NA_real_)
    opt_obj <- res$WhShTot
    n_tot   <- res$nhTot
    pct_gap <- if (is.finite(cur_obj) && opt_obj > 0)
                 round(100 * (cur_obj - opt_obj) / opt_obj, 2) else NA_real_
    gap_cls <- if (!is.na(pct_gap) && abs(pct_gap) <= 0.5) "green" else "amber"
    gap_lbl <- if (is.finite(pct_gap)) sprintf("%+.2f%%", pct_gap) else "N/A"

    div(class = "summary-meta",
      div(class = "meta-item",
        span("Current ΣWₕSₕ:"),
        span(class = "meta-badge",
          if (is.finite(cur_obj)) formatC(cur_obj, digits = 5, format = "f") else "—")),
      div(class = "meta-item",
        span("Optimal ΣWₕSₕ:"),
        span(class = "meta-badge green",
          formatC(opt_obj, digits = 5, format = "f"))),
      div(class = "meta-item",
        span("Gap vs optimal:"),
        span(class = paste("meta-badge", gap_cls), gap_lbl)),
      div(class = "meta-item",
        span("V*(n) current:"),
        span(class = "meta-badge",
          if (is.finite(cur_obj))
            formatC(cur_obj ^ 2 / n_tot, digits = 6, format = "f") else "—"))
    )
  })

  if (FALSE) {
    # ── placeholder – never executed ──────────────────────────────────────────
    n_sliders    <- 0L

    # ── Inline objective functions (replicate package internals exactly) ─────────
    data_sorted <- if (!is.null(pop_data) && length(pop_data) >= 2L)
      sort(pop_data) else NULL

    obj_data <- function(b_test) {
      fB  <- c(x_lo, sort(b_test), x_hi)
      Hh  <- length(b_test) + 1L
      obj <- 0
      for (h in seq_len(Hh)) {
        sl <- if (h == 1L)
          data_sorted[data_sorted >= fB[h]  & data_sorted <= fB[h + 1L]]
        else
          data_sorted[data_sorted >  fB[h]  & data_sorted <= fB[h + 1L]]
        Nh <- length(sl)
        if (Nh < 2L) next
        obj <- obj + (Nh / N_pop) * sqrt(stats::var(sl))
      }
      obj
    }

    obj_dens <- function(b_test) {
      fB  <- c(x_lo, sort(b_test), x_hi)
      Hh  <- length(b_test) + 1L
      tot <- sum(diff(x_seq) * (head(dens, -1) + tail(dens, -1))) / 2
      Wh  <- numeric(Hh); Sh <- numeric(Hh)
      for (h in seq_len(Hh)) {
        idx <- x_seq >= fB[h] & x_seq < fB[h + 1L]
        if (sum(idx) < 2L) next
        xs <- x_seq[idx]; fs <- dens[idx]; dx <- diff(xs)
        Wh[h] <- sum(dx * (head(fs,  -1) + tail(fs,  -1))) / 2 / max(tot, 1e-12)
        if (Wh[h] < 1e-12) next
        mu  <- sum(dx * (head(xs     * fs, -1) + tail(xs     * fs, -1))) /
               2 / Wh[h] / max(tot, 1e-12)
        mu2 <- sum(dx * (head(xs^2   * fs, -1) + tail(xs^2   * fs, -1))) /
               2 / Wh[h] / max(tot, 1e-12)
        Sh[h] <- sqrt(max(mu2 - mu^2, 0))
      }
      sum(Wh * Sh)
    }

    obj_fn <- if (!is.null(data_sorted)) obj_data else obj_dens

    # ── Precompute slider grids + step labels ────────────────────────────────────
    b_grids    <- vector("list", n_sliders)
    steps_list <- vector("list", n_sliders)

    for (bi in seq_len(n_sliders)) {
      b_lo_i <- full_B[bi]       + 0.02 * x_rng
      b_hi_i <- full_B[bi + 2L]  - 0.02 * x_rng
      if (b_lo_i >= b_hi_i) b_lo_i <- full_B[bi] + 1e-6
      b_grid <- seq(b_lo_i, b_hi_i, length.out = n_steps)
      b_grids[[bi]] <- b_grid

      steps_list[[bi]] <- lapply(seq_len(n_steps), function(si) {
        b_test  <- sort(replace(internal_OSB, bi, b_grid[si]))
        obj_val <- tryCatch(obj_fn(b_test), error = function(e) NA_real_)
        if (!is.finite(obj_val)) obj_val <- 0

        # annotations[1] = dynamic stats box (index 0=header, 1=stats)
        list(
          label  = "",   # hide rail tick labels; currentvalue prefix shows the value
          method = "update",
          args   = list(
            list(),
            list(
              `annotations[1].text` = sprintf(
                "<b>b<sub>%d</sub> = %.4f</b><br>ΣW<sub>h</sub>S<sub>h</sub> = %.5f<br>V(n) = %.6f",
                bi, b_grid[si], obj_val, obj_val^2 / n_total)
            )
          )
        )
      })
    }

    # ── Slider layout sizing (bottom grey-box, full width) ───────────────────────
    px_per_slider <- 110L                             # px allocated per slider
    margin_b_px   <- 100L + n_sliders * px_per_slider # total bottom margin (100px base for x-axis title clearance)
    h_px          <- max(580L, 440L + n_sliders * px_per_slider) # 440 = 355 plot + 85 top, keeps plot area fixed despite larger margin base
    step_paper    <- px_per_slider / h_px             # paper-unit height per slider
    bottom_y      <- 82.0 / h_px                      # gap at very bottom (clear x-axis label)

    sub_h <- c("₁","₂","₃","₄","₅",
               "₆","₇","₈","₉")

    slider_specs <- lapply(seq_len(n_sliders), function(bi) {
      opt_step <- which.min(abs(b_grids[[bi]] - OSB[bi])) - 1L
      y_pos    <- bottom_y + (n_sliders - bi) * step_paper
      list(
        active       = opt_step,
        currentvalue = list(
          prefix  = paste0("b", sub_h[min(bi, 9L)], " = "),
          font    = list(size = 11, color = "#C0392B"),
          visible = TRUE,
          offset  = 8
        ),
        x       = 0.10,
        y       = y_pos,
        len     = 0.90,
        xanchor = "left",
        yanchor = "top",
        pad     = list(t = 24, b = 4),
        ticklen = 0,
        steps   = steps_list[[bi]]
      )
    })

    # ── Build base figure ────────────────────────────────────────────────────────
    fig <- plotly::plot_ly()

    if (!is.null(pop_data) && length(pop_data) >= 10L) {
      hh <- hist(pop_data, breaks = "Sturges", plot = FALSE)
      fig <- fig |>
        plotly::add_bars(
          x = hh$mids, y = hh$density, name = "Histogram",
          marker = list(color = "rgba(91,158,214,0.40)",
                        line  = list(color = "rgba(91,158,214,0.75)", width = 0.7)),
          width = diff(hh$breaks)[1],
          hovertemplate = "Midpoint: %{x:.2f}<br>Density: %{y:.5f}<extra></extra>"
        )
    }

    for (h in seq_len(H)) {
      lo_h <- full_B[h]; hi_h <- full_B[h + 1L]
      idx  <- x_seq >= lo_h & x_seq <= hi_h
      if (sum(idx) < 2L) next
      wh <- res$Wh[h]
      sh <- round(sqrt(max(res$Vh[h], 0)), 4)
      nh <- res$nh[h]
      fig <- fig |>
        plotly::add_trace(
          x = c(x_seq[idx], rev(x_seq[idx])),
          y = c(dens[idx],  rep(0, sum(idx))),
          type = "scatter", mode = "lines", fill = "toself",
          fillcolor = grDevices::adjustcolor(cols[h], alpha.f = 0.38),
          line      = list(color = "transparent"),
          name      = sprintf("Stratum %d", h),
          hovertemplate = sprintf(
            "<b>Stratum %d</b><br>Wₕ=%.4f  Sₕ=%.4f  nₕ=%d<extra></extra>",
            h, wh, sh, nh)
        )
    }

    distr_lbl <- if (!is.null(res$distr) && nzchar(res$distr)) res$distr else "KDE"
    fig <- fig |>
      plotly::add_trace(
        x = x_seq, y = dens, type = "scatter", mode = "lines",
        line = list(color = "#1a1a2e", width = 2.6),
        name = sprintf("f(y) — %s", distr_lbl),
        hovertemplate = "y=%{x:.3f}  f(y)=%{y:.5f}<extra></extra>"
      )

    for (i in seq_along(OSB)) {
      fig <- fig |>
        plotly::add_trace(
          x = rep(OSB[i], 2), y = c(0, d_max * 1.08),
          type = "scatter", mode = "lines",
          line = list(color = "#C0392B", width = 2, dash = "dash"),
          showlegend = FALSE, hoverinfo = "none"
        )
    }

    # ── Annotations — ORDER MATTERS: index 0 = header, 1 = stats (slider target)
    ibe_header <- list(
      x = 0.655, y = 0.99, xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      text = sprintf(
        "<b style='font-size:12px'>⚙ Boundary Explorer</b><br><span style='color:#555'>%d strata &nbsp;│&nbsp; n = %d</span>",
        H, n_total),
      showarrow   = FALSE,
      font        = list(size = 11.5, color = "#1A2B3C"),
      bgcolor     = "#DDE8FA",
      bordercolor = "#3B5FAE",
      borderwidth = 2,
      borderpad   = 9
    )

    # Stats box goes BELOW the legend — estimate legend height from item count
    n_legend_items <- H + 2L   # Histogram + H strata + density line
    legend_h_paper <- (n_legend_items * 26L + 20L) / h_px   # realistic row height
    stats_y        <- max(0.05, 0.78 - legend_h_paper - 0.10) # clear gap below legend

    init_txt <- sprintf(
      "<b>Optimal boundaries</b><br>ΣW<sub>h</sub>S<sub>h</sub> = %.5f<br>V(n) = %.6f",
      res$WhShTot, res$WhShTot^2 / n_total)
    dyn_stats <- list(
      x = 0.655, y = stats_y, xref = "paper", yref = "paper",
      xanchor = "left", yanchor = "top",
      text = init_txt, showarrow = FALSE,
      font    = list(size = 11, color = "#C0392B"),
      bgcolor = "rgba(255,240,240,0.92)",
      bordercolor = "#C0392B", borderwidth = 1.2, borderpad = 8
    )

    sub_h2 <- c("₁","₂","₃","₄","₅",
                "₆","₇","₈","₉")
    osb_annots <- lapply(seq_along(OSB), function(i) {
      list(x = OSB[i], y = d_max * 0.52,
           xref = "x", yref = "y",
           xanchor = "center", yanchor = "middle",
           text = sprintf("b%s = %.3f", sub_h2[min(i, 9L)], OSB[i]),
           textangle = -90,
           showarrow = FALSE,
           font    = list(size = 10, color = "#C0392B"),
           bgcolor = "rgba(255,255,255,0.60)",
           borderpad = 2)
    })

    # annotation[0]=header, [1]=dyn_stats (slider updates this), [2+]=osb labels
    all_annots <- c(list(ibe_header, dyn_stats), osb_annots)

    ttl <- { t <- plot_main(); if (!is.null(t) && nzchar(t)) t else
      sprintf("%d-Strata Boundary Explorer", H) }

    # Grey box behind slider area
    margin_b_paper <- margin_b_px / h_px
    slider_box <- list(
      type = "rect", layer = "below",
      xref = "paper", yref = "paper",
      x0 = -0.05, x1 = 1.05,
      y0 = -0.005, y1 = margin_b_paper + 0.005,
      fillcolor = "#F3F4F6",
      line = list(color = "#D1D5DB", width = 1)
    )

    fig |> plotly::layout(
      height = h_px,
      title  = list(text = ttl,
                    font = list(size = 15, color = "#1A2B3C"),
                    x = 0.31, xanchor = "center"),
      xaxis  = list(domain   = c(0, 0.62),
                    title    = list(text = "Y  (variable value)", font = list(size = 14)),
                    tickfont = list(size = 12),
                    zeroline = FALSE, gridcolor = "#EEEEEE"),
      yaxis  = list(title    = list(text = "Density  f(y)", font = list(size = 14)),
                    tickfont = list(size = 12),
                    zeroline = FALSE, gridcolor = "#EEEEEE",
                    range    = c(0, d_max * 1.15)),
      sliders      = slider_specs,
      shapes       = list(slider_box),
      legend       = list(orientation = "v",
                          x = 0.655, y = 0.78,
                          xanchor = "left", yanchor = "top",
                          bgcolor     = "rgba(255,255,255,0.85)",
                          bordercolor = "#DDDDDD", borderwidth = 1,
                          font        = list(size = 11)),
      paper_bgcolor = "white",
      plot_bgcolor  = "white",
      hovermode     = "x unified",
      margin        = list(t = 65, b = margin_b_px, l = 95, r = 20),
      annotations   = all_annots
    )
  }   # end if(FALSE) placeholder

  # ── Generate R Code ───────────────────────────────────────────────────────────
  .build_r_code <- function(input, res) {

    h   <- as.integer(input$h_strata)
    n   <- as.integer(input$n_sample)
    mtd <- input$solver

    header <- paste0(
      "# ================================================================\n",
      "# stratifyR 2.0 — Reproducible R Code\n",
      "# Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n",
      "# ================================================================\n\n",
      "library(stratifyR)\n"
    )

    if (input$data_source == "distr") {
      # ---- distribution pathway -----------------------------------------------
      pn   <- distr_params[[input$distr_name]]$names
      pv   <- vapply(pn, function(p) {
        v <- input[[paste0("dp_", p)]]
        if (is.null(v)) 0 else as.numeric(v)
      }, numeric(1))
      p_lines <- paste(
        sprintf("    %-10s = %s", pn, formatC(pv, format = "g", digits = 6)),
        collapse = ",\n")

      data_call <- sprintf(
        "\nres <- strata.distr(\n  h       = %dL,\n  initval = %s,\n  dist    = %s,\n  distr   = \"%s\",\n  params  = c(\n%s\n  ),\n  n       = %dL,\n  N       = %dL,\n  method  = \"%s\"\n)",
        h,
        formatC(as.numeric(input$initval),    format = "g", digits = 6),
        formatC(as.numeric(input$dist_range), format = "g", digits = 6),
        input$distr_name,
        p_lines,
        n,
        as.integer(input$distr_N),
        mtd)

    } else if (input$data_source == "builtin") {
      # ---- built-in dataset pathway -------------------------------------------
      parts    <- strsplit(input$builtin_ds, "\\|")[[1]]
      ds_name  <- parts[1]; col_name <- parts[2]

      N_arg <- if (!is.null(res) && !is.null(res$N) && res$N > 0 && res$N != res$nhTot)
        sprintf(",\n  N      = %dL", as.integer(res$N)) else ""

      data_call <- sprintf(
        "\n# Load built-in data\ndata(\"%s\", package = \"stratifyR\")\ny <- %s$%s\n\nres <- strata.data(\n  data   = y,\n  h      = %dL,\n  n      = %dL,\n  method = \"%s\"\n)",
        ds_name, ds_name, col_name,
        h, n, mtd)

    } else {
      # ---- uploaded file pathway -----------------------------------------------
      col_name <- if (!is.null(input$data_col)) input$data_col else "your_column"

      N_arg <- if (isTRUE(input$use_N) && !is.null(input$pop_N))
        sprintf(",\n  N      = %dL", as.integer(input$pop_N)) else ""

      data_call <- sprintf(
        "\n# Replace the path with your actual file\ny_df <- read.csv(\"your_data.csv\", stringsAsFactors = FALSE)\ny    <- y_df$%s\n\nres <- strata.data(\n  data   = y,\n  h      = %dL,\n  n      = %dL,\n  method = \"%s\"\n)",
        col_name, h, n, mtd)
    }

    # ---- result annotation (if a run has been completed) --------------------
    result_block <- ""
    if (!is.null(res)) {
      x_lo    <- res$maxval * res$initval
      x_hi    <- max(res$maxval * res$finval, max(res$OSB))
      int_osb <- res$OSB[res$OSB > x_lo & res$OSB < x_hi]
      osb_lines <- paste(
        sprintf("#   b%d = %.4f", seq_along(int_osb), int_osb),
        collapse = "\n")
      Nh_lines  <- paste(
        sprintf("#   Stratum %d:  nh = %d", seq_along(res$nh), res$nh),
        collapse = "\n")
      conv_txt <- if (isTRUE(res$converged)) "YES" else
        if (is.null(res$converged)) "N/A (DP)" else "NO"

      result_block <- paste0(
        "\n\n",
        "# ── Result summary (from app run) ────────────────────────\n",
        sprintf("# Method          : %s\n", toupper(mtd)),
        sprintf("# H (strata)      : %d\n", nrow(res$h)),
        sprintf("# Total n         : %d\n", res$nhTot),
        sprintf("# ΣWhSh           : %.6f\n", res$WhShTot),
        sprintf("# V*(n)           : %.8f\n", res$WhShTot^2 / res$nhTot),
        "# Converged       : ", conv_txt, "\n",
        if (!is.na(res$optimality_gap) && is.finite(res$optimality_gap))
          sprintf("# Optimality gap  : %.4f%%\n", 100 * res$optimality_gap) else "",
        "#\n",
        "# Optimal strata boundaries (OSB):\n",
        osb_lines, "\n",
        "#\n",
        "# Neyman sample allocation:\n",
        Nh_lines, "\n",
        "# ─────────────────────────────────────────────────────────"
      )
    }

    # ---- visualisation calls ------------------------------------------------
    viz_block <- paste0(
      "\n\n",
      "summary(res)\n\n",
      "plot(res)                           # 2D density + strata\n",
      "plot(res, type = \"3d\")              # 3D surface\n",
      "plot(res, type = \"interactive\")     # boundary explorer\n"
    )

    paste0(header, data_call, result_block, viz_block)
  }

  observeEvent(input$gen_code_btn, {
    res  <- strata_res()
    code <- .build_r_code(input, res)

    showModal(modalDialog(
      title = tagList(
        icon("code", style = "color:#1D4ED8; margin-right:6px;"),
        "Generated R Code"
      ),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        tags$button(
          id        = "copy_r_code",
          class     = "btn btn-primary r-code-copy-btn",
          onclick   = paste0(
            "var el=document.getElementById('r_code_pre');",
            "navigator.clipboard.writeText(el.innerText).then(function(){",
            "  document.getElementById('copy_r_code').innerHTML='&#10003; Copied!';",
            "  setTimeout(function(){",
            "    document.getElementById('copy_r_code').innerHTML='&#128203; Copy to clipboard';",
            "  },2000);",
            "});"
          ),
          HTML("&#128203; Copy to clipboard")
        ),
        modalButton("Close")
      ),
      tags$pre(
        id    = "r_code_pre",
        class = "r-code-block",
        code
      )
    ))
  })

  # ── CSV download ───────────────────────────────────────────────────────────────
  output$dl_results <- downloadHandler(
    filename = function()
      paste0("stratifyR_", Sys.Date(), "_h", input$h_strata, "_n", input$n_sample, ".csv"),
    content = function(file) {
      df <- bounds_df()
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
}

# ==============================================================================
shinyApp(ui, server, options = list(launch.browser = TRUE))
