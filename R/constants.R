BIDS_ENTITY_ORDER <- c(
  "sub", "ses", "sample", "task", "tracksys",
  "acq", "ce", "trc", "stain", "rec", "dir",
  "run", "mod", "echo", "flip", "inv", "mt",
  "part", "proc", "hemi", "space", "split", "recording",
  "chunk", "seg", "res", "den", "label", "desc"
)

AUTO_EXTRACTORS <- list(
  SeriesDescription = list("task-(?P<task>[a-zA-Z0-9]+)"),
  PhaseEncodingDirection = list("(?P<dir>(j|i)-?)"),
  EchoNumber = list("(?P<echo>[0-9])")
)

AUTO_ENTITIES <- list(
  anat_IRT1 = c("inv"),
  anat_MEGRE = c("echo"),
  anat_MESE = c("echo"),
  anat_MP2RAGE = c("inv"),
  anat_MPM = c("flip", "mt"),
  anat_MTS = c("flip", "mt"),
  anat_MTR = c("mt"),
  anat_VFA = c("flip"),
  func_cbv = c("task"),
  func_bold = c("task"),
  func_sbref = c("task"),
  func_event = c("task"),
  func_stim = c("task"),
  func_phase = c("task"),
  fmap_epi = c("dir"),
  fmap_m0scan = c("dir"),
  fmap_TB1DAM = c("flip"),
  fmap_TB1EPI = c("echo", "flip"),
  fmap_TB1SRGE = c("echo", "inv"),
  perf_physio = c("task"),
  perf_stim = c("task")
)

ENTITY_DIR_MAP <- c(
  "j-" = "AP",
  "j" = "PA",
  "i-" = "LR",
  "i" = "RL",
  "AP" = "AP",
  "PA" = "PA",
  "LR" = "LR",
  "RL" = "RL"
)

PATH_SIDECAR_CHANGE_KEYS <- c("IntendedFor", "Sources")

ALLOWED_SEARCH_METHODS <- c("fnmatch", "re")
ALLOWED_DUP_METHODS <- c("run", "dup")
ALLOWED_BIDS_URI <- c("URI", "relative")

TMP_DIR_NAME <- "tmp_dcm2bids"
HELPER_DIR_NAME <- "helper"
DEFAULT_DCM2NIIX_OPTIONS <- "-b y -ba y -z y -f '%3s_%f_%p_%t'"
