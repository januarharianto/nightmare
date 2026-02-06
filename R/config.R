# Configuration for NIGHTMARE application

NIGHTMARE_CONFIG <- list(
  ui = list(
    version = "0.0.9000",
    app_name = "NIGHTMARE",
    subtitle = "New Incredibly Glitchy Hacking Tool to Manage Records that Enrage me",
    author = "Januar Harianto"
  ),

  data = list(
    sample_data_dir = "_sample-data",
    canvas_file = "canvas gradebook.csv",
    consids_file = "special considerations.csv",
    plans_file = "plans.xlsx",
    default_unit = "BIOL2022"
  ),

  backup = list(
    dir = "backups",
    auto_interval_minutes = 5
  ),

  notifications = list(
    duration_message = 5,
    duration_undo_redo = 3
  ),

  risk_scoring = list(
    low_threshold = 25,
    high_threshold = 50,
    policy_limit_days = 10,
    warning_days = 5
  )
)
