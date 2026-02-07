# Configuration for NIGHTMARE application

NIGHTMARE_CONFIG <- list(
  ui = list(
    version = "0.0.9000",
    app_name = "NIGHTMARE",
    subtitle = "New Incredibly Glitchy Hacking Tool to Manage Records that Enrage me",
    author = "Januar Harianto"
  ),

  data = list(
    data_dir = "data"
  ),

  notifications = list(
    duration_message = 5
  ),

  risk_scoring = list(
    low_threshold = 25,
    high_threshold = 50,
    policy_limit_days = 10,
    warning_days = 5
  )
)
