

let seconds_since_midnight (hours : int) (mins : int) (secs : int) : int =
  secs + 60 * (mins + 60 * hours)

TEST "midnight" = seconds_since_midnight 0 0 0 = 0
TEST "12:59:59PM" = seconds_since_midnight 23 59 59 = 86399
TEST "1:30AM" = seconds_since_midnight 1 30 0 = 3600 + 30 * 60


