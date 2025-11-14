#' @import dplyr
NULL

#' sick
#'
#' Filter out the sketchy rows! (filter equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Logical predicates to keep rows
#' @return A data frame with filtered rows removed
#' @export
#' @examples
#' mtcars |> thats_sick(mpg > 20, cyl == 4)
thats_sick <- function(.data, ...) {
  filter(.data, (...))
}

#' mood
#'
#' What columns are you in the mood for? (select equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Columns to keep
#' @return A data frame with selected columns
#' @export
#' @examples
#' mtcars |> mood(mpg, cyl, hp)
mood <- function(.data, ...) {
  select(.data, ...)
}

#' level_up
#'
#' Level up your columns. (mutate equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Name-value pairs of expressions
#' @return A data frame with new/modified columns
#' @export
#' @examples
#' mtcars |> level_up(mpg_squared = mpg^2, high_hp = hp > 150)
level_up <- function(.data, ...) {
  mutate(.data, ...)
}

#' tldr
#'
#' There's too much data, just get the summary stats! (summarize equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Name-value pairs of summary expressions
#' @return A data frame with summary statistics
#' @export
#' @examples
#' mtcars |> tldr(avg_mpg = mean(mpg), max_hp = max(hp))
tldr <- function(.data, ...) {
  summarise(.data, ...)
}

#' GOAT
#'
#' See who comes out on top! (arrange equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Variables to sort by
#' @return A sorted data frame
#' @export
#' @examples
#' mtcars |> GOAT(desc(mpg), hp)
GOAT <- function(.data, ...) {
  arrange(.data, ...)
}

#' FOMO
#'
#' Make sure you don't miss out! (group_by equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Variables to group by
#' @return A grouped data frame
#' @export
#' @examples
#' mtcars |> FOMO(cyl) |> tldr(avg_mpg = mean(mpg))
FOMO <- function(.data, ...) {
  group_by(.data, ...)
}

#' shook
#'
#' Shock everyone! (ungroup equivalent)
#'
#' @param .data A grouped data frame
#' @return An ungrouped data frame
#' @export
#' @examples
#' mtcars |> FOMO(cyl) |> shook()
shook <- function(.data) {
  ungroup(.data)
}

#' literally
#'
#' Do you mean literally or literally? (rename equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Name-value pairs (new_name = old_name)
#' @return A data frame with renamed columns
#' @export
#' @examples
#' mtcars |> literally(miles_per_gallon = mpg, horse_power = hp)
literally <- function(.data, ...) {
  rename(.data, ...)
}

#' YOLO
#'
#' You only live once (distinct equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Optional columns to determine uniqueness
#' @return A data frame with unique rows
#' @export
#' @examples
#' mtcars |> YOLO(cyl)
YOLO <- function(.data, ...) {
  distinct(.data, ...)
}

#' THIS
#'
#' Exactly this (pull equivalent)
#'
#' @param .data A data frame or tibble
#' @param var Column to extract
#' @return A vector
#' @export
#' @examples
#' mtcars |> bootstraps(mpg)
THIS <- function(.data, var) {
  pull(.data, {{ var }})
}

#' sup
#'
#' What's up? (head/slice_head equivalent)
#'
#' @param .data A data frame or tibble
#' @param n Number of rows to keep
#' @return A data frame with first n rows
#' @export
#' @examples
#' mtcars |> whats_up(5)
sup <- function(.data, n = 6) {
  slice_head(.data, n = n)
}

#' all_that
#'
#' See if your variable is all that or not (count equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Variables to count by
#' @return A data frame with counts
#' @export
#' @examples
#' mtcars |> all_that(cyl, gear)
all_that <- function(.data, ...) {
  count(.data, ...)
}


#' for_real
#'
#' Is your data for real? (left join equivalent)
#'
#' @param x A data frame or tibble (primary residence)
#' @param y A data frame or tibble (vacation home)
#' @param by Columns to join by
#' @return A merged data frame
#' @examples
#' df1 <- data.frame(id = 1:3, name = c("Alice", "Bob", "Charlie"))
#' df2 <- data.frame(id = c(2, 3, 4), score = c(90, 85, 88))
#' df1 |> for_real(df2, by = "id")
#' @export
for_real <- function(x, y, ...) {
  left_join(x, y, ...)
}

#' adulting
#'
#' A redundant function to make your code sound harder than it actually is (right_join equivalent)
#'
#' @param x A data frame or tibble (main table)
#' @param y A data frame or tibble (rubber-stamping table)
#' @param by Columns to join by
#' @return A merged data frame
#' @export
adulting <- function(x, y, ...) {
  right_join(x, y, ...)
}

#' tight
#'
#' Are your dataframes tight with each other? (inner join equivalent)
#'
#' @param x A data frame or tibble
#' @param y A data frame or tibble
#' @param by Columns to join by
#' @return A merged data frame
#' @export
#'
tight <- function(x, y, ...) {
  inner_join(x, y, ...)
}

#' thirsty
#'
#' A little desperate if you ask me (full_join equivalent)
#'
#' @param x A data frame or tibble
#' @param y A data frame or tibble
#' @return A merged data frame
#' @export
thirsty <- function(x, y, ...) {
  full_join(x, y, ...)
}

#' sike
#'
#' jk (anti_join equivalent)
#'
#' @param x A data frame or tibble
#' @param y A data frame or tibble
#' @return A data frame with non-matching rows
#' @export
sike <- function(x, y, ...) {
  anti_join(x, y, ...)
}

#' cringe
#'
#' Only cool kids allowed (semi_join equivalent)
#'
#' @param x A data frame or tibble
#' @param y A data frame or tibble
#' @return A data frame with matching rows
#' @export
cringe <- function(x, y, ...) {
  semi_join(x, y, ...)
}


#' quiet_quit
#'
#' Let the other columns do the work (coalesce equivalent)
#'
#' @param ... One or more vectors.
#' @param .ptype  An optional prototype declaring the desired output type.
#' @param .size An optional size declaring the desired output size.
#' @return A data frame with matching rows
#' @export
#' @examples
#' y <- c(1, 2, NA, NA, 5); z <- c(NA, NA, 3, 4, 5); quiet_quit(y, z)
quiet_quit <- function(..., .ptype = NULL, .size = NULL) {
  coalesce(..., .ptype = NULL, .size = NULL)
}

#' humblebrag
#'
#' More info than we needed to be honest (explain equivalent)
#'
#' @param x An object to explain
#' @param ... Other parameters possibly used by generic
#' @return The first argument, invisibly
#' @export
humblebrag <- function(x, ...) {
  explain(x, ...)
}

#' epic_fail
#'
#' Well that didn't work (na_if equivalent)
#'
#' @param x Vector to modify
#' @param y Value or vector to compare against. When x and y are equal, the value in x will be replaced with NA.
#' @return A modified version of x that replaces any values that are equal to y with NA.
#' @export
epic_fail <- function(x, y) {
  na_if(x, y)
}
