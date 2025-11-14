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
  filter(.data, !(...))
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

#' glow_up
#'
#' Give your columns a glow up! (mutate equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Name-value pairs of expressions
#' @return A data frame with new/modified columns
#' @export
#' @examples
#' mtcars |> glow_up(mpg_squared = mpg^2, high_hp = hp > 150)
glow_up <- function(.data, ...) {
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


#' epic_fail
#'
#' Couldn't have been worse (failwith equivalent)
#'
#' @param .data A data frame or tibble
#' @param ... Name-value pairs (new_name = old_name)
#' @return A data frame with renamed columns
#' @export
epic_fail <- function(default = NULL, f, quiet = FALSE) {
  failwith(default = NULL, f, quiet = FALSE)
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

#' humblebrag
#'
#' Ugh, your problem is so complex that you need everyone to know about it (create_progress_bar equivalent)
#'
#' @param .data A grouped data frame
#' @return An ungrouped data frame
#' @export
humblebrag <- function(name = "none", ...) {
  create_progress_bar(name = "none", ...)
}

