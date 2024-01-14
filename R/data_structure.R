# Function force_structure ------------------------------------------------

#' Call corresponding function of a `syscalls` pairlist
#'
#' @description
#' The function receives a `syscalls` pairlist from the previous function call,
#' i.e. test_correl with a vector how the x and/or y variables shall be treated.
#' The original call is replaced with the appropriate `apply_structure_`-func-
#' tion. Structure information "numeric" or "factor" is appended to the call.
#'
#' @param syscalls Function call from executing function as pairlist.
#' @param strctr Character vector, how to format data `c("numeric", "factor")`.
#'
#' @importFrom utils tail
#'
#' @return Returns an evaluated function call.
#' @export
force_structure <- function(syscalls, strctr) {
  syscalls <- tail(syscalls, n = 1) # Use last element

  force <- gsub(
    syscalls[[1]][[1]],
    ifelse(length(strctr) == 1, "apply_structure_x", "apply_structure_xy"),
    syscalls
  )
  force <- substr(force, 1, nchar(force) - 1)
  force <- paste0(force, ", strctr = strctr)")

  return(eval(parse(text = force)))
}


# Function apply_structure_xy ---------------------------------------------

#' Applies expected data structure for x/y variable and provides params list
#'
#' @param x The x-variable from the original call, passed through `syscalls`.
#' @param y The y-variable from the original call, passed through `syscalls`.
#' @param data An optional data frame including `x` and `y` columns.
#' @param strctr Character vector, how to format data `c("numeric", "factor")`.
#' @param ... Other arguments to the function (i.e., paired, alternative, etc.)
#'
#' @return Returns a params list
#' @export
apply_structure_xy <- function(x, y, data = FALSE, ..., strctr = FALSE) {
  if (!is.data.frame(data)) {
    # v_name
    x_name <- deparse(substitute(x))
    y_name <- deparse(substitute(y))

    # v_var_name
    x_var_name <- build_var_name(x_name)
    y_var_name <- build_var_name(y_name)

    # v
    x <- build_var(x, strctr[1])
    y <- build_var(y, strctr[2])
  } else if (is.data.frame(data)) {
    # v_name
    x_name <- paste0(deparse(substitute(data)), "$", x)
    y_name <- paste0(deparse(substitute(data)), "$", y)

    # v_var_name
    x_var_name <- x
    y_var_name <- y

    # v
    x <- build_var(x, strctr[1], data)
    y <- build_var(y, strctr[2], data)
  } else {
    warning("\n\tdata does not correspond to expected")
  }

  # Modify v if required
  x <- modify_factors(x, strctr[1])
  y <- modify_factors(y, strctr[2])

  # Build params list
  params_list <- list()
  params_list$x_name <- x_name
  params_list$x_var_name <- x_var_name
  params_list$x <- x
  params_list$y_name <- y_name
  params_list$y_var_name <- y_var_name
  params_list$y <- y

  return(params_list)
}


# Function apply_structure_x ----------------------------------------------

#' Applies expected data structure for x variable and provides params list
#'
#' @param x The x-variable from the original call, passed through `syscalls`.
#' @param data An optional data frame including `x` column.
#' @param strctr Character vector, how to format data `c("numeric", "factor")`.
#' @param ... Other arguments to the function (i.e., paired, alternative, etc.)
#'
#' @return Returns a params list
#' @export
apply_structure_x <- function(x, data = FALSE, ..., strctr = FALSE) {
  if (!is.data.frame(data)) {
    # v_name
    x_name <- deparse(substitute(x))

    # v_var_name
    x_var_name <- build_var_name(x_name)

    # v
    x <- build_var(x, strctr[1])
  } else if (is.data.frame(data)) {
    # v_name
    x_name <- paste0(deparse(substitute(data)), "$", x)

    # v_var_name
    x_var_name <- x

    # v
    x <- build_var(x, strctr[1], data)
  } else {
    warning("\n\tdata does not correspond to expected")
  }

  # Modify v if required
  x <- modify_factors(x, strctr[1])

  # Build params list
  params_list <- list()
  params_list$x_name <- x_name
  params_list$x_var_name <- x_var_name
  params_list$x <- x

  return(params_list)
}


# Helper functions --------------------------------------------------------

#' Builds the var_name for data without data frame
#'
#' @param var_name The `v_name` from `apply_structure_`-functions.
#'
#' @return Returns a string with the formatted `v_var_name`.
#' @export
#'
#' @examples
#' build_var_name("ToothGrowth$len")
#' build_var_name("ToothGrowth[len]")
#' build_var_name("ToothGrowth[[len]]")
build_var_name <- function(var_name) {
  if (grepl("[$]", var_name) == TRUE) {
    result <- gsub(".*[$]", "", var_name)
  } else {
    result <- gsub("\"", "", gsub(".*\\[([^]]+)\\].*", "\\1", var_name))
  }
  return(result)
}


#' Builds the var for data with and without data frame
#'
#' @param var The `v` from `apply_structure_`-functions.
#' @param strctr String of required structure.
#' @param data An optional data frame including the `v` column.
#'
#' @return Returns structure-formatted data.
#' @export
#'
#' @examples
#' build_var(ToothGrowth$len, "numeric")
#' build_var(ToothGrowth["len"], "numeric")
#' build_var(ToothGrowth[["len"]], "numeric")
#' build_var("len", "numeric", ToothGrowth)
build_var <- function(var, strctr, data = FALSE) {
  if (missing(data)) {
    # For data without data frame
    if (strctr == "factor") {
      if (is.data.frame(var)) {
        result <- as.factor(unlist(var[, 1]))
      } else {
        result <- as.factor(unlist(var))
      }
    } else if (strctr == "numeric") {
      if (is.data.frame(var)) {
        result <- as.numeric(unlist(var[, 1]))
      } else {
        result <- as.numeric(unlist(var))
      }
    }
  } else {
    # For data with data frame
    if (strctr == "factor") {
      result <- as.factor(data[[var]])
    } else if (strctr == "numeric") {
      result <- as.numeric(data[[var]])
    }
  }

  return(result)
}

#' Modifies factors which start with numbers or contain special characters
#'
#' @param var The `v` from `apply_structure_`-functions.
#' @param strctr String of required structure.
#'
#' @return Return structure-formatted and modified data.
#' @export
#'
#' @examples
#' modify_factors(esoph$agegp, "factor")
#' modify_factors(ToothGrowth$supp, "factor")
modify_factors <- function(var, strctr) {
  if (strctr == "factor") {
    var <- gsub("[^A-Za-z0-9 ]", ".", var)
    if (TRUE %in% grepl("[0-9]", (substring(unique(var), 1, 1)))) {
      var <- paste0("G.", var)
    }
    result <- as.factor(var)
  } else {
    result <- var
  }

  return(result)
}
