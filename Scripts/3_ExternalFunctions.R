#' @title 3_ExternalFunctions.R
#' @description Adding functions to an external R file
#' @author NMCG
#' @bugs None
#' @keywords functions, Roxygen, comments
 

#' @title LinearContrastStretch
#' @description Linearly stretches a value from an original range (min_val to max_val) to a new range (new_min to new_max).
#' @param value Numeric. The value to stretch.
#' @param min_val Numeric. Minimum of the original range.
#' @param max_val Numeric. Maximum of the original range.
#' @param new_min Numeric. Minimum of the new range.
#' @param new_max Numeric. Maximum of the new range.
#' @return Numeric. The value stretched into the new range.
#' @examples
#' LinearContrastStretch(5, 0, 10, 20, 30) # returns 25
#' @export
LinearContrastStretch <- function(value, min_val, max_val, new_min, new_max) {
  stretched_value <- ((value - min_val) / (max_val - min_val) * (new_max - new_min)) + new_min
  return(stretched_value)
}

#' @title LogAbs
#' @description Calculates the logarithm of the absolute value of x to the specified base.
#' @param x Numeric. Input value.
#' @param base Numeric. The logarithmic base.
#' @return Numeric. The logarithm of abs(x) to the given base.
#' @examples
#' LogAbs(-16, 2) # returns 4
#' @export
LogAbs <- function(x, base) {
  return(log(abs(x), base))
}

#' @title Add
#' @description Adds two numbers and returns the result.
#' @param a Numeric. First number.
#' @param b Numeric. Second number.
#' @return Numeric. The sum of a and b.
#' @examples
#' Add(3, 5) # returns 8
#' @export
Add <- function(a, b) {
  return(a + b)
}

#' @title SetwdToScript
#' @description Sets the working directory to the location of the running R script. Compatible with RStudio (Source) and Rscript (command line).
#' @return None. Sets the working directory via setwd().
#' @examples
#' SetwdToScript()
#' @export
SetwdToScript <- function() {
  this_file <- tryCatch(parent.frame(2)$ofile, error = function(e) NULL)
  
  if (is.null(this_file)) {
    args <- commandArgs(trailingOnly = FALSE)
    this_file <- sub("--file=", "", args[grep("--file=", args)])
  }
  
  if (length(this_file) > 0 && this_file != "") {
    setwd(dirname(this_file))
    cat("Working directory set to:", getwd(), "\n")
  } else {
    cat("Could not determine script location automatically.\n")
  }
}







