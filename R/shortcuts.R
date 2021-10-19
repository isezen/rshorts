unlockBinding("q", baseenv())
#' Shortcut to quit function
#' @param save Default is \code{"no"}
#' @param ... Other arguments passed to \link{quit} function
#' @seealso \link{quit}
#' @export
q <- function(save = "no", ...) quit(save = save, ...)
lockBinding("q", baseenv())

#' Shortcut to quit function
#' @param save Default is \code{"yes"}
#' @param ... Other arguments passed to \code{quit} function.
#' @seealso \link{quit}
#'@export
exit <- function(save = "yes", ...) quit(save = save, ..., runLast = FALSE)

#' Shortcut to length function
#' @seealso \link{length}
#' @export
len <- base::length

#' Shortcut to summary function
#' @param object an object for which a summarhy is desired.
#' @param ... additional arguments affecting the summary produced.
#' @seealso \link{summary}
#' @export
s   <- base::summary

#' Shortcut to head function
#' @param x an object
#' @param ... arguments to be passed to or from other methods
#' @seealso \link{head}
#' @export
h   <- utils::head

#' Shortcut to names function
#' @seealso \link{names}
#' @export
n   <- base::names

#' Shortcut to setwd function
#' @param dir Directory name to change.
#' @seealso \link{setwd}
#' @export
cd  <- function(dir) base::setwd(dir)

#' Shortcut to getwd function
#' @seealso \link{getwd}
#' @export
pwd <- function() base::getwd()

#' Clear environment
#'
#' Clear global environment variables
#' @export
cle <- function() rm(list = ls(envir = globalenv()))

#' List only variables
#'
#' List ONLY variables in global environment
#' @param name name of environment
#' @export
lsv <- function(name = parent.frame()) {
  obj <- ls(name = name)
  obj[!sapply(obj, function(x) is.function(get(x)))]
}

#' Clear Screen
#' @export
cls <- function() {
  if (.Platform$GUI[1] == "Rgui") {
    # if (!require("rcom", quietly = TRUE))
    if (!("rcom" %in% rownames(utils::installed.packages())))
      stop("Package rcom is required for 'cls()'")
    wsh <- rcom::comCreateObject("Wscript.Shell")
    if (is.null(wsh)) {
      return(invisible(FALSE))
    } else {
      rcom::comInvoke(wsh, "SendKeys", "\014")
      return(invisible(TRUE))
    }
  }else if (.Platform$GUI[1] == "RStudio") {
    cat("\014")
    return(invisible(TRUE))
  } else {
    cat("\014")
    term <- Sys.getenv("TERM")
    if (term == "") {
      cat("\014")
      return(invisible(TRUE))
    }
    else if (term == "xterm-256color") {
      system("clear")
      return(invisible(TRUE))
    } else {
      return(invisible(FALSE))
    }
  }
}

#' Save History
#' @export
sh <- function() {
  if (!is.na(Sys.getenv("RSTUDIO", unset = NA))) return(invisible(NULL))
  if (interactive()) {
    hist_file <- Sys.getenv("R_HISTFILE")
    if (hist_file == "") {
      hist_file <- ".Rhistory"
      if (!file.exists(hist_file)) hist_file <- paste0("~/", hist_file)
    }
    if (!nzchar(Sys.getenv("RADIAN_VERSION"))) utils::savehistory(hist_file)
    message("History saved: ", hist_file)
  }
}

#' Load History
#' @export
lh <- function() {
  if (is.na(Sys.getenv("RSTUDIO", unset = NA))) {
    hist_file <- Sys.getenv("R_HISTFILE")
    if (hist_file == "") {
      hist_file <- ".Rhistory"
      if (!file.exists(hist_file)) hist_file <- paste0("~/", hist_file)
    }
    if (!nzchar(Sys.getenv("RADIAN_VERSION"))) {
      utils::loadhistory(hist_file)
      message("History loaded: ", hist_file)
    }
  }
}

#' Restart R
#' @export
restart <- function() {
  if (interactive()) {
    if (.Platform$GUI[1] == "Rgui") {
      shell("Rgui"); q("no")
    } else if (.Platform$GUI[1] == "RStudio") {
      sh()
      # .rs.restartR()
      rstudioapi::restartSession()
    } else {
      if (!nzchar(Sys.getenv("RADIAN_VERSION"))) {
        system("R"); q("no")
      }
    }
  }
  invisible(NULL)
}

#' Shortcut to microbenchmark function
#' @param ... Arguments passed to \code{microbenchmark} function.
#' @seealso \link[microbenchmark]{microbenchmark}
#' @export
mb <- function(...) {
  if (!("microbenchmark" %in% rownames(utils::installed.packages()))) {
    print("Install microbenchmark package")
  }
  microbenchmark::microbenchmark(...)
}

#' cat plot on terminal
#' This function requires iterm terminal and shell integration features.
#' @param x plot object to be printed to terminal.
#' @param ... Arguments passed to \link{png} function.
#' @export
icat <- function(x, ...) {
  if (is.na(Sys.getenv("RSTUDIO", unset = NA))) {
    plt <- .Platform$GUI[1]
    if (interactive() & plt == "X11") {
      path <- tempfile(fileext = ".png")
      grDevices::png(path, ...)
      x
      grDevices::dev.off()
      system(paste("~/.iterm2/imgcat", path))
      file.remove(path)
    } else message("icat disabled in ", plt)
  }
}
