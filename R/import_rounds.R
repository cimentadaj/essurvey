#' Download integrated rounds from the European Social Survey
#'
#' @param rounds a numeric vector with the rounds to download. See \code{\link{show_rounds}}
#' for all available rounds.
#' @param ess_email a character vector with your email, such as "your_email@email.com".
#' If you haven't registered in the ESS website, create an account at
#' \url{http://www.europeansocialsurvey.org/user/new}. A preferred method is to login
#' through \code{\link{set_email}}.
#'
#' @param output_dir a character vector with the output directory in case you want to only download the files using
#' the \code{download_rounds}. Defaults to your working directory. This will be interpreted as
#' a \strong{directory} and not a path with a file name.
#'
#' @param format the format from which to download the data. By default it is NULL for \code{import_*} functions and tries to read 'stata', 'spss' and 'sas' in the specific order. This can be useful if some countries don't have a particular format available.  Alternatively, the user can specify the format which can either be 'stata', 'spss' or 'sas'. For the \code{download_*} functions it is set to 'stata' because the format should be specified before downloading. When using \code{import_country} the data will be downloaded and read in the \code{format} specified. For \code{download_country}, the data is downloaded from the specified \code{format} (only 'spss' and 'stata' supported, see details).
#'
#' @details
#' Use \code{import_rounds} to download specified rounds and import them to R.
#' \code{import_all_rounds} will download all rounds by default and \code{download_rounds}
#' will download rounds and save them in a specified \code{format} in the supplied
#' directory.
#'
#' The \code{format} argument from \code{import_rounds} should not matter to the user
#' because the data is read into R either way. However, different formats might have
#' different handling of the encoding of some questions. This option was preserved
#' so that the user
#' can switch between formats if any encoding errors are found in the data. For more
#' details see the discussion \href{https://github.com/ropensci/essurvey/issues/11}{here}.
#' For this particular argument in, 'sas' is not supported because the data formats have
#' changed between ESS waves and separate formats require different functions to be
#' read. To preserve parsimony and format errors between waves, the user should use
#' 'spss' or 'stata'.
#'
#' @return for \code{import_rounds} if \code{length(rounds)} is 1, it returns a tibble
#' with the latest version of that round. Otherwise it returns a list of \code{length(rounds)}
#' containing the latest version of each round. For \code{download_rounds}, if
#' \code{output_dir} is a valid directory, it returns the saved directories invisibly
#' and saves all the rounds in the chosen \code{format} in \code{output_dir}
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' set_email("your_email@email.com")
#'
#' # Get first three rounds
#' three_rounds <- import_rounds(1:3)
#'
#' temp_dir <- tempdir()
#'
#' # Only download the files to output_dir, this will return nothing.
#' download_rounds(
#'  rounds = 1:3,
#'  output_dir = temp_dir,
#' )
#'
#' # By default, download_rounds saves a 'stata' file. You can
#' # also download 'spss' and 'sas' files.
#'
#' download_rounds(
#'  rounds = 1:3,
#'  output_dir = temp_dir,
#'  format = 'spss'
#' )
#'
#' # If rounds are repeated, will download only unique ones
#' two_rounds <- import_rounds(c(1, 1))
#'
#' # If email is not registered at ESS website, error will arise
#' two_rounds <- import_rounds(c(1, 2), "wrong_email@email.com")
#'
#' # Error in authenticate(ess_email) :
#' # The email address you provided is not associated with any registered user.
#' # Create an account at https://www.europeansocialsurvey.org/user/new
#'
#' # If selected rounds don't exist, error will arise
#'
#' two_rounds <- import_rounds(c(1, 22))
#' # Error in round_url(rounds) :
#' # ESS round 22 is not a available. Check show_rounds()
#' }
#'
import_rounds <- function(rounds, ess_email = NULL, format = NULL) {

  stopifnot(is.numeric(rounds), length(rounds) > 0)
  if (!is.null(format) && format == "sas") {
    stop(
      "You cannot read SAS but only 'spss' and 'stata' files with this function. See ?import_rounds for more details") # nolint
  }

  urls <- round_url(rounds, format = format)

  dir_download <- download_format(ess_email = ess_email,
                                  urls = urls)

  all_data <- read_format_data(dir_download)
  # Remove everything that was downloaded
  unlink(dir_download, recursive = TRUE, force = TRUE)

  # If only one dataframe, return that
  all_data <- if (length(all_data) == 1) all_data[[1]] else all_data

  all_data
}

#' @rdname import_rounds
#' @export
import_all_rounds <- function(ess_email = NULL, format = NULL) {
  import_rounds(show_rounds(), ess_email, format)
}

#' @rdname import_rounds
#' @export
download_rounds <- function(rounds,
                            ess_email = NULL,
                            output_dir = getwd(),
                            format = 'stata') {

  stopifnot(is.numeric(rounds), length(rounds) > 0)
  urls <- round_url(rounds, format = format)

  invisible(download_format(urls = urls,
                            ess_email= ess_email,
                            only_download = TRUE,
                            output_dir = output_dir))
}
