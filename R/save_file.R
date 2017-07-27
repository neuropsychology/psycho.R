#' Save the results to a file.
#'
#' @param x A psycho class object.
#' @param file The Filename.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @import rtf
#' @export
save_file <- function(x, file="results"){
  # Initialize
  values <- x$values
  filename <- paste(file, ".rtf", sep="")
  rtf <- rtf::RTF(filename, width=8.5, height=11, font.size=12, omi=c(1,1,1,1))

  # Summary
  summary <- x$summary
  colnames(summary) <- gsub("\\."," ", colnames(summary)) # format column names
  rtf::addTable(rtf, summary, font.size=9, row.names=FALSE, NA.string="-")

  # Plot
  rtf::addHeader(rtf, title=values$psycho_function, subtitle=values$psycho_name)
  print(x$plot)
  rtf::addPlot(rtf, plot.fun=print, width=6, height=6, res=300, x$plot)

  # SessionInfo
  rtf::addSessionInfo(rtf)
  rtf::done(rtf)
}
