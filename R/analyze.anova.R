#' Analyze anova objects.
#'
#' Analyze anova objects.
#'
#' @param x anova object.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_omega_sq]{interpret_omega_sq}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' df <- psycho::affective
#'
#' x <- anova(lm(df$Tolerating ~ df$Salary * df$Sex))
#' x <- anova(lmerTest::lmer(Tolerating ~ Salary + (1|Sex), data=df))
#'
#' summary(analyze(x))
#' print(analyze(x))
#' }
#'
#'
#' @references
#' \itemize{
#'  \item{Levine, T. R., & Hullett, C. R. (2002). Eta squared, partial eta squared, and misreporting of effect size in communication research. Human Communication Research, 28(4), 612-625.}
#'  \item{JPierce, C. A., Block, R. A., & Aguinis, H. (2004). Cautionary note on reporting eta-squared values from multifactor ANOVA designs. Educational and psychological measurement, 64(6), 916-924.}
#'}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
analyze.anova <- function(x, effsize_rules="field2013", ...){
  stop("Not available yet.")
}
# analyze.anova <- analyze.aov

