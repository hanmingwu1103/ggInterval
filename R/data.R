#' @name facedata
#' @aliases facedata
#' @title Face Data Example
#' @description
#' Symbolic data matrix with all the variables of interval type.
#' The 27 rows are nine subjects (FRA, HUS, INC, ISA, JPL, KHA, LOT, PHI,
#' ROM) with three image sequences per subject; AD, BC, AH, DH, EH and GH are six
#' positive-width interval variables.
#' @usage data('facedata')
#' @format
#' $I;AD;AD;$I;BC;BC;......... \cr
#'
#' HUS1;$I;168.86;172.84;$I;58.55;63.39;.........\cr
#' HUS2;$I;169.85;175.03;$I;60.21;64.38;.........\cr
#' HUS3;$I;168.76;175.15;$I;61.4;63.51;.........\cr
#' INC1;$I;155.26;160.45;$I;53.15;60.21;.........\cr
#' INC2;$I;156.26;161.31;$I;51.09;60.07;.........\cr
#' INC3;$I;154.47;160.31;$I;55.08;59.03;.........\cr
#' ISA1;$I;164;168;$I;55.01;60.03;.........\cr
#' ISA2;$I;163;170;$I;54.04;59;.........\cr
#' ISA3;$I;164.01;169.01;$I;55;59.01;.........\cr
#' JPL1;$I;167.11;171.19;$I;61.03;65.01;.........\cr
#' JPL2;$I;169.14;173.18;$I;60.07;65.07;.........\cr
#' JPL3;$I;169.03;170.11;$I;59.01;65.01;.........\cr
#' KHA1;$I;149.34;155.54;$I;54.15;59.14;.........\cr
#' KHA2;$I;149.34;155.32;$I;52.04;58.22;.........\cr
#' KHA3;$I;150.33;157.26;$I;52.09;60.21;.........\cr
#' LOT1;$I;152.64;157.62;$I;51.35;56.22;.........\cr
#' LOT2;$I;154.64;157.62;$I;52.24;56.32;.........\cr
#' LOT3;$I;154.83;157.81;$I;50.36;55.23;.........\cr
#' PHI1;$I;163.08;167.07;$I;66.03;68.07;.........\cr
#' PHI2;$I;164;168.03;$I;65.03;68.12;.........\cr
#' PHI3;$I;161.01;167;$I;64.07;69.01;.........\cr
#' ROM1;$I;167.15;171.24;$I;64.07;68.07;.........\cr
#' ROM2;$I;168.15;172.14;$I;63.13;68.07;.........\cr
#' ROM3;$I;167.11;171.19;$I;63.13;68.03;.........\cr
#' @references
#' Arce Garro, Jorge and Rodr\enc{í}{i}guez Rojas, Oldemar (2019). Optimized
#' dimensionality reduction methods for interval-valued variables and their
#' application to facial recognition. \emph{Entropy}, 21(10), 1016, pp. 9--10.
#' \doi{10.3390/e21101016}
#'
#' Billard, Lynne and Diday, Edwin (2006).
#' \emph{Symbolic Data Analysis: Conceptual Statistics and Data Mining}.
#' Chichester, UK: John Wiley and Sons.
#' @examples
#' \donttest{
#' data(facedata)
#' ggInterval_hist(facedata, aes(x = AD))
#' }
#' @keywords datasets
#' @source Adapted from \code{RSDA::facedata}; see
#'   \url{https://CRAN.R-project.org/package=RSDA}.
"facedata"


#' @name Cardiological
#' @title Cardiological data example
#' @description Cardiological interval data example.
#' @usage data(Cardiological)
#' @references Billard, Lynne and Diday, Edwin (2006).
#' \emph{Symbolic Data Analysis: Conceptual Statistics and Data Mining}.
#' Chichester, UK: John Wiley and Sons.
#' @examples
#' data(Cardiological)
#' ggInterval_indexplot(Cardiological, aes(x = Syst))
#' @keywords datasets
#' @source Adapted from \code{RSDA::Cardiological}; see
#'   \url{https://CRAN.R-project.org/package=RSDA}.
"Cardiological"


#' @name abalone.i
#' @title abalone.i data example
#' @description abalone.i interval data example.
#' @usage data(abalone.i)
#' @examples
#' data(abalone.i)
#' ggInterval_indexplot(abalone.i, aes(x = Length))
#' @keywords datasets
#' @source Adapted from \code{MAINT.Data::AbaloneIdt}; the underlying
#'   Abalone data are from the UCI Machine Learning Repository.
"abalone.i"

#' @name blood.i
#' @title blood.i data example
#' @description blood.i interval data example.
#' @usage data(blood.i)
#' @references Billard, Lynne and Diday, Edwin (2006).
#' \emph{Symbolic Data Analysis: Conceptual Statistics and Data Mining}.
#' Chichester, UK: John Wiley and Sons.
#' @examples
#' data(blood.i)
#' ggInterval_MMplot(blood.i, aes(x = Hematocrit))
#' @keywords datasets
#' @source Adapted from \code{HistDAWass::BLOOD}; see
#'   \url{https://CRAN.R-project.org/package=HistDAWass}.
"blood.i"

#' @name oils
#' @title oils data example
#' @description oils interval data example.
#' @usage data(oils)
#' @references Cazes, Pierre; Chouakria, Assia; Diday, Edwin; and
#' Schektman, Youri (1997). Extension de l'analyse en composantes principales
#' a des donnees de type intervalle. \emph{Revue de Statistique Appliquee},
#' 45(3), 5--24.
#' @examples
#' data(oils)
#' \donttest{
#' ggInterval_scatterplot(oils, aes(x = GRA, y = IOD))
#' }
#' @keywords datasets
#' @source Adapted from \code{RSDA::oils}; see
#'   \url{https://CRAN.R-project.org/package=RSDA}.
"oils"

#' @name mushroom
#' @title mushroom data example
#' @description mushroom interval data example.
#' @usage data(mushroom)
#' @references Billard, Lynne and Diday, Edwin (2006).
#' \emph{Symbolic Data Analysis: Conceptual Statistics and Data Mining}.
#' Chichester, UK: John Wiley and Sons.
#' @examples
#' data(mushroom)
#' \donttest{
#' ggInterval_scatterplot(mushroom, aes(x = Cap.Widths, y = Stipe.Lengths))
#' }
#' @keywords datasets
#' @source Adapted from \code{RSDA::mushroom}; see
#'   \url{https://CRAN.R-project.org/package=RSDA}.
"mushroom"

#' @name Environment
#' @title Environment data example
#' @description Environmental-questionnaire data with 14 symbolic objects and
#' 17 variables: 13 interval variables (CONTROL through VEHICLE) and four modal
#' variables (URBANICITY, INCOMELEVEL, EDUCATION, REGIONDEVELOPME).
#' @details Stored values match the SODAS example distributed with Umbleja,
#' Ichino and Yaguchi (2020). Source object identifiers 1241, 1242, 1441, 1442,
#' 1641, 1642, 1741, 2241, 2242, 2441, 2442, 2641, 2642, 2741 correspond in
#' order to rows 1--14. Modal categories are numeric codes without a codebook.
#' Since version 0.2.6 the INCOMELEVEL category labels follow the order of the
#' SODAS source files, 25, 75, 50, 90, 100. Earlier versions listed the labels
#' as 25, 50, 75, 90, 100 while storing the proportions in the source order, so
#' the labels 50 and 75 were interchanged; the proportions are unchanged.
#' @references Umbleja, K., Ichino, M. and Yaguchi, H. (2020). Improving
#' symbolic data visualization for pattern recognition and knowledge
#' discovery. \emph{Visual Informatics}, 4(1), 23--31.
#' \doi{10.1016/j.visinf.2019.12.003}.
#' @usage data(Environment)
#' @examples
#' data(Environment)
#' ggInterval_radarplot(Environment,
#'  plotPartial = 2,
#'  showLegend = FALSE,
#'  base_circle = TRUE,
#'  base_lty = 2,
#'  addText = FALSE)
#' @keywords datasets
"Environment"

#' @name iris.i
#' @title iris.i data example
#' @description iris.i interval data example.
#' @usage data(iris.i)
#' @examples
#' data(iris.i)
#' ggInterval_indexplot(iris.i, aes(x = Sepal.Length))
#' @keywords datasets
"iris.i"

#' @name mtcars.i
#' @title mtcars.i data example
#' @description mtcars.i interval and modal data example.
#' @usage data(mtcars.i)
#' @examples
#' data(mtcars.i)
#' ggInterval_indexplot(mtcars.i, aes(x = mpg))
#' @keywords datasets
"mtcars.i"


#' @name Cardiological2
#' @title Cardiological data example
#' @description Cardiological interval data example.
#' @usage data(Cardiological2)
#' @references Billard, Lynne and Diday, Edwin (2006).
#' \emph{Symbolic Data Analysis: Conceptual Statistics and Data Mining}.
#' Chichester, UK: John Wiley and Sons.
#' @examples
#' data(Cardiological2)
#' ggInterval_indexplot(Cardiological2, aes(x = Syst))
#' @keywords datasets
#' @source Adapted from \code{RSDA::Cardiological2}; see
#'   \url{https://CRAN.R-project.org/package=RSDA}.
"Cardiological2"

#' @name stock.data.LH
#' @title Synthetic financial low-high intervals
#' @description A 62-observation subset of the randomly generated
#' \code{TTR::ttrc} series, with dates in January--March 1985. The data do not
#' represent an actual security. Each row retains a low-high interval and
#' the auxiliary columns used in the interval-valued line-plot example.
#' @format A \code{symbolic_tbl} with 62 rows and seven columns:
#' \describe{
#'   \item{V1}{A \code{symbolic_interval} column with lower endpoint \code{Low}
#'     and upper endpoint \code{High}.}
#'   \item{Date}{Date labels, from 1985-01-02 to 1985-03-29.}
#'   \item{Close}{Synthetic closing prices.}
#'   \item{Low}{Synthetic daily low prices, retained for reconstruction.}
#'   \item{High}{Synthetic daily high prices, retained for reconstruction.}
#'   \item{Month}{A factor with levels \code{Jan}, \code{Feb}, and \code{Mar}.}
#'   \item{Day}{Integer day of month, used as the horizontal coordinate.}
#' }
#' @details There are 22, 19, and 21 observations in January, February,
#' and March, respectively. The interval centers are the averages of
#' \code{Low} and \code{High}; they need not equal \code{Close}.
#' The interval column is constructed with \code{classic2sym()}; the source
#' columns are then attached with \code{dplyr::bind_cols()}. The result is
#' a static symbolic table, so \code{head(stock.data.LH)} displays the
#' intervals and their accompanying columns together.
#' TTR is needed to reconstruct the data, but not to load this stored subset.
#' @source Adapted from \code{TTR::ttrc} in TTR version 0.24.4. TTR documents
#' the source as randomly generated data for offline examples, not prices
#' of an actual security. See \url{https://CRAN.R-project.org/package=TTR}.
#' @usage data("stock.data.LH")
#' @examples
#' data("stock.data.LH")
#' head(stock.data.LH)
#' ggInterval_lineplot(stock.data.LH, aes(y = V1, x = Day), barWidth = 0.6) +
#'   geom_point(aes(y = Close), colour = "#D95F02") +
#'   facet_wrap(~Month, ncol = 1, scales = "free_y")
#' @seealso \code{ggInterval_lineplot}, \code{classic2sym}
#' @keywords datasets
"stock.data.LH"
