#' @name facedata
#' @aliases facedata
#' @title Face Data Example
#' @description
#' Symbolic data matrix with all the variables of interval type.
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
#' @description Environment interval and modal data example.
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
