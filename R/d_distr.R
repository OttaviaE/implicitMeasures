#' Plot IAT or SC-IAT scores (distribution)
#'
#' Plot the distribution of the IAT \emph{D-score} or the SC-IAT \emph{D}.
#'
#' @param data Dataframe with either class \code{dscore} or \code{dsciat}.
#' @param graph String. Indicates the graphs to display. Default is
#'                \code{histogram}
#' @param n_bin Numeric. Indicates the number of bins to diplay.
#' @param col_fill String. Indicates the color for filling the bars of the
#'                  \code{histogram} or the curve of the \code{density}. Default
#'                  is \code{royalblue}.
#' @param col_point String. Indicates the color for the individual scores --each
#'                   point -- in the violin plot. Default is \code{red}.
#' @param include_stats Logical. Indicates whether to add descriptive statistics.
#'                       The \code{mean} is depicted with a solid line. The two
#'                       dashed lines represent +/- 2 \emph{s.d.} from the mean.
#'                       Default is \code{FALSE}.
#'
#' @return A ggplot object.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#'   # Plotting the IAT D-score
#'   data("raw_data") # import data
#'   iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
#'                           block_id = "blockcode",
#'                           mapA_practice = "practice.iat.Milkbad",
#'                           mapA_test = "test.iat.Milkbad",
#'                           mapB_practice = "practice.iat.Milkgood",
#'                           mapB_test = "test.iat.Milkgood",
#'                           latency_id = "latency",
#'                           accuracy_id = "correct",
#'                           trial_id = "trialcode",
#'                           trial_eliminate = c("reminder", "reminder1"),
#'                           demo_id = "blockcode",
#'                           trial_demo = "demo")
#'   iat_data <- iat_cleandata[[1]]
#' # calculate D-score
#'   iat_dscore <- computeD(iat_data,
#'                        Dscore =  "d2")
#'   d_distr(iat_dscore) # Default graph
#'   d_distr(iat_dscore, graph = "histogram",
#'             n_bin = 30) # Histogram with a different number of bins
#'   d_distr(iat_dscore, graph = "density") # IAT D-score density plot
#'   d_distr(iat_dscore, graph = "violin") # IAT D-score violin plot
#'
#'   # Plot the SC-IAT D for the first SC-IAT
#'   data("raw_data") # load data
#'   sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
#'                          block_id = "blockcode",
#'                          latency_id = "latency",
#'                          accuracy_id = "correct",
#'                          block_sciat_1 = c("test.sc_dark.Darkbad",
#'                                            "test.sc_dark.Darkgood"),
#'                          block_sciat_2 = c("test.sc_milk.Milkbad",
#'                                            "test.sc_milk.Milkgood"),
#'                          trial_id  = "trialcode",
#'                          trial_eliminate = c("reminder",
#'                                              "reminder1"))
#'
#'  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
#'  d_sciat1 <- Dsciat(sciat1,
#'                   mappingA = "test.sc_dark.Darkbad",
#'                   mappingB = "test.sc_dark.Darkgood",
#'                   non_response = "alert")
#'   d_distr(d_sciat1, graph = "histogram",
#'              include_stats = TRUE) # SC-IAT D histogram with descriptive
#'                                    #  statistics

d_distr <- function(data,
                    graph = c("histogram", "density", "violin"),
                    n_bin = 80,
                    col_fill = "royalblue",
                    col_point = "red",
                    include_stats = FALSE){
  graph = match.arg(graph)
  data$variable <- 0
  if (is.na(class(data)[2])){
    stop("data must be an object of class dscore or dsciat")
  } else if (class(data)[2] == "dscore"){
    data$d <- data[, grep("dscore", colnames(data))]
    x_lab <- "D-score"
  } else if (class(data)[2] == "dsciat"){
    data$d <- data[, grep("d_sciat", colnames(data))]
    x_lab <- "D-sciat"
  } else {
    stop("data must be an object of class dscore or dsciat")
  }

  if (graph == "histogram"){
    d_graph <- ggplot(data,
                      aes(x = data$d)) +
      geom_histogram(bins = n_bin, col = col_fill,
                     fill = col_fill,
                     alpha = .50)
    d_graph <- d_graph  + theme_minimal() + xlab(x_lab)  +
      theme(axis.title.y = element_blank())
    if (include_stats == FALSE){
      d_graph <- d_graph
    } else {
      d_graph <- d_graph + geom_vline(xintercept = mean(data$d))
      d_graph <- d_graph + geom_vline(xintercept = (mean(data$d) +
                                                      2*sd(data$d)),
                                      linetype = "dotted")
      d_graph <- d_graph + geom_vline(xintercept = (mean(data$d) -
                                                      2*sd(data$d)),
                                      linetype = "dotted")
    }
  }
  else if (graph == "density"){
    d_graph <- ggplot(data,
                      aes(x = data$d)) +
      geom_density(alpha = 0.70, fill = col_fill ,
                   col = col_fill)
    d_graph <- d_graph  + theme_minimal() + xlab(x_lab) +
      theme(axis.title.y = element_blank())
    if (include_stats == FALSE){
      d_graph <- d_graph
    } else {
      d_graph <- d_graph + geom_vline(xintercept = mean(data$d))
      d_graph <- d_graph + geom_vline(xintercept = (mean(data$d) +
                                                      2*sd(data$d)),
                                      linetype = "dotted")
      d_graph <- d_graph + geom_vline(xintercept = (mean(data$d) -
                                                      2*sd(data$d)),
                                      linetype = "dotted")
    }
  }
  else if (graph == "violin"){
    d_graph <- ggplot(data,
                      aes(y = data$d,
                          x = data$variable)) +
      geom_violin(trim = F)  +
      geom_jitter(shape=16, col = col_point,
                  position=position_jitter(0.2))
    d_graph <- d_graph  + theme_minimal()
    d_graph <- d_graph + theme(axis.text.x = element_blank(),
                               axis.title.x = element_blank()) +
      ylab(x_lab)

    if (include_stats == FALSE){
      d_graph <- d_graph
    } else {
      d_graph <- d_graph + stat_summary(fun.data=mean_sdl,
                                        geom="pointrange",
                                        color="black")
    }

  }
  return(d_graph)
}
