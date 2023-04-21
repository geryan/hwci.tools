#' @title HWCI plots
#' @description
#' Automatically generated plot types for `hwcresult` and `hwci_lm` objects.
#'
#'
#' @param data A results object of class `hwciresult` or `hwci_lm`.
#' @param type Type of plot to be produced: `"index"`, `"sub-index"`, or `"indicator` produce plots for single level of index suite, while default `"all"` produces composite of index, sub-index, and indicators. This parameter is unused where `"hwci_lm" %in% class(data)`, and a single index plot is produced in that instance.
#'
#' @return A `ggplot2`/`patchwork`-style plot.
#' @importFrom ggplot2 ggplot geom_line aes labs theme_classic theme scale_y_continuous scale_colour_manual facet_wrap
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Plot index suite
#' plot_hwci(sp1_indices)
#' # Plot only sub-indices
#' plot_hwci(sp2_indices, type = "sub-index")
#'
plot_hwci <- function(
    data,
    type = c("all", "index", "sub-index", "indicator")
  ){

  cl <- class(data)

  if("hwci_lm" %in% cl){

    return(plot_index(data))

  } else {

    type <- match.arg(type)

    switch(
      type,
      "all"       = plot_all_hwci(data),
      "index"     = plot_index(data),
      "sub-index" = plot_subindex(data),
      "indicator" = plot_indicator(data)
    )


  }
}

plot_all_hwci <- function(data){
  pidx <- plot_index(data)
  psid <- plot_subindex(data)
  pidc <- plot_indicator(data)

  pidx + psid + pidc + patchwork::plot_layout(ncol = 1)
}

plot_index <- function(data){

  cl <- class(data)

  if("hwci_lm" %in% cl){
    df <- tibble::tibble(
      x = data$t,
      y = data$Psi_m
    )

    colour <- "dark orchid"

  } else if("hwci_la" %in% cl){
    df <- tibble::tibble(
      x = data$t,
      y = data$Psi_a
    )

    colour <- "dark orchid"

  } else if("hwci_single_species" %in% cl){
    df <- tibble::tibble(
      x = data$t,
      y = data$X_s
    )

    colour <- "#FA4A4E"

  }

  ggplot2::ggplot(data = df) +
    geom_line(aes(x = .data$x, y = .data$y), colour = colour, size = 2) +
    labs(x = "Year", y = "Index score") +
    theme_classic() +
    theme(axis.line = element_line(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),
          axis.text = element_text(colour = "grey40")) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1))

}

plot_subindex <- function(data){

  df <- bind_rows(
    tibble(
      x = data$t,
      y = data$x_h,
      z = "Human"
    ),
    tibble(
      x = data$t,
      y = data$x_e,
      z = "Economic"
    ),
    tibble(
      x = data$t,
      y = data$x_w,
      z = "Wildlife"
    )
  )

  ggplot2::ggplot(df) +
    geom_line(aes(x = .data$x, y = .data$y, colour = .data$z), size = 1) +
    #geom_line(data = hwci.pr.si, aes(x = year, y = value, linetype = id.provn), size = 1, colour = "grey") +
    labs(x = "Year", y = "Sub-index score", col = "Dimension") +
    theme_classic() +
    theme(axis.line = element_line(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),
          axis.text = element_text(colou = "grey40", size = 6),
          strip.background = element_blank()) +
    #scale_y_continuous(limits = c(0,1)) +
    scale_colour_manual(values = c("#fbe93c", "#7bc043", "#35a7d8")) +
    facet_wrap(~z) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1))

}

plot_indicator <- function(data){

  df_f <- dplyr::bind_rows(
    tibble::tibble(
      x = data$t,
      y = data$h_f,
      z = "Human"
    ),
    tibble::tibble(
      x = data$t,
      y = data$e_f,
      z = "Economic"
    ),
    tibble::tibble(
      x = data$t,
      y = data$w_f,
      z = "Wildlife"
    )
  )

  p_f <- ggplot2::ggplot(df_f) +
    geom_line(aes(x = .data$x, y = .data$y, colour = .data$z), size = 1) +
    #geom_line(data = hwci.pr.si, aes(x = year, y = value, linetype = id.provn), size = 1, colour = "grey") +
    labs(x = "Year", y = "Frequency indicator", col = "Dimension") +
    theme_classic() +
    theme(axis.line = element_line(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),
          axis.text = element_text(colou = "grey40", size = 6),
          strip.background = element_blank()) +
    #scale_y_continuous(limits = c(0,1)) +
    scale_colour_manual(values = c("#fbe93c", "#7bc043", "#35a7d8")) +
    facet_wrap(~z, ) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1)) +
    theme(axis.title.x = element_blank())

  df_s <- dplyr::bind_rows(
    tibble::tibble(
      x = data$t,
      y = data$h_s,
      z = "Human"
    ),
    tibble::tibble(
      x = data$t,
      y = data$e_s,
      z = "Economic"
    ),
    tibble::tibble(
      x = data$t,
      y = data$w_s,
      z = "Wildlife"
    )
  )

  p_s <- ggplot2::ggplot(df_s) +
    geom_line(aes(x = .data$x, y = .data$y, colour = .data$z), size = 1) +
    #geom_line(data = hwci.pr.si, aes(x = year, y = value, linetype = id.provn), size = 1, colour = "grey") +
    labs(x = "Year", y = "Severity Indicator", col = "Dimension") +
    theme_classic() +
    theme(axis.line = element_line(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),
          axis.text = element_text(colour = "grey40", size = 6),
          strip.background = element_blank()) +
    #scale_y_continuous(limits = c(0,1)) +
    scale_colour_manual(values = c("#fbe93c", "#7bc043", "#35a7d8")) +
    facet_wrap(~z, ) +
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), limits = c(0,1)) +
    theme(axis.title.x = element_blank(), strip.text.x = element_blank())

  df_m <- dplyr::bind_rows(
    tibble::tibble(
      x = data$t,
      y = data$h_m,
      z = "Human"
    ),
    tibble::tibble(
      x = data$t,
      y = data$e_m,
      z = "Economic"
    ),
    tibble::tibble(
      x = data$t,
      y = data$w_m,
      z = "Wildlife"
    )
  )

  p_m <- ggplot2::ggplot(df_m) +
    geom_line(aes(x = .data$x, y = .data$y, colour = .data$z), size = 1) +
    #geom_line(data = hwci.pr.si, aes(x = year, y = value, linetype = id.provn), size = 1, colour = "grey") +
    labs(x = "Year", y = "Magnitude indicator", col = "Dimension") +
    theme_classic() +
    theme(axis.line = element_line(colour = "grey30"),
          axis.ticks = element_line(colour = "grey30"),
          axis.text = element_text(colour = "grey40", size = 6),
          strip.background = element_blank()) +
    #scale_y_continuous(limits = c(0,1)) +
    scale_colour_manual(values = c("#fbe93c", "#7bc043", "#35a7d8")) +
    facet_wrap(~z, scales = "free")+
    theme(strip.text.x = element_blank())


  p <- p_f + p_s + p_m + patchwork::plot_layout(guides = "collect", ncol = 1)

  p
}


