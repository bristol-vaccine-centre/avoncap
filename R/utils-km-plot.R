

#' Faceted Kaplan-Meier plot
#'
#' @param df the data
#' @param coxmodel the cox model output of survival::coxph from the data
#' @param facet the division to highlight in the KM strata. Defaults to first
#'   term on the lhs of the cox model formula
#' @inheritDotParams survival::survfit
#' @param maxtime the longest x value to plot
#' @param ylab the y axis label
#' @param xlab the x axis label
#' @param facetlab a label to add as a facet title
#' @param ylim the range to show on the KM plot
#' @param n_breaks number of x axis breaks to display this also determines the
#'   timing and number of "at risk" counts to display.
#' @param heights the relative height between the KM plot and the "at risk" table
#' @param invert reverse survival statistics to count number of affected
#'
#' @return a ggplot patchwork.
#' @export
km_plot = function(df, coxmodel,
                   facet=NULL, ...,
                   maxtime = max(sfit$time),
                   ylab=if (!invert) "surviving (%)" else "affected (%)",
                   xlab="time (days)",
                   facetlab = NULL,
                   ylim=c(NA,100),
                   n_breaks=5,
                   heights = c(10,1),
                   invert=FALSE) {

  if (is.null(facet)) facet = all.vars(rlang::f_rhs(coxmodel$formula))[[1]]

  form = stats::update(coxmodel$formula, stats::as.formula(sprintf(". ~ %s",facet)))
  sfit = survival::survfit(formula = form, data = df, ...)

  # get the level names from the strata.
  lvl = names(sfit$strata) %>% stringr::str_extract("=(.*)",1)

  # adds on a zero time point for all levels.
  ci = tibble::tibble(
    strata = purrr::lmap(sfit$strata, ~ list(rep(names(.x),.x))) %>% purrr::list_c() %>%
      stringr::str_extract("=(.*)",1) %>% c(lvl) %>% factor(levels = lvl),
    time = c(sfit$time, rep(0,length(lvl))),
    n.risk = c(sfit$n.risk, sfit$n),
    n.event = c(sfit$n.event, rep(0,length(lvl))),
    n.censor = c(sfit$n.censor, rep(0,length(lvl))),
    surv.estimate = c(sfit$surv, rep(1,length(lvl))),
    surv.lower = c(sfit$lower, rep(1,length(lvl))),
    surv.upper = c(sfit$upper, rep(1,length(lvl))),
    std.error = c(sfit$std.err, rep(0,length(lvl)))
  ) %>% dplyr::arrange(strata, time, n.event) %>%
    dplyr::group_by(strata) %>%
    # needed to get the end of the rectangles.
    dplyr::mutate(next.time = dplyr::lead(time))

  # flip survival probability into failure probability
  if (invert) {
    ci = ci %>% dplyr::mutate(
      surv.estimate = 1-surv.estimate,
      surv.lower2 = 1-surv.upper,
      surv.upper = 1-surv.lower
    ) %>% dplyr::mutate(
      surv.lower = surv.lower2
    )
  }

  p1 = ggplot2::ggplot(ci) +
    ggplot2::geom_step(ggplot2::aes(x = time, y = surv.estimate*100, color = strata))+
    ggplot2::geom_rect(ggplot2::aes(xmin = time,xmax = next.time, ymin = surv.lower*100, ymax = surv.upper*100, fill = strata), alpha=0.2)+
    ggplot2::ylab(ylab)+
    ggplot2::xlab(NULL)+
    ggplot2::coord_cartesian(ylim = ylim, xlim = c(0,maxtime))+
    ggplot2::geom_point(ggplot2::aes(x = time, y = surv.estimate*100),data = ci %>% dplyr::filter(n.censor>0))+
    ggplot2::scale_x_continuous(breaks = scales::breaks_extended(n=n_breaks))+
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_blank())

  times = scales::breaks_extended(n=n_breaks)(0:maxtime)

  if (!is.null(facetlab)) {
    form = stats::as.formula(sprintf(" ~ \"%s\" ",facetlab))
    p1 = p1 + ggplot2::facet_wrap(form)
  }

  # at risk table.

  ci2 = ci %>% dplyr::mutate(
    # this finds the next lowest value for time which is in the data and maps
    # it to the times in list of x axis breaks.
      time = purrr::map_int(time, ~ max(c(times[which(times<=.x)],0)))
    ) %>%
    dplyr::group_by(time,strata) %>%
    dplyr::summarise(n.risk = max(n.risk)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(strata,time) %>%
    tidyr::fill(n.risk)

  t = ggplot2::ggplot(ci2, ggplot2::aes(y = forcats::fct_rev(strata), x = time, colour=strata)) +
    ggplot2::geom_text(ggplot2::aes(label = n.risk),show.legend = FALSE) +
    ggplot2::scale_x_continuous(breaks = times)+
    ggplot2::coord_cartesian(xlim = c(0,maxtime))+
    ggplot2::theme(panel.grid = ggplot2::element_blank())+
    ggplot2::ylab(NULL)+
    ggplot2::xlab(xlab)

  out = p1+t+patchwork::plot_layout(ncol=1, guides = "collect",heights = heights)

  return(out)

}
