


#' Stacked bar plot
#'
#' This function plots a stacked bar of proportions for an input set of data
#'
#' @param data the data
#' @param mapping a aes mapping with at least `x` and `fill`. If facetting then
#'   `group` must contain the facet variable
#' @param ... passed to `geom_bar`
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' stacked_barplot(
#'     ggplot2::diamonds,
#'     ggplot2::aes(x=cut, fill=clarity, group=color)
#'   )+
#'   ggplot2::facet_wrap(dplyr::vars(color))
#'
stacked_barplot = function(data, mapping, ...) {
  tmp = .summary_binomial(data,mapping,.fill)
  ggplot2::ggplot(tmp$data, tmp$mapping)+
    ggplot2::geom_bar(mapping=ggplot2::aes(y=mean), stat ="identity", position = "stack", ...)+
    ggplot2::ylab("proportion")
}


binomial_proportion_points = function(data, mapping, ..., width = 0.8, size=0.5) {
  tmp = .summary_binomial(data,mapping,.x)
  ggplot2::ggplot(tmp$data, tmp$mapping)+
    ggplot2::geom_point(mapping=ggplot2::aes(y=mean), position=ggplot2::position_dodge(width=width), size=size, ...)+
    ggplot2::geom_errorbar(mapping=ggplot2::aes(ymin=lower, ymax=upper), position=ggplot2::position_dodge(width=width), width=width*0.9, ...)+
    ggplot2::ylab("proportion")
}

binomial_proportion_bars = function(data, mapping, ..., width = 0.8, size=0.5,error_colour="grey50", lbl_size = 6) {
  tmp = .summary_binomial(data,mapping,.x)
  ggplot2::ggplot(tmp$data, tmp$mapping)+
    ggplot2::geom_bar(mapping=ggplot2::aes(y=mean), stat="identity", position=ggplot2::position_dodge(width=width), width=width*0.9, size=size, ...)+
    ggplot2::geom_point(mapping=ggplot2::aes(y=mean), position=ggplot2::position_dodge(width=width), size=size, colour=error_colour, ...)+
    ggplot2::geom_errorbar(mapping=ggplot2::aes(ymin=lower, ymax=upper), position=ggplot2::position_dodge(width=width), width=width*0.9, colour=error_colour, ...)+
    ggplot2::geom_text(mapping=ggplot2::aes(x = !!tmp$mapping$x, label = sprintf(" %d", x), colour = !!tmp$mapping$fill, y=-0.01), inherit.aes = FALSE, position = ggplot2::position_dodge(width=0.8),size=gg_label_size(lbl_size), angle=90, hjust=1)+
    ggplot2::ylab("proportion")
}

gg_label_size = function(pts) {
  return(pts/ggplot2::.pt)
}

.summary_binomial = function(data, mapping, denom) {
  denom = rlang::ensym(denom)
  plot_data = names(mapping) %>% purrr::reduce(.f = function(d, colname, ...) {
    f = mapping[[colname]]
    colname = paste0(".",colname)
    d %>% dplyr::mutate(!!colname := !!f)
  }, .init = data) %>% dplyr::select(tidyselect::all_of(paste0(".",names(mapping))))

  labs = lapply(sapply(mapping, rlang::as_label),as.symbol)
  names(labs) = names(mapping)

  rename = paste0(".",names(mapping))
  names(rename) = sapply(mapping, rlang::as_label)

  tmp = plot_data %>% dplyr::group_by(dplyr::across(tidyselect::everything())) %>%
    dplyr::summarise(count = dplyr::n(), .groups="drop") %>%
    # This is a very useful way to use tidyselect syntax within complete:
    tidyr::complete(!!!dplyr::select(., -count), fill = list(count=0))
  # TODO: detect in mapping everything that is the same as .fill and
  # exclude them from the grouping otherwise specifying color and fill
  # could result in incorrect grouping and total calculation.
  tmp2 = tmp %>%
    dplyr::group_by(dplyr::across(c(tidyselect::everything(),-!!denom,-count))) %>%
    dplyr::mutate(total = sum(count))
  #TODO: a fisher exact test.

  tmp3 = tmp2 %>%
    dplyr::mutate(binom::binom.confint(count, total, methods="wilson")) %>%
    dplyr::rename(!!!rename)
  # TODO: this does not handle NaNs as produced by 0/0 groups.
  # These are probably stripped out by ggplot.

  list(
    data = tmp3,
    mapping = ggplot2::aes(!!!labs)
  )
}
