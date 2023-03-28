


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
#' stacked_barplot(diamonds, aes(x=cut, fill=clarity, group=color))+facet_wrap(vars(color))
#'
stacked_barplot = function(data, mapping, ...) {
  tmp = .summary_binomial(data,mapping,.fill)
  ggplot(tmp$data, tmp$mapping)+
    geom_bar(mapping=aes(y=mean), stat ="identity", position = "stack", ...)+
    ylab("proportion")
}


binomial_proportion_points = function(data, mapping, ..., width = 0.8, size=0.5) {
  tmp = .summary_binomial(data,mapping,.x)
  ggplot(tmp$data, tmp$mapping)+
    geom_point(mapping=aes(y=mean), position=position_dodge(width=width), size=size, ...)+
    geom_errorbar(mapping=aes(ymin=lower, ymax=upper), position=position_dodge(width=width), width=width*0.9, ...)+
    ylab("proportion")
}

binomial_proportion_bars = function(data, mapping, ..., width = 0.8, size=0.5,error_colour="grey50", lbl_size = 6) {
  tmp = .summary_binomial(data,mapping,.x)
  ggplot(tmp$data, tmp$mapping)+
    geom_bar(mapping=aes(y=mean), stat="identity", position=position_dodge(width=width), width=width*0.9, size=size, ...)+
    geom_point(mapping=aes(y=mean), position=position_dodge(width=width), size=size, colour=error_colour, ...)+
    geom_errorbar(mapping=aes(ymin=lower, ymax=upper), position=position_dodge(width=width), width=width*0.9, colour=error_colour, ...)+
    geom_text(mapping=aes(x = !!tmp$mapping$x, label = sprintf(" %d", x), colour = !!tmp$mapping$fill, y=-0.01), inherit.aes = FALSE, position = position_dodge(width=0.8),size=gg_label_size(lbl_size), angle=90, hjust=1)+
    ylab("proportion")
}



.summary_binomial = function(data, mapping, denom) {
  denom = ensym(denom)
  plot_data = names(mapping) %>% reduce(.f = function(d, colname, ...) {
    f = mapping[[colname]]
    colname = paste0(".",colname)
    d %>% mutate(!!colname := !!f)
  }, .init = data) %>% select(all_of(paste0(".",names(mapping))))

  labs = lapply(sapply(mapping, as_label),as.symbol)
  names(labs) = names(mapping)

  rename = paste0(".",names(mapping))
  names(rename) = sapply(mapping, as_label)

  tmp = plot_data %>% group_by(across(everything())) %>%
    summarise(count = n(), .groups="drop") %>%
    # This is a very useful way to use tidyselect syntax within complete:
    tidyr::complete(!!!select(., -count), fill = list(count=0))
  # TODO: detect in mapping everything that is the same as .fill and
  # exclude them from the grouping otherwise specifying color and fill
  # could result in incorrect grouping and total calculation.
  tmp2 = tmp %>%
    group_by(across(c(everything(),-!!denom,-count))) %>%
    mutate(total = sum(count))
  #TODO: a fisher exact test.

  tmp3 = tmp2 %>%
    mutate(binom::binom.confint(count, total, methods="wilson")) %>%
    rename(!!!rename)
  # TODO: this does not handle NaNs as produced by 0/0 groups.
  # These are probably stripped out by ggplot.

  list(
    data = tmp3,
    mapping = aes(!!!labs)
  )
}
