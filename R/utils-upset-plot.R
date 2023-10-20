# Upset plot

#' Upset plot with counts stratified by a categorical column
#'
#' @param df the data
#' @param boolean_cols a tidyselect specification selecting the columns to be
#'   used as binary one-hot encoded classes
#' @param categorical_col a column containing a disjoint category as a factor
#' @param lbl_size font sise of the label
#'
#' @return a ggplot
#' @export
upset_plot = function(df, boolean_cols, categorical_col, lbl_size=5) {

  categorical_col = rlang::ensym(categorical_col)
  cols = tidyselect::eval_select(expr = rlang::enexpr(boolean_cols), data = df)
  boolean_cols = lapply(names(cols), as.symbol)

  # utmp contains the overlapping groups as a `intersection` factor. It is one
  # row per entity
  utmp = df %>% dplyr::ungroup() %>% dtrackr::untrack() %>%
    dplyr::mutate(total = dplyr::n()) %>%

    dplyr::group_by(category = !!categorical_col) %>%
    dplyr::mutate(total_in_category = dplyr::n()) %>%
    dplyr::ungroup() %>%

    # replace FALSE with NAs in booleans
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(cols), .fns = function(.x) ifelse(.x, dplyr::cur_column(), NA_character_), .names=".u.{.col}")) %>%
    # Unite creates a labelled column of the intersection that this item is in based on the column names
    tidyr::unite(col = "intersection", tidyselect::starts_with(".u."), sep=" & ", na.rm=TRUE) %>%
    dplyr::filter(intersection != "") %>%
    dplyr::mutate(intersection = paste0(intersection," only")) %>%


    dplyr::group_by(intersection) %>%
    dplyr::mutate(total_in_intersection = dplyr::n()) %>%
    dplyr::ungroup() %>%
    # convert to a frequency ordered factor.
    dplyr::mutate(intersection = forcats::fct_infreq(factor(intersection),ordered = TRUE))

  # utmp2 contains each membership of each group as a new row. It is one
  # row per group membership.
  utmp2 = utmp %>%
    dplyr::mutate(.id = dplyr::row_number()) %>%
    dplyr::mutate(dplyr::across(.cols = tidyselect::all_of(cols),.fns = function(.x) ifelse(.x,dplyr::cur_column(),NA_character_), .names=".u.{.col}")) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with(".u."),names_to="name", values_to = "primary") %>%
    dplyr::filter(!is.na(primary)) %>% dplyr::select(-name) %>%
    dplyr::group_by(primary) %>%
    dplyr::mutate(total_in_primary = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(primary = paste0("All ",primary)) %>%
    dplyr::mutate(primary = forcats::fct_rev(forcats::fct_infreq(factor(primary),ordered = TRUE)))

  # # p1 gives us the proportions of each intersection compared to the overall
  # # number of items. Since utmp is the
  # p1 = utmp %>% dplyr::group_by(
  #   category, total_in_intersection, intersection) %>%
  #   dplyr::summarise(n=dplyr::n(), .groups="drop") %>%
  #   dplyr::mutate(binom::binom.confint(n, total_in_intersection, methods = "wilson")) %>%
  #   tidyr::complete(intersection, category, fill=list(x=0,mean=0,lower=0,upper=0))
  #
  # # p2 gives us the proportions of each primary set compared to total of each
  # # category.
  # p2 = utmp2 %>%
  #   dplyr::group_by(category, primary, total_in_primary) %>%
  #   dplyr::summarise(n=dplyr::n(),.groups = "drop") %>%
  #   dplyr::mutate(binom::binom.confint(n, total_in_primary, methods = "wilson")) %>%
  #   tidyr::complete(category, primary, fill=list(x=0,mean=0,lower=0,upper=0))

  p1 = utmp %>% dplyr::group_by(
    category, total_in_category, intersection) %>%
    dplyr::summarise(n=dplyr::n(), .groups="drop_last") %>%
    tidyr::complete(intersection, fill=list(n=0)) %>%
    dplyr::mutate(binom::binom.confint(n, total_in_category, methods = "wilson")) %>%
    dplyr::mutate(label = sprintf("%d/%d",x,n))

  # p2 gives us the proportions of each primary set compared to total of each
  # category.
  p2 = utmp2 %>%
    dplyr::group_by(category, total_in_category, primary) %>%
    dplyr::summarise(n=dplyr::n(), .groups="drop_last") %>%
    tidyr::complete(primary, fill=list(n=0)) %>%
    dplyr::mutate(binom::binom.confint(n, total_in_category, methods = "wilson")) %>%
    dplyr::mutate(label = sprintf("%d/%d",x,n))


  #
  # # p2 = utmp2 %>% dplyr::group_by(admission.category,primary) %>% dplyr::summarise(n=dplyr::n())  %>% dplyr::mutate(binom::binom.confint(n , sum(n), methods = "wilson")) %>% dplyr::ungroup() %>%tidyr::complete(admission.category,primary,fill=list(x=0,mean=0))

  x_size = utmp2 %>% dplyr::select(intersection) %>% dplyr::distinct() %>% nrow()
  y_size = utmp2 %>% dplyr::select(primary) %>% dplyr::distinct() %>% nrow()

  m = utmp2 %>% dplyr::select(primary,intersection) %>% dplyr::distinct()
  m_line = m %>% dplyr::group_by(intersection) %>% dplyr::summarise(start = min(primary), end = max(primary))

  # total_plot = ggplot2::ggplot(
  #       utmp %>% dplyr::ungroup() %>% dplyr::select(category,total_in_category) %>% dplyr::distinct(),
  #       ggplot2::aes(y=forcats::fct_rev(category),x=category,label=total_in_category,colour=category))+
  #   ggplot2::geom_text(angle=45,size=gg_label_size(lbl_size))+
  #   ggplot2::guides(colour=ggplot2::guide_none())+
  #   ggplot2::scale_x_discrete(breaks=NULL)+
  #   ggplot2::scale_y_discrete(breaks=NULL)+
  #   gg_hide_X_axis()+
  #   gg_hide_Y_axis()+
  #   ggplot2::scale_color_brewer(palette="Dark2")#+
  #   #ggplot2::coord_fixed(ratio = 1)

  total_plot = ggplot2::ggplot(
    utmp %>% dplyr::ungroup() %>% dplyr::select(category,total_in_category) %>% dplyr::distinct(),
    ggplot2::aes(y=factor(1),x=factor(1),label=total_in_category,colour=category))+
    ggplot2::geom_text(position = ggstance::position_dodge2v(height=0.9,reverse = TRUE),size=gg_label_size(lbl_size))+
    ggplot2::guides(colour=ggplot2::guide_none())+
    ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::scale_y_discrete(breaks=NULL)+
    gg_hide_X_axis()+
    gg_hide_Y_axis()+
    ggplot2::scale_color_brewer(palette="Dark2")#+
  #ggplot2::coord_fixed(ratio = 1)

  nplot = ggplot2::ggplot(p2, ggplot2::aes(y=primary,x=as.factor(1),label=x,colour=category))+
    ggplot2::geom_text(position = ggstance::position_dodge2v(height=0.8, reverse=TRUE),size=gg_label_size(lbl_size))+
    ggplot2::guides(colour=ggplot2::guide_none())+
    ggplot2::scale_x_discrete(breaks=NULL)+
    gg_hide_X_axis()+
    ggplot2::scale_color_brewer(palette="Dark2")#+
    #ggplot2::coord_fixed(ratio = y_size)

  nplot2 = ggplot2::ggplot(p1, ggplot2::aes(x=intersection,y=as.factor(1),label=x,colour= category))+
    ggplot2::geom_text(position = ggplot2::position_dodge(width=0.8),size=gg_label_size(lbl_size),angle=90)+
    ggplot2::guides(colour=ggplot2::guide_none())+
    ggplot2::scale_y_discrete(breaks=NULL)+
    gg_hide_Y_axis()+ggplot2::scale_color_brewer(palette="Dark2")#+
    #ggplot2::coord_fixed(ratio = 1/y_size)

  mplot = ggplot2::ggplot()+
    ggplot2::geom_point(data=m, mapping=ggplot2::aes(x=intersection, y=primary), size=2)+
    ggplot2::geom_segment(data=m_line, mapping=ggplot2::aes(x=intersection, y=start,xend=intersection, yend=end), size=0.5)+
    ggplot2::xlab(NULL)+ggplot2::ylab(NULL)+
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle=30,hjust = 1,vjust = 0))+
    ggplot2::scale_x_discrete(position = "top")#+
    #ggplot2::coord_fixed(ratio = y_size/x_size)

  bplot =
    ggplot2::ggplot(p2, ggplot2::aes(y=primary,fill = category))+
    ggplot2::geom_bar(ggplot2::aes(x=mean*100), stat="identity", colour="black", position=ggstance::position_dodge2v(height=0.9, reverse=TRUE), width = 0.8)+
    #ggplot2::geom_errorbarh(ggplot2::aes(xmin=lower*100,xmax=upper*100), position=ggstance::position_dodgev(height=0.7), height = 0.4)+
    ggplot2::xlab(NULL)+
    ggplot2::scale_x_continuous(position="top", labels = function(x) sprintf("%1.0f%%",x))+
    ggplot2::scale_fill_brewer(palette="Dark2",name=NULL)

  b2plot =
    ggplot2::ggplot(p1, ggplot2::aes(x=intersection,fill = category))+
    ggplot2::geom_bar(ggplot2::aes(y=mean*100), stat="identity", colour="black", position=ggplot2::position_dodge(width=0.9), width = 0.8)+
    #ggplot2::geom_errorbar(ggplot2::aes(ymin=lower*100,ymax=upper*100), colour="black", position=ggplot2::position_dodge(width=0.7), width = 0.4)+
    ggplot2::ylab(NULL)+
    ggplot2::scale_y_continuous(labels = function(x) sprintf("%1.0f%%",x))+
    ggplot2::scale_fill_brewer(palette="Dark2",aesthetics = c("fill","colour"),name=NULL)

  # browser()

  layout = "
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  AAAAAAAAAAEBBB
  DDDDDDDDDDFHHH
  DDDDDDDDDDFHHH
  CCCCCCCCCCIGGG
  CCCCCCCCCCIGGG
  CCCCCCCCCCIGGG
  CCCCCCCCCCIGGG
  CCCCCCCCCCIGGG
  CCCCCCCCCCIGGG
  "
  # DDDDDDDDDDEEEE
  # DDDDDDDDDDEEEE
  # DDDDDDDDDDEEEE
  # DDDDDDDDDDEEEE
  # DDDDDDDDDDGGGG
  # DDDDDDDDDDGGGG
  # FFFFFFFFFFGGGG
  # FFFFFFFFFFGGGG
  # FFFFFFFFFFGGGG
  # FFFFFFFFFFGGGG
  # FFFFFFFFFFGGGG
  # FFFFFFFFFFGGGG
  # "

  p =
    mplot+ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=340,vjust = 0,hjust=1))+ggplot2::theme(axis.text.y = ggplot2::element_text(angle=340, hjust=1, vjust=1))+ #A
    bplot+gg_hide_Y_axis()+ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle=0,vjust = 0,hjust=0.5)) + #B
    b2plot+gg_hide_X_axis()+
    #ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle=340, hjust=0, vjust=1))+
    ggplot2::xlab(NULL)+ #C

    # agePlot+gg_hide_X_axis()+ #D
    # ageTable+ #E
    #
    # cciPlot+gg_hide_X_axis()+ #E
    # cciTable+ #G

    (nplot2+gg_hide_X_axis()+ patchwork::plot_layout(tag_level = 'new')) + #D
    (nplot+gg_hide_Y_axis()+ patchwork::plot_layout(tag_level = 'new'))+ #E
    (total_plot+gg_hide_Y_axis()+gg_hide_X_axis()+ patchwork::plot_layout(tag_level = 'new'))+ #F
    patchwork::guide_area()+ # G

    patchwork::plot_spacer()+ # H
    patchwork::plot_spacer()+ # I

    patchwork::plot_layout(guides = "collect",design = layout)+
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(plot.tag = ggplot2::element_text(size = 8),
          plot.tag.position = c(0, 1))

  return(p)
}

gg_hide_X_axis = function() {
  ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.text.x = ggplot2::element_blank(),
                 axis.text.x.bottom = ggplot2::element_blank(), axis.text.x.top = ggplot2::element_blank())
}

gg_hide_Y_axis = function() {
  ggplot2::theme(axis.title.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_blank(),
                 axis.text.y.left = ggplot2::element_blank(), axis.text.y.right = ggplot2::element_blank())
}
