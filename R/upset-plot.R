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

  categorical_col = ensym(categorical_col)
  cols = tidyselect::eval_select(expr = enexpr(boolean_cols), data = df)
  boolean_cols = lapply(names(cols), as.symbol)

  # utmp contains the overlapping groups as a `intersection` factor. It is one
  # row per entity
  utmp = df %>% ungroup() %>% untrack() %>%
    mutate(total = n()) %>%

    group_by(category = !!categorical_col) %>%
    mutate(total_in_category = n()) %>%
    ungroup() %>%

    # replace FALSE with NAs in booleans
    mutate(across(.cols = all_of(cols), .fns = function(.x) ifelse(.x, cur_column(), NA_character_), .names=".u.{.col}")) %>%
    # Unite creates a labelled column of the intersection that this item is in based on the column names
    tidyr::unite(col = "intersection", starts_with(".u."), sep=" & ", na.rm=TRUE) %>%
    filter(intersection != "") %>%
    mutate(intersection = paste0(intersection," only")) %>%


    group_by(intersection) %>%
    mutate(total_in_intersection = n()) %>%
    ungroup() %>%
    # convert to a frequency ordered factor.
    mutate(intersection = forcats::fct_infreq(factor(intersection),ordered = TRUE))

  # utmp2 contains each membership of each group as a new row. It is one
  # row per group membership.
  utmp2 = utmp %>%
    mutate(.id = row_number()) %>%
    mutate(across(.cols = all_of(cols),.fns = function(.x) ifelse(.x,cur_column(),NA_character_), .names=".u.{.col}")) %>%
    pivot_longer(cols = starts_with(".u."),names_to="name", values_to = "primary") %>%
    filter(!is.na(primary)) %>% select(-name) %>%
    group_by(primary) %>%
    mutate(total_in_primary = n()) %>%
    ungroup() %>%
    mutate(primary = paste0("All ",primary)) %>%
    mutate(primary = forcats::fct_rev(forcats::fct_infreq(factor(primary),ordered = TRUE)))

  # # p1 gives us the proportions of each intersection compared to the overall
  # # number of items. Since utmp is the
  # p1 = utmp %>% group_by(
  #   category, total_in_intersection, intersection) %>%
  #   summarise(n=n(), .groups="drop") %>%
  #   mutate(binom::binom.confint(n, total_in_intersection, methods = "wilson")) %>%
  #   tidyr::complete(intersection, category, fill=list(x=0,mean=0,lower=0,upper=0))
  #
  # # p2 gives us the proportions of each primary set compared to total of each
  # # category.
  # p2 = utmp2 %>%
  #   group_by(category, primary, total_in_primary) %>%
  #   summarise(n=n(),.groups = "drop") %>%
  #   mutate(binom::binom.confint(n, total_in_primary, methods = "wilson")) %>%
  #   tidyr::complete(category, primary, fill=list(x=0,mean=0,lower=0,upper=0))

  p1 = utmp %>% group_by(
    category, total_in_category, intersection) %>%
    summarise(n=n(), .groups="drop_last") %>%
    tidyr::complete(intersection, fill=list(n=0)) %>%
    mutate(binom::binom.confint(n, total_in_category, methods = "wilson")) %>%
    mutate(label = sprintf("%d/%d",x,n))

  # p2 gives us the proportions of each primary set compared to total of each
  # category.
  p2 = utmp2 %>%
    group_by(category, total_in_category, primary) %>%
    summarise(n=n(),, .groups="drop_last") %>%
    tidyr::complete(primary, fill=list(n=0)) %>%
    mutate(binom::binom.confint(n, total_in_category, methods = "wilson")) %>%
    mutate(label = sprintf("%d/%d",x,n))


  #
  # # p2 = utmp2 %>% group_by(admission.category,primary) %>% summarise(n=n())  %>% mutate(binom::binom.confint(n , sum(n), methods = "wilson")) %>% ungroup() %>%tidyr::complete(admission.category,primary,fill=list(x=0,mean=0))

  x_size = utmp2 %>% select(intersection) %>% distinct() %>% nrow()
  y_size = utmp2 %>% select(primary) %>% distinct() %>% nrow()

  m = utmp2 %>% select(primary,intersection) %>% distinct()
  m_line = m %>% group_by(intersection) %>% summarise(start = min(primary), end = max(primary))

  # total_plot = ggplot(
  #       utmp %>% ungroup() %>% select(category,total_in_category) %>% distinct(),
  #       aes(y=forcats::fct_rev(category),x=category,label=total_in_category,colour=category))+
  #   geom_text(angle=45,size=gg_label_size(lbl_size))+
  #   guides(colour=guide_none())+
  #   scale_x_discrete(breaks=NULL)+
  #   scale_y_discrete(breaks=NULL)+
  #   ggrrr::gg_hide_X_axis()+
  #   ggrrr::gg_hide_Y_axis()+
  #   scale_color_brewer(palette="Dark2")#+
  #   #coord_fixed(ratio = 1)

  total_plot = ggplot(
    utmp %>% ungroup() %>% select(category,total_in_category) %>% distinct(),
    aes(y=factor(1),x=factor(1),label=total_in_category,colour=category))+
    geom_text(position = ggstance::position_dodge2v(height=0.9,reverse = TRUE),size=gg_label_size(lbl_size))+
    guides(colour=guide_none())+
    scale_x_discrete(breaks=NULL)+
    scale_y_discrete(breaks=NULL)+
    ggrrr::gg_hide_X_axis()+
    ggrrr::gg_hide_Y_axis()+
    scale_color_brewer(palette="Dark2")#+
  #coord_fixed(ratio = 1)

  nplot = ggplot(p2, aes(y=primary,x=as.factor(1),label=x,colour=category))+
    geom_text(position = ggstance::position_dodge2v(height=0.8, reverse=TRUE),size=gg_label_size(lbl_size))+
    guides(colour=guide_none())+
    scale_x_discrete(breaks=NULL)+
    ggrrr::gg_hide_X_axis()+
    scale_color_brewer(palette="Dark2")#+
    #coord_fixed(ratio = y_size)

  nplot2 = ggplot(p1, aes(x=intersection,y=as.factor(1),label=x,colour= category))+
    geom_text(position = position_dodge(width=0.8),size=gg_label_size(lbl_size),angle=90)+
    guides(colour=guide_none())+
    scale_y_discrete(breaks=NULL)+
    gg_hide_Y_axis()+scale_color_brewer(palette="Dark2")#+
    #coord_fixed(ratio = 1/y_size)

  mplot = ggplot()+
    geom_point(data=m, mapping=aes(x=intersection, y=primary), size=2)+
    geom_segment(data=m_line, mapping=aes(x=intersection, y=start,xend=intersection, yend=end), size=0.5)+
    xlab(NULL)+ylab(NULL)+
    theme(axis.text.y = element_text(angle=30,hjust = 1,vjust = 0))+
    scale_x_discrete(position = "top")#+
    #coord_fixed(ratio = y_size/x_size)

  bplot =
    ggplot(p2, aes(y=primary,fill = category))+
    geom_bar(aes(x=mean*100), stat="identity", colour="black", position=ggstance::position_dodge2v(height=0.9, reverse=TRUE), width = 0.8)+
    #geom_errorbarh(aes(xmin=lower*100,xmax=upper*100), position=ggstance::position_dodgev(height=0.7), height = 0.4)+
    xlab(NULL)+
    scale_x_continuous(position="top", labels = function(x) sprintf("%1.0f%%",x))+
    scale_fill_brewer(palette="Dark2",name=NULL)

  b2plot =
    ggplot(p1, aes(x=intersection,fill = category))+
    geom_bar(aes(y=mean*100), stat="identity", colour="black", position=position_dodge(width=0.9), width = 0.8)+
    #geom_errorbar(aes(ymin=lower*100,ymax=upper*100), colour="black", position=position_dodge(width=0.7), width = 0.4)+
    ylab(NULL)+
    scale_y_continuous(labels = function(x) sprintf("%1.0f%%",x))+
    scale_fill_brewer(palette="Dark2",aesthetics = c("fill","colour"),name=NULL)

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
    mplot+theme(axis.text.x.top = element_text(angle=340,vjust = 0,hjust=1))+theme(axis.text.y = element_text(angle=340, hjust=1, vjust=1))+ #A
    bplot+gg_hide_Y_axis()+theme(axis.text.x.top = element_text(angle=0,vjust = 0,hjust=0.5)) + #B
    b2plot+gg_hide_X_axis()+
    #theme(axis.text.x.bottom = element_text(angle=340, hjust=0, vjust=1))+
    xlab(NULL)+ #C

    # agePlot+gg_hide_X_axis()+ #D
    # ageTable+ #E
    #
    # cciPlot+gg_hide_X_axis()+ #E
    # cciTable+ #G

    (nplot2+gg_hide_X_axis()+ plot_layout(tag_level = 'new')) + #D
    (nplot+gg_hide_Y_axis()+ plot_layout(tag_level = 'new'))+ #E
    (total_plot+gg_hide_Y_axis()+gg_hide_X_axis()+ plot_layout(tag_level = 'new'))+ #F
    patchwork::guide_area()+ # G

    patchwork::plot_spacer()+ # H
    patchwork::plot_spacer()+ # I

    patchwork::plot_layout(guides = "collect",design = layout)+
    patchwork::plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(size = 8),
          plot.tag.position = c(0, 1))

  return(p)
}

