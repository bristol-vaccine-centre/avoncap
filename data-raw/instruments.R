devtools::load_all()


tmpfn = function(name) {
  fns = as.character(ls.str(envir = getNamespace("avoncap"),mode = "function"))
  frameworks = fns[stringr::str_starts(fns, sprintf("(%s)\\.",name))]
  tmp = data.tree::as.Node(
    tibble::tibble(
      pathString = frameworks,
      defined = TRUE
    ), pathDelimiter = ".")
  return(tmp)
}

frameworks = list(
  norm = tmpfn("normalise"),
  aug = tmpfn("augment"),
  validate = tmpfn("validate")
)

usethis::use_data(frameworks)
