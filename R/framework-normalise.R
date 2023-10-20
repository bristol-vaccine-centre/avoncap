## Generic data normalisation infrastructure ----
# This applies a set of mappings

# Dispatch normalisation based on data type and subtype ---

#' Sanitise AvonCap data columns
#'
#' AvonCap data has lots of columns which are named in a difficult to remember
#' fashion, composed of data items that have enumerated values with no
#' semantics. This makes displaying them difficult and any filtering done on the
#' raw data inscrutable. Depending on the source of the data some different
#' columns may be present due to differences in the NHS and UoB data sets. The
#' redcap database has some options that may be checklists and some that are
#' radio buttons, both of these end up with mysterious names in the data.
#'
#' This function maps the data into a tidy dataframe with consistently named
#' columns, and named factors where appropriate. The mapping is defined in data.
#'
#' files Most of the sanitisation code is held in the
#' `normalise-xxx.R` file. but these in turn may depend on the `mapping-xxx.R`
#' files
#'
#' @param rawData - the raw data from `load_data()`
#' @param instrument the numeric instrument number if applicable
#' @inheritDotParams normalise_generic
#'
#' @return a tracked dataframe with n
#' @export
normalise_data = function(
    rawData,
    instrument=NULL,
    ...
) {

  type = attr(rawData,"type")
  subtype = attr(rawData,"subtype")
  norm_fn = c("normalise",type,subtype,instrument) %>% stringr::str_replace("-","_") %>% paste0(collapse=".")
  message("Normalising data using: ",norm_fn)
  norm_fn = tryCatch(
    # utils::getFromNamespace(norm_fn,"avoncap"),
    get(norm_fn),
    error = function(e) {
      message("No data normalisation defined for ",norm_fn)
      return(function(x, ...) x)
    }
  )
  # dispatch the call to the specific subtype of the call
  # or to a noop function if there is no specific method
  norm_fn(rawData, ...) %>%
    .restore_attributes(rawData) %>%
    magrittr::set_attr("instrument", instrument)

}

#' Sanitise RedCap data columns
#'
#' @keywords internal
#' @param rawData - the raw data from `load_data()`
#' @param remove_mapped gets rid of original columns for which we have a mapping
#'   (leaving the new versions)
#' @param remove_unmapped gets rid of columns for which we do not have a mapping
#' @param mappings a set of mappings (see `zzz-avoncap-mappings.R`)
#' @param messages a set of `dtrackr` glue specs that populate the first box fo
#'   the flow chart. (can use `{files}`, `{reproduce_at}`, `{date}`, `{.total}`)
#' @param data_source_info - if not null a filename, and the function will write
#'   out a file with the details of the input files used.
#' @param ... passed onto `.cached(...)`. e.g. `nocache = TRUE` can be used to
#'   defeat caching.
#'
#' @return a tracked dataframe with mappings applied
normalise_generic = function(
    rawData,
    mappings,
    remove_mapped = TRUE,
    remove_unmapped = TRUE,
    messages = c("files: {files}","{.total} rows","files from: {date}"),
    data_source_info = NULL,
    ...
) {

  files = attr(rawData,"file")
  date = attr(rawData,"date")

  if(!is.null(data_source_info)) save_data_source_info(rawData, ethn, .file = data_source_info)

  tomap = names(mappings)
  if (any(names(tomap)=="")) stop("Not all mappings have a non zero name. Mappings must be a named list. The offending items are after, ",tomap[which(tomap=="")-1] %>% paste0(collapse=", "))
  missingCols = tomap[unlist(purrr::map(tomap, ~ !any(colnames(rawData) %>% stringr::str_starts(.x))))]

  if (length(missingCols)>0) message("Some columns expected in the mappings were not present: ",paste0(missingCols,collapse=", "))

  tmp = rawData
  #TODO: store error messages in an attribute and retrieve later.
  tmp2 = .cached({

    originalColnames = colnames(tmp)
    # prefix all the original columns with a single "."
    tmp = tmp %>% dplyr::rename_with(.fn= ~ paste0(".",.x))

    # mutate field values into new values
    for(i in 1:length(mappings)) {
      mappingName = names(mappings)[[i]]

      .fn = mappings[[i]]
      # add in the "."
      tomap = paste0(".",mappingName)
      if (any(stringr::str_starts(colnames(tmp),tomap))) {
        tryCatch({
          # if (mappingName %>% stringr::str_starts("viroldays")) browser()
          tmp = tmp %>% .fn(tomap)
        }, error = function(e) {
          message("could not process column or column set ",mappingName," due to ",e$message)
        })
      } else {
        message("the input data set does not have a ",mappingName," column (or column set)")
      }

    }

    fileName = files

    mappedCols = tmp %>% dplyr::select(tidyselect::starts_with("..")) %>% colnames()
    message("Mapped ",mappedCols %>% length()," columns")

    if(remove_mapped) {
      tmp = tmp %>% dplyr::select(-tidyselect::starts_with(".."))
    } else {
      message("renamed original columns as: ",tmp %>% dplyr::select(tidyselect::starts_with("..")) %>% colnames() %>% paste0(collapse="; "))
    }

    unmappedCols = tmp %>% dplyr::select(c(tidyselect::starts_with("."), -tidyselect::starts_with(".."))) %>% colnames()
    message("Did not map ", unmappedCols %>% length()," columns")

    if(remove_unmapped) {
      tmp = tmp %>% dplyr::select(!(c(tidyselect::starts_with("."), -tidyselect::starts_with(".."))))
    } else {
      message("renamed unmapped columns as: ",unmappedCols %>% paste0(collapse="; "))
    }

    tmp %>% dtrackr::track(.messages = messages) %>%
      dtrackr::capture_exclusions() %>%
      magrittr::set_attr("unmapped", unmappedCols) %>%
      magrittr::set_attr("mapped", mappedCols)

  }, rawData, deparse1(mappings), remove_mapped, remove_unmapped, ..., .prefix="norm") %>%
    magrittr::set_attr("missing", missingCols)
  return(tmp2)
}


## Keys ----

# key_spec = keys_urine_antigens_serotype()
# normUA %>% create_keys(key_spec)
create_keys = function(df, key_spec = list("id"="{dplyr::row_number()}")) {
  for (key in names(key_spec)) {
    newCol = as.symbol(paste0("key.",key))
    keyValues = glue::glue_data(df, key_spec[[key]], .na=NULL)
    # keyValues = keyValues %>% magrittr::set_attr("spec", key_spec[[key]])
    df = df %>% dplyr::mutate(!!newCol := as.character(keyValues))
  }
  return(df)
}

## Documentation utility ----

# depth first traverse
.find_list = function(ast) {
  if (is.call(ast) && ast[[1]]==as.name("list")) return(ast)
  if (length(ast)==1) return(NULL)
  for (i in 1:length(ast)) {
    ast2 = ast[[i]]
    tmp = .find_list(ast2)
    if (!is.null(tmp)) return(tmp)
  }
  return(NULL)
}

# Used internally to document a mapping function
.document_mapping = function(map_fn) {
  tmp = body(map_fn)
  # tmp2 = eval(tmp)
  tmp2 = .find_list(tmp)
  old = names(tmp2)
  # tmp3 = deparse1(tmp)
  tmp5 = tmp2[old!=""]
  old = old[old!=""]

  new = lapply(tmp5, all.vars) %>% sapply(paste0, collapse=", ")
  type = lapply(tmp5, all.names) %>% sapply(`[`, 1) %>% stringr::str_remove(".normalise_")

  return(sprintf("* %s -> %s (%s)",old,new,type) %>%
    paste0(collapse = "\n") %>%
    knitr::asis_output())
}

.document_data_format = function(df) {
  cls = sapply(df, function(x) class(x)[[1]])
  return(sprintf("* %s: %s",colnames(df),cls) %>%
           paste0(collapse = "\n") %>%
           knitr::asis_output()
         )
}

.document_new_columns = function(df_new, df_old) {
  new_cols = setdiff(colnames(df_new),colnames(df_old))
  .document_data_format(
    df_new %>% dplyr::select(tidyselect::all_of(new_cols))
  )
}

## Column specific mappers ----

.relocate_old = function(df, renameTo, valueCol) {
  valueCol = as.symbol(valueCol)
  renameTo = as.symbol(renameTo)
  df %>%
    dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
    dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
}

# Allows a
.normalise_variant = function(renameTo) {
  renameTo = rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    df %>% dplyr::mutate(!!renameTo := dplyr::case_when(
      !!valueCol %>% stringr::str_detect("(P|p).*(R|r)") ~ "Delta",
      !!valueCol %>% stringr::str_detect("(K|k).*(N|n)") ~ "Omicron",
      is.na(!!valueCol) ~ NA_character_,
      TRUE ~ "unknown"
    ) %>% factor(levels = c("unknown","Delta","Omicron"))) %>%
    .relocate_old(renameTo, valueCol)
  })
}

#TODO:
# .normalise_pneumo_serotype

# .sort_serotypes(pcv24_Affinivax)
# sorts into an ascending type/subtype text order e.g. 1,2A,2B,4,10B,11A
.serotype_levels = function(serotypes) {
  tmp = unique(serotypes)
  t1 = tmp %>% unique() %>% stringr::str_extract("^[0-9]*") %>% as.integer()
  t1 = ifelse(is.na(t1),100000,t1)
  t2 = tmp %>% unique() %>% stringr::str_remove_all("^[0-9]*") %>% rank()
  tmp[order(t1+t2/1000)]
}

.normalise_pneumo_serotype =  function(renameTo, levelFn = .serotype_levels) {
  levelFn = purrr::as_mapper(levelFn)
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    tmp = df %>% dplyr::pull(!!valueCol)

    # lots of possible formats to detect here:
    # linelist %>% dplyr::select(c(record_number, tidyselect::starts_with("pn_st"))) %>% dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>% tidyr::pivot_longer(tidyselect::starts_with("pn_st")) %>% dplyr::pull(value) %>% unique()
    # gives a list from the raw data.

    tmp = tmp %>%
      as.character() %>%
      stringr::str_match_all("(^|_|\\s)([0-9][0-9]?[A-Za-z]?)($|_|\\s)") %>%
      purrr::map_chr(~ ifelse(length(.x)==0,NA_character_,.x[[3]])) %>%
      toupper()
    # TODO: consider the need to merge serotypes like 6A/6C
    levels = levelFn(tmp)
    tmp = factor(tmp,levels = levels)
    df %>%
      dplyr::mutate(!!renameTo := tmp) %>%
      .relocate_old(renameTo, valueCol)
  })
}
# e.g. matches item 1,2,3 but not others
# tibble::tibble(s = c("PN_13B_UAD","1a", "15", "x1", "300", "NA","UNknown")) %>% .normalise_pneumo_serotype(serotype)("s")

.normalise_study_id = function(renameTo) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    tmp = df %>% dplyr::pull(!!valueCol)
    # convert numeric or unhyphenated number as string to XXXX-XXXX format
    if (is.numeric(tmp) || all(stringr::str_detect(tmp,"^[0-9]+$"))) {
      tmp = sprintf("%s-%s",
                    stringr::str_sub(as.character(tmp),1,4),
                    stringr::str_sub(as.character(tmp),5,-1)
      )
    }
    # Exclude anthng not matching XXXX-XXXX
    correct = stringr::str_detect(tmp,"^[0-9]{4}-[0-9]+$")
    if (!all(correct,na.rm = TRUE)) message("CAUTION: some consented study identifiers are incorrectly formatted. These have been set to NA.")
    tmp = ifelse(correct,tmp,NA_character_)
    df %>%
      dplyr::mutate(!!renameTo := tmp) %>%
      .relocate_old(renameTo, valueCol)
  })
}

# TODO: use labelled:: for column names

.normalise_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    # TODO: do we need to handle a NaN value (explicit unavailable) and if so how?
    df %>%
      dplyr::mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_yesno = function(renameTo) {
  #TODO: error checking
  .normalise_list({{renameTo}}, c("no","yes"), zeroValue=TRUE)
}

.normalise_yesno_unknown = function(renameTo) {
  #TODO: error checking
  .normalise_list({{renameTo}}, c("yes","no","unknown"))
}

.normalise_ppi = function(renameTo) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := sapply(!!valueCol, digest::digest)) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_name = function(renameTo) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := !!valueCol) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_integer = function(renameTo, limits=c(-Inf,Inf)) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo :=  suppressWarnings(as.numeric(!!valueCol))) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo != round(!!renameTo), NA_real_,!!renameTo)) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],NA_real_,!!renameTo)) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_pos_integer = function(renameTo, upper=Inf) {
  renameTo=rlang::ensym(renameTo)
  .normalise_integer(!!renameTo, limits = c(0,upper))
}

.normalise_double = function(renameTo, limits=c(-Inf,Inf)) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo :=  suppressWarnings(as.numeric(!!valueCol))) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],NA_real_,!!renameTo)) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_trunc_double = function(renameTo, lower, upper) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := dplyr::case_when(
        stringr::str_starts(!!valueCol, "<") ~ lower,
        stringr::str_starts(!!valueCol, ">") ~ upper,
        TRUE ~ suppressWarnings(as.numeric(!!valueCol))
      )) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],NA_real_,!!renameTo)) %>%
      .relocate_old(renameTo, valueCol)
  })
}



# ggplot2::diamonds %>% dplyr::mutate(cut = as.character(cut)) %>% .normalise_text_to_factor(new_cut, preprocess = ~ dplyr::case_when(.x=="Fair"~"XXX", TRUE~tolower(.x)))("cut")
.normalise_text_to_factor = function(renameTo, levels = NULL, preprocess = tolower, sorter = sort) {
  renameTo=rlang::ensym(renameTo)
  preprocess = purrr::as_mapper(preprocess)
  sorter = purrr::as_mapper(sorter)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    if (is.null(levels)) levels = df %>% dplyr::pull(!!valueCol) %>% stats::na.omit() %>% preprocess() %>% unique() %>% sorter()
    df %>%
      dplyr::mutate(!!renameTo := suppressWarnings(factor(preprocess(!!valueCol), levels=preprocess(levels), labels=levels))) %>%
      .relocate_old(renameTo, valueCol)
  })
}

# ggplot2::diamonds %>% dplyr::mutate(cut = as.character(cut)) %>% .normalise_text_to_factor(new_cut, preprocess = ~ dplyr::case_when(.x=="Fair"~"XXX", TRUE~tolower(.x)))("cut")
.normalise_text = function(renameTo, preprocess = tolower) {
  renameTo=rlang::ensym(renameTo)
  preprocess = purrr::as_mapper(preprocess)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := suppressWarnings(preprocess(!!valueCol))) %>%
      .relocate_old(renameTo, valueCol)
  })
}

.normalise_date = function(renameTo, limits=as.Date(c("2000-01-01","2030-01-01")), tryFormats="%d/%m/%Y") {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo :=  suppressWarnings(as.Date(!!valueCol, tryFormats = tryFormats))) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],as.Date(NA),!!renameTo)) %>%
      .relocate_old(renameTo, valueCol)
  })
}



.normalise_checkboxes = function(renameToVars) {
  return(function(df, valueColPrefix) {
    i=1
    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = rlang::as_label(naCol) %in% colnames(df)
    for(renameTo in renameToVars) {
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",i))
      renameTo = as.symbol(rlang::as_label(renameTo))
      message("mapping ",valueCol," to ",renameTo)
      # rename original ___1, ___2, etc to something meaningful and convert to ordered factor
      df = df %>% dplyr::mutate(!!renameTo := !!valueCol)
      if(hasNa) {
        # deal with ___na columns etc, by making the renamed checkbox variables have an NA in them for the values where NA has been checked
        # TODO: maybe this shoudl be a NaN and we handle this as a known unknown?
        df = df %>% dplyr::mutate(!!renameTo := dplyr::if_else(!!naCol == 1, NA_real_, !!renameTo))
      }
      df = df %>%
        dplyr::mutate(!!renameTo := !!renameTo %>% factor(levels=c(0,1), labels=c("no","yes"))) %>%
        .relocate_old(renameTo, valueCol)
      #TODO: validation checking?
      i=i+1
    }
    # hide original ___na columns
    if(hasNa) df = df %>% dplyr::rename(!!(paste0(".",naCol)) := (!!naCol))

    return(df)


  })
}

.normalise_checkboxes_to_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=rlang::ensym(renameTo)
  # return(function(df, valueCol) {
  #   valueCol = as.symbol(valueCol)
  #   message("mapping ",valueCol," to ",renameTo)
  #   df %>%
  #     dplyr::mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
  #     dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  # })

  return(function(df, valueColPrefix) {

    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = rlang::as_label(naCol) %in% colnames(df)

    df = df %>% dplyr::mutate(!!renameTo := methods::as(NA,class(values)))

    for(i in 1:length(values)) {
      value = values[i]
      code = codes[i]
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",code))
      message("mapping ",valueCol," to ",renameTo,", value ",value)
      df = df %>% dplyr::mutate(!!renameTo := ifelse(!!valueCol == 1 & is.na(!!renameTo),value,!!renameTo))
      # TODO: maybe this shoudl be a NaN and we handle this as a known unknown?
      # no need to deal with ___na columns etc as columns start with NA. no way to tell missing from present but NA.
      # hide original ___1, ___2, etc columns
      df = df %>% dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
      #TODO: validation checking?
    }

    # hide original ___na columns
    if(hasNa) df = df %>% dplyr::rename(!!(paste0(".",naCol)) := (!!naCol))
    df = df %>% dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueColPrefix))
    return(df)

  })
}


.normalise_checkboxes_to_nested_list = function(renameTo, values, nameCol = "name", valueCol = "value") {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueColPrefix) {

    valueColPrefix = paste0(valueColPrefix,"___")
    # which columns start with the prefix?
    codes = colnames(df)[colnames(df) %>% stringr::str_starts(valueColPrefix)]
    # identify na columns
    naCol = paste0(valueColPrefix,"na")
    hasNa = naCol %in% colnames(df)
    codes = setdiff(codes, naCol)
    codemap = tibble::tibble(!!nameCol := factor(values,levels = values)) %>% dplyr::mutate(.code = paste0(valueColPrefix, dplyr::row_number()))
    if (length(codes) != length(values)) stop("identified ",length(codes)," matching columns for `",valueColPrefix,"` but ",length(values)," values supplied")

    # subset dataframe to columns of interest excluding na:
    tmp = df %>% dplyr::select(tidyselect::all_of(codes)) %>%
      dplyr::mutate(.id = dplyr::row_number())

    if (hasNa) {
      # If there is a naCol alter each column setting as NA if the naCol has value 1
      naCol = as.symbol(naCol)
      nas = (df %>% dplyr::pull(!!naCol)) == 1
      tmp = tmp %>% dplyr::mutate(dplyr::across(tidyselect::all_of(codes), .fns = ~ ifelse(nas, NA_real_, .x)))
    }

    tmp = tmp %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(codes), names_to = ".code", values_to = ".value") %>%
      dplyr::mutate(!!valueCol := factor(.value,levels=c(0,1), labels=c("no","yes"))) %>%
      dplyr::select(-.value) %>%
      dplyr::inner_join(
        codemap, by = ".code"
      ) %>% dplyr::select(-.code) %>%
      tidyr::nest(.new_data = -.id)

    df %>% dplyr::mutate(!!renameTo := tmp %>% dplyr::pull(.new_data)) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",codes)) %>%
      dplyr::rename_with(.cols = tidyselect::starts_with(valueColPrefix), ~ paste0(".",.x))


  })
}
