redcap_import_sepmerge <- function(input_data,
                                   dict = NULL,
                                   rc_token,
                                   rc_url) {


  # evaluate inputs ----

  # data
  check_data(input_data)


  # dict

  if(is.null(dict)) {
    dict_downloaded <- TRUE
    check_token(rc_token)
    check_url(rc_url)

    meta <- redcap_export_meta(rc_token, rc_url)
    dict <- meta$meta
    missing_codes <- meta$project$missing_data_codes

  } else {
    dict_downloaded <- FALSE
  }

  check_dict(dict)

# intro ----
#TODO


    name_vars <- colnames(input_data)


  cat("What would you like to do?")
  cat("\n 1 = Separate one variable into two or more")
  cat("\n 2 = Merge two or more variable into one")
  what_ans <- ""

  while (what_ans != '1' &
         what_ans != '2') {

    what_ans <- readline(prompt="Answer: ")

    if (what_ans != '1' &
        what_ans != '2') {

      cat("Please check your answer!")
      cat("\n 1 = Separate one variable into two or more")
      cat("\n 2 = Merge two or more variable into one")
      what_ans <- ""
    }
  }

  if (what_ans == 1) {
    cat("Which variable would you like to separate?\n\n")
    cat(name_vars,sep = "\n")
    sep_which_ans <- ""

    while (!any(grepl(paste0("^",sep_which_ans,"$"),name_vars))) {

      sep_which_ans <- readline(prompt="Answer:  ")

      if (!any(grepl(paste0("^",sep_which_ans,"$"),name_vars))) {
        cat("Variable name not recognized: Please try again!")
      }
    }
  }

  var <- input_data |>
    pull(sep_which_ans)

  cat(paste0("\n\nVariable Label: ",italic(sep_which_ans)))
  cat("\n\nData Summary:")
  summary_table <- data.frame(Value = names(summary(as.factor(var))),
                              Count = as.numeric(summary(as.factor(var))))
  print(kable(summary_table))
  cat(str(var))

  cat("\n\n")
  cat("How should this variable be separated?\n")
  cat('Please define all delimiters in a character vector.\nE.g., c(", ", " ", " (")\n')
  sep_delim_ans <- ""
  sep_delim_invalid <- TRUE

  while (sep_delim_invalid) {

    sep_delim_ans <- readline(prompt="Answer: ")

    if (!grepl("^c\\(.*\\)$", sep_delim_ans)) {
      cat("Please check your answer!\n")
      cat('Please define all delimiters in a character vector.\nE.g., c(", ", " ", " (")\n')
      sep_delim_ans <- ""
    }

    if (grepl("^c\\(.*\\)$", sep_delim_ans)) {
      sep_delims <- eval(parse(text = sep_delim_ans))

      if (!is.character(sep_delims)) {
        cat("Please check your answer!\n")
        cat('Please define all delimiters in a character vector.\nE.g., c(", ", " ", " (")\n')
        sep_delim_ans <- ""
      } else {
        sep_delim_invalid <- FALSE
      }
    }
  }

  sep_delims <- gsub("([\\|().])", "\\\\\\1", sep_delims)

  for (sep in sep_delims) {
    var <- gsub(sep, "split_here",var)
  }



  cat("\n\n")
  cat("Variable names of new variables:\n")
  cat('Please define new names in a character vector.\nE.g., c("var1", "var2", var3")\n')
  sep_names_ans <- ""
  sep_names_invalid <- TRUE

  while(sep_names_invalid) {

    sep_names_ans <- readline(prompt="Answer: ")

    sep_names_error <- ""
    tryCatch({
      new_vars <- eval(parse(text = sep_names_ans))
    }, error = function(e) {
      sep_names_error <<- "yes"
    })

    if (sep_names_error == "yes") {
      cat("\nPlease check your answer!\n")
      cat('Please define new names in a character vector.\nE.g., c("var1", "var2", var3")\n')
      sep_names_ans <- ""

    } else {
      new_vars <- eval(parse(text = sep_names_ans))


      tryCatch({
        separated_data <- as.data.frame(var) |>
          separate_wider_delim(var, delim = "split_here", names = new_vars)
      }, error = function(e) {
        if (any(grepl("too_few", e$body))) {
          sep_names_error <<- "too few"
        }
      })

      if (sep_names_error == "too few") {
        cat("\nNot all elements can be split. Decide where NA's should be added.\n")
        cat("left = NA's will be added to the left-hand variable(s)\n")
        cat("right = NA's will be added to the right-hand variable(s)\n")
        too_few_ans <- ""

        while (too_few_ans != "left" &
               too_few_ans != "right") {

          too_few_ans <- readlines(prompt = "Answer: ")

          if (too_few_ans != "left" &
              too_few_ans != "right") {

            cat("\nPlease check your answer!\n")
            cat("left = NA's will be added to the left-hand variable(s)\n")
            cat("right = NA's will be added to the right-hand variable(s)\n")

          }
        }

        if (too_few_ans == "left") {
          sep_align <- "align_end"
        } else if (too_few_ans == "right") {
          sep_align <- "align_start"
        }

        # hier versuchen den neuen code mit separate einzubauen und auf too_few = align_end/start einzugehen,
        # dann die schlaufe einfügen, was geschehen soll wenn too_many (muss etwas anderes eingeben!)
        # vielleicht ist das doch zu kompliziert und doof und es wäre besser align_end als default zu setzen
        # und nur die too_many zu checken

        # neuer code hier???


      }

      # oder hier???

     if (any(grepl("too_many", e$body))) {
      sep_names_error <<- "too many"
    }


      if (sep_names_error == "too many") {
        cat("\nPlease check your answer!\n")
        cat("Not enough variable names have been defined.\n")
        cat('Please define new names in a character vector.\nE.g., c("var1", "var2", var3")\n')
        sep_names_ans <- ""
      } else {
        sep_names_invalid <- FALSE
      }



    }




    }


  separated_data <- as.data.frame(var) |>
    separate_wider_delim(var, delim = "split_here", names = new_vars,too_few = "align_start")

  for (name in names(separated_data)) {

    sumvar <- separated_data |>
      pull(name)

    summary_table <- data.frame(Value = names(summary(as.factor(sumvar))),
                                Count = as.numeric(summary(as.factor(sumvar))))
    cat(name)
    print(kable(summary_table))

  }







}
