library(haven)
library(readxl)
library(data.table)
library(dplyr)
library(readr)
library(sjlabelled)
library(stringr)
library(foreign)

options(scipen = 999)
#library(msgxtractr) Til outlook-mails. Endnu ikke implementeret (men kan gåres relativt let).
#Overbygning af outlook, docx, R og html

#Function to read in all sheets of excel-document
excel_allsheets_func <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) tryCatch(readxl::read_excel(filename, sheet = X, col_names = F), error=function(e) NULL))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

read_excel_files <- function(x) {
  tryCatch(
      return(excel_allsheets_func(x)),
      error = function(e) {return(as.data.frame("Kan ikke åbne filen"))}
    )
}

#Function to read CSV
read_csv_files <- function(x) {
  tryCatch(
    return(read.delim(x,stringsAsFactors =F, header = F, quote = "")),
    error = function(e) {return(as.data.frame("Kan ikke åbne filen"))}
  )
}


#Small helper funcs
which_true_func <- function(x) {
  return(which(x==T))
}

return_keys <- function(x,y) {
  x <- as.list(x)
  for (i in 1:length(x)) {
    x[[i]] <- y[x[[i]]]
  }
  x <- unlist(lapply(x, paste, collapse = ","))
  return(x)
}

hl_func <- function(x) {ifelse(x=="",unique(x)[unique(x)!=""],x)}

#Function to search for matches in SPSS & STATA datasets
sav_func <- function(x) {
  if (dim(x)[1] == 0) {
    content <- 0
    names <- 0
  } else if (c("Kan ikke åbne filen") %in% x[1,1]) {
    content <- 0
    names <- 0
  } else {
  names <- unique(ceiling(str_which(rbind(names(x),get_label(x)), regex(paste(keywords_header,collapse = "|"), ignore_case = T))/2))
  if (length(names)>0) {
    if (length(apply(outer(tolower(names(x)[names]),keywords_header,str_detect),1,which_true_func))>0) {
    head <- return_keys(apply(outer(tolower(names(x)[names]),keywords_header,str_detect),1,which_true_func), keywords_header)
    } else {head <- ""}
    if (length(apply(outer(tolower(get_label(x)[names]),keywords_header,str_detect),1,which_true_func))>0) {
  label <- return_keys(apply(outer(tolower(get_label(x)[names]),keywords_header,str_detect),1,which_true_func), keywords_header)
    } else {label <- ""}
  match_names <- paste("I kolonnenavnet (", names,"): ", paste(unique(apply(rbind(head,label),2,hl_func))), sep = "")
  } else {
  match_names <- ""
  }
  if (dim(x)[1] > 1) {
  content <- which(colSums(apply(x, 2, str_detect, pattern = regex(paste(keywords,collapse = "|"), ignore_case = T)), na.rm=T)>0)
  } else {
    content <- NULL
  }

  if (length(content)>0) {
    content_match <- NULL
    for (i in 1:length(keywords)) {
      indice <- which(colSums(apply(x, 2, str_detect, pattern = regex(keywords[i], ignore_case = T)), na.rm=T)>0)
      if (length(indice)>0) {
      content_match <- rbind(content_match,c(keywords[i],indice))
      }
    }

  match_indhold <- paste("Indhold i kollonne (nr.", content_match[,2],"): ", paste(content_match[,1]), sep = "")
  } else {
    match_indhold <- ""
  }
  }

  if (sum(names) > 0 & sum(content) > 0) {
    paste(c(match_names, match_indhold), collapse = ", ")
  } else if (sum(names) == 0 & sum(content) > 0) {
    paste(c(match_names, match_indhold), collapse = ", ")
  } else if (sum(names) > 0 & sum(content) == 0) {
    paste(c(match_names, match_indhold), collapse = ", ")
  } else if (sum(names) == 0 & sum(content) == 0) {
    paste("OK :-)")
  }
}

#Function to search for matches in text, csv, do and sps files
csv_func <- function(x) {
  csv_match <- grep(paste(keywords,collapse = "|"),x, ignore.case = T)
  if (sum(csv_match) > 0) {
    matches <- NULL
    for (i in 1:length(keywords)) {
      matches <-rbind(matches,ifelse(length(grep(keywords[i],x, ignore.case = T))>0,keywords[i],""))
    }
    paste("Keyword matches: ", paste(matches[matches!=""],collapse = ", "))
  } else {
    paste("OK :-)")
  }
}

#Functions to search for matches in excel files (and their sheets)
sheet_func <- function(x) {
  match_ind <- unique(ceiling(grep(paste(keywords,collapse = "|"),as.matrix(x), ignore.case = T)/nrow(x)))
    if (length(match_ind) > 0) {
      match_sheet <- NULL
      for (i in 1:length(keywords)) {
        mc <- unique(ceiling(grep(keywords[i],as.matrix(x), ignore.case = T)/nrow(x)))
        if (length(mc)>0) {
        match_sheet <- rbind(match_sheet,c(keywords[i],mc))
        }
      }

  paste0("(Kolonne ", c(t(match_sheet[,-1])),"): ", paste(match_sheet[,1]), collapse = ", ")
  }
}

xls_func <- function(x) {

  sheet_match <- lapply(x,sheet_func)
  sheet_match <- sheet_match[which(lapply(sheet_match, is.null)==F)]

  if (length(sheet_match) > 0) {
    paste("I Excelark:",names(sheet_match)," ::: ", lapply(sheet_match, paste, collapse = ", "), collapse = " // ")
  } else {
    paste("OK :-)")
  }
}


#Read in SPSS files
read_spss_files <- function(x) {
  tryCatch(
    {
    return(read_sav(x))
    }, error = function(e) {
      return(suppressMessages(read.spss(x)))
    }, error = function(e) {
    return(as.data.frame("Kan ikke åbne filen"))
    })
}


#Matching function
match_func <- function(path, return = "problems") {
  #Find all files of specified types in path folder
  files_types <- list()
  filelist <- list.files(path = path, pattern = "\\.(csv|xls|xlsx|xlsm|sps|sav|do|dta)$", full.names = T, ignore.case = T, all.files = T, recursive = T)


  #
  if (length(filelist>0)) {

    files_types[[1]] <- filelist[grep(pattern = "\\.(csv|txt|sps|do)", filelist, ignore.case = T)]
    files_types[[2]] <- filelist[grep(pattern = "\\.(xls|xlsx|xlsm)", filelist, ignore.case = T)]
    files_types[[3]] <- filelist[grep(pattern = "\\.sav", filelist, ignore.case = T)]
    files_types[[4]] <- filelist[grep(pattern = "\\.dta", filelist, ignore.case = T)]


    files_csv <- lapply(files_types[[1]], read_csv_files)
    files_xls <- suppressMessages(lapply(files_types[[2]], read_excel_files))
    files_sav <- lapply(files_types[[3]], function(x) as.data.frame(read_spss_files(x), stringsAsFactors = F))
    files_dta <- lapply(files_types[[4]], function(x) as.data.frame(read_stata(x), stringsAsFactors = F))

    matches <- list(
      "CSV" = as.data.frame(cbind("Fil:" = paste(files_types[[1]]),"Hvad er fundet?" = unlist(lapply(files_csv,csv_func))), stringsAsFactors = F),
      "XLS" = as.data.frame(cbind("Fil:" = paste(files_types[[2]]),"Hvad er fundet?" = unlist(lapply(files_xls,xls_func))), stringsAsFactors = F),
      "SAV" = as.data.frame(cbind("Fil:" = paste(files_types[[3]]),"Hvad er fundet?" = unlist(lapply(files_sav,sav_func))), stringsAsFactors = F),
      "DTA" = as.data.frame(cbind("Fil:" = paste(files_types[[4]]),"Hvad er fundet?" = unlist(lapply(files_dta,sav_func))), stringsAsFactors = F)
    )
    Matches_all <- bind_rows(matches)
    Matches_problematic <- Matches_all[Matches_all[,2] != "OK :-)",]


    #Filename search
    problematic_filnames <- list.files(path = path, pattern = paste(keywords_names,collapse = "|"), full.names = T, ignore.case = T, all.files = T, recursive = T)
    if (length(problematic_filnames>0)) {
      problematic_filnames <- cbind(problematic_filnames,
                                    paste("Match i filnavn:", return_keys(apply(outer(tolower(problematic_filnames),keywords_names,str_detect),1,which_true_func), keywords_names)))
      colnames(problematic_filnames) <- names(Matches_problematic)
      Matches_problematic <- rbind(problematic_filnames, Matches_problematic)
    }



    if (return == "all") return(Matches_all)
    else if (return == "problems") {
      return(Matches_problematic)
    }
  } else {
  paste("Ingen matches fundet.")
}
}




