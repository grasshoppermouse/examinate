
library(tidyverse)

#' @title qnames
#' @description Generate question variable names (e.g., q01, q02, ...)
#' @param n Number of questions
#' @return A character vector of question variable names, from q01...qn
#' @details An internal utility function.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  qnames(25)
#'  }
#' }
#' @rdname qnames
#' @export
qnames <- function(n){
    paste0(rep('q', n), sprintf('%02d', 1:n))
}

#' @title roster_to_df
#' @description Imports the WSU portal roster file.
#' @param filename The roster file name, Default: 'roster.xlsx'
#' @param out The output file name of the converted tsv file, Default: 'roster.tsv'
#' @return A data frame with student ids, names, and email addresses.
#' @details The WSU portal roster file is downloaded in an HTML table but
#'   with an xls or xlsx file extension (Excel).
#' @examples
#' \dontrun{
#' if(interactive()){
#'  roster_to_df()
#'  }
#' }
#' @rdname roster_to_df
#' @export
#' @importFrom dplyr select
#' @importFrom readr write_tsv
#' @importFrom stringr str_sub
#' @importFrom xml2 read_html
roster_to_df <- function(filename = 'roster.xlsx', out = 'roster.tsv') {
    require(dplyr)
    require(tidyr)
    if (!file.exists(filename)){
        stop("Roster file does not exist.", call. = F)
    }

    # Old code to import roster file downloaded from WSU website
    # roster <- XML::readHTMLTable(filename, as.data.frame = F)[[1]]
    # roster <- as.data.frame(roster, stringsAsFactors = F)

    roster <-
        xml2::read_html(filename) %>%
        rvest::html_node("table") %>%
        rvest::html_table() %>%
        dplyr::select(ID, Name, Email) %>%
        tidyr::separate(Name, c("Last_name", "First_name"), sep = ",") %>%
        dplyr::rename(STUDENT_ID = ID) %>%
        mutate(
            STUDENT_ID = as.character(STUDENT_ID),
            STUDENT_ID = stringr::str_pad(STUDENT_ID, width = 9, pad = '0')
        )

    if (!is.null(out)) {
        readr::write_tsv(roster, out)
    }
    return(roster)
}

#' @title import_formreturn
#' @description Import the csv file output by FormReturn into a data frame.
#' @param exported_data The filename of the FormReturn csv file, Default: 'captured_data_export.csv'
#' @param number_of_questions Number of exam questions.
#' @return A named list. "key" is the answer key as a character vector. "captured" is
#'  a data frame with STUDENT_ID's and responses to each question.
#' @details FormReturn generates a csv file with the scanned student IDs and exam responses.
#'   This function imports that file into a data frame. It identifies the key, which should
#'   have STUDENT_ID = "00000000", and exports it in a named list as a character vector named "key".
#'   The student responses are in a data frame named "captured".
#' @examples
#' \dontrun{
#' if(interactive()){
#'  import_formreturn(number_of_questions=25)
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{filter}}

#'  \code{\link[readr]{read_tsv}}
#' @rdname import_formreturn
#' @export
#' @importFrom dplyr rename filter
#' @importFrom readr read_tsv
import_formreturn <- function(exported_data = 'captured_data_export.csv', number_of_questions) {
    captured <- readr::read_tsv(exported_data)
    qnms <- qnames(number_of_questions)

    if (any(captured$STUDENT_ID == '00000000')){
        key <-
            as.character(captured[captured$STUDENT_ID == '00000000', qnms])
    } else {
        key <- ""
        warning("Missing key ID: 00000000")
    }

    captured <-
        captured %>%
        dplyr::rename(IMAGE = page1_image_file_name) %>%
        dplyr::filter(STUDENT_ID != '00000000')

    captured <- captured[c('STUDENT_ID', 'IMAGE', qnms)]

    return(list(key = key,
                captured = captured))
}

#' @title import_exams
#' @description Import scanned exams (as png files) and produce a key (character vector)
#'  and student answers (data frame).
#' @param scan_directory The directory with the scanned exams in png format, Default: 'exams'
#' @param number_of_questions Number of questions on the exam.
#' @return A named list. "key" is a chacter vector of correct answers; "captured" is a
#'   data frame of student answers.
#' @details A special "bubble sheet" from the exams package should be used. Scan these and
#'   generate a separate png file for each one. This function will read every file in the
#'   specified directory and generate a named list with "key", a character vector of correct answers
#'   corresonding to student ID = "000000000", and "captured", the student answers in a data frame.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  import_exams(number_of_questions=25)
#'  }
#' }
#' @seealso

#'  \code{\link[exams]{nops_scan}}

#'  \code{\link[parallel]{detectCores}}

#'  \code{\link[purrr]{imap_chr}}

#'  \code{\link[stringr]{str_split}},\code{\link[stringr]{str_c}}
#' @rdname import_exams
#' @export
#' @importFrom exams nops_scan
#' @importFrom parallel detectCores
#' @importFrom purrr imap_chr
#' @importFrom stringr str_split str_c
import_exams <- function(scan_directory = 'exams', number_of_questions, threshold = c(0.1, 0.5)) {
    qnms <- qnames(number_of_questions)

    tmp <-
        exams::nops_scan(
            dir = scan_directory,
            density = 200,
            threshold = threshold,
            minrot = 0.002
            # cores = parallel::detectCores()
            )

    # return(tmp)

    file.remove(dir(
        pattern = "\\.zip$",
        path = scan_directory,
        full.names = T
    ))

    # Check for errors
    errors = F
    for (exam in tmp){
        if (stringr::str_detect(stringr::str_to_lower(exam), "error")){
            cat(paste(exam, '\n'))
            errors = T
        }
    }

    if (errors) stop("Error scanning one or more exams", call. = F)

    tmp <- t(sapply(
        tmp,
        FUN = function(x)
            stringr::str_split(x, ' ')[[1]]
    ))

    tmp <- tmp[,1:(number_of_questions + 6)]

    rownames(tmp) <- NULL

    colnames(tmp) <-
        c(
            'IMAGE',
            'testcode1',
            'testcode2',
            'testcode3',
            'testcode4',
            'STUDENT_ID',
            qnms
        )

    # return(tmp)

    # Convert the "binary" format to A, B, etc.
    bin2letters <- function(x) {
        if (x == "00000")
            return("")
        v <- as.integer(stringr::str_split(x, pattern = "")[[1]])
        w <-
            purrr::imap_chr(v, ~ ifelse(.x, c("A", "B", "C", "D", "E")[.y], NA))
        stringr::str_c(na.omit(w), collapse = ",")
    }

    tmp[, qnms] <-
        apply(tmp[, qnms], 2, function(x)
            sapply(x, bin2letters, USE.NAMES = F))
    tmp <-
        as.data.frame(tmp[, c('STUDENT_ID', 'IMAGE', qnms)], stringsAsFactors = F)

    key_v <- tmp$STUDENT_ID == '000000000' & stringr::str_detect(tmp$IMAGE, '-0.')

    if (any(key_v)){
        key <- tmp[key_v, ]
        key <- as.character(key[qnms])
        names(key) <- qnms
        tmp <- tmp[!key_v, ]

        if(sum(key == "")) warning("Key has blank answer(s)")
    } else {
        key <- ""
        warning("Missing key ID 000000000, or no exam-0.png")
    }

    return(list(key = key,
                captured = tmp))
}

# Manually fix student ids, blanks, and multiple marks

#' @title display_scantron
#' @description Utility function to display the scanned exam (in png format) in the Rstudio viewer.
#' @param scanfilename Filename of scanned image (png format)
#' @param imagedir Directory of scanned exam images, Default: 'exams'
#' @return None. Used for side effect of image display.
#' @details Displays scanned image of exam.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  display_scantron("exam03.png")
#'  }
#' }
#' @rdname display_scantron
#' @export

display_scantron <- function(scanfilename, imagedir = 'exams') {
    viewer <- getOption("viewer")
    test_png <- file.path(tempdir(), "test.png")
    if (file.exists(test_png)) {
        file.remove(test_png)
    }
    file.copy(file.path(imagedir, scanfilename), test_png)
    viewer(test_png)
}

#' @title fix_problems
#' @description Manually fix any problems with unknown student ID numbers,
#'   possibly blank answers, or multiple marks.
#' @param captured A data frame of all student answers, as produced by, e.g., import_exams.
#' @param key A character vectors of correct answers, as produced by, e.g., import_exams.
#' @param roster The student roster in a data frame, Default: roster_to_df()
#' @param number_of_problems Number of exam questions.
#' @param out Filename for corrected tsv file, Default: 'captured.corrected.tsv'
#' @return A data frame with the corrected student ID's and answers.
#' @details Scanned exams often have artifacts, and students frequently enter an incorrect ID number.
#'   This function identifies student IDs that do not appear in the roster, double-checks that blank
#'   answers are actually blank, and allows the instructor to choose one among multiple marks.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  fix_problems(captured, number_of_problems=25)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}

#'  \code{\link[readr]{write_tsv}}
#' @rdname fix_problems
#' @export
#' @importFrom glue glue
#' @importFrom readr write_tsv
fix_problems <- function(captured, key, roster = roster_to_df(), imagedir = "exams", out = 'captured.corrected.tsv') {

    if (!dir.exists(imagedir)){
        stop('No directory named: ', imagedir, call. = F)
    }

    if ("" %in% key){
        stop("Key has blank answers", call. = F)
    }

    number_of_problems <- length(key)
    qnms <- qnames(number_of_problems)
    ans <- 1
    for (i in 1:nrow(captured)) {

        if (ans == 0) break

        cat(glue::glue("Processing scantron {i}. \n\n"))

        wrong_id <- !(captured$STUDENT_ID[i] %in% roster$STUDENT_ID)
        blanks <- sort(c(which(is.na(captured[i, qnms])), which(captured[i, qnms] == "")))

        if (wrong_id | length(blanks)) {
            display_scantron(captured$IMAGE[i], imagedir = imagedir)
        }

        while(wrong_id) {
            cat(paste0('Incorrect ID: ', captured$STUDENT_ID[i]))
            cat("\nEnter correct ID (or 0 to skip, or -1 to stop) :")
            id <- readline()
            if (id == -1){
                cat("\n\nStopping")
                return()
            }
            if (id == 0) break
            if (id %in% roster$STUDENT_ID){
                wrong_id <- F
                captured$STUDENT_ID[i] <- id
            }
        }

        if (length(blanks)) {
            for (j in blanks) {
                cat(glue::glue("Q{j} Blank? "))
                cat("Enter letter, or return for blank:")
                ans <- readline()
                if (ans == 0)
                    break
                captured[i, j + 2] <- ans
            }
        }

        mult_marks <-
            which(!captured[i, qnms] %in% c('A', 'B', 'C', 'D', 'E', ''))

        if (length(mult_marks)) {
            display_scantron(captured$IMAGE[i], imagedir = imagedir)
            for (j in mult_marks) {
                cat(
                    glue::glue(
                        "Q{j} multiple marks: {captured[i,j+2]}.\nEnter correct answer:"
                    )
                )
                ans <-
                    toupper(readline()) # readLines(con=stdin(),1)
                if (ans == 0)
                    break
                captured[i, j + 2] <- ans
            }
        }
    }

    # Compute score
    captured$Score <- score_exam(captured, key, cols = qnms)

    # Write to tsv
    readr::write_tsv(captured, out)

    # Merge with roster
    captured_short <- dplyr::left_join(roster, captured[c('STUDENT_ID', 'Score')])

    # Write scores
    readr::write_tsv(captured_short, "scores.tsv")

    return(captured)
}


#' @title score_exam
#' @description compute scores by comparing answers to key
#' @param captured A data frame of answers
#' @param key A one-row data frame with the key.
#' @param cols The column names of the questions
#' @return A numeric vector of number correct
#' @details A utility function, mostly for internal use, that computes number of correct based on the key.
#' @rdname score_exam

score_exam <- function(captured, key, cols){

    # apply(captured[cols], 1, function(x)
    #     sum(x == key))
    apply(captured[cols], 1, function(x)
        sum(str_detect(x, key)))
}

#' @title Create key
#' @description Create key vector from test key
#' @param fn File name of test with answers marked with *
#' @return A character vector of answers (e.g., DBBACDA...)
#' @details Reads an Rmd exam with answers marked with *, and creates a key vector
#' @rdname create_key
#' @export

create_key <- function(fn){
    conn <- file(fn,open="r")
    linn <-readLines(conn, warn = F)
    close(conn)
    key <- character(0)
    for (i in 1:length(linn)){
        if(stringr::str_detect(linn[i], "a\\. \\*")) key <- c(key, 'A')
        if(stringr::str_detect(linn[i], "b\\. \\*")) key <- c(key, 'B')
        if(stringr::str_detect(linn[i], "c\\. \\*")) key <- c(key, 'C')
        if(stringr::str_detect(linn[i], "d\\. \\*")) key <- c(key, 'D')
        if(stringr::str_detect(linn[i], "e\\. \\*")) key <- c(key, 'E')
    }
    if (length(key) == 0) stop('Key is of length 0. Are you sure this is a key file?')
    names(key) <- qnames(length(key))
    return(key)
}



#' open_scantron
#'
#' Opens the scantron PDF for printing
#'
#'
#' @examples
#' open_scantron()
#' @export
open_scantron <- function(){
    system2('open', system.file("scantron.pdf", package = "examinate"))
}
