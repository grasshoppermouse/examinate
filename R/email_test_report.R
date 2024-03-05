library(tidyverse)

#' @title matrix_to_ascii
#' @description A utility function to produce an ascii representation of a matrix for inclusion in
#'   the report emailed to students.
#' @param x The matrix with columns: Question #, Answer, Key
#' @return A string representation of the matrix.
#' @details The email report is text-only. This function is used to include a table of incorrect responses.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  matrix_to_ascii(mat) # matrix with columns: Question #, Answer, Key
#'  }
#' }
#' @rdname matrix_to_ascii
#' @export

matrix_to_ascii <- function(x){
  if (nrow(x) == 0) return('All answers were correct!')
  txt_tbl <- "Question  Your answer  Key"
  for (i in 1:nrow(x)){
    txt_tbl <- paste(txt_tbl, "\n", x[i,1], "           ", x[i,2], "       ", x[i,3])
  }
  return(txt_tbl)
}

#' @title email_report
#' @description Emails a report of exam results to each student
#' @param scores A data frame with student ID's and exam scores.
#' @param key A character vector of correct answers.
#' @param roster The roster data frame.
#' @param subject A string with the email subject line.
#' @param from "from" email address, Default: 'edhagen@wsu.edu'
#' @param imagedir A string specifying the directory with the exam images.
#' @param include_exam Include the student's exam. Logical.
#' @param dry_run Print the body of the emails, but don't actually send them.
#' @return None. Called for side effect of emailing reports.
#' @details Run this function last, after fixing all problems and checking the key.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  email_report(captured, key)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue_data}}

#'  \code{\link[mailR]{send.mail}}

#'  \code{\link[stringr]{str_split}}
#' @rdname email_report
#' @export
#' @importFrom glue glue_data
#' @importFrom stringr str_split
email_report <- function(scores, key, roster, subject, from = 'edhagen@wsu.edu', imagedir = 'exams', include_exam = T, dry_run = T){

    cat(getwd())

    wd_correct <- readline(prompt = "Is the working directory correct (y/n)? ")
    if (wd_correct != 'y'){
        stop("Incorrect working directory", call. = F)
    }

    if (include_exam & !dir.exists(imagedir)){
        stop('No directory named: ', imagedir, call. = F)
    }

    scores <- dplyr::left_join(scores, roster)

    if (any(is.na(scores$Email))){
        stop('Missing email address(es)', call. = F)
    }

    email_template <-
    "
    {{first_name}} {{last_name}},

    Your test score was:

    {{score}}

    The class stats were

    Maximum score: {{max}}
    Average score: {{mean}}
    Minimum score: {{min}}

    The approximate grade cutoffs are:

    A: {A_range}
    B: {B_range}
    C: {C_range}
    D: {D_range}

    Your incorrect answers:

    {{incorrect}}

    "

    grade_ranges <-
        list(
            "A_range" = readline(prompt = "A range: "),
            "B_range" = readline(prompt = "B range: "),
            "C_range" = readline(prompt = "C range: "),
            "D_range" = readline(prompt = "D range: ")
        )

    email_template <-
        glue::glue_data(grade_ranges, email_template)

    username = stringr::str_split(from, "@")[[1]][1]

    cat("\nEnter WSU password for:", username)
    password <- readline()

    d <- scores

    qnms <- qnames(length(key))

    # quantile(d$SCORE, probs=c(.05, .10, .55, .85, 1))
    # table(d$SCORE)

    # Create server
    wsuserver <- emayili::server(
      host = 'smtp.wsu.edu',
      username = username,
      password = password,
      port = 465
    )

    for (i in 1:nrow(d)){

        answer <- as.character(d[i, qnms])
        incorrect <-
            cbind(qnms[key != answer], answer[key != answer], key[key != answer])

        colnames(incorrect) <- c('Question', 'Your answer', 'Key')

        test_data = list(
            first_name = d$First_name[i],
            last_name = d$Last_name[i],
            score = d$Score[i],
            max = max(d$Score),
            mean = round(mean(d$Score), 1),
            min = min(d$Score),
            incorrect = matrix_to_ascii(incorrect)
        )

        body <- glue::glue_data(test_data, email_template)
        imagepath <- if(include_exam){
            file.path(imagedir, d$IMAGE[i])
        } else {
            NULL
        }
        to = d$Email[i]

        if (dry_run){
            cat(body, '\n')
        } else {

          msg <- emayili::envelope() %>%
            emayili::from(from) %>%
            emayili::to(to) %>%
            emayili::subject(subject) %>%
            emayili::body(body) %>%
            emayili::attachment(imagepath)

          tryCatch({
              wsuserver(msg)
              cat('\nSuccessfully emailed:', d$Last_name[i], d$Email[i], '\n')
              },
            error = function(c) cat('\nPROBLEM EMAILING:', d$Last_name[i], d$Email[i], '\n')
          )
        }
    }
}

#' @title grades
#' @description Utility function to assign grade based on cutoffs. If
#'   cutoffs are not supplied, they will be computed from probabilities
#' @param scores The exam score
#' @param cutoffs A named character vector. Names must be "A", "B", "C", "D". Values must
#'   be the minimum score to get that grade. If left NULL, cutoffs will be computed
#'   using the probabilities (probs), Default: NULL
#' @param probs The probabilities to compute cutoffs using the quantile function, Default: c(0.05, 0.1, 0.55, 0.85)
#' @return A character vector of simple grades: A, B, C, or D. Plus/minus grades are not available.
#' @details This is a utility function that is primarily used to generate midterm grades.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  grades(captured$SCORE)
#'  }
#' }
#' @rdname grades
#' @export

grades <- function(scores, cutoffs = NULL, probs = c(.05, .10, .55, .85)){
    if (is.null(cutoffs)){
        cutoffs <- quantile(scores, probs=probs, na.rm=T)
        names(cutoffs) <- c("D", "C", "B", "A")
    }
    grades <- rep("F", length(scores))
    grades[scores >= cutoffs["D"]] <- "D"
    grades[scores >= cutoffs["C"]] <- "C"
    grades[scores >= cutoffs["B"]] <- "B"
    grades[scores >= cutoffs["A"]] <- "A"
    return(grades)
}

#' @title midterm_grades
#' @description Create a csv file with student ids and grades for WSU midterm grades.
#' @param captured A data frame with columns: STUDENT_ID, SCORE
#' @param cutoffs A named character vector. Names must be "A", "B", "C", "D". Values must
#'   be the minimum score to get that grade. If left NULL, cutoffs will be computed
#'   using the probabilities (probs) in the grades function.
#' @return A csv file with student ids and grades is written to the working directory.
#' @details This function creates a file in the correct format for the WSU midterm grade system.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  midterm_grades(captures)
#'  }
#' }
#' @seealso
#'  \code{\link[readr]{write_csv}}

#'  \code{\link[stringr]{str_pad}}
#' @rdname midterm_grades
#' @export
#' @importFrom readr write_csv
#' @importFrom stringr str_pad
midterm_grades <- function(captured, cutoffs = NULL){
    captured$grade <- grades(captured$Score, cutoffs)
    captured$id2 <-stringr::str_pad(captured$STUDENT_ID, 9, side = 'left', pad = '0')
    readr::write_csv(captured[captured$id2 != "000000000", c('id2', 'grade')], path = 'midterm_grades.csv', col_names = F)
}

#' @title plot_scores
#' @description A ggplot of test score distributions, with a vertical line for one student's score.
#' @param e A data frame with all test scores (Score) and all grades (Grade).
#' @param score The student's individual score
#' @return A ggplot object
plot_scores <- function(e, score){
    require(ggplot2)
    ggplot(e, aes(x = Score, fill = Grade)) +
        geom_dotplot(binwidth=1) +
        scale_x_continuous(breaks = 5:25) +
        scale_y_continuous(NULL, breaks = NULL) +
        geom_vline(xintercept = score, color='red') +
        annotate(geom = 'text', x = score, y = .9, label = "Your score ", hjust = 1) +
        labs(title = 'Test score distribution')
}

#' @title email_scores
#' @description An alternative to email_report to email scores to students
#' @param scores a data frame with a 'Scores' column of scores
#' @param subject Email subject line
#' @param from Email 'From' address, Default: 'edhagen@wsu.edu'
#' @param dry_run Print out email texts to console, Default: T
#' @return Send status of each email
#' @details Will email each student in the scores data frame their score
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #email_scores(d, dry_run = F)
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[emayili]{server}}, \code{\link[emayili]{envelope}}, \code{\link[emayili]{addresses}}, \code{\link[emayili]{subject}}, \code{\link[emayili]{text}}
#' @rdname email_scores
#' @export
#' @importFrom glue glue_data
#' @importFrom stringr str_split
#' @importFrom emayili server envelope from to subject
email_scores <- function(scores, subject, from = 'edhagen@wsu.edu', dry_run = T){

    cat(getwd())

    wd_correct <- readline(prompt = "Is the working directory correct (y/n)? ")
    if (wd_correct != 'y'){
        stop("Incorrect working directory", call. = F)
    }

    if (any(is.na(scores$Email))){
        stop('Missing email address(es)', call. = F)
    }

    email_template <-
        "
    {{name}},

    Your test score was:

    {{score}}

    The class stats were

    Maximum score: {{max}}
    Average score: {{mean}}
    Minimum score: {{min}}

    The approximate grade cutoffs are:

    A: {A_range}
    B: {B_range}
    C: {C_range}
    D: {D_range}

    "

    grade_ranges <-
        list(
            "A_range" = readline(prompt = "A range: "),
            "B_range" = readline(prompt = "B range: "),
            "C_range" = readline(prompt = "C range: "),
            "D_range" = readline(prompt = "D range: ")
        )

    email_template <-
        glue::glue_data(grade_ranges, email_template)

    username = stringr::str_split(from, "@")[[1]][1]

    cat("\nEnter WSU password for:", username)
    password <- readline()

    d <- scores
    d$Score <- d$`Total points`

    # Create server
    wsuserver <- emayili::server(
        host = 'sdl6.vancouver.wsu.edu',
        username = username,
        password = password,
        port = 587
    )

    for (i in 1:nrow(d)){

        test_data = list(
            name = d$Name[i],
            score = d$Score[i],
            max = max(d$Score),
            mean = round(mean(d$Score), 1),
            min = min(d$Score)
        )

        body <- glue::glue_data(test_data, email_template)
        to = d$Email[i]

        if (dry_run){
            cat(body, '\n')
        } else {

            msg <- emayili::envelope() %>%
                emayili::from(from) %>%
                emayili::to(to) %>%
                emayili::subject(subject) %>%
                emayili::text(body)

            tryCatch({
                wsuserver(msg)
                cat('\nSuccessfully emailed:', d$Name[i], d$Email[i], '\n')
            },
            error = function(c) cat('\nPROBLEM EMAILING:', d$Name[i], d$Email[i], '\n')
            )
        }
    }
}
