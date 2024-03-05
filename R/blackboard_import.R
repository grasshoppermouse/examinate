

#' @title blackboard_import
#' @description Import an exam text file and output a tab delimited version suitable for import into blackboard
#' @param fn The filename of the input file
#' @param out The filename of the output file
#' @return NULL. Call this function only for the side effect of creating the output file
#' @details Make sure input file is UTF-8 with lines separated by linefeeds (see blackboard docs).
#' @source \url{https://help.blackboard.com/Learn/Instructor/Tests_Pools_Surveys/Reuse_Questions/Upload_Questions}
#' @examples
#' Questions must be in this format (with one blank line between questions):
#'
#' 1. About 65 million years ago, animals that later evolved into mammals were probably:
#' a. nocturnal.
#' b. warm-blooded.
#' c. specialized chewers.
#' d. able to turn easily while locomoting on land.
#' *e. all of the above
#'
#' \dontrun{
#' if(interactive()){
#'  blackboard_import('my exam.txt', 'out.txt')
#'  }
#' }
#' @rdname blackboard_import
#' @export
#' @importFrom readr read_file
#' @importFrom stringr str_split str_detect
blackboard_import <- function(fn, out="out.txt"){
    questions <- readr::read_file(fn)
    questions <- stringr::str_split(questions, '\n\n')[[1]]
    conn <- file(out,open="w")
    for (q in questions){
        tmp <- stringr::str_split(q, '\n')[[1]]
        question <- str_replace(tmp[1], '\\d+\\. ', '')
        if(length(tmp)>3){
            for (i in 2:6){
                if (stringr::str_detect(tmp[i], "\\*")){
                    tmp[i] <- stringr::str_replace(tmp[i], '\\*[a-e]\\.', '')
                    tmp[i] <- paste0(tmp[i], '\tcorrect')
                } else {
                    tmp[i] <- stringr::str_replace(tmp[i], '[a-e]\\.', '')
                    tmp[i] <- paste0(tmp[i], '\tincorrect')
                }
                question <- paste0(question, '\t', tmp[i])
            }
            question <- paste0('MC\t', question)
            write(question, conn)
        } else {
            if(stringr::str_detect(tmp[2], "\\*a\\.")){
                tf <- "true"
            } else {
                tf <- "false"
            }
            question <- paste0("TF", "\t", question, "\t", tf)
            write(question, conn)
        }
    }
    close(conn)
    return()
}

