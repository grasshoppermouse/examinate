

#' @title itan
#' @description Item analysis
#' @param x Student answers (be sure to omit ID, IMAGE, and other extraneous columns)
#' @param keys Answer key, Default: NULL
#' @param digits Digits to report, Default: 2
#' @param no.resp Number of possible responses (e.g., A,B,C,D,E), Default: 5
#' @return
#' Stats for each test item:
#' \itemize{
#'    \item Proportion correct
#'    \item Biserial correlation with total score
#'    \item Mean score of students who got item correct
#'    \item Mean score of students who got item incorrect
#'    \item Number correct
#'    \item Number omitted
#'    \item Item response frequencies
#' }
#' @details This function provides some summary statistics for each item on the test.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  itan(df$captured, df$key)
#'  }
#' }
#' @rdname itan
#' @export

itan <- function(x,
                 keys = NULL,
                 digits = 2,
                 no.resp = 5) {
    # x is an n subjects by k items matrix
    # keys is a vector with the correct keys (A, B, C, D)
    stopifnot(ncol(x) > 1)
    x <- x[names(keys)]
    x[x == ""] <- "X"
    if (is.null(keys))
        keys <- rep("A", ncol(x))
    require(ltm)
    raw.resp <- matrix(nrow = ncol(x), ncol = no.resp + 1)
    colnames(raw.resp) <- c(LETTERS[1:no.resp], "X")
    for (i in 1:ncol(x)) {
        tmp <- table(x[, i])
        print(c(i, names(tmp)))
        raw.resp[i, names(tmp)] <- tmp
    }
    freq.resp <- raw.resp / apply(raw.resp, 1, sum, na.rm = T)
    na.resp <- apply(x, 2, function(x)
        sum(is.na(x)))
    correct.resp <- t(apply(x, 1, function(x)
        x == keys))
    total.score <- apply(correct.resp, 1, sum, na.rm = T)
    pbis <- apply(correct.resp, 2,
                  function(x)
                      biserial.cor(total.score, as.numeric(x),
                                   use = "complete.obs"))
    nb.correct <- apply(correct.resp, 2, sum, na.rm = T)
    p.obs <- nb.correct / nrow(x)
    MC <- MI <- numeric(ncol(x))
    for (i in 1:ncol(x))
        MC[i] <- mean(total.score[correct.resp[, i]], na.rm = T)
    for (i in 1:ncol(x))
        MI[i] <- mean(total.score[!correct.resp[, i]], na.rm = T)

    out <- cbind(
        P = p.obs,
        R = round(pbis, digits),
        MC = round(MC, digits),
        MI = round(MI, digits),
        NC = nb.correct,
        OMIT = na.resp,
        raw.resp
    )
    return(out)
}

# number_of_questions = 25
# number_of_students = 30

# d = read.delim('students.csv', colClasses = "character")
#
# answers = as.matrix(d[2:(number_of_students+1),7:(number_of_questions+6)])
# key = as.character(d[1,7:(number_of_questions+6)])

# 25 questions
#answers = as.matrix(d[2:55,6:30])
#key = as.character(d[1,6:30])

# 30 questions
#answers = as.matrix(d[2:32,6:35])
#key = as.character(d[1,6:35])

# 50 questions
# answers = as.matrix(d[2:87,6:55])
# key = as.character(d[1,6:55])

# out = itan(answers, key, no.resp=5)
# print(out)
# print(out[out[,1]<.5,1])

# Cutpoints for 5% D, 45% C, 30% B, 15% A
# quantile(scores, probs=c(.05, .10, .55, .85, 1))

 # Cutpoints for 10% D-F, 30% C, 30% B, 30% A
# quantile(scores, probs=c(.10, .40, .70, 1))

