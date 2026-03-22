
lpar <- list(
  quote = "'\"",
  comment.char = "#"
)

text <- c("
          -- Create a summary table
          DROP TABLE IF EXISTS ABALONE_SUMMARY;
          ",
          "CREATE TABLE ABALONE_SUMMARY AS 
          SELECT SEX, 
                COUNT(*) as TOTAL_COUNT,
                ROUND(AVG(LENGTH), 3) as AVG_LENGTH,
                ROUND(AVG(WHOLE), 3) as AVG_WEIGHT
          FROM ABALONE 
          GROUP BY SEX;
          ",
          "-- Query the results  
          SELECT * FROM ABALONE_SUMMARY ORDER BY SEX;
          ",
          "-- Parameterized query example
          SELECT 'PLOS;PLUM;', SEX, \"LOOP;'--'null\", COUNT(*) as COUNT 
          FROM ABALONE 
          WHERE LENGTH > :min_length
          GROUP BY SEX;
          ")

  text <- readLines(textConnection(text))

    rx <- paste0("([", lpar$quote, "])(?:\\\\.|(?!\\1).)*\\1")

    list_matches <- gregexpr(rx, text, perl = TRUE)

    text2 <- character(length(text))
    list_quoted <- list()
    quoted_counter <- 0
    for (ii in seq_along(list_matches)) {

      if (list_matches[[ii]][1] > 0) {

        out_text <- ""

        for (jj in seq_along(list_matches[[ii]])) {

          match_start <- list_matches[[ii]][jj]
          if (jj == 1 && match_start > 1) {
            pre_text <- substr(text[ii], 1, match_start - 1)
          } else {
            pre_text <- ""
          }

          match_length <- attr(list_matches[[ii]], "match.length")[jj]
          match_text <- substr(text[ii], match_start,
                               match_start + match_length - 1)

          quoted_counter <- quoted_counter + 1
          quoted_id <- paste0("@@@QUOTED_", quoted_counter)
          list_quoted[[quoted_id]] <- match_text

          out_text <- paste0(out_text, pre_text, quoted_id)

          if (jj <= length(list_matches[[ii]]) - 1) {
            post_start <- match_start + match_length
            if (post_start != list_matches[[ii]][jj + 1]) {
              pos_length <- list_matches[[ii]][jj + 1] - post_start
              post_text <- substr(text[ii], post_start,
                                  post_start + pos_length - 1)
              out_text <- paste0(out_text, post_text)
            }
          } else if (jj == length(list_matches[[ii]])) {
            post_start <- match_start + match_length
            if (post_start <= nchar(text[ii])) {
              post_text <- substr(text[ii], post_start, nchar(text[ii]))
              out_text <- paste0(out_text, post_text)
            }
          }
        }

      }  else {
        out_text <- text[ii]
      }
      text2[ii] <- out_text
    }

  text2 <- text2[grep("^--", text2, invert = TRUE)]

  idx <- grep("--", text2)
  text2[idx] <- sub("(--).+", "", text2[idx])

  sql <- unlist(strsplit(paste(text2, collapse = " "), ";", fixed = TRUE))
  sql <- gsub("^\\s+", "", sql)
  sql <- gsub("\\s+$", "", sql)
  sql <- gsub("\\s+", " ", sql)
  sql <- sql[sql != ""]

  sql <- sapply(X = sql, FUN = function(x) {
    for (quoted_id in names(list_quoted)) {
      x <- gsub(quoted_id, list_quoted[[quoted_id]], x, fixed = TRUE)
    }
    return(x)
  }, simplify = TRUE, USE.NAMES = FALSE)
