# ReportArticles.R
#
# AUTHOR
# Herbert Barrientos
# hpbarr@gmail.com
#
# CREATION DATE
# 2020-04-12
#
# VERSION
# 2022-02-16-1332 (latest revision)
#
# PROJECT NAME
# COVID-19 Open Research Dataset Challenge (CORD-19)
# https://www.kaggle.com/allen-institute-for-ai/CORD-19-research-challenge/tasks
#
# DATA
# The following three files are pre-processed data from the downloaded "metadata.csv" file:
#
# "metadata_full_text.csv"
#    Description  : dataset with all rows having full-text articles.
#    Search column: "full_text".
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData_for_2021-12-20-1427_dataset_v2022-02-13-1751.R
#    May be empty : yes.
#
# "metadata_abstract.csv"
#    Description  : dataset with all rows not having full text articles,
#                    but having a non-empty abstract.
#    Search column: "abstract".
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData_for_2021-12-20-1427_dataset_v2022-02-13-1751.R
#    May be empty : yes.
#
# "metadata_title.csv"
#    Description  : dataset with all rows not having neither full-text
#                   articles nor an abstract, but a non-empty title.
#    Search column: "title"
#    Editable     : no. For a new version of the data, execute script 
#                   PreProcessInputData_for_2021-12-20-1427_dataset_v2022-02-13-1751.R
#    May be empty : yes.
# 
# INPUT
# Question: 
#   May be a single keyword, a sequence of keywords divided by spaces,
#   or a regular expression.
#
# Question type: 
#   - Single-term: keyword or a sequence of keywords.
#   - Not single-term: a regular expression.
#
# OUTPUT
# A DataTable containing the search results to the question.
#
# DESCRIPTION 
# This module contains functionality to handle input, perform 
# the search process, format output, and deliver the results.
#
# Once the input has been validated, a search is performed in every data subset 
# in the corpora. When the question is "single-term" or a list of terms, the 
# program extracts full sentences where occurrences of the search terms were 
# found. Sentences are then reported as the excerpt for the processed data subset 
# row. When the question is "not single-term", the output is generally a large 
# chunk of text. To make reading easier, the excerpt is created by extracting the 
# first and last NUM_WORDS_PER_CHUNK words and concatenating them together. On the 
# other hand, if the size of the output text is less than, or equal to, the limit 
# specified by MAX_WORDS, the text is left as is. Just as with the sentences, the 
# resulting excerpt is also added to the processed data subset row. 
#
# All processed rows are added to a table (i.e., outDataTable).
#
# PRECONDITION
# ((|corpusFullText| + |corpusAbstract| + |corpusTitle|) > 0)
#
# FUTURE IMPROVEMENTS
# 1. Performance: currently the response time is not satisfactory.


# ################################|
# ########## LIBRARIES ###########
# ################################
library(data.table)
library(readr)


# ################################
# ######## CONFIGURATION #########
# ################################
STANDALONE <- FALSE
LOGGING    <- TRUE

MAX_WORDS <- 80
NUM_WORDS_PER_CHUNK <- 30

# Some full-text queries may produce thousands of records whose memory loads most 
# likely will cause system crashes, especially if this application is hosted on a 
# low-resource server like ShinyApps - Free Account. In such case, in an attempt 
# to reduce, or at best, avoid such error conditions, function sanityCheck() will 
# report the likelihood of a potential memory exhaustion problem. To accomplish
# this action, setting DO_SANITY_CHECK is set to TRUE. On the other hand, if the 
# application is hosted on a server having adequate resources (e.g., memory, disk 
# space, processor, etc.) then sanityCheck() will no longer be needed, in which 
# case setting DO_SANITY_CHECK needs to be set to FALSE.
DO_SANITY_CHECK <- TRUE

# These are just estimates based on testing outcomes
MEMORY_THRESHOLD  <- 0.8
NUM_ROW_THRESHOLD <- 0.8

# Another strategy to manage memory resources on a low-resource server is to limit 
# size of the result set to the "most recent articles", which of course leaves out
# many potentially interesting and useful articles. For this action to take place,
# setting LATEST_ARTICLES is set to <F, F, T>, in order to give sanityCheck() the 
# chance to test for potential memory errors using full title and abstract result 
# sets (which are generally not as large as full-text result sets). In future 
# versions of this application, however, this setting ought to be a user-defined 
# preference, rather that an application constraint.
LATEST_ARTICLES <- c(FALSE, FALSE, TRUE)


# ################################
# ########## CONSTANTS ###########
# ################################

# Determine whether or not to show progress bars in function read_csv_chunked
options(readr.show_progress = ifelse(STANDALONE, TRUE, FALSE))

ROOT_INPUT_DIR       <- paste0(getwd(), ifelse(STANDALONE, "/COVID-19/covid19_ReportApp_2022/data", "/data"))
FILE_PATH_FULL_TEXT  <- paste0(ROOT_INPUT_DIR, "/metadata_full_text.csv")
FILE_PATH_ABSTRACT   <- paste0(ROOT_INPUT_DIR, "/metadata_abstract.csv")
FILE_PATH_TITLE      <- paste0(ROOT_INPUT_DIR, "/metadata_title.csv")

CONTIGUOUS_WHITE_SPACES <- "  +"
EMPTY_STRING            <- ""
PIPE_CHAR               <- "|"
PERIOD                  <- "."
COMMA                   <- ","
L_PARENTHESIS           <- "("
R_PARENTHESIS           <- ")"
L_SQ_BRACKET            <- "["
R_SQ_BRACKET            <- "]"
ELLIPSIS                <- "..."
WHITE_SPACE             <- " "

INPUT_TEXT  <- 1
INPUT_REGEX <- 2

SEARCH_COLNAMES <- c("X", "cord_uid", "sha", "source_x", "title", "doi", "pmcid", "pubmed_id", "license", "abstract", "publish_time", "authors", "journal", "mag_id", "who_covidence_id", "arxiv_id", "pdf_json_files", "pmc_json_files", "url", "s2_id", "short_author_list")
SEARCH_COLNAME_TYPES <- c("ccccccccccccccccccccc")

FULL_TEXT_SEARCH_COLNAMES <- c(SEARCH_COLNAMES, "full_text")
FULL_TEXT_SEARCH_COLNAME_TYPES <- c("cccccccccccccccccccccc")

# Error handling
COND_OUT_OF_MEMORY  <- "Out of memory"  # Error condition as reported by R
ERROR_OUT_OF_MEMORY <- -1

SANITY_CHECK_GREEN <- 1
SANITY_CHECK_RED   <- 0

LATEST_ARTICLES_TIME_PERIOD   <- "(2020.(10|11|12)|202(1|2))"
LATEST_ARTICLES_TITLES_IDX    <- 1
LATEST_ARTICLES_ABSTRACTS_IDX <- 2
LATEST_ARTICLES_FULL_TEXT_IDX <- 3


# ################################
# ########## VARIABLES ###########
# ################################
searchExpression  <- EMPTY_STRING
latestArticlesIdx <- LATEST_ARTICLES_FULL_TEXT_IDX  # Default


# ################################
# ########## FUNCTIONS ###########
# ################################

# ######## INPUT HANDLING ########
isRegexValid <- function(regexpr) {
  
  t <- ""
  r <- NULL
  
  r <- tryCatch(ifelse(grepl(regexpr, t), TRUE, TRUE), error = function(e) { return(FALSE)})
  
  return(!is.null(r) && (r == TRUE))
  
}  # END isRegexValid

convertInputToRegex <- function(input) {
  
  return(paste0(L_PARENTHESIS, gsub(WHITE_SPACE, PIPE_CHAR, input), R_PARENTHESIS))
  
}  # END convertInputToRegex

validateTextInput <- function(input) {
  
  if (is.null(input) || ((newInput <- trimws(input)) == EMPTY_STRING))
    return(EMPTY_STRING)
  
  newInput <- tolower(newInput)
  newInput <- gsub(CONTIGUOUS_WHITE_SPACES, WHITE_SPACE, newInput)
  
  if (!grepl("^[-_a-z0-9 ]+( ?,[-_a-z0-9 ]+ ?)*$", newInput))
    return(EMPTY_STRING)
  
  return(newInput)
  
}  # END validateTextInput

validateInput <- function(inputString, inputType) {
  
  if (inputType == INPUT_REGEX) {
    
    if (!isRegexValid(inputString))
      return(EMPTY_STRING)
    
    return(inputString)
    
  }  # END if
  
  if (inputType == INPUT_TEXT) {
    
    t <- validateTextInput(inputString)
    if (t == EMPTY_STRING) return(EMPTY_STRING)
    
    return(convertInputToRegex(t))
    
  }  # END if
  
  return(EMPTY_STRING)
  
}  # END validateInput


# ######## OUTPUT HANDLING ########
createOutputTable <- function(articles, excerpts) {
  
  titles   <- c(articles[["articlesInFullTexts"]]$title,             articles[["articlesInAbstracts"]]$title,             articles[["articlesInTitles"]]$title)
  authors  <- c(articles[["articlesInFullTexts"]]$short_author_list, articles[["articlesInAbstracts"]]$short_author_list, articles[["articlesInTitles"]]$short_author_list)
  journals <- c(articles[["articlesInFullTexts"]]$journal,           articles[["articlesInAbstracts"]]$journal,           articles[["articlesInTitles"]]$journal)
  publish  <- c(articles[["articlesInFullTexts"]]$publish_time,      articles[["articlesInAbstracts"]]$publish_time,      articles[["articlesInTitles"]]$publish_time)
  urls     <- c(articles[["articlesInFullTexts"]]$url,               articles[["articlesInAbstracts"]]$url,               articles[["articlesInTitles"]]$url)
  excerpts <- c(excerpts[["excerpt_full_text"]],                     excerpts[["excerpt_abstract"]],                      excerpts[["excerpt_title"]])
  
  outDataTable <- data.table(title=titles, authors=authors, journal=journals, 
                             publish_time=publish, publish_url=urls, excerpt=excerpts)
  
  return(outDataTable)
  
}  # END createOutputTable

formatResults <- function(t) {
  
  idx <- 1
  titlesWithUrl <- as.vector(lapply(t$publish_url, function(x) { 
    nxtUrl <- paste0("<a href='", trimws(x), "', target=_blank>", trimws(t[idx,]$title), "</a>")
    idx <<- (idx+1)
    return(nxtUrl)}))
  
  outTable <- data.table(Title=titlesWithUrl, Authors=t$authors, Journal=t$journal, PublishTime=t$publish_time, Excerpt=t$excerpt)
  
  return(outTable)
  
}  # END formatResults


# ######## ERROR HANDLING ########
evalError <- function(condition) {
  
  if (grepl(COND_OUT_OF_MEMORY, condition)) return(ERROR_OUT_OF_MEMORY)
  
}  # END evalError


# DELETE THIS: 
# This function is not very accurate in terms of memory size evaluation. However, it will
# be left here for future reference that this option was at one time tested
# sanityCheck <- function(x) {
# 
#   memfree <- as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE))
#   xSize   <- as.numeric(object.size(x))
#   
#   if (LOGGING) print(paste0("sanityCheck: memfree=", memfree, ", xSize=", xSize))
#   
#   return(ifelse((xSize/memfree) >= MEMORY_THRESHOLD, SANITY_CHECK_RED, SANITY_CHECK_GREEN))
#   
# }  # END sanityCheck
  

sanityCheck <- function(filePath, numReadRows) {
  
  # If this function is no longer needed, return green
  if (!DO_SANITY_CHECK) return(SANITY_CHECK_GREEN)

  numFileRows <- length(vroom::vroom_lines(filePath, altrep = TRUE, progress = FALSE))
  pct <- (numReadRows/numFileRows)
  
  if (LOGGING) print(paste0("sanityCheck: numReadRows=", numReadRows, ", numFileRows=", numFileRows, ", pct=", pct))

  return(ifelse(pct >= NUM_ROW_THRESHOLD, SANITY_CHECK_RED, SANITY_CHECK_GREEN))

}  # END sanityCheck


# ######## SEARCH PROCESS ########
getLatestArticles <- function(articles) {
  
  if (isTRUE(LATEST_ARTICLES[latestArticlesIdx])) {
    
    articleIdx <- grep(LATEST_ARTICLES_TIME_PERIOD, articles$publish_time)
    articles   <- articles[articleIdx,]
    
  }  # END if

  return(articles)
  
}  # END getLatestArticles

retrieveArticlesinTitles <- function(articles, idx) {
  
  latestArticlesIdx <<- LATEST_ARTICLES_TITLES_IDX
  
  # Create an article subset of rows matching the search expression
  articleIdx <- grep(searchExpression, articles$title)
  articles   <- articles[articleIdx,]
  articles   <- getLatestArticles(articles)

  return(articles)
  
}  # END retrieveArticlesinTitles

retrieveArticlesinAbstracts <- function(articles, idx) {
  
  latestArticlesIdx <<- LATEST_ARTICLES_ABSTRACTS_IDX
  
  # Create an article subset of rows matching the search expression
  articleIdx <- grep(searchExpression, articles$abstract)
  articles   <- articles[articleIdx,]
  articles   <- getLatestArticles(articles)
  
  return(articles)
  
}  # END retrieveArticlesinAbstracts

retrieveArticlesinFullTexts <- function(articles, idx) {
  
  latestArticlesIdx <<- LATEST_ARTICLES_FULL_TEXT_IDX
  
  # Create an article subset of rows matching the search expression
  articleIdx <- grep(searchExpression, articles$full_text)
  articles   <- articles[articleIdx,]
  articles   <- getLatestArticles(articles)
  
  return(articles)
  
}  # END retrieveArticlesinFullTexts

retrieveArticles <- function(searchExp) {
  
  error <- tryCatch(
    {
      searchExpression <<- searchExp
      
      if (LOGGING) print("retrieveArticles: begin...")
      
      articlesInTitles <- read_csv_chunked(FILE_PATH_TITLE, DataFrameCallback$new(retrieveArticlesinTitles),    col_names = SEARCH_COLNAMES, col_types = SEARCH_COLNAME_TYPES)
      if (LOGGING) print(paste0("retrieveArticles: read_csv_chunked: FILE_PATH_TITLE: ", nrow(articlesInTitles), " rows"))
      
      # FILE_PATH_TITLE is the shortest of files and, given its little search text contents (i.e., only title), 
      # it produces the least number of results without overloaded memory (i.e., no abstract and no full-text). 
      # Heuristic: Hence, if the sanity check returns red, there is a good chance that the next larger files
      # to process will produce large data sets that may exhaust the memory. Therefore, return an error condition
      if (sanityCheck(FILE_PATH_TITLE, nrow(articlesInTitles)) == SANITY_CHECK_RED)
        return(ERROR_OUT_OF_MEMORY)
      
      articlesInAbstracts <- read_csv_chunked(FILE_PATH_ABSTRACT, DataFrameCallback$new(retrieveArticlesinAbstracts), col_names = SEARCH_COLNAMES, col_types = SEARCH_COLNAME_TYPES)
      if (LOGGING) print(paste0("retrieveArticles: read_csv_chunked: FILE_PATH_ABSTRACT: ", nrow(articlesInAbstracts), " rows"))
      
      # Do a sanity check again...
      if (sanityCheck(FILE_PATH_ABSTRACT, nrow(articlesInAbstracts)) == SANITY_CHECK_RED)
        return(ERROR_OUT_OF_MEMORY)
      
      articlesInFullTexts <- read_csv_chunked(FILE_PATH_FULL_TEXT, DataFrameCallback$new(retrieveArticlesinFullTexts), col_names = FULL_TEXT_SEARCH_COLNAMES, col_types = FULL_TEXT_SEARCH_COLNAME_TYPES)
      if (LOGGING) print(paste0("retrieveArticles: read_csv_chunked: FILE_PATH_FULL_TEXT: ", nrow(articlesInFullTexts), " rows"))
      
      if (LOGGING)  print("retrieveArticles: creating results list")
      articles <- list("articlesInTitles" = articlesInTitles, "articlesInAbstracts" = articlesInAbstracts, "articlesInFullTexts" = articlesInFullTexts)
      
      if (LOGGING) print("retrieveArticles: done...")
      
      return(articles)
    },
      error=function(cond) {
    
      return(evalError(cond))
        
    })  # END tryCatch
  
  return(error)
  
}  # END retrieveArticles


# ######## EXCERPT EXTRACTION PROCESS ########
getExcerpt <- function(articleText) {
  
  extractedTextPositions <- gregexpr(searchExpression, articleText, perl = TRUE)
  extractedTextPositionsAttr <- lapply(extractedTextPositions, attributes)
  
  extractedTextPositionsAttrLen <- extractedTextPositionsAttr[[1]][1]
  if (is.vector(extractedTextPositionsAttrLen[[1]])) extractedTextPositionsAttrLen <- extractedTextPositionsAttrLen[[1]][1]
  
  startIdx <- extractedTextPositions[[1]][1]
  stopIdx  <- (extractedTextPositionsAttrLen + startIdx - 1)
  
  # Reduce the excerpt to a manageable sized text
  excerpt      <- paste0(ELLIPSIS, WHITE_SPACE)
  reportedText <- trimws(substr(articleText, start = startIdx, stop = stopIdx))
  words        <- strsplit(reportedText, WHITE_SPACE)
  
  if (length(words[[1]]) > MAX_WORDS) {
    
    for (idx in 1:NUM_WORDS_PER_CHUNK)
      excerpt <- paste0(excerpt, words[[1]][idx], WHITE_SPACE)
    
    excerpt <- paste0(excerpt, L_SQ_BRACKET, WHITE_SPACE, ELLIPSIS, WHITE_SPACE, R_SQ_BRACKET, WHITE_SPACE)
    
    for (idx in (length(words[[1]])-NUM_WORDS_PER_CHUNK):length(words[[1]]))
      excerpt <- paste0(excerpt, words[[1]][idx], WHITE_SPACE)
    
    excerpt <- paste0(excerpt, ELLIPSIS)
    
    return(excerpt)
    
  }  # END if 
  
  if (length(words[[1]]) == 1) {
    
    # Attempt to find a sentence, including the search term,
    # that may bring some insight to the reader. Begin with
    # the start index backwards until a punctuation mark is 
    # found, or the beginning of the text is reached
    articleTextSplit <- strsplit(articleText, EMPTY_STRING)[[1]]
    while (startIdx > 1) {
      
      if (articleTextSplit[startIdx] == PERIOD) {
        startIdx <- (startIdx+1)
        break()
      }
      startIdx <- (startIdx-1)
      
    }  # END while
    
    # Continue with stop index until a punctuation mark is 
    # found, or the end of the text is reached
    stopIdx <- (stopIdx + 1)
    
    while (stopIdx < length(articleTextSplit)) {
      
      if (articleTextSplit[stopIdx] == PERIOD) {
        stopIdx <- (stopIdx-1)
        break()
      }
      stopIdx <- (stopIdx+1)
      
    }  # END while
    
    # Extract the excerpt from the text
    excerpt <- paste0(ELLIPSIS, WHITE_SPACE, 
                      trimws(substr(articleText, start = startIdx, stop = stopIdx)), 
                      WHITE_SPACE, ELLIPSIS)
    
    return(excerpt)
    
  }  # END if
  
  # Here: (2 <= |words| <= MAX_WORDS)
  excerpt <- paste0(excerpt, reportedText, WHITE_SPACE, ELLIPSIS)
  
  return(excerpt)
  
}  # END getExcerpt

extractExcerpts <- function(articles) {

  excerpt_title <- lapply(articles[["articlesInTitles"]]$title, getExcerpt)
  excerpt_abstract  <-lapply(articles[["articlesInAbstracts"]]$abstract, getExcerpt)
  excerpt_full_text <-lapply(articles[["articlesInFullTexts"]]$full_text, getExcerpt)
  
  excerpts <- list("excerpt_title" = excerpt_title, "excerpt_abstract" = excerpt_abstract, "excerpt_full_text" = excerpt_full_text)
  
  return(excerpts)
  
}  # END extractExcerpts


# ######## MAIN ########
findArticles <- function(searchExpression) {
  
  error <- tryCatch(
    {
      articles <- retrieveArticles(searchExpression)
      gc()
      
      # Check for error condition
      if (!is.list(articles)) return(articles)
      
      if (LOGGING) print("extractExcerpts: begin...")
      excerpts <- extractExcerpts(articles)
      if (LOGGING) print("extractExcerpts: done...")
      gc()
      
      if (LOGGING) print("createOutputTable: begin...")
      outputTable <- createOutputTable(articles, excerpts)
      if (LOGGING) print("createOutputTable: done...")
      gc()
      
      return(outputTable)
    },
      error=function(cond) {
      
      return(evalError(cond))
    })
  
  return(error)
  
}  # END findArticles
