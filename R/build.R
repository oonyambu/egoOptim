##exportPattern("^[[:alpha:]]+")

source_fun <- function(link){
  lns <- readLines(link, warn = FALSE, encoding = 'UTF-8')
  fun <- grep("^[^<]", lns, value = TRUE)
  if(inherits(try(str2lang(paste(fun, collapse = '\n')),TRUE),
              'try-error')){
    library(rvest)
    fun <- link %>%
      read_html() %>%
      html_elements('body') %>%
      html_text2()
  }
  fun
}




get_functions <- function(){
  link <- "https://www.sfu.ca/~ssurjano/"
  library(rvest)
  hrefs <- read_html(sprintf("%soptimization.html", link))|>
    html_elements('#functionlist a[href]') |>
    html_attr('href') |>
    paste0(link, x = _)

    lapply(hrefs, \(x)read_html(x) |>
             html_elements('#codes a[href*="r.html"]')|>
             html_attr('href'))|>
    setNames(hrefs)|>
    stack()|>
      transform(nm = sub("r.html", "", basename(values)),
                link = paste0(link, values),
                page = ind)|>
      subset(select = c('page', 'nm', 'link')) |>
      type.convert(as.is = TRUE)
}

build <- function(page, nm, link){
  cat("writting", nm,"...\n")
  a <- source_fun(link)
  if(length(a) == 1) a <- scan(text=a, what="", quiet = TRUE, sep="\n")
  b <- trimws(grep("# +\\w", a, value = TRUE)) |>
    grep("INPUT", x = _, value = TRUE, invert = TRUE)|>
    sub("#", "#'", x = _) |>
    sub("Authors:", '@author', x = _) |>
    sub("(\\w+) =", "@param \\1", x = _)

  d <- grep("Questions/Comments|reference information", b)
  e <- b[-(d[1]:d[2])]|>
    grep("^\\W+http", x=_, value = TRUE, invert = TRUE)

  ##------

  describe <- read_html(page)%>%
    html_element('#info')%>%
    html_text() %>%
    scan(text=., what="", sep="\n", quiet = TRUE)

  delete <- c("Description:","For questions","Code:","MATLAB",
  "Global","Input Domain")

  f <- sprintf("^(%s)", paste(delete,collapse = "|")) |>
    grep(describe, value = TRUE, invert = TRUE) |>
    sub("References:", sprintf("@references \\\\url{%s}", page), x=_)|>
    paste("#'", x=_)|>
    append(e,values = _, after = 1)|>
    c(sprintf("#' @export %s", nm)) |>
    c("#'\n",grep("#", a, value = TRUE, invert = TRUE))|>
    append("#'", 1)
    f[-(1:3)] <- sub("^(?=#' [^@])(\\W+)", "\\1\t", f[-(1:3)], perl = TRUE)
    #cat(f, file=sprintf("R/%s.R", nm), sep='\n')
  f
}

#invisible(do.call(Map, c(build, get_functions())))
