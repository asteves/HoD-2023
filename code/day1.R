## day 1 solution
data = readr::read_csv("../data/noahs-customers.csv", show_col_types = F)

text2num = function(text){
  ## Get all the Letters into a mapped vector 
  ## Q and X are special cases 
  map = sort(c(setNames(rep(2:9, each = 3), 
                        paste0(LETTERS[c(-17, -26)])), 
               setNames(c(7, 9), 
                        paste0(c("Q", "X")))))
  ## Split to get last name and make all text upper case
  v = strsplit(text, split = "\\s")[[1]][[2]] |> 
    toupper()
  
  ## Convert text to numbers 
  ## paste back to together as single character vector string
  result = paste(
    as.character(
      unname(
        sapply(
          unlist(strsplit(v, "")),
          function(x) map[x]))
    ), 
    collapse = "")
  return(result)
}

text2num("text text")

correct_phone = data |> 
  mutate(phone2 = str_replace_all(phone, "-", "")|>trimws(),
         name2 =  sapply(name, function(x) text2num(x))) |>
  filter(phone2 == name2) |>
  select(phone) |>
  pull()
