wait <- function(wait = T){
  if(wait){
    Sys.sleep(runif(1, 1, 1.2))
    cat(paste0(i, " - waited for ", round(t, 2), "s. "))
  }
}

scrape_indexpage <- function(url) { # URL = XXV, XXIV, XXIII, ...
  
  #read in page
  indexpage <- read_html(url)
  
  # get dates of proposals (website upload?), type (RV, A, VOLKBG, BRA), descriptive title, 
  indexpagedata <- indexpage %>%
    html_nodes("span.table-responsive__inner") %>%
    html_text(trim = T) %>%  # get table inner text and trim
    matrix(ncol = 5, byrow = T) %>%  # put into matrix, then dataframe (tibble)
    .[, -5] %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("web_date", "type", "desc_title", "proposal_id")) 
  
  indexpagedata <- indexpagedata %>%
    mutate(proposal_link = indexpage %>%
             html_nodes("a.link-indicator") %>%
             html_attr("href") %>%
             .[seq(1, length(.), by = 2)] %>%
             paste0("https://www.parlament.gv.at", .), 
           period = url %>%
             str_extract("GP=\\w+") %>%
             str_sub(., start = 4), 
           status = indexpage %>%
             html_nodes("img.status") %>%
             html_attr("src") %>%
             .[seq(1, length(.), by = 2)] %>%
             str_extract("[1-9]") %>%
             as.character(),
           proposal_filename = proposal_id %>%
             gsub("[^A-Za-z0-9]", "" , .) %>%
             paste(period, ., sep = "_"))
  
  #exporting to appropriatly named .csv
  write_csv(indexpagedata, file = paste0("data/df_", indexpagedata$period[1], ".csv"))
  
}

fix_obj <- function(obj, pattern = NA_character_){if(length(obj) == 0) return(pattern) else return(obj)}

get_meta_info <- function(.x){
  page <- read_html(.x$proposal_link)
  
  bill_link <- page %>%
    html_nodes("div.floatLeft p a") %>%
    html_attr("href") %>%
    str_subset("BNR_\\d+") %>%
    str_replace("\\/pls.+[=]", "https://www.parlament.gv.at") %>%
    fix_obj
  
  bill_id <- bill_link %>%
    str_extract("XX.+BNR_[0-9]*") %>%
    str_replace("\\/B.+_00", "_BNR") %>%
    fix_obj
  
  parties <- page %>%
    html_node("div.floatLeft p") %>%
    html_text() %>%
    str_to_lower() %>%
    str_extract("(?<=daf.r\\:).*") %>%
    str_split(", dagegen\\: ") %>%
    .[[1]] %>%
    str_split(", ") 
  
  resolution_NR <- tibble(party = unlist(parties)) %>%
    mutate(pro = party %in% parties[[1]]) %>%
    list %>%
    fix_obj(pattern = list())
  
  
  proposal_download <- case_when(
    .x$type %in% c("A", "RV", "GABR") ~ {page %>%
        html_nodes("ul.fliesstext li a") %>%
        html_attr("href") %>%
        str_subset("\\d.html") %>% 
        .[1]},
    .x$type %in% c("BUA") ~ {page %>%
        html_nodes("ul.fliesstext li a") %>%
        html_attr("href") %>%
        str_subset("\\d.html") %>%
        .[2]}
  ) %>%
    fix_obj
  
  iniator <- page %>%
    html_nodes("div.c_2 p") %>%
    .[-1] %>%
    html_text(trim = T) %>%
    str_squish %>%
    str_extract("(?<=\\:)\\s*[A-Z].+") %>%
    str_trim() %>%
    str_subset("[0-9]{3}", negate = T) %>%
    str_subset("\\s{3,}", negate = T) %>%
    fix_obj
    # fix_obj(pattern = list())
  
  verfahren <- page %>%
    html_node(".tabelleHistorieResponsive") %>%
    html_nodes(".historyShowAlways") %>%
    html_text(trim = T) %>%
    keep(~str_detect(.x, "\\d{2}.\\d{2}.\\d{4}")) %>%
    tibble(verfahren = .) %>%
    list
  
  # dates <- page %>%
  #   html_nodes("table.table-nonresponsive") %>%
  #   html_nodes("tr.historyShowAlways") %>%
  #   html_text(trim = T) %>%
  #   str_extract("\\d{2}.\\d{2}.\\d{4}") %>%
  #   discard(is.na) 
  # 
  # processes <- page %>%
  #   html_nodes("a.historieOverviewToggle") %>%
  #   html_text()
  # 
  # parl_verfahren <- tibble(date = dates,
  #                          process = processes) %>%
  #   list %>%
  #   fix_obj(pattern = list())
  
  return(tibble::tibble( .id = .x$.id, bill_link, bill_id, proposal_link = .x$proposal_link, proposal_download, iniator, resolution_NR, verfahren))
  
}