library(tidyverse)

page2006 <- "https://es.wikipedia.org/wiki/Anexo:LX_Legislatura_del_Congreso_de_la_Unión_de_México"
page2009 <- "https://es.wikipedia.org/wiki/Anexo:LXI_Legislatura_del_Congreso_de_la_Unión_de_México"
page2012 <- "https://es.wikipedia.org/wiki/Anexo:LXII_Legislatura_del_Congreso_de_la_Unión_de_México"
page2015 <- "https://es.wikipedia.org/wiki/Anexo:LXIII_Legislatura_del_Congreso_de_la_Unión_de_México"
page2018 <- "https://es.wikipedia.org/wiki/Anexo:LXIV_Legislatura_del_Congreso_de_la_Unión_de_México"
page2021 <- "https://es.wikipedia.org/wiki/Anexo:LXV_Legislatura_del_Congreso_de_la_Unión_de_México"


extract_mex_dip <- function(pagina) {
    year_page <- rlang::englue("{{ pagina }}")
    year <- year_page %>% 
        str_extract("\\d{4}") %>% 
        as.numeric()
    
    ttt <- read_lines(pagina) %>% 
        as_tibble() %>% 
        mutate(n = row_number(),
               smd = str_detect(value, "Diputados por distrito uninominal \\(mayoría relativa\\)") %>% 
                   cumsum(),
               pr = str_detect(value, "Diputados por representación proporcional") %>% 
                   cumsum(),
               pres = str_detect(value, "Presidentes? de la Cámara de Diputados") %>% 
                   cumsum())
    
    wrangle <- function(dat) {
        leg <- dat %>% 
            mutate(state = str_extract(value,
                                       "(?<=<td>)\\w+(\\s\\w+)?(\\s\\w+)?(?=</td>)"),
                   name = str_extract(value,
                                      '(?<=title=")\\w+\\.?([-\\s]\\w+){1,7}(\\s\\(diputado\\))?(\\s\\(aún no redactado\\))?(?=")'),
                   name = if_else(name == "Movimiento Ciudadano",
                                  NA_character_,
                                  name),
                   name = if_else(is.na(name),
                                  lag(name),
                                  name),
                   party = str_extract(value,
                                       '(?<=img alt=").*?(?=\\.(png|svg|JPG|jpg))'),
                   party = if_else(is.na(party),
                                   str_extract(value,
                                               "(?<=File:)\\w+"),
                                   party),
                   party = str_remove(party, "[Ll]ogo") %>% 
                       str_remove("\\s+Part(y|ido)\\s+") %>% 
                       str_remove("\\s*[Aa]ntiguo\\s*") %>%
                       str_remove("\\s*\\(.*?\\)") %>% 
                       str_remove_all("_"),
                   party = case_when(party == "Nueva Alianza" ~ "Nueva alianza",
                                     party == "PVE" ~ "PVEM",
                                     TRUE ~ party),
                   letra = str_extract(name, "\\w(?=[\\s\\.])")) %>% 
            fill(state) %>% 
            filter(!is.na(party)) %>% 
            select(state, name, party, letra)
        
        return(leg)
    }
    
    smd <- ttt %>% 
        filter(smd == 2 & pr == 1) %>% 
        wrangle() %>% 
        mutate(type = "smd")
    
    pr <- ttt %>% 
        {if(year > 2009 & max(.$pres) >= 3) mutate(., pres = pres-1) else .} %>% 
        filter(pr == 2 & pres == 1) %>% 
        wrangle() %>% 
        mutate(type = "pr")
    
    leg <- bind_rows(smd, pr) %>% 
        arrange(party, letra, name) %>% 
        group_by(party) %>% 
        group_modify(~ add_row(.x)) %>% 
        mutate(party = if_else(is.na(name), NA_character_, party),
               fem = if_else(letra == "a" |
                                 (letra == "i" &
                                      !str_detect(name, "^Sami\\b") &
                                      !str_detect(name, "^Cuauhtli\\b")) |
                                 (letra == "t" &
                                      !str_detect(name, "^Limbert\\b") &
                                      !str_detect(name, "^Riult\\b") &
                                      !str_detect(name, "^Hamlet\\b")) |
                                 (letra == "y" &
                                      !str_detect(name, "^Roy\\b") &
                                      !str_detect(name, "^Harvey\\b")) |
                                 (letra == "z" &
                                      !str_detect(name, "^\\bCruz\\b")) |
                                 str_detect(name, "^Xitlalic\\b") |
                                 str_detect(name, "^\\w+dad\\b") |    
                                 str_detect(name, "^\\w+rid\\b") |
                                 str_detect(name, "^\\w+et[t]e\\b") |
                                 str_detect(name, "^Angie\\b") |
                                 str_detect(name, "^Celeste\\b") |
                                 str_detect(name, "^Ja.{0,2}ck?ie\\b") |
                                 str_detect(name, "^Leslie\\b") |
                                 str_detect(name, "^Sue\\b") |
                                 str_detect(name, "^\\w+[li][cs]+e\\b") |
                                 str_detect(name, "^Denisse\\b") |
                                 str_detect(name, "^\\w+[oi]nn?e\\b") |
                                 str_detect(name, "^Mait.{1,2}\\b") |
                                 str_detect(name, "^\\w+th\\b") |
                                 str_detect(name, "^Rosi\\b") |
                                 str_detect(name, "^Araceli\\b") |
                                 str_detect(name, "^Rub.{1,2}\\b") |
                                 str_detect(name, "^Anah.{1,2}\\b") |
                                 str_detect(name, "^Noem.{1,2}\\b") |
                                 str_detect(name, "^Itzel\\b") |
                                 str_detect(name, "^Michel\\b") |
                                 str_detect(name, "^Raquel\\b") |
                                 str_detect(name, "^Carol\\b") |
                                 str_detect(name, "^Marisol\\b") |
                                 str_detect(name, "^Yeidckol\\b") |
                                 str_detect(name, "^Yahleel\\b") |
                                 str_detect(name, "^\\w{2,}[bv]ell?\\b") |
                                 str_detect(name, "^Guadalupe\\b") |
                                 str_detect(name, "^Delvim\\b") |
                                 str_detect(name, "^Miriam\\b") |
                                 str_detect(name, "^Carmen\\b") |
                                 str_detect(name, "^Concepci.{1,2}n\\b") |
                                 str_detect(name, "^Evelyn\\b") |
                                 str_detect(name, "^Joann\\b") |
                                 str_detect(name, "^Karen\\b") |
                                 str_detect(name, "^Lilli.{1,2}n\\b") |
                                 str_detect(name, "^Purificaci.{1,2}n\\b") |
                                 str_detect(name, "^Sharon\\b") |
                                 str_detect(name, "^Wendolin\\b") |
                                 str_detect(name, "^Consuelo\\b") |
                                 str_detect(name, "^Refugio\\b") |
                                 str_detect(name, "^Rosario\\b") |
                                 str_detect(name, "^Roc.{1,2}o\\b") |
                                 str_detect(name, "^Socorro\\b") |
                                 str_detect(name, "^Flor\\b") |
                                 str_detect(name, "^Pilar\\b") |
                                 str_detect(name, "^\\bEsther\\b") |
                                 str_detect(name, "^\\w+des\\b") |
                                 str_detect(name, "^Dolores\\b") |
                                 str_detect(name, "^Janet\\b") |
                                 str_detect(name, "^Ar√°nzazu\\b") |
                                 str_detect(name, "^Anilu\\b") |
                                 str_detect(name, "^Paz\\b")
                                 ,
                             1,
                             NA_real_),
               n = row_number(),
               n = if_else(!is.na(name),
                           n,
                           NA_integer_),
               year = year)
    
    write_csv(leg, file.path("data-raw", paste0(year_page, ".csv")), na = "")
    return(leg)
}

p2006 <- extract_mex_dip(page2006) %>% 
    filter(!is.na(party))
p2009 <- extract_mex_dip(page2009) %>% 
    filter(!is.na(party))
p2012 <- extract_mex_dip(page2012) %>% 
    filter(!is.na(party))
p2015 <- extract_mex_dip(page2015) %>% 
    filter(!is.na(party))
p2018 <- extract_mex_dip(page2018) %>% 
    filter(!is.na(party))
p2021 <- extract_mex_dip(page2021) %>% 
    filter(!is.na(party))

ttt <- bind_rows(p2006,
                 p2009, 
                 p2012,
                 p2015,
                 p2018,
                 p2021) %>%
    filter(!is.na(party)) %>% 
    group_by(year, party) %>% 
    summarize(absseat_a = max(n),
              mujeres = sum(fem, na.rm = TRUE),
              pfem_a = mujeres*100/absseat_a) %>%
    select(year, party, pfem_a, mujeres, absseat_a) %>% View()
