pacman::p_load(tidyverse, tidymodels, caret, rvest, rlist, 
               rjson, Rcrawler, httr, jsonlite, dplyr, rjstat, lubridate,
               RSQLite, dbplyr, ggplot2, usethis, readr, purrr, leaps, glmnet, stringr)

# ================ SUPERSTATS WEBSCRAPE ================ 

# ---- Scrape funktion ----
get_home_table <- function(url) {
  statpage <- read_html(url)
  tables <- statpage |>
    html_nodes("table") |>
    html_table(fill = TRUE, convert = FALSE)
  
  # Kig efter de tabeller i sidderne og vælg den tabel vi skal arbejde med
  tables[[3]] # tabellen 3 er den der viser informationen vi skal brug
}

# Læs html-indeholdet fra URLs for sæsoner 2002–2026
urls <- c(
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2025%2F2026",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2024%2F2025",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2023%2F2024",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2022%2F2023",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2021%2F2022",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2016%2F2017",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2015%2F2016",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2013%2F2014",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2007%2F2008",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2006%2F2007",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2005%2F2006",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2004%2F2005",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2003%2F2004",
  "https://superstats.dk/hold/sason?id=11&vis=hjemme&aar=2002%2F2003"
)

season_labels <- c(
  "2025/2026", "2024/2025", "2023/2024", "2022/2023", "2021/2022",
  "2016/2017", "2015/2016", "2013/2014", "2007/2008", "2006/2007",
  "2005/2006", "2004/2005", "2003/2004", "2002/2003"
)

# ---- Scrape alle sæsoner ----
all_seasons <- map(urls, get_home_table)
names(all_seasons) <- season_labels

# Gem alle sæesoner samen i en tabel
raw <- bind_rows(all_seasons, .id = "Season")

# Kør for at se dataen 
glimpse(raw)

# ---- Superstats rensning og transformering ----

clean_ml <- raw |>
  # Fjern støj-kolonner + grundlæggende dato-cleaning
  select(-`...7`) |>
  mutate(
    Dato = as.Date(Dato, format = "%d.%m.%Y")
  ) |>
  # Rens "Tilskuere"
  mutate(
    Tilskuere = Tilskuere |>
      str_replace_all("\\.", "") |> # fjern tusindtals-punktum
      na_if("") |> # tom streng -> NA
      as.integer()
  ) |>
  # behold kun kampe der er spillet (har tilskuere)
  filter(!is.na(Tilskuere)) |>
  # Rens og split "Kamp" i HomeTeam / AwayTeam
  mutate(
    Kamp = str_replace_all(Kamp, "–", "-") # fix unicode-dash
  ) |>
  separate(
    Kamp,
    into   = c("HomeTeam", "AwayTeam"),
    sep    = "\\s*-\\s*", # s* fjerner alle spacing, mere safe
    remove = TRUE
  ) |>
  # Rens og split "Res" i HomeGoals / AwayGoals
  mutate(
    Res = str_replace_all(Res, "–", "-") # samme unicode-fix
  ) |>
  separate(
    Res,
    into   = c("HomeGoals", "AwayGoals"),
    sep    = "\\s*-\\s*",
    remove = TRUE
  ) |>
  mutate(
    HomeGoals = as.integer(HomeGoals),
    AwayGoals = as.integer(AwayGoals)
  ) |>
  # Udvalgte variable i rækkefølgende layout til dataset
  select(
    Season,
    Rnd,
    Date = Dato,
    HomeTeam,
    AwayTeam,
    HomeGoals,
    AwayGoals,
    Attendance = Tilskuere
  ) |>
  arrange(Date)

# Tjek dataen 
glimpse(clean_ml)

# ---- Gem som csv ----
# write_csv(clean_ml, "clean_ml.csv")
# View(clean_ml)

# ================ SUPERSTATS KAMPTID WEBSCRAPE ================ 
# ---- URLs + labels ----
program_urls <- c( # links til SuperStats kampprogram pr. sæson
  "https://superstats.dk/program?season=2026",
  "https://superstats.dk/program?season=2025",
  "https://superstats.dk/program?season=2024",
  "https://superstats.dk/program?season=2023",
  "https://superstats.dk/program?season=2022",
  "https://superstats.dk/program?season=2017",
  "https://superstats.dk/program?season=2016",
  "https://superstats.dk/program?season=2014",
  "https://superstats.dk/program?season=2008",
  "https://superstats.dk/program?season=2007",
  "https://superstats.dk/program?season=2006",
  "https://superstats.dk/program?season=2005",
  "https://superstats.dk/program?season=2004",
  "https://superstats.dk/program?season=2003"
)

season_labels_program <- c(
  "2025/2026", "2024/2025", "2023/2024", "2022/2023", "2021/2022",
  "2016/2017", "2015/2016", "2013/2014",
  "2007/2008", "2006/2007", "2005/2006", "2004/2005",
  "2003/2004", "2002/2003"
)

# ---- Scrape funktion ----
# scraper én program-side (én sæson)
scrape_kampprogram_season <- function(url, season_label) {
  
  page <- read_html(url)
  
  tables <- page |>
    html_elements("table") |>
    html_table(fill = TRUE)
  
  if (length(tables) == 0) return(tibble())   # hvis siden ikke indeholder tabeller, returner tom tibble
  
  # behold kun tabeller der indeholder runde-information
  runde_tables <- purrr::keep(tables, \(tb) {
    nms <- names(tb)
    any(str_detect(nms, "^Runde\\s+\\d+")) || any(str_detect(unlist(tb), "^Runde\\s+\\d+"))
  })
  
  if (length(runde_tables) == 0) return(tibble())
  
  purrr::map_dfr(runde_tables, \(tb) {
    
    # udtræk runde-nummer fra kolonnenavn
    round_nr <- names(tb) |>
      str_extract("^Runde\\s+\\d+") |>
      na.omit() |>
      unique() |>
      str_extract("\\d+") |>
      as.integer()
    
    if (length(round_nr) == 0) round_nr <- NA_integer_
    
    df <- as_tibble(tb, .name_repair = "unique")
    
    df2 <- df |> # saml hele rækken til én tekststreng (kolonner varierer mellem sæsoner)
      mutate(
        row_txt = purrr::pmap_chr(across(everything()), \(...) paste(na.omit(c(...)), collapse = " ")) |>
          str_squish()
      )
    
    out <- df2 |>     # parse dato, tid og hold fra tekst
      mutate(
        dato_raw  = str_extract(row_txt, "\\b\\d{1,2}/\\d{1,2}\\b"),
        tid_raw   = str_extract(row_txt, "\\b\\d{1,2}:\\d{2}\\b"),
        teams_raw = str_extract(row_txt, "\\b[A-ZÆØÅ]{2,6}-[A-ZÆØÅ]{2,6}\\b")
      ) |>
      filter(!is.na(dato_raw), !is.na(tid_raw), !is.na(teams_raw)) |>
      separate(teams_raw, into = c("HomeTeam", "AwayTeam"), sep = "-", remove = TRUE) |>
      mutate(
        Season = season_label,
        Round = round_nr,
        
        # udled årstal ud fra sæson (DD/MM uden år i programmet)
        start_year = as.integer(str_sub(Season, 1, 4)),
        end_year   = as.integer(str_sub(Season, 6, 9)),
        
        d1 = as.integer(str_extract(dato_raw, "^\\d{1,2}")),
        d2 = as.integer(str_extract(dato_raw, "\\d{1,2}$")),
        dd = d1,
        mm = d2,
        
        yyyy = if_else(mm >= 7, start_year, end_year),
        
        Date = as.Date(sprintf("%04d-%02d-%02d", yyyy, mm, dd)),
        Time = tid_raw,
        
        # standardiser Viborg-navn
        HomeTeam = if_else(str_detect(str_to_lower(HomeTeam), "viborg"), "VFF", HomeTeam),
        AwayTeam = if_else(str_detect(str_to_lower(AwayTeam), "viborg"), "VFF", AwayTeam)
      ) |>
      select(Season, Round, Date, Time, HomeTeam, AwayTeam)
    
    out
  }) |>
    distinct() |> # fjern evt. dubletter
    arrange(Season, Round, Date, Time)
}

# Scrape alle sæsoner og binder dem sammen
program_rounds_final <- purrr::map2_dfr(
  program_urls,
  season_labels_program,
  scrape_kampprogram_season
)

kamptid_vff <- program_rounds_final |> # Filter til VFF hjemmekampe
  filter(HomeTeam == "VFF") 

glimpse(kamptid_vff)
#view(kamptid_vff)

# ================ SUPERSTATS LIGAPLACERING WEBSCRAPE ================ 
# URLs til ligaplacering
ligaplacering_urls <- c(
  "https://superstats.dk/stilling/pladser-runde?id=&season=2026", 
  "https://superstats.dk/stilling/pladser-runde?id=&season=2025",  
  "https://superstats.dk/stilling/pladser-runde?id=&season=2024",  
  "https://superstats.dk/stilling/pladser-runde?id=&season=2023",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2022",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2017",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2016",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2014",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2008",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2007",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2006",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2005",   
  "https://superstats.dk/stilling/pladser-runde?id=&season=2004",
  "https://superstats.dk/stilling/pladser-runde?id=&season=2003")  

# Labels der matcher 
season_labels_ligaplacering <- c(
  "2025/2026", "2024/2025", "2023/2024", "2022/2023", "2021/2022", "2016/2017", "2015/2016", "2013/2014", "2007/2008", "2006/2007", "2005/2006", "2004/2005", "2003/2004", "2002/2003")

# ---- Scrape funktion ----

# funktion der scraper EN sæson OG tilføjer sæson direkte
get_ligaplacering_one <- function(url, season_labels_ligaplacering) {
  page <- read_html(url)
  
  tables <- page |>
    html_nodes("table") |>
    html_table(fill = TRUE, convert = FALSE)
  
  tbl <- tables[[1]]
  
  # sørg for at første kolonne er klub
  names(tbl)[1] <- "klub"
  
  tbl |>
    mutate(sæson = season_labels_ligaplacering)
}

# scrape alle sæsoner – én tabel per sæson
all_ligaplacering <- map2(ligaplacering_urls, season_labels_ligaplacering, get_ligaplacering_one)

# bind sammen – NU har alle rækker en korrekt sæson-tekst
raw_ligaplacering <- bind_rows(all_ligaplacering) |> 
  select(sæson, everything())

glimpse(raw_ligaplacering)
# view(raw_ligaplacering)
# ---- Ligaplacering rensning og transformering ----

ligaplacering_clean <- raw_ligaplacering |>
  filter(!is.na(klub), klub != "") |> 
  pivot_longer(
    cols      = -c(sæson, klub),
    names_to  = "runde",
    values_to = "ligaplacering"
  ) |>
  mutate(
    runde        = as.integer(runde),
    klub         = str_trim(klub),
    ligaplacering = suppressWarnings(as.integer(ligaplacering))
  ) |>
  filter(!is.na(ligaplacering)) |>
  select(sæson, runde, klub, ligaplacering) |>
  arrange(sæson, runde, ligaplacering)

glimpse(ligaplacering_clean)
# View(ligaplacering_clean)

# ---- Gem som CSV / RDS ----
# write_csv(ligaplacering_clean, "ligaplacering_long.csv")
# saveRDS(ligaplacering_clean, "ligaplacering_long.rds")

# ================ HELLIGDAGE WEBSCRAPE ================ 
# ---- Scrape funktion ----

# Henter alle hellidage i Danmark
get_holidays_year <- function(year) {
  url <- paste0(
    "https://date.nager.at/api/v3/PublicHolidays/",
    year,
    "/DK" 
  )
  
  resp <- GET(url)
  
  # læs JSON-tekst
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  # lav til data.frame/tibble
  dat  <- jsonlite::fromJSON(txt, flatten = TRUE)
  
  # til tibble + tilføj år og dato som Date
  tibble::as_tibble(dat, .name_repair = "unique") |>
    dplyr::transmute(
      year      = year,
      date      = as.Date(date),
      localName = localName,
      name      = name,
      global    = global
    )
}

#  
years <- 2002:2025 # en bedre metode end webscraping med superstats for datoer

holidays_list <- purrr::map(years, get_holidays_year)
holidays_all  <- dplyr::bind_rows(holidays_list)

glimpse(holidays_all)

# ---- Helligdage Rensning og transformering ----

hd_clean <- holidays_all |>
  select(date, localName) |>
  rename(
    Date = date,
    Name = localName
  )

# View(hd_clean)

# Add day-after column (used only for filtering)
hd_clean_dayafter <- hd_clean |>
  mutate(Day_after = Date + days(1))

# Kamp dates
kamp_dates <- clean_ml$Date

# Holidays ON same day as kamp
helligdage_on <- hd_clean_dayafter |>
  filter(Date %in% kamp_dates) |>
  mutate(type = "kamp_dag")

# Holidays the DAY AFTER a kamp
helligdage_after <- hd_clean_dayafter |>
  filter(Day_after %in% kamp_dates) |>
  mutate(type = "dagen_efter")

# Combine + sort + keep only the columns you want
helligdage_relevant <- bind_rows(helligdage_on, helligdage_after) |>
  arrange(Date) |>
  select(Date, Name, type)

# View(helligdage_relevant)

# ---- Gem som CSV ----
# write_csv(helligdage_relevant, "helligdage_relevant.csv")

# ================ DMI WEBSCRAPE RELEVANT DATOER ================ 
# ---- Scrape funktion ---- 
# bygge vores url til at hente fra DMIs hjemmesiden

dmi_list <- list() # tom list til resultaterne

base_url <- "https://dmigw.govcloud.dk/v2/"
info_url <- "metObs/collections/observation/items?"
api_key  <- Sys.getenv("MY_API_KEY") #api nøgle hentes fra environment
station_id <- "06060"   #station er sat til Karup

glimpse(clean_ml)

hjemmekampe_datoer <- unique(clean_ml$Date) # unique så vi ikke får samme dag

# laver vi en list med alle kampdatoer som skal bruges til funktion
dmi_list <- vector("list", length(hjemmekampe_datoer))

for (i in seq_along(hjemmekampe_datoer)) {
  
  dato_raw <- hjemmekampe_datoer[i]
  
  dato <- as_datetime(dato_raw, tz = "UTC") + hours(14)
  message("Henter dmi for: ", dato)
  
  from <- format(dato, "%Y-%m-%dT%H:%M:%SZ")
  to <- from
  
  req_url <- paste0(
    "stationId=", station_id,
    "&datetime=", from, "/", to,
    "&limit=10000",
    "&api-key=", api_key
  )
  
  full_url <- paste0(base_url, info_url, req_url)
  
  # API call
  api_call <- httr::GET(full_url)
  httr::stop_for_status(api_call)
  
  api_char <- rawToChar(api_call$content)
  api_JSON <- jsonlite::fromJSON(api_char, flatten = TRUE)
  
  if (!("features" %in% names(api_JSON)) ||
      is.null(api_JSON$features) || NROW(api_JSON$features) == 0) {
    message("Ingen data for kamp ", dato_raw)
    next
  }
  
  df_year_long <- api_JSON$features |>
    as_tibble() |>
    transmute(
      Observationstidspunkt = properties.observed,
      Observationer         = properties.parameterId,
      Value                 = properties.value
    )
  
  df_year_wide <- df_year_long |>
    pivot_wider(
      id_cols     = Observationstidspunkt,
      names_from  = Observationer,
      values_from = Value
    ) |>
    select(
      Observationstidspunkt, any_of(c("precip_past1h", "temp_dry", "wind_speed"))
    ) |>
    mutate(
      datotid_utc = ymd_hms(Observationstidspunkt, tz = "UTC"),
      datotid_dk  = with_tz(datotid_utc, tzone = "Europe/Copenhagen")
    ) |>
    arrange(datotid_utc)
  
  dmi_list[[i]] <- df_year_wide
}

dmi_all <- bind_rows(dmi_list)

str(dmi_all)
range(dmi_all$datotid_utc)
# View(dmi_all)

# write_csv(dmi_all, "dmi_all.csv")

# ---- DMI Rensning og transformering ----

dmi_clean <- dmi_all  |> 
  # Lav ordentlige kolonnenavne
  rename(
    timestamp = Observationstidspunkt,
    temp = temp_dry,
    precipitation = precip_past1h,
    wind = wind_speed
  ) |> 
  
  # Konverter timestamp til rigtig tid
  mutate(
    timestamp = ymd_hms(timestamp, tz = "UTC"),
    date_dk = with_tz(timestamp, "Europe/Copenhagen"),
    date = as.Date(date_dk)
  )  |> 
  
  # Fjern alle rækker uden temperatur
  filter(!is.na(temp)) |> 
  
  # behold kun de kolonner der er relevant til os 
  select(date, temp, precipitation, wind)

# view(dmi_clean)

# ---- Gem som CSV ----
# write_csv(dmi_clean,"dmi_clean.csv")


# ================ GOOGLE TRENDS FRA GOOGLE DATA ================ 

# Dataen selv kan hentes fra https://trends.google.com/trends/explore?geo=DK&q=%2Fm%2F07zqnm&hl=en-GB i CSV format
gt <- read_csv(
  "Googletrends.csv",
  skip = 3,                                    # skips metadata + blank line
  col_names = c("month", "google_trend"),      # rename columns
  show_col_types = FALSE)  

gt <- gt |> #små klargøring af data til videre brug
  mutate(
    month = ym(month),                 # convert YYYY-MM to proper Date format
    google_trend = as.numeric(google_trend)) |> 
  arrange(month)

glimpse(gt)
# View(gt)

# ---- Gem som csv ----
#write_csv(gt, "clean_googletrends.csv")

# ================ TILSKUERE FRA VFFKORT FIL ================ 
# ---- Importering af fil ----
kort <- readRDS("vffkort01.rds") # Indlæse data fra filen fra Bjarne

glimpse(kort)
# view(kort)

# write_csv(kort, "vffkort.csv", row.names = FALSE)

# ---- Vffkort01 Rensning og transformering ----
kort |> 
  summarise(across(everything(), ~sum(is.na(.)))) # overblik over manglende værdier (NA) i datasættet

kort |> # filterer det relevant kolonner til at beholde kun rækker hvor der findes historisk tilskuertal
  filter(
    !is.na(d10_tilskuere), !is.na(d7_tilskuere), !is.na(d3_tilskuere))

kort_clean <- kort |> # laver en renset version af tabellen, med de værdi vi skal bruge og opdeling der matcher vores fremtidig join
  separate(
    hold, # splitter kolonnen 'hold' op i hjemme- og udehold
    into   = c("HomeTeam", "AwayTeam"),
    sep    = "\\s*-\\s*", # s* fjerner alle spacing, mere sikkert
    remove = TRUE
  ) |> 
  mutate( # trim til at sikre vi arbejde med de samme acrymon, uden ekstra spacing
    HomeTeam = str_trim(HomeTeam),
    AwayTeam = str_trim(AwayTeam),
    sæson    = str_trim(sæson),
    runde    = as.integer(runde),
    AwayTeam = if_else(AwayTeam == "SDR", "SJF", AwayTeam)) |> # vi ændret SDR til SJF da de er de samme hold = Sønderjyllands Fodbold
  select(-år) 

# ---- Gem som CSV ----
#write_csv(kort_clean, "kort_clean.csv", row.names = FALSE)
# view(kort_clean)

# ================ MASTER DATASÆT ================ 
# ---- Opret SQLite database forbindelse ----
con <- dbConnect(RSQLite::SQLite(), "fodbolddata.sqlite")

# ----  Forberedelse af alle datasætterne ----
# Vi forbereder dataen med mutate og rename bare for at være sikker alle kolonner hedder den samme og har samme type

# Hjemmekampe data (clean_ml)       #fra superstats
# summary(clean_ml) #tjekker datasættene for at se hvad skal ændres
df_hjemmekampe <- clean_ml  |> 
  rename(dato = Date,              #omdøb til dansk
         sæson = Season,
         runde = Rnd,
         hjemmehold = HomeTeam,
         udehold = AwayTeam,
         hjemme_mål = HomeGoals,
         ude_mål = AwayGoals,
         tilskuere = Attendance)  |>        
  mutate(dato = as.Date(dato),           #laver kolonner type for match
         runde = as.integer(runde),
         måned_nøgle = floor_date(dato, "month"))
summary(df_hjemmekampe) #tjekker om alt er klar i dataset

# Kamptid data (kamptid_vff)
summary(kamptid_vff)
df_kamptid <- kamptid_vff |>
  rename(sæson = Season,
         runde = Round,
         dato = Date,
         tid = Time,
         hjemmehold = HomeTeam,
         udehold = AwayTeam) |>
  mutate(dato = as.Date(dato)) 
summary(df_kamptid)

# Ligaplacering data (ligaplacering_clean)
# summary(ligaplacering_clean) #tjekker datasættene for at se hvad skal ændres
df_ligaplacering <- ligaplacering_clean  |> 
  mutate(ligaplacering = as.factor(ligaplacering))   #laver kolonner type for match
summary(df_ligaplacering) #tjekker om alt er klar i dataset

# Vejr fra DMI (dmi_clean) 
# summary(dmi_clean) #tjekker datasættene for at se hvad skal ændres
df_dmi <- dmi_clean  |> 
  rename(dato = date,
         vind = wind,
         precip = precipitation)  |>         
  mutate(dato = as.Date(dato))   #laver kolonner type for match 
#bare for at være sikker siden det er den kolonner vi bruger til at joine
summary(df_dmi) #tjekker om alt er klar i dataset

# Danske Helligdage (helligdage_relevant)
# summary(helligdage_relevant) #tjekker datasættene for at se hvad skal ændres
df_helligdage <- helligdage_relevant  |> 
  rename(dato = Date,
         helligdage_navn = Name) |> 
  mutate(dato = as.Date(dato))   #laver kolonner type for match
summary(df_helligdage) #tjekker om alt er klar i dataset

# Google Trends (gt) 
# summary(gt) #tjekker datasættene for at se hvad skal ændres
df_trends <- gt |> 
  rename(måned_nøgle = month) 
summary(df_trends) #tjekker om alt er klar i dataset

# Tilskuer-historik (kort_clean)
# summary(kort_clean) #tjekker datasættene for at se hvad skal ændres
df_tilskuere <- kort_clean  |> 
  rename(hjemmehold = HomeTeam,
         udehold = AwayTeam) |> 
  select(sæson, runde, tilskuere, hjemmehold, udehold, d10_tilskuere, d7_tilskuere, d3_tilskuere) |>  #select kolonner vi skal bruge
  distinct(sæson, runde, hjemmehold, udehold, .keep_all = TRUE) #der var 5 kampe med duplikate række når man gruppere sæson, runde, hjemmehold og udehold, med distinct filtrerer vi til én
summary(df_tilskuere)

# ----  Skriv databaser som SQL i SQLite ---- 
dbWriteTable(con, "hjemmekampe", df_hjemmekampe, overwrite = TRUE)
dbWriteTable(con, "dmi", df_dmi, overwrite = TRUE)
dbWriteTable(con, "helligdage", df_helligdage, overwrite = TRUE)
dbWriteTable(con, "google_trends", df_trends, overwrite = TRUE)
dbWriteTable(con, "tilskuere_historik", df_tilskuere, overwrite = TRUE)
dbWriteTable(con, "ligaplacering", df_ligaplacering, overwrite = TRUE)
dbWriteTable(con, "kamptid", df_kamptid, overwrite = TRUE)

# ========= BYG MASTERDATA I SQL =========
# ---- VIEW 1 - hjemmekampe master ----
dbExecute(con, "DROP VIEW IF EXISTS hjemmekampe_master;") #oppret SQL dataset og slette hvis den allerede eksistere
dbListTables(con) #tjekker tabeller der ligger ind i SQLite fil

sql_hjemmekampe_master <- "
CREATE VIEW hjemmekampe_master AS 
SELECT hjemmekampe.*, kt.tid, d.temp, d.precip, d.vind, h.helligdage_navn, t.d10_tilskuere, t.d7_tilskuere, t.d3_tilskuere, g.google_trend, lh.ligaplacering AS vff_placering, lu.ligaplacering AS mod_placering,
 CASE WHEN h.type = 'kamp_dag' THEN 'ja'
    ELSE 'nej'
  END AS kamp_pa_helligdage,
  CASE WHEN h.type = 'dagen_efter' THEN 'ja'
    ELSE 'nej'
  END AS dagen_efter_helligdage
FROM hjemmekampe
LEFT JOIN dmi as d
ON hjemmekampe.dato = d.dato
LEFT JOIN helligdage AS h
ON hjemmekampe.dato = h.dato
LEFT JOIN google_trends AS g
ON hjemmekampe.måned_nøgle = g.måned_nøgle
LEFT JOIN tilskuere_historik AS t
ON hjemmekampe.sæson = t.sæson
AND hjemmekampe.runde = t.runde
AND hjemmekampe.hjemmehold = t.hjemmehold
AND hjemmekampe.udehold = t.udehold
LEFT JOIN ligaplacering AS lh
ON hjemmekampe.sæson = lh.sæson
AND hjemmekampe.runde = lh.runde
AND hjemmekampe.hjemmehold = lh.klub
LEFT JOIN ligaplacering AS lu
ON hjemmekampe.sæson = lu.sæson
AND hjemmekampe.runde = lu.runde
AND hjemmekampe.udehold = lu.klub
LEFT JOIN kamptid as kt
ON hjemmekampe.sæson = kt.sæson
AND hjemmekampe.runde = kt.runde
AND hjemmekampe.dato = kt.dato
AND hjemmekampe.udehold = kt.udehold
GROUP BY hjemmekampe.sæson, 
         hjemmekampe.runde,
         hjemmekampe.hjemmehold,
         hjemmekampe.udehold,
         hjemmekampe.dato
HAVING COUNT(*) = 1
       AND kt.tid IS NOT NULL
       AND hjemmekampe.tilskuere IS NOT NULL
ORDER BY hjemmekampe.sæson DESC"

# matcher alle tabeller sammen og sikre at vi har unikke resultaterne med having
# omdøbber helligdage kolonne i join, til ja/nej
# for at sikre at vores ligaplacering indeholder vff og modstander, bliver 
# ligaplacering joined 2 gang med forskellige navn
dbExecute(con, sql_hjemmekampe_master)

# ---- Hent viewerne tilbage i R ----
# hjemmekampe_master
hjemmekampe_master <- dbGetQuery(con, "SELECT * FROM hjemmekampe_master;") |>
  as_tibble() |>
  mutate(
    dato = as.Date(dato),
    sæson = as.factor(sæson),
    runde = as.integer(runde),
    tilskuere = as.integer(tilskuere),
    temp = as.numeric(temp),
    precip = as.numeric(precip),
    vind = as.numeric(vind),
    google_trend = as.numeric(google_trend),
    d10_tilskuere = as.integer(d10_tilskuere),
    d7_tilskuere  = as.integer(d7_tilskuere),
    d3_tilskuere  = as.integer(d3_tilskuere),
    kamp_pa_helligdage = factor(kamp_pa_helligdage, levels = c("nej", "ja")),
    dagen_efter_helligdage = factor(dagen_efter_helligdage, levels = c("nej", "ja")),
    vff_placering = as.integer(vff_placering),
    mod_placering = as.integer(mod_placering),
    tid = hms::parse_hm(tid))


glimpse(hjemmekampe_master)
# view(hjemmekampe_master)

# ---- Gem som RDS og CSV ---- 
# saveRDS(hjemmekampe_master, "rds/hjemmekampe_master.rds")
# write_csv(hjemmekampe_master, "csv/hjemmekampe_master.csv")

# ---- Luk forbindelse ---- 
dbDisconnect(con)

# ========= FEATURE ENGINEERING =========

# tjekker om der er dubletter efter joins
sum(duplicated(hjemmekampe_master))

# laver ekstra features, retter type af kolonner efter at den kommer ud af SQLite og tilpasse master til brug
hjemmekampe_features <- hjemmekampe_master |> 
  filter(!sæson %in% c("2002/2003", "2003/2004")) |> # slette kampe fra 2002/2003 & 2003/2004 da vi manglede DMI og Google trends til disse år
  mutate(dato = as.Date(dato),
         år = lubridate::year(dato),
         weekday = wday(dato, label = TRUE, week_start = 1), # tilføje ugedag til kamperne
         weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         weekend = if_else(weekday %in% c("Sat", "Sun"), 1L, 0L), #laver dummy til weekend eller ugedag
         weekend = factor(weekend, levels = c(0, 1),
                          labels = c("hverdag", "weekend")),
         måned = month(dato),
         årstid = case_when(
           måned %in% c(12, 1, 2) ~ "vinter",
           måned %in% c(3, 4, 5) ~ "forår",
           måned %in% c(6, 7, 8) ~ "sommer",
           måned %in% c(9, 10, 11) ~ "efterår"), # tilføje årstid til kamperne
         årstid = factor(årstid, levels = c("vinter", "forår", "sommer", "efterår")),
         regn = case_when(
           is.na(precip) ~ NA_character_,
           precip > 0.5  ~ "ja",
           TRUE          ~ "nej"), # regn eller nej
         regn = factor(regn, levels = c("nej", "ja")),
         goal_diff = hjemme_mål - ude_mål,
         vff_perf = case_when(
           goal_diff > 0  ~  1L,
           goal_diff == 0 ~  0L, # vff performance 1 point til vandt kamp, 0 til lige, -1 til tabt 
           TRUE           ~ -1L),
         sæson = factor(sæson),
         kamp_pa_helligdage = factor(kamp_pa_helligdage, levels = c("nej", "ja")),
         dagen_efter_helligdage = factor(dagen_efter_helligdage, levels = c("nej", "ja")),
         hjemmehold = factor(hjemmehold),
         udehold = factor(udehold),
         mod_gruppe = case_when(
           udehold %in% c("BIF", "FCM", "FCK", "AGF") ~ "Top",
           udehold %in% c("AaB", "RFC", "VB", "SIF", "LBK", "OB") ~ "Mellem",
           TRUE ~ "Bund"), # laver modstander gruppe, baseret på gennemsnit tilskuere per modstand
         mod_gruppe = factor(mod_gruppe, levels = c("Bund", "Mellem", "Top"))) |> 
  select(sæson, runde, år, dato, tid, weekday, weekend, årstid, vff_placering, 
         mod_placering, mod_gruppe, hjemmehold, udehold, hjemme_mål, ude_mål, 
         goal_diff, vff_perf, tilskuere, d10_tilskuere, d7_tilskuere, 
         d3_tilskuere, google_trend, regn, precip, temp, vind, everything(), 
         -weekday,  -måned, -måned_nøgle, -helligdage_navn, -dagen_efter_helligdage) |>  #vælge og organisere variable der går ind i vores finale master
  tidyr::drop_na() #fjern alle NA

glimpse(hjemmekampe_features)
# view(hjemmekampe_features)

# ---- Gem som CSV ----
# write_csv(hjemmekampe_features, "csv/hjemmekampe_features.csv")

# =========  EXPLORATORY ANALYSIS =========

# ---- NA Tjek ----
hjemmekampe_eda_tjek <- hjemmekampe_features |> # viser hvor mange NAs er i hver kolonner og hvad skal vi evt. undgå
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variabel", values_to = "na_antal") |> #navner til summary kolonner
  arrange(desc(na_antal))
hjemmekampe_eda_tjek

# ---- Deskriptiv statistik (tilskuere) ----
hjemmekampe_statistik <- hjemmekampe_features |> # laver en summary over vores deskriptiv statistik
  summarise(
    n      = sum(!is.na(tilskuere)),
    mean   = mean(tilskuere, na.rm = TRUE),
    median = median(tilskuere, na.rm = TRUE),
    min    = min(tilskuere, na.rm = TRUE),
    max    = max(tilskuere, na.rm = TRUE),
    sd     = sd(tilskuere, na.rm = TRUE))
hjemmekampe_statistik


# ---- Modstanderens styrke (placering i ligaen på kampetid) ----

hjemmekampe_eda_placering <- hjemmekampe_features |> # tjekker om mod_placering er relevant som forklarende feature
  filter(!is.na(tilskuere), !is.na(mod_placering)) |>
  ggplot(aes(x = mod_placering, y = tilskuere)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Tilskuere vs. modstanderens ligaplacering",
    subtitle = "Lavere placering (stærkere hold) er ofte forbundet med højere tilskuertal",
    x = "Modstanderens placering",
    y = "Tilskuere")

hjemmekampe_eda_placering

# ---- Tilskuere over tid (gennemsnit pr. kl) ----  ###################### ved ikke om måske den her er unøvendigt

hjemmekampe_tilskuere <- hjemmekampe_features |>
  mutate(
    kickoff_hour = lubridate::hour(tid),
    kickoff_label = sprintf("%02d:00", kickoff_hour)
  ) |>
  group_by(kickoff_label, kickoff_hour) |>
  summarise(
    gennemsnit_tilskuere = mean(tilskuere, na.rm = TRUE),
    n_kampe = n(),
    .groups = "drop"
  ) |>
  arrange(kickoff_hour) |>
  ggplot(aes(x = kickoff_label, y = gennemsnit_tilskuere, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  geom_text(aes(label = n_kampe), vjust = -0.7, size = 3) +
  labs(
    title = "Gennemsnitligt tilskuertal efter kickoff-tidspunkt",
    subtitle = "Tal over punkter = antal kampe",
    x = "Kickoff-tid (time)",
    y = "Gennemsnitligt antal tilskuere"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hjemmekampe_tilskuere

# ---- Helligdag vs. ikke helligdag ----

hjemmekampe_eda_helligdage <- hjemmekampe_features |> #  tjekker om der er indflydelse af hellidage
  mutate(
    helligdag = if_else(kamp_pa_helligdage == "ja", "Helligdag", "Ikke helligdag")) |>
  ggplot(aes(x = helligdag, y = tilskuere)) +
  geom_boxplot() +
  labs(
    title = "Tilskuertal: helligdage vs. normale dage",
    x = "",
    y = "Tilskuere")

hjemmekampe_eda_helligdage

# ---- Weekend vs. hverdag ----

hjemmekampe_eda_ugedag <- hjemmekampe_features |> # tjekker om der er indflydelse af ugedag
  ggplot(aes(x = factor(weekend, labels = c("Hverdag", "Weekend")),
             y = tilskuere)) +
  geom_boxplot() +
  labs(
    title = "Tilskuertal: hverdag vs. weekend",
    x = "",
    y = "Tilskuere")

hjemmekampe_eda_ugedag

# ========= MODEL DATA ==========
# ud fra EDA, sletter vi nogle kolonner der ikke giver menning for vores analyse
master_df <- hjemmekampe_features |> 
  select(-hjemmehold, -udehold, -sæson)
glimpse(master_df)

# ---- Opdeling af variabler til 3d, 7d, 10d, og 3m ----
# vælger variabler til hver tidshorisont
keep_vars_3d <- c(
  "runde",
  "weekend",
  "mod_gruppe",
  "årstid",
  "vff_placering",
  "mod_placering",
  "kamp_pa_helligdage",
  "d3_tilskuere",
  "temp",
  "precip", # mest akurat precipitation
  "tid",
  "år")

keep_vars_7d <- c(
  "runde",
  "weekend",
  "årstid",
  "mod_gruppe",
  "vff_placering",
  "mod_placering",
  "kamp_pa_helligdage",
  "d7_tilskuere",
  "tid",
  "år")

keep_vars_10d <- c(
  "runde",
  "weekend",
  "årstid",
  "mod_gruppe",
  "kamp_pa_helligdage",
  "d10_tilskuere",
  "tid",
  "år")

keep_vars_3m <- c(
  "runde",
  "weekend",
  "årstid",
  "kamp_pa_helligdage",
  "mod_gruppe",
  "tid",
  "år")

# ========= Split train og test pr. horisont ========= 

# ---- Hovedsplit på train og test ----
set.seed(10) # seed med vores gruppe nmr

n <- nrow(master_df)
train <- sample(seq_len(n), size = floor(0.80 * n)) # 80% / 20%

train_master <- master_df[train, , drop = FALSE]
test_master <- master_df[-train, , drop = FALSE]

# ---- Stor model (alle variable) ----
train_df <- train_master
test_df <- test_master

# ---- 3 måneder ----
train_df_3m <- train_master |> select(tilskuere, all_of(keep_vars_3m))
test_df_3m <- test_master  |> select(tilskuere, all_of(keep_vars_3m))

# ---- 10 dage ----
train_df_10 <- train_master |> select(tilskuere, all_of(keep_vars_10d))
test_df_10 <- test_master  |> select(tilskuere, all_of(keep_vars_10d))

# ---- 7 dage ----
train_df_7 <- train_master |> select(tilskuere, all_of(keep_vars_7d))
test_df_7 <- test_master  |> select(tilskuere, all_of(keep_vars_7d))


# ---- 3 dage ----
train_df_3  <- train_master |> select(tilskuere, all_of(keep_vars_3d))
test_df_3   <- test_master  |> select(tilskuere, all_of(keep_vars_3d))

# ========= Stor model (LM) ========= 

# stor modellen virker some benchmark, derfor er der kun lm på den 
lm_full <- lm(tilskuere ~ ., data = train_df)
lm_pred <- predict(lm_full, newdata = test_df)
mse_lm <- mean((lm_pred - test_df$tilskuere)^2)
rmse_lm <- sqrt(mse_lm)

mse_lm
rmse_lm

# ========= Subset selection (Best subset + Backward) + CV ========= 

# ---- Hjælpefunktion: prediction til regsubsets ----
predict.regsubsets <- function(object, newdata, id, ...) { 
  form <- as.formula(object$call[[2]]) # henter model-formlen (tilskuere ~ .) fra regsubsets-objektet
  mat <- model.matrix(form, newdata) # laver design-matrix for de nye data (samme struktur som træningsdata)
  coefi <- coef(object, id = id) # henter koefficienter for den valgte subset-model (modelstørrelse id)
  xvars <- names(coefi) # finder hvilke variable der indgår i modellen
  mat[, xvars, drop = FALSE] %*% coefi # beregner forudsigelser via matrix-multiplikation
}

# ---- Subset-modeller: 3 måneder ----
nv <- ncol(train_df_3m) - 1 

# best subset
bestsub.3m <- regsubsets(
  tilskuere ~ .,
  data = train_df_3m,
  nvmax = nv,
  method = "exhaustive")  #### we don't need this line, exhaustive is default on regsubsts

# backward metode
bw.3m <- regsubsets(
  tilskuere ~ .,
  data = train_df_3m,
  nvmax = nv,
  method = "backward")

## ---- Modeludvælgelse by cross validation ----

k <- 10
n <- nrow(train_df_3m)
set.seed(10)

folds <- sample(rep(1:k, length.out = n)) # vektor på længde n, hvor hver række får et fold-nummer
# sample() blander rækkefølgen tilfældigt

cv.errors.bw <- matrix(NA, k, nv) # tom tabeller til backwards 
cv.errors.bs <- matrix(NA, k, nv) # tom tabeller til best subset 

## ---- Cross Validation loop ---- 
# træning på k-1 folds, validering på fold j 

for (j in 1:k) { # (j) én bestemt fold ad gangen ud af (1:k) alle fold-numre
  # vi træner modellen på 9/10 af data og evaluerer på 1/10
  fit.bw <- regsubsets(
    tilskuere ~ .,
    data = train_df_3m[folds != j, ], # træningsdata i CV (uden fold j)
    nvmax = nv,
    method = "backward")
  
  fit.bs <- regsubsets(
    tilskuere ~ .,
    data = train_df_3m[folds != j, ], # samme træningsdata for fair sammenligning
    nvmax = nv,
    method = "exhaustive") # we don't need this line
  
  # ---- Indre loop ----
  # evaluer alle modelstørrelser 1:nv på fold j 
  # Vi vil finde hvilken modelstørrelse der giver lavest gennemsnitlig CV-MSE.
  
  for (i in 1:nv) {  
    pred_bw <- predict.regsubsets( # forudsigelser på valideringsfolden (fold j) for modelstørrelse i
      fit.bw,
      newdata = train_df_3m[folds == j, ],
      id = i)
    pred_bs <- predict.regsubsets(
      fit.bs,
      newdata = train_df_3m[folds == j, ],
      id = i)
    
    # Beregn MSE for fold j og modelstørrelse i
    cv.errors.bw[j, i] <- mean((train_df_3m$tilskuere[folds == j] - pred_bw)^2) # (y - yhat)^2 gennemsnit, her kun for de observationer der ligger i fold j
    cv.errors.bs[j, i] <- mean((train_df_3m$tilskuere[folds == j] - pred_bs)^2) 
  }
}

## ---- Gennemsnitlig CV-MSE pr modelstørrelse (over alle folds) ----
# colMeans tager gennemsnittet ned over rækkerne (folds) for hver kolonne (modelstørrelse)
mean.cv.bw <- colMeans(cv.errors.bw, na.rm = TRUE)
mean.cv.bs <- colMeans(cv.errors.bs, na.rm = TRUE)

## ---- Vælg bedste modelstørrelse (laveste CV-MSE) ----
best.size.bw_3m <- which.min(mean.cv.bw) # optimal størrelse for backward
best.size.bs_3m <- which.min(mean.cv.bs) # optimal størrelse for best subset

# Cross Valitation på test
cv_mse_bw_3m <- mean.cv.bw[best.size.bw_3m]
cv_mse_bs_3m <- mean.cv.bs[best.size.bs_3m]

## ---- Test-evaluering ----
# bruger den valgte modelstørrelse på test-sæt

# vælger den modelstørrelse, som CV fandt bedst
pred_test_bw_3m <- predict.regsubsets( #funktionen vi lavede derop
  bw.3m,
  newdata = test_df_3m,
  id = best.size.bw_3m)

pred_test_bs_3m <- predict.regsubsets(
  bestsub.3m,
  newdata = test_df_3m,
  id = best.size.bs_3m)

## ---- Test-fejl: MSE og RMSE ----

mse_bw_3m <- mean((test_df_3m$tilskuere - pred_test_bw_3m)^2)
rmse_bw_3m <- sqrt(mse_bw_3m)

mse_bs_3m <- mean((test_df_3m$tilskuere - pred_test_bs_3m)^2)
rmse_bs_3m <- sqrt(mse_bs_3m)

# mse_bw_3m
# rmse_bw_3m

# mse_bs_3m
# rmse_bs_3m

## ---- Subset-modeller: 10 dage ----
nv <- ncol(train_df_10) - 1

bestsub.10d <- regsubsets(
  tilskuere ~ .,
  data = train_df_10,
  nvmax = nv,
  method = "exhaustive"
)
bw.10d <- regsubsets(
  tilskuere ~ .,
  data = train_df_10,
  nvmax = nv,
  method = "backward"
)

k <- 10
n <- nrow(train_df_10)
set.seed(10)
folds <- sample(rep(1:k, length.out = n))

cv.errors.bw <- matrix(NA, k, nv)
cv.errors.bs <- matrix(NA, k, nv)

for (j in 1:k) {
  fit.bw <- regsubsets(
    tilskuere ~ .,
    data = train_df_10[folds != j, ],
    nvmax = nv,
    method = "backward"
  )
  fit.bs <- regsubsets(
    tilskuere ~ .,
    data = train_df_10[folds != j, ],
    nvmax = nv,
    method = "exhaustive"
  )
  
  for (i in 1:nv) {
    pred_bw <- predict.regsubsets(
      fit.bw,
      newdata = train_df_10[folds == j, ],
      id = i
    )
    pred_bs <- predict.regsubsets(
      fit.bs,
      newdata = train_df_10[folds == j, ],
      id = i
    )
    
    cv.errors.bw[j, i] <- mean((train_df_10$tilskuere[folds == j] - pred_bw)^2)
    cv.errors.bs[j, i] <- mean((train_df_10$tilskuere[folds == j] - pred_bs)^2)
  }
}

mean.cv.bw <- colMeans(cv.errors.bw, na.rm = TRUE)
mean.cv.bs <- colMeans(cv.errors.bs, na.rm = TRUE)

best.size.bw_10d <- which.min(mean.cv.bw)
best.size.bs_10d <- which.min(mean.cv.bs)

cv_mse_bw_10d <- mean.cv.bw[best.size.bw_10d]
cv_mse_bs_10d <- mean.cv.bs[best.size.bs_10d]

pred_test_bw_10d <- predict.regsubsets(
  bw.10d,
  newdata = test_df_10,
  id = best.size.bw_10d
)
pred_test_bs_10d <- predict.regsubsets(
  bestsub.10d,
  newdata = test_df_10,
  id = best.size.bs_10d
)

mse_bw_10d <- mean((test_df_10$tilskuere - pred_test_bw_10d)^2)
rmse_bw_10d <- sqrt(mse_bw_10d)

mse_bs_10d <- mean((test_df_10$tilskuere - pred_test_bs_10d)^2)
rmse_bs_10d <- sqrt(mse_bs_10d)

# ---- Subset-modeller: 7 dage ----
nv <- ncol(train_df_7) - 1

bestsub.7d <- regsubsets(
  tilskuere ~ .,
  data = train_df_7,
  nvmax = nv,
  method = "exhaustive"
)
bw.7d <- regsubsets(
  tilskuere ~ .,
  data = train_df_7,
  nvmax = nv,
  method = "backward"
)

k <- 10
n <- nrow(train_df_7)
set.seed(10)
folds <- sample(rep(1:k, length.out = n))

cv.errors.bw <- matrix(NA, k, nv)
cv.errors.bs <- matrix(NA, k, nv)

for (j in 1:k) {
  fit.bw <- regsubsets(
    tilskuere ~ .,
    data = train_df_7[folds != j, ],
    nvmax = nv,
    method = "backward"
  )
  fit.bs <- regsubsets(
    tilskuere ~ .,
    data = train_df_7[folds != j, ],
    nvmax = nv,
    method = "exhaustive"
  )
  
  for (i in 1:nv) {
    pred_bw <- predict.regsubsets(
      fit.bw,
      newdata = train_df_7[folds == j, ],
      id = i
    )
    pred_bs <- predict.regsubsets(
      fit.bs,
      newdata = train_df_7[folds == j, ],
      id = i
    )
    
    cv.errors.bw[j, i] <- mean((train_df_7$tilskuere[folds == j] - pred_bw)^2)
    cv.errors.bs[j, i] <- mean((train_df_7$tilskuere[folds == j] - pred_bs)^2)
  }
}

mean.cv.bw <- colMeans(cv.errors.bw, na.rm = TRUE)
mean.cv.bs <- colMeans(cv.errors.bs, na.rm = TRUE)

best.size.bw_7d <- which.min(mean.cv.bw)
best.size.bs_7d <- which.min(mean.cv.bs)

cv_mse_bw_7d <- mean.cv.bw[best.size.bw_7d]
cv_mse_bs_7d <- mean.cv.bs[best.size.bs_7d]

pred_test_bw_7d <- predict.regsubsets(
  bw.7d,
  newdata = test_df_7,
  id = best.size.bw_7d
)
pred_test_bs_7d <- predict.regsubsets(
  bestsub.7d,
  newdata = test_df_7,
  id = best.size.bs_7d
)

mse_bw_7d <- mean((test_df_7$tilskuere - pred_test_bw_7d)^2)
rmse_bw_7d <- sqrt(mse_bw_7d)

mse_bs_7d <- mean((test_df_7$tilskuere - pred_test_bs_7d)^2)
rmse_bs_7d <- sqrt(mse_bs_7d)

# ---- Subset-modeller: 3 dage ----
nv <- ncol(train_df_3) - 1

bestsub.3d <- regsubsets(
  tilskuere ~ .,
  data = train_df_3,
  nvmax = nv,
  method = "exhaustive")

bw.3d <- regsubsets(
  tilskuere ~ .,
  data = train_df_3,
  nvmax = nv,
  method = "backward")

k <- 10
n <- nrow(train_df_3)
set.seed(10)
folds <- sample(rep(1:k, length.out = n))

cv.errors.bw <- matrix(NA, k, nv)
cv.errors.bs <- matrix(NA, k, nv)

for (j in 1:k) {
  fit.bw <- regsubsets(
    tilskuere ~ .,
    data = train_df_3[folds != j, ],
    nvmax = nv,
    method = "backward"
  )
  fit.bs <- regsubsets(
    tilskuere ~ .,
    data = train_df_3[folds != j, ],
    nvmax = nv,
    method = "exhaustive"
  )
  
  for (i in 1:nv) {
    pred_bw <- predict.regsubsets(
      fit.bw,
      newdata = train_df_3[folds == j, ],
      id = i
    )
    pred_bs <- predict.regsubsets(
      fit.bs,
      newdata = train_df_3[folds == j, ],
      id = i
    )
    
    cv.errors.bw[j, i] <- mean((train_df_3$tilskuere[folds == j] - pred_bw)^2)
    cv.errors.bs[j, i] <- mean((train_df_3$tilskuere[folds == j] - pred_bs)^2)
  }
}

mean.cv.bw <- colMeans(cv.errors.bw, na.rm = TRUE)
mean.cv.bs <- colMeans(cv.errors.bs, na.rm = TRUE)

best.size.bw_3d <- which.min(mean.cv.bw)
best.size.bs_3d <- which.min(mean.cv.bs)

cv_mse_bw_3d <- mean.cv.bw[best.size.bw_3d]
cv_mse_bs_3d <- mean.cv.bs[best.size.bs_3d]

pred_test_bw_3d <- predict.regsubsets(
  bw.3d,
  newdata = test_df_3,
  id = best.size.bw_3d
)
pred_test_bs_3d <- predict.regsubsets(
  bestsub.3d,
  newdata = test_df_3,
  id = best.size.bs_3d
)

mse_bw_3d <- mean((test_df_3$tilskuere - pred_test_bw_3d)^2)
rmse_bw_3d <- sqrt(mse_bw_3d)

mse_bs_3d <- mean((test_df_3$tilskuere - pred_test_bs_3d)^2)
rmse_bs_3d <- sqrt(mse_bs_3d)

# =========== Ridge (alpha = 0) ===========

# ---- Ridge: 3 måneder ----
# Konstruktion af design-matricer 
# glmnet kræver numeriske input i matrix-form.
# model.matrix() konverterer faktorer til dummy-variabler
# og sikrer korrekt design-matrix svarende til lm().
# [-1] fjerner intercept-kolonnen, da glmnet selv håndterer intercept.
x_train_3m <- model.matrix(tilskuere ~ ., data = train_df_3m)[, -1]
y_train_3m <- train_df_3m$tilskuere
x_test_3m <- model.matrix(tilskuere ~ ., data = test_df_3m)[, -1]
y_test_3m <- test_df_3m$tilskuere

# Estimering af Ridge-model med cross-validation
# cv.glmnet() udfører k-fold cross-validation og finder den optimale regulariseringsparameter lambda.
# alpha = 0 specificerer Ridge regression.
cv_ridge_3m <- cv.glmnet(x = x_train_3m, y = y_train_3m, alpha = 0, nfolds = 10)

# Prediction på test-sættet med optimal lambda
# lambda.min er den lambda-værdi, der minimerer CV-MSE.
ridge_pred_3m <- predict(cv_ridge_3m, s = "lambda.min", newx = x_test_3m) # Modellen evalueres på et separat test-sæt for at vurdere generaliseringsevnen

# Performance-mål
# MSE anvendes som kvadreret fejlmål, mens RMSE bruges i rapportering, da den har samme enhed som responsvariablen (antal tilskuere)
mse_ridge_3m <- mean((y_test_3m - ridge_pred_3m)^2)
rmse_ridge_3m <- sqrt(mse_ridge_3m)

# ---- Ridge: 10 dage ----
x_train_10 <- model.matrix(tilskuere ~ ., data = train_df_10)[, -1]
y_train_10 <- train_df_10$tilskuere
x_test_10 <- model.matrix(tilskuere ~ ., data = test_df_10)[, -1]
y_test_10 <- test_df_10$tilskuere

cv_ridge_10 <- cv.glmnet(x = x_train_10, y = y_train_10, alpha = 0, nfolds = 10)
ridge_pred_10 <- predict(cv_ridge_10, s = "lambda.min", newx = x_test_10)

mse_ridge_10 <- mean((y_test_10 - ridge_pred_10)^2)
rmse_ridge_10 <- sqrt(mse_ridge_10)

# ---- Ridge: 7 dage ----
x_train_7 <- model.matrix(tilskuere ~ ., data = train_df_7)[, -1]
y_train_7 <- train_df_7$tilskuere
x_test_7 <- model.matrix(tilskuere ~ ., data = test_df_7)[, -1]
y_test_7 <- test_df_7$tilskuere

cv_ridge_7 <- cv.glmnet(x = x_train_7, y = y_train_7, alpha = 0, nfolds = 10)
ridge_pred_7 <- predict(cv_ridge_7, s = "lambda.min", newx = x_test_7)

mse_ridge_7 <- mean((y_test_7 - ridge_pred_7)^2)
rmse_ridge_7 <- sqrt(mse_ridge_7)

# ---- Ridge: 3 dage ----
x_train_3 <- model.matrix(tilskuere ~ ., data = train_df_3)[, -1]
y_train_3 <- train_df_3$tilskuere
x_test_3 <- model.matrix(tilskuere ~ ., data = test_df_3)[, -1]
y_test_3 <- test_df_3$tilskuere

cv_ridge_3 <- cv.glmnet(x = x_train_3, y = y_train_3, alpha = 0, nfolds = 10)
ridge_pred_3 <- predict(cv_ridge_3, s = "lambda.min", newx = x_test_3)

mse_ridge_3 <- mean((y_test_3 - ridge_pred_3)^2)
rmse_ridge_3 <- sqrt(mse_ridge_3)

# ---- CV MSE Ridge ----
# OBS: behold dine oprindelige navne her
cv_mse_ridge_3m <- min(cv_ridge_3m$cvm)
cv_mse_ridge_10 <- min(cv_ridge_10$cvm)
cv_mse_ridge_7 <- min(cv_ridge_7$cvm)
cv_mse_ridge_3 <- min(cv_ridge_3$cvm)

# =========== Lasso (alpha = 1) ===========
# reducerer varians og udfører variabelselektion ved at sætte nogle koefficienter præcist lig 0.
# Dette resulterer i en mere parsimonisk model end Ridge

# ---- Lasso: 3 måneder ----
# Estimering af Lasso-model med cross-validation
# laver den samme som ridge men med alpha = 1 her
cv_lasso_3m <- cv.glmnet(x = x_train_3m, y = y_train_3m, alpha = 1, nfolds = 10)

# Prediction på test-sættet med optimal lambda
lasso_pred_3m <- predict(cv_lasso_3m, s = "lambda.min", newx = x_test_3m)

# Performance-mål
#igen samme forklaring som Ridge
mse_lasso_3m <- mean((y_test_3m - lasso_pred_3m)^2)
rmse_lasso_3m <- sqrt(mse_lasso_3m)

# ---- Lasso: 10 dage ----
cv_lasso_10 <- cv.glmnet(x = x_train_10, y = y_train_10, alpha = 1, nfolds = 10)
lasso_pred_10 <- predict(cv_lasso_10, s = "lambda.min", newx = x_test_10)

mse_lasso_10 <- mean((y_test_10 - lasso_pred_10)^2)
rmse_lasso_10 <- sqrt(mse_lasso_10)

# ---- Lasso: 7 dage ----
cv_lasso_7 <- cv.glmnet(x = x_train_7, y = y_train_7, alpha = 1, nfolds = 10)
lasso_pred_7 <- predict(cv_lasso_7, s = "lambda.min", newx = x_test_7)

mse_lasso_7 <- mean((y_test_7 - lasso_pred_7)^2)
rmse_lasso_7 <- sqrt(mse_lasso_7)

# ---- Lasso: 3 dage ----
cv_lasso_3 <- cv.glmnet(x = x_train_3, y = y_train_3, alpha = 1, nfolds = 10)
lasso_pred_3 <- predict(cv_lasso_3, s = "lambda.min", newx = x_test_3)

mse_lasso_3 <- mean((y_test_3 - lasso_pred_3)^2)
rmse_lasso_3 <- sqrt(mse_lasso_3)

# ---- CV MSE LASSO ----
# OBS: behold dine oprindelige navne her
cv_mse_lasso_3m <- min(cv_lasso_3m$cvm)
cv_mse_lasso_10 <- min(cv_lasso_10$cvm)
cv_mse_lasso_7 <- min(cv_lasso_7$cvm)
cv_mse_lasso_3 <- min(cv_lasso_3$cvm)

# =========== Samlet oversigt (MSE + RMSE) ===========

# En tibble med alle resultaterne så vi kan klart se hvilken performer bedst
results <- tibble(
  model = c("Backward", "Best subset", "Ridge", "Lasso"),
  
  ## 3 dage
  mse_3d = c(mse_bw_3d, mse_bs_3d, mse_ridge_3, mse_lasso_3),
  rmse_3d = c(rmse_bw_3d, rmse_bs_3d, rmse_ridge_3, rmse_lasso_3),
  
  ## 7 dage
  mse_7d = c(mse_bw_7d, mse_bs_7d, mse_ridge_7, mse_lasso_7),
  rmse_7d = c(rmse_bw_7d, rmse_bs_7d, rmse_ridge_7, rmse_lasso_7),
  
  ## 10 dage
  mse_10d = c(mse_bw_10d, mse_bs_10d, mse_ridge_10, mse_lasso_10),
  rmse_10d = c(rmse_bw_10d, rmse_bs_10d, rmse_ridge_10, rmse_lasso_10),
  
  ## 3 måneder
  mse_3m = c(mse_bw_3m, mse_bs_3m, mse_ridge_3m, mse_lasso_3m),
  rmse_3m = c(rmse_bw_3m, rmse_bs_3m, rmse_ridge_3m, rmse_lasso_3m)
) |>
  arrange(rmse_3d)

results

# Stor model
mse_lm
rmse_lm

# =========== CV vs Test (MSE/RMSE + GAP) =========== 

results_cv_test <- tibble(
  model = c("Backward", "Best subset", "Ridge", "Lasso"),
  
  # --- 3 dage ---
  cv_mse_3d = c(cv_mse_bw_3d, cv_mse_bs_3d, cv_mse_ridge_3, cv_mse_lasso_3),
  test_mse_3d = c(mse_bw_3d, mse_bs_3d, mse_ridge_3, mse_lasso_3),
  
  # --- 7 dage ---
  cv_mse_7d = c(cv_mse_bw_7d, cv_mse_bs_7d, cv_mse_ridge_7, cv_mse_lasso_7),
  test_mse_7d = c(mse_bw_7d, mse_bs_7d, mse_ridge_7, mse_lasso_7),
  
  # --- 10 dage ---
  cv_mse_10d = c(
    cv_mse_bw_10d,
    cv_mse_bs_10d,
    cv_mse_ridge_10,
    cv_mse_lasso_10
  ),
  test_mse_10d = c(mse_bw_10d, mse_bs_10d, mse_ridge_10, mse_lasso_10),
  
  # --- 3 måneder ---
  cv_mse_3m = c(cv_mse_bw_3m, cv_mse_bs_3m, cv_mse_ridge_3m, cv_mse_lasso_3m),
  test_mse_3m = c(mse_bw_3m, mse_bs_3m, mse_ridge_3m, mse_lasso_3m)
) |>
  mutate(
    cv_rmse_3d = sqrt(cv_mse_3d),
    test_rmse_3d = sqrt(test_mse_3d),
    cv_rmse_7d = sqrt(cv_mse_7d),
    test_rmse_7d = sqrt(test_mse_7d),
    cv_rmse_10d = sqrt(cv_mse_10d),
    test_rmse_10d = sqrt(test_mse_10d),
    cv_rmse_3m = sqrt(cv_mse_3m),
    test_rmse_3m = sqrt(test_mse_3m),
    
    gap_rmse_3d = test_rmse_3d - cv_rmse_3d,
    gap_rmse_7d = test_rmse_7d - cv_rmse_7d,
    gap_rmse_10d = test_rmse_10d - cv_rmse_10d,
    gap_rmse_3m = test_rmse_3m - cv_rmse_3m
  ) |>
  arrange(test_rmse_3d)

results_cv_test


# ---- Performance og evaluering ----
gap_long <- results_cv_test %>%
  select(
    model,
    starts_with("cv_rmse_"),
    starts_with("test_rmse_")
  ) %>%
  pivot_longer(
    cols = -model,
    names_to = c("type", "horizon"),
    names_pattern = "(cv|test)_rmse_(.*)",
    values_to = "rmse"
  ) %>%
  pivot_wider(
    names_from = type,
    values_from = rmse
  ) %>%
  mutate(
    gap_rmse = test - cv,
    horizon = factor(horizon, levels = c("3d", "7d", "10d", "3m"))
  ) %>%
  arrange(model, test)

gap_long

# --- Heatmap af GAP (Test_RMSE - CV_RMSE) ---
gap_long <- results_cv_test %>%
  select(model, starts_with("gap_rmse_")) %>%
  pivot_longer(
    cols = starts_with("gap_rmse_"),
    names_to = "horizon",
    values_to = "gap_rmse"
  ) %>%
  mutate(
    horizon = recode(
      horizon,
      gap_rmse_3d = "3d",
      gap_rmse_7d = "7d",
      gap_rmse_10d = "10d",
      gap_rmse_3m = "3m"
    ),
    # sortér horizons pænt
    horizon = factor(horizon, levels = c("3d", "7d", "10d", "3m"))
  )

ggplot(gap_long, aes(x = horizon, y = model, fill = gap_rmse)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(gap_rmse, 1)), size = 5) +
  theme_minimal() +
  labs(
    title = "Gap RMSE (Test − CV)",
    fill = "Gap RMSE",
    x = "Horisont",
    y = "Model"
  )
#fortolkning
#vi har kigget på, hvorfor forskellige modeller (best subset, ridge og lasso) performer forskelligt afhængigt af prognosehorisonten, og det kan forklares ret klart med bias–variance tradeoff og hvordan signalet i data ændrer sig over tid.
#Grundidéen er, at modellerne laver forskellige kompromiser mellem bias (hvor simple de er) og variance (hvor følsomme de er over for støj).
#Best subset har lav bias, fordi den kun bruger de variable, der forklarer mest, men til gengæld høj variance, fordi små ændringer i data kan ændre modelvalget. Den klarer sig bedst ved 3 dage, fordi signalet her er meget stærkt og simpelt – i praksis forklares det meste af modstandergruppe og år. Der er relativt lidt støj, så den høje variance er ikke et stort problem, og den lave bias giver den bedste performance.
#Ridge har lidt højere bias, men lavere variance, fordi den beholder alle variable og bare dæmper dem. Den performer bedst ved 7 dage, hvor flere variable bidrager lidt hver, og hvor der er mere overlap mellem forklaringsvariable (fx kalender og struktur). Her er det vigtigere at holde variance nede end at ramme helt præcist, og derfor bliver ridge mest stabil.
#Lasso har endnu højere bias, men meget lav variance, fordi den fjerner irrelevante variable helt. Den klarer sig bedst ved 3 måneder, hvor signalet er svagt, og de fleste variable primært er støj. Her er det afgørende at undgå overfitting, og derfor vinder lasso ved at tvinge modellen til at være meget simpel.
#Forskellene mellem modellerne er generelt små, fordi næsten al forklaringskraft ligger i meget få variable, og fordi støjen vokser kraftigt med prognosehorisonten. Det betyder, at vi hurtigt rammer en grænse for, hvor meget RMSE kan forbedres uanset modelvalg.
#Vi ser også, at RMSE er højere på testdata end ved 10-fold cross-validation. Det er helt forventeligt. Cross-validation tester modellen på data, der stadig ligner træningsdata, og modellen er samtidig valgt til at performe godt på CV. Testdata er helt nye observationer og derfor sværere at forudsige, især ved lange horisonter. At forskellen mellem CV-RMSE og test-RMSE vokser med horisonten viser, at usikkerheden stiger jo længere frem vi forsøger at forudsige.

# ---- Prædiktion på nye kampe – lasso (3 måneder) ----

# (Genbruger dine eksisterende objekter: x_train_3m, cv_lasso_3m, rmse_lasso_3m)

new_game_predic <- tibble(
  runde = 20,
  weekend = factor("weekend", levels = c("hverdag", "weekend")),
  år = 2026,
  årstid = factor("forår", levels = c("vinter", "forår", "sommer", "efterår")),
  kamp_pa_helligdage = factor("nej", levels = c("nej", "ja")),
  mod_gruppe = factor("Top", levels = c("Bund", "Mellem", "Top")),
  tid = hms::parse_hm("18:00")
)

# Byg design-matrix på samme måde som træning (bemærk: ingen 'tilskuere' i ny data)
x_new_3m_predic <- model.matrix(
  ~ .,
  data = new_game_predic
)[, -1, drop = FALSE]

# Align kolonner til træningsmatricen (glmnet kræver samme antal + rækkefølge)
missing_cols_predic <- setdiff(colnames(x_train_3m), colnames(x_new_3m_predic))
if (length(missing_cols_predic) > 0) {
  zeros_predic <- matrix(0, nrow = nrow(x_new_3m_predic), ncol = length(missing_cols_predic))
  colnames(zeros_predic) <- missing_cols_predic
  x_new_3m_predic <- cbind(x_new_3m_predic, zeros_predic)
}
x_new_3m_predic <- x_new_3m_predic[, colnames(x_train_3m), drop = FALSE]

# Predict med din eksisterende CV-LASSO
pred_new_3m_lasso_predic <- predict(cv_lasso_3m, newx = x_new_3m_predic, s = "lambda.min")

# Saml output
new_game_result_predic <- new_game_predic |>
  mutate(
    pred_tilskuere = as.numeric(pred_new_3m_lasso_predic),
    error_rmse = rmse_lasso_3m
  )

new_game_result_predic

#udvalg af variabler med subset ud fra laveste CV.
coef_bw_3m <- coef(bw.3m, id = best.size.bw_3m)
coef_bw_10d <- coef(bw.10d, id = best.size.bw_10d)
coef_bw_7d <- coef(bw.7d, id = best.size.bw_7d)
coef_bw_3d <- coef(bw.3d, id = best.size.bw_3d)

#forskel på udvalg af variabler for 10d model BW vs. BS
coef_bs_10d <- coef(bestsub.10d, id = best.size.bs_10d)
#RMSE viser at BW har lavere fejl end 
