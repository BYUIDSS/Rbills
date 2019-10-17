#' Scrape all power bill PDF's in a folfer/directory
#'
#' Takes in a working direcotry which contains PDF files and scrapes them
#' @param x x is a path to a direcotry
#' @return A CSV or Table
#' @export
read_all_powerbills <- function(x){
  

  pb_vector <- list.files(path = x,  pattern = "pdf$")

  energy_charge <- data.frame()

  for (bill_name in pb_vector) {

    pb_text <- pdf_text(paste0(x, bill_name)) %>%
      read_lines()

    bldg_87 <- pb_text[str_which(pb_text, "28206904")[2]] %>%
      str_squish() %>%
      str_remove(",") %>%
      str_split(" ") %>%
      plyr::ldply() %>%
      select(V11, V12) %>%
      rename("kwh" = V12, "meter_multiplier" = V11) %>%
      mutate(#kwh = as.numeric(kwh),
        meter_multiplier = as.numeric(meter_multiplier),
        building = "Bldg 87",
        on_peak = NA)

    east_main_on <- pb_text[str_which(pb_text, "28819881|80361028")[2]] %>%
      str_squish() %>%
      str_remove(",") %>%
      str_split(" ") %>%
      plyr::ldply() %>%
      select(V11, V12) %>%
      rename("kwh" = V12, "meter_multiplier" = V11) %>%
      mutate(#kwh = as.numeric(kwh),
        meter_multiplier = as.numeric(meter_multiplier),
        building = "East Main",
        on_peak = TRUE)

    east_main_off <- pb_text[str_which(pb_text, "28819881|80361028")[3]] %>%
      str_squish() %>%
      str_remove(",") %>%
      str_split(" ") %>%
      plyr::ldply() %>%
      select(V11, V12) %>%
      rename("kwh" = V12, "meter_multiplier" = V11) %>%
      mutate(#kwh = as.numeric(kwh),
        meter_multiplier = as.numeric(meter_multiplier),
        building = "East Main",
        on_peak = FALSE)


    if (bill_name == "2019_01_17.pdf") { #If statement included to handle problem caused by bill 2019_01_17 switching meters mid month
      malt_on_mm <- pb_text[str_which(pb_text, "36438011")[2]] %>% #Block of code extracts and stores the meter multipler
        str_squish() %>%                                           #from on peak hours for MALT building
        str_remove(",") %>%
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V10) %>%
        rename("meter_multiplier" = V10) %>%
        mutate(#kwh = as.numeric(kwh),
          meter_multiplier = as.numeric(meter_multiplier),
          building = "M.a.l.t",
          on_peak = TRUE)

      malt_on_kwh <- pb_text[str_which(pb_text, "286,500")[2]] %>% #Block of code extracts and stores kwh
        str_squish() %>%                                           #for MALT building
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V6) %>%
        rename("kwh" = V6) %>%
        mutate(#kwh = as.numeric(kwh),
          building = "M.a.l.t",
          on_peak = TRUE)

      malt_on <- c(malt_on_mm, malt_on_kwh) #Combines vectors containing kwh and meter multiplier for
      #MALT building on Peak hours

      malt_off_mm <- pb_text[str_which(pb_text, "36438011")[3]] %>% #Block of code extracts meter multiplier
        str_squish() %>%                                         #from off peak hours for MALT building
        str_remove(",") %>%
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V10) %>%
        rename("meter_multiplier" = V10) %>%
        mutate(#kwh = as.numeric(kwh),
          meter_multiplier = as.numeric(meter_multiplier),
          building = "M.a.l.t",
          on_peak = FALSE)

      malt_off_kwh <- pb_text[str_which(pb_text, "378")[3]] %>% #Block of code extracts kwh from off peak hours
        str_squish() %>%                                     # for MALT building
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V6) %>%
        rename("kwh" = V6) %>%
        mutate(#kwh = as.numeric(kwh),
          building = "M.a.l.t",
          on_peak = FALSE)

      malt_off <- c(malt_off_mm, malt_off_kwh) #Combines vectors containing meter multiplier and kwh for MALT building
      #off peak hours

    }else { #Extracts data from all other bills for the MALT building
      malt_on <- pb_text[str_which(pb_text, "36438011|81509947")[2]] %>%
        str_squish() %>%
        str_remove(",") %>%
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V11, V12) %>%
        rename("kwh" = V12, "meter_multiplier" = V11) %>%
        mutate(#kwh = as.numeric(kwh),
          meter_multiplier = as.numeric(meter_multiplier),
          building = "M.a.l.t",
          on_peak = TRUE)

      malt_off <- pb_text[str_which(pb_text, "36438011|81509947")[3]] %>%
        str_squish() %>%
        str_remove(",") %>%
        str_split(" ") %>%
        plyr::ldply() %>%
        select(V11, V12) %>%
        rename("kwh" = V12, "meter_multiplier" = V11) %>%
        mutate(#kwh = as.numeric(kwh),
          meter_multiplier = as.numeric(meter_multiplier),
          building = "M.a.l.t",
          on_peak = FALSE)
    }

    new_prod1 <- pb_text[str_which(pb_text, "54033700")[2]] %>%
      str_squish() %>%
      str_remove(",") %>%
      str_split(" ") %>%
      plyr::ldply() %>%
      select(V11, V12) %>%
      rename("kwh" = V12, "meter_multiplier" = V11) %>%
      mutate(#kwh = as.numeric(kwh),
        meter_multiplier = as.numeric(meter_multiplier),
        building = "New Prod 1",
        on_peak = NA)

    new_prod2 <- pb_text[str_which(pb_text, "54033628")[2]] %>%
      str_squish() %>%
      str_remove(",") %>%
      str_split(" ") %>%
      plyr::ldply() %>%
      select(V11, V12) %>%
      rename("kwh" = V12, "meter_multiplier" = V11) %>%
      mutate(#kwh = as.numeric(kwh),
        meter_multiplier = as.numeric(meter_multiplier),
        building = "New Prod 2",
        on_peak = NA)

    date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
      str_squish() %>%
      str_split("BILLING DATE: ") %>%
      plyr::ldply() %>%
      select(2) %>%
      rename("date" = V2) %>%
      mutate(date = mdy(date)) %>%
      mutate(date = as_date(date))

    bldg_87 <- bind_cols(bldg_87, date)
    east_main_on <- bind_cols(east_main_on, date)
    east_main_off <- bind_cols(east_main_off, date)
    malt_on <- bind_cols(malt_on, date)
    malt_off <- bind_cols(malt_off, date)
    new_prod1 <- bind_cols(new_prod1, date)
    new_prod2 <- bind_cols(new_prod2, date)

    energy_charge <- bind_rows(energy_charge, bldg_87) %>%
      bind_rows(., east_main_on) %>%
      bind_rows(., east_main_off) %>%
      bind_rows(., malt_on) %>%
      bind_rows(., malt_off) %>%
      bind_rows(., new_prod1) %>%
      bind_rows(., new_prod2)
  }

  energy_charge <- energy_charge %>% #Remove excess columns created from extracting data from 2019_01_17 bill
    select(-building1, -on_peak1)


  pb_text[157:158] %>%
    str_squish() %>%
    str_remove(",") %>%
    str_split(" ")

  str_which(pb_text, "Energy Charge")



  pb_text[str_which(pb_text, "Energy Charge")[1]] %>%
    str_squish() %>%
    str_remove(",") %>%
    str_split(" ") %>%
    plyr::ldply() %>%
    select(V3, V5) %>%
    rename("kwh" = V3, "cost_per_unit" = V5) %>%
    mutate(kwh = as.numeric(kwh),
           cost_per_unit = as.numeric(cost_per_unit),
           building = "Bldg 87")
}
