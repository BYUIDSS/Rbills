#' All PDF data
#'
#' @description get pdf files and list it.
#'
#' @param path_x write the path of your file.
#' @param choose.file choose.file = FALSE means you want all data files in the folder, choose.file = TRUE means you want a specific pdf file.
#'
#' @return A vector of files paths.
#' @export
#'
#' @seealso Check \href{https://github.com/BYUIDSS/Rbills}{README.md} to know how to use.
get_pdf <- function(path_x, choose.file = FALSE){

  if (!choose.file) {

    out <- list.files(path = path_x,  pattern = "pdf$")


  } else {

    out <- file.choose()


  }

  basename(out)

}




#' Read All Gas PDF Files
#'
#' @param path_x a string that sets the directory where the filenames are located.
#' @param x a vector of strings that have filenames.
#'
#' @return A table
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom readr read_lines
#' @importFrom stringr str_which
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_split
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @import utils
#' @importFrom tidyr unite
#' @importFrom dplyr rename
#' @import dplyr
#' @seealso Check \href{https://github.com/BYUIDSS/Rbills}{README.md} to know how to use.
read_pdf_seg <- function(path_x, x){

  # if (is.null(x)) {
  #
  #   gas_files <- list.files(path = path_x,  pattern = "pdf$")
  #
  #
  # } else {
  #
  #   gas_files <- x
  #
  # }

  gas_table <- data.frame()

  for(invoice in x){

    current_invoice <- pdf_text(paste0(path_x, "/", invoice)) %>% read_lines()


    bill_start <- str_which(current_invoice, pattern = "Billing Month")

    month <- str_which(current_invoice, pattern = "Billing Month") + 1
    meter <- str_which(current_invoice, pattern = "Meter ID")
    supplied <- str_which(current_invoice, pattern = "Gas Supplied")
    trans <- str_which(current_invoice, pattern = "Transportation Fuel")
    totals <- str_which(current_invoice, pattern = "Totals")

    bill_end <- str_which(current_invoice, pattern = " Totals")


    lines_wanted <- c(month, meter,supplied, trans, totals)

    current_invoice <- current_invoice[lines_wanted] %>%
      read_lines() %>%
      str_remove_all(pattern = "\\(") %>%
      str_remove_all(pattern = "\\)") %>%
      str_remove_all(pattern = ",") %>%
      str_remove_all(pattern = "\\$") %>%
      str_squish() %>%
      str_split(" ") %>%
      unlist()

    index <- c(current_invoice[12])

    temp_table <- as_tibble(index) %>% mutate(billing_month = current_invoice[1],
                                              year = current_invoice[2],
                                              invoice_m = current_invoice[4],
                                              invoice_d = current_invoice[5],
                                              invoice_y = current_invoice[6],
                                              gas_supp_price = as.numeric(current_invoice[16]),
                                              gas_supp_mmbtu = as.numeric(current_invoice[17]),
                                              gas_supp_ext = as.numeric(current_invoice[18]),
                                              trans_fuel_price = as.numeric(current_invoice[24]),
                                              trans_fuel_mmbtu = as.numeric(current_invoice[25]),
                                              trans_fuel_ext = as.numeric(current_invoice[26]))

    gas_table <- bind_rows(gas_table,temp_table)


  }

  gas_table <- gas_table %>% rename("meter_id" = value)

  gas_table <- gas_table %>%
    unite(billing_month, year, sep = " ", remove = T, col = "billing_month") %>%
    unite(invoice_m, invoice_d, sep = " ", remove = T, col = "invoice_date") %>%
    unite(invoice_date, invoice_y, sep = ", ", remove = T, col = "invoice_date")

  return(gas_table)
  #write.csv(gas_table, "gas_table.csv", row.names = FALSE)

}







#' Read All Power PDF Files
#'
#' @param path_x a string that sets the directory where the filenames are located.
#' @param x a vector of strings that have filenames.
#' @param building_names defaults to NULL.  If NULL then user is prompted to type names.  Else a vector of building names can be provided to avoid the prompt.
#'
#' @return A table
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#' @importFrom stringr str_which
#' @importFrom stringr str_remove
#' @importFrom stringr str_squish
#' @importFrom stringr str_split
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom lubridate mdy
#' @importFrom lubridate as_date
#' @importFrom utils View
#' @importFrom purrr map_df
#' @importFrom data.table transpose
#' @seealso Check \href{https://github.com/BYUIDSS/Rbills}{README.md} to know how to use.
read_pdf_rmp <- function(path_x, x, building_names = NULL) {

  energy_charge <- data.frame(date = as.Date(Sys.time()), building = "name",
                              meter_number = " ",
                              onkwh = 9, offkwh = 9, totalkwh = 9, kvarh = 9,
                              stringsAsFactors = FALSE)[-1,]


  # Prompts users to enter one or more meter numbers to extract data for
  if(is.null(building_names)) {
    building <- readline(prompt = "Please enter one or more building names, separated by commas and ensure names match the building names on the bill: ")
    # Separates the user input into individual strings
    building_list <- unlist(strsplit(building, split = ", "))
  }
  # just use the building_names input.
  if(!is.null(building_names)) {
    building_list <- building_names
  }






  for (bill_name in x) { #Runs through each pdf selected and extracts the data

    # Converts each line of the pdf into a string
    pb_text <- pdf_text(paste0(path_x, "/", bill_name)) %>% read_lines()

    # For every pdf, extracts data associated with each input meter number
    for (building in building_list) {

      meter_number <- pb_text[(str_which(pb_text, building)[1])] %>%
        str_squish() %>%
        str_remove(",") %>%
        str_split(" ") %>%
        map_df(as.data.frame) %>%
        transpose %>%
        select(str_which(., "\\d{8}"))

      if (dim(meter_number)[2] == 0){

        meter_number <- pb_text[(str_which(pb_text, building)[1]) + 1] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          map_df(as.data.frame) %>%
          transpose %>%
          select(str_which(., "\\d{8}"))

      }

      meter_number <- meter_number %>%
        unlist(as.character(meter_number[1]))


      kwh <- pb_text[str_which(pb_text, meter_number)[2]] %>% #Extracts kwh from the pdf for a given meter number
        str_squish() %>% #Removes spaces from the selected string
        str_remove(",") %>% #Removes commas from the string
        str_split(" ") %>% #Splits the string into individual strings
        map_df(as.data.frame) %>%
        transpose #Converts the strings into a data frame

      if ("V12" %in% colnames(kwh)) {

        kwh <- kwh %>%
          select(V12, V13) %>% #Selects the data we want
          rename("kwh" = V12, "on_kwh" = V13) %>%
          mutate(kwh = as.numeric(gsub("\\,", "", kwh))) #Converts the kwh column from a character data type into numeric data type

        if (kwh$on_kwh == "onkwh") {#If statement determines if the meter number is broken into on peak and off peak hours and extracts if necessary

          offkwh <- pb_text[str_which(pb_text, meter_number)[3]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V12) %>%
            rename("offkwh" = V12) %>%
            mutate(offkwh = as.numeric(gsub("\\,", "", offkwh)))

          kvarh <- pb_text[str_which(pb_text, meter_number)[4]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V12) %>%
            rename("kvarh" = V12) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, building, meter_number, kwh, offkwh, kvarh) %>% #Binding all of the extracted data into one data frame
            rename("onkwh" = kwh) %>%
            mutate(totalkwh = onkwh + offkwh)

        } else{

          kvarh <- pb_text[str_which(pb_text, meter_number)[3]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V12) %>%
            rename("kvarh" = V12) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, building, meter_number, kwh, kvarh, stringsAsFactors = FALSE) %>%
            rename("totalkwh" = kwh)

        }

      } else if (("V12" %in% colnames(kwh)) == FALSE)  { #Determines if the format changes due to meter number changing mid-month

        kwh <- pb_text[(str_which(pb_text, meter_number)[2]) + 1] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          map_df(as.data.frame) %>%
          transpose %>%
          select(V3, V4) %>%
          rename("kwh" = V3, "on_kwh" = V4) %>%
          mutate(kwh = as.numeric(gsub("\\,", "", kwh)))

        if (kwh$on_kwh == "onkwh"){

          offkwh <- pb_text[(str_which(pb_text, meter_number)[3]) + 2] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V3) %>%
            rename("offkwh" = V3) %>%
            mutate(offkwh = as.numeric(gsub("\\,", "", offkwh)))

          kvarh <- pb_text[str_which(pb_text, meter_number)[4] + 1] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V3) %>%
            rename("kvarh" = V3) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, building, meter_number, kwh, offkwh, kvarh, stringsAsFactors = FALSE) %>% #Binding all of the extracted data into one data frame
            rename("onkwh" = kwh) %>%
            mutate(totalkwh = onkwh + offkwh)

        } else{

          kvarh <- pb_text[(str_which(pb_text, meter_number)[3]) + 1] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(V12) %>%
            rename("kvarh" = V12) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            map_df(as.data.frame) %>%
            transpose %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, building, number, kwh, kvarh, stringsAsFactors = FALSE) %>%
            rename("totalkwh" = kwh)

        }
      }

      energy_charge <- bind_rows(energy_charge, rows) %>% #Binding data from each iteration of the loop onto the main dataframe
        select(-on_kwh)

    }


  }

  energy_charge <- energy_charge %>% rename("invoice_date" = date)

  return(energy_charge)
}

# utils::globalVariables(c("V11", "V12", "V2", "meter_multiplier"), package = "Rbills", add = FALSE)
