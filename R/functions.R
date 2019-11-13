#' My Hello World Function
#'
#' @param x The name of the person to say hi to
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("Sean")
#' \dontrun{
#' hello("John")
#' }
hello <- function(x) {
  print(paste0("Hello ", x, ", this is the world!"))
}


#' All PDF data
#'
#' @description get pdf files and list it.
#'
#' @param path_x write the path of your file.
#' @param choose.file choose.file = FALSE means you want all data files, choose.file = TRUE means you want specific data files.
#'
#' @return A list
#' @export
#'
#' @importFrom utils choose.files
#' @examples
#' devtools::load_all()
#' pdf_folder <- system.file("data-raw", package = "Rbills", mustWork = TRUE)
#' get_pdf_all(pdf_folder)
get_pdf <- function(path_x, choose.file = FALSE){

  if (!choose.file) {

    list.files(path = path_x,  pattern = "pdf$")


  } else {

    sean <- choose.files(default = paste0(path_x, "/*.*"), caption = "Select files")
    basename(sean)

  }

}




#' Read All Gas PDF Files
#'
#' @param path_x write the path of your file.
#' @param x put the file's list
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
#' @import dplyr
#' @import utils
#'
#' @examples
#' devtools::load_all()
#' pdf_file <- system.file("data-raw", "example_gasbill.pdf", package = "Rbills", mustWork = TRUE)
#' pdf_folder <- system.file("data-raw", package = "Rbills", mustWork = TRUE)
#' read_pdf_seg(pdf_folder, x)
#' read_pdf_seg("C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_package/Rbills/data-raw", x)
read_pdf_seg <- function(path_x = getwd(), x){

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

    temp_table <- as_tibble(index) %>% mutate(billing_month = current_invoice[1], year = current_invoice[2], gas_supplied_price = current_invoice[16], gas_supplied_MMbtu = current_invoice[17], gas_paid = current_invoice[18], transportation_fuel_price = current_invoice[24], transportation_fuel_MMbtu = current_invoice[25], transportation_fuel_paid = current_invoice[26])

    gas_table <- bind_rows(gas_table,temp_table)


  }

  return(gas_table)
  #write.csv(gas_table, "gas_table.csv", row.names = FALSE)

}







#' Read All Power PDF Files
#'
#' @param x write the path of your file.
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
#' @importFrom plyr ldply
#' @importFrom lubridate mdy
#' @importFrom lubridate as_date
#' @import utils
#'
#' @examples
#' devtools::load_all()
#' pdf_file <- system.file("data-raw", "example_powerbill.pdf", package = "Rbills", mustWork = TRUE)
#' pdf_folder <- system.file("data-raw", package = "Rbills", mustWork = TRUE)
#' read_pdf_rmp(pdf_folder)
#' read_pdf_rmp("C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_FA19/documents/reference_material/power_bills")
read_pdf_rmp <- function(x) {

  # Determines which pdfs will have data extracted
  pb_vector <- list.files(path = x,  pattern = "pdf$")

  energy_charge <- data.frame()

  # Prompts users to enter one or more meter numbers to extract data for
  meter_number <- readline(prompt = "Please enter one or more Meter Numbers (separated by a comma and a space): ")

  # Separates the user input into individual strings
  meter_numbers_list <- unlist(strsplit(meter_number, split = ", "))


  for (bill_name in pb_vector) { #Runs through each pdf selected and extracts the data

    for (number in meter_numbers_list) { #For every pdf, extracts data associated with each input meter number

      pb_text <- pdf_text(paste0(x, "/", bill_name)) %>% #Converts each line of the pdf into a string
        read_lines()

      if (sum(str_detect(pb_text, number)) > 2) { #Determines if the meter number shows up 3 or more times in the PDF to make sure data is being pulled for main meter number


        kwh <- pb_text[str_which(pb_text, number)[2]] %>% #Extracts kwh from the pdf for a given meter number
          str_squish() %>% #Removes spaces from the selected string
          str_remove(",") %>% #Removes commas from the string
          str_split(" ") %>% #Splits the string into individual strings
          plyr::ldply() %>% #Converts the strings into a data frame
          select(V12, V13) %>% #Selects the data we want
          rename("kwh" = V12, "on_kwh" = V13) %>%
          mutate(kwh = as.numeric(gsub("\\,", "", kwh))) #Converts the kwh column from a character data type into numeric data type

        # bldg <- pb_text[((str_which(pb_text, number)[2]) - 4)] %>%
        #   str_squish() %>%
        #   str_remove(",") %>%
        #   str_split(" ") %>%
        #   plyr::ldply() %>%
        #   select(V1, V2, V3) %>%
        #   mutate(building = paste(V1, V2, V3), sep = " ") %>%
        #   select(building)

        if (kwh$on_kwh == "onkwh") {#If statement determines if the meter number is broken into on peak and off peak hours and extracts if necessary

          offkwh <- pb_text[str_which(pb_text, number)[3]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            plyr::ldply() %>%
            select(V12) %>%
            rename("offkwh" = V12) %>%
            mutate(offkwh = as.numeric(gsub("\\,", "", offkwh)))

          kvarh <- pb_text[str_which(pb_text, number)[4]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            plyr::ldply() %>%
            select(V12) %>%
            rename("kvarh" = V12) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            plyr::ldply() %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, number, kwh, offkwh, kvarh) %>% #Binding all of the extracted data into one data frame
            rename("onkwh" = kwh,
                   "meter_number" = number) %>%
            mutate(totalkwh = onkwh + offkwh)


        } else{

          kvarh <- pb_text[str_which(pb_text, number)[3]] %>%
            str_squish() %>%
            str_remove(",") %>%
            str_split(" ") %>%
            plyr::ldply() %>%
            select(V12) %>%
            rename("kvarh" = V12) %>%
            mutate(kvarh = as.numeric(gsub("\\,", "", kvarh)))

          date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
            str_squish() %>%
            str_split("BILLING DATE: ") %>%
            plyr::ldply() %>%
            select(2) %>%
            rename("date" = V2) %>%
            mutate(date = mdy(date)) %>%
            mutate(date = as_date(date))

          rows <- cbind(date, number, kwh, kvarh) %>%
            rename("totalkwh" = kwh,
                   "meter_number" = number)

        }

        energy_charge <- bind_rows(energy_charge, rows) %>% #Binding data from each iteration of the loop onto the main dataframe
          select(-on_kwh)

      }

    }

    energy_charge <- energy_charge %>% #Reordering the columns
      select(date, meter_number, onkwh, offkwh, totalkwh, kvarh)

  }

  return(energy_charge)

}


# utils::globalVariables(c("V11", "V12", "V2", "meter_multiplier"), package = "Rbills", add = FALSE)
