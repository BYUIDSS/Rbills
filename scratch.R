
#' Read All Power PDF Files
#'
#' @param x write the path of your file.
#'
#' @return A table
#' @export
#'
#' @importFrom pdftools pdf_text
#' @importFrom readr read_lines
#' @importFrom stringr str_which
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_split
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#'
#' @examples
#' c <- "C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_FA19/documents/reference_material/power_bills/"
#' read_pdf_power("C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_FA19/documents/reference_material/power_bills")
read_pdf_power <- function(x) {

  require("dplyr")

  pb_vector <- list.files(path = x,  pattern = "pdf$")

  energy_charge <- data.frame()

  extract_data <- "Yes"

  for (bill_name in pb_vector) {

    while (extract_data == "Yes") {

      on_off <- readline(prompt = "Does this building have onkwh/offkwh? (Yes or No): ")
      meter_number <- readline(prompt = "Please enter Meter Number: ")
      building_name <- readline(prompt = "Please enter Building Name: ")

      pb_text <- pdf_text(paste0(x, "/", bill_name)) %>% read_lines()

      if (on_off == "Yes") {

        onkwh <- pb_text[str_which(pb_text, meter_number)[2]] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          plyr::ldply() %>%
          select(V11, V12) %>%
          rename("onkwh" = V12, "meter_multiplier" = V11) %>%
          mutate(
            meter_multiplier = as.numeric(meter_multiplier),
            building = building_name)

        offkwh <- pb_text[str_which(pb_text, meter_number)[3]] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          plyr::ldply() %>%
          select(V12) %>%
          rename("offkwh" = V12)

        kvarh <- pb_text[str_which(pb_text, meter_number)[4]] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          plyr::ldply() %>%
          select(V12) %>%
          rename("kvarh" = V12)

        date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
          str_squish() %>%
          str_split("BILLING DATE: ") %>%
          plyr::ldply() %>%
          select(2) %>%
          rename("date" = V2)
        # %>%
        #   mutate(date = mdy(date)) %>%
        #   mutate(date = as_date(date))

        onkwh_offkwh <- cbind(onkwh, offkwh, kvarh, date)

        energy_charge <- bind_rows(energy_charge, onkwh_offkwh)

      } else{

        kwh <- pb_text[str_which(pb_text, meter_number)[2]] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          plyr::ldply() %>%
          select(V11, V12) %>%
          rename("kwh" = V12, "meter_multiplier" = V11) %>%
          mutate(
            meter_multiplier = as.numeric(meter_multiplier),
            building = building_name)

        kvarh <- pb_text[str_which(pb_text, meter_number)[3]] %>%
          str_squish() %>%
          str_remove(",") %>%
          str_split(" ") %>%
          plyr::ldply() %>%
          select(V12) %>%
          rename("kvarh" = V12)

        date <- pb_text[str_which(pb_text, "BILLING DATE:")[1]] %>%
          str_squish() %>%
          str_split("BILLING DATE: ") %>%
          plyr::ldply() %>%
          select(2) %>%
          rename("date" = V2)
        # %>%
        #   mutate(date = mdy(date)) %>%
        #   mutate(date = as_date(date))


        normal <- cbind(kwh, kvarh, date)

        energy_charge <- bind_rows(energy_charge, normal)

      }

      extract_data <- readline(prompt = "Would you like to pull data from another PDF? (Yes or No): ")

    }

  }

  return(energy_charge)

}
