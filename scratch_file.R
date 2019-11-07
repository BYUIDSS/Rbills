
#' Read Gas PDF Files You Want
#'
#' @param x write the path of your file.
#'
#' @return A table
#' @export
#'
#' @importFrom utils choose.files
#' @importFrom pdftools pdf_text
#' @importFrom readr read_lines
#' @importFrom stringr str_which
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_squish
#' @importFrom stringr str_split
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#'
#' @examples
#' c <- "C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_FA19/documents/reference_material/Gas_bills_plain_text/"
#' read_pdf_gas("C:/Users/User/Desktop/MATH 488 Brother Hathaway/merit_medical_FA19/documents/reference_material/Gas_bills_plain_text")
read_pdf_gas_want <- function(x){

  gas_files <- choose.files(default = paste0(x, "/*.*"), caption = "Select files")
  basename(gas_files)

  gas_table <- data.frame()

  for(invoice in gas_files){

    current_invoice <- pdf_text(paste0(x, "/", invoice)) %>% read_lines()


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

  View(gas_table)

}









