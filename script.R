library(httr)
library(rvest)
library(tidyverse)

start <- as.Date("01-01-2019",format="%m-%d-%Y")
end   <- as.Date("09-01-2023",format="%m-%d-%Y")

final_data <- rbind()

while (start <= end){
  
  url <- paste0("https://www.dab.gov.af/exchange-rates?field_date_value=",format(start,"%m/%d/%y"))
  print(paste0("Extracting table from: ", url))
  
  response <- httr::GET(url, config = httr::config(ssl_verifypeer = 0)) # Disable SSL certificate verification; not always recommended
  content <- httr::content(response, "text")
  df <- read_html(content)
  
  logic <- html_nodes(df, xpath = "/html/body/div[1]/main/section/div/div/div/div[2]/table") %>% html_table() %>% unlist()  
  
  if (!is.null(logic)) {  
    df_final <- html_nodes(df, xpath = "/html/body/div[1]/main/section/div/div/div/div[2]/table") %>% html_table() %>% data.frame() %>%  mutate(date = start)
    final_data <- rbind(final_data, df_final)
  }
  
  start <- start + 1                      
}

final_data <- final_data %>% 
  mutate(Currency2 = case_when(
    Currency %in% c("EURO", "یورو")~ "EURO€",
    Currency %in% c("INDIAN", "INR", "روپیه هندی", "رویپه هندی")~ "INDIAN Rs.",
    Currency %in% c("IRT", "TOMAN|IR TO", "تومان ایرانی")~ "IRAN Toman",
    Currency %in% c("PAKISTAN Rs|PKR", "PKR", "روپیه پاکستانی")~ "PAKISTAN Rs.",
    Currency %in% c("POUND", "GBP", "پوند استرلنگ") ~ "POUND£",
    Currency %in% c("SAR", "SAUDI RIYAL|SAR", "ریال سعودی") ~ "SAUDI RIYAL",
    Currency %in% c("SWISS", "فرانک  سویس", "فرانک سویس") ~ "SWISS₣",
    Currency %in% c("UAE DARHAM|UAE", "AED", "درهم امارات") ~ "UAE DIRHAM",
    Currency %in% c("US DOLLAR", "USD", "دالر امریکایی", "دالرامریکایی") ~ "USD$",
    Currency %in% c("یو وان چینایی", "یووان چینایی") ~ "CNY¥",
    TRUE ~ Currency
  ))


writexl::write_xlsx(final_data, "exchange_rate_01.xlsx")


final_data %>%   
  filter(Currency2 %in% c("CNY¥", "EURO€", "INDIAN Rs.", "IRAN Toman", "PAKISTAN Rs.", "POUND£", "SAUDI RIYAL", "SWISS₣", "UAE DIRHAM", "USD$")) %>%
  mutate(  
    Cash_sell = as.numeric(Cash..Sell.),  
    Cash_buy = as.numeric(Cash..Buy.)  
  ) %>%   
  select(Currency2, date, contains("Cash_")) %>%   
  pivot_longer(-c("Currency2", "date"), names_to = "names", values_to = "values") %>%   
  filter(!is.na(values)) %>%   
  ggplot(aes(x = date, y = values, color = Currency2)) +  
  geom_line(show.legend = F) +  
  geom_point(show.legend = F, size = 0.1, color = "black") +  
  facet_grid(Currency2~names, scales = "free_y") +  
  theme_bw() +  
  theme(strip.text.x = element_text(face = "bold"),  
        strip.text.y = element_text(size = 8),  
        plot.caption = element_text(color = "blue")  
  ) +  
  labs(x = NULL, y = NULL, caption = "Source: https://www.dab.gov.af/exchange-rates")


