library(odbc)
library(dplyr)
library(plotly)

driver = "SnowflakeDSIIDriver"
creds <- jsonlite::fromJSON("snowflake-details.json")

# Local Credentials are a (gitignored) JSON file of the form 
# uncomment, save in txt file, replace username/password SAVE AS **json**

# { "driver": "Snowflake",
#   "server_url": "no http URL: snowflakecomputing.com",
#   "username": "USERNAME",
#   "password": "PASSWORD",
#   "role": "DATASCIENCE",
#   "warehouse": "DATA_SCIENCE",
#   "database": ""
# }

submitSnowflake <- function(query, driver = "Snowflake", user, pass, 
                            role, server, warehouse, database = NULL){
  
  connection <- dbConnect(
    odbc::odbc(),
    .connection_string = paste0("Driver={",driver,"}",
                                ";Server={",server,
                                "};uid=",user,
                                ";role=",role,
                                ";pwd=",pass,
                                ";warehouse=", warehouse,
                                ";database=", database)
  )
  
  output <- dbGetQuery(connection, query)
  dbDisconnect(connection)
  return(output)
  
}

base_olhc <- submitSnowflake("select * from pro_charliemarketplace.evm_uniswap.evm_olhc",
                             driver = driver, 
                             user = creds$username, 
                             pass = creds$password,
                             role = creds$role,
                             server = creds$server_url,
                             warehouse = creds$warehouse)

colnames(base_olhc) <- tolower(colnames(base_olhc))

# outlier standard deviations just make 10
base_olhc$std_[base_olhc$std_ > 10] <- 10


plot_ly() %>% 
  add_trace(data = base_olhc,
            x = ~std_, y = ~median_adj_liq, color = ~as.factor(paste0(symbol1, fee_tier)), 
        type = 'scatter', mode = 'markers') 

plot_ly() %>% 
  add_trace(data = base_olhc %>% filter(fee_tier == 0.0005), 
            x = ~hr_, y = ~close_, name = ~symbol1, type = "scatter",
            mode = 'lines')
  

# seems only 0.05% fee tier has any worthwhile volume
base_olhc %>% group_by(fee_tier, symbol1) %>%
  filter(hr_ >= as.POSIXct('2024-01-01')) %>% summarise(
  sum(eth_fee),
  sum(usdc_fee)
)

plot_ly() %>% 
  add_trace(data = base_olhc %>% filter(fee_tier == 0.0005), 
            x = ~hr_, y = ~eth_fee, name = ~symbol1, type = "scatter",
            mode = 'lines')

  