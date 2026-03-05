db_user <- "o_cherchenko"
db_password <- Sys.getenv("MF_DB")

library(RPostgreSQL)

con <- dbConnect(DBI::dbDriver("PostgreSQL"), 
                 dbname = "magload", host = "193.233.79.159", port = 5432, 
                 user = db_user, password = db_password)

dbListTables(con)

card_issns <- dbGetQuery(con, 
                         "SELECT *
          FROM platform_article.card_issns  LIMIT 100;"
) |> 
  select(card_id, issn, title, year_start, year_end, title_original, type_issn)

x <- dbListTables(con)

data <- map(x, ~ dbGetQuery(con, paste0("SELECT * FROM platform_article.", .x, " LIMIT 50;")))

names(data) <- x
