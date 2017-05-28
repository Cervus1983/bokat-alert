library(rvest)
library(stringr)
library(tidyverse)

options(stringsAsFactors = FALSE)


# try & fetch Bokat page
event_id <- "75460575788069"

html <- tryCatch(
	read_html(paste("http://www.bokat.se/eventInfoOpen.jsp?eventId", event_id, sep = "=")),
	error = function(e) e
)


# if successful
if ("xml_document" %in% class(html)) {
	# parse HTML
	status <- html %>% 
		html_nodes("table.Text[cellspacing='7'] td[align='left'] img") %>% 
		html_attr("src") %>% 
		str_match(., "^/images/(.+)\\.png$") %>% 
		.[,2]
	
	name <- html %>% 
		html_nodes("table.Text[cellspacing='7'] td.TextSmall") %>% 
		html_text(trim = TRUE) %>% 
		.[c(TRUE, FALSE)] %>% 
		str_match(., "^(.+)\\(") %>% 
		.[,2]
	
	ts <- html %>% 
		html_nodes("table.Text[cellspacing='7'] td.TextSmall") %>% 
		html_text(trim = TRUE) %>% 
		.[c(TRUE, FALSE)] %>% 
		str_match(., "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})") %>% 
		.[,2]
	
	guest <- html %>% 
		html_nodes("table.Text[cellspacing='7'] td.TextSmall") %>% 
		html_text(trim = TRUE) %>% 
		.[c(FALSE, TRUE)]
	
	comment <- html %>% 
		html_nodes("table.Text[cellspacing='7'] textarea") %>% 
		html_text(trim = TRUE)
	
	df_new <- as.tibble(cbind(status, name, ts, guest, comment))
	
	# compare to cached data
	if (file.exists(event_id)) {
		load(event_id)
		
		# if differs
		if (!identical(df, df_new)) {
			topic <- readRDS("topic.rds")
			
			aws.sns::publish(
				topic = topic,
				message = sprintf(
					"[%s] %s",
					sum(df_new$status == "yes") + sum(as.integer(df_new$guest), na.rm = TRUE),
					df_new %>% 
						arrange(ts) %>% 
						tail(1) %>% 
						unlist() %>% 
						paste(collapse = " ")
				)
			)
		}
	}
	
	# cache
	df <- df_new
	save(df, file = event_id)
}
