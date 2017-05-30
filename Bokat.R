library(rvest)
library(stringr)
library(tidyverse)

options(stringsAsFactors = FALSE)

args = commandArgs(trailingOnly = TRUE)

if (length(args) == 1) {
	# try & fetch Bokat page
	event_id <- args[1]
	
	html <- tryCatch(
		read_html(paste("http://www.bokat.se/statPrint.jsp?changeLang=1&eventId", event_id, sep = "=")),
		error = function(e) e
	)
	
	# if successful
	if ("xml_document" %in% class(html)) {
		Bokat <- list()
		
		# parse HTML
		Bokat$tbl <- tibble(
			status = html %>% 
				html_nodes("td.TextLarge") %>% 
				html_text(trim = TRUE),
		
			name = html %>% 
				html_nodes("td.TextSmall[align='left'][width!='50']") %>% 
				html_text(trim = TRUE) %>% 
				str_match(., "^([^(]+)") %>% 
				.[,2],
		
			ts = html %>% 
				html_nodes("td.TextSmall[align='left'][width!='50']") %>% 
				html_text(trim = TRUE) %>% 
				str_match(., "(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})") %>% 
				.[,2],
				
			comment = html %>% 
				html_nodes("td.TextSmall[align!='left']") %>% 
				html_text(trim = TRUE)
		)
		
		# count players...
		Bokat$count <- Bokat$tbl %>% filter(status == "Yes!") %>% nrow() +
			# ... plus guests
			html %>% 
				html_nodes("td.TextSmall[align='left'][width='50']") %>% 
				html_text(trim = TRUE) %>% 
				as.integer() %>% 
				sum()
	
		# compare to cached data
		if (file.exists(paste(event_id, "rds", sep = ".")) && !identical(Bokat, readRDS(paste(event_id, "rds", sep = ".")))) {
			# if differs
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
		
		# cache
		saveRDS(Bokat, file = paste(event_id, "rds", sep = "."))
	}
}
