library(rvest)
library(stringr)
library(tidyverse)

options(stringsAsFactors = FALSE)

args = commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
	event_id <- args[1]
	method <- args[2]
	
	# try & fetch Bokat page
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
				sum(na.rm = TRUE)
	
		# check if my answer is "Yes!" & compare to cached data
		if (
			Bokat$tbl %>% filter(name == "Mikhail Zhilkin", status == "Yes!") %>% nrow() > 0 &&
			file.exists(paste(event_id, "rds", sep = ".")) && 
			!identical(Bokat, readRDS(paste(event_id, "rds", sep = ".")))
		) {
			# if differs
			aws.sns::publish(
				topic = readRDS("topic.rds")[[method]],
				message = sprintf(
					"[%s] %s %s",
					Bokat$count,
					Bokat$tbl %>% arrange(desc(ts)) %>% head(1) %>% unlist() %>% paste(collapse = " "),
					rep(
						paste("http://www.bokat.se/stat.jsp?userId=41368194059144&eventId=", event_id, sep = "="),
						args[2] == "mail"
					)
				)
			)
		}
		
		# cache
		saveRDS(Bokat, file = paste(event_id, "rds", sep = "."))
	}
}
