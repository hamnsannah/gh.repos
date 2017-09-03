#Purpose: This function does the following
#1 reads in a list of market owners (BDs) and their email addresses
#2 uses a loop to run the R Markdown file for the individual schools report
#3 then emails the report to the individual BD it concerns
#4 Updates the counter file

#sample usage: reportee.loop("ne.recipient.counter.csv")

# You must forward report to yourself if your names isn't an explicit recipient

reportee.loop <- function(file.name.in.az){
    file.path.use <- paste0("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/az/", file.name.in.az)
    recip.counter <- read.csv(file.path.use, stringsAsFactors = FALSE)
    full.counter.to.restore <- recip.counter
    #print(recip.counter)
    row.length <- nrow(recip.counter)
    for(i in 1:row.length){
      recip.counter <- read.csv(file.path.use, stringsAsFactors = FALSE)
      owner.to.input <- recip.counter[1,1]
      print(paste("Use", owner.to.input, "in calculations"))
      #complete R Markdown calcs
      setwd("D://Users/SPritchard/Music/Documents/R/R Markdown Files")
      library(rmarkdown)
      rmarkdown::render("Individual.BD.Tri-Report.v2.Rmd", pdf_document())
      
      email.send <- recip.counter[1,2]
      print(paste("Use", email.send, "as email address"))
      #send email
      
      library(gmailr)
      gmail_auth(scope = c("read_only", "modify", "compose", "full"))
      d <- date()
      subj.msg <- paste("School Bar Graphs Reports Run", substr(d, 5, 24))
      mime() %>%
        to(recip.counter[1,2]) %>%
        from("sam.pritchard@kaplan.com") %>%
        subject(subj.msg) %>%
        html_body("See attachment for the updated field report") %>%  
        attach_part("See attachment for the updated field report") %>%
        attach_file("Individual.BD.Tri-Report.v2.pdf") -> html_msg
      send_message(html_msg)
      
      #cut top row from file for next loop
      #R Markdown file relies on row one, so to have unique files, must remove top row and restore at end
      
      recip.counter.cut <- recip.counter[-1,]
      write.csv(recip.counter.cut, file.path.use, row.names = FALSE)
      print(paste("end of loop", i))
    }
    write.csv(full.counter.to.restore, file.path.use, row.names = FALSE)
    
}