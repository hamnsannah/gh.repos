#Purpose: This function does the following
    #1 Download updated PC Net BoB file if one is available (and if it's forwarded to me)
    #2 reads in a list of market owners (BDs) and their email addresses
    #3 uses a loop to run the R Markdown file for the individual schools report
    #4 then emails the report to the individual BD it concerns
    #5 Updates the counter file
          #sample usage: reportee.loop.dl("ne.recipient.counter.csv")


#####sample usage2: reportee.loop.dl("field.recipient.counter.jul17.csv") #ALSO HAVE TO CHANGE file.az near line 573 to use different list

# You must forward report to yourself if your name isn't an explicit recipient

reportee.loop.dl <- function(file.name.in.az){ # argument should correspond to a file name (see path below)
  
  print("Function will not work if report is in file on a distribution list")
  
  pcnetbob.download <- function(){
    library(gmailr)
    gmail_auth(scope = c("read_only", "modify", "compose", "full"))
    msgs <- messages(search = "to:sam.pritchard@kaplan.com", label_ids = "Label_19",num_results = 1)
    if(length(msgs[[1]])>1){
      msg <- message(msgs[[1]][[1]][[1]]$id )
      print(msg)
      save_attachments(msg, path = "D://Users/SPritchard/Downloads")
      
      modify_message(msg$id, remove_labels = "Label_19") # removes label 'New PC NetBoB'
      modify_message(msg$id, remove_labels = "UNREAD")
      print("New file downloaded")
    } else {
      print("Nothing with 'New PC NetBoB' label to download")
    }
  }
  
  reportee.loop <- function(file.name.in.az){
    file.path.use <- paste0("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/az/", file.name.in.az)
    recip.counter <- read.csv(file.path.use, stringsAsFactors = FALSE)
    full.counter.to.restore <- recip.counter
    #print(recip.counter)
    row.length <- nrow(recip.counter)
    for(i in 1:row.length){
      print("Function will not work if report is in file on a distribution list")
      recip.counter <- read.csv(file.path.use, stringsAsFactors = FALSE)
      owner.to.input <- recip.counter[1,1]
      print(paste("Use", owner.to.input, "in calculations"))
      #complete R Markdown calcs
      setwd("D://Users/SPritchard/Music/Documents/R/R Markdown Files")
      library(rmarkdown)
      rmarkdown::render("Individual.BD.Tri-Report.v4.Rmd", pdf_document())
      
      email.send <- recip.counter[1,2]
      print(paste("Use", email.send, "as email address"))
      #send email
      
      library(gmailr)
      gmail_auth(scope = c("read_only", "modify", "compose", "full"))
      d <- date()
      subj.msg <- paste("School Bar Graphs Reports Run for", owner.to.input, substr(d, 5, 24))
      mime() %>%
        to(recip.counter[1,2]) %>%
        from("sam.pritchard@kaplan.com") %>%
        subject(subj.msg) %>%
        html_body("See attachment for the updated report on individual schools.

What you should know about these reports:
1. Known duplicate school names are identified and school names is adjusted so enrollments aren't diluted among similar sounding school names in the same market.
2. If student email has a known school domain, then that school is assigned to enrollment
3. If enrollment is in a class at a venue with private classes, then that school is assigned
4. Only schools that meet both the Net BoB criteria (> $5000 annually) and the relationship criteria (some ability to influence) are included
5. Since the rate at which we capture school name varies, these reports compare that rate for 
each time period and adjust the baseline numbers for an 'apples to apples' comparison
6. The primary data source for these reports is the PC NetBoB Daily report that comes out each morning from the Business Intelligence team
7. There are processes for fixing enrollments that are miscoded and for schools to newly qualify over the $5000 threshold at least quarterly
8. The lists relied upon for each of these steps (duplicate school names, private class venues, etc) can and should be updated with all available information.  Contact sam.pritchard@kaplan.com if you have new info.

") %>%  
attach_part("See attachment for the updated report on individual schools.

What you should know about these reports:
1. Known duplicate school names are identified and school names is adjusted so enrollments aren't diluted among similar sounding school names in the same market.
2. If student email has a known school domain, then that school is assigned to enrollment
3. If enrollment is in a class at a venue with private classes, then that school is assigned
4. Only schools that meet both the Net BoB criteria (> $5000 annually) and the relationship criteria (some ability to influence) are included
5. Since the rate at which we capture school name varies, these reports compare that rate for 
each time period and adjust the baseline numbers for an 'apples to apples' comparison
6. The primary data source for these reports is the PC NetBoB Daily report that comes out each morning from the Business Intelligence team
7. There are processes for fixing enrollments that are miscoded and for schools to newly qualify over the $5000 threshold at least quarterly
8. The lists relied upon for each of these steps (duplicate school names, private class venues, etc) can and should be updated with all available information.  Contact sam.pritchard@kaplan.com if you have new info.

                  ") %>%
        attach_file("Individual.BD.Tri-Report.v4.pdf") -> html_msg
      send_message(html_msg)
      
      #cut top row from file for next loop
      #R Markdown file relies on row one, so to have unique files, must remove top row and restore at end
      
      recip.counter.cut <- recip.counter[-1,]
      write.csv(recip.counter.cut, file.path.use, row.names = FALSE)
      print(paste("end of loop", i))
    }
    write.csv(full.counter.to.restore, file.path.use, row.names = FALSE)
    
  }
  
  pcnetbob.download()
  reportee.loop(file.name.in.az = file.name.in.az)
}