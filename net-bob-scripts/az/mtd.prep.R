# THIS SCRIPT PREPARES A MTD OBJECT READ IN BY new.month.R FOR USE IN R MARKDOWN RECURRING FIELD REPORTS


#1 utilize new.month.R to read in the latest PC NetBoB Daily report
#2 do three.fix to it and the appending
#3 join it to a file that has prior data from 2016-01-01 up until the end of the most recent month
#  this file should already be "fixed" and appended
#4 format should be points table first, horiz.bars next and % net bob last for MTD, QTD, YTD
# Arguments needed: YTD start date, QTD start date, MTD start date, 
# (most recent date should be able to be pulled automatically)
# probably still best to keep functions separate: 
#1 to read in, fix, append, and join, 
#2 to use that file to generate the reports


#1 Import new mtd object with new.month.R
#2 combine it with old monthly files found in R/net-bob-scripts/monthly.data
#3 Use hug.handshake.dates.R with a YTD net bob object and a prior year object including the current month to create a single opbject with matching timelines

#4, 5, 7 : three.fix.R but doesn't do enrollment adjustments
#4 use email.glean.R to add in school info using email domains
#5 use priv.venue.fix2.R to add in school info using private venue list
#6 add any fixes from the enrollment adjustment doc using market.school.fix.R
#7 use dupe.schools.R to switch duplicated school names
#7 check for any schools that newly qualified as hug or handshake using hhpart.ytdcalc.R to update the partners file
#8 append the category and include.exclude using hhpart.append.R
#9 add owners using cols.owns2.R
#10 use pivot.growth.R which includes aggregate function to make final calculations of numbers year over year (or dump into excel for now)


#example import friday.yy <- friday.update(apr425, apr16, q1.17f, class.data, "2016-04-01", "2017-04-25")

mtd.prep <- function(mtd.obj, class.data.obj, st.date.priormnth, end.date.curmnth, cache.if.exists = TRUE){
  
  cached.file.name <- paste0("D://Users/SPritchard/Music/Documents/R/cached/netbob.fix.thru", gsub("-", "", end.date.curmnth),".csv")
  if(cache.if.exists == FALSE){
    cached.file.name <- "D://Users/SPritchard/Music/Documents/R/cached/fake.name/"
  }
  
  if(file.exists(cached.file.name)){
    cy.append2 <- read.csv(cached.file.name, stringsAsFactors = FALSE)
  } else{
  
  three.fix <- function(df.to.fix, class.data.object, start.date, end.date){
    
    #READ IN email.glean.R (inside three.fix.R)
    
    email.glean <- function(df.to.fix) {
      df.f <- df.to.fix
      df.len <- nrow(df.f)
      df.col.names <- colnames(df.f)
      if(df.col.names[36] != "EMAIL1"){
        print("ERROR: Column 36 not EMAIL1 in input")
      }
      split.addr <- strsplit(df.f[,36], "@")
      doms <- character(length = 0)
      for(i in 1:df.len){
        dom.i <- split.addr[[i]][2]
        doms <- c(doms, dom.i)
        
      }
      print("domains extracted")
      df.wdoms <- cbind(df.f, doms)
      #print(head(df.wdoms, 1))
      
      
      sch.emails <- read.csv("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/emaildoms.schools2.csv", stringsAsFactors = FALSE)
      col.sch.emails <- colnames(sch.emails)
      sch.emails.rows <- nrow(sch.emails)
      all.doms <- sch.emails[,3]
      #print(class(all.doms))
      #print(colnames(sch.emails))
      
      #fixed.enrolls <- vector(mode = "character", length = 39)
      for(t in 1:sch.emails.rows){
        df.f.t <- df.wdoms[df.wdoms$doms %in% all.doms[t],]
        msg.t <- paste0("checking email for row ", t, " of ", sch.emails.rows)
        print(msg.t)
        
        t.name <- row.names(df.f.t)
        
        if(nrow(df.f.t) >=1) {
          for(q in 1:nrow(df.f.t)){
            if(df.f.t[q,21] == ""){
              df.f.t[q,21] <- sch.emails[t, 4]
            }
          }
          
        } 
        
        if(nrow(df.f.t) >=1) {
          for(q in 1:nrow(df.f.t)){
            df.wdoms[rownames(df.wdoms) == t.name[q],] <- df.f.t[q,]
          }
        }
     
      }
      df.wdoms
    }
    # END OF email.glean.R (inside three.fix.R)
    
    #READ IN priv.venue.fix2.R (inside three.fix.R)
    
    priv.venue.fix2 <- function(df.to.fix, now.class.object, start.date, end.date){
      
      all.netbob <- sum(df.to.fix$NETBOB)
      #print(paste("all net bob equals", all.netbob))
      missing.r1 <- df.to.fix[df.to.fix$SCHOOL.NAME == "",]
      missing.nb1 <- sum(missing.r1[,27])
      print(paste("all net bob equals", all.netbob))
      #print(paste("missing net bob equals", missing.nb1))
      
      now.class.object[,15] <- as.Date(now.class.object[,15], "%m/%d/%Y")
      sdate <- as.Date(start.date)
      edate <- as.Date(end.date)
      now.class.short <- now.class.object[now.class.object$Enrollment.Date >= sdate & now.class.object$Enrollment.Date <= edate ,]
      
      
      pri.vens <- read.csv("D://USers/SPritchard/Music/Documents/R/net-bob-scripts/private-class-venues.csv", stringsAsFactors = FALSE)
      
      classobj.ids <- now.class.short[now.class.short$Market %in% pri.vens$facility.market 
                                      & now.class.short$Facility %in% pri.vens$facility, ]
      pri.ids.sch <- merge(classobj.ids, pri.vens, by.x = "Facility", by.y = "facility", all.y = FALSE)
      #pri.narrow <- pri.ids[,c(8,10,12,13,14,15)]
      
      #print(head(classobj.ids))
      print(head(pri.ids.sch))
      
      df.f <- df.to.fix
      df.col.names <- colnames(df.f)
      adjusts <- pri.ids.sch
      adjust.rows <- nrow(adjusts)
      adjusts.ids <- adjusts[,12]
      market.hier <- read.csv("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/market.hierarchy.csv", stringsAsFactors = FALSE)
      #print(head(adjusts.ids))
      fixed.enrolls <- vector(mode = "character", length = 22)
      for(i in 1:adjust.rows){
        df.f.i <- df.f[df.f$ENROLLMENT.ID %in% adjusts.ids[i],]
        
        msg.i <- paste0("Checking EID ", i, " of ", adjust.rows, " in private class venues")
        print(msg.i)
        
        #print("df.f.i dim equals")
        #print(dim(df.f.i))
        #print(paste("i equals", i))
        i.name <- row.names(df.f.i)
        #print(i.name)
        #print(paste("df.f dim equals", dim(df.f)))
        #print(head(df.f, 2))
        
        if(nrow(df.f.i) >= 1) { df.f.i[,4] <- adjusts[i, 25]} #market
        
        i.hier <- market.hier[market.hier$MARKET_NAME == adjusts[i, 25],]
        #print(adjusts[i, 25])
        #print(i.hier)
        df.f.i[,c(1,2,3)] <- i.hier[,c(1,2,3)] # aligns territory, region & market group with new market
        
        if(nrow(df.f.i) >= 1) { df.f.i[,21] <- adjusts[i, 24]} #school
        #print(df.f.i)
        df.f.i <- df.f.i[1,] #first row only in case of mult entries with EID such as drop switch
        df.f[rownames(df.f) == i.name,] <- df.f.i
        #print(i)
        #print(df.f.i)
      }
      #print(paste("all net bob equals", all.netbob))
      #print(paste("missing net bob equals", missing.nb1))
      
      missing.r2 <- df.f[df.f$SCHOOL.NAME == "",]
      #print(colnames(df.f))
      missing.nb2 <- sum(missing.r2[,27])
      #print(paste("missing net bob after priv.venue.fix2 equals", missing.nb2))
      perc.miss1 <- (missing.nb1/all.netbob)
      perc.miss2 <- (missing.nb2/all.netbob)
      print(paste0("% of Net Bob missing school in input = ", (round(perc.miss1, 3)*100), "%"))
      
      print(paste0("% of Net Bob missing school in OUTPUT = ", (round(perc.miss2, 3))*100, "%"))
      print(paste("Additional Net Bob assigned school name =", round(missing.nb1-missing.nb2)))
      df.f
      
      #print(head(now.class.short))
    }
    
    # END OF READ IN priv.venue.fix2.R (inside three.fix.R)
    
    # READ IN dupe.schools.R (inside three.fix.R)
    
    dupe.schools <- function(df.to.fix){
      dfs <- df.to.fix
      dschools.tbl <- read.csv("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/dupe.schools.csv", stringsAsFactors = FALSE)
      print("dupe table read in")
      dfs.rows <- nrow(dfs)
      print(dfs.rows)
      for(p in 1:dfs.rows){
        print(paste0("Checking for dupes in row ", p, " of ", dfs.rows))
        if(dfs$SCHOOL.NAME[p] %in% dschools.tbl$Duplicate.Name ){
          dupe.row <- dschools.tbl[dschools.tbl$Duplicate.Name == dfs$SCHOOL.NAME[p],]
          if(dfs$CP4.MARKET[p] == dupe.row[1,1]){
            dfs$SCHOOL.NAME[p] <- dupe.row[1,3]  
          }
          
        }
      }
      dfs
    }
    
    # END OF READ IN dupe.schools.R (inside three.fix.R)
    
    # USE FUNCTIONS (inside three.fix.R)
    
    df.gleaned <- email.glean(df.to.fix)
    
    df.priv <- priv.venue.fix2(df.gleaned, class.data.object, start.date, end.date )
    
    df.unduped <- dupe.schools(df.priv)
    
    missing.r1 <- df.to.fix[df.to.fix$SCHOOL.NAME == "",]
    missing.nb1 <- sum(missing.r1$NETBOB, na.rm = TRUE)
    
    missing.r2 <- df.unduped[df.unduped$SCHOOL.NAME == "",]
    missing.nb2 <- sum(missing.r2$NETBOB, na.rm = TRUE)
    all.netbob <- sum(df.to.fix$NETBOB, na.rm = TRUE)
    
    perc.miss1 <- (missing.nb1/all.netbob)
    perc.miss2 <- (missing.nb2/all.netbob)
    print(paste0("% of Net Bob missing school in input = ", (round(perc.miss1, 3)*100), "%"))
    
    print(paste0("% of Net Bob missing school in OUTPUT = ", (round(perc.miss2, 3))*100, "%"))
    print(paste("Additional Net Bob assigned school name =", round(missing.nb1-missing.nb2)))
    
    df.unduped
  }
  # END OF three.fix.R
  
  # Purpose: adds HHW category and whether to include or exclude as a partner school
  # Next step 2: instead of NA schools not on the list or blank should be "Unknown"
  # has an extra row of zeros at top which can be removed
  
  hhpart.append <- function(df.append){
    #1 import filter file
    #2 add hug, handshake, Wave, Unknown, and Exclude, Okay to df.append
    #3 intermediate step of keepiing original doc
    hh.list <- read.csv("D://Users/SPritchard/Music/Documents/R/net-bob-scripts/hh.partners.csv", stringsAsFactors = FALSE)
    df.length <- nrow(df.append)
    df.width <- length(colnames(df.append))
    matrix1 <- matrix(0, ncol = (df.width+2))
    df.new <- as.data.frame(matrix1)
    #print(dim(df.new))
    for(i in 1:df.length){
      i.msg <- paste0("adding categories to row ", i, " of ", df.length)
      print(i.msg)
      df.i <- df.append[i,]
      #print(df.i)
      #print(head(hh.list[,2]))
      #print(df.append$SCHOOL.NAME[i])
      df.i.cats <- hh.list[hh.list[,2] %in% df.append$SCHOOL.NAME[i] 
                           & hh.list[,1] %in% df.append$CP4.MARKET[i] ,c(1,2,3,4)]
      
      # should check that market is correct in addition to school
      if(nrow(df.i.cats)==0){
        df.i.cats2 <- rep(0, 2)
      } else if(nrow(df.i.cats)>=2){
        #df.i.cats2 <- hh.list[hh.list[,1] %in% df.i.cats[,1],]
        df.i.cats2 <- df.i.cats[1,]
      } else if(nrow(df.i.cats)==1){
        df.i.cats2 <- df.i.cats
      }
      #print(df.i.cats)
      #print(nrow(df.i.cats))
      #print(dim(df.i.cats))
      #print("cats2 =")
      #print(df.i.cats2)
      
      df.i.append <- c(df.i, df.i.cats2[c(3:4)])
      df.i.df <- as.data.frame(df.i.append)
      
      cnames <- colnames(df.i.df)
      cname.len <- length(cnames)
      
      cnames[c((cname.len - 1),(cname.len))] <- c("Category", "Include.Exclude") # IF DOMS COL ADDED, MAYBE USE cnames[c(41,42)] <- c("Category", "Include.Exclude")
      colnames(df.i.df) <- cnames
      colnames(df.new) <- colnames(df.i.df)
      df.new <- rbind(df.new, df.i.df)
      
    }
    df.new
  }
  
  
  
  #END OF hhpart.append.R
  
  cols.owns <- function(object, market.column){
    
    object.width <- length(colnames(object))
    
    mark.own <- read.csv("D://Users/SPritchard/Music/Documents/R/zip-sort/market-ownership-old.csv", stringsAsFactors = FALSE)
    
    object.wgeos <- object
    market.col <- object.width
    
    owners.df <- data.frame("bd" = 0, "crd" = 0, "owner" = 0, "rd" = 0)
    owners.cols <- colnames(owners.df)
    vec.len <- nrow(object.wgeos)
    print(head(mark.own, 4))
    for(t in 1:vec.len){
      msg.t <- paste0("adding owners to row ", t, " of ", vec.len)
      print(msg.t)
      owners.t <- if(nrow(mark.own[mark.own[, 4] %in% object.wgeos[t, 4], c(5, 6, 7, 8)])>=1){
        mark.own[mark.own[, 4] %in% object.wgeos[t, 4], c(5, 6, 7, 8)]
      } else {
        data.frame("A"=1, "B"=1, "C"=1, "D"=1)
      }
      owners.t <- owners.t[1,]
      #print(head(mark.own[,4]))
      #print(object.wgeos[t, market.col])
      #print(owners.t)
      colnames(owners.t) <- owners.cols
      owners.df <- rbind(owners.df, owners.t)
      msg2 <- paste("second loop number", t)
      #print(msg2)
    }
    #print("loop 2 complete")
    owners.df <- owners.df[-1,]
    print(head(object.wgeos))
    print(tail(object.wgeos))
    print(head(owners.df))
    print(tail(owners.df))
    
    obj.geo.own <- cbind(object.wgeos, owners.df)
    print("do we get this far?")
    obj.wth <- length(colnames(obj.geo.own))
    
    print("checking split markets")
    ss.evan <- obj.geo.own[(obj.geo.own$CP4.MARKET == "North LA" 
                            | obj.geo.own$CP4.MARKET == "South Shore")
                           | obj.geo.own$CP4.MARKET == "Downers Grove (East)" ,]
    ss.evan.rows <- rownames(ss.evan)
    ss.evan.length <- nrow(ss.evan)
    
    school.table <- read.csv("D://Users/SPritchard/Music/Documents/R/zip-sort/ss.evan.owns.csv", stringsAsFactors = FALSE)
    zip.table <- read.csv("D://Users/SPritchard/Music/Documents/R/zip-sort/zip-to-market.csv", stringsAsFactors = FALSE)
    
    zips <- substr(zip.table[,1], 2, 6)
    zips <- as.numeric(zips)
    zip.table[,1] <- zips
    
    new.owners <- read.csv("D://Users/SPritchard/Music/Documents/R/zip-sort/market-ownership.csv", stringsAsFactors = FALSE)
    
    for(k in 1:ss.evan.length){
      msg.k <- paste0("checking split market zip ", k, " of ", ss.evan.length)
      print(msg.k)
      ss.evan.k <- ss.evan[rownames(ss.evan) == ss.evan.rows[k],]
      #print(paste("k =", k))
      #print(class(ss.evan.k))
      #print(dim(ss.evan.k))
      #print(head(ss.evan.k))
      #print(ss.evan.k)
      #print(ss.evan.k[21] != "")
      #print(ss.evan.k[21] %in% school.table$SCHOOL.NAME)
      
      #print(nrow(school.table[school.table$SCHOOL.NAME %in% ss.evan.k[21],]))
      if((ss.evan.k[21] != "") & (ss.evan.k[21] %in% school.table$SCHOOL.NAME)){
        obj.geo.own[rownames(obj.geo.own) == ss.evan.rows[k], c((obj.wth - 3):obj.wth)] <- school.table[school.table$SCHOOL.NAME == ss.evan.k[1,21],c(13:16)]
        #print(obj.geo.own[rownames(obj.geo.own) == ss.evan.rows[k],]) 
      } else {
        print("no school here")
        ss.evan.kzips <- ss.evan.k[1, c(23:26)]
        ss.evan.kzipsclean <- ss.evan.kzips[!is.na(ss.evan.kzips)]
        zip.check <- ss.evan.kzipsclean[1]
        #print(zip.check)
        
        
        zip.check <- as.character(zip.check)
        if(!is.na(zip.check)){
          
          if(nchar(zip.check) <=3){
            zip.check <- paste0("00",zip.check)
          } else if (nchar(zip.check) == 4){
            zip.check <- paste0("0", zip.check)
          }
          
          zip.check <- as.numeric(zip.check)
          #print(zip.check)
          
          #print(head(zip.table, 2))
          
          new.market <-  zip.table[zip.table[,1] == zip.check, 21 ]
          
          new.market <- new.market[1]
          #print(new.market)
          nmarket.owns <- new.owners[new.owners[,4] == new.market, c(5:8)]
          #print(head(nmarket.owns))
          
          obj.geo.own[rownames(obj.geo.own) == ss.evan.rows[k], c((obj.wth - 3):obj.wth)] <- nmarket.owns
        }
      }
    }
    
    obj.geo.own
  }
  
  #END OF cols.owns3.R
  
  cy.fixed <- three.fix(mtd.obj, class.data.obj, st.date.priormnth, end.date.curmnth)
  cy.append1 <- hhpart.append(cy.fixed)
  cy.append2 <- cols.owns(cy.append1)
  
  #DROP FILE IN CACHE
  cached.file.name <- paste0("D://Users/SPritchard/Music/Documents/R/cached/netbob.fix.thru", gsub("-", "", end.date.curmnth),".csv")
  write.csv(cy.append2, cached.file.name, row.names = FALSE)
  
} # using cached file if available
  
  cy.append2
  
}