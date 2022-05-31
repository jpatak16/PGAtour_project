pacman::p_load(rvest, tidyverse, memoise, lubridate, data.table)

# as of May 2022, this script will webscrape individual PGA Tour tournament results from espn.com


#place the tournament IDs that you want to scrape in this vector
#optimally there is a way to webscrape the same data without having to manually pull the IDs yourself, but I couldn't get the methods I tried to work. Maybe one day I will go back and automate this step
#tID_index.txt has tournament ID number ranges for the 2008 season to present (as of May 2022)
ids = c(411:459, 526:628, 764:846, 892:988, 996:1152, 1181:1220, 1306:1351, 2230:2291, 2482:2605, 2689:2730, 3065:3069, 3735, 
        3742:3803, 401025221:401025271, 401056500:401056560, 401056252, 401077167, 401148233:401148245, 401155413:401155476, 
        401219481:401219501, 401223553, 401223829, 401230732, 401231117:401231118, 401219793:401219802, 401219333:401219480, 
        401256493, 401242996:401243012, 401243401:401243433, 401317529, 401353193:401353294, 401366873)


#some of our IDs will not exist (since we are grabbing a convinient range instead of individually putting in each ID)
#so we will create a function that will help us determine whether or not there is a tournament to webscrape for a given ID
#this function is later used within our main webscraping function
is_error = function(url_id){read_html(url_id) %>% html_element(".leaderboard__content")}

#create a progress bar for the scraping function
progress_bar = txtProgressBar(min=0, max=length(ids), style = 3, char="=")

scrape_espn_pga = function(id){
  #create empty df with the column that we want
  all_tourney = data.frame(position = NA, player = NA, score = NA, cut=NA, withdraw=NA, R1 = NA, R2 = NA, R3= NA, R4=NA, R5 = NA,
                         total = NA, earnings = NA, fedex_points = NA, tournament = NA, start_date = NA, location = NA, course = NA, fedex_cup_event = NA)[-1,]
  for(i in 1:length(id)){
    #be nice to the server
    Sys.sleep(2)
    
    #update the progress bar
    setTxtProgressBar(progress_bar, value = i)
    
    #create url var for each individual tournament webpage
    url_id = paste('https://www.espn.com/golf/leaderboard/_/tournamentId/', id[i], sep = '')
    
    #some of our IDs don't exist and therefore wont produce a table. If we get an error trying to webscrape, we will skip to the next iteration
    e = tryCatch(is_error(url_id), error=function(e){e='yes'})
    if(length(e)==0){next}
    if(e[1]=='yes'){next}
    y = is_error(url_id)
    if(is.na(y[1])){next}
    
    #some of the IDs are for events not on the PGA Tour. Let's eliminate those
    pga = read_html(url_id) %>%
      html_node('#fittPageContainer > div:nth-child(4) > div > div.PageLayout__Main > section:nth-child(1) > div > div > nav > ul > li.tabs__list__item.tabs__list__item--active > a') %>%
      html_text()
    pga = ifelse(grepl("Women", pga)==TRUE, 'LPGA', pga)
    if(pga=='Champions' | pga=='Korn Ferry' | pga=='LPGA' | pga=='DP World' | pga=='ANA Inspiration' | pga=='The Evian Championship'){next}
    
    #some events were canceled, so we don't want to include those empty leaderboards; we also wont include events that are in progress
    cancel = read_html(url_id) %>%
      html_node('.mb4 span') %>%
      html_text()
    if(cancel=='Canceled'){next}
    if(grepl('In Progress', cancel)==TRUE){next}
    
    #webscrape the elements we want from the webpage
    x = read_html(url_id) %>%
      html_element(".leaderboard__content") %>% 
      html_table()
    t = read_html(url_id) %>%
      html_node('.Leaderboard__Event__Title') %>%
      html_text()
    d = read_html(url_id) %>%
      html_node('#fittPageContainer > div:nth-child(4) > div > div > section:nth-child(1) > div > div > div:nth-child(3) > span') %>%
      html_text()
    l = read_html(url_id) %>%
      html_node('.Leaderboard__Course__Location') %>%
      html_text() %>%
      str_split(pattern = ' - ')
    
    #combine the 4 elements we took from the webpage into one table
    x$tourn = t
    x$start_date = d
    x$location = l[[1]][length(l[[1]])]
    x$course = l[[1]][1]
    
    #some tournament pages returned empty dfs, so we won't include them (since there is nothing to include!)
    if(nrow(x)==0){next}
    
    #we don't want to include PGA qualifiers
    if(x$tourn[length(x$tourn)-2] == 'PGA Tour Qualifying Tournament'){next}
    
    #some pages don't have the weird first column we account for in the next step, so we make one for it
    if(colnames(x)[1]=='POS'){x = add_column(x, col1='', .before = 1)}
    
    #some tournaments are lacking most/all round scores. we wont include those
    if(ncol(x) < 10){next}
    if((sum(ifelse(x[,5] == '--' | is.na(x[,5]), 1, 0))/nrow(x))>.9){next}
    
    #some tournament pages pulled things that we dont want. This section eliminates those things
    colnames(x)[1] = 'col1'
    x$col1 = ifelse(x$col1== "" | is.na(x$col1), 'value', x$col1)
    x = x[which(x$col1=='value'),]
    x = x[,-1]
    if(ncol(x)==26){x = x[-seq(11,22)]}
    if(ncol(x)==27){x = x[-seq(12,23)]}
    
    #accounts for 3 round tournaments
    x[[1]] = str_replace(x[[1]], 'T', '') %>% as.numeric()
    if(as.numeric(x[order(x[[1]]),][3,7]) >100){x = add_column(x, R4 = NA, .after = 6)}
    
    #some tournaments had one less round column, so we added a round column for those that need it
    if(ncol(x) <= 14){x = add_column(x, R5 = NA, .after = 7)}
    
    #some iterations of the webscrape added the column headers as a row, so we will take that row out if it is there
    if(x[1,10]=='EARNINGS'){x=x[-1,]}
    if(x[2,10]=='EARNINGS'){x=x[-c(1,2),]}
    
    #some events dont include earnings, so lets add that column (empty)
    if(ncol(x)==13){x = add_column(x, earnings=NA, .after = 9)}
    
    #some listed PGA events are not part of the fedex cup 
    #these include fall series events before the 2013-14 season and some other unofficial pga events in any season
    if(ncol(x)==14){x = add_column(x, fedex_points = 'fs', .after = 10)}
    colnames(x) = colnames(all_tourney)[-c(4,5,18)]
    x$fedex_cup_event = ifelse(x$fedex_points == 'fs', FALSE, TRUE)
    x$fedex_points = ifelse(x$fedex_points == 'fs', NA, x$fedex_points)
    
    #creates new columns using data from our current data frame
    x = add_column(x, cut = ifelse(x$score == 'CUT' | x$score == 'Cut', TRUE, FALSE), .after = 3)
    x = add_column(x, withdraw = ifelse(x$score == 'WD', TRUE, FALSE), .after = 4)
    
    #matches column names of single tourney to our main df
    colnames(x) = colnames(all_tourney)
    
    #converts columns/data into usable class types
    x$player = as.character(paste(x$player, '', sep=''))
    x$position = as.numeric(str_remove_all(x$position, '[T]'))
    x$score = as.numeric(ifelse(x$score=='E', 0, x$score))
    x$earnings = str_remove_all(x$earnings, '[$]') ; x$earnings = str_remove_all(x$earnings, '[,]')
    for(col in 6:13){x[[col]] = as.numeric(x[[col]])}
    date1 = str_split(x$start_date, ' - ')[[1]][1] ; date2 = str_split(x$start_date, ' - ')[[1]][2] ; date2 = str_split(date2, ', ')[[1]][2]
    x$start_date = mdy(paste(date1, date2, sep = ', '))
    x = x %>% arrange(position)
    
    #add the rows for the single tournament df into the df for all tournaments
    all_tourney = rbind(all_tourney, x)
  }
  close(progress_bar)
  return(all_tourney)
}


all_tourn = scrape_espn_pga(id = ids)

fwrite(all_tourn, paste0(getwd(), '/all_tourn.csv'))
