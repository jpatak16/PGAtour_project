#this script is similar to webscrape_espn_pga.R
#I am using it to see what tours my ID ranges are so that I know what all options are
#once I know what all options are, I can decide which ones to exclude in the webscrape_espn_pga.R script

pacman::p_load(rvest, tidyverse, memoise)

# as of May 2022, this script will webscrape individual PGA Tour tournament results from espn.com


#place the tournament IDs that you want to scrape in this vector
ids = c(400:500)

#some of our IDs will not exist, and so we will create a function that will help us determine whether or not there is a tournament to webscrape for a give ID
#this function is later used within our main webscraping function
is_error = function(url_id){read_html(url_id) %>% html_element(".leaderboard__content")}

#create a progress bar for the scraping function
progress_bar = txtProgressBar(min=0, max=length(ids), style = 3, char="=")

tour = c()

check_tours = function(url, ids){
  for(i in 1:length(ids)){
    #be nice to the server
    Sys.sleep(2)
    
    #update the progress bar
    setTxtProgressBar(progress_bar, value = i)
    
    #create url var for each individual tournament webpage
    url_id = paste('https://www.espn.com/golf/leaderboard/_/tournamentId/', ids[i], sep = '')
    
    #some of our IDs don't exist and therefore wont produce a table. If we get an error trying to webscrape, we will skip to the next iteration
    e = tryCatch(is_error(url_id), error=function(e){e='yes'})
    if(length(e)==0){next}
    if(e=='yes'){next}
    y = is_error(url_id)
    if(is.na(y[1])){next}
    
    #check what tour each event is on
    pga = read_html(url_id) %>%
      html_node('#fittPageContainer > div:nth-child(4) > div > div.PageLayout__Main > section:nth-child(1) > div > div > nav > ul > li.tabs__list__item.tabs__list__item--active > a') %>%
      html_text()
    tour[length(tour)+1] = pga
  }
  close(progress_bar)
  return(tour)
}

tours = check_tours(ids = ids)

unique(tours)
