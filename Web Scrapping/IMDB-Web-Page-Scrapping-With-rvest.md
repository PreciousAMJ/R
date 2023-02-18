IMDB Web Page Scrapping With rvest
================

Load the packages we’ll be using

``` r
library(rvest)

library(stringr)

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

we’ll get the html content from the web page

``` r
link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure" # The link to the page we want to scrap

page = read_html(link) # we'll get the html content of the page using "read_html" function from rvest

print(page)
```

    ## {html_document}
    ## <html xmlns:og="http://ogp.me/ns#" xmlns:fb="http://www.facebook.com/2008/fbml">
    ## [1] <head>\n<meta http-equiv="Content-Type" content="text/html; charset=UTF-8 ...
    ## [2] <body id="styleguide-v2" class="fixed">\n            <img height="1" widt ...

we’ll use the ‘SelectorGadget’ browser extension (can be downloaded from
the edge extension store) to identify the nodes containing each
text/link we want to extract we’ll extract the text from html_node
containing the Movie Title.

``` r
Titles = page %>%
  html_nodes(".lister-item-header a") %>%
  html_text()

print(Titles)
```

    ##  [1] "Black Panther: Wakanda Forever"                   
    ##  [2] "Avatar: The Way of Water"                         
    ##  [3] "Plane"                                            
    ##  [4] "Everything Everywhere All at Once"                
    ##  [5] "Puss in Boots: The Last Wish"                     
    ##  [6] "Pathaan"                                          
    ##  [7] "Black Panther"                                    
    ##  [8] "Avatar"                                           
    ##  [9] "Harry Potter and the Sorcerer's Stone"            
    ## [10] "Black Adam"                                       
    ## [11] "Interstellar"                                     
    ## [12] "Jurassic World: Dominion"                         
    ## [13] "The Hunger Games"                                 
    ## [14] "The Northman"                                     
    ## [15] "Dune: Part One"                                   
    ## [16] "RRR (Rise Roar Revolt)"                           
    ## [17] "The Lord of the Rings: The Fellowship of the Ring"
    ## [18] "Thor: Love and Thunder"                           
    ## [19] "Inception"                                        
    ## [20] "Spider-Man: No Way Home"                          
    ## [21] "Gladiator"                                        
    ## [22] "Avengers: Endgame"                                
    ## [23] "Strange World"                                    
    ## [24] "Arctic"                                           
    ## [25] "Puss in Boots"                                    
    ## [26] "Raiders of the Lost Ark"                          
    ## [27] "Minions: The Rise of Gru"                         
    ## [28] "Independence Day"                                 
    ## [29] "Inglourious Basterds"                             
    ## [30] "No Time to Die"                                   
    ## [31] "Uncharted"                                        
    ## [32] "Harry Potter and the Goblet of Fire"              
    ## [33] "The Suicide Squad"                                
    ## [34] "The Mummy"                                        
    ## [35] "Back to the Future"                               
    ## [36] "Jurassic Park"                                    
    ## [37] "Kingsman: The Golden Circle"                      
    ## [38] "The Goonies"                                      
    ## [39] "Willow"                                           
    ## [40] "The Grand Budapest Hotel"                         
    ## [41] "Edge of Tomorrow"                                 
    ## [42] "Dances with Wolves"                               
    ## [43] "The Lord of the Rings: The Return of the King"    
    ## [44] "Sing 2"                                           
    ## [45] "The Lost City"                                    
    ## [46] "The Blue Lagoon"                                  
    ## [47] "The Hunger Games: Catching Fire"                  
    ## [48] "The King's Man"                                   
    ## [49] "Doctor Strange in the Multiverse of Madness"      
    ## [50] "Beast"

To get the movie year

``` r
Year = page %>%
  html_nodes(".text-muted.unbold") %>%
  html_text()

print(Year)
```

    ##  [1] "(2022)"     "(2022)"     "(2023)"     "(2022)"     "(2022)"    
    ##  [6] "(2023)"     "(2018)"     "(2009)"     "(2001)"     "(2022)"    
    ## [11] "(2014)"     "(2022)"     "(2012)"     "(2022)"     "(2021)"    
    ## [16] "(2022)"     "(2001)"     "(2022)"     "(2010)"     "(2021)"    
    ## [21] "(2000)"     "(2019)"     "(2022)"     "(2018)"     "(2011)"    
    ## [26] "(1981)"     "(2022)"     "(1996)"     "(2009)"     "(2021)"    
    ## [31] "(2022)"     "(2005)"     "(2021)"     "(1999)"     "(1985)"    
    ## [36] "(1993)"     "(2017)"     "(1985)"     "(1988)"     "(2014)"    
    ## [41] "(2014)"     "(1990)"     "(2003)"     "(2021)"     "(2022)"    
    ## [46] "(1980)"     "(2013)"     "(2021)"     "(2022)"     "(I) (2022)"

From the output, we still need to remove the both paranthesis in the
text

``` r
remove <- c("[(]", "[)]" ) # we have to include the square bracket, because the "(" is a special character.

for (x in remove){
  Year <- str_remove(Year, x)
  }

Year
```

    ##  [1] "2022"     "2022"     "2023"     "2022"     "2022"     "2023"    
    ##  [7] "2018"     "2009"     "2001"     "2022"     "2014"     "2022"    
    ## [13] "2012"     "2022"     "2021"     "2022"     "2001"     "2022"    
    ## [19] "2010"     "2021"     "2000"     "2019"     "2022"     "2018"    
    ## [25] "2011"     "1981"     "2022"     "1996"     "2009"     "2021"    
    ## [31] "2022"     "2005"     "2021"     "1999"     "1985"     "1993"    
    ## [37] "2017"     "1985"     "1988"     "2014"     "2014"     "1990"    
    ## [43] "2003"     "2021"     "2022"     "1980"     "2013"     "2021"    
    ## [49] "2022"     "I (2022)"

To get the movie rating

``` r
Rating = page %>%
  html_nodes(".ratings-imdb-rating strong") %>%
  html_text()

print(Rating)
```

    ##  [1] "6.8" "7.8" "6.5" "8.0" "7.9" "6.6" "7.3" "7.9" "7.6" "6.4" "8.6" "5.6"
    ## [13] "7.2" "7.1" "8.0" "7.9" "8.8" "6.3" "8.8" "8.2" "8.5" "8.4" "5.6" "6.8"
    ## [25] "6.6" "8.4" "6.6" "7.0" "8.3" "7.3" "6.3" "7.7" "7.2" "7.1" "8.5" "8.2"
    ## [37] "6.7" "7.7" "7.2" "8.1" "7.9" "8.0" "9.0" "7.4" "6.1" "5.8" "7.5" "6.3"
    ## [49] "6.9" "5.6"

To get the movie synopsis

``` r
Synopsis = page %>%
  html_nodes(".ratings-bar+ .text-muted") %>%
  html_text()

print(Synopsis)
```

    ##  [1] "\nThe people of Wakanda fight to protect their home from intervening world powers as they mourn the death of King T'Challa."                                                                                                                                                                                   
    ##  [2] "\nJake Sully lives with his newfound family formed on the extrasolar moon Pandora. Once a familiar threat returns to finish what was previously started, Jake must work with Neytiri and the army of the Na'vi race to protect their home."                                                                    
    ##  [3] "\nA pilot finds himself caught in a war zone after he's forced to land his commercial aircraft during a terrible storm."                                                                                                                                                                                       
    ##  [4] "\nA middle-aged Chinese immigrant is swept up into an insane adventure in which she alone can save existence by exploring other universes and connecting with the lives she could have led."                                                                                                                   
    ##  [5] "\nWhen Puss in Boots discovers that his passion for adventure has taken its toll and he has burned through eight of his nine lives, he launches an epic journey to restore them by finding the mythical Last Wish."                                                                                            
    ##  [6] "\nAn Indian spy takes on the leader of a group of mercenaries who have nefarious plans to target his homeland."                                                                                                                                                                                                
    ##  [7] "\nT'Challa, heir to the hidden but advanced kingdom of Wakanda, must step forward to lead his people into a new future and must confront a challenger from his country's past."                                                                                                                                
    ##  [8] "\nA paraplegic Marine dispatched to the moon Pandora on a unique mission becomes torn between following his orders and protecting the world he feels is his home."                                                                                                                                             
    ##  [9] "\nAn orphaned boy enrolls in a school of wizardry, where he learns the truth about himself, his family and the terrible evil that haunts the magical world."                                                                                                                                                   
    ## [10] "\nNearly 5,000 years after he was bestowed with the almighty powers of the Egyptian gods--and imprisoned just as quickly--Black Adam is freed from his earthly tomb, ready to unleash his unique form of justice on the modern world."                                                                         
    ## [11] "\nA team of explorers travel through a wormhole in space in an attempt to ensure humanity's survival."                                                                                                                                                                                                         
    ## [12] "\nFour years after the destruction of Isla Nublar, Biosyn operatives attempt to track down Maisie Lockwood, while Dr Ellie Sattler investigates a genetically engineered swarm of giant insects."                                                                                                              
    ## [13] "\nKatniss Everdeen voluntarily takes her younger sister's place in the Hunger Games: a televised competition in which two teenagers from each of the twelve Districts of Panem are chosen at random to fight to the death."                                                                                    
    ## [14] "\nA young Viking prince is on a quest to avenge his father's murder."                                                                                                                                                                                                                                          
    ## [15] "\nA noble family becomes embroiled in a war for control over the galaxy's most valuable asset while its heir becomes troubled by visions of a dark future."                                                                                                                                                    
    ## [16] "\nA fictitious story about two legendary revolutionaries and their journey away from home before they started fighting for their country in the 1920s."                                                                                                                                                        
    ## [17] "\nA meek Hobbit from the Shire and eight companions set out on a journey to destroy the powerful One Ring and save Middle-earth from the Dark Lord Sauron."                                                                                                                                                    
    ## [18] "\nThor enlists the help of Valkyrie, Korg and ex-girlfriend Jane Foster to fight Gorr the God Butcher, who intends to make the gods extinct."                                                                                                                                                                  
    ## [19] "\nA thief who steals corporate secrets through the use of dream-sharing technology is given the inverse task of planting an idea into the mind of a C.E.O., but his tragic past may doom the project and his team to disaster."                                                                                
    ## [20] "\nWith Spider-Man's identity now revealed, Peter asks Doctor Strange for help. When a spell goes wrong, dangerous foes from other worlds start to appear, forcing Peter to discover what it truly means to be Spider-Man."                                                                                     
    ## [21] "\nA former Roman General sets out to exact vengeance against the corrupt emperor who murdered his family and sent him into slavery."                                                                                                                                                                           
    ## [22] "\nAfter the devastating events of Avengers: Infinity War (2018), the universe is in ruins. With the help of remaining allies, the Avengers assemble once more in order to reverse Thanos' actions and restore balance to the universe."                                                                        
    ## [23] "\nThe legendary Clades are a family of explorers whose differences threaten to topple their latest and most crucial mission."                                                                                                                                                                                  
    ## [24] "\nA man stranded in the Arctic after a plane crash must decide whether to remain in the relative safety of his makeshift camp or to embark on a deadly trek through the unknown."                                                                                                                              
    ## [25] "\nAn outlaw cat, his childhood egg-friend, and a seductive thief kitty set out in search for the eggs of the fabled Golden Goose to clear his name, restore his lost honor, and regain the trust of his mother and town."                                                                                      
    ## [26] "\nIn 1936, archaeologist and adventurer Indiana Jones is hired by the U.S. government to find the Ark of the Covenant before the Nazis can obtain its awesome powers."                                                                                                                                         
    ## [27] "\nThe untold story of one twelve-year-old's dream to become the world's greatest supervillain."                                                                                                                                                                                                                
    ## [28] "\nThe aliens are coming and their goal is to invade and destroy Earth. Fighting superior technology, mankind's best weapon is the will to survive."                                                                                                                                                            
    ## [29] "\nIn Nazi-occupied France during World War II, a plan to assassinate Nazi leaders by a group of Jewish U.S. soldiers coincides with a theatre owner's vengeful plans for the same."                                                                                                                            
    ## [30] "\nJames Bond has left active service. His peace is short-lived when Felix Leiter, an old friend from the CIA, turns up asking for help, leading Bond onto the trail of a mysterious villain armed with dangerous new technology."                                                                              
    ## [31] "\nStreet-smart Nathan Drake is recruited by seasoned treasure hunter Victor \"Sully\" Sullivan to recover a fortune amassed by Ferdinand Magellan, and lost 500 years ago by the House of Moncada."                                                                                                            
    ## [32] "\nHarry Potter finds himself competing in a hazardous tournament between rival schools of magic, but he is distracted by recurring nightmares."                                                                                                                                                                
    ## [33] "\nSupervillains Harley Quinn, Bloodsport, Peacemaker, and a collection of nutty cons at Belle Reve prison join the super-secret, super-shady Task Force X as they are dropped off at the remote, enemy-infused island of Corto Maltese."                                                                       
    ## [34] "\nAt an archaeological dig in the ancient city of Hamunaptra, an American serving in the French Foreign Legion accidentally awakens a mummy who begins to wreak havoc as he searches for the reincarnation of his long-lost love."                                                                             
    ## [35] "\nMarty McFly, a 17-year-old high school student, is accidentally sent 30 years into the past in a time-traveling DeLorean invented by his close friend, the maverick scientist Doc Brown."                                                                                                                    
    ## [36] "\nA pragmatic paleontologist touring an almost complete theme park on an island in Central America is tasked with protecting a couple of kids after a power failure causes the park's cloned dinosaurs to run loose."                                                                                          
    ## [37] "\nAfter the Kingsman's headquarters are destroyed and the world is held hostage, an allied spy organization in the United States is discovered. These two elite secret agencies must band together to defeat a common enemy."                                                                                  
    ## [38] "\nA group of young misfits called The Goonies discover an ancient map and set out on an adventure to find a legendary pirate's long-lost treasure."                                                                                                                                                            
    ## [39] "\nA young farmer is chosen to undertake a perilous journey in order to protect a special baby from an evil queen."                                                                                                                                                                                             
    ## [40] "\nA writer encounters the owner of an aging high-class hotel, who tells him of his early years serving as a lobby boy in the hotel's glorious years under an exceptional concierge."                                                                                                                           
    ## [41] "\nA soldier fighting aliens gets to relive the same day over and over again, the day restarting every time he dies."                                                                                                                                                                                           
    ## [42] "\nLieutenant John Dunbar, assigned to a remote western Civil War outpost, befriends wolves and Native Americans, making him an intolerable aberration in the military."                                                                                                                                        
    ## [43] "\nGandalf and Aragorn lead the World of Men against Sauron's army to draw his gaze from Frodo and Sam as they approach Mount Doom with the One Ring."                                                                                                                                                          
    ## [44] "\nBuster Moon and his friends must persuade reclusive rock star Clay Calloway to join them for the opening of a new show."                                                                                                                                                                                     
    ## [45] "\nA reclusive romance novelist on a book tour with her cover model gets swept up in a kidnapping attempt that lands them both in a cutthroat jungle adventure."                                                                                                                                                
    ## [46] "\nIn the Victorian period, two children are shipwrecked on a tropical island in the South Pacific. With no adults to guide them, the two make a simple life together, unaware that sexual maturity will eventually intervene."                                                                                 
    ## [47] "\nKatniss Everdeen and Peeta Mellark become targets of the Capitol after their victory in the 74th Hunger Games sparks a rebellion in the Districts of Panem."                                                                                                                                                 
    ## [48] "\nIn the early years of the 20th century, the Kingsman agency is formed to stand against a cabal plotting a war to wipe out millions."                                                                                                                                                                         
    ## [49] "\nDoctor Strange teams up with a mysterious teenage girl from his dreams who can travel across multiverses, to battle multiple threats, including other-universe versions of himself, which threaten to wipe out millions across the multiverse. They seek help from Wanda the Scarlet Witch, Wong and others."
    ## [50] "\nA father and his two teenage daughters find themselves hunted by a massive rogue lion intent on proving that the Savanna has but one apex predator."

To get the movie URL

``` r
Movie_url = page %>%
  html_nodes(".lister-item-header a") %>%
  html_attr("href")

Movie_url = str_c("https://www.imdb.com", Movie_url, sep = "", collapse = NULL)

print(Movie_url)
```

    ##  [1] "https://www.imdb.com/title/tt9114286/?ref_=adv_li_tt" 
    ##  [2] "https://www.imdb.com/title/tt1630029/?ref_=adv_li_tt" 
    ##  [3] "https://www.imdb.com/title/tt5884796/?ref_=adv_li_tt" 
    ##  [4] "https://www.imdb.com/title/tt6710474/?ref_=adv_li_tt" 
    ##  [5] "https://www.imdb.com/title/tt3915174/?ref_=adv_li_tt" 
    ##  [6] "https://www.imdb.com/title/tt12844910/?ref_=adv_li_tt"
    ##  [7] "https://www.imdb.com/title/tt1825683/?ref_=adv_li_tt" 
    ##  [8] "https://www.imdb.com/title/tt0499549/?ref_=adv_li_tt" 
    ##  [9] "https://www.imdb.com/title/tt0241527/?ref_=adv_li_tt" 
    ## [10] "https://www.imdb.com/title/tt6443346/?ref_=adv_li_tt" 
    ## [11] "https://www.imdb.com/title/tt0816692/?ref_=adv_li_tt" 
    ## [12] "https://www.imdb.com/title/tt8041270/?ref_=adv_li_tt" 
    ## [13] "https://www.imdb.com/title/tt1392170/?ref_=adv_li_tt" 
    ## [14] "https://www.imdb.com/title/tt11138512/?ref_=adv_li_tt"
    ## [15] "https://www.imdb.com/title/tt1160419/?ref_=adv_li_tt" 
    ## [16] "https://www.imdb.com/title/tt8178634/?ref_=adv_li_tt" 
    ## [17] "https://www.imdb.com/title/tt0120737/?ref_=adv_li_tt" 
    ## [18] "https://www.imdb.com/title/tt10648342/?ref_=adv_li_tt"
    ## [19] "https://www.imdb.com/title/tt1375666/?ref_=adv_li_tt" 
    ## [20] "https://www.imdb.com/title/tt10872600/?ref_=adv_li_tt"
    ## [21] "https://www.imdb.com/title/tt0172495/?ref_=adv_li_tt" 
    ## [22] "https://www.imdb.com/title/tt4154796/?ref_=adv_li_tt" 
    ## [23] "https://www.imdb.com/title/tt10298840/?ref_=adv_li_tt"
    ## [24] "https://www.imdb.com/title/tt6820256/?ref_=adv_li_tt" 
    ## [25] "https://www.imdb.com/title/tt0448694/?ref_=adv_li_tt" 
    ## [26] "https://www.imdb.com/title/tt0082971/?ref_=adv_li_tt" 
    ## [27] "https://www.imdb.com/title/tt5113044/?ref_=adv_li_tt" 
    ## [28] "https://www.imdb.com/title/tt0116629/?ref_=adv_li_tt" 
    ## [29] "https://www.imdb.com/title/tt0361748/?ref_=adv_li_tt" 
    ## [30] "https://www.imdb.com/title/tt2382320/?ref_=adv_li_tt" 
    ## [31] "https://www.imdb.com/title/tt1464335/?ref_=adv_li_tt" 
    ## [32] "https://www.imdb.com/title/tt0330373/?ref_=adv_li_tt" 
    ## [33] "https://www.imdb.com/title/tt6334354/?ref_=adv_li_tt" 
    ## [34] "https://www.imdb.com/title/tt0120616/?ref_=adv_li_tt" 
    ## [35] "https://www.imdb.com/title/tt0088763/?ref_=adv_li_tt" 
    ## [36] "https://www.imdb.com/title/tt0107290/?ref_=adv_li_tt" 
    ## [37] "https://www.imdb.com/title/tt4649466/?ref_=adv_li_tt" 
    ## [38] "https://www.imdb.com/title/tt0089218/?ref_=adv_li_tt" 
    ## [39] "https://www.imdb.com/title/tt0096446/?ref_=adv_li_tt" 
    ## [40] "https://www.imdb.com/title/tt2278388/?ref_=adv_li_tt" 
    ## [41] "https://www.imdb.com/title/tt1631867/?ref_=adv_li_tt" 
    ## [42] "https://www.imdb.com/title/tt0099348/?ref_=adv_li_tt" 
    ## [43] "https://www.imdb.com/title/tt0167260/?ref_=adv_li_tt" 
    ## [44] "https://www.imdb.com/title/tt6467266/?ref_=adv_li_tt" 
    ## [45] "https://www.imdb.com/title/tt13320622/?ref_=adv_li_tt"
    ## [46] "https://www.imdb.com/title/tt0080453/?ref_=adv_li_tt" 
    ## [47] "https://www.imdb.com/title/tt1951264/?ref_=adv_li_tt" 
    ## [48] "https://www.imdb.com/title/tt6856242/?ref_=adv_li_tt" 
    ## [49] "https://www.imdb.com/title/tt9419884/?ref_=adv_li_tt" 
    ## [50] "https://www.imdb.com/title/tt13223398/?ref_=adv_li_tt"

The steps above will suffice if our goal is to scrap only the first page
of the weblink, in which case we’ll just add each variable into a
dataframe.

What we’ll do is to create a for loop that will scrap each page (first 3
pages) using the same code from the steps above.

``` r
Movies <- data.frame() # create an empty dataframe to receive our data.

for (page_result in seq(from = 1, to = 101, by = 50))
  {link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&start=",  page_result, "&ref_=adv_nxt")
                 page = read_html(link)
                 
                 Titles = page %>%
                   html_nodes(".lister-item-header a") %>%
                   html_text()
                 
                 Year = page %>%
                   html_nodes(".text-muted.unbold") %>%
                   html_text()
                 
                remove <- c("[(]", "[)]" )

                for (x in remove){Year <- str_remove(Year, x)}
                 
                 
                 Rating = page %>%
                   html_nodes(".ratings-imdb-rating strong") %>%
                   html_text()
                 
                 
                 Synopsis = page %>%
                   html_nodes(".ratings-bar+ .text-muted") %>%
                   html_text()
                 
                 
                 Movie_url = page %>%
                   html_nodes(".lister-item-header a") %>%
                   html_attr("href")%>%
                   str_c()
                 
                 Movie_url = str_c("https://www.imdb.com", Movie_url, sep = "", collapse = NULL)
                 
                 
                 
               Movies = rbind(Movies, data.frame(Titles,Year,Rating,Synopsis,Movie_url, stringsAsFactors = FALSE))}  # rbind prevents the new data from overriding the old ones. instead, they'll join the old ones as a row. The first argument (Movies, a dataframe we've created earlier), will take in the old data, the second argument(dataframe) will take in the new data and the rbind function adds it to the old ones a new set of rows.

View(Movies)
```

Next, we’ll write the dataframe into a csv file so we can save it as csv
file.

``` r
setwd("C:\\Users\\HP\\Documents\\Datasets") # to set the working directory - where our csv file we'll be saved

Movies <- as.data.frame(Movies) # to convert book to a dataframe, this is needed so we can write it into a csv file

library("readr") # the library we'll using to write the csv
```

    ## 
    ## Attaching package: 'readr'

    ## The following object is masked from 'package:rvest':
    ## 
    ##     guess_encoding

``` r
write_csv(Movies, file = "C:\\Users\\HP\\Documents\\Datasets\\Movies.csv")
```
