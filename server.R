server <- function(input,output, session){
  

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#
#                                  TAB 1                                    #
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#

  
animes <- eventReactive(input$file ,{
    read.csv("data/animes.csv")
  })

animes_bonus <- eventReactive(input$file ,{
  read.csv("data/animes_bonus.csv")
})


output$video <- renderUI({
  tags$video(src = "animes_home", type = "video/mp4", autoplay = NA, controls = NA)
  })

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#
#                                  TAB 2                                    #
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#


output$def_type <- eventReactive(input$deftype, {
  if(input$type == "Movie"){"A recording of moving images that tells a story and that people watch on a screen or television. 
    Synonymous with film. There are two types of film: Short and Long. 
    A short film is a film that lasts less than thirty minutes, and by definition: a long film lasts thirty minutes or more." 
  }
  else if(input$type == "Music"){"Includes musical anime and Anime Music Video (AMV)."
  }
  else if(input$type == "ONA"){"An original net animation, known in Japan as web anime [ウェブアニメ], is an animation that is directly released onto the Internet."
  }
  else if(input$type == "OVA"){"Original video animation [オリジナル・ビデオ・アニメーション] abbreviated as OVA or OAV, are Japanese animated films and series made specially for release in home video formats without prior showings on television or in theaters."
  }
  else if(input$type == "Special"){"Find the definition !"
  }
  else if(input$type == "TV"){"Shows broadcasted on television.
    Synonymous with 'TV Shows' or 'Series'.
    If the standard length of an episode is generally about 24 minutes, the total number of episodes varies according to the period."
  }
  # else if(input$type == "Other"){"A type for anime that are not affected to any other category."
  # }
})


output$def_genre <- eventReactive(input$defgenre, {
  if(input$genre == "Drama"){"[テレビドラマ] It is the name given to a short television series in all Asian countries, although this genre originated in Japan before spreading to Asia. 
    There is no specific genre associated with dramas, which can even be adaptations of manga or anime, performed by professional actors, but not only. 
    Unlike in France, dramas regularly invite idols appreciated by young people to play in the series, since the audience is generally not very old."
  }
  else if(input$genre == "Action"){"It is a wide spectrum with sub-categories that each qualifies as their own distinguishing articles (in which some are going to be). 
    In some instances, it’s fights with bare knuckles, firearms, and/or non-firearm weapons such as swords and blades and whatever the imagination can come up with. 
    In other instances, action is expressed through sci-fi and there are those where the mystic arts or some form of the supernatural is applied.
    An action scene is essentially a small play, but played seriously, and a majority of action in anime does a great job of portraying that notion. 
    A sequence tells a story of struggle and overcoming the odds and a large body of anime does an excellent job of utilizing that ultimate rule."
  }
  else if(input$genre == "Adventure"){"Adventure is something that creeps into just about any anime. 
    The characters at some point throughout the series have to go from point A to point B, while they may not stock up on items and equipment, many characters do and then set out off towards the horizon. 
    Going on the adventure could be physical in that the characters move around, but it also frequently is, a journey of self discovery and development. 
    The characters are almost never the same from when they start and this is almost always triggered by a smile to let the viewer know that the characters are back to their antics."
  }
  else if(input$genre == "Fantasy"){"Anime that represents imaginary supernatural phenomena, often associated with myth and often represented by the intervention or use of magic."
  }
  else if(input$genre == "Comedy"){"Anime designed to make the audience laugh through amusement and most often work by exaggerating characteristics for humorous effect. 
    Comedy is… something that is at a bit of a crossroads in Japan. 
    While it’s okay for men to laugh, sometimes women can still be frowned upon for laughing in public. 
    Anime however, is one of those safe zones since people aren’t usually watching it in public–except the guy we saw watching it on the train in Akihabara the other day–but at home. "
  }
  else if(input$genre == "Sci-Fi"){"It consists in telling fictions based on scientific and technical progress obtained in a more or less distant future, sometimes in a fictitious past or parallel universe, or progress physically impossible, at least in the current state of our knowledge. 
    Sci-fi anime are almost always set into the future; because that way, whatever is being presented in the story seems more plausible."
  }
  else if(input$genre == "Military"){"Anime truly devoted to war and the military.
    For the most part, expect dramatic, morose stories that will either move you or go right over your head for being too violent."  
  }
  else if(input$genre == "Romance"){"Anime about a love story or love affair, highlighting the passion, emotions and emotional commitment of the main characters. 
    It is cliche and full of hopes and dreams that come out in a story involving love."
  }
  else if(input$genre == "Mystery"){"Anime that revolves around the solution of a problem or a crime. 
    It focuses on the efforts of the detective, private investigator or amateur sleuth to solve the mysterious circumstances of an issue by means of clues, investigation, and clever deduction.
    It generaly tells an interesting enough story to go long way for most viewers.
    One of the facets that mystery anime can do is flaunt the key information in front of you for hours before you realize what you needed was there all along.
    Sometimes, you might even want tp rewatch an episode just to see if there is something that you missed."
  }
  else if(input$genre == "Dementia"){"Anime that treats of madness, insanity usually as a chronic or persistent disorder of the mental processes caused by brain disease or injury and marked by memory disorders, personality changes, and impaired reasoning.
    It can be define as a sub-genre of psychological anime."
  }
  else if(input$genre == "Music"){"[1] Musical Anime is a genre which contains music, songs or dance.
    [2] Anime Music Video (AMV) is a video work made from excerpts of anime (and, to a lesser extent, from cartoons in the broad sense, from video games or from film scenes). 
    The soundtrack of an AMV is generally a music unrelated to the world of animation. The video part is composed of sequences extracted from one or several animated works (sad or epic scenes depending on the music chosen by its creator) and edited in synchronization with the music in order to produce the equivalent of a classic music video. 
    AMV-making is the art of combining animation and music."
  }
  else if(input$genre == "Mecha"){"[メカ] It may refer to both scientific ideas and science-fiction genres that center on giant robots or machines (mechs) controlled by people.
    Mechas are typically depicted as humanoid mobile robots. 'Mecha' is is the shorthand for Mechanical."  
  }
  else if(input$genre == "School"){"Anime that takes place at school, and where students do not learn as strictly as in real life.
    It tends to focus on those moments between classes, during lunch, or before and after school.
    Maybe the students are having full-blown conversations, notes are being passed back and forth between each other, something out of this world is going on just outside the window, or who knows what is about to happen?"
  }
  else if(input$genre == "Game"){"Can refer to anime issued from a licensed game (such as the DBZ, Naruto, etc.), but not necessarily : it seems to fall under a general umbrella of 'Japanese computer and video games'.
    Game anime will always feature a game of some sort, but since watching just a game is boring, it is often paired with other genres like drama, adventure, mystery, comedy, and more."
  }
  else if(input$genre == "Historical"){"Anime that takes place in the past and generally refers to Japan history, but can include historical references from other parts of the world. 
    Due to Japan’s long history before it was united as one country under the shogunate, there were many issues that were rampant across the country especially with the Warring States Period where many a faction was trying to get the most power and consolidate their losses. 
    It also has to deal with the capital moving back and forth between Kyoto, Tokyo, and other areas. While yes, Japan was ravaged by many different elements, the two that stand out the most are poverty reinforced by the social class system and samurai and ronin. 
    Regardless of whatever was going on in Japan, those two elements were rampant across the country and continue to this day, to be a popular medium of entertainment for anime, manga, novels, and games."
  }
  else if(input$genre == "Cars"){"Anime based on car racing."
  }
  else if(input$genre == "Shounen"){"[少年] An anime is considered a shounen if its primary target audience is male teenagers. 
    The recurring themes are 'the initiatory quest of the main characters, encompassing values such as friendship, the taste for effort, group spirit and surpassing oneself'."  
  }
  else if(input$genre == "Harem"){"It is an anime with a male hero surrounded by several girls who almost all love him.
    In general, the anime is focused on the relationships that the girls have with the boy, in order to determine which one will be in a relationship with him."
  }
  else if(input$genre == "Horror"){"An anime genre whose objective is to create a feeling of fear, repulsion or anguish in the viewer."
  }
  else if(input$genre == "Martial Arts"){"Martial arts may be considered as a sub-genre to action. 
    What makes it very distinguishing compared to other action titles is how it can emphasize a specific style, or several styles of novelty martial arts in its choreography/presentation.
    Martial Arts anime also present the philosophical value : the life lessons you can learn from these arts."
  }
  else if(input$genre == "Ecchi"){"[エッチ] Qualifies a category of anime that feature content with sexual connotations. 
    Ecchi works are aimed at a predominantly male target audience whether shōnen or seinen. Some works aimed at a female readership also have ecchi characteristics. 
    These, offer sexual content for romantic purposes.
    Ecchi genre remains quite soft and the sentimental aspect often takes precedence over the sexual aspect."
  }
  else if(input$genre == "Kids"){"Synonymous with kodomo [子供]. It is an anime especially for children."  
  }
  else if(input$genre == "Slice of Life"){"Slice of life anime is a narrative without fantastical aspects, which takes place in a recognisable, everyday setting, such as a suburban high school. 
    It focuses on human relationships that are often romantic in nature. 
    The genre favors the creation of emotional ties with the characters."
  }
  else if(input$genre == "Demons"){"Anime that deals with possession by a demon or Akuma [悪魔] i.e. an evil spirit (from Japanese folklore) that has no form of its own (but is often represented by a butterfly) and is the residue of a tormented human soul.
    Setting aside summoning circles - which we rarely see anyway - anime usually has a regular pattern that allows demons and devils to enter our world and do their work. 
    You have a boy or girl obsessed with the occult who summons a demon who 'happens' to be of the opposite sex, and then they start living together, whether in comedy mode, supernatural battle mode, or, if the victim is lucky, sexy slice-of-life mode."
  }
  else if(input$genre == "Magic"){"Magic anime tend to revolve around beings with great magical power that may even surpass others in the magical world that they live in.
    However, sometimes characters will have to hide the fact that they have powers from mere mortals. 
    To resume, it has to lie in the possession of magical powers that may or may not be in a world of magic."
  }
  else if(input$genre == "Sports"){"Focuses on stories involving sports and other athletic and competitive pursuits."
  }
  else if(input$genre == "Shoujo"){"[少女漫画] An anime is considered a shoujo if its primary target audience is female teenagers.
    'Shoujo' allows us to describe female adolescence, an intermediate period between childhood on the one hand and the status of adult woman on the other hand, embodied by marriage and motherhood."  
  }
  else if(input$genre == "Psychological"){"Where the main focus of the anime is: the mental state of the characters; the effect that the events taking place have on the character's minds; 
    mental disorders such as anxiety, depression, or an existential crisis; abstract, mentally-taxing mindgames."
  }
  else if(input$genre == "Police"){"Includes works that feature the crime or police community. 
    Most Police anime are built around the resolution of an investigation by a policeman or a detective, often featuring the roles of criminals or delinquents in a pronounced way."
  }
  else if(input$genre == "Thriller"){"A suspenseful work, which provides strong sensations.
    One of the great concepts of suspense anime is death or threat of death. 
    These stories are known as death game anime: the characters of the story die one after another and the threat of death persists because we do not know who the killer is. 
    Some Thrillers anime are based on a mystery to be solved or a goal to be reached with sometimes a time limit in which the mission must be completed."
  }
  else if(input$genre == "Parody"){"Relies on shamelessly poking fun at established genres, stereotypes, and sometimes, even people"
  }
  else if(input$genre == "Supernatural"){"Centered on supernatural themes, often contradicting naturalist assumptions of the real world.
    It will always be coupled with another genre or three to lend better to a story."  
  }
  else if(input$genre == "Seinen"){"[青年漫画] A genre whose editorial target is primarily young adult males. 
    These anime were introduced in 1946, as realistic anime with with violent or sexually explicit scenes.
    The target audience is supposed to be older than the shounen's one."
  }
  else if(input$genre == "Samurai"){"Historical anime that focuses on Samurai, which was a warrior affiliated with a feudal lord."
  }
  else if(input$genre == "Super Power"){"Stories where the hero has super power acquired through genetics, or an accident; or that he inherits as a 'chosen one'."
  }
  else if(input$genre == "Vampire"){"Sub-genre of the supernatural which is only about vampies. 
    These anime carry the following concepts: violence, predation of others, darkness. Sometimes a hero faces the final evil at the end, but in many anime, the vampire is the main character! 
    Charged with carrying out the orders of a human or other supernatural being, he uses his powers, which usually exceed those of an ordinary vampire, and strikes quickly and powerfully to decimate those who stand against him. 
    This hero, considered a 'good guy' in a vampire story, is usually a deranged individual who makes everything more difficult while screaming or chanting Bible verses."
  }
  else if(input$genre == "Josei"){"[女性漫画] A genre whose editorial target is primarily young adult females.
    The target audience is supposed to be older than the shoujo's one."  
  }
  else if(input$genre == "Hentai"){"[変態] Means 'perverse', includes pornographic scenes sometimes abnormal. 
    There are often parodies of popular series but also totally original and well constructed stories."
  }
  else if(input$genre == "Yaoi"){"[やおい] Also called boys' love, is in Japanese popular culture a genre of fictional works centered on sentimental and/or sexual relationships between male characters (initially intended for a female audience)."
  }
  else if(input$genre == "Space"){"Anime that takes place in Space: human evolve in galaxies with spaceships and interact with aliens.
    Space anime love to paint the picture that humanity has somehow made it beyond the solar system and is continuing to expand."
  }
  else if(input$genre == "Shoujo Ai"){"Shoujo anime especially love-oriented ('Ai' means 'love'). We could translate these terms by 'love of young girls'."
  }
  else if(input$genre == "Shounen Ai"){"Shounen anime especially love-oriented ('Ai' means 'love'). We could translate these terms by 'love of young boys'."  
  }
  else if(input$genre == "Yuri"){"[百合] Also called Girls' Love, refers to a genre of fiction in Japanese popular culture that focuses on intimate relationships between women, whether emotional, sentimental or sexual. 
    This genre is not only limited to lesbianism, but also to other types of intimate relationships such as spiritual bonds or fusional relationships between women."
  }
})

# Representation of genres by types.

rv <- reactiveValues()

observeEvent(input$mov, {
    rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "Movie") & !is.na(type))
})
observeEvent(input$mus, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "Music") & !is.na(type))
  })
observeEvent(input$ona, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "ONA") & !is.na(type))
  })
observeEvent(input$ova, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "OVA") & !is.na(type))
  })
observeEvent(input$spe, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "Special") & !is.na(type))
  })
observeEvent(input$tv, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "TV") & !is.na(type))
  })
observeEvent(input$oth, {
  rv$typ <- animes() %>% filter((main_genre %in% c("Comedy","Action","Adventure","Drama","Hentai","Fantasy","Music","Kids","Dementia","Historical")) & !is.na(main_genre)) %>%
    filter((type == "Other") & !is.na(type))
})


output$graph_gttype <- renderPlot({
  rv$typ %>% 
    ggplot() + 
    geom_bar(aes(x = main_genre, fill = main_genre), position = "dodge") +
    labs(
      x = "Types",
      y = "Genres",
      title = "Representation of genres by types:"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_linedraw() +
    theme(legend.position = "bottom",
          legend.title = element_text(size=15),
          legend.text = element_text(size=12)) +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12))
})

# Le faire avec possibilité de sélectionner les genres qu'on veut afficher ou non. 

# TABLES of TOP3/5/10 with main_genre.

observeEvent(input$gtop3, {
  rv$gtop <- 3
})
observeEvent(input$gtop5, {
  rv$gtop <- 5
})
observeEvent(input$gtop10, {
  rv$gtop <- 10
})


output$table_gtop <- renderDataTable({
  animes() %>%
      group_by(main_genre) %>%
      summarise(count = n()) %>%
      top_n(n = rv$gtop, wt = count)
})

# GRAPH top10 by type

top_anime_type <- eventReactive(input$top_type, {
  if(input$toptype == "Movie"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("Movie")) & !is.na(type))
  }
  else if(input$toptype == "Music"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("Music")) & !is.na(type))
  }
  else if(input$toptype == "ONA"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("ONA")) & !is.na(type))
  }
  else if(input$toptype == "OVA"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("OVA")) & !is.na(type))
  }
  else if(input$toptype == "Special"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("Special")) & !is.na(type))
  }
  else if(input$toptype == "TV"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("TV")) & !is.na(type))
  }
  else if(input$toptype == "Other"){
    animes() %>% 
      arrange(desc(rating)) %>% 
      group_by(type) %>% slice(1:10) %>% 
      filter((type %in% c("Other")) & !is.na(type))
  }
})

output$graph_toptype <- renderPlot({
  top_anime_type() %>%
    ggplot() + 
    geom_col(aes(x = name,y = rating, fill = name), position = "dodge") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_linedraw() +
    theme(legend.position = "right",
          legend.title = element_text(size=15),
          legend.text = element_text(size=12)) +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
})

# GRAPH top10 by genre 

top_anime_genre <- eventReactive(input$top_genre, {
  if(input$topgenre == "Drama"){
    animes_bonus() %>%
      filter(Drama == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Action"){
    animes_bonus() %>%
      filter(Action == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Adventure"){
    animes_bonus() %>%
      filter(Adventure == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Fantasy"){
    animes_bonus() %>%
      filter(Fantasy == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Comedy"){
    animes_bonus() %>%
      filter(Comedy == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Sci-Fi"){
    animes_bonus() %>%
      filter(Sci_Fi == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Military"){
    animes_bonus() %>%
      filter(Military == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Romance"){
    animes_bonus() %>%
      filter(Romance == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Mystery"){
    animes_bonus() %>%
      filter(Mystery == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Dementia"){
    animes_bonus() %>%
      filter(Dementia == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Music"){
    animes_bonus() %>%
      filter(Music == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Mecha"){
    animes_bonus() %>%
      filter(Mecha == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "School"){
    animes_bonus() %>%
      filter(School == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Game"){
    animes_bonus() %>%
      filter(Game == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Historical"){
    animes_bonus() %>%
      filter(Historical == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Cars"){
    animes_bonus() %>%
      filter(Cars == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Shounen"){
    animes_bonus() %>%
      filter(Shounen == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Harem"){
    animes_bonus() %>%
      filter(Harem == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Horror"){
    animes_bonus() %>%
      filter(Horror == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Martial Arts"){
    animes_bonus() %>%
      filter(Martial_Arts == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Ecchi"){
    animes_bonus() %>%
      filter(Ecchi == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Kids"){
    animes_bonus() %>%
      filter(Kids == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Slice of Life"){
    animes_bonus() %>%
      filter(Slice_of_Life == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Demons"){
    animes_bonus() %>%
      filter(Demons == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Magic"){
    animes_bonus() %>%
      filter(Magic == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Sports"){
    animes_bonus() %>%
      filter(Sports == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Shoujo"){
    animes_bonus() %>%
      filter(Shoujo == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Psychological"){
    animes_bonus() %>%
      filter(Psychological == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Police"){
    animes_bonus() %>%
      filter(Police == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Thriller"){
    animes_bonus() %>%
      filter(Thriller == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Parody"){
    animes_bonus() %>%
      filter(Parody == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Supernatural"){
    animes_bonus() %>%
      filter(Supernatural == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Seinen"){
    animes_bonus() %>%
      filter(Seinen == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Samurai"){
    animes_bonus() %>%
      filter(Samurai == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Super Power"){
    animes_bonus() %>%
      filter(Super_Power == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Vampire"){
    animes_bonus() %>%
      filter(Vampire == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Josei"){
    animes_bonus() %>%
      filter(Josei == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Hentai"){
    animes_bonus() %>%
      filter(Hentai == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Yaoi"){
    animes_bonus() %>%
      filter(Yaoi == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Space"){
    animes_bonus() %>%
      filter(Space == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Shoujo Ai"){
    animes_bonus() %>%
      filter(Shoujo_Ai == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Shounen Ai"){
    animes_bonus() %>%
      filter(Shounen_Ai == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
  else if(input$topgenre == "Yuri"){
    animes_bonus() %>%
      filter(Yuri == 1) %>%
      arrange(desc(rating)) %>% 
      slice(1:10)
  }
})


output$graph_topgenre <- renderPlot({
  top_anime_genre() %>%
    ggplot() + 
    geom_col(aes(x = name,y = rating, fill = name), position = "dodge") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_linedraw() +
    theme(legend.position = "right",
          legend.title = element_text(size=15),
          legend.text = element_text(size=12)) +
    theme(axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
})



#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#
#                                  TAB 3                                    #
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––#

# Watch list 

#Pour la table movie

observeEvent(input$savemov1, {
  rv$listmov1 <- input$mov1 
})
observeEvent(input$savemov2, {
  rv$listmov2 <- input$mov2
})
observeEvent(input$savemov3, {
  rv$listmov3 <- input$mov3
})
observeEvent(input$savemov4, {
  rv$listmov4 <- input$mov4
})

output$mov_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listmov1,rv$listmov2,rv$listmov3,rv$listmov4)) 
})

#Pour la table music 

observeEvent(input$savemus1, {
  rv$listmus1 <- input$mus1 
})
observeEvent(input$savemus2, {
  rv$listmus2 <- input$mus2
})
observeEvent(input$savemus3, {
  rv$listmus3 <- input$mus3
})
observeEvent(input$savemus4, {
  rv$listmus4 <- input$mus4
})

output$mus_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listmus1,rv$listmus2,rv$listmus3,rv$listmus4)) 
})

#Pour la table ona 

observeEvent(input$saveona1, {
  rv$listona1 <- input$ona1 
})
observeEvent(input$saveona2, {
  rv$listona2 <- input$ona2
})
observeEvent(input$saveona3, {
  rv$listona3 <- input$ona3
})
observeEvent(input$saveona4, {
  rv$listona4 <- input$ona4
})

output$ona_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listona1,rv$listona2,rv$listona3,rv$listona4)) 
})

#Pour la table ova 

observeEvent(input$saveova1, {
  rv$listova1 <- input$ova1 
})
observeEvent(input$saveova2, {
  rv$listova2 <- input$ova2
})
observeEvent(input$saveova3, {
  rv$listova3 <- input$ova3
})
observeEvent(input$saveova4, {
  rv$listova4 <- input$ova4
})

output$ova_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listova1,rv$listova2,rv$listova3,rv$listova4)) 
})

#Pour la table Special

observeEvent(input$savespe1, {
  rv$listspe1 <- input$spe1 
})
observeEvent(input$savespe2, {
  rv$listspe2 <- input$spe2
})
observeEvent(input$savespe3, {
  rv$listspe3 <- input$spe3
})
observeEvent(input$savespe4, {
  rv$listspe4 <- input$spe4
})

output$spe_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listspe1,rv$listspe2,rv$listspe3,rv$listspe4)) 
})

#Pour la table tv

observeEvent(input$savetv1, {
  rv$listtv1 <- input$tv1 
})
observeEvent(input$savetv2, {
  rv$listtv2 <- input$tv2
})
observeEvent(input$savetv3, {
  rv$listtv3 <- input$tv3
})
observeEvent(input$savetv4, {
  rv$listtv4 <- input$tv4
})

output$tv_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listtv1,rv$listtv2,rv$listtv3,rv$listtv4)) 
})

#Pour la table other 

observeEvent(input$saveoth1, {
  rv$listoth1 <- input$oth1 
})
observeEvent(input$saveoth2, {
  rv$listoth2 <- input$oth2
})

output$other_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listoth1,rv$listoth2)) 
})

#Pour la table globale :

observeEvent(input$chargelist, {
  rv$listmovie <- c(rv$listmov1,rv$listmov2,rv$listmov3,rv$listmov4)
  rv$listmusic <- c(rv$listmus1,rv$listmus2,rv$listmus3,rv$listmus4)
  rv$listona <- c(rv$listona1,rv$listona2,rv$listona3,rv$listona4)
  rv$listova <- c(rv$listova1,rv$listova2,rv$listova3,rv$listova4)
  rv$listspecial <- c(rv$listspe1,rv$listspe2,rv$listspe3,rv$listspe4)
  rv$listtv <- c(rv$listtv1,rv$listtv2,rv$listtv3,rv$listtv4)
  rv$listother <- c(rv$listoth1,rv$listoth2)
})

output$table_list <- renderDataTable({
  animes() %>%
    filter(name %in% c(rv$listmovie,rv$listmusic,rv$listona,rv$listova,rv$listspecial,rv$listtv,rv$listother)) 
})


# Random 

observeEvent(input$rand1, {
   rv$rand <- 0.00008134049
})
observeEvent(input$rand5, {
   rv$rand <- 0.00040670245
})
observeEvent(input$rand10, {
   rv$rand <- 0.00081340491
})
 
output$table_rand <- renderDataTable({
  animes() %>% sample_frac(rv$rand)
})

}