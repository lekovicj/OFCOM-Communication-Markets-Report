library(tidyverse)
library(extrafont)
library(gganimate)


# set general theme -------------------------------------------------------

highlight <- "#EE5534"
dark <-  "#1D1E1D"
light <-  "#EEE8E8"
font <- "Roboto"

theme_set(theme_minimal()+
              theme(text = element_text(family = font, size = 14, colour = dark),
                    line = element_line(size = 1),
                    #plot.margin = margin(20, 20, 20, 20, "pt"),
                    plot.subtitle = element_text(size = 14, hjust = 1, margin = margin(4,0.1,4,0.1, unit = "mm")),
                    plot.title = element_text(face = "bold", size = 20, hjust = 1, margin = margin(4,0.1,0.5,0.1, unit = "mm")),
                    panel.grid.major.y = element_line(linetype = "dotted", colour = highlight, size = 0.5),
                    panel.grid.major.x = element_line(linetype = "dotted", colour = highlight, size = 0.5),
                    panel.grid.minor = element_blank(),
                    axis.text = element_text(colour = dark, size = 12),
                    plot.background = element_rect(fill = light, color = light)))

update_geom_defaults("text", list(colour = dark, family = font))



# set up data -------------------------------------------------------------

#https://www.ofcom.org.uk/research-and-data/multi-sector-research/cmr/cmr-2018/data-downloads

#download.file(url = "https://www.ofcom.org.uk/__data/assets/file/0024/118473/CMR-2018-Interactive-Report-Data-TV-and-AV.csv", destfile = "data/CMR-2018-Interactive-Report-Data-TV-and-AV.csv")

TV <- read_csv("data/CMR-2018-Interactive-Report-Data-TV-and-AV.csv")

viewing <- TV %>% 
    filter(str_detect(string = Metric, pattern = "mins of daily"), 
           Breakdown == "By age") 

v2013 <- TV %>%     
    filter(str_detect(string = Metric, pattern = "mins of daily"), 
           Breakdown == "By age", 
           Year == 2013) %>% 
    select(Type, Value2013 = Value)


viewing <- viewing %>% 
    left_join(y = v2013, by = "Type") %>% 
    mutate(change = Value-Value2013)%>% 
    mutate(Value = Value, #/60 
           change = change) %>% #/60 to get minutes
    mutate(Type = str_replace(string = Type, pattern = "Adults ",""), 
           Type = str_replace(string = Type, pattern = "Children \\(4-15\\)"," 4-15"),
           Type = str_replace(string = Type, pattern = "individuals ",""))

rm(v2013)



# let's try an animation of each demographic ------------------------------

# commentary --------------------------------------------------------------

Type <- viewing %>%
    filter(Type != "All (4+)") %>% 
    distinct(Type) %>% 
    arrange(desc(Type)) 

notes <- c("Older audiences keep watching\nabout the same amount of TV...", 
           "...but the younger we are...\n", 
           "...the less TV we watch...\n",
           "...the less TV we watch...\n",
           "...and that trend is getting more\npronounced in recent years.",
           "...and that trend is getting more\npronounced in recent years.",
           " \n ")

commentary <- data.frame(Type,notes)


# plot --------------------------------------------------------------------

ani <- viewing %>%  
    filter(Type != "All (4+)") %>% 
    left_join(y = commentary,by = "Type") %>% 

    arrange(desc(Type)) %>% 
    mutate(revealTime = row_number()) %>% 
    ggplot(mapping = aes(x = Year, y = Value, group = Type))+
    geom_hline(mapping = aes(yintercept = 380), size = 1, colour = dark)+
    geom_segment(mapping = aes(x = 2012.9, xend = 2017.1, y = 0, yend = 0), size = 1, colour = dark)+
    
    
    geom_segment(mapping = aes(xend = 2012.6, yend = Value), col = dark, linetype = "dotted")+
    
    geom_path(size = 1, colour = highlight)+
    
    geom_point(size = 3, colour = dark)+
    geom_point(mapping = aes(size = rep(x = c(0,1,0,1,0), length.out = 35)), shape = 1, colour = dark)+
    
    
    #text labels for values
    geom_text(mapping = aes(x = 2012, label = paste0(floor(Value/60),"h ",floor(Value%%60), "m")),
              nudge_y = 0, nudge_x = 0, colour = highlight, vjust = 0.5,
              hjust = 0.5, size = 6, family = font, fontface = "bold", check_overlap = T)+

    
    geom_text(mapping = aes(x = 2017, label = Type), check_overlap = T, 
              size = 6, hjust = 0, nudge_x = 0.2, family = font)+
    
    geom_text(mapping = aes(x = 2015, y = 40, label = notes), size = 6, check_overlap = T, family = font)+
    
    geom_text(data = NULL, mapping = aes(x = 2017, y = 368, label = "ages"), check_overlap = T,
              size = 5, family = font, colour = dark, nudge_x = 0.2, hjust = 0, vjust = 0.6)+
    
    scale_x_continuous(expand = expand_scale(add = c(0.5,1)), breaks = c(2013:2017))+
    scale_y_continuous(limits = c(0,NA), expand = c(0,1))+
    
    labs(title = "Who's got the remote?",
         subtitle = "Average time spent watching TV per day\nUnited Kingdom 2018",
         x = "", y = "",       
         caption = "Source: BARB/OFCOM | The Communications Market Report 2018\nVisualisation by Jovan Leković @synthesisbureau\nPublished on constantlyplotting.com")+
    
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = margin(0.2, 2.3, 0.5, 2.3, "cm"),
          axis.ticks.x = element_line(colour = dark, size = 1),
          axis.text.y = element_blank(),
          plot.subtitle = element_text(face = "italic", colour = highlight),
          legend.position = "none")+ 
    
    transition_reveal(along = revealTime, keep_last = T)+
    ease_aes("sine-out")+
    enter_grow()+
    exit_shrink()

#ani

#preview
animate(plot = ani, fps = 3, duration = 25, #detail = 8,
        start_pause = 10, #end_pause = 50, 
        width = 550, height = 550,
        renderer = gifski_renderer(loop = F)) 

#production
aniGif <- animate(plot = ani, fps = 20, duration = 23, detail = 20, 
               width = 550, height = 550, 
               start_pause = 10,# end_pause = 80,
               type = "cairo", renderer = gifski_renderer(loop = F)) 

anim_save(animation = aniGif, filename = "TV Viewing.gif", path = "animation/")




aniVid <- ani #+ theme(plot.margin = margin( r = 114.6*3,l = 114.6*3, unit = "pt") )

aniVid <- animate(plot = aniVid, fps = 20, duration = 23, detail = 20, 
               width = 550, height = 550, 
               start_pause = 10, end_pause = 80,
               type = "cairo", 
               renderer = gganimate::ffmpeg_renderer()) 

anim_save(animation = aniVid, filename = "TV Viewing.mp4", path = "animation/")


