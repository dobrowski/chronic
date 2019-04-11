

#  Load libraries needed ------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
        tidyverse
        ,here
        ,janitor
        ,ggthemes
        ,scales
        ,RColorBrewer
        ,ggalt
        ,grid
        ,shiny
)



# Create the species table for select input.
species <- tibble(dist = unique(joiner$DistrictName), code =1:27)


# Set choices of title and code for select input.
choicesSpecies <- setNames(as.numeric( species$code), species$dist)

title <- unique(joiner$DistrictName)
code <- c(1:27)
names(title) <- title

# UI -------

ui <- fluidPage(
        
        # App title ----
        titlePanel("Chronic Absenteeism by District"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # Input: Slider for the number of bins ----
                        selectInput("select",
                                    h3("Select a School District"), 
                                    # choices = list("Choice 1" = 1, "Choice 2" = 2,
                                    #                "Choice 3" = 3), selected = 1))
                                    choices = title),
                        
                        p(span("Red is 2016-17", style = "color:red"),
                          " and",
                          span( "Dark blue is 2017-18",  style = "color:darkblue")
                          )
                ),
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Histogram ----
                        plotOutput(outputId = "distPlot")
                        
                )
        )


)





#  Load raw data ------

chronic1617 <- read.delim("ChrAbsRate1617.txt")
chronic1718 <- read.delim("ChrAbsRate1718.txt")

subgroups <- tribble(~ReportingCategory,~Subgroup,
        "TA" , "Total",
        "RB" , "African American",
        "RI" , "American Indian or \nAlaska Native",
        "RA" , "Asian",
        "RF" , "Filipino",
        "RH" , "Hispanic or Latino",
        "RP" , "Pacific Islander",
        "RW" , "White",
        "RT" , "Two or More Races",
        "RD" , "Did not Report",
        "GM" , "Male",
        "GF" , "Female",
        "SE" , "English Learners",
        "SD" , "Students with Disabilities",
        "SS" , "Socioeconomically \nDisadvantaged",
        "SM" , "Migrant",
        "SF" , "Foster",
        "SH" , "Homeless",
        "GRK" , "Kindergarten",
        "GR13" , "Grades 1–3",
        "GR46" , "Grades 4–6",
        "GR78" , "Grades 7–8",
        "GRK8" , "Grades K–8",
        "GR912" , "Grades 9–12",
        "GRUG" , "Ungraded Elementary \nand Secondary"
        ) %>%
        mutate(Subgroup = fct_inorder(Subgroup))

# Clean and refine the data ----

chr1617 <- chronic1617 %>% 
        filter(County.Name == "Monterey"| Aggregate.Level == "T", Aggregate.Level != "D1") %>%
        select(AggregateLevel = Aggregate.Level ,DistrictCode = District.Code, SchoolCode = School.Code ,DistrictName = District.Name, SchoolName = School.Name, ReportingCategory = Reporting.Category , CumulativeEnrollment1617 = Cumulative.Enrollment , Y1617 = Chronic.Absenteeism.Rate) %>%
        mutate(SchoolCode = if_else(AggregateLevel == "D2", 0L, SchoolCode  ),
               SchoolName = if_else(AggregateLevel == "D2", "District Office", as.character(SchoolName)  ))



chr1718 <- chronic1718 %>% 
        filter(CountyName == "Monterey"| AggregateLevel == "T", AggregateLevel != "D1") %>%
        select(AggregateLevel , DistrictCode , SchoolCode, DistrictName, SchoolName, ReportingCategory , CumulativeEnrollment1718 = CumulativeEnrollment  , Y1718 = ChronicAbsenteeismRate) 


#  Join datasets together ------

joiner <- full_join(chr1617, chr1718 ) %>%
        left_join(subgroups) %>%
        filter(!str_detect(SchoolName, "Nonpublic"),
               ReportingCategory != "GRUG") %>%
        mutate(Y1718 = as.numeric(as.character(Y1718)),
               Y1617 = as.numeric(as.character(Y1617)),
               SchoolName = case_when(AggregateLevel == "C" ~ "Monterey County",
                                      AggregateLevel == "T" ~ "California",
                                      TRUE ~ SchoolName),
               DistrictName = case_when(AggregateLevel == "C" ~ "Monterey County",
                                      AggregateLevel == "T" ~ "California",
                                      TRUE ~ DistrictName),
               SchoolName = fct_inorder(SchoolName),
               nlabel = case_when(AggregateLevel == "D2" ~ paste0(Subgroup, " \n(",  CumulativeEnrollment1718,")"),
                                  TRUE ~ paste0(Subgroup))
        )



#  Graphing ------



# 
# for(i in unique(joiner$DistrictName)){
#         
# joiner.df <- joiner %>%
#                 filter(str_detect(DistrictName, i) | AggregateLevel %in% c("C","T"),
#                        !is.na(Y1718)) %>%
#                 mutate(Subgroup = fct_drop(Subgroup))
# 
# xlabels <- joiner.df %>%
#                 filter(AggregateLevel %in% c("T","D2" ) ) %>%
#                 group_by(Subgroup) %>%
#                 arrange(AggregateLevel) %>%
#                 filter(row_number() == 1) %>%
#                 ungroup() %>%
#                 arrange(Subgroup) %>%
#                 select(nlabel) %>%
#                 mutate(nlabel = fct_inorder(nlabel)) 
#         
# xlabs <- paste(levels(xlabels$nlabel), sep ="")
#         
#         
#                 
#                                 
#   chronic.table <- ggplot(joiner.df, aes(  Subgroup     ,  fct_rev(SchoolName),   fill = Y1718 )) + 
#                 geom_tile(colour = "white") +
#                 geom_text(aes(label= percent( Y1718/100)), size = 3) +
#                 scale_x_discrete(labels = xlabs) +
#                 theme_hc() +
#                 scale_fill_gradient( low = "light yellow", high = "blue" )+
#                 theme(
#                         legend.position = "none",
#                         axis.ticks.x = element_blank(),
#                         strip.background = element_rect(fill = "black"),
#                         strip.text = element_text(colour = 'white'),
#                         axis.text.x = element_text(angle = 45, hjust = 1)
#                 ) +
#                 labs(x="Subgroup",
#                      y="School",
#                      title = paste0(i," Chronic Absenteeism by Subgroup and School in 2017-18"), 
#                      subtitle="", 
#                      fill="")
#  
#  ggsave(here("tables", paste0(i," Chronic Absenteeism by Subgroup and School in 2017-18.png") ), height = 7, width = 14)
# 
#  
# change.graph  <- joiner.df %>% filter(str_detect(SchoolName,"District Office")) %>%
#          ggplot( aes(x=Y1617/100, xend = Y1718/100, y = fct_rev( Subgroup) ) ) +
#          geom_dumbbell(color= "grey" ,       #  "#a3c4dc", 
#                        size=1.5, 
#                        colour_x = "red",
#                        colour_xend = "dark blue"         #"#0e668b"
#          ) +
#          theme_hc() +
#          scale_x_continuous(label= percent) +
#         labs(y="",
#              x="",
#              title = paste0(i," Change \n in Chronic Absenteeism from 2016-17 to 2017-18 \nby Subgroup")
#         ) +
#         geom_text(color="red", size=3, vjust=1.5,
#                   aes(x=Y1617/100, label=Y1617))+
#         geom_text(aes(x=Y1718/100, label=Y1718), 
#                   color="dark blue", size=3, vjust=-0.5)+
#         annotation_custom(grobTree(t1, t2, t3),xmin=0.1,xmax=0.1,ymin=-0.15,ymax=-0.15)
# 
# # create gtable and remove clipping
# g <- ggplot_gtable(ggplot_build(change.graph))
# g$layout$clip[g$layout$name == "panel"] <- "off"
# 
# # re-draw
# 
# png(here("tables", paste0(i," Change in Chronic Absenteeism by Subgroup.png") ),width = 600, height = 750, units = "px" ) 
# grid.draw(g) 
# dev.off()
# 
#  
#  
#          
# } 

# i <- "Monterey County"
# i <- "California"
# #i <- select$value
# 
# change.graph  <- joiner.df %>% filter(SchoolName == i) %>%
#         ggplot( aes(x=Y1617/100, xend = Y1718/100, y = fct_rev( Subgroup) ) ) +
#         geom_dumbbell(color= "grey" ,       #  "#a3c4dc", 
#                       size=1.5, 
#                       colour_x = "red",
#                       colour_xend = "dark blue"         #"#0e668b"
#         ) +
#         theme_hc() +
#         scale_x_continuous(label= percent) +
#         labs(y="",
#              x="",
#              title = paste0(i," Change in Chronic Absenteeism \nfrom 2016-17 to 2017-18 by Subgroup")
#              ) +
#         geom_text(color="red", size=3, vjust=1.5,
#                   aes(x=Y1617/100, label=Y1617))+
#         geom_text(aes(x=Y1718/100, label=Y1718), 
#                   color="dark blue", size=3, vjust=-0.5)+
#         annotation_custom(grobTree(t1, t2, t3),xmin=0.2,xmax=0.3,ymin=-0.3,ymax=-0.3)
# 
# # create gtable and remove clipping
# g <- ggplot_gtable(ggplot_build(change.graph))
# g$layout$clip[g$layout$name == "panel"] <- "off"
# 
# # re-draw
# 
# png(here("tables", paste0(i," Change in Chronic Absenteeism by Subgroup.png") ),width = 600, height = 750, units = "px" ) 
# grid.draw(g) 
# dev.off()
# 






server <- function(input, output) {
        
        output$distPlot <- renderPlot({
                
                i <- input$select
                
                
                joiner.df <- joiner %>%
                        filter(str_detect(DistrictName, i) ,
                               !is.na(Y1718)) %>%
                        mutate(Subgroup = fct_drop(Subgroup)) %>%
                        filter(str_detect(SchoolName,"District Office")) 
                        
                        ggplot(joiner.df, aes(x=Y1617/100, xend = Y1718/100, y = fct_rev( Subgroup) ) ) +
                        geom_dumbbell(color= "grey" ,       #  "#a3c4dc", 
                                      size=1.5, 
                                      colour_x = "red",
                                      colour_xend = "dark blue"         #"#0e668b"
                        ) +
                        theme_hc() +
                        scale_x_continuous(label= percent) +
                        labs(y=input$select,
                             x="",
                             title = paste0(i," Change in Chronic Absenteeism \nfrom 2016-17 to 2017-18 by Subgroup")
                        ) +
                        geom_text(color="red", size=3, vjust=1.5,
                                  aes(x=Y1617/100, label=Y1617))+
                        geom_text(aes(x=Y1718/100, label=Y1718), 
                                  color="dark blue", size=3, vjust=-0.5)                
        })
        
}




shinyApp(ui = ui, server = server)