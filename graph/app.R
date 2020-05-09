#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(gifski)
library(gganimate)
library(png)
library(broom)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gt)

#  Imported all necessary rds files. 

e2007 <- read_rds("e2007.rds")
e2008 <- read_rds("e2008.rds")
e2009 <- read_rds("e2009.rds")
e2010 <- read_rds("e2010.rds")
e2011 <- read_rds("e2011.rds")
e2012 <- read_rds("e2012.rds")
e2013 <- read_rds("e2013.rds")
e2014 <- read_rds("e2014.rds")
e2015 <- read_rds("e2015.rds")
e2016 <- read_rds("e2016.rds")
e2017 <- read_rds("e2017.rds")
e2018 <- read_rds("e2018.rds")
e2019 <- read_rds("e2019.rds")
aid <- read_rds("aid.rds")
school2 <- read_rds("school2.rds")
locs <- read_rds("locs.rds")
enrollment <- read_rds("enrollment.rds")
budget <- read_rds("budget.rds")
dropout <- read_rds("dropout.rds")


ui <- fluidPage(theme = shinytheme("flatly"),
    
    # Created navigation bar. There  are 4 tabs overall with more subtabs beneath
    
    navbarPage("Chicago Public Schools (CPS)",
               
               # Essentially the About section as well as some background information on topic.
               
               tabPanel("Introduction",
                        # plotOutput("image2"),
                        h2("Overview"),
                        p("The Chicago Public Schools (CPS) district has been lacerated with budget
                            cuts, perpetuating a cycle of schools being forced to close, students being
                            displaced, and the district suffering. Put simply, the primary vehicle through
                            which funding is distributed is thought Student Based Budgeting (SBB), wherein
                            schools receive funding based on their enrollment. A small minority of schools,
                            named selective enrollment schools, draw most students' attention, causing
                            bustling waitlists, boatloads of resources, and stellar performances. But this
                            siphons enrollment from other schools in the district, which are often well
                            below capacity, meaning less funding is received and therefore programs at
                            those schools have to be cut. This makes schools even less appealing, and then
                            even less students enroll, perpetuating a vicious cycle until school closure.
                            This is disastrous for CPS students, especially those within the South side
                            of Chicago where conditions are very dangerous; a school closing means students
                            have to commute farther, and each longer second within such an unstable
                            environment is severely dangerous. This risk alone potentially deters many 
                            students from school, despite the ostensibly 'helpful' school openings in 
                            their place."),
                        p("This is merely an oversimplified window into the obfuscated system that is the
                            Chicago Public Schools district. I hope to use this
                            project as a platform to explore the interplay of school closures, the budget
                            crises faced by CPS, and student enrollment."),
                        
                        # Created a map showcasing CPS school locations
                        
                        h2("Map"),
                        p("Being the 3rd largest school district in the nation, CPS comprises many elementary and high schools throughout
                          the Chicagoland area. Below is a map I've created using census data and geographic locations obstained
                          from the CPS website portraying all of CPS' schools to get an idea of its size:"),
                        plotOutput("map"),
                        
                        # Listed sources for data out
                        
                        h2("Sources"),
                        p("Data was obtained from numerous sources, chiefly CPS' own website  at cps.edu. Through this, I was 
                          able to retrieve geographic locations for schools for the map, access the financial records for many years, and retrieve
                          the dropout rates as well."),
                        p("I also used additional sources to gather data about school closings throughout the years, which are listed here:"),
                        p("https://interactive.wbez.org/generation-school-closings/"),
                        p("https://docs.google.com/spreadsheets/u/1/d/e/2PACX-1vRmKox-lDNqhtUNL4WLl8x6DljIi3b0k1pYEmD7adfCwX-rGYyFw0XIjxNWkOfL6og3CHfUEHPMwv6k/pubhtml?urp=gmail_link#"),
                        
                        # Information about myself! Most exciting part of the project. 
                        
                        h2("About Me"),
                        p("My name is Ibraheem Khan, and I'm a sophomore studying Applied Mathematics and Economics. My email address is ikhan@college.harvard.edu, and
                          my github can be found here: https://github.com/ibskhan4")
                ),
               
               # Tab regarding enrollment data
               
               tabPanel("Enrollment",
                        
                        tabsetPanel(
                          
                          # This is the interactive component wherein people can select a school and view enrollment history
                          
                            tabPanel("Enrollment Data",
                                     h2("School Enrollment Data", align = "center"),
                                     br(),
                                     sidebarLayout(
                                       
                                       # This section has some background information and the selection mechanism
                                       
                                         sidebarPanel(
                                             h4("CPS School Enrollments"),
                                             br(),
                                             p("Student-Based Budgeting has historically been the method through which CPS has been allocated funding, 
                                                with the attendance and enrollment data as of the 20th day of every academic year being used."),
                                             p("Therefore, student enrollment is an unequivocally crucial piece of information. Use the interactive tool to view the 
                                                enrollment history for schools across CPS! For ease of use, the drop-down menu lists School ID, since school names varied 
                                                across CPS' spreadsheets."),
                                             br(),
                                             selectInput("Select a School ID to view its enrollment history.", "Choose an ID:",
                                                         inputId = "school",
                                                         choices = unique(enrollment$`School ID`))),
                                         
                                         # This section had the actual graph. 
                                         
                                         mainPanel(
                                             fluidRow(
                                                 align = "center",
                                                 h4(textOutput("name"))),
                                             plotOutput("plot1")))),
                            
                            # This tab looks at the overall enrollment of CPS over time. Quite a drastic drop!
                            
                            tabPanel("CPS Enrollment",
                                     br(),
                                     h2("CPS Total Enrollment Data from 2007-2019", align = "center"),
                                     plotOutput("plot2"),
                                     br(),
                                     h2("Commentary"),
                                     h4("As can be seen from above, there has been a substantial dropoff in enrollment from 2007 to 2019, an amount
                                   in the range of nearly 52,380 students.")))),
               
               # This tab explored the issue of budget deficits
               
               
               
               
               
               tabPanel("Dropout Data",
                        
                        tabsetPanel(
                          
                          # This is the interactive component wherein people can select a school and view enrollment history
                          
                          tabPanel("Total Dropout Numbers",
                                   h2("Group Enrollment Data", align = "center"),
                                   br(),
                                   sidebarLayout(
                                     
                                     # This section has some background information and the selection mechanism
                                     
                                     sidebarPanel(
                                       h4("CPS Group Enrollment Data"),
                                       br(),
                                       p("Student-Based Budgeting has historically been the method through which CPS has been allocated funding, 
                                                with the attendance and enrollment data as of the 20th day of every academic year being used."),
                                       p("Therefore, student enrollment is an unequivocally crucial piece of information. Use the interactive tool to view the 
                                                enrollment history for schools across CPS! For ease of use, the drop-down menu lists School ID, since school names varied 
                                                across CPS' spreadsheets."),
                                       br(),
                                       selectInput("Select a School ID to view its enrollment history.", "Choose an ID:",
                                                   inputId = "group1",
                                                   choices = unique(dropout$group))),
                                     
                                     # This section had the actual graph. 
                                     
                                     mainPanel(
                                       h4("Total Dropout Numbers for Various Groups", align = "center"),
                                       plotOutput("group")))
                                   ),
                          
                          # This tab looks at the overall enrollment of CPS over time. Quite a drastic drop!
                          
                          tabPanel("Dropout Correlations",
                                   h2("Dropout Correlations", align = "center"),
                                   br(),
                                   sidebarLayout(
                                     
                                     # This section has some background information and the selection mechanism
                                     
                                     sidebarPanel(
                                       h4("CPS Dropout and Total Govt. Aid "),
                                       br(),
                                       p("Here, we are finding the correlation between state aid and total dropout statistics, all of which was retrieved from the
                                         CPS website. The reasoning behind my focus on state aid is because in past years, CPS would receive state aid at abysmal rates; in 2008, Illinois
                                         was ranked 49th amongst the 50 states in regards to state-contributed to funding of its schools. As such, in a district trodden by a paucity of state-distributed
                                         funding, I wish to explore the relationship between these two key metrics."),
                                       p("We stumble upon quite interesting conclusions here. The correlation coefficient between the citywide dropout rate and
                                         total state aid is approximately -0.424. This indicates a relatively strong inverse proportionality, meaning lower total state aid
                                         results in higher total dropouts."),
                                       br(),
                                       selectInput("Select a group to view its correlation with total governmental aid.", "Choose a group:",
                                                   inputId = "group2",
                                                   choices = unique(dropout$group))),
                                     
                                     # This section had the actual graph. 
                                     
                                     mainPanel(
                                       plotOutput("races"),
                                       br(),
                                       gt_output("corrTable"))),
                                   h2("Discussion"),
                                   p("The statistics delineating the citywide correlations already portray a strong inverse proportionality. However, we can dive further into the data,
                                     and see that some groups' dropout numbers had intriguing correlations. One interesting anomaly would be the Native American group, which seems like
                                     a strong positive correlation (i.e. higher state aid results in more dropouts). However, if one looks at the dropout numbers, the dropout numbers are 
                                     tremendously small relative to the other groups. Also, it's interesing to note that Black students have the strongest negative correlation, indicating that
                                     their dropout numbers increase most with less state aid. It's also important to contextualize this; many school closings and funding deficiencies are present
                                     in the South side of Chicago, wherein over 93% of inhabitants are African-Americans.")
                                   )
                          
                          )),
               
               # This tab explored the issue of budget deficits
               
               
               
               
               
               
               tabPanel("Budget Deficit",
                        tabsetPanel(
                          
                          # Here we explore the budget deficit in regards to governmental aid and CPS' expenses.
                          
                            tabPanel("Deficit Data",
                                     h1("Where's the money?", align = "center"),
                                     
                                     # I used splitrow to showcase both graphs side-by-side so that the gap graph could be seen as well. 
                                     # I didn't want to put it all in one so as to clutter it.
                                     
                                     fluidRow(
                                         align = "center",
                                         splitLayout(cellWidths = c("50%", "50%"), plotOutput("data2", height = "80%"), plotOutput("data3", height = "80%"))),
                                     
                                     br(),
                                     
                                     # A small debrief of the graphs and some other information
                                     
                                     h2("Discussion"),
                                     
                                     h4("To begin our foray into Chicago Public Schools, it's important to discuss the budget issues. In regards to governmental aid, 
                              CPS has been running at a substantial deficit for years, which the above graphs display. As the third largest school district in America, this
                              is problematic given the prodigious student body population. Now obviously, budget issues and deficits especially within such a large district are intricate and complicated,
                              and to purport that one can encapsulate them within a few sentences is grossly misguided. However, some issues do include insufficient funds being allocated to CPS
                              by the state, an action which drew great discontent towards former Governor Bruce Rauner from CPS faculty and administrators, as well as the CTU (Chicago Teacher's Union).")),
                            
                            # Here we perform our first foray into statistical analysis with enrollment and funding deficits
                            
                            tabPanel("Correlation with Enrollment",
                                     
                                     h2("Correlation Between Funding Deficits and School Enrollment"),
                                     
                                     # Use another sidebar layout to explicate findings and also showcase graph
                                     
                                     sidebarLayout(
                                         
                                       # Perform analysis here. There was a strong negative correlation.
                                       
                                       sidebarPanel(
                                             h4("Analysis"),
                                             br(),
                                             p("To the right is the correlation between total enrollment and the funding deficits from 2007 - 2018, where 
                                               the data coincided. In comparison to school closings, the relationship here is much stronger; specifically, we are
                                               dealing with an inverse proportionality, where a larger funding gap is correlated with a smaller enrollment. Obviously, it would 
                                               be wholly ignorant to assume there aren't other confousing or interacting variables, which there are, but nevertheless
                                               a relationship is clear."),
                                             p("Having calculated the correlation coefficient, I found it to be about -0.821, which is indicative of a strong negative
                                               correlation. "),
                                             br(),
                                             p("The regression equation is y = -13.379x + 444263.53")),
                                         
                                       # Plot the graph here. 
                                       
                                         mainPanel(
                                             plotOutput("corrgaps")))))),
               
               # Now we look at school closings. The problem here was the paucity of information in that there are many confounding variables
               # I don't quite know how to account for yet.
               
               tabPanel("School Closings",
                        
                        tabsetPanel(
                          
                          # Simple graph of school closings over time
                          
                            tabPanel("School Closings Data",
                                     plotOutput("plot3"),
                                     
                             # Commentary
                                     
                            h2("Commentary"),
                            h4("Here is a graph over time of school closings in CPS. It's interesting to note that despite
                               the monumental school closures in 2013, there weren't noticeable changes to many metrics, at least
                               moreso than the usual. This leads to an indication of confounding variables, such as schools opened. 
                               As I finalize my project, it may be worthwhile to dive into those variables as well and perform
                               a more informed, holistic analysis.")),
                            
                            # Now we dive again into statistical analysis
                            
                            tabPanel("Correlation with Enrollment",
                                     h2("Correlation Between School Closings and Total Enrollment"),
                                     sidebarLayout(
                                       
                                       # There was a weak POSITIVE correlation, which shows that the data isn't the best or most reliable.
                                       
                                         sidebarPanel(
                                             h4("Analysis"),
                                             br(),
                                             p("To the right is the correlation between total enrollment and school closings from 2007 - 2018, where 
                                               the data coincided. Admittedly, this is counter to my hypothesis, but the paucity of the data and the
                                               voluminous potential for confounding variables muddies the relationship (e.g. school openings.)"),
                                             p("I also calculated the correlation coefficient to be about 0.139, which is a weak positive relationship"),
                                             br(),
                                             p("The regression equation is y = 115.61x + 398240.55")
                                         ),
                                         
                                         mainPanel(
                                             plotOutput("corrclosings")
                                         )))))))



# Server 

server <- function(input, output) {
    
  # This is the interactive plot showing enrollment for a school over time
  
    output$plot1 <- renderPlot({
      
        enrollment %>% 
            filter(`School ID` == input$school) %>%
            ggplot(aes(x = Year, y = Total)) +
            geom_line() +
            scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
            ylab("Total Enrollment as of 20th School Day")
    })
    
    output$group <- renderPlot({
      
      dropout %>% 
        filter(group == input$group1) %>%
        ggplot(aes(x = year, y = total_dropout)) +
        geom_line() +
        scale_x_continuous(breaks = seq(1999, 2014, by = 1)) +
        ylab("Total Dropout Numbers") +
        xlab("Year")
    })
    
    # This graph shows total enrollment over time
    
    output$plot2 <- renderPlot({
        enrollment %>% 
            filter(!is.na(Total)) %>% 
            group_by(Year) %>% 
            summarize(sum = sum(Total)) %>% 
            ggplot(aes(Year, sum)) +
            geom_point(color = "navyblue") +
            scale_x_continuous(breaks = seq(2007, 2019, by = 1)) +
            scale_y_continuous(breaks = seq(360000, 415000, by = 9000)) +
            theme_bw() +
            ylab("Total Enrollment (As of 20th Academic Day")
    })
    
    # This graph shows the number of school closings over time
    
    output$plot3 <- renderPlot({
        school2 %>% 
            group_by(year) %>% 
            filter(type == "school closing") %>% 
            count(type) %>% 
            ggplot(aes(year, n)) +
            geom_line() +
            labs(x = "Year", 
                 y = "Number of School Closings", 
                 title = "Number of School Closings in CPS from 2002-2018") +
            scale_x_continuous(breaks = seq(2002, 2018, by = 1)) +
            scale_y_continuous(breaks = seq(0, 60, by = 5)) +
            theme_light() +
            theme(plot.title = element_text(face = "bold", color = "#063376", size = 20))
    })
    
    # This shows the correlation between school closings and CPS student enrollment
    
    output$corrclosings <- renderPlot({
        school_join <- school2 %>% 
            group_by(year) %>% 
            filter(type == "school closing") %>% 
            filter(year > 2006 && year < 2019) %>% 
            count(type) 
        
        ejoin <- enrollment %>% 
            filter(!is.na(Total)) %>% 
            group_by(Year) %>% 
            summarize(sum = sum(Total)) %>% 
            rename("year" = Year)
        
        school_e_join <- ejoin %>%
            inner_join(school_join, by = "year") 
        
        school_e_join %>%
            ggplot(aes(n, sum, color = year)) +
            geom_point(show.legend = FALSE, alpha = 0.7) +
            scale_x_log10() +
            scale_y_log10() +
            geom_smooth(method = "lm", se = TRUE) +
            labs(
                x = "School Closures",
                y = "Total CPS Enrollment", 
                title = "Relationship between School Closures and Total CPS Enrollment") +
            theme(legend.position = "none")
    })
    
    # This shows correlation between deficit gaps and CPS enrollment.
    
    output$corrgaps <- renderPlot({
        ejoin <- enrollment %>% 
          filter(!is.na(Total)) %>% 
          group_by(Year) %>% 
          summarize(sum = sum(Total)) %>% 
          rename("year" = Year)
        
        aid2 <- aid %>% 
            filter(year < 2019) %>% 
            inner_join(ejoin, by = "year")
        
        aid2 %>% 
            ggplot(aes(gap, sum, color = year)) +
            geom_point(show.legend = FALSE, alpha = 0.7) +
            scale_x_log10() +
            scale_y_log10() +
            geom_smooth(method = "lm", se = TRUE) +
            labs(
                x = "Funding Deficit (in millions of dollars)",
                y = "Total CPS Enrollment", 
                title = "Relationship between Funding Gaps and Total CPS Enrollment",
                subtitle = "Funding gaps represent difference between total expenses and federal and state aid"
            ) +
            theme(legend.position = "none")
        
        # aid2 %>%
        #     lm(sum ~ gap, data = .) %>%
        #     tidy(conf.int = TRUE)
    })
    
    # This is the map of Cook County with the geographic locations of schools overlayed. Census data was very helpful.
    
    output$map <- renderPlot({
        library(tidycensus)
        census_api_key('9f9584127dd506cdaf80bdb78927e9c01c12f2b8', overwrite = TRUE)
        
        locs3 <- locs %>% 
            select(X, Y)
        
        locx <- locs3 %>% 
            pull(X)
        
        locy <- locs3 %>% 
            pull(Y)
        
        racevars <- c(White = "B02001_002", 
                      Black = "B02001_003", 
                      Asian = "B02001_005",
                      Hispanic = "B03003_003")
        cook <- get_acs(geography = "tract",
                        variables = racevars, 
                        year = 2018,
                        state = "IL",
                        county = "Cook",
                        geometry = TRUE,
                        summary_var = "B02001_001") 
        
        sites <- data.frame(longitude = locx, latitude = locy)
        
        
        cook %>%
            mutate(Percent = 100 * (estimate / summary_est)) %>%
            ggplot() +
            geom_sf() +
            geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
                       shape = 23, fill = "darkred") +
            scale_fill_viridis_c(direction = -1) +
            scale_color_viridis_c(direction = -1) +
            labs(title = "All CPS School Locations in Cook County, IL",
                 caption = "Source: American Community Survey 2014-2018") 
    })
    
    # Used to list the name of the school the user picks
    
    output$name <- renderText({
        schoolname <- enrollment %>% 
            filter(`School ID` == input$school) %>% 
            arrange(desc(Year)) %>% 
            slice(1) %>% 
            pull(School)
        
        paste("School Name:", schoolname)
    })
    
    # Animated graph showing federal and state aid, as well as expenses for CPS over time
    
    output$data2 <- renderImage({
        outfile <- tempfile(fileext='.gif')
        
        colors <- c("State Aid" = "darkred",
                    "Federal Aid" = "darkred", 
                    "Total Govt. Aid  (Federal + State)" = "steelblue",
                    "Total Expenses" = "blue")
        
        p <- aid %>% 
            mutate(total_aid = federal_aid + state_aid) %>% 
            ggplot(aes(year)) +
            geom_line(aes(y = state_aid, color = "State Aid"), linetype = "twodash") +
            geom_line(aes(y = federal_aid, color = "Federal Aid"), linetype = "twodash") +
            geom_line(aes(y = total_aid, color = "Total Govt. Aid  (Federal + State)"), linetype = "twodash") +
            geom_line(aes(y = total_expenses, color = "Total Expenses")) +
            labs(x = "Year", 
                 y = "Amount (in millions of dollars)", 
                 color = "Legend",
                 title = "State Aid, Federal Aid, and Total Expenses for CPS from 2007-2019") +
            theme(
                legend.position = c(.95, .70),
                legend.justification = c("right", "top"),
                legend.box.just = "right",
                legend.margin = margin(6, 6, 6, 6)) +
            scale_color_manual(values = colors) +
            transition_reveal(year)
        
        anim_save("outfile.gif", animate(p)) # New
        
        list(src = "outfile.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)
    
    # Another animated plot showing the gap between governmental aid and expenses over time.
    
    output$data3 <- renderImage({
        outfile2 <- tempfile(fileext='.gif')
        
        s <- aid %>% 
            ggplot(aes(year, gap)) +
            geom_line() +
            labs(x = "Year", 
                 y = "Amount (in millions of dollars)",
                 title = "Funding Gap Between Total Aid and Expenses for CPS from 2007-2019") +
            transition_reveal(year)
        
        anim_save("outfile2.gif", animate(s)) # New
        
        list(src = "outfile2.gif",
             contentType = 'image/gif'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = TRUE)
            
    # This is a plot image I was using before I decided to make the plot myself.
    
    output$image <- renderImage({
        # Return a list containing the filename
        list(src = "graph.png",
             width = 800,
             height = 800,
             contentType = 'image/png',
             alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    # This is a CPS logo that I tried including in my Introduction, but for some reason it 
    # left a huge gap afterwards. Will try to fix before final submission. Kept it out for now.
    
    output$image2 <- renderImage({
        # Return a list containing the filename
        list(src = "download.png",
             contentType = 'image/png',
             alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    
    output$races <- renderPlot({
      
      aid3 <- aid %>% 
        filter(year < 2015) 
      
      drop_join <- dropout %>% 
        filter(year > 2006) %>% 
        inner_join(aid3) %>% 
        filter(group == input$group2)
      
      drop_join %>% 
        ggplot(aes(x = total_dropout, y = state_aid, color = year)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        scale_x_log10() +
        scale_y_log10() +
        labs(x = "Total Dropout",
             y = "Total State Aid",
             color = "Year",
             title = "Correlation Between State Aid and Total Dropout Numbers",
             subtitle = "Group data procured from CPS reports") 
      
    })
    
    output$corrTable <- render_gt({
      
      drop_join %>% 
        group_by(group) %>% 
        summarize(Correlation = cor(state_aid, total_dropout)) %>% 
        rename(Group = group) %>% 
        select(Group, Correlation) %>% 
        gt() %>% 
        tab_header(
          title = "Correlation Coefficients of State Aid & Student Groups",
          subtitle = "Data spans 2007-2014") %>% 
        tab_spanner(
          label = "Coefficients",
          columns = c("Group", "Correlation"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

