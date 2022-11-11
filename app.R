library(readxl)

library(ggplot2)

library(shiny)

library(shinydashboard)

library(dplyr)

library(tidyr)

library(devtools)

library(ggrepel)

library(scales)

library(plotly)

 

#extract data from source

raw_machine_data = read.csv("HisshoDataBase.csv")

 

#extract data from source

MitigationPlan_data = read.csv("MitigationPlan.csv")

 

#rename cols

colnames(MitigationPlan_data) <- c("Index", "Risk", "Feature", "CMP", "TD", "Category", "Description", "M/E", "Vendor", "Ship Dates", "Why Late", "Status", "Mitigation Plans")

 

#select applicable cols and rows

engineer_machine_data = raw_machine_data[ ,c(3:4, 6:7, 9:9, 11:11, 13:15, 33:33, 40:41, 46:46, 48:48, 52:52, 61:63)] %>%

    subset(QTY != 0 & (Procure.At == "GEM" | Procure.At == "Local" | Procure.At == "Restrict"))

 

#rename cols

colnames(engineer_machine_data) <- c("Feature#", "FeatureName", "CMP", "Description", "PartNo", "M/E", "TD", "MechDesign", "ElecDesign", "Vendor",

                                     "Procurement", "RFQStatus", "POStatus", "ShipDate", "UnitStatus", "Category", "QTY","Risk")

 

gathered_machine_data <- gather(engineer_machine_data, key = "ProcurementCategory", value = "Status", RFQStatus, POStatus)

 

#kits allocation by unit status

UnitStatus_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$UnitStatus),

                                     FUN = sum

)

 

#rename cols

colnames(UnitStatus_machine_data) <- c("UnitStatus", "QTY")

 

#kits allocation by RFQ status

RFQStatus_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$RFQStatus,engineer_machine_data$POStatus),

                                     FUN = sum

)

 

#rename cols

colnames(RFQStatus_machine_data) <- c("RFQStatus", "POStatus", "QTY")

 

#kits allocation by PO status

POStatus_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$POStatus),

                                     FUN = sum

)

 

#rename cols

colnames(POStatus_machine_data) <- c("POStatus", "QTY")

 

#kits allocation by Ship Dates

ShipDate_raw_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$ShipDate),

                                     FUN = sum

)

 

#rename cols

colnames(ShipDate_raw_machine_data) <- c("ShipDate", "QTY")

 

#kits allocation by Ship dates, Category and TD

ShipDate_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$TD, engineer_machine_data$ShipDate, engineer_machine_data$Category),

                                     FUN = sum

)

 

#rename cols

colnames(ShipDate_machine_data) <- c("TD", "ShipDate", "Category", "QTY")

 

#kits allocation by Mech Design

MechDesign_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$MechDesign),

                                     FUN = sum

)

 

#rename cols

colnames(MechDesign_machine_data) <- c("MechDesign", "QTY")

 

#kits allocation by Elec Design

ElecDesign_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                     by = list(engineer_machine_data$ElecDesign),

                                     FUN = sum

)

 

#rename cols

colnames(ElecDesign_machine_data) <- c("ElecDesign", "QTY")

#kits allocation by RISK

Risk_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                                   by = list(engineer_machine_data$Risk),

                                   FUN = sum

)

 

#rename cols

colnames(Risk_machine_data) <- c("Risk", "QTY")

 

 

#kits allocation by vendors

vendor_machine_data <- aggregate(as.integer(engineer_machine_data$QTY),

                by = list(engineer_machine_data$Vendor),

                    FUN = sum

)

 

# Define UI for application that draws a histogram

ui <- dashboardPage(

        dashboardHeader(title = "Project Hissho Dashboard", titleWidth = 300),

        skin = "black",

        dashboardSidebar(

           

            width = 165,

            sidebarMenu(

               

                menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),

                menuItem("Equipment Tracker", tabName = "EquipmentTracker", icon = icon("dashboard")),

                menuItem("Mitigation Plan", tabName = "MitigationPlan", icon = icon("dashboard"))

            )

        ),

       

        dashboardBody(

           

            tabItems(

               

                #Tab Overview

                tabItem(

                 

                    tabName = "Overview",

                   

                    #Project Description

                    fluidPage(

                       

                        h4(strong("Project Description")),

                       

                        em("D8.8 will deliver further benefits on 'sarasara' breathability concept and will focus on addressing the different needs of consumers across multiple sizes.

Overall, D8.8 all sizes (S2-S6) will apply aperture sponbond TS and ATB AQL to improve interior softness & dryness, Wider Path to improve pad shape, pad width and absorbency perception improvement.

                                  On top of that, D8.8 Small sizes S2 and S3 include include a BM Waist Guard (BMWG) feature to further improve BM handling."),

                       

                        h6(strong("GEM ID: 2617")),

                       

                        h6(strong("IL ROS Date: 2020-09-25")),

                       

                        h4(strong("Where are we"))

                      

                    ),

                   

                    fluidPage(

                        box(

                           

                            column(

                                width = 6,

                                plotOutput(outputId = "UnitStatus_piechart")

                            ),

                           

                            column(

                                width = 6,

                                plotOutput(outputId = "RFQPOStatus_barchart")

                            ),

                       

                            column(

                                width = 6,

                                plotOutput(outputId = "ShipDate_barchart")

                            ),

                           

                            # column(

                            #     width = 6,

                            #     plotOutput(outputId = "Size_piechart")

                            # ),

                            

                            column(

                                width = 6,

                                plotOutput(outputId = "Risk_piechart")

                            ),                       

                            width = "100%",

                            height = "900px"

                        )

                    ),

                   

 

                   

                    fluidPage(

                       

                        h6(strong("Powered by Shangxi Engneering"))

                       

                    )

                ),

               

                #Tab Breakdown by size

                tabItem(

                   

                    tabName = "EquipmentTracker",

                   

                    fluidPage(

                       

                        h4(strong("D8.8 IL Equipment Tracker"))

                       

                    ),

                   

                    fluidRow(

                        column(width = 12,
                              

                               box(width = 3, status = "success",

                                   checkboxGroupInput("chosen_date", "Ship Date",

                                                      choices = unique(ShipDate_machine_data$ShipDate),

                                                      selected = unique(ShipDate_machine_data$ShipDate)

                                   ),
                                  

                               ),

                               

                               box(width = 3, status = "success",

                                   checkboxGroupInput("chosen_TD", "TD",

                                                      choices = unique(engineer_machine_data$TD),

                                                      selected = unique(engineer_machine_data$TD)

                                   )

                               ),

                              

                               box(width = 3, status = "success",

                                   checkboxGroupInput("chosen_size", "Category",

                                                      choices = unique(ShipDate_machine_data$Category),

                                                      selected = unique(ShipDate_machine_data$Category)

                                   )

                               ),

                              

                              

                               

                               box(width = 3, status = "success",

                                   checkboxGroupInput("chosen_UnitStatus", "Unit Status",

                                                      choices = unique(engineer_machine_data$UnitStatus),

                                                      selected = unique(engineer_machine_data$UnitStatus)

                                   )

                               )                             

                        )

                        

                    ),
                   
                    fluidPage(

                       

                        h5(strong("*Equipment Delivery Chart"))

                       

                    ),

                   

                    fluidPage(

                       

                        box(plotOutput("kits_allocation_ShipDate"),

                            width = "100%",

                            height = "400px"

                        )

                    ),

                   

                    #selectInput("SelectDate", "Select an Onsite Date", choices = engineer_machine_data$OnsiteDate),

                    fluidPage(

                       

                        h5(strong("*Dynamic Equipment Tracker"))

                        

                    ),

                   

                    fluidRow(

                       

                        mainPanel(

                           

                            tableOutput("EquipmentTracker_Date")

                        )

                       

                    ),

                   

                    fluidPage(

                       

                        h6(strong("Powered by Shangxi Engneering"))

                       

                    )

                ),

               

                tabItem(

                   

                    tabName = "MitigationPlan",

                   

                    fluidPage(

                       

                        h4(strong("D8.8 IL Equipment Mitigation Plan"))

                       

                    ),

                   

                    fluidRow(                        

                        column(width = 12,

                              

                               box(width = 3, status = "success",

                                   checkboxGroupInput("chosen_Risk", "Risk Level",

                                                      choices = unique(MitigationPlan_data$Risk),

                                                      selected = unique(MitigationPlan_data$Risk)

                                   )

                               )

                              

                        )

                       

                    ),

                   

                    fluidRow(

                       

                        mainPanel(

                           

                            tableOutput("MitigationPlan_Tracker")

                        )

                       

                    ),

                   

                    fluidPage(

                       

                        h6(strong("Powered by Shangxi Engneering"))

                       

                    )

                   

                )

            )

        )

)

 

# Define server logic required to draw a histogram

shinyServer <- function(input, output) {
   

    #Onsite Date bar chart

    output$ShipDate_barchart <- renderPlot(
         

            ggplot(

               

                ShipDate_raw_machine_data,

               

                aes(

 

                            x = ShipDate,

                            y = QTY

                        )

               

            )

           

            + geom_bar(stat = "identity", width = 0.3, fill = "#FF6666")

            + geom_label(aes(label = QTY), color = "Black")

            + labs(x = "Ship Date", y = "QTY", title = "Overall Delivery Status")

            + theme_classic()

           

    )

   

    #Overview Graph

    output$kits_allocation_ShipDate <- renderPlot(

       

        ggplot(

           

            ShipDate_machine_data %>%

                filter(ShipDate %in% input$chosen_date,

                       TD %in% input$chosen_TD,

                       Category %in% input$chosen_size),

           

            aes(

               

                x = ShipDate,

                y = QTY,

                shape = Category,

                col = Category

            )

        )

       

        #+ geom_bar(stat = "identity", width = 0.2, fill = "#FF6666")

        + geom_point(size = 8)

        #+ legend(col = 1:4)

        # + scale_colour_manual(

        #     values = c(

        #         'Base Kit' = '#218E8DFF',

        #         'Size 3' = '#55C568FF',

        #         'Other Sizes' = '#218E8DFF',

        #         'Major Spare' = '#218E8DFF'

        #        

        #        

        #     )

        # )

        + labs(x = "Ship Date", y = "QTY")

        + theme_grey()

        + geom_label_repel(aes(label = QTY))

    )

   

    #Overview Table

    output$EquipmentTracker_Date <- renderTable(

        #DateFilter <- subset(select(engineer_machine_data, -QTY),engineer_machine_data$OnsiteDate == input$SelectDate)

        # DateFilter <- subset(select(engineer_machine_data, -QTY),engineer_machine_data$OnsiteDate == input$chosen_date)

        engineer_machine_data %>%

            select(-QTY, -FeatureName, -MechDesign, -ElecDesign, -RFQStatus, -POStatus, -Risk) %>%

            filter(ShipDate %in% input$chosen_date,

                   TD %in% input$chosen_TD,

                   Category %in% input$chosen_size,

                   UnitStatus %in% input$chosen_UnitStatus

                   )

    )

   

    #Breakdown by size graph

    output$kits_allocation_category <- renderPlot(

       

        ggplot(

           

            category_machine_data %>%

                arrange(desc(Category)) %>%

                mutate(size_sum = sum(QTY)) %>%

                mutate(lab.position = cumsum(QTY) - 0.5*QTY) %>%

                mutate(percent_size = QTY/size_sum),

           

            aes(

                

                x = "",

                y = QTY,

                fill = Category

            )

        )

       

        + geom_bar(width = 1, stat = "identity", color = "white")

        + coord_polar("y", start=0)

        + geom_text(aes(y= lab.position, label = percent(percent_size)), color = "white")

        # + scale_fill_manual(values = QTY)

        + theme_void()

        # #+ geom_point(size = 8)

        + labs(title = "Kits Distribution by Category")

        # + theme_bw()

        # + geom_label_repel(aes(label = QTY))

    )

   

    #Unit Status pie chart

    output$UnitStatus_piechart <- renderPlot(

       

        ggplot(

           

            UnitStatus_machine_data %>%

                arrange(desc(UnitStatus)) %>%

                mutate(UnitStatus_sum = sum(QTY)) %>%

                mutate(lab.position = cumsum(QTY) - 0.5*QTY) %>%

                mutate(percent_UnitStatus = QTY/UnitStatus_sum),

           

            aes(

               

                x = "",

                y = QTY,

                fill = UnitStatus

            )

        )

       

        + geom_bar(width = 1, stat = "identity", color = "white")

        + coord_polar("y", start=0)

        + geom_text(aes(y= lab.position, label = percent(percent_UnitStatus)), color = "white")

        # + scale_fill_manual(values = QTY)

        + theme_void()

        # #+ geom_point(size = 8)

        + labs(title = "Overall Procurement Status")

        # + theme_bw()

        # + geom_label_repel(aes(label = QTY))

    )
   

    #Mech Design pie chart

    output$MechDesign_piechart <- renderPlot(

 

        ggplot(

 

           MechDesign_machine_data %>%

                arrange(desc(MechDesign)) %>%

                mutate(MechDesign_sum = sum(QTY)) %>%

                mutate(lab.position = cumsum(QTY) - 0.5*QTY) %>%

                mutate(percent_MechDesign = QTY/MechDesign_sum),

 

            aes(

 

                x = "",

                y = QTY,

                fill = MechDesign

            )

        )

 

        + geom_bar(width = 1, stat = "identity", color = "white")

        + coord_polar("y", start=0)

        + geom_text(aes(y= lab.position, label = percent(percent_MechDesign)), color = "white")

        # + scale_fill_manual(values = QTY)

        + theme_void()

        # #+ geom_point(size = 8)

        + labs(title = "Mechnical Design Status")

        #+ scale_color_manual(values = "Green")

        # + theme_bw()

        # + geom_label_repel(aes(label = QTY))

    )

   

    #Elec Design pie chart

    output$ElecDesign_piechart <- renderPlot(

       

        ggplot(

           

            ElecDesign_machine_data %>%

                arrange(desc(ElecDesign)) %>%

                mutate(ElecDesign_sum = sum(QTY)) %>%

                mutate(lab.position = cumsum(QTY) - 0.5*QTY) %>%

                mutate(percent_ElecDesign = QTY/ElecDesign_sum),

           

            aes(

               

                x = "",

               y = QTY,

                fill = ElecDesign

            )

        )

       

        + geom_bar(width = 1, stat = "identity", color = "white")

        + coord_polar("y", start=0)

        + geom_text(aes(y= lab.position, label = percent(percent_ElecDesign)), color = "white")

        # + scale_fill_manual(values = QTY)

        + theme_void()

        # #+ geom_point(size = 8)

        + labs(title = "Electrical Design Status")

        #+ scale_color_manual(values = "Green")

        # + theme_bw()

        # + geom_label_repel(aes(label = QTY))

    )

   

    #RFQ/PO status bar chart

    output$RFQPOStatus_barchart <- renderPlot(
     

        ggplot(

 

            RFQStatus_machine_data,

 

               aes(

 

                   x = RFQStatus,

                   y = POStatus

 

           )

        )       

    )
   

    #Risk pie chart

    output$Risk_piechart <- renderPlot(

       

        ggplot(

           

            Risk_machine_data %>%

                arrange(desc(Risk)) %>%

                mutate(Risk_sum = sum(QTY)) %>%

                mutate(lab.position = cumsum(QTY) - 0.5*QTY) %>%

                mutate(percent_Risk = QTY/Risk_sum),

           

            aes(

               

                x = "",

                y = QTY,

                fill = Risk

            )

        )

       

        + geom_bar(width = 1, stat = "identity", color = "white")

        + coord_polar("y", start=0)

        + geom_text(aes(y= lab.position, label = percent(percent_Risk)), color = "white")

        # + scale_fill_manual(values = QTY)

        + theme_void()

        # #+ geom_point(size = 8)

        + labs(title = "Risk Distribution")

        # + theme_bw()

        # + geom_label_repel(aes(label = QTY))

    )

   

    #Overview Table

    output$MitigationPlan_Tracker <- renderTable(

        #DateFilter <- subset(select(engineer_machine_data, -QTY),engineer_machine_data$OnsiteDate == input$SelectDate)

        # DateFilter <- subset(select(engineer_machine_data, -QTY),engineer_machine_data$OnsiteDate == input$chosen_date)

        MitigationPlan_data %>%

            filter(Risk %in% input$chosen_Risk

            )

    )

   

}

 

# Run the application
shinyApp(ui = ui, server = shinyServer)
