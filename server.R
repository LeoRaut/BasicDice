

library(shiny)
library(shinythemes)
library(plotly)
source("helper.R")


shinyServer(function(input, output) {


  
  
     
  output$summary <- renderText({

    n_Sims <- 10000
    A_models	<-	as.numeric(input$A_models)
    A_attacks	<-	input$A_attacks
    A_hit	<-	as.numeric(input$A_hit)
    A_wound	<-	as.numeric(input$A_wound)
    A_rend	<-	as.numeric(input$A_rend)
    A_damage	<-	input$A_damage
    D_save	<-	as.numeric(input$D_save)
    D_models <- as.numeric(input$D_models)
    D_wounds <- as.numeric(input$D_wounds)
#    D_wardsave1	<-	input$D_wardsave1
#    D_wardsave2	<-	input$D_wardsave2
#    D_wardsave3	<-	input$D_wardsave3
#    Plus_attacks	<-	input$Plus_attacks
#    Plus_hit	<-	input$Plus_hit
#    Plus_wound	<-	input$Plus_wound
#    Plus_rend	<-	input$Plus_rend
#    Plus_damage	<-	input$Plus_damage
#    Minus_attack	<-	input$Minus_attack
#    Minus_hit	<-	input$Minus_hit
#    Minus_wound	<-	input$Minus_wound
#    Minus_rend	<-	input$Minus_rend
#    Minus_damage	<-	input$Minus_damage
#    Hit_rr_plus	<-	input$Hit_rr_plus
#    Hit_rr_minus	<-	input$Hit_rr_minus
#    Wound_rr_plus	<-	input$Wound_rr_plus
#    Wound_rr_minus	<-	input$Wound_rr_minus
#    Save_rr_plus	<-	input$Save_rr_plus
#    Save_rr_minus	<-	input$Save_rr_minus
#    Special_save	<-	input$Special_save
#    Spec_save_rr	<-	input$Spec_save_rr
    
    
    Damage_Distribution <-  func_DamageSimulator(n_Sims = n_Sims,A_models=A_models,A_attacks=A_attacks,A_hit=A_hit,A_wound=A_wound,A_rend=A_rend,A_damage=A_damage,
                                                 D_save=D_save,D_wardsave1=7,D_wardsave2=7,D_wardsave3=7,Plus_attacks=0,Plus_hit=2,Plus_wound=0,
                                                 Plus_rend=0,Plus_damage=0,Minus_attack=0,Minus_hit=0,Minus_wound=0,Minus_rend=0,
                                                 Minus_damage=0,Hit_rr_plus=1,Hit_rr_minus=1,Wound_rr_plus=1,Wound_rr_minus=1,
                                                 Save_rr_plus=1,Save_rr_minus=1,Special_save=1,Spec_save_rr=1) 
    
    col_n <- c("SimNo", "Damage")
    colnames(Damage_Distribution) <- col_n
    
     
    Pct_Unit_Dies <- sum(Damage_Distribution$Damage>=D_models*D_wounds)/sum(Damage_Distribution$Damage>=0)
    dmg <- summary(Damage_Distribution)
    
    paste('Chance of enemy unit dying (%):',Pct_Unit_Dies*100,dmg[4,2],dmg[2,2],dmg[5,2],sep="\n")  
         
    })  


    

    
  output$plot <- renderPlotly({
      n_Sims <- 10000
      A_models	<-	as.numeric(input$A_models)
      A_attacks	<-	input$A_attacks
      A_hit	<-	as.numeric(input$A_hit)
      A_wound	<-	as.numeric(input$A_wound)
      A_rend	<-	as.numeric(input$A_rend)
      A_damage	<-	input$A_damage
      D_save	<-	as.numeric(input$D_save)
      D_models <- as.numeric(input$D_models)
      D_wounds <- as.numeric(input$D_wounds)
  #    D_wardsave1	<-	input$D_wardsave1
  #    D_wardsave2	<-	input$D_wardsave2
  #    D_wardsave3	<-	input$D_wardsave3
  #    Plus_attacks	<-	input$Plus_attacks
  #    Plus_hit	<-	input$Plus_hit
  #    Plus_wound	<-	input$Plus_wound
  #    Plus_rend	<-	input$Plus_rend
  #    Plus_damage	<-	input$Plus_damage
  #    Minus_attack	<-	input$Minus_attack
  #    Minus_hit	<-	input$Minus_hit
  #    Minus_wound	<-	input$Minus_wound
  #    Minus_rend	<-	input$Minus_rend
  #    Minus_damage	<-	input$Minus_damage
  #    Hit_rr_plus	<-	input$Hit_rr_plus
  #    Hit_rr_minus	<-	input$Hit_rr_minus
  #    Wound_rr_plus	<-	input$Wound_rr_plus
  #    Wound_rr_minus	<-	input$Wound_rr_minus
  #    Save_rr_plus	<-	input$Save_rr_plus
  #    Save_rr_minus	<-	input$Save_rr_minus
  #    Special_save	<-	input$Special_save
  #    Spec_save_rr	<-	input$Spec_save_rr
      
                  
      Damage_Distribution <-  func_DamageSimulator(n_Sims = n_Sims,A_models=A_models,A_attacks=A_attacks,A_hit=A_hit,A_wound=A_wound,A_rend=A_rend,A_damage=A_damage,
                           D_save=D_save,D_wardsave1=7,D_wardsave2=7,D_wardsave3=7,Plus_attacks=0,Plus_hit=2,Plus_wound=0,
                           Plus_rend=0,Plus_damage=0,Minus_attack=0,Minus_hit=0,Minus_wound=0,Minus_rend=0,
                           Minus_damage=0,Hit_rr_plus=1,Hit_rr_minus=1,Wound_rr_plus=1,Wound_rr_minus=1,
                           Save_rr_plus=1,Save_rr_minus=1,Special_save=1,Spec_save_rr=1)    
 
       
    f <- list(
      family = "Courier New, monospace",
      size = 18,
      color = "#7f7f7f"
    )
    x <- list(
      title = "Damage",
      titlefont = f
    )
    y <- list(
      title = "Frequency(Count)",
      titlefont = f
    )
    
    col_n <- c("SimNo", "Damage")
    colnames(Damage_Distribution) <- col_n
    
    plot_ly(x = Damage_Distribution$Damage, type = "histogram") %>%
      layout(legend = 'l', xaxis = x, yaxis = y, title='Damage simulations') %>%
      add_segments(x = D_models*D_wounds, xend = D_models*D_wounds, y = 0,yend=n_Sims/13, showlegend=FALSE, name ='Unit Dies')


    
  })
  
})



