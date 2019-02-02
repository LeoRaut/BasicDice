




func_HitRolls <- function(A_models,A_attacks,A_hit,Plus_attacks=0,Minus_attacks=0,Plus_hit=0,Minus_hit=0,Hit_rr_plus=1,Hit_rr_minus=1) {
  
  A_models <- as.numeric(A_models)
  # A_attacks gets special treatment
  A_hit <- as.numeric(A_hit)
  
  
  ## Determine how many attacks per model before pluses and minuses to attacks  
  
  if(A_attacks =='2d6') {
    A_per_model <- sum(sample(1:6, size = 2, replace = TRUE))
  } else if (A_attacks == 'd6') {
    A_per_model <- sum(sample(1:6, size = 1, replace = TRUE))
  } else if (A_attacks == 'd3') {
    A_per_model <- sum(sample(1:3, size = 1, replace = TRUE))
  } else { #not random amount of attacks
    A_per_model <- as.numeric(A_attacks)
  }
  
  
  ## Generate first step of hit rolls
  
  Hit_Rolls  <- sample(1:6, size = A_models*(A_per_model+Plus_attacks-Minus_attacks), replace = TRUE)  
  
  ## Positive re-rolls
  
  if(Hit_rr_plus==4) { # reroll failed hit rolls
    
    RR_Hit_Value = A_hit-1
    
  } else if (Hit_rr_plus==3) { # reroll hits of 1 and 2
    RR_Hit_Value = 2
    
  } else if (Hit_rr_plus==2) { # reroll hits of 1
    RR_Hit_Value = 1    
  }
  
  
  if (Hit_rr_plus>1) { # compute good hit rerolls
    Not_RR_Hits = subset(Hit_Rolls,Hit_Rolls>RR_Hit_Value)
    
    RR_Hit_Rolls <- length(subset(Hit_Rolls,Hit_Rolls<=RR_Hit_Value))
    RR_Hits <-sample(1:6, size = RR_Hit_Rolls, replace = TRUE)
    
    Hit_Rolls <- c(Not_RR_Hits,RR_Hits) 
  }
  
  ## Negative re-rolls
  
  if(Hit_rr_minus==3) {
    Neg_hit_reroll <- A_hit
  } else if (Hit_rr_minus==2) {
    Neg_hit_reroll <- 6
  }
  
  if(Hit_rr_minus>1) {
    
    Not_RR_Hits = subset(Hit_Rolls,Hit_Rolls<Neg_hit_reroll)
    RR_Hit_Rolls = length(subset(Hit_Rolls,Hit_Rolls>=Neg_hit_reroll))
    RR_Hits <-sample(1:6, size = RR_Hit_Rolls, replace = TRUE)
    
    Hit_Rolls <- c(Not_RR_Hits,RR_Hits)    
  }
  
  # Amount of successful hits
  S_Hits <- sum(Hit_Rolls>=min(6,max(2,A_hit-Plus_hit+Minus_hit)))
  
  return (S_Hits)
  
}

func_WoundRolls <- function(S_Hits, A_wound, Plus_wound=0,Minus_wound=0,Wound_rr_plus=1,Wound_rr_minus=1) {
  
  A_wound <- as.numeric(A_wound)  
  
  ## Generate first step of wound rolls
  
  Wound_Rolls <- sample(1:6, size = S_Hits, replace = TRUE)
  
  ## Positive re-rolls
  
  if(Wound_rr_plus==4) { # reroll failed wounds rolls
    
    RR_Wound_Value = A_wound-1
    
  } else if (Wound_rr_plus==3) { # reroll wounds of 1 and 2
    RR_Wound_Value = 2
    
  } else if (Wound_rr_plus==2) { # reroll wounds of 1
    RR_Wound_Value = 1    
  }
  
  
  if (Wound_rr_plus>1) { # compute good wound rerolls
    Not_RR_Wound = subset(Wound_Rolls,Wound_Rolls>RR_Wound_Value)
    
    RR_Wound_Rolls <- length(subset(Wound_Rolls,Wound_Rolls<=RR_Wound_Value))
    RR_Wound <-sample(1:6, size = RR_Wound_Rolls, replace = TRUE)
    
    Wound_Rolls <- c(Not_RR_Wound,RR_Wound) 
  }
  
  ## Negative re-rolls
  
  if(Wound_rr_minus==3) { ## reroll successful wound rolls
    Neg_Wound_reroll <- A_wound
  } else if (Wound_rr_minus==2) { # reroll wounds rolls of 6
    Neg_Wound_reroll <- 6
  }
  
  if(Wound_rr_minus>1) { # compute negative wound rerolls
    
    Not_RR_Wound = subset(Wound_Rolls,Wound_Rolls<Neg_Wound_reroll)
    RR_Wound_Rolls = length(subset(Wound_Rolls,Wound_Rolls>=Neg_Wound_reroll))
    RR_Wound <-sample(1:6, size = RR_Wound_Rolls, replace = TRUE)
    
    Wound_Rolls <- c(Not_RR_Wound,RR_Wound)    
  }
  
  
  # Amount of successful wounds
  S_Wound <- sum(Wound_Rolls>=min(6,max(2,A_wound-Plus_wound+Minus_wound)))
  
  return (S_Wound)
  
}


func_SaveRolls <-function(S_Wounds, A_rend, D_save,Plus_rend=0,Minus_rend=0,Save_rr_plus=1,Save_rr_minus=1,Special_save=1){
  
  A_rend <- as.numeric(A_rend)  
  D_save <- as.numeric(D_save)    
  
  ## Generate first step of save rolls
  
  Save_Rolls <- sample(1:6, size = S_Wounds, replace = TRUE)
  
  ## Positive re-rolls
  
  if(Save_rr_plus==4) { # reroll failed save rolls
    
    RR_Save_Value = D_save-1
    
  } else if (Save_rr_plus==3) { # reroll save of 1 and 2
    RR_Save_Value = 2
    
  } else if (Save_rr_plus==2) { # reroll save of 1
    RR_Save_Value = 1    
  }
  
  
  if (Save_rr_plus>1) { # compute good save rerolls
    Not_RR_Save = subset(Save_Rolls,Save_Rolls>RR_Save_Value)
    
    RR_Save_Rolls <- length(subset(Save_Rolls,Save_Rolls<=RR_Save_Value))
    RR_Save <-sample(1:6, size = RR_Save_Rolls, replace = TRUE)
    
    Save_Rolls <- c(Not_RR_Save,RR_Save) 
  }
  
  ## Negative re-rolls
  
  if(Save_rr_minus==3) { # reroll successful save rolls
    Neg_Save_reroll <- D_save
  } else if (Save_rr_minus==2) { # reroll saves of 6
    Neg_Save_reroll <- 6
  }
  
  if(Save_rr_minus>1) { # compute negative save rerolls
    
    Not_RR_Save = subset(Save_Rolls,Save_Rolls<Neg_Save_reroll)
    RR_Save_Rolls = length(subset(Save_Rolls,Save_Rolls>=Neg_Save_reroll))
    RR_Save <-sample(1:6, size = RR_Save_Rolls, replace = TRUE)
    
    Save_Rolls <- c(Not_RR_Save,RR_Save)    
  }  
  
  if (Special_save == 3 && A_rend == 1) { # ignores rend 1
    Save_modifier = 0
  } else if (Special_save == 2) { #ethereal
    Save_modifier = 0
  } else {
    Save_modifier = A_rend+Plus_rend-Minus_rend
  }  
  
  Failed_Saves <- sum(Save_Rolls<max(2,(D_save+Save_modifier)))  
  
  return (Failed_Saves)  
  
}


func_DamageRolls <- function(A_damage,Failed_Saves,Plus_damage=0,Minus_damage=0){
  
  
  if (A_damage == 'd3') { # d3 damage
    
    Damage <- sum(sample(1:3, size = Failed_Saves, replace = TRUE)+Failed_Saves*(Plus_damage-Minus_damage))
    
  } else if (A_damage == 'd6') { # D6 damage
    Damage <- sum(sample(1:6, size = Failed_Saves, replace = TRUE)+Failed_Saves*(Plus_damage-Minus_damage))
    
  } else if (A_damage == '2d6') { # 2d6 damage
    Damage <- sum(sample(1:6, size = Failed_Saves*2, replace = TRUE)+Failed_Saves*(Plus_damage-Minus_damage))
    
  } else { # non-random damage
    A_damage <- as.numeric(A_damage)
    Damage <- max(0,A_damage+Plus_damage-Minus_damage) * Failed_Saves
  }
  
  return (Damage)  
  
}

func_Wardsaves <- function(Damage,D_wardsave1=7,D_wardsave2=7,D_wardsave3=7,Spec_save_rr=1){
  
  D_wardsave1 <- as.numeric(D_wardsave1)
  D_wardsave2 <- as.numeric(D_wardsave2)
  D_wardsave3 <- as.numeric(D_wardsave3)
  
  
  if (D_wardsave1 < 7) {
    
    wardsave1_Rolls <- sample(1:6, size = Damage, replace = TRUE)
    
    if (Spec_save_rr == 3) { # reroll failed wardsaves
      RR_1_wardsave1 = 0
      
      RR_wardsave1_Value = D_wardsave1-1
      
      Not_RR_wardsave1s = subset(wardsave1_Rolls,wardsave1_Rolls>RR_wardsave1_Value)
      
      RR_wardsave1_Rolls <- length(subset(wardsave1_Rolls,wardsave1_Rolls<=RR_wardsave1_Value))
      RR_wardsave1s <-sample(1:6, size = RR_wardsave1_Rolls, replace = TRUE)
      
      wardsave1_Rolls <- c(Not_RR_wardsave1s,RR_wardsave1s)
      
    } else if (Spec_save_rr == 2) { #reroll 1s to save
      
      RR_wardsave1_Value = 1
      
      Not_RR_wardsave1s = subset(wardsave1_Rolls,wardsave1_Rolls>RR_wardsave1_Value)
      
      RR_wardsave1_Rolls <- length(subset(wardsave1_Rolls,wardsave1_Rolls<=RR_wardsave1_Value))
      RR_wardsave1s <-sample(1:6, size = RR_wardsave1_Rolls, replace = TRUE)
      
      wardsave1_Rolls <- c(Not_RR_wardsave1s,RR_wardsave1s)
      
    }
    
    Damage <- sum(wardsave1_Rolls<max(2,D_wardsave1))
    
    
  }   
  
  if (D_wardsave2 < 7) {
    wardsave2_Rolls <- sample(1:6, size = Damage, replace = TRUE)
    Damage <- sum(wardsave2_Rolls<max(2,D_wardsave2))     
  }
  
  if (D_wardsave3 < 7) {
    wardsave3_Rolls <- sample(1:6, size = Damage, replace = TRUE)
    Damage <- sum(wardsave3_Rolls<max(2,D_wardsave3))          
  } 
  
  
  return (Damage)  
  
}


func_DamageSimulator <-function(n_Sims,A_models,A_attacks,A_hit,A_wound,A_rend,A_damage,D_save,
                                D_wardsave1,D_wardsave2,D_wardsave3,Plus_attacks,Plus_hit,Plus_wound,
                                Plus_rend,Plus_damage,Minus_attack,Minus_hit,Minus_wound,Minus_rend,
                                Minus_damage,Hit_rr_plus,Hit_rr_minus,Wound_rr_plus,Wound_rr_minus,
                                Save_rr_plus,Save_rr_minus,Special_save,Spec_save_rr) {
  
  Damage_Distribution <- data.frame(matrix(ncol = 2, nrow = 0))  
  
  for (i in 1:n_Sims) {
    
    S_Hits <- func_HitRolls(A_models=A_models,A_attacks=A_attacks,A_hit=A_hit,Plus_attacks=Plus_attacks,Minus_attacks=Minus_attack,Plus_hit=Plus_hit,Minus_hit=Minus_hit,Hit_rr_plus=Hit_rr_plus,Hit_rr_minus=Hit_rr_minus)
    
    S_Wounds <- func_WoundRolls(S_Hits=S_Hits, A_wound=A_wound, Plus_wound=Plus_wound,Minus_wound=Minus_wound,Wound_rr_plus=Wound_rr_plus,Wound_rr_minus=Wound_rr_minus)
    
    Failed_Saves <- func_SaveRolls(S_Wounds=S_Wounds, A_rend=A_rend, D_save=D_save,Plus_rend=Plus_rend,Minus_rend=Minus_rend,Save_rr_plus=Save_rr_plus,Save_rr_minus=Save_rr_minus,Special_save=Special_save)
    
    Damage <- func_DamageRolls(A_damage=A_damage,Failed_Saves=Failed_Saves,Plus_damage=Plus_damage,Minus_damage=Minus_damage)
    
    Final_Damage <- func_Wardsaves(Damage=Damage,D_wardsave1=D_wardsave1,D_wardsave2=D_wardsave2,D_wardsave3=D_wardsave3,Spec_save_rr=Spec_save_rr)
    
    
    Damage_Distribution <- rbind(Damage_Distribution,c(i,Final_Damage))
    
  }
  
  
  return (Damage_Distribution)
}
