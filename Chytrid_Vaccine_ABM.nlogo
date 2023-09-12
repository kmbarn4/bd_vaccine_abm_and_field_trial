;Chytridiomycosis ABM

extensions [rnd]

globals
[
  zspn-inc                                       ;incidence of zoosporangia per day
  bd-mortality                                   ;number of frogs dying of Bd
  baseline-mortality                             ;number of metamorphs dying due to background mortality
  lambda-zsp                                     ;growth rate of zoospores
  n-zspn                                         ;sum of zoosporangium
  shoreline                                      ;shoreline perimeter
  k                                              ;max number of frogs
  num-metamorphosis                              ;number of tadpoles that undergo metamorphosis (number of metamorphs prior to baseline mortality or bd death)
  var_spn_abun_tadpoles                          ;variance of tadpole parasite abundances
  avg_spn_abun_tadpoles                          ;mean parasite abundance of tadpoles
  aggregation_abun_tadpoles                      ;parasite abundance aggregation
  prop_not_s_k_abun                              ;proportion of tadpoles with spn less than s_k
  avg_spn_inten_tadpoles                         ;mean infection intensity of tadpoles
  median_spn_inten_tadpoles                      ;median infection intensity of tadpoles
  var_spn_inten_tadpoles                         ;variance of tadpole infection intensities
  aggregation_inten_tadpoles                     ;infection intensity aggregation
  prop_not_s_k_inten                             ;proportion of tadpoles with infection intensities greater than zero and less than carrying capacity
  tad_prev                                       ;prevalence of bd in tadpoles
  avg_spn_inten_metas                            ;mean infection intensity of metamorphs
  median_spn_inten_metas                         ;median infection intensity of metamorphs
  var_spn_inten_metas                            ;variance of metamorph infection intensities
  aggregation_inten_metas                        ;parasite aggregation in metamorphs
  metas_prev                                     ;prevalence of Bd in metas
  prop_deaths_due_to_bd                          ;proportion of metamorph deaths attributable to Bd = bd-mortality / (bd-mortality + baseline-mortality)
  total-zsp-density                              ;desnity of zsp across perimeter pond patches
  realized-coverage                              ;actual proportion of tadpoles that are immunized
]

breed [ tadpoles tadpole ]
breed [ metamorphs metamorph ]

patches-own
[
  border                                          ;this patch variable is set to 1 if a patch is located on the border of the model landscape, 0 if a non-border patch
  pond                                            ;this patch variable is set to 1 if a patch has pond, 0 if otherwise
  nspn                                            ;sum of zoosporangia on all frogs in this patch
  zsp                                             ;number of zoospores in a pond patch
  prev-zsp                                        ;number of zoospores in the previous time step (to calculate lambda)
  pondid                                          ;pond number
  pp                                              ;pond perimeter
  new-infections                                  ;output of multinomial draws, represents a list where for each time a host appears in the list it has a new zoospore successfully infecting it
  perimeter-land                                  ;perimeter land patch
]

metamorphs-own
[
  aid                                            ;age in days
  bd                                             ;1 if infected, 0 if uninfected
  spn                                            ;sporangia load
  imm                                            ;immunity
  smax                                           ;maximum sporangia load - mortality occurs when sporangia load exceeds smax
  est                                            ;probability of successful infection establishment
  expo                                           ;amount of environmental units each host is exposed to per day
  infprob
  new-pz0                                        ;temporary new pz0 for each patch
  pz0                                            ;prezoosporangium (pz0-pz4 are used to create a 4 day lag period from zoospore establishment till the maturation of an infectious sporangia) - Voyles et al. 2012
  pz1
  pz2
  pz3
  pz4
  b_cohort                                       ;specifies birth cohort
  on-land                                        ;move to a land patch
  immunized                                      ;1 if immunity is from vaccine, 0 if unvaccinated or vaccine ineffective at inducing an immune response
]

tadpoles-own
[
  aid                                           ;age in days
  bd                                            ;1 if infected, 0 if uninfected
  spn                                           ;sporangia load
  imm                                           ;immunity
  s_k                                           ;maximum number of zoospores on tadpoles due to space limitation (sporangia carrying capacity)
  est
  expo
  infprob
  new-pz0
  pz0                                           ;prezoosporangium (pz0-pz4 are used to create a 4 day lag period from zoospore establishment till the maturation of an infectious sporangia)- Voyles et al. 2012
  pz1
  pz2
  pz3
  pz4
  b_cohort                                      ;specifies birth cohort if modeling birth pulses
  immunized                                     ;1 if immunity is from vaccine, 0 if unvaccinated or vaccine ineffective at inducing an immune response
]

to setup
  ca
  ifelse SimplePond = TRUE
  [ resize-world 0 3 0 3
    set-patch-size 80 ]
  [ set-patch-size 17     ;5
    resize-world 0 20 0 20 ]

  ask patches [
    set pcolor brown
    set pond 0
    ]
  ;defines border patches
  let maxx max [ pxcor ] of patches
  let maxy max [ pycor ] of patches
  let minx min [ pxcor ] of patches
  let miny min [ pycor ] of patches
  ask patches [
   ifelse pxcor = maxx or pxcor = minx or pycor = maxy or pycor = miny
    [ set border 1 ]
    [ set border 0 ]
  ]
  ;define non-border patches (nbpatches) because we don't want pond patches on the border
  let nbpatches patches with [ border = 0 ]
  ifelse SimplePond = TRUE
  [ ask one-of nbpatches [
    set pond 1
    set pondid 1
    set pcolor blue
    initialize-tadpole-pop
    initialize-Bd-tadpoles
    ]
    ]
  [
  create-pond
  ]
    ;below code specifies which patches are on the perimeter.
  ask patches with [ pond = 1 ] [
     if count neighbors4 with [ pond = 0 ] > 0 [
        set pp 1
        set pcolor 107
      ]
  ]
   ask patches with [ pond = 0 ] [
     if count neighbors4 with [ pond = 1 ] > 0 [
        set perimeter-land 1
    ]
  ]
    ask patches with [pp = 1][
    ;reports the shoreline in the command center (if I want that)
  ;  calculate-shoreline
    ]
   ; print shoreline  ;in case I want to know what the shoreline/perimeter is
    let perimeterp patches with [ pp = 1 ]
    ask perimeterp [
     initialize-tadpole-pop
    if inf-ponds = 1 [
      initialize-Bd-tadpoles
    ]
  ]
  ; if only wanting 1 pond patch to be infected, comment out lines 145-147 and use lines 151-155
;  if inf-ponds = 1 [
;      ask n-of 1 perimeterp [  ;if only wanting to initialize 1 patch with Bd tadpole
;      initialize-Bd-tadpoles
;    ]
;  ]
  reset-ticks
end

to go
;  random-seed behaviorspace-run-number
  if ticks = last-day [ stop ]
  ask tadpoles [
    set aid aid + 1                  ;adding a day to the tadpole's age
   if random-float 1 < tad-mort [    ;tad-mort is a scale of mortality probabilities, kept at 0.06 (Govindarajulu 2006)
    die
    ]
  ]
  if birth_pulses = 2 [
  if ticks = 7 [
    ask patches with [ pp = 1 ] [
    initialize-tadpole-pop_2
    ]
  ]
  ]
  if birth_pulses = 3 [
  if ticks = 7 [
   ask patches with [ pp = 1 ] [
   initialize-tadpole-pop_2
    ]
  ]
  if ticks = 14 [
    ask patches with [ pp = 1 ] [
     initialize-tadpole-pop_3
    ]
  ]
  ]
  if ticks = vaccination-day [      ;execute vaccination step
    let num-vaccinate round(v-coverage * (count tadpoles))
    ask n-of num-vaccinate tadpoles [
    if bd = 0 [                     ;vaccination only effective in uninfected tadpoles (Barnett et al. 2023)
    set imm v-efficacy - (0.5 * relative_variation * v-efficacy) + random-float (relative_variation * v-efficacy)  ;implement variation in response to vaccination
    set est baseline_est * exp(c_est * imm)
    set immunized 1
    ]
    let vac-immunized-tadpoles count tadpoles with [immunized = 1]
    set realized-coverage precision (vac-immunized-tadpoles / (count tadpoles)) 2
    ]
  ]
  ask metamorphs [
    set aid aid + 1                                  ;adding a day to the metamorph's age
    if random-float 1 < meta-mort [                  ;meta_mort is a scale of mortality probabilities, kept at 0.02
      set baseline-mortality baseline-mortality + 1  ;keep track of baseline mortality
      die
    ]
  ]

  if any? tadpoles with [ aid > 55 ] [  ;initiate metamorphosis in (some) tadpoles greater than 55 days old
  ask tadpoles with [ aid > 55 ] [
  if random-float 1 < 0.11 [
  metamorphosis
    ]
    ]
  ]
;  if ticks = 75 [                  ;implement if tadpoles in a later birth pulse have less time as tadpoles before metamorphosis
;    if any? tadpoles [
;    ask tadpoles[
;    metamorphosis
;      ]
;    ]
;  ]
    if any? tadpoles with [ aid > 74 ] [   ;regardless of birth cohort, all tadpoles have the same amount of time to metamorphose
    ask tadpoles[                          ;metamorphosis ranges from 56-75 days in tadpole age
    metamorphosis
      ]
    ]
 ;tadpole movement submodel
  if SimplePond = FALSE [
    tadpoles-move
   ; metamorphs-move
   ; metamorphs-move-simple
    metamorphs-move-complex
  ]
  let pondppatches patches with [ pond = 1 and pp = 1]
  ask pondppatches [
    set prev-zsp zsp                                                            ;store the last tick's zoospores as previous zoospores
  ;  set pcolor scale-color red zsp 1000000 0
    ]
    let bd-tadpoles tadpoles with [ bd = 1  and spn < s_k ]
  ask bd-tadpoles
    [
      update-infections
      if spn >= 8000 [
        set color red
        ]
      tadpole-zsp-shedding-and-reinfection
        ]
    let maxbd-tadpoles tadpoles with [ bd = 1  and spn >= s_k ]      ;maxbd-tadpoles are tadpoles that have met or exceeded sporangia carrying capacity
    ask maxbd-tadpoles [                                             ;maxbd-tadpoles cannot get reinfected but can still contribute to the zoospore pool and clear sporangia
      if spn >= 8000 [                                               ;set these tadpoles as red if their sporangia load is above 8000
        set color red
        set est 0  ;this prevents maxbd-tadpoles from getting reinfected, but they can still be exposed to Bd
        ]
        max-bd-tadpole-shedding
    ]

  let bd-metamorphs metamorphs with [ bd = 1 ]
    ask bd-metamorphs
    [
     update-infections
     if spn >= smax [
        set bd-mortality bd-mortality + 1
       ;set color red
        die
        ]
       ; binomial draw from probability of sporangia clearance to determine how many sporangia survive to the next time step
       let n spn
       let p exp(- (baseline_spn_clearance) * exp(c_clear * imm))
       set spn length filter [i -> i < p] n-values n [random-float 1]
      if spn = 0 [                                                    ;uninfected metamorphs appear green
        set bd 0
        set imm imm + natural_imm_efficacy                            ;can update immunity with natural resistance acquired by clearing an infection
        set color green
        ]
      let zsp-release round (baseline_shedding * exp(c_shedding * imm)* spn) ;baseline zoospore release rate 17.8 per spn per day at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI), but as a function of immunity
      let f-selfinfect round (0.1 * zsp-release)                             ;fraction of the released zoospores that immediately encounter the host - SELF EXPOSURE
      set pz0 f-selfinfect * est ;pz0 + 1                                    ;self infection
     ask patch-here [
        set zsp zsp + (zsp-release - f-selfinfect)
        ]
      ]
    ;asking infected pond perimeter patches
   ask patches with [pond = 1] [
    set pcolor scale-color red zsp 1000000 0
    ]
  ;make netlogo plot here
  ;plot-zsp
  let zsp-environment count patches with [pp = 1]
  set total-zsp-density total-zsp-density + round(sum [zsp] of patches with [pp = 1] / zsp-environment)
  ;print total-zsp-density
 ;ask patches with [zsp > 0] [  ;both land and pond patches are infectious
 ask patches with [zsp > 0 and pond = 1] [  ;only pond patches are infectious
  infection-step
  ]
  ask patches with [zsp > 0 and pond = 0] [
    let z-mort 2
    let p-left-in-water exp ( - (z-mort) )
    let surviving-in_water round(zsp * p-left-in-water)
    set zsp surviving-in_water
  ]
  plot-zsp
  ask turtles with [pz0 > 0] [
    set bd 1
    set color white
  ]
  if ticks > 1 [
    if sum [ prev-zsp ] of patches > 0 [
      set lambda-zsp precision (sum [ zsp ] of patches / sum [ prev-zsp ] of patches) 2
      ]
    ]
  set n-zspn sum [ spn ] of tadpoles
  set zspn-inc 0
  ;write-data-file
;  if ticks = 53 [
;  summary-stats-tad-intensity
;  ]
  ask metamorphs with [bd = 0] [
    set spn 0
  ]
  let total-bd-metas count metamorphs with [spn > 0]
  if ticks = last-day - 1 [
  set prop_deaths_due_to_bd bd-mortality / (bd-mortality + baseline-mortality)
  if total-bd-metas >= 2 [
  summary-stats-meta-intensity
  ]
  ]
  tick
end

;procedures below

to summary-stats-tad-intensity
  set median_spn_inten_tadpoles median [ spn ] of tadpoles with [spn > 0]
  set avg_spn_inten_tadpoles mean [ spn ] of tadpoles with [spn > 0]
  set var_spn_inten_tadpoles variance [ spn ] of tadpoles with [spn > 0]
  set aggregation_inten_tadpoles (variance [spn] of tadpoles with [spn > 0]) / mean [spn] of tadpoles with [spn > 0]
  set prop_not_s_k_inten (count tadpoles with [spn < s_k and spn > 0]) / count tadpoles with [spn > 0]
  set tad_prev (count tadpoles with [ bd = 1 ]) / count tadpoles
end

to summary-stats-meta-intensity
  set median_spn_inten_metas median [ spn ] of metamorphs with [spn > 0]
  set avg_spn_inten_metas mean [ spn ] of metamorphs with [spn > 0]
  set var_spn_inten_metas variance [ spn ] of metamorphs with [spn > 0]
  set aggregation_inten_metas (variance [spn] of metamorphs with [spn > 0]) / mean [spn] of metamorphs with [spn > 0]
;  set prop_not_smax_inten (count metamorphs with [spn < smax and spn > 0]) / count metamorphs with [spn > 0]
  set metas_prev (count metamorphs with [ bd = 1 ]) / count metamorphs
end

to plot-zsp
  set-current-plot "Zsp Count"
  set-current-plot-pen "zsp"
  plot sum [ zsp ] of patches with [pond = 1]
end

;populate patches with tadpoles
to initialize-tadpole-pop

  if birth_pulses = 1 [
  sprout-tadpoles ini-tadpoles-per-pondpatch [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    ;c_est is a constant determining the scale of imm parameter's impact on establishment parameter
    set expo 0.25
   ; exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
    ]
  ]

    if birth_pulses = 2 [
    let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 2)
    sprout-tadpoles new-tadpoles [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    set expo 0.25
    ;set expo 0.07 ;exposure rate
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
    ]
  ]

   if birth_pulses = 3 [
   let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 3)
  sprout-tadpoles new-tadpoles [
    set b_cohort 1        ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    set expo 0.25
    ; exposure rate
    set infprob est * expo
    set color 65
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
    ]
  ]
end

;select a certain number of tadpoles in each patch to have Bd
to initialize-Bd-tadpoles
  if Bd-inf-tadpoles-per-infpondpatch > 0 [
    ask n-of Bd-inf-tadpoles-per-infpondpatch tadpoles-here [
      set bd 1
      set color white
      set spn 100
      ]
    ]
end

;populate patches with tadpoles with birth cohort 2
;there's probably an easier way to do this than creating multiple submodels but will do this for now
to initialize-tadpole-pop_2
  if birth_pulses = 2 [   ;divide initial tadpoles per patch by three because there are 3 birth pulses, this way total initial tadpoles is the same
    let new-tadpoles round (ini-tadpoles-per-pondpatch) / 2
   sprout-tadpoles new-tadpoles [
    set b_cohort 2                  ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    set expo 0.25
    set infprob est * expo
    set color 115
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
    ]
  ]

    if birth_pulses = 3 [
    let new-tadpoles round ((ini-tadpoles-per-pondpatch) / 3)    ;divide initial tadpoles per patch by three because there are 3 birth pulses, this way total initial tadpoles is the same
    sprout-tadpoles new-tadpoles [
    set b_cohort 2                 ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    set expo 0.25
    ;set expo 0.07  ;exposure rate
    set infprob est * expo
    set color 115
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
  ]
  ]
end

;populate patches with tadpoles with birth cohort 3
;there's probably an easier way to do this than creating multiple submodels but will do this for now
to initialize-tadpole-pop_3
  let new-tadpoles3 round ((ini-tadpoles-per-pondpatch) / 3)
  sprout-tadpoles new-tadpoles3 [
    set b_cohort 3                  ;specify birth cohort
    set shape "frog top"
    set size 0.2
    set spn 0
    set imm 0
    set s_k  10000
    set est baseline_est * exp(c_est * imm)
    set expo 0.25
    ;exposure rate: amount of the environmental untis per host per day (units = liters per host per day), like a search term
                                  ;functions as a removal rate of parasites from the environemnt due to contact process
    set infprob est * expo
    set color 125
    let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
    let face-dir-x [ pxcor ] of dir-neighbor
    let face-dir-y [ pycor ] of dir-neighbor
    facexy face-dir-x face-dir-y
    rt random 40
    fd (0.32 + random-float 0.13)
    ]
end

to tadpoles-move
  let n-mobile-tadpoles round (t-movement * count tadpoles)                           ;25% of tadpoles move to a different perimeter patch each tick
    ask n-of n-mobile-tadpoles tadpoles [
      let nextpatch patches with [pp = 1]
      move-to one-of nextpatch
      let dir-neighbor min-one-of patches with [ pond = 0 ] [ distance myself ]
      let face-dir-x [ pxcor ] of dir-neighbor
      let face-dir-y [ pycor ] of dir-neighbor
      facexy face-dir-x face-dir-y
      rt random 40
      fd (0.32 + random-float 0.13)
  ]
end

;to metamorphs-move
;let n-land-metamorphs round (0.20 * count metamorphs)
;ask n-of n-land-metamorphs metamorphs [
;    ;move-to one-of patches with [ pond = 1 and pp = 0] ;metamorphs in deep pond
;    move-to one-of patches with [ perimeter-land = 1]
;  ]
;end

to metamorphs-move-complex
ask metamorphs[
    set on-land 0          ;re-set it so that all metamorphs are defaulted to be equally likely to move between land and water, regardless of what type of patch currently on
  ]
let n-land-metamorphs round (m-land * count metamorphs)
ask n-of n-land-metamorphs metamorphs[
set on-land 1             ;select a subset of metamorphs to move to a land patch
  ]
ask metamorphs [
    ifelse on-land = 1
    [move-to one-of patches with [ perimeter-land = 1]]
    [move-to one-of patches with [ pond = 1  and pp = 1]]
  ]
end

to metamorphs-move-simple
ask metamorphs [                                                             ;all metamorphs move to another perimeter patch
    move-to one-of patches with [ pond = 1  and pp = 1]
    ]
end


to update-infections
  let z1 pz0             ;upon establishment, a zoospore takes ~4 days to mature into a sporangia (Voyles et al. 2012)
      let z2 pz1
      let z3 pz2
      let z4 pz3
      set pz1 z1
      set pz2 z2
      set pz3 z3
      set pz4 z4
      set pz0 0
      set spn spn + pz4
      set zspn-inc zspn-inc + pz4
end

to metamorphosis
        hatch-metamorphs 1 [
          set aid 1
          set b_cohort [ b_cohort ] of myself     ;keep track of birth cohort identity
          set bd [ bd ] of myself                 ;maintain bd infection through metamorphosis (McMahon & Rohr, 2015)
          set expo [expo] of myself
          set est [est] of myself
          set infprob [infprob] of myself
          set immunized [immunized] of myself
;          set new-pz0 [new-pz0] of myself
          if bd = 1 [
             set pz0 [ pz0 ] of myself   ;carryover 100% of Bd infections through metamorphosis
             set pz1 [ pz1 ] of myself
             set pz2 [ pz2 ] of myself
             set pz3 [ pz3 ] of myself
             set pz4 [ pz4 ] of myself
             set spn [ spn ] of myself   ; maintain tadpole infection intensity loads

    ]
          set smax baseline_smax * exp(c_smax * imm)    ;defining smax as a function of immune status
          set shape "frog top"
          set size 0.4
          set color brown
          ]
        set num-metamorphosis num-metamorphosis + 1
        die                             ;remove tadpole from population once it's metamorphosed
end

to tadpole-zsp-shedding-and-reinfection
     ; binomial draw from probability of sporangia clearance to determine how many sporangia survive to the next time step
       let n spn
       let p exp(- (baseline_spn_clearance) * exp(c_clear * imm))     ;baseline_spn_clearance is 0.20 (used in Briggs et al. 2010)
       set spn length filter [i -> i < p] n-values n [random-float 1]
       if spn = 0 [                                                   ;uninfected metamorphs appear green
        set bd 0
        set imm imm + natural_imm_efficacy                            ;update immunity with natural resistance acquired by clearing an infection
        set color green
        ]
      let zsp-release round (baseline_shedding * exp(c_shedding * imm)* spn)  ;baseline zoospore release rate 17.8 per spn per day at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI), but as a function of immunity
      let f-selfinfect round (0.1 * zsp-release)                ;fraction of the released zoospores that immediately self-infect the host (Briggs et al. 2010)
      set pz0 f-selfinfect * est ;pz0 + 1
      let same-patch-zsp round (0.4 * (zsp-release - f-selfinfect)) ;40% of zoospores in pool deposited into the patch the tadpole is currently on
      ask patch-here [
      set zsp zsp + same-patch-zsp
        ]
      let near-shallow-patch-zsp round (0.58 * (zsp-release - f-selfinfect - same-patch-zsp)) ;35% of zoospores in pool deposited onto neighbor patch
      ask one-of neighbors with [pp = 1][
      set zsp zsp + near-shallow-patch-zsp
        ]
      let deep-patch-zsp round (zsp-release - f-selfinfect - same-patch-zsp - near-shallow-patch-zsp) ;25% of zoospores in pool deposited onto neighbor patch
      ask one-of patches with [pp = 0] [
      set zsp zsp + deep-patch-zsp
        ]
end

to max-bd-tadpole-shedding
     ; binomial draw from probability of sporangia clearance to determine how many sporangia survive to the next time step
       let n spn
       let p exp(- (baseline_spn_clearance) * exp(c_clear * imm))
       set spn length filter [i -> i < p] n-values n [random-float 1]
     if spn = 0 [                                                      ;uninfected metamorphs appear green
        set bd 0
        set imm imm + natural_imm_efficacy                            ;update immunity with natural resistance acquired by clearing an infection
        set color green
        ]
     let zsp-release round (baseline_shedding * exp(c_shedding * imm)* spn) ;baseline zoospore release rate 17.8 per spn per day at 23 degrees C (Woodhams et al., 2008; Briggs 2010 SI), but as a function of immunity
     let f-selfinfect round (0.1 * zsp-release)                   ;fraction of the released zoospores that immediately self-infect the host (Briggs et al. 2010)
                                                                    ;because these tadpoles have maxxed out their infection loads, these f-selfinfect zoospores do not actually establish pz0
                                                                    ;but they are prevented from re-entering the zoospore pool
  ;     set pz0 f-selfinfect * est ;pz0 + 1                               ;no self-reinfection since they are at their max so all self-reinfections fail
      let same-patch-zsp round (0.4 * (zsp-release - f-selfinfect)) ;40% of zoospores in pool deposited into the patch the tadpole is currently on
      ask patch-here [
      set zsp zsp + same-patch-zsp
        ]
      let near-shallow-patch-zsp round (0.58 * (zsp-release - f-selfinfect - same-patch-zsp)) ;35% of zoospores in pool deposited onto neighbor patch
      ask one-of neighbors with [pp = 1][
      set zsp zsp + near-shallow-patch-zsp
        ]
      let deep-patch-zsp round (zsp-release - f-selfinfect - same-patch-zsp - near-shallow-patch-zsp) ;25% of zoospores in pool deposited onto neighbor patch
      ask one-of patches with [pp = 0] [
      set zsp zsp + deep-patch-zsp
        ]
end

to infection-step
    set nspn (sum [ spn ] of tadpoles-here + sum [ spn ] of metamorphs-here)                           ;count total number of zoosporangia on all infected frogs
    ;print nspn
    let z-mort 2
    let frogs count tadpoles-here + count metamorphs-here
    let total-expo sum [expo] of turtles ;contact process (exposure) is dependent on the number of hosts - i.e. more hosts, more likely a zoospore will find one
    let p-left-in-water exp ( - (z-mort + total-expo) )
    let surviving-in_water round(zsp * p-left-in-water) ;deterministic how many survive in water
    let successful-infections round( zsp * (1 - p-left-in-water) * (sum [expo * est] of turtles) / (z-mort + total-expo)) ;assumed number of zoospores that have established
    set new-infections rnd:weighted-n-of-with-repeats successful-infections turtles [infprob]
     ask turtles-here [
      let my-new-infections filter [i -> i = self] new-infections ;subset new-infections list to create a list of only myself
      if length my-new-infections > 0[
      set new-pz0 length my-new-infections  ;the length of the subsetted list is how many new zoospores were established in this specific host
     ; show new-pz0
      set pz0 pz0 + new-pz0 ;add new infections to previous pz0
    ]
  ]
    set zsp surviving-in_water
    ;plot-zsp
end

to create-data-file
   if not file-exists? (word "results/no_Bd_baseline_bddynamics.csv") [
   ; file-open (word "results/baseline_bddynamics" date-and-time ".csv")
    file-open (word "results/no_Bd_baseline_bddynamics.csv")
    file-print "day, N_tadpoles, INF_tadpoles, dead_frogs, zspn, zsp, prevzsp, lambda, zspn-inc, N_metamorphs, INF_metamorphs"  ;dead_frogs includes all life stages (tadpoles, metas, frogs) dead with Bd but really only metas are dying of Bd
    file-close
    ]
end

to write-data-file
  file-open (word "results/no_Bd_baseline_bddynamics.csv")
  file-type ticks file-type "," file-type count tadpoles file-type "," file-type count tadpoles with [ bd = 1 ] file-type "," file-type bd-mortality file-type "," file-type n-zspn file-type ","file-type sum [ zsp ] of patches file-type "," file-type sum [ prev-zsp ] of patches file-type "," file-type lambda-zsp file-type "," file-type zspn-inc file-type "," file-type count metamorphs file-type "," file-type count metamorphs with [ bd = 1 ] file-type ","
  file-print ""
  file-close
end

to create-pond
  let nbpatches patches with [ border = 0 ]
  let counter 0
    ask one-of nbpatches [
      set pond 1
      set counter counter + 1
      set pcolor blue
      ;below lines specify the size of the pond (mean and variance)
      let pondsize round random-normal 135 18
      repeat pondsize [
        ask min-one-of patches with [ pond = 0 and border = 0 ] [ distance myself ] [
          set pond 1
          set pcolor blue
          ]
        ]
      ]
end

to calculate-shoreline
        if count neighbors4 with [ pond = 0 ] = 4 [
        set shoreline shoreline + 4
      ]
      if count neighbors4 with [ pond = 0] = 3 [
        set shoreline shoreline + 3
      ]
      if count neighbors4 with [ pond = 0] = 2 [
        set shoreline shoreline + 2
      ]
      if count neighbors4 with [ pond = 0] = 1 [
        set shoreline shoreline + 1
      ]
end
@#$#@#$#@
GRAPHICS-WINDOW
330
10
695
376
-1
-1
17.0
1
10
1
1
1
0
0
0
1
0
20
0
20
1
1
1
ticks
30.0

BUTTON
19
38
83
71
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
121
39
184
72
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
703
14
760
59
day
ticks + 1
17
1
11

SLIDER
14
213
234
246
ini-tadpoles-per-pondpatch
ini-tadpoles-per-pondpatch
0
1000
500.0
10
1
NIL
HORIZONTAL

SLIDER
14
250
275
283
Bd-inf-tadpoles-per-infpondpatch
Bd-inf-tadpoles-per-infpondpatch
0
10
1.0
1
1
NIL
HORIZONTAL

MONITOR
702
66
844
111
Tadpole population size
count tadpoles
17
1
11

PLOT
700
118
934
277
Sporangia per infected tadpole
Sporangia/host
No. of hosts
0.0
9000.0
0.0
50.0
false
false
"" ""
PENS
"default" 100.0 1 -16777216 true "" "histogram [ spn ] of tadpoles with [bd = 1]"

MONITOR
947
276
1018
321
Zoospores
sum  [ zsp ] of patches
17
1
11

MONITOR
937
119
1027
164
Total sporangia
sum [ spn ] of tadpoles with [ bd = 1 ]
17
1
11

MONITOR
851
65
967
110
Infected tadpoles
count tadpoles with [ bd = 1 ]
17
1
11

PLOT
1045
281
1245
431
total-zsp-density
days
Zoospores
0.0
90.0
0.0
600000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-zsp-density"

SLIDER
418
540
590
573
v-coverage
v-coverage
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
236
540
408
573
v-efficacy
v-efficacy
0
1
1.0
0.1
1
NIL
HORIZONTAL

MONITOR
764
14
892
59
NIL
round lambda-zsp
17
1
11

PLOT
1041
122
1241
272
per capita sporangia
day
mean number of sporangia
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"pen-1" 1.0 0 -13345367 true "" "if any? tadpoles [ plot ( sum [ spn ] of tadpoles) / count tadpoles ]"
"pen-2" 1.0 0 -8053223 true "" "plot (sum [ spn ] of metamorphs) / (count metamorphs + 1)"

SLIDER
15
290
187
323
inf-ponds
inf-ponds
0
1
1.0
1
1
NIL
HORIZONTAL

SWITCH
15
95
133
128
SimplePond
SimplePond
1
1
-1000

MONITOR
977
66
1138
111
Metamorph population size
count metamorphs
17
1
11

MONITOR
1246
124
1378
169
Infected metamorphs
count metamorphs with [ bd = 1 ]
17
1
11

MONITOR
941
171
1024
216
spn tadpoles
sum [ spn ] of tadpoles
17
1
11

MONITOR
944
223
1026
268
spn meta
round sum [ spn ] of metamorphs
17
1
11

SLIDER
14
173
186
206
birth_pulses
birth_pulses
1
3
1.0
1
1
NIL
HORIZONTAL

PLOT
701
284
936
449
Sporangia per infected metamorph
Sporangia/host
No. of hosts
0.0
600.0
0.0
20.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ spn ] of metamorphs with [bd = 1]"

PLOT
1261
436
1461
586
pz0 per metamorph
pz0 per meta
no. metamorphs
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ pz0 ] of metamorphs"

PLOT
1265
282
1465
432
pz0 per tadpole
pz0 per tadpole
no. tadpoles
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ pz0 ] of tadpoles"

PLOT
1049
439
1249
589
Zsp Count
day
zoospores
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"zsp" 1.0 0 -16777216 true "" "plot sum [ zsp ] of patches"

SLIDER
18
481
190
514
t-movement
t-movement
0
1
0.25
0.25
1
NIL
HORIZONTAL

SLIDER
19
522
191
555
m-land
m-land
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
15
413
187
446
meta-mort
meta-mort
0
0.09
0.02
0.01
1
NIL
HORIZONTAL

SLIDER
14
372
186
405
tad-mort
tad-mort
0
0.06
0.06
0.1
1
NIL
HORIZONTAL

MONITOR
39
564
149
609
% metas on land
count metamorphs with [on-land = 1] / count metamorphs
2
1
11

MONITOR
1073
11
1242
56
prop. of deaths due to Bd
bd-mortality / (bd-mortality + baseline-mortality)
2
1
11

MONITOR
899
10
1068
55
Number of perimeter patches
count patches with [pp = 1]
17
1
11

MONITOR
1249
12
1376
57
NIL
num-metamorphosis
17
1
11

MONITOR
1247
177
1339
222
NIL
bd-mortality
17
1
11

MONITOR
1246
227
1372
272
NIL
baseline-mortality
17
1
11

SLIDER
353
386
525
419
baseline_smax
baseline_smax
0
1000000
562.0
1
1
NIL
HORIZONTAL

SLIDER
14
137
186
170
last-day
last-day
0
250
90.0
1
1
NIL
HORIZONTAL

SLIDER
352
460
524
493
baseline_est
baseline_est
0
1
0.25
0.01
1
NIL
HORIZONTAL

SLIDER
352
423
536
456
baseline_spn_clearance
baseline_spn_clearance
0
1
0.2
1
1
NIL
HORIZONTAL

SLIDER
538
422
642
455
c_clear
c_clear
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
538
461
644
494
c_est
c_est
-10
10
0.0
1
1
NIL
HORIZONTAL

SLIDER
538
385
641
418
c_smax
c_smax
0
5
0.0
1
1
NIL
HORIZONTAL

SLIDER
351
498
523
531
baseline_shedding
baseline_shedding
0
100
17.8
1
1
NIL
HORIZONTAL

SLIDER
537
499
644
532
c_shedding
c_shedding
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
600
540
772
573
relative_variation
relative_variation
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
419
581
592
614
vaccination-day
vaccination-day
0
90
0.0
1
1
NIL
HORIZONTAL

SLIDER
235
581
408
614
natural_imm_efficacy
natural_imm_efficacy
0
1
0.0
0.1
1
NIL
HORIZONTAL

MONITOR
1146
65
1317
110
realized vaccine coverage
realized-coverage
17
1
11

PLOT
803
454
1003
604
total prevalence
day
prev
0.0
90.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [bd = 1] / (count turtles)"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

frog top
true
0
Polygon -7500403 true true 146 18 135 30 119 42 105 90 90 150 105 195 135 225 165 225 195 195 210 150 195 90 180 41 165 30 155 18
Polygon -7500403 true true 91 176 67 148 70 121 66 119 61 133 59 111 53 111 52 131 47 115 42 120 46 146 55 187 80 237 106 269 116 268 114 214 131 222
Polygon -7500403 true true 185 62 234 84 223 51 226 48 234 61 235 38 240 38 243 60 252 46 255 49 244 95 188 92
Polygon -7500403 true true 115 62 66 84 77 51 74 48 66 61 65 38 60 38 57 60 48 46 45 49 56 95 112 92
Polygon -7500403 true true 200 186 233 148 230 121 234 119 239 133 241 111 247 111 248 131 253 115 258 120 254 146 245 187 220 237 194 269 184 268 186 214 169 222
Circle -16777216 true false 157 38 18
Circle -16777216 true false 125 38 18

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="no_vaccination" repetitions="250" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="c_clear_vs_coverage" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="c_est_vs_coverage" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="c_smax_vs_coverage" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="c_shedding_vs_coverage" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="harm_c_shedding_vs_coverage" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="harm_c_smax_vs_coverage" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="harm_c_est_vs_coverage" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="harm_c_clear_vs_coverage" repetitions="25" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tolerance_vs_establishment" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tolerance_vs_shedding" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
      <value value="-0.11"/>
      <value value="-0.223"/>
      <value value="-0.357"/>
      <value value="-0.511"/>
      <value value="-0.693"/>
      <value value="-0.916"/>
      <value value="-1.204"/>
      <value value="-1.609"/>
      <value value="-2.303"/>
      <value value="-16.118"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="tolerance_vs_clearance" repetitions="25" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="90"/>
    <metric>count metamorphs</metric>
    <metric>prop_deaths_due_to_bd</metric>
    <metric>avg_spn_inten_metas</metric>
    <metric>median_spn_inten_metas</metric>
    <metric>metas_prev</metric>
    <metric>aggregation_inten_metas</metric>
    <metric>realized-coverage</metric>
    <metric>total-zsp-density</metric>
    <enumeratedValueSet variable="SimplePond">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="last-day">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="birth_pulses">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ini-tadpoles-per-pondpatch">
      <value value="500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Bd-inf-tadpoles-per-infpondpatch">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="inf-ponds">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tad-mort">
      <value value="0.06"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="meta-mort">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="t-movement">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m-land">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_smax">
      <value value="562"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_spn_clearance">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_est">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="baseline_shedding">
      <value value="17.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-efficacy">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="v-coverage">
      <value value="0.75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-day">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="relative_variation">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="natural_imm_efficacy">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_smax">
      <value value="0"/>
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_clear">
      <value value="0"/>
      <value value="0.095"/>
      <value value="0.182"/>
      <value value="0.262"/>
      <value value="0.336"/>
      <value value="0.405"/>
      <value value="0.47"/>
      <value value="0.531"/>
      <value value="0.588"/>
      <value value="0.642"/>
      <value value="0.693"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_est">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="c_shedding">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
