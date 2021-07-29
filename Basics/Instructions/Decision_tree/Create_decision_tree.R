rm(list = ls())

#install.packages("DiagrammeR")
library(DiagrammeR)
library(DiagrammeRsvg)
library(magrittr)
library(rsvg)

string_graph_text <- paste0(as.character("graph LR; A[Detection] --> |Watchful waiting| B(( ));
                                    A --> |Aneurism treatment| C(( )); B --> |"), 
                            "Aneurysm rupture",
                       as.character("| D(( ));"),
                       as.character("B --> |"), "Aneurysm remains stable",
                       as.character("| E(( ));"),
                       as.character("D --> |"), "Patient dies",
                       as.character("| F[A];"),
                       as.character("D --> |"), "Patient becomes disabled",
                       as.character("| G[B];"),
                       as.character("D --> |"), "Patient survives in good health",
                       as.character("| H[C];"),
                       as.character("E --> |"), "Patient anxious from aneurysm",
                       as.character("| I[D];"),
                       as.character("E --> |"), "Patient not anxious and in good health",
                       as.character("| J[E];"),
                       as.character("C --> |"), "Aneurysm is clipped",
                       as.character("| K(( ));"),
                       as.character("C --> |"), "Aneurysm is coiled",
                       as.character("| L(( ));"),
                       as.character("K --> |"), "Patient dies",
                       as.character("| M[F];"),
                       as.character("K --> |"), "Patient becomes disabled",
                       as.character("| N[G];"),
                       as.character("K --> |"), "Patient survives in good health",
                       as.character("| O[H];"),
                       as.character("L --> |"), "Patient dies",
                       as.character("| P[I];"),
                       as.character("L --> |"), "Patient becomes disabled",
                       as.character("| Q[J];"),
                       as.character("L --> |"), "Patient survives in good health",
                       as.character("| R[K]")
)

string_graph_probs_txt <- paste0(as.character("graph LR; A[Detection] --> |Watchful waiting| B(( ));
                                    A --> |Aneurism treatment| C(( )); B --> |"), 
                                 "p_Rupture",
                             as.character("| D(( ));"),
                             as.character("B --> |"), "p_Stable",
                             as.character("| E(( ));"),
                             as.character("D --> |"), "p_DeathRupture",
                             as.character("| F[A];"),
                             as.character("D --> |"), "p_DisabledRupture",
                             as.character("| G[B];"),
                             as.character("D --> |"), "p_SurvivalRupture",
                             as.character("| H[C];"),
                             as.character("E --> |"), "p_Anxious",
                             as.character("| I[D];"),
                             as.character("E --> |"), "p_NotAnxious" ,
                             as.character("| J[E];"),
                             as.character("C --> |"), "p_Clipping",
                             as.character("| K(( ));"),
                             as.character("C --> |"), "p_Coiling",
                             as.character("| L(( ));"),
                             as.character("K --> |"), "p_DeathClipping",
                             as.character("| M[F];"),
                             as.character("K --> |"), "p_DisabledClipping",
                             as.character("| N[G];"),
                             as.character("K --> |"), "p_SurvivalClipping",
                             as.character("| O[H];"),
                             as.character("L --> |"), "p_DeathCoiling",
                             as.character("| P[I];"),
                             as.character("L --> |"), "p_DisabledCoiling",
                             as.character("| Q[J];"),
                             as.character("L --> |"), "p_SurvivalCoiling",
                             as.character("| R[K];")
                             
)

DiagrammeR(string_graph_text)
DiagrammeR(string_graph_probs_txt)