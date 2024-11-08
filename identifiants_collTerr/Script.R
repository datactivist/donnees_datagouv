########### EXTRACTION IDENTIFIANTS COLLECTIVITES ET EPCI - DATACTIVIST ########### 

library(tidyverse)



#----------------------------------------- REGIONS


# Import comptes consolidés - data OFGL
OFGL_regions <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-regions-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.agregat=true&refine.agregat=D%C3%A9penses+totales&refine.exer=2020&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_regions <- OFGL_regions %>% select(`Code Insee 2021 Région`:`Code Siren Collectivité`)

# On les renomme
OFGL_regions <- OFGL_regions %>% rename(COG = `Code Insee 2021 Région`,
                                        nom = `Nom 2021 Région`,
                                        type = `Catégorie`,
                                        SIREN = `Code Siren Collectivité`)
    
# On réordonne 
OFGL_regions <- OFGL_regions %>% select(nom, SIREN, COG, type)

# On met au bon format les variables 
OFGL_regions <- OFGL_regions %>% mutate(SIREN = as.character(SIREN),
                                        COG = as.character(COG))

# On trie les observations avec COG par ordre croissant
OFGL_regions <- OFGL_regions %>% arrange(COG)





#----------------------------------------- DEPARTEMENTS


# Import comptes consolidés - data OFGL
OFGL_departements <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-departements-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_tranche_population=true&disjunctive.dep_name=true&disjunctive.agregat=true&refine.exer=2020&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_departements <- OFGL_departements %>% select(`Code Insee 2021 Région`, `Code Insee 2021 Département`:`Code Siren Collectivité`)

# On les renomme
OFGL_departements <- OFGL_departements %>% rename(COG = `Code Insee 2021 Département`,
                                                  nom = `Nom 2021 Département`,
                                                  type = `Catégorie`,
                                                  code_region = `Code Insee 2021 Région`,
                                                  SIREN = `Code Siren Collectivité`)

# On réordonne 
OFGL_departements <- OFGL_departements %>% select(nom, SIREN, COG, type, code_region)

# Modification des variables 
OFGL_departements <- OFGL_departements %>% mutate(type = "DEP", # on modifie le type "DEPT" par "DEP"
                                                  SIREN = as.character(SIREN), # variables au bon format
                                                  code_region = as.character(sprintf("%02d", code_region)), # ajout d'un "0" pour les COG de 1 à 9
                                                  COG_3digits = str_pad(COG, 3, pad = "0")) %>% # COG avec 3 digits pour permettre plus de jointures
    select(nom:type,COG_3digits, code_region)

# Ajout de la Corse (pas présente dans les données) + Haut et Bas Rhin (nom et COG mal renseignés)
OFGL_departements <- OFGL_departements %>% add_row(nom = c("Corse-du-Sud","Haute-Corse", "Bas-Rhin", "Haut-Rhin"), 
                                                   SIREN = c("222000028","222000036", "226700011", "226800019"), 
                                                   COG = c("2A","2B", "67", "68"), 
                                                   type = c("DEP","DEP", "DEP","DEP"), 
                                                   code_region = c("94","94", "44", "44"), 
                                                   COG_3digits = c("02A","02B", "067", "068"))

# Modification du département "Alsace" mal renseigné et suppression "Métropole de Lyon" présente dans les EPCI
OFGL_departements <- OFGL_departements %>% filter(COG != "67A", COG != "691") %>% arrange(COG)




#----------------------------------------- COMMUNES


# Import jeu des comptes consolidés - data OFGL
OFGL_communes <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-communes-consolidee/download/?format=csv&disjunctive.reg_name=true&disjunctive.dep_name=true&disjunctive.epci_name=true&disjunctive.tranche_population=true&disjunctive.tranche_revenu_imposable_par_habitant=true&disjunctive.com_name=true&disjunctive.agregat=true&refine.exer=2020&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_communes <- OFGL_communes %>% select(`Code Insee 2021 Région`, `Code Insee 2021 Département`, `Code Insee 2021 Commune`:`Code Siren Collectivité`)

# On les renomme
OFGL_communes <- OFGL_communes %>% rename(COG = `Code Insee 2021 Commune`,
                                          code_region = `Code Insee 2021 Région`,
                                          code_departement = `Code Insee 2021 Département`,
                                          nom = `Nom 2021 Commune`,
                                          type = `Catégorie`,
                                          SIREN = `Code Siren Collectivité`)

# On réordonne 
OFGL_communes <- OFGL_communes %>% select(nom, SIREN, COG, type, code_departement, code_region)

# Modification des variables 
OFGL_communes <- OFGL_communes %>% mutate(type = "COM", # on modifie le type "Commune" par "COM"
                                          SIREN = as.character(SIREN), # variable au bon format
                                          code_departement_3digits = str_pad(code_departement, 3, pad = "0")) %>% # COG avec 3 digits pour permettre plus de jointures
    select(nom:code_departement, code_departement_3digits, code_region) %>% arrange(COG)




#------------------------------------------- TOUTES COLL


# On regroupe ces 3 niveaux de collectivités pour avoir toutes les informations en une seule base
infos_coll <- rbind(OFGL_regions, 
                    OFGL_departements %>% select(nom:type), 
                    OFGL_communes %>% select(nom:type))




#----------------------------------------- INTERCO (juste SIREN et noms)


# Import comptes consolidés - data OFGL 
OFGL_interco <- read_delim("https://data.ofgl.fr/explore/dataset/ofgl-base-gfp-consolidee/download/?format=csv&disjunctive.dep_name=true&disjunctive.gfp_tranche_population=true&disjunctive.nat_juridique=true&disjunctive.mode_financement=true&disjunctive.gfp_tranche_revenu_imposable_par_habitant=true&disjunctive.epci_name=true&disjunctive.agregat=true&refine.exer=2020&refine.agregat=D%C3%A9penses+totales&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B", ";")

# On garde les champs qui nous intéressent 
OFGL_interco <- OFGL_interco %>% select(`Code Insee 2021 Région`, `Code Insee 2021 Département`, `Nature juridique 2021 abrégée`, `Code Siren 2021 EPCI`, `Nom 2021 EPCI`)

# On les renomme
OFGL_interco <- OFGL_interco %>% rename(nom = `Nom 2021 EPCI`,
                                        type = `Nature juridique 2021 abrégée`,
                                        code_region = `Code Insee 2021 Région`,
                                        code_departement = `Code Insee 2021 Département`,
                                        SIREN = `Code Siren 2021 EPCI`)

# On réordonne 
OFGL_interco <- OFGL_interco %>% select(nom, SIREN, type, code_departement, code_region)

# Modification des variables 
OFGL_interco <- OFGL_interco %>% mutate(type = str_replace_all(type, c("MET69" = "M", "MET75" = "M", "M" = "MET")),
                                        SIREN = as.character(SIREN)) %>% arrange(nom)



#------------------------------------------- EXPORT


# On exporte toutes ces bases qui aideront pour croiser des variables de différents jeux quand les noms d'organisation ne sont pas exactement les mêmes
rio::export(OFGL_regions, paste("./Data/identifiants_regions_", format(Sys.Date(), "%Y"), ".csv", sep = ""))
rio::export(OFGL_departements, paste("./Data/identifiants_departements_", format(Sys.Date(), "%Y"), ".csv", sep = ""))
rio::export(OFGL_communes, paste("./Data/identifiants_communes_", format(Sys.Date(), "%Y"), ".csv", sep = ""))
rio::export(infos_coll, paste("./Data/identifiants_collectivites_", format(Sys.Date(), "%Y"), ".csv", sep = ""))
rio::export(OFGL_interco, paste("./Data/identifiants_epci_", format(Sys.Date(), "%Y"), ".csv", sep = ""))


