## DEFIS DATA & TERRITOIRES, 4A SCIENCES PO ST GERMAIN EN LAYE


# Récupération des partis politiques à partir des noms des maires

library(tidyverse)

# Import données RNE
rne <- read_delim("https://www.data.gouv.fr/fr/datasets/r/d5f400de-ae3f-4966-8cb6-a85c70c6c24a", ";")

# Import données des partis politiques
  #source : résultats municipales 2020 (https://public.opendatasoft.com/explore/?sort=modified&q=Elections+municipales+2020)
tour1 <- read_delim("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/election-france-municipale-2020-premier-tour/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B", ";")
tour2 <- read_delim("https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/election-france-municipale-2020-deuxieme-tour/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B", ";")

# Import données des identifiants des communes
id_communes <- read_csv("https://www.data.gouv.fr/fr/datasets/r/863a5d6f-cc5c-40e0-a349-8a15719cb25e")


# Mise en forme données pour optimiser le match
  # RNE
rne_traite <- rne |> 
  filter(`Libellé de la fonction` == "Maire") |> 
  select(`Code de la commune`, `Libellé de la commune`:`Prénom de l'élu`) |> 
  mutate(nom_prenom = paste(`Nom de l'élu`, `Prénom de l'élu`),
         `Code de la commune` = str_pad(`Code de la commune`, 5, pad = "0"),
         cog_nom_prenom = paste(`Code de la commune`, nom_prenom, sep = "-")) |> 
  select(-c(`Prénom de l'élu`, `Nom de l'élu`))
  # RESULTATS MUNICIPALES 2020
tours <- bind_rows(tour1 |> 
                     select(`Code Officiel Commune`, `Nom Officiel Commune`, `Code Nuance`, `NOM Prénom`), 
                   tour2 |> 
                     select(`Code Officiel Commune`, `Nom Officiel Commune`, `Code Nuance`, `Nom Prénom`)) |> 
  mutate(nom_prenom = coalesce(`NOM Prénom`, `Nom Prénom`),
         cog_nom_prenom = paste(`Code Officiel Commune`, nom_prenom, sep = "-")) |> 
  select(-c(`NOM Prénom`, `Nom Prénom`)) |> 
  distinct()

# Jointure : ajout des partis po au RNE
rne_enrichi <- rne_traite |> 
  left_join(tours |> select(`Code Nuance`, cog_nom_prenom), 
            by = "cog_nom_prenom") |> 
  mutate(`Code Nuance` = paste0(unique(na.omit(`Code Nuance`)), collapse = ","), .by = cog_nom_prenom) |> 
  distinct() |> 
  select(-cog_nom_prenom) |> 
  mutate_all(na_if, "") |> 
  # familles de nuance basées sur legifrance (https://www.legifrance.gouv.fr/download/pdf/circ?id=44929)
  mutate(famille_nuance = case_when(`Code Nuance` %in% c("NC", "LNC") ~ "Non classé",
                                    str_extract(`Code Nuance`, "^[^,]+") == "LEXG" ~ "Extrême gauche",
                                    str_extract(`Code Nuance`, "^[^,]+") %in% c("LCOM", "LFI", "LSOC", "LRDG", "LDVG", "LUG", "LVEC") ~ "Gauche",
                                    str_extract(`Code Nuance`, "^[^,]+") %in% c("LECO", "LDIV", "LREG", "LGJ") ~ "Courants politiques divers",
                                    str_extract(`Code Nuance`, "^[^,]+") %in% c("LREM", "LMDM", "LUDI", "LUC", "LDVC") ~ "Centre",
                                    str_extract(`Code Nuance`, "^[^,]+") %in% c("LLR", "LUD", "LDVD", "LDLF") ~ "Droite",
                                    str_extract(`Code Nuance`, "^[^,]+") %in% c("LRN", "LEXD") ~ "Extrême droite",
                                    .default = NA_character_)) |> 
  # ajout du SIREN pour préparer le match avec les dépenses culturelles
  left_join(id_communes |> 
              filter(row_number() == 1, .by = COG) |> #10 communes avec plusieurs SIREN, on garde le 1er
              select(SIREN, COG), 
            by = c("Code de la commune" = "COG")) |> 
  # renommage des colonnes
  rename(cog_commune = `Code de la commune`,
         nom_commune = `Libellé de la commune`,
         siren_commune = SIREN,
         nom_prenom_maire = nom_prenom,
         nuance_politique = `Code Nuance`) |> 
  relocate(nom_commune, cog_commune, siren_commune) |> 
  arrange(cog_commune)

# Export
rio::export(rne_enrichi, "2024/defis4A_SciencesPo/RNE_enrichi_couleur_politique.csv")




#----- BROUILLON, RECHERCHE SQL WIKIDATA

library(WikidataQueryServiceR)

# Puisque les chefs ne sont pas tous à jour on récupère une liste générale de tous les politiciens français vivants et leur(s) parti(s) associé(s)
wikidata_partis_po <- query_wikidata('SELECT DISTINCT ?chefLabel ?partiLabel
WHERE {
  ?chef wdt:P31 wd:Q5 .    #tous les êtres humains
  ?chef wdt:P106 wd:Q82955 .    #on limite à ceux qui sont politiciens 
  ?chef wdt:P27 wd:Q142  .    #on limite aux pers françaises
  ?chef wdt:P102 ?parti .     #on recup le parti
  ?chef wdt:P569 ?date .   #on recup la date de naissance
  FILTER(YEAR(?date) > 1920).    #on trie pour ne garder que les politiciens "actuels" (moins de 100 ans)
  SERVICE wikibase:label { bd:serviceParam wikibase:language "fr". }
}
ORDER BY ?chefLabel ')

# Matcher par le nom de l'élu 
rne_enrichi <- rne |> 
  mutate(chef_upper = stringi::stri_trans_general(str = gsub("-", " ", toupper(`Prenom.Nom.élu`)), id = "Latin-ASCII")) |> 
  left_join(wikidata_partis_po |> 
              mutate(chef_upper = stringi::stri_trans_general(str = gsub("-", " ", toupper(chefLabel)), id = "Latin-ASCII")))

# Export des politiciens avec leur couleur politique
rio::export(wikidata_partis_po, "~/Downloads/wikidata_partis_politiques.csv")


