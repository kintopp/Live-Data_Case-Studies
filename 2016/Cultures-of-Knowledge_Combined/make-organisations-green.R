## =========================== Find Organisations and colour them green ====================================
## ==============================================================================


## ============= Get list of non-persons in life events and people.df ===========
## ==============================================================================

non_people_in_life_events <- life.events.df[life.events.df$Secondary.Participant.Type != "Person",]

nrow(non_people_in_life_events)

str(non_people_in_life_events)

non_people_in_life_events_emlo_ids <- unique(non_people_in_life_events$Secondary.Participant.Emlo_ID)
non_people_in_life_events_emlo_ids <- non_people_in_life_events_emlo_ids[!is.na(non_people_in_life_events_emlo_ids)]

non_people_in_people_df <- intersect(non_people_in_life_events_emlo_ids, people.df$iperson_id)


## =========================== Find those individuals also in people.df =========
## ==============================================================================


nodes <- 1:10

overlap <- 5:8

mapvalues()

mapvalues(nodes %in% overlap, from = c(TRUE, FALSE), to = c("#a1d76a","#7570b3"))
