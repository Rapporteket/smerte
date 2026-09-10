#' Lag kodebok for kategoriske variabler i smerteregisteret.
#'
#' @description
#' Trekker ut verditekst for kategoriske variabler fra 'text'-tabellen i
#' databasen. Utdata lagres på format som er forenelig med 'rapwhale::kb_fyll()'
#' og har variablene `tabell`, `variabel_id`, `verdi` og `verditekst`.
#'
#' Vær obs på at du kobler mot riktig tabell da jeg ikke har sjekket om alle
#' kategoriske variabler har samme verdi-verditekst kombinasjon på tvers av skjema.
#'
#' @returns
#' Returnerer en tibble med kategoriske variabler for alle tabeller oppgitt i
#' 'tabeller'-objektet i funksjonen. Denne kan redigeres hvis nødvendig.
#'
#' @export
#'
#' @examples
#' kb_smerte_kategoriske = lag_smerte_kb_kategoriske()
# FIXME - Denne funksjonen må oppdateres når vi får tilgang til LISTBOXTEXTROW-tabell.
# Fungerer ikke per i dag, da text-tabell inneholder koder for rekkefølge i grensesnitt,
# ikke listeverdi for variabel.
lag_smerte_kb_kategoriske = function(
    d_text = loadRegData(registryName, query = "SELECT * FROM text")
    ) {

  # Liste med tabeller vi vil ha labels for.
  tabeller = c("CENTRE", "DEPARTMENTS", "EMP11", "EMP12",
               "EMP22", "HADS", "LOCATION", "MCE",
               "OPPIOIDOPPF", "PATEVAL", "PATIENT")

  # Labels ligger lagret i tabellen 'text' som del av en tekst-streng.
  d_text |>
    filter(str_detect(ID, "_L_"), LANGUAGEID == "no") |>
    separate_wider_regex(ID,
      patterns = c(tabell = "[A-Z][A-Z0-9]*",
                   "_",
                   variabel_id = "[A-Za-z0-9_]+",
                   "_L_",
                   verdi = "-?[0-9]{1,4}",
                   "_D"
                   )
      ) |>
    select(tabell, variabel_id, verdi, "verditekst" = TEXT) |>
    mutate(verdi = as.integer(verdi)) |>
    filter(tabell %in% tabeller) |>
    as_tibble()
}
