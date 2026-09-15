
#' Kvalitetsindikator behandlertilsyn
#'
#' @description
#' Indikatoren regner ut andel pasienter som mottar tilsyn fra to eller flere
#' behandlergrupper.
#' Dette finnes ved å telle opp antall registrerte tilsyn for
#' Lege (`AntTilsLege`), Sykepleier (`AntTilsSykPleier`),
#' Fysioterapeut (`AntTilsFysioT`), Psykolog (`AntTilsPsyk`),
#' Sosionom (`AntTilsSosio`), og Konferert med lege (`AntTilsKonfLege`).
#'
#' Funksjonen teller opp antall registrerte tilsyn for overnevnte variabler
#' og returnerer `ki_krit_nevner = TRUE/FALSE` avhengig av om pasienten skal
#' inkluderes i indikator.
#' `ki_krit_teller = TRUE/FALSE` avhengig av om det er registrert to eller flere
#' tilsyn og `ki_krit_nevner = TRUE`.
#'
#' Utdata kan videre mates inn i `rapwhale::aggreger_ki_prop()` for å beregne
#' andel for aktuelle undergrupper.
#'
#' @param d datasett som inneholder de radene vi skal beregne indikator for.
#'
#' @returns
#' Returnerer Inndatasett med følgende ekstra variabler.
#'
#' \item{ki_krit_teller}{TRUE/FALSE avhengig av om pasienten skal være med i teller.}
#' \item{ki_krit_nevner}{TRUE/FALSE avhengig av om pasienten skal være med i nevner.}
#' @export
#'
#' @examples
#' d_behtils = ki_behandlertilsyn(d)
ki_behandlertilsyn = function(d) {

  indikatorvariabler = c("AntTilsLege", "AntTilsSykPleier", "AntTilsFysioT",
                         "AntTilsPsyk", "AntTilsSosio", "AntTilsKonfLege")

  assertthat::assert_that(all(assertthat::has_name(d, indikatorvariabler)),
                          msg = paste0("'",
                                       rapwhale::kjed_ord(indikatorvariabler[!indikatorvariabler %in% names(d)], "', '", "' og '"), "' må være med i inndata."))

  d |> mutate(
    across(all_of(indikatorvariabler), \(x) replace_na(x, replace = 0)),
    ki_krit_nevner = TRUE,
    ki_krit_teller = ki_krit_nevner &
      (rowSums(pick(all_of(indikatorvariabler))) >= 2)
    )
}


