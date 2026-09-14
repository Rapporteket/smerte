
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
#' og returnerer `ki_nevner = TRUE/FALSE` avhengig av om pasienten skal
#' inkluderes i indikator.
#' `ki_teller = TRUE/FALSE` avhengig av om det er registrert to eller flere
#' tilsyn og `ki_nevner = TRUE`.
#'
#' Utdata kan videre mates inn i `rapwhale::aggreger_ki_prop()` for å beregne
#' andel for aktuelle undergrupper.
#'
#' @param d datasett som inneholder de radene vi skal beregne indikator for.
#'
#' @returns
#' Returnerer Inndatasett med følgende ekstra variabler.
#'
#' \item{ki_teller}{TRUE/FALSE avhengig av om pasienten skal være med i teller.}
#' \item{ki_nevner}{TRUE/FALSE avhengig av om pasienten skal være med i nevner.}
#' @export
#'
#' @examples
#' d_behtils = ki_behandlertilsyn(d)
ki_behandlertilsyn = function(d) {

}
