########################################
# Miljøvariabeler for kjøring hos SKDE #
# ------------------------------------ #
# Noe av dette kan også brukes hos HV, #
# men MYSQL_-variablene må endres.     #
########################################

Sys.setenv(R_RAP_INSTANCE="QAC")
Sys.setenv(R_RAP_CONFIG_PATH=paste0(getwd(), "/dev/config"))

Sys.setenv(MYSQL_PASSWORD="root")
Sys.setenv(MYSQL_DB_LOG="db_log")
Sys.setenv(MYSQL_DB_AUTOREPORT="db_autoreport")
Sys.setenv(MYSQL_DB_STAGING="staging")
Sys.setenv(MYSQL_HOST="localhost")
Sys.setenv(MYSQL_USER="root")

Sys.setenv(FALK_EXTENDED_USER_RIGHTS='[
  {\"A\":101,\"R\":\"SC\",\"U\":0},
  {\"A\":101,\"R\":\"SC\",\"U\":100089},
  {\"A\":101,\"R\":\"SC\",\"U\":100082},
  {\"A\":101,\"R\":\"SC\",\"U\":4214288},
  {\"A\":101,\"R\":\"SC\",\"U\":4201115},
  {\"A\":101,\"R\":\"SC\",\"U\":4207789},
  {\"A\":101,\"R\":\"SC\",\"U\":705652},
  {\"A\":101,\"R\":\"SC\",\"U\":705758},
  {\"A\":101,\"R\":\"SC\",\"U\":100320},
  {\"A\":101,\"R\":\"SC\",\"U\":101719},
  {\"A\":101,\"R\":\"SC\",\"U\":4204083},
  {\"A\":101,\"R\":\"LU\",\"U\":0},
  {\"A\":101,\"R\":\"LU\",\"U\":100082}
]')

Sys.setenv(FALK_APP_ID="101")
Sys.setenv(FALK_USER_EMAIL="jesus@sky.no")
Sys.setenv(FALK_USER_FULLNAME="Helse Helsesen")

Sys.setenv(SHINYPROXY_USERNAME="sivh")
Sys.setenv(SHINYPROXY_USERGROUPS="pilot")
