## ------------------------------------------------------------------------
options(QAPI_ORG_ID  = "OrganizationId",
        QAPI_API_KEY = "abcdefghijklmnopqrstuvwxyz")

## ---- eval=FALSE---------------------------------------------------------
#  options(QAPI_ORG_ID  = "OrganizationId",
#          QAPI_API_KEY = "abcdefghijklmnopqrstuvwxyz")

## ---- eval = FALSE-------------------------------------------------------
#  qapi_connect(auth_file = "~/.qapi_credentials.R")

## ---- eval=FALSE---------------------------------------------------------
#  qapi_connect(org_id = "OrganizationID",
#               api_key = "abcdefghijklmnopqrstuvwxyz")
#  

