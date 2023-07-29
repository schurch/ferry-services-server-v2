#!/bin/bash

curl https://scottishferryapp.com/api/services/ | jq '. | .[].status |= -99 | .[].additional_info |= null | .[].disruption_reason |= null | .[].last_updated_date |= null | .[].updated |= null | .[].vessels |= [] | .[].locations[].weather |= null'  > services.json
