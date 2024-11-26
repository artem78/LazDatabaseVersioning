#!/bin/bash

# PasDoc 0.16.0 was used
pasdoc --output docs \
    --include-creation-time \
    --visible-members public,published,automated \
    databaseversioning.pas 
