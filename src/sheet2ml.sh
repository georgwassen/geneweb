#!/bin/sh
camlp4 ./pa_sheet.cma pr_o.cmo -sep "\n\n" -param `head -1 $1 | sed -e 's/<comm param = //' -e 's/>//'` -impl $1
