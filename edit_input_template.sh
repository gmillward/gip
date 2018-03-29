#!/bin/sh

#############################################################
#                                                           #
# SCRIPT: edit_input_template.sh                            #
#                                                           #
#                                                           #
#############################################################

todays_day_number=`date_to_daynumber.py`
echo $todays_day_number

sed -e "s/%DDD%/$todays_day_number/" input_template> input_file 
