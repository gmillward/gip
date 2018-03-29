#!/usr/bin/python

#############################################################
#                                                           #
# SCRIPT: date_to_daynumber                                 #
#                                                           #
#############################################################

from datetime import datetime
from datetime import date
from datetime import timedelta
import sys

#year = int(sys.argv[1])
#month = int(sys.argv[2])
#day = int(sys.argv[3])

todays_date2 = datetime.now()
jan_01_date = date(todays_date2.year, 1, 1)
todays_date = date(todays_date2.year, todays_date2.month, todays_date2.day)
delta_date = todays_date - jan_01_date

#print str(todays_date2.year)
#print str(jan_01_date)
print str(delta_date.days + 1)
