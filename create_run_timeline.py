#!/usr/bin/python

#############################################################
#                                                           #
# SCRIPT: create_run_timeline                               #
#                                                           #
#############################################################

from datetime import datetime
from datetime import date
from datetime import time
from datetime import timedelta
import math
import sys

#year = int(sys.argv[1])
#month = int(sys.argv[2])
#day = int(sys.argv[3])

start_year = 2018
start_month = 2
start_day = 17
start_hour = 3
start_minute = 0

stop_year = 2018
stop_month = 2
stop_day = 17
stop_hour = 4
stop_minute = 0

#todays_date2 = datetime.now()
#jan_01_date = date(todays_date2.year, 1, 1)
#todays_date = date(todays_date2.year, todays_date2.month, todays_date2.day)

start_datetime = datetime(start_year, start_month, start_day, start_hour, start_minute)
stop_datetime = datetime(stop_year, stop_month, stop_day, stop_hour, stop_minute)
delta_runtime = stop_datetime - start_datetime
#print delta_runtime
runtime_minutes = ((delta_runtime.days * 86400) + delta_runtime.seconds)/60

#print runtime_minutes

start_date = date(start_year, start_month, start_day)
start_time = time(start_hour, start_minute, 0)
jan_01_date = date(start_year, 1, 1)
delta_date = start_date - jan_01_date
day_number = delta_date.days + 1
#print day_number

outfile = open("GT_GIP_input_file","w")

outfile.write(('data/GT_RESTART_'+repr(start_year).zfill(4)+repr(start_month).zfill(2)+repr(start_day).zfill(2)
	+'_'+repr(start_hour).zfill(2)+repr(start_minute).zfill(2)+'\n'))
outfile.write('data/GT_RESTART\n')
outfile.write(('data/GIP_RESTART_'+repr(start_year).zfill(4)+repr(start_month).zfill(2)+repr(start_day).zfill(2)
	+'_'+repr(start_hour).zfill(2)+repr(start_minute).zfill(2)+'\n'))
outfile.write('data/GIP_RESTART\n')

outfile.write(str(runtime_minutes)+"\n")
cd = start_datetime
outstring = str(repr(0).rjust(4) + ' ' + repr(cd.year).rjust(4)  + ' ' + \
repr(cd.month).zfill(2)  + ' ' + repr(cd.day).zfill(2)  + ' ' + \
repr(cd.hour).rjust(2)  + ' ' + repr(cd.minute).rjust(2)  + ' ' + repr(day_number).rjust(3)+'\n')
outfile.write(outstring)
for x in range(1, runtime_minutes + 1):
	d = timedelta(seconds=60)
        cd = start_datetime + (x * d)
#        current_time = start_time + (x * d)
#        print x, cd.hour, cd.minute, cd.second
        ut_hours = cd.hour + (cd.minute / 60.)

  	g = 2.*math.pi/365.*(day_number - 1.0 + (ut_hours - 12.)/24.)

 	decl = 180.0/math.pi*(0.006918 - 0.399912*math.cos(g) + 0.070257*math.sin(g) - 0.006758*math.cos(2.*g) \
        + 0.000907*math.sin(2.*g) - 0.002697*math.cos(3.*g) + 0.00148*math.sin(3.*g))

	outstring = str(repr(x).rjust(4) + ' ' + repr(cd.year).rjust(4)  + ' ' + \
	repr(cd.month).zfill(2)  + ' ' + repr(cd.day).zfill(2)  + ' ' + \
	repr(cd.hour).rjust(2)  + ' ' + repr(cd.minute).rjust(2)  + ' ' + repr(day_number).rjust(3)  + ' ' + ("%.4f" % decl)+'\n')
	outfile.write(outstring)
outfile.close()
