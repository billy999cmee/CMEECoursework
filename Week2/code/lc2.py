#!/usr/bin/env python3

"""Practical on comprehension and for loops with rainfall data"""

__appname__ = 'lc2.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# Average UK Rainfall (mm) for 1910 by month
# http://www.metoffice.gov.uk/climate/uk/datasets
rainfall = (('JAN',111.4),
            ('FEB',126.1),
            ('MAR', 49.9),
            ('APR', 95.3),
            ('MAY', 71.8),
            ('JUN', 70.2),
            ('JUL', 97.1),
            ('AUG',140.2),
            ('SEP', 27.0),
            ('OCT', 89.4),
            ('NOV',128.4),
            ('DEC',142.2),
           )

# (1) Use a list comprehension to create a list of month,rainfall tuples where
# the amount of rain was greater than 100 mm.
 
Great_rain = [month for month in rainfall if month[1]>100]

# (2) Use a list comprehension to create a list of just month names where the
# amount of rain was less than 50 mm. 

Lesser_rain = [month[0] for month in rainfall if month[1]<50]

# (3) Now do (1) and (2) using conventional loops (you can choose to do 
# this before 1 and 2 !). 

# (1)
Great_rain = []
for month in rainfall:
    if month[1]>100:
        Great_rain.append(month)
print(Great_rain)

# (2)
Lesser_rain = []
for months in rainfall:
    if months[1]<50:
        Lesser_rain.append(months[0])
print(Lesser_rain)

