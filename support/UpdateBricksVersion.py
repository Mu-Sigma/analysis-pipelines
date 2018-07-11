# Title : Update brick version 
# Created : May 15, 2015
# Author : Arushi Khattri
# Version : 17.05.01
# This code is used to read version from a file and update it automatically.

import json
from datetime import date

#Read the file and get the current version
version_file_json = json.load(open("Description.json","r"))
version_from_file = version_file_json['version'].split(".")

#Check the current year and month, update the version accordingly
if version_from_file[0] == str(date.today().year%100) and version_from_file[1] == str(date.today().month).zfill(2):
	version_count = int(version_from_file[2])
	updated_version = str(date.today().year%100)+"."+str(date.today().month).zfill(2)+"."+str(version_count+1).zfill(2)
else:
	updated_version = str(date.today().year%100)+"."+str(date.today().month).zfill(2)+".01"

version_file_json['version'] = updated_version

#Write the updated version
json.dump(version_file_json, open("Description.json", "w"))