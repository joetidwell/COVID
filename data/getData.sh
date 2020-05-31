#!/bin/bash

curl -X GET "https://services.arcgis.com/g1fRTDLeMgspWrYp/arcgis/rest/services/vDateCOVID19_Tracker_Public/FeatureServer/0/query?where=1%3D1&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&sqlFormat=none&f=pjson&token=" > SA.json
curl -X GET "https://covidtracking.com/api/v1/states/daily.csv" > daily.csv
wget -O countries.csv 'http://opendata.ecdc.europa.eu/covid19/casedistribution/csv/'



