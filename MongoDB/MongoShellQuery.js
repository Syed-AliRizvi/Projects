//using the right db
use fit5148_db;

//q1 Import the data (Fire data-Part 1 and Weather data-Part 1) into two different collections in MongoDB
//fire data - (commenting this out as when running on mongo this shows error)
//mongoimport --host localhost --db fit5148_db --collection fire --type csv --file C:\Users\rizvi\Documents\Monash_University\2018S1\FIT5148_Distributed_Databases_BD\Assessments\A2_FIT5148\FireData-Part1.csv --headerline
//climate data - (commenting this out as when running on mongo this shows error)
//mongoimport --host localhost --db fit5148_db --collection climate --type csv --file C:\Users\rizvi\Documents\Monash_University\2018S1\FIT5148_Distributed_Databases_BD\Assessments\A2_FIT5148\ClimateData-Part1.csv --headerline


//q2 Find climate data on 15th December 2017 
db.climate.find({'Date':/(12-15$)/}).pretty();


//q3  Find the latitude , longitude and confidence when the surface temperature (°C) was between 65 °C and 100 °C .
db.fire.find({$and:[{'Surface Temperature (Celcius)':{$gt:65}},{'Surface Temperature (Celcius)':{$lt:100}}]}, {'_id':0, 'Latitude':1,'Longitude':1,'Confidence':1}).pretty();


//q4  Find surface temperature (°C), air temperature (°C), relative humidity and maximum wind speed on 15th and 16th of December 2017.
//with unwind
//db.climate.aggregate([  {$match: {'Date':/(12-(15|16)$)/}}, { $lookup:{ from: 'fire', localField: 'Date', foreignField: 'Date', as: 'fire' } },{ $unwind : "$fire" } , {$project: {'_id':0, 'Air Temperature(Celcius)':1, 'Relative Humidity':1 , 'Max Wind Speed':1, 'fire.Surface Temperature (Celcius)':1}} ]).pretty();
//without unwind
db.climate.aggregate([  {$match: {'Date':/(12-(15|16)$)/}}, { $lookup:{ from: 'fire', localField: 'Date', foreignField: 'Date', as: 'fire' } }, {$project: {'_id':0, 'Air Temperature(Celcius)':1, 'Relative Humidity':1 , 'Max Wind Speed':1, 'fire.Surface Temperature (Celcius)':1}} ]).pretty();


//q5  Find datetime, air temperature (°C), surface temperature (°C) and confidence when the confidence is between 80 and 100.
db.fire.aggregate([  
	{$match: {$and:[{'Confidence':{$gt:80}},{'Confidence':{$lt:100}}]}}, 
	{ $lookup:{ from: 'climate', localField: 'Date', foreignField: 'Date', as: 'climate' } }, 
	{$project: {'_id':0, 'climate.Air Temperature(Celcius)':1, 'Datetime':1 , 'Confidence':1, 'Surface Temperature (Celcius)':1}} 
]).pretty();


//q6  Find datetime, air temperature (°C), surface temperature (°C) and confidence when the confidence is between 80 and 100.
db.fire.aggregate( [ {$sort: {'Surface Temperature (Celcius)':-1}}, {$limit:10} ]).pretty();


//q7. Find the number of fire in each day. You are required to only display total number of fire and the date in the output
db.fire.aggregate([  {$group : { _id : {Date:'$Date'}, count: {$sum:1}}}]).pretty();


//q8.  Find the average surface temperature (°C) for each day . You are required to only display average surface temperature (°C) and the date in the output.
db.fire.aggregate([  {$group : { _id : '$Date', avg_ST: {$avg:'$Surface Temperature (Celcius)'}}}]).pretty();