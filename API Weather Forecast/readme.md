# API Weather Forecast 

A simple Python program using openweathermap.org API to generate current weather conditions and forecast for a zip code or city.

## How it works
User enters a zip code or city. Entry is validated and converted into query string.

Query string is passed to current_cond() which requests JSON data from http://api.openweathermap.org/data/2.5/weather? of current conditions and converts to user friendly display. 

Query string is passed to forecast() which requests JSON data from http://api.openweathermap.org/data/2.5/forecast? of 5 day/3 hour forecast and converts to user friendly display. 

User is prompted to enter a new zip code or city or quit.

## Libraries
* requests
* json
* datetime

## openweathermap.org Documentation
Current weather: https://openweathermap.org/current

Forecast: https://openweathermap.org/forecast5

## Example

![image](https://user-images.githubusercontent.com/58178574/148156181-58be0ed7-277f-4328-a180-da159ca5f2eb.png)

## Author

Michael Loos
loos.mickey@gmail.com
