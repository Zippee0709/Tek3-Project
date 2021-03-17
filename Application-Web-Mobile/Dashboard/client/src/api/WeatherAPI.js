import axios from 'axios'
const currentWeatherURL = "https://api.openweathermap.org/data/2.5/find?q="
const hourlyWeatherURL = "https://api.openweathermap.org/data/2.5/forecast?q="
const dailyWeatherURL = "https://api.openweathermap.org/data/2.5/onecall?"
const exclude = "&exclude=current,minutely,hourly"
const weatherOption = "&units=metric"
const weatherAPIKEY = "&appid=Your api key"
const weatherAPIKEY2 = "&appid=Your api key"


export async function weatherByCity(city)
{
    const url = currentWeatherURL + city + weatherOption + weatherAPIKEY2
    const res = await axios.get(url);
    if (res.status == 500) {
        console.error("OULA VASHE")
    }
    if (res.status !== 200) {
        console.error("Error with the test API");
        return null;
    }
    return res;
}

export async function hourlyWeatherByCity(city)
{
    const url = hourlyWeatherURL + city + weatherOption + weatherAPIKEY2
    const res = await axios.get(url);
    if (res.status == 500) {
        console.error("OULA VASHE")
    }
    if (res.status !== 200) {
        console.error("Error with the test API");
        return null;
    }
    return res;
}

export async function dailyWeatherByCity(lat, lon)
{
    const url = dailyWeatherURL + "lat=" + lat + "&lon=" + lon + weatherOption + exclude + weatherAPIKEY2
    const res = await axios.get(url);
    if (res.status == 500) {
    }
    if (res.status !== 200) {
        console.error("Error with the test API");
        return null;
    }
    return res;
}
