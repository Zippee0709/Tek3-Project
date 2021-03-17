<template>
  <div class="column" v-if="weatherData && status">
    <br>
    <div class="ui fluid card">
      <div class="card">
        <div class="content">
          <CloseButton :widgetInstanceId="widgetInstanceId"/>
          <h2 class="header"><br>Current Weather</h2>
          <div class="meta">
              <i class="location arrow icon"></i>
              <font>{{city}}, {{weatherData.sys.country}}</font>
          </div>
          <div id="current weather">
            <img class="ui tiny image" v-bind:src="this.weatherIconId">
            <font size="+1">{{weatherData.main.temp}}</font>
            <font size="+1"><strong> °C</strong></font>
          </div>
          <div id="current weather description">
            <font size="+2"><strong>{{weatherData.weather[0].description}}</strong></font>
          </div>
          <br>
          <div class="temp-max-min">
            <div class="min-desc">
              <div id="min-details">
                  <i class="double angle down icon"></i>
                  <font size="+1">{{weatherData.main.temp_min}}</font>
                  <font size="+1"><strong> °C</strong></font>
              </div>
              <div id="min-summary">Min temperature</div>
            </div>
            <br>
            <div class="max-desc">
              <div id="max-details">
                  <i class="double angle up icon"></i>
                  <font size="+1">{{weatherData.main.temp_max}}</font>
                  <font size="+1"><strong> °C</strong></font>
              </div>
              <div id="max-summary">Max temperature</div>
            </div>
          </div>
          <br>
          
          <div class="current time">
            <i class="calendar alternate outline icon"></i>
            <font>{{currentDate}} </font>

            <i class="clock outline icon"></i>
            <font>{{currentTime}}</font>
          </div>

          <div class="current coordinates">
            <i class="compass outline icon"></i>
            <a><font>{{weatherData.coord.lat}}, {{weatherData.coord.lon}}</font></a>
            <p>Timer: {{seconds}} </p>
          </div>
          <br>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import CloseButton from "./CloseButton.vue"
let weatherAPI = require('../api/WeatherAPI');

export default {
  name: "WeatherCard",
  data () {
    return {
      status: true,
      seconds: 0,
      limit: 10,
      showResult: false,
      weatherData : null,
      weatherIconId : null,
      currentDate: null,
      currentTime: null,
    };
  },
  components:{
    CloseButton,
  },
  props: ["city", "widgetInstanceId"],
  mounted() {
    this.getWeatherInfo()
    this.createTimer()
  },
  destroyed() {
    clearInterval(this.$timer)      
  },
  methods: {
    createTimer() {
      this.$timer = setInterval(() => {
        this.seconds++
        if (this.seconds > this.limit) {
          clearInterval(this.$timer);
          this.getWeatherInfo();
          this.seconds = 0;
          this.createTimer()
        }
      }, 1000)
    },
    async getWeatherInfo (){
      if (this.city != "Location" && this.city) {
        let tmp = await weatherAPI.weatherByCity(this.city)
        this.weatherData = tmp.data.list[0];
        this.weatherData.weather[0].description = this.capitaliseFirstLetter(this.weatherData.weather[0].description)
        this.weatherIconId = "http://openweathermap.org/img/w/" + tmp.data.list[0].weather[0].icon + ".png";
        this.weatherData.coord = this.setFormatCoordinates(this.weatherData.coord)
        this.getCurrentTime()
      }
    },
    capitaliseFirstLetter(string) {
      return string.charAt(0).toUpperCase() + string.slice(1)
    },
    getCurrentTime() {
      const today = new Date();
      const date = today.getFullYear()+'-'+(today.getMonth()+1)+'-'+today.getDate();
      
      var hours = today.getHours()
      if (hours < 10)
        hours = "0" + today.getHours()
      var minutes = today.getMinutes()
      if (minutes < 10)
        minutes = "0" + today.getMinutes()
      const time = hours + ":" + minutes;
      this.currentDate = date;
      this.currentTime = time;
    },
    setFormatCoordinates(coordinates) {
      if (coordinates.lat > 0) {
        coordinates.lat =
          (Math.round(coordinates.lat * 10000) / 10000).toString() + '°N';
      } else if (coordinates.lat < 0) {
        coordinates.lat =
          (-1 * (Math.round(coordinates.lat * 10000) / 10000)).toString() +
          '°S';
      } else {
        coordinates.lat = (
          Math.round(coordinates.lat * 10000) / 10000
        ).toString();
      }
      if (coordinates.lon > 0) {
        coordinates.lon =
          (Math.round(coordinates.lon * 10000) / 10000).toString() + '°E';
      } else if (coordinates.lon < 0) {
        coordinates.lon =
          (-1 * (Math.round(coordinates.lon * 10000) / 10000)).toString() +
          '°W';
      } else {
        coordinates.lon = (
          Math.round(coordinates.lon * 10000) / 10000
        ).toString();
      } return coordinates
  }
  }
};
</script>

<style scoped>
</style>