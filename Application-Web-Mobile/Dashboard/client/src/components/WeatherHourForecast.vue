<template>
  <div v-if="city && weatherData && status">
    <br />
    <div class="ui fluid top attached menu">
      <div
        @click="getWeatherInfo"
        class="ui vertical animated button"
        tabindex="0"
      >
        <div class="visible content"><i class="clock icon"></i></div>
        <div class="hidden content"><i class="refresh icon"></i></div>
      </div>
      <div v-if="currentDate" class="five ui buttons">
        <button @click="changePageStatus(0)" class="ui button">J</button>
        <button @click="changePageStatus(1)" class="ui button">J + 1</button>
        <button @click="changePageStatus(2)" class="ui button">J + 2</button>
        <button @click="changePageStatus(3)" class="ui button">J + 3</button>
        <button @click="changePageStatus(4)" class="ui button">J + 4</button>
      </div>
    </div>
    <div class="ui fluid bottom attached segment">
      <CloseButton :widgetInstanceId="widgetInstanceId"/>
      <h3 class="header">Hourly forecast</h3>
      <div class="ui grid">
        <div class="eight wide column">
          <div class="current time">
            <i class="calendar alternate outline icon"></i
            ><font>{{ currentDate }} </font> <i class="clock outline icon"></i
            ><font>{{ currentTime }}</font>
          </div>
        </div>
        <div class="eight wide column">
          <div v-if="city && weatherData" class="current location">
            <i class="location arrow icon"></i>
            <font>{{ city }},{{ weatherData.city.country }} </font>
            <i class="compass outline icon"></i>
            <a
              ><font
                >{{ weatherData.city.coord.lat }},
                {{ weatherData.city.coord.lon }}</font
              ></a
            >
          </div>
          <div v-else>Unknow Location</div>
        </div>
      </div>
      <br />
      <p>Timer: {{seconds}} </p>
      <!-- <p>{{weatherData}}</p> -->
      <div class="ui grid" v-if="weatherData">
        <div
          class="four wide column"
          v-for="object in weatherData.list"
          v-bind:key="object.dt"
        >
          <div
            class="item"
            v-if="checkDateIfIsInNext24Hours(object.dt_txt, actualPage)"
          >
            <img
              class="ui avatar image"
              v-bind:src="
                'http://openweathermap.org/img/w/' +
                object.weather[0].icon +
                '.png'
              "
            />
            <div class="content">
              <div class="header">{{ object.dt_txt }}</div>
              {{ object.main.temp }}°C | {{ object.weather[0].main }}
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
let weatherAPI = require("../api/WeatherAPI");
import CloseButton from "./CloseButton.vue"

export default {
  name: "WeatherHourForecast",
  data() {
    return {
      status: true,
      actualPage: 0,
      seconds: 0,
      limit: 10,
      currentDate: null,
      currentTime: null,
      weatherData: null,
      weekDate: [],
    };
  },
  props: ["city", "widgetInstanceId"],
  components:{
    CloseButton,
  },
  mounted: function () {
    this.createTimer();
    this.getCurrentTime();
    this.getWeatherInfo();
  },
  destroyed() {
    clearInterval(this.$timer);
  },
  methods: {
    createTimer() {
      this.$timer = setInterval(() => {
        this.seconds++;
        if (this.seconds > this.limit) {
          clearInterval(this.$timer);
          this.getWeatherInfo();
          this.seconds = 0;
          this.createTimer();
        }
      }, 1000);
    },
    async getWeatherInfo() {
      if (this.city) {
        let tmp = await weatherAPI.hourlyWeatherByCity(this.city);
        this.weatherData = tmp.data;
        this.weatherData.city.coord = this.setFormatCoordinates(
          this.weatherData.city.coord
        );
        this.getCurrentTime();
      }
    },
    changePageStatus(value) {
      this.actualPage = value;
    },
    checkDateIfIsInNext24Hours(date_txt, nbDay) {
      const tmpDate = date_txt.split(" ");
      var today = new Date();
      today = new Date(
        today.getFullYear(),
        today.getMonth(),
        today.getDate() + nbDay
      );
      var today_txt = null;
      if (today.getDate() < 10)
        today_txt =
          today.getFullYear().toString() +
          "-" +
          (today.getMonth() + 1).toString() +
          "-0" +
          today.getDate().toString();
      else
        today_txt =
          today.getFullYear().toString() +
          "-" +
          (today.getMonth() + 1).toString() +
          "-" +
          today.getDate().toString();
      if (today_txt == tmpDate[0]) return true;
      return false;
    },
    capitaliseFirstLetter(string) {
      return string.charAt(0).toUpperCase() + string.slice(1);
    },
    getCurrentTime() {
      const today = new Date();
      const date =
        today.getFullYear() +
        "-" +
        (today.getMonth() + 1) +
        "-" +
        today.getDate();

      var hours = today.getHours();
      if (hours < 10) hours = "0" + today.getHours();
      var minutes = today.getMinutes();
      if (minutes < 10) minutes = "0" + today.getMinutes();
      const time = hours + ":" + minutes;
      this.currentDate = date;
      this.currentTime = time;
    },
    setFormatCoordinates(coordinates) {
      if (coordinates.lat > 0) {
        coordinates.lat =
          (Math.round(coordinates.lat * 10000) / 10000).toString() + "°N";
      } else if (coordinates.lat < 0) {
        coordinates.lat =
          (-1 * (Math.round(coordinates.lat * 10000) / 10000)).toString() +
          "°S";
      } else {
        coordinates.lat = (
          Math.round(coordinates.lat * 10000) / 10000
        ).toString();
      }
      if (coordinates.lon > 0) {
        coordinates.lon =
          (Math.round(coordinates.lon * 10000) / 10000).toString() + "°E";
      } else if (coordinates.lon < 0) {
        coordinates.lon =
          (-1 * (Math.round(coordinates.lon * 10000) / 10000)).toString() +
          "°W";
      } else {
        coordinates.lon = (
          Math.round(coordinates.lon * 10000) / 10000
        ).toString();
      }
      return coordinates;
    },
  },
};
</script>

<style scoped>
</style>