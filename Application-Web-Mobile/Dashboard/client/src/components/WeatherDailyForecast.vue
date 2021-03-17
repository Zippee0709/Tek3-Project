<template>
  <div class="column" v-if="status">
    <br />
    <div class="ui fluid card">
      <div class="card">
        <div class="content">
          <CloseButton :widgetInstanceId="widgetInstanceId"/>
          <div class="description">
            <div id="openweathermap-widget-11"></div>
          </div>
            <p>Timer: {{seconds}} </p>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import CloseButton from "./CloseButton.vue"
let weatherAPI = require("../api/WeatherAPI");

export default {
  name: "WeatherMap",
  data() {
    return {
      status: true,
      actual: "Temperature",
      seconds: 0,
      limit: 10,
      currentDate: null,
      currentTime: null,
      latitude: null,
      longitude: null,
      weatherData: null,
      nextWeekData: null,
      cityId: null,
      weekDate: [],
    };
  },
  props: ["city", "widgetInstanceId"],
  mounted() {
    this.createTimer();
    this.getWeatherInfo();
  },
  destroyed() {
    clearInterval(this.$timer);
  },
  components:{
    CloseButton,
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
        let tmp = await weatherAPI.weatherByCity(this.city);
        this.weatherData = tmp.data;
        this.latitude = tmp.data.list[0].coord.lat;
        this.longitude = tmp.data.list[0].coord.lon;
        this.cityId = tmp.data.list[0].id;
        this.formatedCoordinates = this.setFormatCoordinates(
          this.weatherData.list[0].coord
        );
        if (this.cityId) this.getOpenWeatherWidget(this.cityId);
        this.getCurrentTime();
      }
    },
    getOpenWeatherWidget(cityId) {
      window.myWidgetParam ? window.myWidgetParam : (window.myWidgetParam = []);
      window.myWidgetParam.push({
        id: 11,
        cityid: cityId,
        appid: "5196ed935b72fbca6d622d130a0bfebe",
        units: "metric",
        containerid: "openweathermap-widget-11",
      });

      (function () {
        var script = document.createElement("script");
        script.async = true;
        script.charset = "utf-8";
        script.src =
          "//openweathermap.org/themes/openweathermap/assets/vendor/owm/js/weather-widget-generator.js";
        var s = document.getElementsByTagName("script")[0];
        s.parentNode.insertBefore(script, s);
      })();
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
#openweathermap-widget-11 {
  margin-left: auto;  
  margin-right: auto;  
}
</style>