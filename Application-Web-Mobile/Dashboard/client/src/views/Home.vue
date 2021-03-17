<template>
  <div class="home">
    <h1 class="ui center aligned header">
      <div class="content">Home</div>
    </h1>
    <p>Welcome {{ userFirstname }}</p>
    <div class="two ui buttons">
      <button @click="redirectServiceList" class="ui button">
        <i class="settings icon"></i>Add Services
      </button>
      <button @click="redirectWidgetList" class="ui button">
        <i class="add icon"></i>Add Widgets
      </button>
    </div>
    <div v-if="isLoading" class="ui segment" id="loader">
      <div class="ui active dimmer">
        <div class="ui large text loader">Loading</div>
      </div>
    </div>
    <div v-else class="ui segment">
      <div class="ui three column grid" id="allCard">
        <WeatherCardVue
          v-for="item in userCurrentWeatherWidgets"
          v-bind:key="item.user_widget_instance_id"
          :city="item.parameters"
          :widgetInstanceId="item.user_widget_instance_id"
        />
      </div>
      <div class="ui two column grid" id="allCard">
        <AnimalWidgetVue
          v-for="item in userAnimalWidgets"
          v-bind:key="item.user_widget_instance_id"
          :animal="item.parameters"
          :widgetInstanceId="item.user_widget_instance_id"
        />
      </div>

      <div class="ui grid" id="allCard">
        <div class="column">
          <WeatherHourForecastVue
            v-for="item in userHourlyWeatherWidgets"
            v-bind:key="item.user_widget_instance_id"
            :city="item.parameters"
            :widgetInstanceId="item.user_widget_instance_id"
          />
        </div>
      </div>
      <div class="ui grid" id="allCard">
        <div class="column">
           <GmailWidgetHome v-for="data in gmailData" 
           :key="data.widgetInstanceId" 
           :data="data.data" 
           :widgetInstanceId="data.widgetInstanceId"/>
        </div>
      </div>
      <div class="ui grid" id="allCard">
        <div class="column">
          <NewsWidgetHome
            v-for="data in newsData"
            :key="data.widgetInstanceId"
            :data="data.data"
            :title="data.title"
            :widgetInstanceId="data.widgetInstanceId"
          />
        </div>
      </div>
      <div class="ui one column grid" id="allCard">
        <div class="column">
          <br />
          <WeatherDailyForecastVue
            v-for="item in userDailyWeatherWidgets"
            v-bind:key="item.user_widget_instance_id"
            :city="item.parameters"
            :widgetInstanceId="item.user_widget_instance_id"
          />
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { mapState } from "vuex";
import WeatherCardVue from "../components/WeatherCard.vue";
import WeatherHourForecastVue from "../components/WeatherHourForecast.vue";
import WeatherDailyForecastVue from "../components/WeatherDailyForecast.vue";
import AnimalWidgetVue from "../components/AnimalWidgetViewer.vue";
import GmailWidgetHome from "../components/GmailWidgetHome.vue";
import NewsWidgetHome from "../components/NewsWidgetHome";
let newsApi = require("../api/NewYorkTimeAPI");
let api = require("../api/DashboardAPI");

export default {
  name: "home",
  data() {
    return {
      userCurrentWeatherWidgets: [],
      userHourlyWeatherWidgets: [],
      userDailyWeatherWidgets: [],
      userAnimalWidgets: [],
      newsData: [],
      gmailData: null,
      isLoading: true,
    };
  },
  components: {
    WeatherCardVue,
    WeatherHourForecastVue,
    WeatherDailyForecastVue,
    AnimalWidgetVue,
    GmailWidgetHome,
    NewsWidgetHome,
  },
  async mounted() {
    if (!this.$store.getters.getIsConnected) {
      this.$router.push({ name: "Login" });
    }
    this.gmailData = (
      await api.getGmailData(this.$store.getters.getUserId)
    ).data.result;
    const userId = this.$store.getters.getUserId;
    if (userId) {
      await this.getUserService(userId);
      await this.getUserWidget(userId);
    }
    const userWidget = this.$store.getters.getUserWidget;
    if (userWidget) {
      this.parseUserWidget(userWidget, 1, this.userCurrentWeatherWidgets);
      this.parseUserWidget(userWidget, 2, this.userHourlyWeatherWidgets);
      this.parseUserWidget(userWidget, 3, this.userDailyWeatherWidgets);
      this.parseUserWidget(userWidget, 5, this.userAnimalWidgets);
      let newsWidgets = userWidget.rows.filter((x) => x.widget_id == 6);
      for (let i = 0; i < newsWidgets.length; i++) {
        this.newsData.push({
          data: await newsApi.search(newsWidgets[i].parameters),
          title: newsWidgets[i].parameters,
          widgetInstanceId: newsWidgets[i].user_widget_instance_id
        });
      }
    }
    this.isLoading = false;
  },
  methods: {
    async getUserService(userId) {
      var res = await api.getUserService(userId);
      if (res.data.error) {
        console.error("Error: get user Service");
        return;
      }
      this.$store.commit("setService", { userService: res.data.result });
    },
    async getUserWidget(userId) {
      var res = await api.getUserWidget(userId);
      if (res.data.error) {
        console.error("Error: get user UserWidget");
        return;
      }
      this.$store.commit("setWidget", { userWidget: res.data.result });
    },
    // parseUserWidget2(userWidget, value, dest) {
    //   for (var i = 0; i < userWidget.rowCount; i++) {
    //     if (userWidget.rows[i].widget_id == value) {
    //       dest.push(userWidget.rows[i].parameters);
    //     }
    //   }
    // },

    parseUserWidget(userWidget, value, dest) {
      for (var i = 0; i < userWidget.rowCount; i++) {
        if (userWidget.rows[i].widget_id == value) {
          dest.push(userWidget.rows[i]);
        }
      }
    },
    redirectServiceList() {
      this.$router.push({ path: "/serviceList" });
    },
    redirectWidgetList() {
      this.$router.push({ path: "/widgetList" });
    },
  },
  computed: {
    ...mapState({
      isConnected: "isConnected",
      userFirstname: "userFirstname",
      userMail: "userMail",
      userImage: "userImage",
      userId: "userId",
      userService: "userService",
      userWidget: "userWidget",
    }),
  },
};
</script>

<style scoped>
#menu {
  background: #2c3e50;
}
#allCard {
  padding-left: 1%;
  padding-right: 1%;
}
#loader {
  padding-bottom: 100%;
}
</style>