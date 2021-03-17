<template>
  <div>
    <h1>Add Widgets</h1>
    <div class="two ui buttons">
      <button @click="redirectHome" class="ui button"><i class="home icon"></i>Home</button>
      <button @click="redirectServiceList" class="ui button"><i class="settings icon"></i>Add Services</button>
    </div>
    <div v-if="weatherStatus && weatherWidgetInfoData">
      <div class="ui three column grid" id="allCard">
        <weatherWidgetListVue v-for="object in weatherWidgetInfoData" v-bind:key="object.widget_id"
          :title="object.widget_name" :description="object.widget_description" :widgetId="object.widget_id" @clicked="handleClick"/>
      </div>
    </div>
    <div v-else><br><h2 class="ui center aligned icon header"><i class="circular warning icon"></i><br>Please add weather service, before add widget</h2></div>

    <div v-if="googleStatus"><gmailWidgetSelection /></div>
    <div v-else><br><h2 class="ui center aligned icon header"><i class="circular warning icon"></i><br>Please add google service, before add widget</h2></div>

    <div v-if="animalStatus && animalWidgetInfoData">
      <div class="ui three column grid" id="allCard">
        <animalWidgetListVue v-for="object in animalWidgetInfoData" v-bind:key="object.widget_id"
          :title="object.widget_name" :description="object.widget_description" :widgetId="object.widget_id" @clicked="handleClick"/>
      </div>
    </div>
    <div v-else><br><h2 class="ui center aligned icon header"><i class="circular warning icon"></i><br>Please add service, before add widget</h2></div>


    <div v-if="newsStatus"><newsWidgetSelection /></div>
    <div v-else><br><h2 class="ui center aligned icon header"><i class="circular warning icon"></i><br>Please add news service, before add widget</h2></div>
  </div>
</template>

<script>
let api = require('../api/DashboardAPI');
import weatherWidgetListVue from "../components/WeatherWidgetList"
import animalWidgetListVue from "../components/AnimalWidgetList"
import gmailWidgetSelection from "../components/GmailWidgetSelection"
import newsWidgetSelection from "../components/NewsWidgetSelection"

export default {
  name: "WidgetList",
  data() {
    return {
      googleStatus: false,
      weatherStatus: false,
      animalStatus: false,
      newsStatus: false,
      weatherWidgetInfoData: [],
      animalWidgetInfoData: [],
      googleWidgetInfoData: [],
      city: "Location",
    }
  },
  components: {
    weatherWidgetListVue,
    gmailWidgetSelection,
    animalWidgetListVue,
    newsWidgetSelection
  },
  mounted() {
    if (!this.$store.getters.getIsConnected) {
      this.$router.push({ name: 'Login' })
    }
    this.getUserService()
    this.getUserWidgetSubscribable()
  },
  methods: {
    handleClick(value, widgetId) {
      if (value && value != "Location") {
        this.addUserWidget(value, widgetId)
      }
    },
    async addUserWidget(city, widgetId) {
      const userId = this.$store.getters.getUserId
      const res = await api.addUserWidget(userId, widgetId, city);
      if (res.data.error) {
        console.error("Error: Add user widget")
        return
      }
      this.$store.commit('setWidget', {userWidget: res.data.result});
    },
    async getUserWidgetSubscribable() {
      const googleService = 1;
      const weatherService = 2;
      const animalService = 3;
      const res = await api.getUserWidgetSubscribable(this.$store.getters.getUserId);
      if (res.data.error) {
        console.error("Error: get widget info")
        return
      }
      for (let i = 0; i < res.data.result.rows.length; i++) {
        if (res.data.result.rows[i].service_id == weatherService) {
          this.weatherWidgetInfoData.push(res.data.result.rows[i]);
        } else if (res.data.result.rows[i].service_id == googleService){
          this.googleWidgetInfoData.push(res.data.result.rows[i]);
        } else if (res.data.result.rows[i].service_id == animalService){
          this.animalWidgetInfoData.push(res.data.result.rows[i]);
        }
      }
    },
    getUserService() {
      const tmp = this.$store.getters.getUserService
      for (var i = 0; i < tmp.rowCount; i++) {
        if (tmp.rows[i].service_id == 2) {
          this.weatherStatus = true
        } else if (tmp.rows[i].service_id == 1) {
          this.googleStatus = true
        } else if (tmp.rows[i].service_id == 3) {
          this.animalStatus = true
        }
        else if (tmp.rows[i].service_id == 4) {
          this.newsStatus = true;
        }
      }
    },
    redirectHome() {
      this.$router.push({ path: '/'})
    },
    redirectServiceList() {
      this.$router.push({ path: '/serviceList'})
    },
  },
};
</script>

<style scoped>
#allCard{
  padding-left: 1%;
  padding-right: 1%;
}
</style>