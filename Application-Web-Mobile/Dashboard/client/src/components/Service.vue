<template>
  <div class="ui two column centered grid">
    <div class="eight wide column">
      <div id="card" class="ui items">
        <div class="item">
          <div class="image">
            <img :src="imageSrc"/>
          </div>
          <div class="content">
            <a class="header">{{title}}</a>
            <div class="meta">
              <span>{{description}}</span>
            </div>
            <div class="description">
              <p></p>
            </div>
            <div class="extra">{{extra}}</div>
          </div>
        </div>
      </div>
    </div>
    <div class="two wide column">
      <div v-if="buttonStatus" @click="addUserService(userId, serviceId)" class="ui right floated circular wide icon green big button"><i class="plus icon"></i>Add</div>
      <div v-else class="ui right floated circular wide icon red big button"><i class="plus icon"></i>Added</div>
    </div>
  </div>
</template>

<script>
import { mapState } from 'vuex';
let api = require('../api/DashboardAPI');

export default {
  name: "Service",
  data () {
    return {
      buttonStatus: false,
    }
  },
  mounted() {
    if (this.serviceId)
      this.buttonStatus = this.checkServiceAlreadyAdded(this.serviceId)
  },
  props: ["title", "description", "imageSrc", "extra", "serviceId"],
  methods: {
    async addUserService (userId, serviceId) {
      const res = await api.addUserService(userId, serviceId);
      if (res.data.error) {
        console.error("Error: when add service into db")
      }
      this.$store.commit('setService', {userService: res.data.result});
      this.buttonStatus = false;
    },
    checkServiceAlreadyAdded (serviceId) {
      const service = this.$store.getters.getUserService;
      if (service.rowCount == 0)
        return true
      for (var i = 0; i < service.rowCount; i++) {
        if (service.rows[i].service_id == serviceId) {
          return false
        }
      }
      return true
    },
  },
  computed: {
    ...mapState({
      userId: 'userId',
      userService: 'userService',
    })
  }
};
</script>

<style scoped>
#card {
  padding-bottom: 1em;
}
</style>