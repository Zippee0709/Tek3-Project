<template>
  <div class="ui middle aligned center aligned grid">
    <div class="column">
      <h2 class="ui header">
        <div id="serviceHeader" class="content">Services</div>
      <div class="two ui buttons">
        <button @click="redirectHome" class="ui button"><i class="home icon"></i>Home</button>
        <button @click="redirectWidgetList" class="ui button"><i class="add icon"></i>Add Widgets</button>
      </div>
      </h2>
      <div v-if="serviceInfoData">
        <Service v-for="object in serviceInfoData.rows" :key="object.service_id"
          :title="object.service_name" :imageSrc="object.service_image_url" :description="object.service_description"
          :serviceId="object.service_id"/>
      </div>
      <div @click="authorizeGoogle()" class="ui button blue ">
        ACCEPTE GOOGLE
      </div>
    </div>
  </div>
</template>

<script>
import Service from "../components/Service";
import { mapState } from 'vuex';
let api = require('../api/DashboardAPI');

export default {
  name: "ServiceList",
  components: {
    Service,
  },
  data() {
    return {
      isSamePassword: true,
      registerFail: false,
      serviceInfoData: null,
    };
  },
  mounted() {
    if (!this.$store.getters.getIsConnected) {
      this.$router.push({ name: 'Login' })
    }
    const code = this.$route.query.code;
    if (code) {
        api.saveGoogleToken(code, this.$store.getters.getUserId);
    }
    this.getServiceInfo()
    const userId = this.$store.getters.getUserId 
    if (userId)
      this.getUserService(userId)
  },
  methods: {
    async getServiceInfo() {
      const res = await api.getServiceInfo();
      if (res.data.error) {
        console.error("Error: when get service info")
        return
      }
      this.serviceInfoData = res.data.result
    },
    async getUserService (userId) {
      var res = await api.getUserService(userId);
      if (res.data.error) {
        console.error("Error: get user Service")
        return
      }
      this.$store.commit('setService', {userService: res.data.result})
    },

    redirectHome() {
      this.$router.push({ path: '/'})
    },

    redirectWidgetList() {
      this.$router.push({ path: '/widgetList'})
    },
    
    async authorizeGoogle() {
      const res = await api.getAuthorizeGoogleUri();
      if (res.data.error) {
        console.error("Error occured with aurthorize Google");
      }
      window.location = res.data.result;
    }
  },
  computed: {
    ...mapState({
      userService: 'userService',
    })
  }
};
</script>

<style scoped>
#serviceHeader {
    padding-bottom: 5em;
}
</style>