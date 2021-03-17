<template>
  <div class="ui cards" id="allCard">
    <div class="card">
      <div class="content">
        <div class="header">News Widget</div>
        <div class="description">Be informed</div>
      </div>
      <div class="ui input" :class="{error: error}">
        <input type="text" placeholder="What do you want to know ?" v-model="query"/>
      </div>
      <div @click="addWidget()" class="ui bottom attached button" id="test">
        <i class="add icon"></i>
        Add Widget
      </div>
    </div>
  </div>
</template>

<script src="https://code.jquery.com/jquery-3.1.1.min.js" crossorigin="anonymous"></script>
<script>
let api = require("../api/DashboardAPI");
export default {
  name: "NewsWidgetSelection",
  data() {
    return {
      query: null,
      error: false,
    };
  },
  async mounted() {},
  methods: {
    async addWidget() {
        if (this.query != null) {
            const res = await api.addUserWidget(this.$store.getters.getUserId, 6, this.query);
        } else {
            this.error = true;
        }
    },
  },
};
</script>

<style scoped>
#allCard {
  padding-left: 1%;
  padding-right: 1%;
}
</style>
