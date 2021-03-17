<template>
  <div class="ui card" id="card" v-if="status">
    <div class="content">
      <CloseButton :widgetInstanceId="widgetInstanceId"/>
      <div class="header">News about {{ title }}</div>
      <div class="ui relaxed divided list" id="content">
        <div class="item" v-for="data in ok" :key="data._id">
          <i class="large newspaper middle aligned icon"></i>
          <div class="content">
            <a class="header" :href="data.web_url" target="_blank">{{data.headline.main}}</a>
            <div class="meta">{{data.pub_date}}</div>
            <div class="description">{{data.snippet}}</div>
            <br/>
            <div class="description">{{data.lead_paragraph}}</div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script src="https://cdnjs.cloudflare.com/ajax/libs/element-ui/2.14.1/index.js"></script>

<script>
import CloseButton from "./CloseButton.vue"
let api = require("../api/DashboardAPI");
export default {
  name: "GmailWidgetSelection",
  props: ["data", "title", "widgetInstanceId"],
  components:{
      CloseButton,
  },
  data() {
    return {        
        status: true,
        ok: this.data.data.response.docs,
    };
  },
  methods: {
  },
};
</script>

<style scoped>
#allCard {
  padding-left: 1%;
  padding-right: 1%;
}
#card {
  width: 100%;
}
#content {
  height: 500px;
  overflow: scroll;
}
</style>
