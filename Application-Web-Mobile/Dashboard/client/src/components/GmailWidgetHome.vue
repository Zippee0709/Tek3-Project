<template>
  <div class="ui card" id="card" v-if="status">
    <div class="content">
      <CloseButton :widgetInstanceId="widgetInstanceId"/>
      <div class="header">Gmail List</div>
      <div class="ui relaxed divided list" id="content">
        <div class="item" v-for="msg in ok" :key="msg[0].id">
          <i class="large envelope middle aligned icon"></i>
          <div class="content">
            <a class="header">{{msg[0].payload.headers.find(x => x.name == "Subject").value}}</a>
            <div class="meta">{{msg[0].payload.headers.find(x => x.name == "From").value}}</div>
            <div class="meta">{{msg[0].payload.headers.find(x => x.name == "Date").value}}</div>
            <div class="description">{{msg[0].snippet}}</div>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script src="https://cdnjs.cloudflare.com/ajax/libs/element-ui/2.14.1/index.js"></script>

<script>
let api = require("../api/DashboardAPI");
import CloseButton from "./CloseButton.vue"

export default {
  name: "GmailWidgetSelection",
  props: ["data", "widgetInstanceId"],
  data() {
    return {   
        status: true,
        ok: this.data,
    };
  },
  components:{
    CloseButton,
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
