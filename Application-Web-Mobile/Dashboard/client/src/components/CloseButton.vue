<template>
    <div class="ui fluid icon button" @click="isClicked"><i class="close icon"></i>
    </div>
</template>

<script>
let API = require("../api/DashboardAPI")

export default {
  name: "CloseButton",
  props: ["widgetInstanceId"],
  methods: {
      async isClicked () {
          this.$parent.$data.status = false;
          const res = await API.deleteUserWidget(this.widgetInstanceId)
      }
  }
};
</script>