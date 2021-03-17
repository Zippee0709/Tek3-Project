<template>
  <div class="ui cards" id="allCard">
    <div class="card">
      <div class="content">
        <div class="header">Gmail Widget</div>
        <div class="description">Gmail Thing</div>
      </div>
      <div class="ui selection dropdown">
        <input type="hidden" name="gender" />
        <i class="dropdown icon"></i>
        <div class="default text">Select Label</div>
        <div class="menu">
          <div
            class="item"
            v-for="label in labels"
            :key="label.id"
            :data-value="label.id"
          >
            {{ label.name }}
          </div>
        </div>
      </div>
      <!-- <select name="labels" multiple="" class="ui fluid dropdown">
        <option value="">Select Labels</option>
        <option v-for="label in labels" :value="label.id" :key="label.id">
          {{ label.name }}
        </option>
      </select> -->
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
  name: "GmailWidgetSelection",
  data() {
    return {
      labels: [],
    };
  },
  async mounted() {
    $(document).ready(function () {
      $(".ui.dropdown").dropdown();
    }); // NOTE: don't remove or the dropdown from semantic ui won't work;

    const res = await api.getGmailLabels(this.$store.getters.getUserId);
    if (res.data.error) {
      console.error("ERROR MOUTNER GETGMAILLABEL");
    }
    this.labels = res.data.result;
  },
  methods: {
    async addWidget() {
      const selected = $(".selection.dropdown").dropdown("get value");
      const res = await api.addUserWidget(this.$store.getters.getUserId, 4, selected);
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
