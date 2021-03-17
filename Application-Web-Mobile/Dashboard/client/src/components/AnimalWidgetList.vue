<template>
  <div class="column">
    <br>
    <div class="ui fluid card">
      <div class="card">
        <br>
        <div class="content">
          <h3 class="header">{{title}}</h3>
          <div class="description">
            <li>{{description}}</li>
            <br>
            <div class="ui left icon input">
                <input type="text" v-model="input" @keyup.enter="addWidget">
                <i class="search icon"></i>
            </div>
          </div>
          <br>
          <div @click="addWidget" class="ui button"><i class="add icon"></i>Add Widget</div>
          <br><br>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  name: "animalWidgetList",
  data() {
    return {
      input: "Anything",
    }
  },
  props: {
    title: null,
    description: null,
    widgetId: null,
  },
  methods: {
    addWidget() {
      this.$emit('clicked', this.input, this.widgetId);
    },
  },
};
</script>

<style scoped>

</style>
