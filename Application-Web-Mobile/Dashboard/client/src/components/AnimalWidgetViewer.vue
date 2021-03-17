<template>
  <div class="column" id="currentCard" v-if="status">
    <br />
    <div class="ui fluid card">
      <div class="card">
        <br />
        <div class="content">
          <CloseButton :widgetInstanceId="widgetInstanceId"/>
          <h3 class="header">Random picture of : {{ animal }}</h3>
          <div class="description">
            <br />
            <img :src="'https://loremflickr.com/480/480/' + animal + '?' + componentKey"/>
          </div>
          <p>Timer: {{ seconds }}</p>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import CloseButton from "./CloseButton.vue"

export default {
  name: "animalWidgetViewer",
  data() {
    return {
      seconds: 0,
      limit: 5,
      componentKey: 0,
      status: true,
    };
  },
  props: {
    animal: null,
    description: null,
    widgetId: null,
    widgetInstanceId: null,
  },
  components:{
    CloseButton,
  },
  mounted() {
    this.createTimer();
  },
  destroyed() {
    clearInterval(this.$timer);
  },
  methods: {
    createTimer() {
      this.$timer = setInterval(() => {
        this.seconds++;
        if (this.seconds > this.limit) {
          clearInterval(this.$timer);
          this.componentKey += 1;
          this.seconds = 0;
          this.createTimer();
        }
      }, 1000);
    },
  },
};
</script>

<style scoped>
#currentCard {
  width: 100%;
}
</style>
