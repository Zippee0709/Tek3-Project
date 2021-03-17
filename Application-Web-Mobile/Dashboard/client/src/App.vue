<template>
  <div id="app">
    <div class="ui inverted menu" id="navBar">
        <router-link class="item" :class="{active: views.home}" @click.native="homeSelected()" to="/"><i class="home icon"></i>Home</router-link>
        <router-link class="item" :class="{active: views.about}" @click.native="aboutSelected()" to="/about"><i class="mail icon"></i>About</router-link> |
        <div class="right menu">
        <router-link v-if="!isConnected" class="item" to="/login" @click.native="loginSelected()"><i class="user icon"></i>Login</router-link>
        <div @click="disconnect()" v-else class="item button"><i class="power off icon"/>Disconnect</div>
        </div>
    </div>
    <img src="https://imgur.com/n8DqWuCl.png"/>
    <div id="view">
      <router-view/>
    </div>
  </div>
</template>

<script>
import { mapState } from 'vuex'; 
export default {
  name: "App",
  data() {
    return {
      views: {
        home: true,
        about: false,
      },
    };
  },
  methods: {
    homeSelected() {
      this.views.home = true;
      this.views.about = false;
    },
    aboutSelected() {
      this.views.home = false;
      this.views.about = true;      
    },
    loginSelected() {
      this.views.home = false;
      this.views.about = false;
    },
    disconnect() {
        this.$store.commit('logout');
        this.$router.push({name: 'Login'});
    }
  },
  computed: {
    ...mapState(['isConnected'])
  }};
</script>

<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
}
#view {
  padding: 0px 2.5%;
}
</style>
