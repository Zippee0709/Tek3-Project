<template>
  <div id="login">
    <div class="ui middle aligned center aligned grid">
      <div class="column">
        <h2 class="ui image header">
          <div class="content">Login</div>
        </h2>
        <form class="ui large form">
          <div class="ui stacked secondary segment">
            <div class="field" :class="{error: !isFilled.email}">
              <div class="ui left icon input">
                <i class="user icon"></i>
                <input type="text" name="email" placeholder="E-mail address" v-model="input.email"/>
              </div>
            </div>
            <div class="field" :class="{error: !isFilled.password}">
              <div class="ui left icon input">
                <i class="lock icon"></i>
                <input type="password" name="password" placeholder="Password" v-model="input.password"/>
              </div>
            </div>
            <div class="ui red message" v-if="loginFail">Login Failed</div>
            <div @click="login()" class="ui fluid large teal submit button">Login</div>
          </div>
        </form>
        <br/>
        <button v-google-signin-button="clientId" class="ui google blue button google-signin-button">
          <i class="google icon"></i>
          Login with Google
        </button>
        <div class="ui message">
          New to us?
          <router-link to="/register">Register</router-link>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
let api = require('../api/DashboardAPI');
import GoogleSignInButton from 'vue-google-signin-button-directive';
import { mapState } from 'vuex'; 
export default {
  name: "Login",
  directives: {
    GoogleSignInButton
  },
  data() {
    return {
      clientId: '286981506129-n9p6i80kcmua04511696jek8cd8i9nmh.apps.googleusercontent.com',
      input: {
        email: null,
        password: null,
      },
      isFilled: {
        email: true,
        password: true
      },
      loginFail: false
    };
  },
  methods: {
    async OnGoogleAuthSuccess(idToken) {
      const res = await api.loginGoogle(idToken);
      this.$store.commit('login', {userId: res.data.result.user_id,
        userFirstname: res.data.result.user_firstname, userMail: res.data.result.user_mail,
        userImage: res.data.result.user_image,
        userGoogleToken: res.data.result.user_google_token});
      this.$router.push({name: "Home"});
    },
    OnGoogleAuthFail (error) {
      console.error(error)
    },
    async login() {
      this.isFilled.email = true;
      this.isFilled.password = true;
      if (this.input.email == null) {
        this.isFilled.email = false;
      }
      if (this.input.password == null) {
        this.isFilled.password = false;
      }
      if (this.isFilled.email && this.isFilled.password) {
        const res = await api.login(this.input);
        if (res.data.error) {
          this.loginFail = true;
          return;
        }
        this.$store.commit('login', {userId: res.data.result.user_id,
          userFirstname: res.data.result.user_firstname, userMail: res.data.result.user_mail,
          userImage: res.data.result.user_image,
          userGoogleToken: res.data.result.user_google_token});
        this.$router.push({ name: 'Home' });
      }
    },
  },
  computed: {
    ...mapState(['isConnected'])
  }
};
</script>

<style scoped>
#login {
  width: 500px;
  border: 1px solid #cccccc;
  background-color: #ffffff;
  margin: auto;
  margin-top: 200px;
  padding: 20px;
}
</style>