<template>
  <div id="register">
    <div class="ui middle aligned center aligned grid">
      <div class="column">
        <h2 class="ui image header">
          <div class="content">Register</div>
        </h2>
        <form class="ui large form">
          <div class="ui stacked secondary segment">
            <div class="field" :class="{error: !isFilled.firstName}">
              <div class="ui left icon input">
                <i class="address card icon"></i>
                <input type="text" name="firstName" placeholder="First Name" v-model="input.firstName"/>
                <div class="ui corner label">
                  <i class="asterisk icon"></i>
                </div>
              </div>
            </div>
            <div class="field" :class="{error: !isFilled.lastName}">
              <div class="ui left icon input">
                <i class="address card icon"></i>
                <input type="text" name="lastName" placeholder="Last Name" v-model="input.lastName"/>
                <div class="ui corner label">
                  <i class="asterisk icon"></i>
                </div>
              </div>
            </div>
            <div class="field" :class="{error: !isFilled.email}">
              <div class="ui left icon input">
                <i class="user icon"></i>
                <input type="text" name="email" placeholder="E-mail address" v-model="input.email"/>
                <div class="ui corner label">
                  <i class="asterisk icon"></i>
                </div>
              </div>
            </div>
            <div class="field" :class="{error: !isSamePassword || !isFilled.password}">
              <div class="ui left icon input">
                <i class="lock icon"></i>
                <input type="password" name="password" placeholder="Password" v-model="input.password"/>
                <div class="ui corner label">
                  <i class="asterisk icon"></i>
                </div>
              </div>
            </div>
            <div class="field" :class="{error: !isSamePassword || !isFilled.confirmPassword}">
              <div class="ui left icon input">
                <i class="lock icon"></i>
                <input type="password" name="confirmPassword" placeholder="Confirm Password" v-model="input.confirmPassword"/>
                <div class="ui corner label">
                  <i class="asterisk icon"></i>
                </div>
              </div>
            </div>
            <div class="ui red message" v-if="!isSamePassword">Passwords are different please check again.</div>
            <div class="ui red message" v-if="registerFail">Please complete all the fields with a *.</div>
            <div class="ui red message" v-if="accountExist">This email has already register.</div>
            <div @click="register()" class="ui fluid large teal submit button">Register</div>
          </div>
        </form>
        <br/>
        <button v-google-signin-button="clientId" class="ui google blue button google-signin-button">
          <i class="google icon"></i>
          Register with Google
        </button>
        <div class="ui message">
          <router-link to="/login">Go Back</router-link>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
let api = require('../api/DashboardAPI');
import GoogleSignInButton from 'vue-google-signin-button-directive';
export default {
  name: "Register",
  directives: {
    GoogleSignInButton
  },
  data() {
    return {
      clientId: '286981506129-n9p6i80kcmua04511696jek8cd8i9nmh.apps.googleusercontent.com',
      input: {
        email: null,
        password: null,
        confirmPassword: null,
        firstName: null,
        lastName: null,
      },
      isSamePassword: true,
      registerFail: false,
      accountExist: false,
      isFilled: {
        email: true,
        password: true,
        confirmPassword: true,
        firstName: true,
        lastName: true,
      },
    };
  },
  methods: {
    async OnGoogleAuthSuccess(idToken) {
      await api.registerGoogle(idToken);
      this.$router.push({name: "Login"});
    },
    OnGoogleAuthFail (error) {
      console.error("Error: ", error)
    },
    async register() {
      for (let value in this.isFilled) {
        this.isFilled[value] = true;
      }
      this.isSamePassword = true;
      if (this.input.password != this.input.confirmPassword) {
        this.isSamePassword = false;
      }
      if (this.input.email == null) {
        this.isFilled.email = false;
      }
      if (this.input.firstName == null) {
        this.isFilled.firstName = false;
      }
      if (this.input.lastName == null) {
        this.isFilled.lastName = false;
      }
      if (this.input.password == null) {
        this.isFilled.password = false;
      }
      if (this.input.confirmPassword == null) {
        this.isFilled.confirmPassword = false;
      }
      if (this.isFilled.email && this.isFilled.firstName && this.isFilled.lastName 
        && this.isFilled.password && this.isFilled.confirmPassword && this.isSamePassword) {
          const res = await api.register(this.input);
          if (res.data.error) {
            this.accountExist = true;
            return;
          }
          this.$router.push({ name: 'Login' })
      }
      this.registerFail = true;
    },
  },
};
</script>

<style scoped>
#register {
  width: 500px;
  border: 1px solid #cccccc;
  background-color: #ffffff;
  margin: auto;
  margin-top: 200px;
  padding: 20px;
}
</style>