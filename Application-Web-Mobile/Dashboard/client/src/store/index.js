import Vue from 'vue'
import Vuex from 'vuex'
import createPersistedState from 'vuex-persistedstate'

Vue.use(Vuex)

export default new Vuex.Store({
    plugins: [createPersistedState({
        storage: window.sessionStorage,
    })],
    state: {
        isConnected: false,
        userId: null,
        userImage: null,
        userFirstname: null,
        userService: null,
        userWidget: null,
        userGoogleToken: null
    },
    mutations: {
        login: (state, data) => {
            state.isConnected = true;
            state.userId = data.userId;
            state.userImage = data.userImage;
            state.userFirstname = data.userFirstname;
            state.userGoogleToken = data.userGoogleToken;
        },
        logout: (state) => {
            state.isConnected = false;
            state.userId = null;
            state.userImage = null;
            state.userFirstname = null;
        },
        setService: (state, data) => {
            state.userService = data.userService;
        },
        setWidget: (state, data) => {
            state.userWidget = data.userWidget;
        },
        setGoogleToken: (state, userGoogleToken) => {
            state.userGoogleToken = userGoogleToken;
        }
    },
    actions: {

    },
    getters: {
        getUserId: state => {
            return state.userId;
        },
        getUserService: state => {
            return state.userService;
        },
        getUserWidget: state => {
            return state.userWidget;
        },
        getIsConnected: state => {
            return state.isConnected;
        }
    },
})