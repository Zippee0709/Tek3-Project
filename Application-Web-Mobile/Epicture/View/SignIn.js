import React, {Component} from 'react'
import {Modal} from 'react-native'
import { getAuthorizationUserUrl } from '../API/ImgurApi'
import { WebView } from 'react-native-webview';
import { StackActions } from '@react-navigation/native'

class SignIn extends Component {
    constructor(props) {
        super(props);
        this.state = {
            url: null,
            isVisible: true
        };
        this.userInfo = {
            accessToken: null,
            refreshToken: null,
            accountUsername: null,
            accountId: null,
            signIn: false
        }
    }

    _parseUserInfo(url) {
        const getUrlInfo = url.split("state=success");
        let splitUrl = getUrlInfo[1].split("&");
        this.userInfo.accessToken = splitUrl[0].split("=")[1];
        this.userInfo.refreshToken = splitUrl[3].split("=")[1];
        this.userInfo.accountUsername = splitUrl[4].split("=")[1];
        this.userInfo.accountId = splitUrl[5].split("=")[1];
    }

    _onNavigationStateChange(navigationState) {
        if (navigationState.url.includes("access_token") && navigationState.url.includes("token_type") 
            && navigationState.url.includes("refresh_token") && navigationState.url.includes("account_username") && navigationState.url.includes("account_id"))
        {
            this.hide();
            this._parseUserInfo(navigationState.url);
            const { navigate } = this.props.navigation;
            const popAction = StackActions.pop(1);
            this.props.navigation.dispatch(popAction);
            navigate("Home", {accessToken: this.userInfo.accessToken, refreshToken: this.userInfo.refreshToken,
                accountUsername: this.userInfo.accountUsername, accountId: this.userInfo.accountId});
        }
    }
    
    hide() {
        this.setState({isVisible: false})
    }

    render() {
        const url = getAuthorizationUserUrl();
        return (
            <Modal
                animationType = {"fade"}
                visible = {this.state.isVisible}
                onRequestClose = {this.hide.bind(this)}
                transparent
            >
                <WebView
                ref={ref => (this.webview = ref)}
                onNavigationStateChange={this._onNavigationStateChange.bind(this)}
                source={{uri : url}}/> 
            </Modal>
        );
    }
}

export default SignIn;