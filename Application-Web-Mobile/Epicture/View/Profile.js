import React, {Component} from 'react'
import {View, StyleSheet, TouchableOpacity, NativeModules} from 'react-native'
import {Icon} from 'react-native-elements';
import ProfileNavigator from '../Components/ProfileNavigator'
import { LogOut } from 'react-native-feather'
import { createStackNavigator } from '@react-navigation/stack';
import { StackActions, CommonActions } from '@react-navigation/native'


class ProfilePage extends Component {
    constructor(props){
        super(props);
        this.userInfo = {
            accessToken: null,
            refreshToken: null,
            accountUsername: null,
            accountId: null
        }
    }

    render() {
        return (
            <View style={styles.main_container}>
                <View style={styles.profileTop}>
                    <Icon name='account-circle' color='black' size={100} type='Entypo'/>
                </View>
                <View style={styles.profileImage}>
                    <ProfileNavigator userInfo={this.props.userInfo}/>
                </View>
            </View>
        );
    }   
}

const ProfilePageStack = createStackNavigator();

class ProfilePageStackScreen extends Component {

    _logOut() {
        const Networking = NativeModules.Networking;
        Networking.clearCookies((cleared) => {
        });
        this.props.navigation.dispatch(
            CommonActions.reset({
                index: 1,
                routes: [{name: "Welcome"}]
            })
        )      
    }

    render() {
        return (
            <ProfilePageStack.Navigator>
                <ProfilePageStack.Screen name="User" 
                    options={{headerStyle: {backgroundColor: "#0b162c"},
                        headerTintColor: "#fff",
                        headerRight: () => (
                            <TouchableOpacity style={styles.logOut} onPress={this._logOut.bind(this)} >
                                <LogOut />
                            </TouchableOpacity>
                        )}}>
                    {props => <ProfilePage userInfo={this.props.userInfo}/>}
                </ProfilePageStack.Screen>
            </ProfilePageStack.Navigator>
        );
    }
}

const styles = StyleSheet.create({
    main_container: {
        flex: 1,
        backgroundColor: "#3b435b",
    },
    profileTop: {
        flex:1,
    },
    profileImage: {
        flex: 5,
        backgroundColor: "#282f43",
    },
    logOut: {
        paddingRight: 10
    }
})

export default ProfilePageStackScreen