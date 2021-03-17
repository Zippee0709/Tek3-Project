import React, {Component} from 'react';
import { createMaterialTopTabNavigator } from '@react-navigation/material-top-tabs';
import ProfilePostStackScreen from '../View/ProfilePost'
import ProfileFavoriteStackScreen from '../View/ProfileFavorites'

// import {Text, View, StyleSheet, Button} from 'react-native'

const Tab = createMaterialTopTabNavigator();

class ProfileNavigator extends Component {
    render() {
        return (
        <Tab.Navigator initialRouteName="Post" tabBarOptions={{activeTintColor:'#282f43', inactiveTintColor:'#3b435b'}}>
            <Tab.Screen name="Post" children={()=><ProfilePostStackScreen userInfo={this.props.userInfo}/>}/>
            <Tab.Screen name="Favorites" children={()=><ProfileFavoriteStackScreen userInfo={this.props.userInfo}/>} />
        </Tab.Navigator>
        )
    }
}

export default ProfileNavigator

