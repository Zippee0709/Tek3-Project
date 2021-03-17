import React, {Component} from 'react';
import {StyleSheet} from 'react-native'
import { createBottomTabNavigator } from '@react-navigation/bottom-tabs';
import {Icon} from 'react-native-elements';
import HomeStackScreen from './HomeStackNavigator'
import SearchStackScreen from './SearchStackNavigator'
import UploadStackScreen from './UploadStackNavigator'
import ProfilePageStackScreen from '../View/Profile';

const Tab = createBottomTabNavigator();

class HomeNavigator extends Component {
  constructor(props) {
    super(props);
    this.userInfo = {
        accessToken: null,
        refreshToken: null,
        accountUsername: null,
        accountId: null
    }
  }

  render(){
        return (
            <Tab.Navigator
                initialRouteName="Home"
                screenOptions={({ route }) => ({
                    tabBarIcon: ({ focused, color, size }) => {
                        let iconName;
                        let iconType = 'Entypo'
                        if (route.name === 'Home') {
                            iconName = 'home';
                        } else if (route.name === 'Search') {
                            iconName = 'search'
                        }
                        else if (route.name == 'User')
                            iconName = 'account-circle'
                        else if (route.name == 'Upload')
                            iconName = 'insert-photo'
                        return <Icon name={iconName} color={color} size={size} type={iconType}/>
                    },                    
                })}
                tabBarOptions={{
                    activeTintColor: 'tomato',
                    inactiveTintColor: 'gray',
                    style: {backgroundColor: "#0b162c"}
                }}
            >
                <Tab.Screen name="Home" children={()=><HomeStackScreen userInfo={this.props.route.params}/>}/>              
                <Tab.Screen name="Search" children={()=><SearchStackScreen userInfo={this.props.route.params} />}/>
                <Tab.Screen name="Upload" children={()=><UploadStackScreen userInfo={this.props.route.params} />}/>
                <Tab.Screen name="User" children={()=><ProfilePageStackScreen navigation={this.props.navigation}  userInfo={this.props.route.params}/>}/>
            </Tab.Navigator>
  );
  }
}

const styles = StyleSheet.create({
    navigationFooter: {
        flex: 1,
        color: 'grey',
        justifyContent: 'center',
        alignItems: 'center'
    },
    icon: {
        color: 'white'
    }
});

export default HomeNavigator
