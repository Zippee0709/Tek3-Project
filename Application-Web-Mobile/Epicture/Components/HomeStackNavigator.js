import React, {Component} from 'react';
import { createStackNavigator } from '@react-navigation/stack';
import Home from '../View/Home'
import ImageDetail from '../View/ImageDetail'


const HomeStack = createStackNavigator();

class HomeStackScreen extends Component {
    render() {
        return (
            <HomeStack.Navigator>
                <HomeStack.Screen name="Home" options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}>
                    {props => <Home {...props} userInfo={this.props.userInfo}/>}
                </HomeStack.Screen>
                <HomeStack.Screen name="ImageDetail" component={ImageDetail} options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}/>
            </HomeStack.Navigator>
        );
    }
}

export default HomeStackScreen
