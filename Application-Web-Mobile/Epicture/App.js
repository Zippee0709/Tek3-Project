import React, {Component} from 'react'
import {createStackNavigator} from '@react-navigation/stack'
import { NavigationContainer } from '@react-navigation/native';
import Welcome from './View/Welcome'
import SignIn from './View/SignIn'
import SignUp from './View/SignUp'
import HomeNavigator from './Components/HomeNavigator'

const Stack = createStackNavigator();

function MyStack() {
    const colorBand = "#0b162c";

    return (
        <Stack.Navigator initialRouteName="Welcome">
            <Stack.Screen name="Welcome" component={Welcome} options={{headerShown: false}}/>
            <Stack.Screen name="SignIn" component={SignIn} options={{headerStyle: {backgroundColor: colorBand}, headerTintColor: "#fff"}} />
            <Stack.Screen name="SignUp" component={SignUp} options={{headerStyle: {backgroundColor: colorBand}, headerTintColor: "#fff" }} />
            <Stack.Screen name="Home" component={HomeNavigator} options={{headerStyle: {backgroundColor: colorBand}, headerTintColor: "#fff", headerShown:false}} />
         </Stack.Navigator>
    );
}

class App extends Component {
    render() {
        return (
            <NavigationContainer>
                <MyStack />
            </NavigationContainer>);
    }
}

export default App;
