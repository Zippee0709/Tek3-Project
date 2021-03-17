import React, {Component} from 'react';
import { createStackNavigator } from '@react-navigation/stack';
import Search from '../View/Search'
import ImageDetail from '../View/ImageDetail'

const SearchStack = createStackNavigator();

class SearchStackSceen extends Component {
    render() {
        return (
            <SearchStack.Navigator>
                <SearchStack.Screen name="Search" component={Search} options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}/>
                <SearchStack.Screen name="ImageDetail" component={ImageDetail} options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}/>
            </SearchStack.Navigator>
        );
    }
}

export default SearchStackSceen