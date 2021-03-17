import React, {Component} from 'react';
import { createStackNavigator } from '@react-navigation/stack';
import Upload from '../View/Upload'
import PostImage from '../View/PostImage'

const UploadStack = createStackNavigator();

class UploadStackSceen extends Component {
    render() {
        return (
            <UploadStack.Navigator>
                <UploadStack.Screen name="UploadImage" options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}>
                    {props => <Upload {...props} userInfo={this.props.userInfo}/>}
                </UploadStack.Screen>
                <UploadStack.Screen name="PostImage" component={PostImage} options={{headerStyle: {backgroundColor: "#0b162c"}, headerTintColor: "#fff"}}/>
            </UploadStack.Navigator>
        );
    }
}

export default UploadStackSceen