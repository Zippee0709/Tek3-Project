import React, {Component} from 'react';
import {View, StyleSheet, Text, Image, TouchableOpacity, Alert} from 'react-native'
import {Icon} from 'react-native-elements';
import * as ImagePicker from 'expo-image-picker'

class Upload extends Component {
    constructor(props) {
        super(props);
        this.state = {
            hasCameraPermission: null,
            result: {
                cancelled: null,
                type: null,
                uri: null,
                height: null,
                width:null
            }
        }
    };

    async componentDidMount() {
        if (this.state.hasCameraPermission == null) {
            const { status } = await ImagePicker.requestCameraRollPermissionsAsync();
            this.setState({ hasCameraPermission: status === "granted" });
        }
    }

    async _getPhotoLibrary() { 
        let result = await ImagePicker.launchImageLibraryAsync({
            allowsEditing: true,
            aspect: [4, 3]
       });
       if (!result.cancelled) {
            this.setState({ result: result });
        }
    };

    renderFileData() {
        if (this.state.result.uri != null) {
            return <Image source={{ uri: this.state.result.uri }} style={styles.image} />
        }
        return <Icon name={'insert-photo'} color={'grey'} size={200} type={'Entypo'}/>
    }

    _onPressNext () {
        if (this.state.result.uri != null) {
            const {navigate} = this.props.navigation
            navigate("PostImage", {result: this.state.result, userInfo: this.props.userInfo})
        }
        else
            Alert.alert("Please choose a image !")
    }

    render() {
        return (
            <View style={styles.main_container}>                
                <View style={styles.imageSection}>
                    {this.renderFileData()}
                </View>
                <View style={styles.btnSection}>
                    <TouchableOpacity onPress={this._getPhotoLibrary.bind(this)} style={styles.Button}>
                        <Text style={styles.btnText}>Choose File</Text>
                    </TouchableOpacity>
                </View>
                <View style={styles.btnNextSection}>
                    <TouchableOpacity onPress={this._onPressNext.bind(this)} style={styles.Button}>
                            <Text style={styles.btnText}>Next</Text>
                    </TouchableOpacity>
                </View>
            </View>
        );
    }
}

const styles = StyleSheet.create({
    main_container: {
        flex: 1,
        justifyContent: 'center',
        borderColor: 'black',
        backgroundColor: "#282f43",
    },
    imageSection: {
        flex: 6,
        justifyContent:'center',
    },
    image: {
        aspectRatio: 1,
        resizeMode: 'contain'
    },  
    btnSection: {
        flex:4,
        flexDirection:'row',
        alignItems: 'center',
        justifyContent: 'center',
        justifyContent: 'space-around',
    },
    btnNextSection: {
        flex:2,
        alignItems: 'center',
    },
    Button: {
        width: 130,
        alignItems: "center",
        backgroundColor: "#0b162c",
        borderRadius: 20,
        padding: 10
    },
    btnText: {
        textAlign: 'center',
        color: 'gray',
        fontSize: 14,
        fontWeight:'bold'
    },
    textInput: {
        flex: 2,
        marginLeft: 5,
        marginRight: 5,
        height: 50,
        borderColor: 'grey',
        borderWidth: 1,
        paddingLeft: 5,
        color: "white"
    },

});



export default Upload