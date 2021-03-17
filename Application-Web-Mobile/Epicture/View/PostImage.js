import React, {Component} from 'react';
import {Icon} from 'react-native-elements';
import {View, StyleSheet, Text, Image, TouchableOpacity, TextInput,  Alert} from 'react-native'
import {postImage} from '../API/ImgurApi';
import * as FileSystem from 'expo-file-system';
// import RNFS from 'react-native-fs'

class PostImage extends Component {
    constructor(props) {
        super(props);
        this.state={
            result: this.props.route.params.result,
            userInfo: this.props.route.params.userInfo,
            imageTitle: null,
            imageDescription: null,
            imagePostResult: null,
            imageBase64: null,
            uploadState: false,
        }
    }

    renderFileData() {
        if (this.state.result.uri != null) {
            return <Image source={{ uri: this.state.result.uri }} style={styles.image} />
        }
        return <Icon name={'insert-photo'} color={'grey'} size={200} type={'Entypo'}/>
    }

    _imageTitleInput(input) {
        this.setState({imageTitle: input})
    }

    _imageDescriptionInput(input) {
        this.setState({imageDescription: input})
    }

    async _onPressPostImage() {
        let res = await FileSystem.readAsStringAsync(this.state.result.uri, {encoding: FileSystem.EncodingType.Base64 });
        let formData = {image: res, title: this.state.imageTitle, description: this.state.imageDescription, type: "base64"};

        if (this.state.uploadState == false) {
            const data = (await postImage(this.state.userInfo.accessToken, formData))
            this.setState({uploadState: true })
            this.setState({imagePostResult: data});
            if (this.state.imagePostResult.success == false || this.state.imagePostResult == null) {
                Alert.alert("Upload failed, please retry later")
                this.setState({uploadState: false })
            }
            else {
                this.setState({uploadState: false })
                const {navigate} = this.props.navigation
                navigate("UploadImage", {result: null, userInfo: this.props.userInfo})
            }
        }
    }

    render() {
        return (
            <View style={styles.main_container}>
                <View style={styles.titleImage}>
                    <TextInput onChangeText={(text) => this._imageTitleInput(text)} style={styles.textInput} placeholder='Title of image (Optional)' placeholderTextColor='white'/>
                </View>
                <View style={styles.imageSection}>
                    {this.renderFileData()}
                </View>
                <View style={styles.description}>
                    <TextInput onChangeText={(text) => this._imageDescriptionInput(text)} style={styles.descriptionInput} placeholder='Description of image (Optional)' placeholderTextColor='white'/>
                </View>
                <View style={styles.postButton}>
                    <TouchableOpacity onPress={this._onPressPostImage.bind(this)} style={styles.Button}>
                            <Text style={styles.btnText}> Post !</Text>
                    </TouchableOpacity>
                </View>
            </View>
        )
    }
}

const styles = StyleSheet.create({
    main_container: {
        flex: 1,
        justifyContent: 'center',
        borderColor: 'black',
        backgroundColor: "#282f43",
    },
    titleImage: {
        flex:2
    },
    imageSection: {
        flex: 6,
        justifyContent:'center',
    },
    image: {
        aspectRatio: 1,
        resizeMode: 'contain'
    },
    description: {
        flex: 5
    },
    postButton: {
        flex: 2,
        justifyContent: 'center',
        alignItems: 'center'
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
        marginTop: 10,
        marginLeft: 5,
        marginRight: 5,
        height: 50,
        borderColor: 'grey',
        borderWidth: 1,
        paddingLeft: 5,
        color: "white"
    },
    descriptionInput:{
        marginTop: 20,
        marginLeft: 5,
        marginRight: 5,
        height: 200,
        borderColor: 'grey',
        borderWidth: 1,
        paddingLeft: 5,
        color: "white"
    }
})

export default PostImage