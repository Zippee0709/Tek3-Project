import React, {Component} from 'react'
import {View, StyleSheet, Dimensions, Text, Image, TextInput, ActivityIndicator, FlatList, Button} from 'react-native'
import {ArrowUp, ArrowDown, MessageCircle, Eye} from 'react-native-feather'
import { ScrollView } from 'react-native-gesture-handler';
import {getComments, postComment} from '../API/ImgurApi'
import EpicComment from '../Components/EpicComment'
import Video from 'react-native-video'
import VideoPlayer from 'expo-video-player'

export default class ImageDetail extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            loadingImage: true,
            data: null,
            comments: null,
            loaded: false,
            commentMessage: null
        }
    }

    async componentDidMount() {
        if (this.props.route.params.data) {
            this.setState({data: this.props.route.params.data});
            const comments = (await getComments(this.props.route.params.data.id)).data;
            this.setState({comments: comments});
            this.loaded = true;
        }  
    }

    async postComment() {
        if (this.state.commentMessage) {
            let response = await postComment(this.props.route.params.userInfo.accessToken, this.state.commentMessage, this.state.data.id);
            this.state.commentMessage = null
            this.textInput.clear();
        }
    }

    renderImageOrVideo(link) {
        if (!this.props.route.params.isImage) {
            return(<VideoPlayer 
                videoProps={{
                    shouldPlay: false,
                    resizeMode: Video.RESIZE_MODE_COVER,
                    source: {
                        uri: link
                    }
                }}
                onLoadEnd={() => this.setState({loadingImage: false})}/>)
        } else {
            return(<Image source={{uri: link}} resizeMethod="scale" style={styles.image} onLoadEnd={() => this.setState({loadingImage: false})}/>)
        }
    }

    renderImageDetail(data) {
        console.log(data);
        const imageUrl = data.images ? data.images[0].link : data.link;
        return(
            <View style={styles.background}>
                <View style={styles.mainView}>
                    <ScrollView keyboardShouldPersistTaps='always'>
                        <Text style={styles.titleText}>{data.title}</Text>
                        {this.loadingImage && <ActivityIndicator size="large" color="#00ff00"/>}
                        {this.renderImageOrVideo(imageUrl)}
                        <Text style={styles.descriptionText}>{data.images ? data.images[0].description : data.description}</Text>
                        <View style={styles.comments}>
                            <FlatList data={this.state.comments}
                                        keyExtractor={(comment) => comment.id}
                                        renderItem={(comment) => <EpicComment
                                                comment={comment}
                                            />
                                }
                            />
                        </View>
                    </ScrollView>
                </View>
                <View style={styles.commentArea}>
                    <TextInput
                        style={styles.userInputComment}
                        placeholder="Comment here"
                        placeholderTextColor="white"
                        onChangeText={(text) => this.setState({commentMessage: text})}
                        ref={input => { this.textInput = input }}
                    />
                    <Button title="Post" onPress={this.postComment.bind(this)}>
                    </Button>
                </View>
            </View>
        );
    }

    loading() {
        return (<ActivityIndicator size="large" color="#00ff00"/>);
    }

    render() {
        return(this.state.comments ? this.renderImageDetail(this.props.route.params.data) : this.loading());
    }
}

const styles = StyleSheet.create({
    background: {
        backgroundColor: "#282f43",
        flex: 1
    },
    mainView: {
        flex: 4,
        margin: 10
    },
    userInputComment: {
        borderWidth: 1,
        borderColor: "#dfd9da",
        color: "white",
        fontSize: 25,
        flex: 5
    },
    commentArea: {
        flex: 0,
        flexDirection: "row",
        paddingVertical: 10
    },
    comments: {
        flex: 1,
        backgroundColor: "#363f57"
    },
    titleText: {
        color: "white",
        fontSize: 30,
        textAlign: "auto",
        fontWeight: "500",
        marginHorizontal: 20,
        marginVertical: 20
    },
    descriptionText: {
        color: "white",
        fontSize: 15,
        textAlign: "auto",
        fontWeight: "500",
        marginHorizontal: 20,
        marginVertical: 20
    },
    image: {
        aspectRatio: 1,
        width: Dimensions.get("screen").width,
        resizeMode: "contain",
    },
});
