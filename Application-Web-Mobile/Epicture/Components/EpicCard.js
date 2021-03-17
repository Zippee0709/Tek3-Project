import React, {Component} from 'react'
import {View, StyleSheet, Dimensions, Text, Image, TouchableOpacity} from 'react-native'
import {ArrowUp, ArrowDown, MessageCircle, Eye, Heart} from 'react-native-feather'
import Video from 'react-native-video'
import VideoPlayer from 'expo-video-player'
import { voteGallery, favorite } from '../API/ImgurApi'
// import * as VideoThumbnails from 'expo-video-thumbnails'

export default class EpicCard extends Component {
    _isImage = true;
    constructor(props) {
        super(props)
        this.state = {
            width: null,
            height: null,
            upTriggered: null,
            downTriggered: null,
            upVoteCount: null,
            downVoteCount: null,
            favorite: null,
        }
    }

    // Source: https://stackoverflow.com/questions/9461621/format-a-number-as-2-5k-if-a-thousand-or-more-otherwise-900
    formatViewCount(num, digits) {
        let i;
        let rx = /\.0+$|(\.[0-9]*[1-9])0+$/;
        let si = [
          { value: 1, symbol: "" },
          { value: 1E3, symbol: "k" },
          { value: 1E6, symbol: "M" },
          { value: 1E9, symbol: "G" },
          { value: 1E12, symbol: "T" },
          { value: 1E15, symbol: "P" },
          { value: 1E18, symbol: "E" }
        ];
        for (i = si.length - 1; i > 0; i--) {
          if (num >= si[i].value) {
            break;
          }
        }
        return (num / si[i].value).toFixed(digits).replace(rx, "$1") + si[i].symbol;
    }

    componentDidMount() {
        this.setState({upTriggered: this.props.data.vote == "up"});
        this.setState({falseTriggered: this.props.data.vote == "down"});
        this.setState({upVoteCount: this.props.data.ups});
        this.setState({downVoteCount: this.props.data.downs});
        this.setState({favorite: this.props.data.favorite == true});    
    }   

    _onPressImage() {
        const { navigate } = this.props.navigation;
        console.log(this.props.data);
        navigate("ImageDetail", {data: this.props.data, userInfo: this.props.userInfo, isImage: this._isImage});
    }

    async _onPressUp() {
        if (this.state.downTriggered) {
            this._onPressDown();
        }
        if (!this.state.upTriggered) {
            let res = await voteGallery(this.props.userInfo.accessToken, this.props.data.id, "up");
            this.setState({upVoteCount: this.state.upVoteCount + 1});
        } else {
            let res = await voteGallery(this.props.userInfo.accessToken, this.props.data.id, "veto");
            this.setState({upVoteCount: this.state.upVoteCount - 1});
        }
        this.setState({upTriggered: !this.state.upTriggered});
    }

    async _onPressDown() {
        if (this.state.upTriggered) {
            this._onPressUp();
        }
        if (!this.state.downTriggered) {
            let res = await voteGallery(this.props.userInfo.accessToken, this.props.data.id, "down");
            this.setState({downVoteCount: this.state.downVoteCount + 1});
        } else {
            let res = await voteGallery(this.props.userInfo.accessToken, this.props.data.id, "veto");
            this.setState({downVoteCount: this.state.downVoteCount - 1});
        }
        this.setState({downTriggered: !this.state.downTriggered});
    }

    async _onPressFavorite() {
        let res = await favorite(this.props.userInfo.accessToken, this.props.data.id);
        this.setState({favorite: !this.state.favorite});
    }

    renderImageOrVideo(link) {
        let split = link.split(".");
        let extension = split[split.length - 1]

        if (extension == "mp4") {
            return(<VideoPlayer videoProps={{
                shouldPlay: false,
                resizeMode: Video.RESIZE_MODE_COVER,
                source: {
                    uri: link
                }}} style={[styles.video, styles.videoColor]}></VideoPlayer>)
        } else if (extension == "jpg" || extension == "png") {
            return(<Image source={{uri: link}} resizeMethod="scale" style={[styles.image, styles.imageColor]}/>)
        }
    }

    isImageOrVideo(link) {
        let split = link.split(".");
        let extension = split[split.length - 1]

        if (extension == "mp4") {
            this._isImage = false;
        }
    }

    render() {
        const imageUrl = this.props.data.images ? this.props.data.images[0].link : this.props.data.link;
        const title = this.props.data.title;
        const commentCount = this.props.data.comment_count;
        const viewCount = this.formatViewCount(this.props.data.views, 1);

        this.isImageOrVideo(imageUrl);
        return(
            <View style={[styles.card, this._isImage ? styles.cardImageColor : styles.cardVideoColor]}>
                <TouchableOpacity style={{flex: 1}} onPress={this._onPressImage.bind(this)}>
                    <View style={{justifyContent: 'flex-start', flexDirection: 'column'}}>
                        <Text numberOfLines={2} style={[styles.text, styles.title, this._isImage ? styles.imageColor : styles.videoColor]}>{title}</Text>
                        {this.renderImageOrVideo(imageUrl)}
                    </View>
                    <View style={styles.band}>
                        <View style={[styles.info, this._isImage ? styles.imageColor : styles.videoColor]}>
                            <TouchableOpacity style={styles.iconInfo} onPress={this._onPressUp.bind(this)}>
                                <ArrowUp style={styles.icon && this.state.upTriggered ? {color: "green"} : {color: "white"}}/>
                                <Text style={styles.text && this.state.upTriggered ? {color: "green"} : {color: "white"}}>{this.state.upVoteCount}</Text>
                            </TouchableOpacity>
                            <TouchableOpacity style={styles.iconInfo} onPress={this._onPressDown.bind(this)}>
                                <ArrowDown style={styles.icon && this.state.downTriggered ? {color: "red"} : {color: "white"}}/>
                                <Text style={styles.text && this.state.downTriggered ? {color: "red"} : {color: "white"}}>{this.state.downVoteCount}</Text>
                            </TouchableOpacity>
                            <TouchableOpacity style={styles.iconInfo}>
                                <MessageCircle style={styles.icon}/>
                                <Text style={styles.text}>{commentCount} </Text>
                            </TouchableOpacity>
                            <TouchableOpacity style={styles.iconInfo}>
                                <Eye style={styles.icon}/>
                                <Text style={styles.text}>{viewCount} </Text>
                            </TouchableOpacity>
                            <TouchableOpacity style={styles.iconInfo} onPress={this._onPressFavorite.bind(this)}>
                                <Heart style={styles.icon && this.state.favorite ? {color: "red"} : {color: "white"}}/>
                            </TouchableOpacity>
                        </View>
                    </View>
                </TouchableOpacity>
            </View>
        );
    }
}

const styles = StyleSheet.create({
    card: {
        flex: 1,
        // height: Dimensions.get("screen").height / 2.4,
        // width: Dimensions.get("screen").width / 2,
        margin: 5,
        borderWidth: 5,
        borderColor: "#282f43",
        borderRadius: 10,
        overflow: "hidden"
    },
    cardImageColor: {
        backgroundColor: "#282f43",
        borderColor: "#282f43"
    },
    cardVideoColor: {
        backgroundColor: "#db5143",
        borderColor: "#db5143"
    },
    iconInfo: {
        flex: 1,
        alignItems: "center"
    },
    image: {
        flex: 1, 
        aspectRatio: 0.9,
        resizeMode: "contain",
    },
    imageColor: {
        backgroundColor: "#282f43",
    },
    videoColor: {
        backgroundColor: "#db5143"
    },
    info: {
        flexDirection: "row",
        paddingTop: 5,
    },
    text: {
        color: "white",
    },
    title: {
        flex: 1,
        fontSize: 25,
        fontWeight: "bold",
        color: "#ebdad9",
        backgroundColor: "#282f43",
        paddingTop: 20,
        paddingBottom: 20
    },
    band: {
        flex: 1,
    },
    icon: {
        color: "white"
    }
});
