import React, {Component} from 'react'
import {View, StyleSheet, Image, Button, FlatList, Text, ActivityIndicator} from 'react-native'
import { getUserAvatar } from '../API/ImgurApi'

export default class EpicComment extends Component {
    constructor(props) {
        super(props);
        this.state = {
            profilImageUrl: null
        }
    }

    async componentDidMount() {
        this.setState({profilImageUrl: (await getUserAvatar(this.props.comment.item.author)).data.avatar});
    }

    renderComment() {
        const comment = this.props.comment.item;
        const date = new Date(comment.datetime * 1000);

        return(
            <View style={styles.commentBox}>
                <View style={styles.imageProfil}>
                    <Image style={styles.imageProfil} source={{uri: this.state.profilImageUrl}}/>
                </View>
                <View style={styles.rightSide}>
                    <Text style={styles.author}>
                        {comment.author}
                    </Text>
                    <View style={styles.commentArea}>
                        <Text style={styles.commentText}>
                            {comment.comment}
                        </Text>
                    </View>
                    <Text style={styles.text}>
                        {date.toLocaleDateString("en-US", { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' })}
                    </Text>
                </View>
            </View>
        )
    }

    loading() {
        return (<ActivityIndicator size="large" color="#00ff00"/>);
    }

    render() {
        return(this.state.profilImageUrl ? this.renderComment() : this.loading());
    }
}


const styles = StyleSheet.create({
    commentBox: {
        flex: 1,
        flexDirection: "row",
        borderBottomWidth: 2,
        borderColor: "#9c9fa6",
        paddingVertical: 5,
        marginVertical: 0.25,
    },
    commentArea: {
        flex: 1
    },
    commentText: {
        flex: 1,
        flexWrap: "wrap",
        marginVertical: 10,
        color: "white"
    },
    text: {
        color: "white",
        fontSize: 15,
        overflow: "scroll"
    },
    author: {
        color: "white",
        fontSize: 20,
        fontWeight: "bold",
        marginBottom: 10  
    },
    rightSide: {
        flex: 1
    },
    imageProfil: {
        alignItems: "flex-start",
        width: 50,
        height: 50,
        borderRadius: 25,
        paddingLeft: 5,
        marginRight: 10
        // resizeMode: "contain",
    }
});