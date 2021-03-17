import React, {Component} from 'react'
import {View, StyleSheet, FlatList, ActivityIndicator} from 'react-native'
import {getHomeViralFeed} from '../API/ImgurApi'
import EpicCard from '../Components/EpicCard'


class Home extends Component {
    constructor(props) {
        super(props);
        this.state = {
            data: null
        }
        this.userInfo = {
            accessToken: null,
            refreshToken: null,
            accountUsername: null,
            accountId: null
        }
    }

    async componentDidMount() {
        try {
            const data = (await getHomeViralFeed()).data
            this.userInfo.accessToken = this.props.userInfo.accessToken
            this.userInfo.refreshToken = this.props.userInfo.refreshToken
            this.userInfo.accountUsername = this.props.userInfo.accountUsername
            this.userInfo.accountId = this.props.userInfo.accountId
            this.setState({data: data});
        } catch(err) {
            console.error(err);
        }
    }

    async componentDidUpdate() {
        const data = (await getHomeViralFeed()).data
        this.setState({data: data});
    }

    renderCards(data) {
        let list = []
        this.state.data.forEach(element => {
            list.push(element)
        });
        return (
        <FlatList numColumns={1}
            data={list}
            keyExtractor={(item) => item.id.toString()}
            renderItem = {({item}) => <EpicCard
                data = {item}
                userInfo = {this.userInfo}
                navigation = {this.props.navigation}
            />
            }
        > 
        </FlatList>
        )
    }

    renderLoading() {
        return <ActivityIndicator size="large" color="#00ff00" style={styles.loader} />
    }

    render() {
        return(
            <View style={styles.home}>
                {this.state.data ? this.renderCards(this.state.data) : this.renderLoading()}
            </View>
        )
    }
};

const styles = StyleSheet.create({
    home: {
        flex: 1,
        backgroundColor: "#151d28",
    },
    loader: {
        flex: 1,
        alignSelf: "center",
    },
    p: {
        color: "#9C27B0",
    }
});

export default Home;

