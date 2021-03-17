import React, {Component} from 'react';
import {View, StyleSheet, FlatList, ActivityIndicator} from 'react-native'
import {getAccountFavorites} from '../API/ImgurApi'
import EpicCard from '../Components/EpicCard'
import { createStackNavigator } from '@react-navigation/stack';
import ImageDetail from '../View/ImageDetail'

class ProfileFavorite extends Component {
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
            const data = (await getAccountFavorites(this.props.userInfo.accountUsername)).data
            this.userInfo.accessToken = this.props.accessToken
            this.userInfo.refreshToken = this.props.refreshToken
            this.userInfo.accountUsername = this.props.accountUsername
            this.userInfo.accountId = this.props.accountId
            this.setState({data: data});
        } catch(err) {
            console.error(err);
        }
    }

    async componentDidUpdate() {
        const data = (await getAccountFavorites(this.props.userInfo.accountUsername)).data
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
            <View style={styles.favorite}>
                {this.state.data ? this.renderCards(this.state.data) : this.renderLoading()}
            </View>
        )
    }
};

const ProfileFavoriteStack = createStackNavigator();

class ProfileFavoriteStackScreen extends Component {
    render() {
        return (
            <ProfileFavoriteStack.Navigator>
                <ProfileFavoriteStack.Screen name="Favorite">
                    {props => <ProfileFavorite {...props} userInfo={this.props.userInfo}/>}
                </ProfileFavoriteStack.Screen>
                <ProfileFavoriteStack.Screen name="ImageDetail" component={ImageDetail} />
            </ProfileFavoriteStack.Navigator>
        );
    }
}


const styles = StyleSheet.create({
    favorite: {
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

//     render(){
//         return (
//             <View style={styles.main_container}>
//                 <Text>Favorite</Text>
//             </View>
//         );
//     }
// }

// const styles = StyleSheet.create({
//     main_container: {
//         flex: 1,
//         backgroundColor: "#282f43",
//     },
// })

export default ProfileFavoriteStackScreen