import React from 'react'
import {View, StyleSheet, TextInput, Button, FlatList, ActivityIndicator} from 'react-native'
import {Search} from 'react-native-feather'
import EpicCard from '../Components/EpicCard'
import {getHomeViralFeed, searchByInput} from '../API/ImgurApi';

class SearchPage extends React.Component {

    constructor(props) {
        super(props);
        this.state = {
            data: [],
            loading: true,
            searchText: null,
            loadingState: null
        }
        this.userInfo = {
            accessToken: null,
            refreshToken: null,
            accountUsername: null,
            accountId: null
        }
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

    async _loadSearchByInput() {
        await searchByInput(this.state.searchText).then(data => this.setState({data: data.data}))

    }

    searchTextInput(text) {
        this.setState({searchText: text})
    }

    render() {
    return (
      <View style={styles.main_container}>
          <View style={styles.mainSearch_container}>
            <View style={styles.search_container}>
                <Search style={styles.searchIcon}/>
                <TextInput onChangeText={(text) => this.searchTextInput(text)} style={styles.textInput} placeholder='Search anything you want !' placeholderTextColor='white'/>      
            </View>
          </View>
          <View style={styles.searchButton}>
            <Button title='Rechercher' color="grey" style={styles.searchButton} onPress={() => this._loadSearchByInput()}/>
          </View>
          <View style={styles.searchResult}>
                {this.state.data ? this.renderCards(this.state.data) : this.renderLoading()}
          </View>
      </View>
    )
  }
}

const styles = StyleSheet.create({
    main_container: {
        flex: 1,
        backgroundColor: 'white'//"#151d28",
    },
    mainSearch_container: {
        flex: 1,
        // height: 100,
        backgroundColor: 'white'
    },
    search_container:{
        flex: 3,
        flexDirection: "row",
        alignItems: "center",
        backgroundColor: '#282f43'//"#151d28",
    },
    searchIcon: {
        flex: 1,
        color: "white",
        backgroundColor: '#282f43'
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
    searchButton: {
        flex: 1,
        backgroundColor: "#282f43"
    },
    searchResult: {
        flex: 9,
        backgroundColor: "#282f43"
    }
})

export default SearchPage