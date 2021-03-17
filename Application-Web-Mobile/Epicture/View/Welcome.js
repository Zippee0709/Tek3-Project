import React from 'react'
import {View, StyleSheet, Text, Image, TouchableOpacity, Linking} from 'react-native'



const Welcome = ({navigation}) => {    
    return (
        <View style = {styles.welcome}>
            <View style = {styles.logo}>
                <Image source={require("../assets/logo_t.png")}></Image>
            </View>
            <TouchableOpacity style={[styles.button, styles.buttonGuest]} onPress={() =>
                Linking.canOpenURL("https://imgur.com/").then(supported => {
                    if (supported) {
                        Linking.openURL("https://imgur.com/");
                    } else {
                        console.error("Don't know how to open URI: " + "https://imgur.com/");
                    }
            })}><Text style={styles.textButton}>Continue as Guest</Text></TouchableOpacity>

            <TouchableOpacity style={[styles.button, styles.buttonSignIn]} onPress={() => navigation.navigate('SignIn')}><Text style={styles.textButton}>Sign In</Text></TouchableOpacity>

            <TouchableOpacity style={[styles.button, styles.buttonSignUp]} onPress={() => 
                Linking.canOpenURL("https://imgur.com/register").then(supported => {
                    if (supported) {
                      Linking.openURL("https://imgur.com/register");
                    } else {
                      console.log("Don't know how to open URI: " + "https://imgur.com/register");
                    }
            })}><Text style={styles.textButton}>Sign Up</Text></TouchableOpacity>
        </View>
    )
}

const styles = StyleSheet.create({
    logo: {
        flexDirection: "row",
        justifyContent: "center",
        marginTop: "45%"
    },
    welcome: {
        flex: 1,
        backgroundColor: "#151d28",
    },
    button: {
        marginTop:25,
        paddingVertical: 10,
        marginLeft:"15%",
        marginRight:"15%",
        backgroundColor:'#878787',
        borderRadius:20,
        borderWidth: 1,
        alignItems: "center"
        
    },
    buttonGuest: {
        backgroundColor:'#878787',
    },
    buttonSignIn: {
        backgroundColor:'#e34c3c',
    },
    buttonSignUp: {
        backgroundColor:'#293042',
    },
    textButton: {
        color: "white",
        fontSize: 20
    }
});

export default Welcome;