import React, { useEffect, useState } from "react";
import { Icon, Button, Notification } from "rsuite";
import  { Redirect } from 'react-router-dom'
import ModalTwilio from './../components/ModalCreateServiceTwilio';
import axios from 'axios';
import { Alert } from 'rsuite';
import querystring from 'querystring';
import { API_URL } from '../config'
import ModalCreateServiceTwillio from "./../components/ModalCreateServiceTwilio";
// import ModalCreateServiceSendgrid from "./../components/ModalCreateServiceSendgrid";
import mainLogo from "../assets/area_logo.png";

import "./utils.css";


const qs = require('qs');

function notificationSuccessOauth(which, color) {
    Notification.open({
        duration: 10000,
        description: (
            <div style={{ display: 'flex', flexDirection: 'row', alignItems: 'center' }}>
                <Icon icon={which.toLowerCase()} style={{ color: color, fontSize: 40 }} />
                <div style={{ lineHeight: "4", borderLeft: "1px solid lightgrey", height: "50px", marginLeft: '10px', marginRight: '20px' }}></div>
                <div><Icon icon="check" style={{ color: 'green' }} />  Successfully logged in with <b>{which}</b> !</div>
            </div>
        )
    });
}

function notificationErrorOauth(which, error) {
    Notification.open({
        duration: 10000,
        description: (
            <div style={{ display: 'flex', flexDirection: 'row', alignItems: 'center' }}>
                <Icon icon="warning" style={{ color: 'red', fontSize: 40 }} />
                <div style={{ lineHeight: "4", borderLeft: "1px solid lightgrey", height: "50px", marginLeft: '10px', marginRight: '20px' }}></div>
                <div>There was an error with <b>{which}</b> !</div>
                <div>{error}</div>
            </div>
        )
    });
}

function Profile({ handleSelect }) {
    const [showTwilio, setShowTwilio] = useState(false);
    const [updateNav, setUpdateNav] = useState(true);
    const [redirection, setRedirection] = useState(false);

    // useEffect(() => {
    //     if (updateNav) {
    //         handleSelect("6");
    //         setUpdateNav(false);
    //     }
    // }, [setUpdateNav, updateNav]);

    useEffect(async () => {
        let regex = /[#?&]([^=#]+)=([^&#]*)/g, params = {}, match;
        while (match = regex.exec(window.location.href))
            params[match[1]] = match[2];
        const { state } = params;
        if (state === 'Trello') {
            const { token } = params;
            try {
                console.log(token)
                const loginResponse = await axios.post(`${API_URL}service/createService`, querystring.stringify({
                    nameServices: state, accessToken: token, refreshToken: 'noRefreshTokenWithTrello', tokenType: 'token'
                }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
                notificationSuccessOauth(state, '#007AC0');
                setRedirection(true);
            } catch (e) {
                if (e?.response)
                    Alert.error(e.response.data.error);
                else
                    console.log(e);
            };
        }
        if (state === 'Discord') {
            const { code } = params;
            const data = qs.stringify({
                'client_id': '815209494161260555',
                'client_secret': 'bQbBGPFrMYv9V0RDY_Nrjj3I7rs2mcCv',
                'grant_type': 'authorization_code',
                'code': code,
                'redirect_uri': `${process.env.REACT_APP_REDIRECT_URI}?state=Discord`,
                'scope': 'bot'
            });
            const config = {
                method: 'post',
                url: 'https://discord.com/api/oauth2/token',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                data: data
            };
            try {
                const discordData = await axios(config);
                const { access_token, refresh_token, guild } = discordData.data
                console.log(access_token, refresh_token, guild.name, guild.id);
                const second = await axios.post(`${API_URL}service/createService`, querystring.stringify({
                    nameServices: 'Discord', accessToken: access_token, refreshToken: refresh_token,
                    tokenType: 'Bearer', discordServerName: guild.name, discordServerId: guild.id
                }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
                console.log(second);
                notificationSuccessOauth(state, '#007AC0');
                setRedirection(true);
            } catch (e) {
                console.log(e);
                Alert.error(e.response.data.error);
            };
        }
        if (state === 'GitLab') {
            const { code } = params;
            const data = qs.stringify({
                'client_id': '431aa4bc4e71c9e1937985be9021cbbf549b4fe79ba8e5e111d818d9bcc7151b',
                'client_secret': '2f0b5ff83a93499c9ddb7863864bcf965f21afe2991794385c0ba57f922b43f9',
                'grant_type': 'authorization_code',
                'code': code,
                'redirect_uri': `${process.env.REACT_APP_REDIRECT_URI}?state=GitLab`,
            });
            const config = {
                method: 'post',
                url: 'https://gitlab.com/oauth/token',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                data: data
            };
            try {
                const gitlabData = await axios(config);
                const { access_token, refresh_token } = gitlabData.data
                console.log(access_token, refresh_token);
                const second = await axios.post(`${API_URL}service/createService`, querystring.stringify({
                    nameServices: 'GitLab', accessToken: access_token, refreshToken: refresh_token,
                    tokenType: 'Bearer'
                }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
                console.log(second);
                notificationSuccessOauth(state, '#007AC0');
                setRedirection(true);
            } catch (e) {
                console.log(e);
                Alert.error(e.response.data.error);
            };
        }
        if (state === 'GitHub') {
            const { code } = params;
            const data = qs.stringify({
                'client_id': '0123714cfdd44c79b26d',
                'client_secret': '7afce251b4e9722fd12e4062258345da93159303',
                'code': code,
                'redirect_uri': `${process.env.REACT_APP_REDIRECT_URI}?state=GitHub`,
            });
            const config = {
                method: 'post',
                url: 'https://github.com/login/oauth/access_token',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                    'Accept' : 'application/json',
                },
                data: data
            };
            try {
                const githubData = await axios(config);
                const { access_token, token_type } = githubData.data
                const second = await axios.post(`${API_URL}service/createService`, querystring.stringify({
                    nameServices: 'GitHub', accessToken: access_token, refreshToken: access_token,
                    tokenType: 'Bearer'
                }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
                console.log(second);
                notificationSuccessOauth(state, '#007AC0');
                setRedirection(true);
            } catch (e) {
                console.log(e);
                Alert.error(e?.response?.data?.error);
            };
        }
        if (state === 'Slack') {
            const { code } = params;
            console.log(code)
            const axios = require('axios');
            var config = {
                method: 'get',
                url: 'https://slack.com/api/oauth.v2.access?client_id=1797353003221.1797494244069&client_secret=fad660328cdbdf60499eed954665e9d4&code=' + code + `&redirect_uri=${process.env.REACT_APP_REDIRECT_URI}`,
                headers: {}
            };
            try {
                const dataSlack = await axios(config);
                const { token_type, access_token, bot_user_id, team, incoming_webhook } = dataSlack.data;

                try {
                    const second = await axios.post(`${API_URL}service/createService`, qs.stringify({
                        nameServices: 'Slack', accessToken: 'xoxp-1797353003221-1812940557571-1829126271009-b7a0ca0d19af62d7d3d28ada8312e806', refreshToken: 'xoxp-1797353003221-1812940557571-1829126271009-b7a0ca0d19af62d7d3d28ada8312e806',
                        tokenType: 'bearer', slackWebhookUri: incoming_webhook.url, slackBotId: bot_user_id, teamName: team.name
                    }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
                    console.log(second);
                    notificationSuccessOauth(state, '#007AC0');
                    setRedirection(true);
                } catch (e) {
                    console.log(e);
                    e?.response !== undefined && Alert.error(e.response.data.error);
                };
            } catch(e) {
                console.log(e);
                e?.response !== undefined && Alert.error(e.response.data.error);
            }
        }
    }, [])
    const createSendgrid = async () => {
        try {
            await axios.post(`${API_URL}service/createService`, querystring.stringify({
                nameServices: 'SendGrid', accessToken: 'null', refreshToken: 'null', tokenType: 'token'
            }), { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
            Alert.success('Sengrid created');
        } catch (e) {
            if (e && e.response && e.response.data && e.response.data.error)
                Alert.error(e.response.data.error);
            else
                Alert.error('Une erreur est survenu');
        }
    };
    // if (authFails)
    //     return <Redirect to='/login'  />
    // }
    return (
        <>
            {redirection && <Redirect to='/create'  />}
            <div
                style={{
                    alignItems: "center",
                    alignSelf: "center",
                    justifyContent: "center",
                    display: "flex",
                    marginTop: "1%",
                }}
            >
                <img
                    src={mainLogo}
                    alt="fireSpot"
                    style={{ width: "200px", height: "200px" }}
                />
                <div>
                    <h1 className="title">Connect a</h1>
                    <h1 className="title">
                        services{" "}
                        <Icon
                            icon="connectdevelop"
                            size="3x"
                            style={{ marginRight: "10px", color: "black" }}
                        />
                    </h1>
                </div>
            </div>
            <div style={{ display: 'flex', flexDirection: 'column', justifyContent: 'center', alignContent: 'center', alignItems: 'center', marginTop: '5%' }}>
                <a style={{ marginTop: '10px' }} href={`https://trello.com/1/authorize?expiration=30days&name=AREA-EPITECH&scope=read,write,account&response_type=token&key=ff4c32d87be5508c24f6390892f7d7a5&return_url=${process.env.REACT_APP_REDIRECT_URI}?state=Trello`}>
                    <Button color="primary">
                        <Icon icon="trello" /> Sign-in to trello
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }} href={`https://discord.com/api/oauth2/authorize?client_id=815209494161260555&permissions=8&redirect_uri=https%3A%2F%2Farea-junaifu-front.wonderful-goose-4.telebit.io%2Fservices%3Fstate%3DDiscord&response_type=code&scope=bot%20identify%20email%20guilds%20guilds.join&response_type=code`}>
                    <Button color="primary">
                        <Icon icon="skype" /> Sign-in to Discord
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }} href={`https://gitlab.com/oauth/authorize?client_id=431aa4bc4e71c9e1937985be9021cbbf549b4fe79ba8e5e111d818d9bcc7151b&redirect_uri=${process.env.REACT_APP_REDIRECT_URI}?state=GitLab&response_type=code&state=GitLab&scope=api`}>
                    <Button color="primary">
                        <Icon icon="gitlab" /> Sign-in to GitLab
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }} href={`https://github.com/login/oauth/authorize?client_id=0123714cfdd44c79b26d&redirect_uri=${process.env.REACT_APP_REDIRECT_URI}?state=GitHub&response_type=code&state=GitHub&scope=user,repo,admin:repo_hook,delete_repo`}>
                    <Button color="primary">
                        <Icon icon="github" /> Sign-in to Github
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }} href={`https://slack.com/oauth/v2/authorize?client_id=1797353003221.1797494244069&scope=incoming-webhook,app_mentions:read,chat:write,chat:write.customize,files:read,files:write&state=Slack&redirect_uri=${process.env.REACT_APP_REDIRECT_URI}`}>
                    <Button color="primary">
                        <Icon icon="slack" /> Sign-in to Slack
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }}>
                    <Button color="primary" onClick={() => setShowTwilio(!showTwilio)}>
                        <Icon icon="commenting" /> Sync Twilio
                    </Button>
                </a>
                <a style={{ marginTop: '10px' }}>
                    <Button color="primary" onClick={createSendgrid}>
                        <Icon icon="envelope" /> Sync Sendgrid
                    </Button>
                </a>
            </div>
            {showTwilio && <ModalCreateServiceTwillio></ModalCreateServiceTwillio>}
        </>
    );
}

export default Profile;