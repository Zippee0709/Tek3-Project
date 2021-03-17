/**
 * Discord Services Controller
*/

const axios = require('axios');

const servicesModel = require('../models/servicesModel');
const discordModel = require('../models/discordServiceModel');

const discordController = require('../controllers/discordArea/discordAreaController');

const botDiscord = require('./discordBotControler');

const { DiscordApiKey, DiscordBotToken } = require('../config/config');

const discordEndPoint = 'https://discord.com/api/v8/';

const getDiscordApiRep = async(tokenType, accessToken, action, route, object={}) => {
    try {
        const discordRep = await axios(discordEndPoint + route, {
            method : action,
            headers: {
                Authorization: tokenType + " " + accessToken
            },
            data : object
        });
        return discordRep.data;
    } catch (error) {
        console.log(error);
        return null
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await discordModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.getServer = async (req, res) => {

    const { userId } = req.body;

    try {
        let listServer = []
        const discordService = await servicesModel.findOne({userIdRef: userId, nameServices: "Discord"});
        if (!discordService) {
            return res.status(404).json({ error: 'Failed to get user\'s Discord info from DB' });
        };
        const allDiscordSub = await discordModel.find({serviceIdRef: discordService._id});
        for (const info of allDiscordSub) {
            listServer.push({
                serverId: info.discordServerID,
                serverName: info.discordServerName,
            })
        }
        res.status(200).json(listServer);
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }

}

exports.getChannels = async (res, req) => {

    const { userId } = req.body;
    const { discordServerID } = req.params;

    try {
        const discordService = await servicesModel.findOne({userIdRef: userId, nameServices: "Discord"});
        if (!discordService) {
            return res.status(404).json({ error: 'Failed to get user\'s Discord info from DB' });
        };
        const {accessToken} = discordService;
        const discordChannels = await getDiscordApiRep("Bot", DiscordBotToken, 'get', 'guilds/' + discordServerID + '/channels', null);
        res.status(200).json({success: discordChannels})
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.postMessage = async (foundAreaLink, apiRep) => {

    try {
        discordController.sendWebhook(foundAreaLink, apiRep);
        return true
    } catch (error) {
        console.log(error);
        return false
    }
}

const createNewDiscordSubService = async (accessToken, serviceIdRef, tokenType, discordServerName, discordServerID, channelName, channelId) => {

    try {
        const { id } = await getDiscordApiRep(tokenType, accessToken, 'get', 'users/@me', null);
        const newDiscordService = new discordModel({serviceIdRef, tokenType, discordUserID: id, discordServerName, discordServerID, discordChannelName, discordChannelID});
        newDiscordService.save(function(err) {
            return false;
        });
        return newDiscordService;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const createWebHook = async (channelId) => {

    try {
        const rep = await getDiscordApiRep("Bot", DiscordBotToken, 'post', 'channels/' + channelId + '/webhooks', {name: "AreaWebhook", avatar: "https://gblobscdn.gitbook.com/spaces%2F-LQMp6pbqRx6A5iwcsfy%2Favatar-1588393559148.png?alt=media"});
        return rep;
    } catch (error) {
        console.log(error);
        return false;
    }

}

exports.setService = async (req, res) => {

    const { userId, serverId, serverName, channelName, channelId } = req.body;

    try {
        let newDiscord;
        const service = await servicesModel.findOne({userIdRef: userId, nameServices: "Discord"});
        if (!service) {
            return res.status(404).json({ error: 'Failed to get user\'s Discord info from DB' });
        };
        const ServiceId = service._id;
        const foundDiscord = await discordModel.findOne({serviceIdRef: ServiceId, discordServerID: serverId, discordChannelID: channelId})
        if (foundDiscord) {
            return res.status(200).json({ success: foundDiscord });
        }
        const emptyDiscord = await discordModel.findOne({serviceIdRef: ServiceId, discordChannelID: null, discordChannelName: null});
        if (!emptyDiscord) {
            newDiscord = await createNewDiscordSubService(accessToken, ServiceId, "bearer", serverName, serverId, channelName, channelId);
            if (!newDiscord) {
                return res.status(404).json({ error: 'Failed to create Trello subservice' });
            }
        } else {
            newDiscord = await emptyDiscord.updateOne({discordServerName: serverName, discordServerID: serverId, discordChannelID: channelId, discordChannelName: channelName});
            newDiscord = await discordModel.findOne({_id: emptyDiscord._id});
        }
        const webhookRep = await createWebHook(channelId);
        if (!webhookRep) {
            res.status(401).json({ error: 'Failed to create webhook' });
        }
        const { _id } = newDiscord;
        newDiscord = await newDiscord.updateOne({_id: _id, webhookID: webhookRep.id, webhookToken: webhookRep.token, webhookName: webhookRep.name});
        newDiscord = await discordModel.findOne({_id: _id});
        res.status(200).json({ success: newDiscord });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.createDiscordService = async (res, serviceIdRef, tokenType, accessToken, serverName, serverId) => {

    try {
        const { id } = await getDiscordApiRep(tokenType, accessToken, 'get', 'users/@me', null);
        const newDiscordService = new discordModel({serviceIdRef, tokenType, discordUserID: id, discordServerID: serverId, discordServerName: serverName });
        newDiscordService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service Discord Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}