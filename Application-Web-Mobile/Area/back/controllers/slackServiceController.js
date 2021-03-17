/**
 * Slack Services Controller
*/

const axios = require('axios');

const servicesModel = require('../models/servicesModel');
const slackModel = require('../models/slackServicesInfoModel');
const areaModel = require('../models/servicesLinkModel');

const areaController = require('../controllers/areaController');
// const slackAreaControler = require('../controllers/slackArea/slackAreaController');
const { slackActionDico } = require('../config/Slack/areaSlackDictionary');

const { SlackClientId, SlackClientSecret, SlackBotToken } = require('../config/config');

const slackEndPoint = 'https://slack.com/api/';

const getSlackApiRep = async(action, route, accessToken, object={}) => {
    try {
        const slackRep = await axios({
            method: action,
            url: slackEndPoint + route,
            data: object,
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
                "Authorization": "Bearer " + accessToken,
            }
        });
        console.log("Rep: ", slackRep.data);
        return slackRep.data;
    } catch (error) {
        console.log(error);
        return null
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await slackModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.findSlackArea = async (formatedData) => {
    try {
        const foundSlack = await slackModel.find({slackBotId: formatedData.slack.bot_id, slackTeamId: formatedData.model.id});
        if (!foundSlack) {
            throw('Cannot find user in db');
        }
        for (const infos of foundSlack) {
            const area = await areaModel.find( {Services: {$elemMatch: { serviceActionId: infos._id.toString(), actionType: slackActionDico.get(formatedData.action.type) }}})
            for (const foundArea of area) {
                if (areaController.dispatchReaction(foundArea, formatedData) === false) {
                    return false;
                }
            }
        }
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.getRegisteredTeam = async (req, res) => {
    const { userId } = req.body;

    try {
        let registeredTeam = [];
        const services = await servicesModel.findOne({userIdRef: userId, nameServices: 'Slack'})
        if (!services) {
            return res.status(404).json({ error: 'Cannot find user in DB' });
        }
        const id = services._id;
        const slackService = await slackModel.find({serviceIdRef: id});
        if (!slackService) {
            return res.status(404).json({ error: 'Cannot find user\'s slack info' });
        }
        for (const info of slackService) {
            registeredTeam.push({
                idService: info._id,
                serverName: info.slackTeamName,
            })
            console.log(info);
        }
        res.status(200).json({ success: registeredTeam });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getListOfBoard = async (req, res) => {
    const { userId, boardId } = req.body;

    try {
        const trello = await servicesModel.findOne({ userIdRef: userId, nameServices: "Trello"});
        if (!trello) {
            return res.status(404).json({ error: 'Failed to get user\'s Trello info from DB' });
        }
        const {accessToken} = trello;
        const TrelloRes = await getTrelloApiRep('get', `boards/${boardId}/lists`,{token:accessToken, key:TrelloApiKey});
        if (!TrelloRes) {
            return res.status(403).json({ error: 'Try to get Trello user info from API failed'});
        }
        res.status(200).json({ success: TrelloRes });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getUserChannels = async (req, res) => {
    const { userId } = req.body;

    try {
        const slack = await servicesModel.findOne({userIdRef: userId, nameServices: "Slack"});
        if (!slack) {
            return res.status(404).json({ error: 'Failed to get user\'s Slack info from DB' });
        }
        const {accessToken} = slack;
        const userChannels = await getSlackApiRep('post', 'conversations.list', accessToken, null);
        if (!userChannels) {
            return res.status(403).json({ error: 'Try to get Slack user info from API failed'});
        }
        res.status(200).json({ success: userChannels });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.createSlackService = async (res, serviceIdRef, tokenType, slackWebhookUri, accessToken, slackBotId, teamName) => {
    try {
        const userInfo = await getSlackApiRep('get', 'users.identity', accessToken);
        const newSlackService = new slackModel({serviceIdRef, tokenType, slackUserID: userInfo.user.id, slackWebhookUri: slackWebhookUri, slackTeamId: userInfo.team.id, slackBotId, slackTeamName: teamName});
        newSlackService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service Slack Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}