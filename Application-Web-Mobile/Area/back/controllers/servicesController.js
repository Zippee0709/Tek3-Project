/**
 * Services Controller
*/

const jwt = require('jsonwebtoken');
const userModel = require('../models/userModel');
const servicesModel = require('../models/servicesModel');
const areaModel = require('../models/servicesLinkModel');

const gitlabServiceController = require('../controllers/gitlabServiceController');
const githubServiceController = require('../controllers/githubServiceController');
const trelloServiceController = require('../controllers/trelloServiceController');
const twilioServiceController = require('../controllers/twilioServiceController');
const discordServiceController = require('../controllers/discordServiceControler');
const slackServiceController = require('../controllers/slackServiceController');
const sendgridServiceController = require('../controllers/sendgridServiceController');

const trelloModel = require('../models/trelloServicesInfoModel');
const discordModel = require('../models/discordServiceModel');
const gitlabModel = require('../models/gitlabServicesInfoModel');
const githubModel = require('../models/githubServicesInfoModel');
const slackModel = require('../models/slackServicesInfoModel');
const twilioModel = require('../models/twilioServicesInfoModel');
const sendgridModel = require('../models/sendgridServicesInfoModel');

const {Services} = require('../config/action_reaction');

exports.getAction = async (req, res) => {
    const {Service1, Service2} = req.params;

    try {
        const servicesAction = {};
        servicesAction[Service1.toString()] = [];
        servicesAction[Service2.toString()] = [];
        servicesAction[Service1.toString()].push(Services[Service1.toString()].Action);
        servicesAction[Service2.toString()].push(Services[Service2.toString()].Action);
        res.status(200).json({ servicesAction });
    } catch(e) {
        console.log(e);
        res.status(500).send('An error has occurred');
    }
}

exports.getReaction = async (req, res) => {
    const {Service1, Service2} = req.params;

    try {
        const servicesReaction = {};
        servicesReaction[Service1.toString()] = [];
        servicesReaction[Service2.toString()] = [];
        servicesReaction[Service1.toString()].push(Services[Service1.toString()].Reaction);
        servicesReaction[Service2.toString()].push(Services[Service2.toString()].Reaction);
        res.status(200).json({ servicesReaction });
    } catch(e) {
        console.log(e);
        res.status(500).send('An error has occurred');
    }
}

exports.getServicesDetails = async (req, res) => {

    const { userId } = req.body;
    const { serviceId } = req.params

    var switcher = ( function() {
        var _nameService, _idService, _functions = {
            "GitLab" : async function(_idService){
                const tmp = await gitlabServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef === serviceId) {
                    return tmp.gitlabRepoName;
                } else {
                    return false;
                }
            },
            "Trello" : async function(_idService){
                const tmp = await trelloServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef === serviceId) {
                    return tmp.trelloBoardName;
                } else {
                    return false;
                }
            },
            "Discord" : async function(_idService) {
                const tmp = await discordServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef == serviceId) {
                    return tmp.discordServerName;
                } else {
                    return false;
                }
            },
            "Slack" : async function(_idService) {
                const tmp = await slackServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef == serviceId) {
                    return "Slack workspace";
                } else {
                    return false;
                }
            },
            "GitHub" : async function(_idService) {
                const tmp = await githubServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef == serviceId) {
                    return tmp.githubRepoName;
                } else {
                    return false;
                }
            },
            "Twilio" : async function(_idService) {
                const tmp = await twilioServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef == serviceId) {
                    return tmp.phoneNumber;
                } else {
                    return false;
                }
            },
            "SendGrid" : async function(_idService) {
                const tmp = await sendgridServiceController.getInfoService(_idService);
                if (tmp.serviceIdRef == serviceId) {
                    return tmp.recipientEmail;
                } else {
                    return false;
                }
            },
            default: function(){throw('nameServices invalid');},
        }
        return async function(nameService, idService){ return await _functions[_nameService=nameService] ? _functions[nameService](_idService=idService) : _functions.default(); };
    })();

    try {
        let areaLinks = [];
        const listArea = await areaModel.find({userIdRef: userId});
        if (!listArea) {
            res.status(403).json({ error: "Can't found area link for this services" });
        }
        for (const info of listArea) {
            const containerAction = await switcher(info.Services[0].serviceActionName, info.Services[0].serviceActionId);
            const containerReaction = await switcher(info.Services[1].serviceReactionName, info.Services[1].serviceReactionId);
            if (containerAction != false || containerReaction != false) {
                areaLinks.push(
                    {
                        id: info._id,
                        areaTitle: info.title,
                        serviceActionName: info.Services[0].serviceActionName,
                        serviceActionType: info.Services[0].actionType,
                        serviceReactionName: info.Services[1].serviceReactionName,
                        serviceReactionType: info.Services[1].reactionType,
                    }
                );
            }
        }
        res.status(200).json({ areaLinks });
    } catch (error) {
        console.log(error);
        res.status(500).send('An error has occurred');
    }
}

exports.getServices = async (req, res) => {

    const { userId } = req.body;
    var switcher = ( function() {
        var _nameService, _idService, _functions = {
            "GitLab" : async function(_idService){
                const nbr = await gitlabModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "GitHub" : async function(_idService){
                const nbr = await githubModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "Trello" : async function(_idService){
                const nbr = await trelloModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "Discord" : async function(_idService) {
                const nbr = await discordModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "Slack" : async function(_idService) {
                const nbr = await slackModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "Twilio" : async function(_idService) {
                const nbr = await twilioModel.count({serviceIdRef: _idService});
                return nbr;
            },
            "SendGrid" : async function(_idService) {
                const nbr = await sendgridModel.count({serviceIdRef: _idService});
                return nbr;
            },
            default: function(){throw('nameServices invalid');},
        }
        return async function(nameService, idService){ return await _functions[_nameService=nameService] ? _functions[nameService](_idService=idService) : _functions.default(); };
    })();

    try {
        let tmp = [];
        let response = [];
        const servicesUser = await servicesModel.find({userIdRef: userId});
        for (const info of servicesUser) {
            const nbrAreaService = await switcher(info.nameServices, info._id);
            tmp.push({
                id: info._id,
                name: info.nameServices,
                nbrArea: nbrAreaService,
            });
        }
        response.push({user: tmp});
        response.push({server: Services.List})
        res.status(200).send({servicesList: response});
    } catch(e) {
        console.log(e);
        res.status(500).send('An error has occurred');
    }
}

exports.createService = async (req, res) => {
    const { nameServices, userId, accessToken, refreshToken, tokenType } = req.body;
    const { slackWebhookUri, slackBotId, teamName } = req.body; // slack only
    const { discordServerName, discordServerId } = req.body; // discord only
    const { phoneNumber } = req.body; // twillio only

    var switcher = (function() {
        var _nameService, _idService, _functions = {
            "GitLab" : function(){gitlabServiceController.createGitlabService(res, _idService, tokenType, accessToken);},
            "GitHub" : function(){githubServiceController.createGithubService(res, _idService, tokenType, accessToken);},
            "Trello" : function(){trelloServiceController.createTrelloService(res, _idService, tokenType, accessToken);},
            "Discord" : function(){discordServiceController.createDiscordService(res, _idService, tokenType, accessToken, discordServerName, discordServerId);},
            "Slack" : function(){slackServiceController.createSlackService(res, _idService, tokenType, slackWebhookUri, accessToken, slackBotId, teamName);},
            "Twilio" : function(){twilioServiceController.createTwilioService(res, _idService, phoneNumber)},
            "SendGrid" : function(){sendgridServiceController.createSendgridService(res, _idService)},
            default: function(){throw('nameServices invalid');},
        }
        return function(nameService, idService, res){ _functions[_nameService=nameService] ? _functions[nameService](_idService=idService) : _functions.default(); };
    })();

    try {
        const foundService = await servicesModel.findOne({ nameServices, userIdRef: userId});
        if (foundService) {
            return res.status(403).json({ error: nameServices + " service already created for the user" });
        }
        const newService = new servicesModel({ nameServices, userIdRef: userId, accessToken, refreshToken });
        switcher(nameServices, newService._id);
        newService.save(function(err) {
            if (err) {res.status(500).json({ error: err.message });}
        });
    } catch (e) {
        console.log(e);
        res.status(500).json({ error: 'An error has occurred' });
    }
};
