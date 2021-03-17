/**
 * Action Reaction Controller
*/

const axios = require('axios');

const areaModel = require('../models/servicesLinkModel');

const trelloController = require('../controllers/trelloArea/trelloAreaController');
const gitlabController = require('../controllers/gitlabArea/gitlabAreaController');
const githubController = require('../controllers/githubServiceController');
const discordController = require('../controllers/discordServiceControler');
const slackController = require('../controllers/slackServiceController');
const twilioController = require('../controllers/twilioServiceController');

const slackReaction = require('../controllers/Reactions/slackReaction');
const sendgridReaction = require('../controllers/Reactions/sendgridReaction');


exports.dispatchReaction = async (foundAreaLink, apiRep) => {
    var switcher = (function() {
        var _reactionType, _functions = {
            "Trello" : function(){trelloController.dispatchTrelloReaction(foundAreaLink, apiRep);},
            "GitLab" : function(){gitlabController.dispatchGitlabReaction(foundAreaLink, apiRep);},
            "GitHub" : function(){githubController.dispatchGithubReaction(foundAreaLink, apiRep);},
            "Discord": function(){discordController.postMessage(foundAreaLink, apiRep)},
            "Slack": function(){slackReaction.postMessage(foundAreaLink, apiRep)},
            "Twilio" : function(){twilioController.sendSMS(foundAreaLink, apiRep)},
            "SendGrid" : function(){sendgridReaction.sendEmail(foundAreaLink, apiRep)},
            default: function(){throw('reactionType invalid');},
        }
        return function(reactionType){ _functions[_reactionType=reactionType] ? _functions[reactionType](foundAreaLink, apiRep) : _functions.default(); };
    })();

    try {
        switcher(foundAreaLink.Services[1].serviceReactionName);
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.deleteArea = async (req, res) => {
    const {serviceId} = req.params

    try {
        const area = await areaModel.deleteOne({_id: serviceId});
        if (!area) {
            res.status(404).json({ error: 'Failed to delete your area' });
        }
        res.status(200).json({ success: 'Deleted successfuly' });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.setLink = async (req, res) => {
    const { userId, title,serviceActionName ,serviceActionId, actionType, serviceReactionName, serviceReactionId, reactionType } = req.body;

    try {
        let cpyTitle = "";
        const object = [{serviceActionName: serviceActionName, serviceActionId: serviceActionId, actionType: actionType},
            {serviceReactionName: serviceReactionName, serviceReactionId: serviceReactionId, reactionType: reactionType}];
        const trello = await areaModel.findOne({ userIdRef: userId, Services: object});
        if (trello) {
            return res.status(404).json({ error: 'Link already make' });
        };
        if(title == null) {
            cpyTitle = "No title for this area"
        } else {
            cpyTitle = title;
        }
        const newLink = new areaModel({userIdRef: userId, Services: object, title: cpyTitle});
        newLink.save(function (err) {
            if (err) { return res.status(500).json({ error: err.message }); }
        });
        return res.status(200).json({ success: 'Link created' });
    } catch (error) {
        console.log(error);
        return res.status(500).json({ error: 'An error has occurred' });
    }
}