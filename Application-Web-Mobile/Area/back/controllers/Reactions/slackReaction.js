const axios = require('axios');

const slackModel = require('../../models/slackServicesInfoModel');

exports.postMessage = async (foundAreaLink, apiRep) => {
    try {
        const slackService = await slackModel.findById(foundAreaLink.Services[1].serviceReactionId);
        if (!slackService) {
            return false;
        }
        const slackPostMessage = await axios(slackService.slackWebhookUri, {
            method: 'post',
            data: {
                "text": apiRep.service + ": " + foundAreaLink.Services[0].actionType,
            }
        });
        return true
    } catch (error) {
        console.log(error);
        return false
    }
}