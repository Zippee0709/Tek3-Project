/**
 * Discord Service Action/Reaction Controller
*/

const axios = require('axios');

const servicesModel = require('../../models/servicesModel');
const discordModel = require('../../models/discordServiceModel');
const areaModel = require('../../models/servicesLinkModel');

const areaController = require('../../controllers/areaController');

const { DiscordListThumbnail } = require('../../config/Discord/LinkThumbnail');

const DiscordWebhookEndpoint = "https://discord.com/api/webhooks/";

exports.sendWebhook = async (foundAreaLink, apiRep) => {
    try {
        const discordService = await discordModel.findById(foundAreaLink.Services[1].serviceReactionId);
        if (!discordService) {
            return false;
        }
        const discordWebhookRep = await axios(DiscordWebhookEndpoint + discordService.webhookID + '/' + discordService.webhookToken + '?wait=true', {
            method: 'post',
            data: {
                "username": "Area bot",
                "content": "",
                "embeds": [{
                    "title": apiRep.service + ': ' + foundAreaLink.Services[0].actionType,
                    "thumbnail": {
                        "url": DiscordListThumbnail.get(apiRep.service),
                    },
                    "description": "[Go to " + apiRep.model.name +"](" + apiRep.model.url + ")\n\n",
                    "color": 16777215,
                    "footer": {
                        "text": "Created by AREA",
                        "icon_url": "https://vignette.wikia.nocookie.net/spiderriders/images/d/dd/Discord.png/revision/latest?cb=20171218232913"
                    },
                }],
            },
        });
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}