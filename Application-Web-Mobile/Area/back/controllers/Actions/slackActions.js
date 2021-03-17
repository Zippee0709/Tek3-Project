const axios = require('axios');

const slackController = require('../../controllers/slackServiceController');

const formatData = async (apiRep) => {

    try {
        const formatedData = {
            service : "Slack",
            model: {
                id: apiRep.team_id,
                name: "Slack server",
                desc: "a description",
                url: null,
            },
            action: {
                id: apiRep.event_id,
                idCreator: apiRep.event.user,
                type: apiRep.event.type,
                title: apiRep.event.text,
                data: null,
            },
            slack : {
                bot_id: apiRep.authorizations[0].user_id,
            }
        };
        return formatedData;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.slackActionsTriggered = async (req, res) => {
    console.log("Started");

    let event = req.body.event ? req.body.event : null; // NOTE: idk why mais l'opérateur ?. ne fonctionne pas
    let eventType = event ? event.type : null;
    let foundAreaLink = null;

    try {
        const formatedData = await formatData(req.body);
        if (!req.body.challenge) {
            const rep = await slackController.findSlackArea(formatedData);
            if (!rep) {
                return res.status(404).json({error: "Failed to get user info in DB"});
            }
            return res.status(200).json({success: "message posted"});
        }
        return res.status(200).json({ challenge: req.body.challenge }); // NOTE: pour le test de l'API Slack, il faut envoyer le challenge
    } catch (error) {
        console.log(error);
        return res.status(500).json({ error: 'An error has occurred' });
    }
}