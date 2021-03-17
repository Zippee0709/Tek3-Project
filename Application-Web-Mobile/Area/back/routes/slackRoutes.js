const slackController = require('../controllers/slackServiceController');
const slackActionControler = require('../controllers/Actions/slackActions');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
    router.get('/Slack/getChannels', isAuth, (req, res) => {
        slackController.getUserChannels(req, res);
    });

    router.get('/Slack/getRegisteredTeam', isAuth, (req, res) => {
        slackController.getRegisteredTeam(req, res);
    });

    router.post('/Slack/actionsTriggered', (req, res) => {
        console.log("Receive request");
        slackActionControler.slackActionsTriggered(req, res);
    });

    router.all('/Slack/hook', (req, res) => {
        trelloController.trelloHook(req, res);
    });
};