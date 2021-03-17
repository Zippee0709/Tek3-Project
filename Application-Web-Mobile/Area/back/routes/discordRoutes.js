const discordController = require('../controllers/discordServiceControler');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {

    router.get('/Discord/getChannels/:discordServerID', [
        check('discordServerID', 'discordServerID cannot be blank!').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        discordController.getChannels(res, req);
    });

    router.get('/Discord/getServer', isAuth, (req, res) => {
        discordController.getServer(req, res);
    });

    router.post('/Discord/setService', [
        check('serverId', 'serverId cannot be empty').notEmpty(),
        check('serverName', 'serverName cannot be empty').notEmpty(),
        check('channelName', 'channelName cannot be empty').notEmpty(),
        check('channelId', 'channelId cannot be empty').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        discordController.setService(req, res);
    });
};