const areaController = require('../controllers/areaController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
    router.post('/setLink', [
        check('serviceActionName', 'serviceActionName cannot be blank!').notEmpty(),
        check('serviceActionId', 'serviceActionId cannot be blank!').notEmpty(),
        check('actionType', 'actionType cannot be blank!').notEmpty(),
        check('serviceReactionId', 'serviceReactionId cannot be blank!').notEmpty(),
        check('serviceReactionName', 'serviceReactionName cannot be blank!').notEmpty(),
        check('reactionType', 'reactionType cannot be blank!').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        areaController.setLink(req, res);
    });

    router.delete('/deleteArea/:serviceId', isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        areaController.deleteArea(req, res);
    });
};