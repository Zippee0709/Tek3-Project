const sendgridController = require('../controllers/sendgridServiceController');
const isAuth = require('../middleware/auth');
const { check, validationResult } = require('express-validator');

module.exports = function (router) {

    router.post('/Sendgrid/setService', [
        check('object', 'boardId cannot be empty').notEmpty(),
        check('message', 'boardName cannot be empty').notEmpty(),
        check('recipientEmail', 'boardId cannot be empty').notEmpty(),
        check('recipientEmail', 'recipientEmail is not an email').isEmail(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        console.log(req.body);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        sendgridController.setSendgridService(req, res);
    });
};