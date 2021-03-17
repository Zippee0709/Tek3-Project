const twilioController = require('../controllers/twilioServiceController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
    router.post('/Twilio/createService', [
        check('phoneNumber', 'phoneNumber cannot be empty').notEmpty(),
    ] , isAuth,(req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        twilioController.createTwilioService(req, res);
    });
};