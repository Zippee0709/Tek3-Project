/**
 * Twilio Service Controller
*/

const {TwillioSid, TwillioToken} = require('../config/config');

const twilio = require('twilio')(TwillioSid, TwillioToken);

const twilioModel = require('../models/twilioServicesInfoModel');
const serviceModel = require('../models/servicesModel');

exports.createTwilioService = async (res, serviceIdRef, phoneNumber) => {

    try {
        const newTwilioService = new twilioModel({serviceIdRef, phoneNumber});
        newTwilioService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service Twilio Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await twilioModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.sendSMS = async (foundAreaLink, apiRep, from = '+1 737 259 5371') => {

    const service = await serviceModel.findOne({userIdRef: foundAreaLink.userIdRef, nameServices: "Twilio"});
    if (!service) {
        throw('Twilio: Failed to find user info in db')
    }
    const twilioService = await twilioModel.findOne({serviceIdRef: service._id});
    if (!twilioService) {
        throw('Twilio: failed to get the num')
    }
    const recipient = twilioService.phoneNumber;
    const message = apiRep.service + ": " + foundAreaLink.Services[0].actionType;
    twilio.messages.create({body: message, from, to: recipient})
    .then(message => res.send(message.sid))
    .catch(e => res.status(500).send('An error has occurred'));
}