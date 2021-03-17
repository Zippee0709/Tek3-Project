/**
 * Sendgrid Services Controller
*/

const sendgridModel = require('../models/sendgridServicesInfoModel');
const servicesModel = require('../models/servicesModel');

const createNewSendgridSubService = async (serviceIdRef, object, message, recipientEmail) => {

    try {
        const newSendgridService = new sendgridModel({ serviceIdRef, object, message, recipientEmail });
        newSendgridService.save(function(err) {
            return false;
        });
        return newSendgridService;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.setSendgridService = async (req, res) => {

    const { userId, object, message, recipientEmail } = req.body;

    try {
        const service = await servicesModel.findOne({ userIdRef: userId, nameServices: "SendGrid"});
        let newSendgrid;
        if (!service) {
            return res.status(404).json({ error: 'Failed to get user\'s Sendgrid info from DB' });
        }
        const ServiceId = service._id;
        const foundSendgrid = await sendgridModel.findOne({serviceIdRef: ServiceId, object, message, recipientEmail});
        if (foundSendgrid) {
            return res.status(200).json({ success: foundSendgrid });
        };
        const emptySendgrid = await sendgridModel.findOne({serviceIdRef: ServiceId, object: null});
        if (!emptySendgrid) {
            newSendgrid = await createNewSendgridSubService(ServiceId, object, message, recipientEmail);
            if (!newSendgrid) {
                return res.status(404).json({ error: 'Failed to create Trello subservice' });
            }
        } else {
            newSendgrid = await emptySendgrid.updateOne({object: object, message: message, recipientEmail: recipientEmail});
            newSendgrid = await sendgridModel.findOne({_id: emptySendgrid._id});
        }
        res.status(200).json({ success: newSendgrid });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await sendgridModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.createSendgridService = async (res, serviceIdRef) => {
    try {
        const newSendgridService = new sendgridModel({serviceIdRef, object: null, message: null, recipientEmail: null});
        newSendgridService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service SendGrid Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}