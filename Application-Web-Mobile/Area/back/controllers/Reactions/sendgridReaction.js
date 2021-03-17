const axios = require('axios');

const { sendgridKey, senderSendGrid } = require('../../config/config');

const sendgridEndPoint = "https://api.sendgrid.com/v3/mail/send";

const sendgridModel = require('../../models/sendgridServicesInfoModel');

const sgMail = require('@sendgrid/mail');

exports.sendEmail = async (foundAreaLink, apiRep) => {
    try {
        sgMail.setApiKey(sendgridKey);

        const sendgrindService = await sendgridModel.findById(foundAreaLink.Services[1].serviceReactionId);
        if (!sendgrindService) {
            return false;
        }
        const msg = {
            to: sendgrindService.recipientEmail,
            from: senderSendGrid,
            subject: sendgrindService.object,
            text: sendgrindService.message,
        }
        sgMail.send(msg).then(() => {
            console.log('Email sent')
        }).catch((error) => {
            console.error(error)
            return (-1);
        })
        return 0;
    } catch (error) {
        console.log(error);
        return null;
    }
}