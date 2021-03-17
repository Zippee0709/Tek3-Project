const mongoose = require('mongoose');

const slackServiceInfoSchema = new mongoose.Schema({
    serviceIdRef: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
    },
    tokenType : {
        type: String,
        required: true,
    },
    description: {
      type: String,
      required: false,
    },
    slackUserID: {
        type: String,
        required: false,
    },
    slackChannelToMessageID: {
        type: String,
        required: false,
    },
    slackWebhookUri: {
        type: String,
        required: false,
    },
    slackTeamName: {
        type: String,
        required: false,
    },
    slackTeamId: {
        type: String,
        required: false,
    },
    slackBotId: {
        type: String,
        required: false,
    }
}, { timestamps: true });

module.exports = mongoose.model('slackServiceInfo', slackServiceInfoSchema);