const mongoose = require('mongoose');

const discordServiceInfoSchema = new mongoose.Schema({
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
    discordServerID: {
        type: String,
        required: false,
    },
    discordServerName: {
        type: String,
        required: false,
    },
    discordChannelName: {
        type: String,
        required: false,
    },
    discordChannelID: {
        type: String,
        required: false,
    },
    discordUserID: {
        type: String,
        required: false,
    },
    webhookID: {
        type: String,
        required: false,
    },
    webhookToken: {
        type: String,
        required: false,
    },
    webhookName: {
        type: String,
        required: false,
    }
}, { timestamps: true });

module.exports = mongoose.model('discordServiceInfo', discordServiceInfoSchema);