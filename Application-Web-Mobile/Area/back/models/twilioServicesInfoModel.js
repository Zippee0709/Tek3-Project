const mongoose = require('mongoose');

const twilioServiceInfoSchema = new mongoose.Schema({
    serviceIdRef: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
    },
    phoneNumber: {
        type: String,
        required: true,
    },
}, { timestamps: true });

module.exports = mongoose.model('twilioServiceInfo', twilioServiceInfoSchema);