const mongoose = require('mongoose');

const sendgridServiceInfoSchema = new mongoose.Schema({
    serviceIdRef: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
    },
    object: {
        type: String,
        required: false,
    },
    message: {
        type: String,
        required: false,
    },
    recipientEmail : {
        type: String,
        required: false,
    }
}, { timestamps: true });

module.exports = mongoose.model('sendgridServiceInfo', sendgridServiceInfoSchema);