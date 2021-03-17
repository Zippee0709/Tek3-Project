const mongoose = require('mongoose');

const servicesSchema = new mongoose.Schema({
    nameServices: {
      type: String,
      required: true,
    },
    userIdRef: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
    },
    accessToken: {
      type: String,
      required: true,
    },
    refreshToken: {
        type: String,
        required: true,
    },
}, { timestamps: true });

module.exports = mongoose.model('services', servicesSchema);