const mongoose = require('mongoose');

const servicesLinkSchema = new mongoose.Schema({
    Services: {
      type: Array(Object),
      required: true,
    },
    userIdRef: {
        type: mongoose.Schema.Types.ObjectId,
        required: true,
    },
    title: {
      type: String,
      required: false,
    },
}, { timestamps: true });

module.exports = mongoose.model('servicesLink', servicesLinkSchema);