const mongoose = require('mongoose');

const githubServicesInfoSchema = new mongoose.Schema({
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
    githubUserName: {
        type: String,
        required: false,
    },
    githubRepoName: {
        type: String,
        required: false,
    },
}, { timestamps: true });

module.exports = mongoose.model('githubServiceInfo', githubServicesInfoSchema);