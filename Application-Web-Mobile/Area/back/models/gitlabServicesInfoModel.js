const mongoose = require('mongoose');

const gitlabServiceInfoSchema = new mongoose.Schema({
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
    gitlabRepoID: {
        type: String,
        required: false,
    },
    gitlabRepoName: {
        type: String,
        required: false,
    },
    gitlabUserID: {
        type: String,
        required: false,
    },
}, { timestamps: true });

module.exports = mongoose.model('gitlabServiceInfo', gitlabServiceInfoSchema);