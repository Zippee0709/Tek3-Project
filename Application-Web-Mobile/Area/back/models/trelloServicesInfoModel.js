const mongoose = require('mongoose');

const trelloServiceInfoSchema = new mongoose.Schema({
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
    trelloBoardName: {
        type: String,
        required: false,
    },
    trelloBoardID: {
        type: String,
        required: false,
    },
    trelloListName: {
        type: String,
        required: false,
    },
    trelloListID: {
        type: String,
        required: false,
    },
    trelloUserID: {
        type: String,
        required: false,
    },
}, { timestamps: true });

module.exports = mongoose.model('trelloServiceInfo', trelloServiceInfoSchema);