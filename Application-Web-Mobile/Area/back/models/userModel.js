const mongoose = require('mongoose');

const userSchema = new mongoose.Schema({
    email: {
      type: String,
      required: true,
    },
    pseudo: {
      type: String,
      required: true,
    },
    password: {
      type: String,
      required: true,
    },
    isActivated: {
      type: Boolean,
      required: true,
    },
    firebaseId: {
      type: String,
      required: false,
    },
}, { timestamps: true });

module.exports = mongoose.model('user', userSchema);