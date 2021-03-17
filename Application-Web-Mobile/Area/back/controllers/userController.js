/**
 * User Controller
*/

const bcrypt = require('bcrypt');
const crypto = require('crypto');
const jwt = require('jsonwebtoken');
const userModel = require('../models/userModel');
const areaModel = require('../models/servicesLinkModel');
const trelloController = require('../controllers/trelloServiceController');
const confirmationToken = require('../models/confirmationTokenModel');
const { secretToken } = require('../config/config');
const sgMail = require('@sendgrid/mail');

const { sendgridKey, urlConfirm } = require('../config/config');

function sendConfirmationEmail(email, confirmationLink) {
    sgMail.setApiKey(sendgridKey);

    const msg = {
        to: email, // Change to your recipient
        from: 'melvyn.fontaine@epitech.eu', // Change to your verified sender
        subject: 'Confirm your email adress',
        html: "Hello,<br> Please Click on the link to verify your email.<br><a href=" + confirmationLink + ">Click here to verify</a>",
    }
    sgMail.send(msg).then(() => {
        console.log('Email sent')
    }).catch((error) => {
        console.error(error)
        return (-1);
    })
    return (0);
}

exports.getLinks = async (req, res) => {
    const { userId } = req.body

    var switcher = ( function() {
        var _nameService, _idService, _functions = {
            "GitLab" : function(_idService){console.log('CALL GITLAB GET SERVICE INFO')},
            "Trello" : async function(_idService){
                const tmp = await trelloController.getInfoService(_idService);
                return tmp.trelloBoardName;
            },
            default: function(){throw('nameServices invalid');},
        }
        return async function(nameService, idService){ return await _functions[_nameService=nameService] ? _functions[nameService](_idService=idService) : _functions.default(); };
    })();

    try {
        let listLinks = [];
        const links = await areaModel.find({userIdRef: userId});
        for (const info of links) {
            const nameContainerAction = await switcher(info.Services[0].serviceActionName, info.Services[0].serviceActionId);
            const nameContainerReaction = await switcher(info.Services[1].serviceReactionName, info.Services[1].serviceReactionId);
            if (!nameContainerAction || !nameContainerReaction) {
                res.status(503).json({ error: 'Failed to get information link' });
            }
            listLinks.push(
                {
                    serviceActionName: info.Services[0].serviceActionName,
                    linkBoardActionName: nameContainerAction,
                    serviceActionType: info.Services[0].actionType,
                    serviceReactionName: info.Services[1].serviceReactionName,
                    linkBoardReactionName: nameContainerReaction,
                    serviceReactionType: info.Services[1].reactionType,
                }
            );
        }
        res.status(200).json({ success: listLinks });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.confirmation = async (req, res) => {
    const {userConfirmationToken} = req.body;

    const tokenInfo = await confirmationToken.findOne({ token: userConfirmationToken });
    if (!tokenInfo) {
        return res.status(401).send({ error: 'We were unable to find a valid token. Your token my have expired.' });
    }
    const userInfo = await userModel.findOne({_id: tokenInfo._userIdRef});
    if (!userInfo) {
        return res.status(401).send({ error: 'We were unable to find a user for this token.' });
    }
    if (userInfo.isActivated) {
        return res.status(401).send({ error: 'You are already verified.' });
    }
    userInfo.isActivated = true;
    userInfo.save(function (err) {
        if (err) { return res.status(500).send({ msg: err.message }); }
    })
    res.status(200).json({ success: 'Email confirmed' });
}

exports.register = async (req, res) => {
    const { email, password, pseudo } = req.body;

    try {
        const cryptedPassword = await bcrypt.hash(password, 10);
        const foundUser = await userModel.findOne({ email });
        if (foundUser) {
            return res.status(403).json({ error: 'Email already use!' });
        }
        const newUser = new userModel({ pseudo, email, password: cryptedPassword, isActivated: false });
        const newConfirmationToken = new confirmationToken({ _userIdRef: newUser._id, token: crypto.randomBytes(16).toString('hex') });
        newUser.save(function (err) {
            if (err) { res.status(500).json({ error: err.message }); }
        });
        newConfirmationToken.save(function (err) {
            if (err) { res.status(500).json({ error: err.message }); }
        });
        var confirmationLink = urlConfirm + 'user\/confirmation\/' + newConfirmationToken.token;
        if (sendConfirmationEmail(email, confirmationLink) == -1) {
            res.status(500).json({ error: 'An error has occurred' });
        }
        res.status(200).json({ success: 'Welcome !' });
    } catch (e) {
        console.log(e);
        res.status(500).json({ error: 'An error has occurred' });
    }
};

exports.login = async (req, res) => {
    const { email, password } = req.body;

    try {
        const getUser = await userModel.findOne({ email });

        if (!getUser) {
            return res.status(404).json({ error: 'Address mail not found!' });
        }
        const verifyPassword = await bcrypt.compare(password, getUser.password);

        if (!verifyPassword) {
            return res.status(401).json({ error: 'Bad password/email!' });
        }

        if (!getUser.isActivated) {
            return res.status(401).json({ error: 'Your account has not been verified.' });
        }

        const token = jwt.sign({ id: getUser.id }, secretToken, {
            expiresIn: 86400 * 30 * 7 * 4, // expires in 1 month
        });
        res.send({ token });
    } catch (e) {
        console.log(e);
        res.status(500).send('An error has occurred');
    }
};

exports.registerFirebase = async (req, res) => {
    const { email, firebaseId } = req.body;

    try {
        const foundUser = await userModel.findOne({ email });
        if (foundUser) {
            return res.status(403).json({ error: 'Email already use!' });
        }
        const newUser = new userModel({ email, firebaseId, isActivated: false });
        const newConfirmationToken = new confirmationToken({ _userIdRef: newUser._id, token: crypto.randomBytes(16).toString('hex') });
        newUser.save(function (err) {
            if (err) { res.status(500).json({ error: err.message }); }
        });
        newConfirmationToken.save(function (err) {
            if (err) { res.status(500).json({ error: err.message }); }
        });
        var confirmationLink = urlConfirm + 'user\/confirmation\/' + newConfirmationToken.token;
        if (sendConfirmationEmail(email, confirmationLink) == -1) {
            res.status(500).json({ error: 'An error has occurred' });
        }
        res.status(200).json({ success: 'Welcome !' });
    } catch (e) {
        console.log(e);
        res.status(500).json({ error: 'An error has occurred' });
    }
};

exports.loginFirebase = async (req, res) => {
    const { email, firebaseId, pseudo } = req.body;

    try {
        let token;
        const cryptedPassword = await bcrypt.hash(firebaseId, 10);
        const getUser = await userModel.findOne({email});
        if (!getUser) {
            const newUser = new userModel({ email, pseudo, password: cryptedPassword, isActivated: true });
            newUser.save(function (err) {
                if (err) {
                    console.log(err.message)
                    return res.status(500).json({ error: err.message });
                }
            });
            token = jwt.sign({ id: newUser._id }, secretToken, {
                expiresIn: 86400 * 30 * 7 * 4, // expires in 1 month
            });
        } else {
            token = jwt.sign({ id: getUser.id }, secretToken, {
                expiresIn: 86400 * 30 * 7 * 4, // expires in 1 month
            });
        }
        res.status(200).json({ token });
    } catch (e) {
        console.log(e);
        res.status(500).send('An error has occurred');
    }
};