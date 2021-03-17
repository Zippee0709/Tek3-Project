/**
 * About.json Controller
*/

const { portBack, hostBack } = require('../config/config');
// const json = require('../config/aboutBase.json');

const areaModel = require('../models/servicesLinkModel');

exports.about = async (req, res) => {
    try {
        res.render('../config/aboutBase.json')
    } catch (error) {
        console.log(error);
        res.status(500).send('An error has occurred');
    }
}