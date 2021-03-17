const aboutController = require('../controllers/aboutController');

module.exports = function (router) {

    router.get('/', (req, res) => {
        aboutController.about(req, res);
    });
};