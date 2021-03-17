const servicesController = require('../controllers/servicesController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
  router.post('/createService', [
    check('nameServices', 'nameServices cannot be blank!').notEmpty(),
    check('accessToken', 'accessToken cannot be blank!').notEmpty(),
    check('refreshToken', 'refreshToken cannot be blank!').notEmpty(),
    check('tokenType', 'tokenType cannot be blank!').notEmpty(),
  ], isAuth, (req, res) => {
    console.log(req.body)
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).send({ error: errors.errors[0].msg });
    }
    servicesController.createService(req, res);
  });

  router.get('/getServices', isAuth, (req, res) => {
    servicesController.getServices(req, res);
  });

  router.get('/getServicesDetails/:serviceId', isAuth, (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
        return res.status(400).send({ error: errors.errors[0].msg });
    }
    servicesController.getServicesDetails(req, res);
  });

  router.get('/getServices/action/:Service1/:Service2', isAuth, (req, res) => {
    servicesController.getAction(req, res);
  });

  router.get('/getServices/reaction/:Service1/:Service2', isAuth, (req, res) => {
    servicesController.getReaction(req, res);
  });
};