const userController = require('../controllers/userController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
  router.post('/login', [
    check('email', 'E-mail invalid').isEmail(),
    check('email', 'Email cannot be blank!').notEmpty(),
    check('password', 'Password cannot be blank!').notEmpty(),
  ], (req, res) => {
    const errors = validationResult(req)
    if (!errors.isEmpty()) {
      return res.status(400).json({ error: errors.errors[0].msg })
    }
    userController.login(req, res);
  });

  router.post('/register', [
    check('email', 'E-mail invalid').isEmail(),
    check('email', 'E-mail cannot be blank!').notEmpty(),
    check('pseudo', 'Pseudo cannot be blank!').notEmpty(),
    check('password', 'Password cannot be blank!').notEmpty(),
  ], (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).send({ error: errors.errors[0].msg });
    }
    userController.register(req, res);
  });

  router.post('/loginFirebase', [
    check('email', 'E-mail invalid').isEmail(),
    check('email', 'Email cannot be blank!').notEmpty(),
    check('firebaseId', 'firebaseId cannot be blank!').notEmpty(),
    check('pseudo', 'pseudo cannot be blank!').notEmpty(),
  ], (req, res) => {
    const errors = validationResult(req)
    if (!errors.isEmpty()) {
      return res.status(400).json({ error: errors.errors[0].msg })
    }
    userController.loginFirebase(req, res);
  });

  router.post('/registerFirebase', [
    check('email', 'E-mail invalid').isEmail(),
    check('email', 'E-mail cannot be blank!').notEmpty(),
    check('firebaseId', 'firebaseId cannot be blank!').notEmpty(),
  ], (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).send({ error: errors.errors[0].msg });
    }
    userController.registerFirebase(req, res);
  });

  router.post('/confirmation/', [
    check('userConfirmationToken', 'Token cannot be blank').notEmpty(),
  ], (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).send({ error: errors.errors[0].msg });
    }
    userController.confirmation(req, res);
  });

  router.get('/getLinks', isAuth, (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).send({ error: errors.errors[0].msg });
    }
    userController.getLinks(req, res);
  });
};
