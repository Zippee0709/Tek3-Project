const gitlabController = require('../controllers/gitlabServiceController');
const isAuth = require('../middleware/auth');
const { check, validationResult } = require('express-validator');


module.exports = function (router) {
    router.get('/gitlab/getRepos', isAuth, (req, res) => {
        gitlabController.getReposUser(req, res);
    });
    router.post('/gitlab/setService', [
        check('projectId', 'projectId cannot be empty').notEmpty(),
        check('projectName', 'projectName cannot be empty').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        gitlabController.setGitlabService(req, res);
    });
    router.all('/gitlab/hook', (req, res) => {
        gitlabController.gitlabHook(req, res);
    });
};