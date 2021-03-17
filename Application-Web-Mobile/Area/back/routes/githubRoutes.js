const githubController = require('../controllers/githubServiceController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
    // Get list of repo of user, no parse
    router.get('/GitHub/getUserRepo', isAuth, (req, res) => {
        githubController.getUserRepo(req, res);
    });
    
    router.post('/GitHub/setService', [
        check('githubUserName', 'githubUserName cannot be empty').notEmpty(),
        check('githubRepoName', 'githubRepoName cannot be empty').notEmpty(),
        check('events', 'events cannot be empty').notEmpty()
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty())
        {
            return res.status(400).send({ error: errors.errors[0].msg});
        }
        githubController.setGithubService(req, res);
    });

    router.all('/GitHub/hook', (req, res) => {
        githubController.githubHook(req, res);
    });
};