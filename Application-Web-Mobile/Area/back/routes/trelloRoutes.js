const trelloController = require('../controllers/trelloServiceController');
const isAuth = require('../middleware/auth');

const { check, validationResult } = require('express-validator');

module.exports = function (router) {
    router.get('/Trello/getBoards', isAuth, (req, res) => {
        trelloController.getBoardUser(req, res);
    });

    router.post('/Trello/setService', [
        check('boardId', 'boardId cannot be empty').notEmpty(),
        check('boardName', 'boardName cannot be empty').notEmpty(),
        check('listName', 'boardId cannot be empty').notEmpty(),
        check('listId', 'boardId cannot be empty').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        trelloController.setTrelloService(req, res);
    });

    router.get('/Trello/getListOfBoard/:boardId', [
        check('boardId', 'boardId cannot be empty').notEmpty(),
    ], isAuth, (req, res) => {
        const errors = validationResult(req);
        if (!errors.isEmpty()) {
            return res.status(400).send({ error: errors.errors[0].msg });
        }
        trelloController.getListOfBoard(req, res);
    });

    router.all('/Trello/hook', (req, res) => {
        trelloController.trelloHook(req, res);
    });
};