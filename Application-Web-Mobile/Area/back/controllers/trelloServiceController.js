/**
 * Trello Services Controller
*/

const axios = require('axios');

const servicesModel = require('../models/servicesModel');
const trelloModel = require('../models/trelloServicesInfoModel');
const trelloAreaControler = require('../controllers/trelloArea/trelloAreaController');

const { TrelloApiKey, TrelloHookURI } = require('../config/config');

const trelloEndPoint = 'https://api.trello.com/1/';

const getTrelloApiRep = async(action, route, object={}) => {
    try {
        const trelloRep = await axios(trelloEndPoint + route, {
            method : action,
            params : object
        });
        return trelloRep.data;
    } catch (error) {
        console.log(error);
        return null
    }
}

const createNewTrelloSubService = async (accessToken, serviceIdRef, tokenType, trelloBoardName, trelloBoardID, trelloListName, trelloListID) => {

    try {
        const { id } = await getTrelloApiRep('get', 'members/me', { token:accessToken, key:TrelloApiKey } );
        const newTrelloService = new trelloModel({ serviceIdRef, tokenType, trelloUserID: id, trelloBoardName, trelloBoardID, trelloListName, trelloListID });
        newTrelloService.save(function(err) {
            return false;
        });
        return newTrelloService;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const formatData = async (apiRep) => {

    try {
        const formatedData = {
            service : "Trello",
            model: {
                id: apiRep.model ? apiRep.model.id : null ,
                name: apiRep.model.name,
                desc: apiRep.model.desc,
                url: apiRep.model.url,
            },
            action: {
                id: apiRep.action.id,
                idCreator: apiRep.action.idMemberCreator,
                type: apiRep.action.type,
                title: apiRep.action.data.card ? apiRep.action.data.card.name : null,
                data: apiRep.action.data,
            },
        };
        return formatedData;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await trelloModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.trelloHook = async (req, res) => {
    try {
        const formatedData = await formatData(req.body);
        if (trelloAreaControler.findTrelloArea(formatedData) == false) {
            res.status(502).json({ error: 'Failed to find Action/Reaction' });
        }
        res.status(200).send({ success: "Thanks for the webhook" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.setTrelloService = async (req, res) => {
    const { userId, boardName, boardId, listName, listId } = req.body;

    try {
        const service = await servicesModel.findOne({ userIdRef: userId, nameServices: "Trello"});
        let newTrello;
        if (!service) {
            return res.status(404).json({ error: 'Failed to get user\'s Trello info from DB' });
        }
        const {accessToken} = service;
        const ServiceId = service._id;
        const foundTrello = await trelloModel.findOne({serviceIdRef: ServiceId, trelloBoardID: boardId, trelloListID: listId});
        if (foundTrello) {
            return res.status(200).json({ success: foundTrello });
        };
        const emptyTrello = await trelloModel.findOne({serviceIdRef: ServiceId, trelloBoardID: null});
        if (!emptyTrello) {
            newTrello = await createNewTrelloSubService(accessToken, ServiceId, "bearer", boardName, boardId, listName, listId);
            if (!newTrello) {
                return res.status(404).json({ error: 'Failed to create Trello subservice' });
            }
        } else {
            newTrello = await emptyTrello.updateOne({trelloBoardName: boardName, trelloBoardID: boardId, trelloListName: listName, trelloListID: listId});
            newTrello = await trelloModel.findOne({_id: emptyTrello._id});
        }
        const hookRep = await getTrelloApiRep('post', "tokens/" + accessToken + "/webhooks/?key=" + TrelloApiKey , {description: "My first webhook", callbackURL: TrelloHookURI + "service/Trello/hook", idModel: boardId,});
        res.status(200).json({ success: newTrello });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getListOfBoard = async (req, res) => {
    const { userId } = req.body;
    const { boardId } = req.params;

    try {
        const trello = await servicesModel.findOne({ userIdRef: userId, nameServices: "Trello"});
        if (!trello) {
            return res.status(404).json({ error: 'Failed to get user\'s Trello info from DB' });
        }
        const {accessToken} = trello;
        const TrelloRes = await getTrelloApiRep('get', `boards/${boardId}/lists`,{token:accessToken, key:TrelloApiKey});
        if (!TrelloRes) {
            return res.status(403).json({ error: 'Try to get Trello user info from API failed'});
        }
        res.status(200).json({ success: TrelloRes });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.getBoardUser = async (req, res) => {
    const { userId } = req.body;

    try {
        const trello = await servicesModel.findOne({ userIdRef: userId, nameServices: "Trello"});
        if (!trello) {
            return res.status(404).json({ error: 'Failed to get user\'s Trello info from DB' });
        }
        const {accessToken} = trello;
        const TrelloRes = await getTrelloApiRep('get', "/members/me/boards?fields=name",{token:accessToken, key:TrelloApiKey});
        if (!TrelloRes) {
            return res.status(403).json({ error: 'Try to get Trello user info from API failed'});
        }
        res.status(200).json({ success: TrelloRes });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.createTrelloService = async (res, serviceIdRef, tokenType, accessToken) => {

    try {
        const { id, username , fullName, avatarUrl , email } = await getTrelloApiRep('get', 'members/me',{token:accessToken, key:TrelloApiKey});
        const newTrelloService = new trelloModel({serviceIdRef, tokenType, trelloUserID: id});
        newTrelloService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service Trello Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}
