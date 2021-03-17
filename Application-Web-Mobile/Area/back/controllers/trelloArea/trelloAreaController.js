/**
 * Trello Service Action/Reaction Controller
*/

const axios = require('axios');

const servicesModel = require('../../models/servicesModel');
const trelloModel = require('../../models/trelloServicesInfoModel');
const areaModel = require('../../models/servicesLinkModel');
const areaController = require('../../controllers/areaController');

const { trelloActionDico } = require('../../config/Trello/areaTrelloDictionary');

const { TrelloApiKey } = require('../../config/config');

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

const createCard = async (foundAreaLink, nameForCard) => {

    try {
        const serviceTrello = await trelloModel.findOne({_id: foundAreaLink.Services[1].serviceReactionId});
        if (!serviceTrello) {
            return false;
        }
        const service = await servicesModel.findOne({_id: serviceTrello.serviceIdRef});
        if (!service) {
            return false;
        }
        const {accessToken} = service;
        const rep = await getTrelloApiRep('post', 'cards', { token:accessToken, key:TrelloApiKey , idList: serviceTrello.trelloListID, name: nameForCard} );
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const createList = async(foundAreaLink, apiRep) => {
    try {
        const serviceTrello = await trelloModel.findOne({_id: foundAreaLink.Services[1].serviceReactionId});
        if (!serviceTrello) {
            return false;
        }
        const service = await servicesModel.findOne({_id: serviceTrello.serviceIdRef});
        if (!service) {
            return false;
        }
        const {accessToken} = service;
        const rep = await getTrelloApiRep('post', 'lists', { token:accessToken, key:TrelloApiKey, idBoard: serviceTrello.trelloBoardID, name: apiRep.action.title });
        return true
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.dispatchTrelloReaction = async (foundAreaLink, apiRep) => {

    var switcher = (function() {
        var _reactionType, _functions = {
            "Create Card" : function(){createCard(foundAreaLink, apiRep.action.title);},
            "Create List" : function(){createList(foundAreaLink, apiRep);},
            default: function(){throw('reactionType invalid');},
        }
        return function(reactionType){ _functions[_reactionType=reactionType] ? _functions[reactionType]() : _functions.default(); };
    })();

    try {
        switcher(foundAreaLink.Services[1].reactionType);
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}


exports.findTrelloArea = async (formatedData) => {
    try {
        const trelloService = (await trelloModel.find({trelloUserID: formatedData.action.idCreator, trelloBoardID: formatedData.model.id}))
        .forEach( async function (rep) {
            const area = (await areaModel.find( {Services: {$elemMatch: { serviceActionId: rep._id.toString(), actionType: trelloActionDico.get(formatedData.action.type) }}} ))
            .forEach(async function (foundArea) {
                if (areaController.dispatchReaction(foundArea, formatedData) === false) {
                    return false;
                }
            });
        });
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}