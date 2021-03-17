const axios = require('axios');

const servicesModel = require('../../models/servicesModel');
const gitlabModel = require('../../models/gitlabServicesInfoModel');
const areaModel = require('../../models/servicesLinkModel');
const areaController = require('../../controllers/areaController');
const { gitlabActionDico } = require('../../config/Gitlab/areaGitlabDictionary');



const gitlabEndPoint = "https://gitlab.com/api/v4/";


const getGitlabInfoUser = async (tokenType, accessToken, action, route="user", object={}) => {
    try {
        const gitlabUserInfo = await axios(gitlabEndPoint + route, {
            method : action,
            headers : {
                Authorization : tokenType + " " + accessToken
            },
            data : object
        });
        return gitlabUserInfo.data;
    } catch (error) {
        console.log(error);
        return null;
    }
}

exports.findGitlabArea = async (formatedData) => {
    try {
        const gitlabService = (await gitlabModel.find({
            gitlabRepoID: formatedData.model.id,
            gitlabRepoName: formatedData.model.name,
        })).forEach (async function (rep) {
            const listArea = (await areaModel.find({
                Services : {$elemMatch : { serviceActionId: rep._id.toString(), actionType: gitlabActionDico.get(formatedData.action.type)}}
        })).forEach (async function (foundArea) {
            if (areaController.dispatchReaction(foundArea, formatedData) === false)
            {
                return false;
            }
        })})
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const createIssue = async(foundAreaLink, apiRep) => {
    try {
        const serviceGitlab = await gitlabModel.findOne({_id: foundAreaLink.Services[1].serviceReactionId});
        if (!serviceGitlab) {
            return false;
        }
        const service = await servicesModel.findOne({_id: serviceGitlab.serviceIdRef});
        if (!service) {
            return false;
        }
        const {accessToken} = service;
        const rep = await getGitlabInfoUser("Bearer" , accessToken , "POST","projects/" + serviceGitlab.gitlabRepoID + "/issues", {title: apiRep} );
        return true
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.dispatchGitlabReaction = async (foundAreaLink, apiRep) => {

    var switcher = (function() {
        var _reactionType, _functions = {
            "Create Issue" : function(){createIssue(foundAreaLink, apiRep.action.title);},
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