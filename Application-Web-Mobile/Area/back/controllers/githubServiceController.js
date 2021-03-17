/**
 * GitHub Services Controller
*/

const axios = require('axios');
const servicesModel = require('../models/servicesModel');
const githubModel = require('../models/githubServicesInfoModel');
const { GithubHookURI } = require('../config/config');
const areaModel = require('../models/servicesLinkModel');
const areaController = require('../controllers/areaController');
const { GitHubActionDico } = require('../config/GitHub/areaGitHubDictionary');

const githubEndPoint = "https://api.github.com";

const getGithubApiRep = async(action, route, accessToken, bodyObj={}) => {
    try {
        const githubRep = await axios(githubEndPoint + route, {
            method: action,
            headers: {"Authorization" : `Bearer ${accessToken}`},
            data: JSON.stringify(bodyObj)
        });
        return githubRep.data;
    } catch (error) {
        console.log(error);
        return null;
    }
}

exports.getInfoService = async (idService) => {
    try {
        const service = await githubModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const createNewGithubSubService = async (serviceIdRef, tokenType, description, githubUserName, githubRepoName) => {
    try {
        const newGithubService = new githubModel({
            serviceIdRef: serviceIdRef,
            tokenType: tokenType,
            description: description,
            githubUserName: githubUserName,
            githubRepoName: githubRepoName});
        newGithubService.save(function(err) {
            return false;
        });
        return newGithubService;
    } catch (error) {
        console.log(error);
        return false;
    }
}

// event possible de passer un tableau [push, issues, ...]
const createGithubWebhookConfig = (username, userRepo, events) => {
    var configObj = {
        owner: username,
        repo: userRepo,
        name: 'web',
        events: events,
        config : {
            url: GithubHookURI + "service/GitHub/hook",
            content_type: 'application/json',
            active: true,
        }
    }
    return configObj;
}

const createGithubWebhook = async (accessToken, githubUserName, githubRepoName, events) => {
    try {
        let githubWebhook;
        // events = ["push"]
        var listRepoWebhooks = await getGithubApiRep('get', `/repos/${githubUserName}/${githubRepoName}/hooks`, accessToken, {});
        let githubWebhookConfig = createGithubWebhookConfig(githubUserName, githubRepoName, events);

        // console.log("listRepoWebhooks = ", listRepoWebhooks)
        // console.log("c'est passer");
        if (listRepoWebhooks && listRepoWebhooks.length > 0)
        {
            //Update existed webhooks
            const webhookId = listRepoWebhooks[0].id;
            for (var i in listRepoWebhooks[0].events)
            {
                if (githubWebhookConfig.events.includes(listRepoWebhooks[0].events[i]) == false)
                {
                    githubWebhookConfig.events.push(listRepoWebhooks[0].events[i]);
                }
            }
            githubWebhook = await getGithubApiRep("patch", `/repos/${githubUserName}/${githubRepoName}/hooks/${webhookId}`, accessToken, githubWebhookConfig);
            // console.log("c'est passer if");
        }
        else
        {
            //Create new webhooks
            // console.log("c'est passer else");
            githubWebhook = await getGithubApiRep("post", `/repos/${githubUserName}/${githubRepoName}/hooks`, accessToken, githubWebhookConfig);
        }
        return githubWebhook;
    }
    catch (error) {
        console.log(error);
        return false;
    }
}

const findGithubArea = async (formatedData) => {
    try {
        const githubService = (await githubModel.find({
            githubRepoName: formatedData.model.name,
            githubUserName: formatedData.model.desc,
        })).forEach (async function (rep) {
            const listArea = (await areaModel.find({
                Services : {$elemMatch : { serviceActionId: rep._id.toString(), actionType: formatedData.action.type}}
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

const formatData = async (apiRep) => {
    try {
        let payload = JSON.parse(apiRep.body.payload);
        // console.log("payload = ", payload);
        // console.log("apiRep.headers = ", apiRep.headers);
        var formatedData;
        if (apiRep.headers['x-github-event'] == "push")
        {
            formatedData = {
                service : "GitHub",
                model: {
                    id: payload.repository.id,
                    name: payload.repository.name,
                    desc: payload.repository.owner.login,
                },
                action: {
                    id : "New push",
                    idCreator: payload.repository.owner.id,
                    type: "New push",
                    title: "New push from " + payload.pusher.name,
                    data: null
                },
            };
        }
        else if (apiRep.headers['x-github-event'] == "issues")
        {
            formatedData = {
                service : "GitHub",
                model: {
                    id: payload.repository.id,
                    name: payload.repository.name,
                    desc : payload.repository.owner.login,
                },
                action: {
                    id : payload.action + " issue",
                    idCreator: payload.repository.owner.id,
                    type: "New issue",
                    title: payload.issue.title,
                    data: null
                },
            };
        }
        else if (apiRep.headers['x-github-event'] == "create")
        {
            formatedData = {
                service : "GitHub",
                model: {
                    id: payload.repository.id,
                    name: payload.repository.name,
                    desc : payload.repository.owner.login,
                },
                action: {
                    id : "Create " + payload.ref_type,
                    idCreator: payload.repository.owner.id,
                    type: "Create branch",
                    title: "Create " + payload.ref_type,
                    data: null
                },
            };
        }
        else if (apiRep.headers['x-github-event'] == "delete")
        {
            formatedData = {
                service : "GitHub",
                model: {
                    id: payload.repository.id,
                    name: payload.repository.name,
                    desc : payload.repository.owner.login,
                },
                action: {
                    name : "Delete " + payload.ref_type,
                    idCreator: payload.repository.owner.id,
                    type: "Delete branch",
                    title: "Delete " + payload.ref_type,
                    data: null
                },
            };
        }
        return formatedData;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.dispatchGithubReaction = async (foundAreaLink, apiRep) => {
    var switcher = (function() {
        var _reactionType, _functions = {
            "Create repo" : function(){createRepo(foundAreaLink, apiRep);},
            "Create issue" : function(){createIssue(foundAreaLink, apiRep);},
            "Invite user" : function(){invitCollaborator(foundAreaLink, apiRep);},
            default: function(){throw('reactionType invalid');},
        };
        return function(reactionType){_functions[_reactionType=reactionType] ? _functions[reactionType](foundAreaLink, apiRep) : _functions.default(); };
    });
    try {
        if (foundAreaLink.Services[1].reactionType == "Create issue") 
        {
            createIssue(foundAreaLink, apiRep);
        } 
        else if (foundAreaLink.Services[1].reactionType == "Create repo")
        {
            createRepo(foundAreaLink, apiRep);
        }
        else if (foundAreaLink.Services[1].reactionType == "Invite user")
        {
            invitCollaborator(foundAreaLink, apiRep);
        }
        // switcher(foundAreaLink.Services[1].reactionType);
        return true;
    } catch (error) {
        console.log("error ca a pas trouver");
        console.log(error);
        return false;
    }
}

exports.githubHook = async (req, res) => {
    try {
        const formatedData = await formatData(req);
        console.log(formatedData);
        if (findGithubArea(formatedData) == false)
        {
            res.status(502).json({ error: 'Failed to find Action/Reaction' });
        }
        console.log("succes get github hook");
        return res.status(200).json({ success: "GitHub webhook success" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }

}

exports.getUserRepo = async (req, res) => {
    const { userId } = req.body;

    try {
        const github = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitHub"});
        if (!github) {
            return res.status(404).json({ error: 'Failed to get user\'s GitHub info from DB' });
        }
        const {accessToken} = github;
        const githubRes = await getGithubApiRep('get', '/user/repos', accessToken, {});
        if (!githubRes) {
            return res.status(403).json({ error: 'Try to get GitHub user info from API failed'});
        }
        res.status(200).json({ success: githubRes });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

// create repo for auth user
const createRepo = async (foundAreaLink, apiRep) => {
    const userId = foundAreaLink.userIdRef;
    const githubRepoName = apiRep.action.title;

    try {
        const github = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitHub"});
        if (!github) {
            return false;
        }
        const {accessToken} = github;
        const githubRes = await getGithubApiRep('post', '/user/repos', accessToken, {name: githubRepoName});
        if (!githubRes) {
            return false;
        }
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

const createIssue = async (foundAreaLink, apiRep) => {
    const userId = foundAreaLink.userIdRef;
    const issueTitle = apiRep.action.title;

    try {
        const github = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitHub"});
        if (!github) {
            return false;
        }
        const {accessToken} = github;
        const {githubUserName, githubRepoName} = await githubModel.findById(foundAreaLink.Services[1].serviceReactionId);

        const githubRes = await getGithubApiRep('post', `/repos/${githubUserName}/${githubRepoName}/issues`, accessToken, {title: issueTitle});
        if (!githubRes) {
            return false;
        }
        return true;
    } catch (error) {
        console.log(error);
        return false
    }
}

const invitCollaborator = async (foundAreaLink, apiRep) => {
    const userId = foundAreaLink.userIdRef;
    const collaboratorUserName = apiRep.action.title;

    try {
        const github = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitHub"});
        if (!github) {
            return false;
        }
        const {accessToken} = github;
        const {githubUserName, githubRepoName} = await githubModel.findById(foundAreaLink.Services[1].serviceReactionId);

        const githubRes = await getGithubApiRep('put', `/repos/${githubUserName}/${githubRepoName}/collaborators/${collaboratorUserName}`, accessToken, {});
        if (!githubRes) {
            return false;
        }
        return true;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.setGithubService = async (req, res) => {
    const {userId, githubUserName, githubRepoName, events} = req.body;
    let newEvents = [];

    try {
        // Get le bon event dans la dico GitHub
        // console.log("events before = ", events);
        newEvents.push(GitHubActionDico.get(events));
        // console.log("events after = ", newEvents);
        // Check si la DB posséde les infos github de l'user
        const service = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitHub"});
        if (!service)
        {
            return res.status(404).json({ error: 'Failed to get user\'s GitHub info from DB' });
        }
        const accessToken = service.accessToken;
        const serviceId = service._id;
        // Check si un subservice a été set avec ce repo
        const foundGithubService = await githubModel.findOne({serviceIdRef: serviceId, githubUserName: githubUserName, githubRepoName: githubRepoName});
        let newGithubService;

        // si un subservice a déja été cree dans ce cas je update juste le webhook
        if(foundGithubService)
        {
            createGithubWebhook(accessToken, githubUserName, githubRepoName, newEvents);
            return res.status(200).json({ success: foundGithubService });
        }
        // Lors de la creation du github service, l'user n'as pas set de repo, donc il faut le set ici une fois qu'il aura choisis quel repo set
        // Check si il trouve un githubModel avec le githubUserName mais avec un githubUserRepo null
        const emptyGithubService = await githubModel.findOne({serviceIdRef: serviceId, githubUserName: githubUserName})
        // Si il trouve pas dans le ce cas je cree un nouveaux
        if (!emptyGithubService)
        {
            newGithubService = await createNewGithubSubService(serviceId, "Bearer", "GitHub sub service" ,githubUserName, githubRepoName);
            if (!newGithubService) {
                return res.status(404).json({ error: 'Failed to create GitHub subservice' });
            }
        }
        else // Si il trouve dans le ce cas je update le subservice
        {
            newGithubService = await emptyGithubService.updateOne({serviceIdRef: serviceId, githubUserName: githubUserName, githubRepoName: githubRepoName});
            newGithubService = await githubModel.findOne({_id : emptyGithubService._id});
        }
        createGithubWebhook(accessToken, githubUserName, githubRepoName, newEvents);
        res.status(200).json({ success: newGithubService });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

exports.createGithubService = async (res, serviceIdRef, tokenType, accessToken) => {
    try {
        const { login } = await getGithubApiRep('get', '/user', accessToken, {});
        const newGithubService = new githubModel({serviceIdRef, tokenType, githubUserName: login, githubUserRepo: null});
        newGithubService.save(function(err) {
            if (err) {return res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service GitHub Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}