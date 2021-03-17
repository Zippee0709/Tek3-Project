/**
 * GitLab Services Controller
*/

const axios = require('axios');

const servicesModel = require('../models/servicesModel');
const gitlabModel = require('../models/gitlabServicesInfoModel');
const gitlabAreaControler = require('../controllers/gitlabArea/gitlabAreaController');

const { GITLAB_HOOK_URL } = require('../config/config');

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

exports.getInfoService = async (idService) => {
    try {
        const service = await gitlabModel.findOne({ _id: idService });
        if (!service) {
            return false;
        }
        return service;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.createGitlabService = async (res, serviceIdRef, tokenType, accessToken) => {

    try {
        const { id, name, username, avatar_url, email } = await getGitlabInfoUser(tokenType, accessToken, 'get');
        if (!id || !name || !username || !avatar_url || !email) {
            return res.status(500).json({ error: 'Try to get gitlab user info from API failed'});
        }
        const newGitlabService = new gitlabModel({serviceIdRef, tokenType, gitlabUserID: id});
        newGitlabService.save(function(err) {
            if (err) {res.status(500).json({ error: err.message });}
        });
        res.status(200).json({ success: "Service GitLab Created" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}


exports.getReposUser = async (req, res) => {
    const { userId } = req.body;

    try {
        const gitlab = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitLab"});
        if (!gitlab) {
            return res.status(404).json({ error: 'Failed to get user\'s Gitlab info from DB' });
        }
        const {_id} = gitlab;
        const { gitlabUserID } = await gitlabModel.findOne({ serviceIdRef: _id});
        const GitlabRes = await getGitlabInfoUser("Bearer" , gitlab.accessToken, "","users/"+ gitlabUserID + "/projects" );
        if (!GitlabRes) {
            return res.status(403).json({ error: 'Try to get Gitlab user info from API failed'});
        }
        res.status(200).json({ success: GitlabRes });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}


exports.setGitlabService = async (req, res) => {
    const { userId, projectId, projectName } = req.body;

    try {
        const service = await servicesModel.findOne({ userIdRef: userId, nameServices: "GitLab"});
        let newGitLab;
        if (!service) {
            return res.status(404).json({ error: 'Failed to get user\'s Gitlab info from DB' });
        }
        const {_id} = service;
        const foundGitlab = await gitlabModel.findOne({serviceIdRef: _id, projectId: projectId, projectName: projectName});
        if (foundGitlab) {
            return res.status(200).json({ success: foundGitlab });
        };
        const emptyGitlab = await gitlabModel.findOne({serviceIdRef: _id, projectId: null});
        if (!emptyGitlab) {
            newGitlab = await createNewGitlabSubService(_id , "bearer" ,tokenType, projectId, projectName , foundGitlab.gitlabUserID);
            if (!newGitlab) {
                return res.status(404).json({ error: 'Failed to create Gitlab subservice' });
            }
        } else {
            newGitlab = await emptyGitlab.updateOne({gitlabRepoId: projectId , gitlabRepoName: projectName, gitlabRepoID: projectId});
            newGitLab = await gitlabModel.findOne({_id: emptyGitlab._id});
        }
        const hookRep = await getGitlabInfoUser("Bearer" , service.accessToken ,"post", "projects/" + projectId+ "/hooks" , { url: GITLAB_HOOK_URL + "service/gitlab/hook", issues_events:true} );
        res.status(200).json({ success: newGitLab });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

const formatData = async (apiRep) => {

    try {
        const formatedData = {
            service : "Gitlab",
            model: {
                id: apiRep.project.id,
                name: apiRep.project.name,
                desc: apiRep.project.name,
            },
            action: {
                id: apiRep.object_attributes.id,
                idCreator: apiRep.object_attributes.author_id,
                type: apiRep.object_attributes.action,
                title: apiRep.object_attributes.title,
                data: null,
            },
        };
        return formatedData;
    } catch (error) {
        console.log(error);
        return false;
    }
}

exports.gitlabHook = async (req, res) => {
    try {
        const formatedData = await formatData(req.body);
        if (gitlabAreaControler.findGitlabArea(formatedData) == false) {
            res.status(502).json({ error: 'Failed to find Action/Reaction' });
        }
        res.status(200).send({ success: "Thanks for the webhook" });
    } catch (error) {
        console.log(error);
        res.status(500).json({ error: 'An error has occurred' });
    }
}

const createNewGitlabSubService = async (serviceIdRef, tokenType, gitlabRepoID, gitlabRepoName, gitlabUserID ) => {
    try {
        const newGitlabService = new gitlabModel({ serviceIdRef, tokenType, gitlabRepoID , gitlabRepoName, gitlabUserID });
        newGitlabService.save(function(err) {
            return false;
        });
        return newGitlabService;
    } catch (error) {
        console.log(error);
        return false;
    }
}

