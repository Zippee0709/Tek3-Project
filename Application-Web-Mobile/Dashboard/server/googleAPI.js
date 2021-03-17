const fs = require('fs');
const readline = require('readline');
const { google } = require('googleapis');
const { OAuth2Client } = require('google-auth-library');

const SCOPES = ['https://www.googleapis.com/auth/gmail.readonly'];
const TOKEN_PATH = 'token.json';

let credentials = fs.readFileSync('credentials.json', 'utf-8');
const { client_secret, client_id, redirect_uris } = JSON.parse(credentials).web;
const oAuth2Client = new google.auth.OAuth2(client_id, client_secret, redirect_uris[0]);

async function listLabels(auth) {
    const gmail = google.gmail({ version: 'v1', auth });
    const res = await gmail.users.labels.list({
        userId: 'me',
    });
    return res.data.labels;
}

async function getListMail(auth) {
    const gmail = google.gmail({ version: 'v1', auth });
    const res = await gmail.users.messages.list({
        userId: 'me',
    });
    const listMessageId = res.data;
    let messagesId = [];
    listMessageId.messages.forEach(async (message) => {
        const res = await gmail.users.messages.get({
            userId: 'me',
            id: message.id
        });
    });
}

async function getListMailWithThread(auth, widgetInstance) {
    const gmail = google.gmail({ version: 'v1', auth });
    const res = await gmail.users.messages.list({
        userId: 'me',
    });
    const listMessageId = res.data;

    let data = {};
    for (let i = 0; i < widgetInstance.rowCount; i++) {
        data[widgetInstance.rows[i].parameters] = {
            widgetInstanceId: widgetInstance.rows[i].user_widget_instance_id,
            data: []
        };
    }
    for (let i = 0; i < listMessageId.messages.length; i++) {
        const res = await gmail.users.threads.get({
            userId: 'me',
            id: listMessageId.messages[i].threadId
        });
        for (let k = 0; k < widgetInstance.rowCount; k++) {
            if (res.data.messages[0].labelIds.includes(widgetInstance.rows[k].parameters)) {
                data[widgetInstance.rows[k].parameters].data.push(res.data.messages);
            }
        }
    }
    console.log("data get list Mail = ", data)
    return data;
}

// async function getListMailWithThread(auth, widgetInstance) {
//     const gmail = google.gmail({ version: 'v1', auth });
//     const res = await gmail.users.messages.list({
//         userId: 'me',
//     });
//     const listMessageId = res.data;

//     let data = {};
//     for (let i = 0; i < widgetInstance.rowCount; i++) {
//         data[widgetInstance.rows[i].parameters] = [];
//     }
//     for (let i = 0; i < listMessageId.messages.length; i++) {
//         const res = await gmail.users.threads.get({
//             userId: 'me',
//             id: listMessageId.messages[i].threadId
//         });
//         for (let k = 0; k < widgetInstance.rowCount; k++) {
//             if (res.data.messages[0].labelIds.includes(widgetInstance.rows[k].parameters)) {
//                 data[widgetInstance.rows[k].parameters].push([widgetInstance.rows[i].user_widget_instance_id, res.data.messages]);
//             }
//         }
//     }
//     return data;
// }


module.exports = {
    authorizeGoogle: function () {
        return oAuth2Client.generateAuthUrl({
            access_type: 'offline',
            prompt: 'consent',
            scope: SCOPES,
        });
    },
    saveGoogleToken: async function (code, userId, myClient, query) {
        oAuth2Client.getToken(code, async (err, token) => {
            if (err) return console.error('Error retrieving access token', err);
            oAuth2Client.setCredentials(token);
            const values = [token.access_token, token.refresh_token, userId];
            try {
                const result = await myClient.query(query, values);
                // res.send({error: false, result: result});
                return true;
            } catch (err) {
                // res.status(200).send({error: true, result: err});
                return false;
            }
        });
        return true;
    },
    getGmailLabels: async function (access_token, refresh_token) {
        oAuth2Client.credentials = {
            access_token: access_token,
            refresh_token: refresh_token
        }
        const res = await listLabels(oAuth2Client);
        return res;
    },
    getGmailData: async function (access_token, refresh_token, widgetInstance) {
        oAuth2Client.credentials = {
            access_token: access_token,
            refresh_token: refresh_token
        }
        const res = await getListMailWithThread(oAuth2Client, widgetInstance);
        return res;
    },
    getPayload: async function (idToken) {
        const client = new OAuth2Client('Your-API-Key.apps.googleusercontent.com');
        const ticket = await client.verifyIdToken({
            idToken: idToken,
            audience: 'Your-API-Key.apps.googleusercontent.com'
        });
        const payload = ticket.getPayload();
        return payload;
    }
}