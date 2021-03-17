import axios from 'axios'
const host = "http://localhost:8080";

export async function test()
{
    const res = await axios.post(host + "/test");
    if (res.status == 500) {
    }
    if (res.status !== 200) {
        console.error("Error with the test API");
        return null;
    }
    return res;
}

export async function register(data)
{
    const res = await axios.post(host + "/register", {firstName: data.firstName, lastName: data.lastName, email: data.email, password: data.password});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function login(data)
{
    const res = await axios.post(host + "/login", {email: data.email, password: data.password});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function loginGoogle(idToken)
{
    const res = await axios.post(host + "/loginGoogle", {idToken: idToken});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function registerGoogle(idToken)
{
    const res = await axios.post(host + "/registerGoogle", {idToken: idToken});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getServiceInfo() {
    const res  = await axios.get(host + "/getServiceInfo");
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getWidgetInfo() {
    const res  = await axios.get(host + "/getWidgetInfo");
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function addUserService(userID, serviceID) {
    const res  = await axios.post(host + "/addUserService", {userID: userID, serviceID: serviceID});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function addUserWidget(userId, widgetId, params) {
    const res  = await axios.post(host + "/addUserWidget", {userId: userId, widgetId: widgetId, params: params});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getUserService(userId) {
    const res  = await axios.get(host + "/getUserService", {params: { userId: userId }});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getUserWidget(userId) {
    const res  = await axios.get(host + "/getUserWidget", {params: { userId: userId }});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getUserWidgetSubscribable(userId) {
    const res  = await axios.get(host + "/getUserWidgetSubscribable", {params: { userId: userId }});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getAuthorizeGoogleUri()
{
    const res = await axios.get(host + "/getAuthorizeGoogleUri");
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function saveGoogleToken(code, userId)
{
    const res = await axios.post(host + "/saveGoogleToken", {code: code, userId: userId});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getGmailData(userId) {
    const res  = await axios.get(host + "/getGmailData", {params: { userId: userId }});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function getGmailLabels(userId) {
    const res  = await axios.get(host + "/getGmailLabels", {params: { userId: userId }});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}

export async function deleteUserWidget(widgetInstanceId) {
    const res  = await axios.post(host + "/deleteUserWidget", {widgetInstanceId: widgetInstanceId});
    if (res.status > 300 && res.status < 200) {
        console.error("Error with server: ", res)
    }
    return res;
}
