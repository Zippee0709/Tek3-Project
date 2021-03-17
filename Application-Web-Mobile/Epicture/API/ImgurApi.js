const CLIENTID = "Your API Key"
const AUTHORIZATION = "Client-ID " + CLIENTID

export function getAccountFavorites(username) {
    const url = "https://api.imgur.com/3/account/" + username + "/gallery_favorites/"
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getAccountPost(username, userAccessToken) {
    const url = "https://api.imgur.com/3/account/" + username + "/images/"
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': "Bearer " + userAccessToken
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getAccountFollowing(username) {
    const url = "https://api.imgur.com/3/account/" + username + "/gallery_favorites/"
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}


export function searchByInput(input) {
    const url = "https://api.imgur.com/3/gallery/search?q=" + input
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getAuthorizationUserUrl() {
    return "https://api.imgur.com/oauth2/authorize?client_id="+ CLIENTID + "&response_type=token&state=success"
}

export function getAuthorizationUser() {
    const url = "https://api.imgur.com/oauth2/authorize?client_id="+ CLIENTID + "&response_type=token&state=success"
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getHomeViralFeed() {
    const url = "https://api.imgur.com/3/gallery/hot/viral";
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getComments(galleryId) {
    const url = "https://api.imgur.com/3/gallery/"+ galleryId +"/comments/best";
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function getUserAvatar(username) {
    const url = "https://api.imgur.com/3/account/" + username + "/avatar"
    return fetch(url, {
        method: "get",
        headers: new Headers({
            'Authorization': AUTHORIZATION
        })
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function postImage(userAccessToken, data) {
    const url = "https://api.imgur.com/3/upload"

    return fetch(url, {
        method: "post",
        headers: new Headers({
            'Authorization': 'Bearer' + userAccessToken,
            'Content-Type': 'application/json'
        }),
        body: JSON.stringify(data)
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function postComment(userAccessToken, commentMessage, imageId) {
    const url = "https://api.imgur.com/3/comment"
    let formData = {"image_id": imageId, "comment": commentMessage};

    return fetch(url, {
        method: "post",
        headers: new Headers({
            'Authorization': 'Bearer' + userAccessToken,
            'Content-Type': 'application/json'
        }),
        body: JSON.stringify(formData)
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function voteGallery(userAccessToken, galleryId, type) {
    const url = "https://api.imgur.com/3/gallery/" + galleryId + "/vote/"+ type

    return fetch(url, {
        method: "post",
        headers: new Headers({
            'Authorization': 'Bearer' + userAccessToken,
        }),
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));
}

export function favorite(userAccessToken, galleryId) {
    const url = "https://api.imgur.com/3/album/" + galleryId+ "/favorite"

    return fetch(url, {
        method: "post",
        headers: new Headers({
            'Authorization': 'Bearer' + userAccessToken,
        }),
    })
    .then((reponse) => reponse.json())
    .catch((error) => console.error(error));    
}