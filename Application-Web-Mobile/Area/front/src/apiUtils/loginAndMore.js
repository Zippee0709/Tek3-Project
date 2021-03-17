import querystring from 'querystring';
import axios from 'axios';
import { Alert } from 'rsuite';
import { API_URL } from '../config'

export const login = async ({ history, email, password, redirectUrl }) => {
    try {
        const loginResponse = await axios.post(`${API_URL}user/login`, querystring.stringify({
            email, password
        }), { headers: { "Content-Type": "application/x-www-form-urlencoded" } });
        localStorage.setItem('token', loginResponse.data.token);
        console.log(redirectUrl);
        if (redirectUrl?.from !== undefined) {
            console.log('Ca rentre dedans', redirectUrl.from);
            history.push(redirectUrl.from);
            return;
        }
        Alert.success('Sucessfuly Logged !')
        setTimeout(() => {
            history.push('/')
            window.location.reload();
        }, 2300)
    } catch (e) {
        Alert.error(e.response?.data?.error);
    };
}

export const loginAuth2 = async ({ data, history }) => {
    try {
        const loginResponse = await axios.post(`${API_URL}user/loginFirebase`, querystring.stringify({
            email: data.email, pseudo: data.pseudo, firebaseId: data.firebaseId
        }), { headers: { "Content-Type": "application/x-www-form-urlencoded" } });
        localStorage.setItem('token', loginResponse.data.token);
        Alert.success('Sucessfuly Logged !')
        setTimeout(() => {
            history.push('/')
            window.location.reload();
        }, 2300)
    } catch (e) {
        Alert.error(e.response?.data?.error);
    };
}

export const getWidgets = async () => {
    try {
        const widgetsResponse = await axios.get(`${API_URL}service/getServices`, {
            headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') }
        });
        return widgetsResponse.data.servicesList[0].user
    } catch (e) {
        console.log(e)
        // Alert.error(e.response);
    };
}

export const getWidgetDetail = async ({ serviceId }) => {
    try {
        const widgetDetailResponse = await axios.get(`${API_URL}service/getServicesDetails/${serviceId}`, { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
        return widgetDetailResponse.data.areaLinks
    } catch (e) {
        console.log(e)
        //Alert.error(e.response.data.error);
    };
}

export const deleteLink = async ({ serviceId }) => {
    try {
        await axios.delete(`${API_URL}area/deleteArea/${serviceId}`, { headers: { "Content-Type": "application/x-www-form-urlencoded", 'Authorization': 'Bearer ' + localStorage.getItem('token') } });
        Alert.success('Link sucessfully deleted')
        setTimeout(() => {
            window.location.reload();
        }, 2300)
    } catch (e) {
        console.log(e)
        Alert.error('Delete failed');
    };
}

export const logout = async ({ history }) => {
    try {
        localStorage.removeItem('token');
        Alert.success('Sucessfully logout, Good BYE!')

        setTimeout(() => {
            history.push('/')
            window.location.reload();
        }, 2300)
    } catch (e) {
        Alert.error(e);
    };
}

export const checkEmailConfirmed = async ({ history }) => {
    try {
        // const getItem = JSON.parse(localStorage.getItem('token'));
        // const confirmEmailResponse = await axios.post(`${API_URL}user/confirmEmail/${JSON.parse(getItem.token)}`,
        //     { headers: { "Content-Type": "application/x-www-form-urlencoded" } });
        // console.log(confirmEmailResponse)
        // Alert.success('Success ! You can now login');
        // localStorage.setItem('token', loginResponse.data);
        // history.push('/')
    } catch (e) {
        Alert.error(e?.response?.data?.error);
    };
}
