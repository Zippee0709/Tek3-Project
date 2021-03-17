require('dotenv').config();


module.exports = {
    API_URL: process.env.REACT_APP_API_DEV === 'YES' ? process.env.REACT_APP_API_URL_DEV : process.env.REACT_APP_API_URL
};
