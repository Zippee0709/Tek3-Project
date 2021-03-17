const jwt = require('jsonwebtoken');
const { secretToken } = require('../config/config');

const auth = (req, res, next) => {
  try {
    const token = req.header('Authorization');
    const tokenSplit = token ? token.split(' ') : token;

    if (!token || !tokenSplit || tokenSplit[0] !== 'Bearer' || !tokenSplit[1]) {
      throw ('no token');
    }
    const { id } = jwt.verify(tokenSplit[1], secretToken);
    if (!id) {
      throw ('Problem with the token');
    }
    req.body.userId = id;
    next();
  } catch (error) {
    res.status(401).send({ error: 'Please authenticate!' });
  }
};

module.exports = auth;