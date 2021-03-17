const express = require('express');
const pg = require('pg');
const bodyParser = require('body-parser');
const async = require('async');
const cors = require('cors');
const bcrypt = require('bcrypt');
const google = require('./googleAPI')
const { OAuth2Client } = require('google-auth-library');
const axios = require('axios');

const app = express();
app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());
app.use(cors());

const port = process.env.PORT || 8080;
let myClient;

let pool = new pg.Pool({
  user: 'postgres',
  // host: 'db',
  host: '172.19.0.2',
  port: 5432,
  database: 'dashboard',
  password: 'password'
});

let corsOptions = {
  origin: "http://localhost",
}

async function hashPassword(password) {
  const salt = await bcrypt.genSalt(15)
  const hash = await bcrypt.hash(password, salt)
  return hash;
}

async.retry(
  { times: 100, interval: 1000 },
  function (callback) {
    pool.connect(function (err, client, done) {
      if (err) {
        console.error("Waiting for db");
      }
      callback(err, client);
    })
  },
  function (err, client) {
    if (err) {
      return console.error("Aïe, error to connect to db");
    }
    console.log("Connection to the db");
    myClient = client;
  }
)

app.options('*', cors(corsOptions));

app.get('/getAuthorizeGoogleUri', (req, res) => {
  const ad = google.authorizeGoogle();
  res.send({ error: false, result: ad });
});

app.get('/getToken', (req, res) => {
  const code = req.query.code;
  res.redirect("http://localhost:8081/serviceList/?code=" + code); // TO CHANGE
});

app.post('/saveGoogleToken', async (req, res) => {
  const code = req.body.code;
  const userId = req.body.userId;

  let query = "UPDATE Users SET user_google_access_token = $1, user_google_refresh_token = $2 WHERE user_id = $3"
  const result = await google.saveGoogleToken(code, userId, myClient, query);
  if (result) {
    res.send({ error: false, result: result });
  } else {
    res.status(200).send({ error: true, result: result });
  }
});

app.get('/getGmailLabels', async (req, res) => {
  let query = "SELECT user_google_access_token, user_google_refresh_token FROM Users WHERE user_id = $1";
  let values = [req.query.userId];

  try {
    const result = await myClient.query(query, values);
    const googleData = await google.getGmailLabels(result.rows[0].user_google_access_token, result.rows[0].user_google_refresh_token);
    console.log("google data = ", data)
    res.send({ error: false, result: googleData });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/getGmailData', async (req, res) => {
  let query = `SELECT user_google_access_token, user_google_refresh_token FROM Users WHERE user_id = ${req.query.userId}`;
  let getWidgetInstanceGmailQuery = `SELECT * FROM UserWidgetInstance Where user_id = ${req.query.userId}
                                      AND widget_id = 4`

  try {
    const result = await myClient.query(query);
    const widgetInstanceGmail = await myClient.query(getWidgetInstanceGmailQuery);
    console.log("widgetInstanceGmail", widgetInstanceGmail);
    const googleData = await google.getGmailData(result.rows[0].user_google_access_token, result.rows[0].user_google_refresh_token, widgetInstanceGmail);
    res.send({ error: false, result: googleData});
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/', (req, res) => {
  res.send('Server is on ;)');
});

app.post('/login', async (req, res) => {
  const query = "SELECT * FROM Users WHERE user_mail = $1";
  const values = [req.body.email];

  try {
    const result = await myClient.query(query, values);

    bcrypt.compare(req.body.password, result.rows[0].user_password, function (err, ok) {
      if (err) {
        res.status(200).send({ error: true, result: err });
      }
      if (ok) {
        res.status(200).send({ error: false, result: { user_id: result.rows[0].user_id, user_firstname: result.rows[0].user_firstname, user_lastname: result.rows[0].user_lastname, user_mail: result.rows[0].user_mail, user_image: result.rows[0].user_image, user_google_token: result.rows[0].user_google_token } });
        return;
      }
      res.status.send({ error: true, result: result });
    });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/addUserService', async (req, res) => {
  const query = "INSERT INTO UserServiceLink (user_id, service_id) VALUES ($1, $2) RETURNING *";
  const values = [req.body.userID, req.body.serviceID];

  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/getUserService', async (req, res) => {
  const query = "SELECT * FROM UserServiceLink WHERE user_id = $1";
  const values = [req.query.userId];

  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/addUserWidget', async (req, res) => {
  const query = "INSERT INTO UserWidgetInstance (user_id, widget_id, parameters) VALUES ($1, $2, $3) RETURNING *";
  const values = [req.body.userId, req.body.widgetId, req.body.params];
  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    console.error(err.stack);
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/getUserWidget', async (req, res) => {
  const query = "SELECT * FROM UserWidgetInstance WHERE user_id = $1";
  const values = [req.query.userId];
 
  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/deleteUserWidget', async (req, res) => {
  const query = "DELETE FROM userWidgetInstance WHERE user_widget_instance_id = $1";
  const values = [req.body.widgetInstanceId];
  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    console.error(err.stack);
    res.status(200).send({ error: true, result: err });
  }
});


app.get('/getServiceInfo', async (req, res) => {
  const query = "SELECT * FROM services";
  try {
    const result = await myClient.query(query);
    res.send({ error: false, result: result });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/getUserWidgetSubscribable', async (req, res) => {
  const query = `select w.*, swl.service_id from servicewidgetlink swl
  join userServiceLink usl on usl.service_id = swl.service_id
  join widgets w on w.widget_id = swl.widget_id
  where usl.user_id = $1
  and usl.service_id = swl.service_id`
  const values = [req.query.userId]

  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    console.error(err.stack);
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/getWidgetInfo', async (req, res) => {
  const query = "SELECT * FROM widgets";
  try {
    const result = await myClient.query(query);
    res.send({ error: false, result: result });
  } catch (err) {
    console.error(err.stack);
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/register', async (req, res) => {
  let query = "INSERT INTO Users(user_firstname, user_lastname, user_mail, user_password) VALUES ($1, $2, $3, $4) RETURNING *"
  const password = await hashPassword(req.body.password);
  const values = [req.body.firstName, req.body.lastName, req.body.email, password];

  try {
    const result = await myClient.query(query, values);
    res.send({ error: false, result: result });
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/registerGoogle', async (req, res) => {
  const payload = await google.getPayload(req.body.idToken);
  const userid = payload['sub'];
  let email = payload['email'];
  let name = payload["name"];

  let query = "INSERT INTO Users(user_firstname, user_mail) VALUES ($1, $2)"
  const values = [name, email];

  try {
    const result = await myClient.query(query, values);
    res.status(200).send({ error: false, result: result });
    return;
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.post('/loginGoogle', async (req, res) => {
  const payload = await google.getPayload(req.body.idToken);
  const userid = payload['sub'];
  let email = payload['email'];
  let name = payload["name"];

  const query = "SELECT * FROM Users WHERE user_mail = $1";
  const values = [email];

  try {
    const result = await myClient.query(query, values);
    res.status(200).send({ error: false, result: { user_id: result.rows[0].user_id, user_firstname: result.rows[0].user_firstname, user_lastname: result.rows[0].user_lastname, user_mail: result.rows[0].user_mail, user_image: result.rows[0].user_image } });
    return;
  } catch (err) {
    res.status(200).send({ error: true, result: err });
  }
});

app.get('/about.json', (req, res) => {
  let data = {};
  data.client = {};
  data.client.host = req.ip;
  data.server = {};
  data.server.current_time = Date.now();
  data.server.services = []
  data.server.services[0] = {name: "Google", widgets: [{name: "Gmail Overview", description: "Watch your email depend on your selection", params: [{name: "label", type:"string"}]}]}
  data.server.services[1] = {name: "Weather", 
    widgets: [{name: "Current weather", description: "Access current weather for any location", params: [{name: "location", type:"string"}]},
    {name: "Hourly Forecast", description: "Hourly forecast is available for 5 days", params: [{name: "location", type: "string"}]},
    {name: "Historical Weather", description: "Provide city historical weather data for 63,000+ cities", params: [{name: "location", type: "string"}]}
  ]};
  data.server.services[2] = {name: "Animal",
    widgets: [{name: "Animal Viewer", description: "Get you favorites animals", params: [{name: "animalType", type:"string"}]},
  ]};
  data.server.services[3] = {name: "News",
    widgets: [{name: "Search News", description: "Search your favorite subject with TheNewYorkTimes", params: [{name: "query", type:"string"}]},
  ]};
  res.json(data);
});

app.listen(port, function () {
  console.log(`Server running at port ${port}`);
});


module.exports = app;