// Importing required modules
const cors = require('cors');
const morgan = require('morgan');
const express = require('express');
const mongoose = require('mongoose');
const bodyParser = require('body-parser');

const app = express();

// load env config
const { mongoURI, portBack, hostBack } = require('./config/config');

// set port
const port = portBack || 8080

// set host
const host = hostBack || 'localhost'

//set json
app.set('views', __dirname + '/views');
app.engine('json', require('ejs').renderFile);

// init DB
mongoose.connect(mongoURI, { useNewUrlParser: true, useUnifiedTopology: true, useCreateIndex: true });
mongoose.connection.once('open', () => {
  console.log('Connected to Mongo');
}).on('error', (err) => {
  console.log('Mongo Error', err);
});

// Configure middlewares
app.use(cors());
app.use(morgan('dev'));
app.use(express.json());
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({
  extended: true,
}));


// import routes
const userRoutes = express.Router();
const servicesRoutes = express.Router();
const trelloRoutes = express.Router();
const githubRoutes = express.Router();
const areaRoutes = express.Router();
const discordRoutes = express.Router();
const sendgridRoutes = express.Router();
const gitlabRoutes = express.Router();


const slackRoutes = express.Router();
const twilioRoutes = express.Router();

const aboutRoutes = express.Router();

// use controllers
require('./routes/userRoutes')(userRoutes);
require('./routes/serviceRoutes')(servicesRoutes);
require('./routes/trelloRoutes')(trelloRoutes);
require('./routes/areaRoutes')(areaRoutes);
require('./routes/githubRoutes')(githubRoutes);
require('./routes/discordRoutes')(discordRoutes);
require('./routes/sendgridRoutes')(sendgridRoutes);
require('./routes/gitlabRoutes')(gitlabRoutes);

require('./routes/slackRoutes')(slackRoutes);
require('./routes/twilioRoutes')(twilioRoutes);

require('./routes/aboutRoutes')(aboutRoutes);

// use routes
app.use('/user', userRoutes);
app.use('/service', servicesRoutes);
app.use('/service', trelloRoutes);
app.use('/service', githubRoutes);
app.use('/service', discordRoutes);
app.use('/service', slackRoutes);
app.use('/area', areaRoutes);
app.use('/service', sendgridRoutes);
app.use('/service', gitlabRoutes);

app.use('/service', twilioRoutes);

app.use('/about.json', aboutRoutes);

app.get('/', (req, res) => {
  res.send('Hello World!')
})

app.listen(port, () => {
  console.log(`Example app listening at http://${host}:${port}`)
})
