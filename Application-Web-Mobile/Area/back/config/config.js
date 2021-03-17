require('dotenv').config();

module.exports = ({
  mongoURI: process.env.MONGO_URI,
  portBack: process.env.PORT,
  secretToken: process.env.TOKEN_SECRET,
  sendgridKey:process.env.SENDGRID_API_KEY,
  urlConfirm:process.env.WEB_URL,
  TrelloApiKey : process.env.TRELLO_API_KEY,
  TrelloHookURI : process.env.TRELLO_REDIRECT_HOOK_URI,
  GITLAB_HOOK_URL : process.env.GITLAB_HOOK_URL,
  GithubHookURI : process.env.GITHUB_REDIRECT_HOOK_URI,
  DiscordApiKey : process.env.DISCORD_CLIENT_ID,
  DiscordBotToken : process.env.DISCORD_BOT_TOKEN,
  SlackClientId: process.env.SLACK_CLIENT_ID,
  SlackClientSecret: process.env.SLACK_CLIENT_SECRET,
  SlackSigningSecret: process.env.SLACK_SIGNING_SECRET,
  SlackBotToken: process.env.SLACK_BOT_TOKEN,
  TwillioToken: process.env.TWILIO_TOKEN,
  TwillioSid: process.env.TWILIO_SID,
  senderSendGrid: process.env.SENDGRID_EMAIL_SENDER,
});