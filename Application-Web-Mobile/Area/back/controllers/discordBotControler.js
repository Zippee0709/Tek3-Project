const Discord = require('discord.js');

const { DiscordApiKey, DiscordBotToken } = require('../config/config');

const bot = new Discord.Client();
bot.login(DiscordBotToken);

bot.on("ready", async () => {
    console.log('Launch discord Bot');
    bot.user.setStatus('online');
    bot.user.setActivity("son créateur: Meru", {type: 'LISTENING'});
})

exports.getServerId = async (res, req) => {

}

exports.sendMessage = async (messageJson, chanId) => {
    console.log('jdksqjld');
    // bot.on('message', function(messageJson, chanId) {

    // })
}

exports.getChannels = async (res, req) => {
    const tmp = Client.getChannels;
    console.log(tmp)
    return true
}