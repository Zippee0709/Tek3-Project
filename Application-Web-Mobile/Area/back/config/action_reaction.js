module.exports = ({
    Services: {
        List: [
            "Trello",
            "GitLab",
            "GitHub",
            "Slack",
            "Discord",
            "Twilio",
            "SendGrid",
        ],
        "Trello": {
            Action: [
                "New Card",
                "Card moved to list",
            ],
            Reaction: [
                "Create Card",
                "Create List"
            ]
        },
        "GitLab": {
            Action: [
                "Create Issue",
            ],
            Reaction: [
                "Create Issue",
            ]
        },
        "GitHub": {
            Action: [
                "New push",
                "New issue",
                "Create branch",
                "Delete branch",
            ],
            Reaction: [
                "Create repo",
                "Create issue",
                "Invite user",
            ]
        },
        "Slack": {
            Action: [
                "New Bot Mention",
                "File Shared",
                "New Channel",
            ],
            Reaction: [
                "Send message",
            ],
        },
        "Discord": {
            Action: [
            ],
            Reaction: [
                "Send message",
            ]
        },
        "Twilio": {
            Action: [],
            Reaction: [
                "Send SMS",
            ]
        },
        "SendGrid": {
            Action: [],
            Reaction: [
                "Send email",
            ]
        },
    },
});