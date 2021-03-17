export const chooseIconsToRender = (name) => {
    switch (name) {
        case 'GitLab':
            return 'gitlab'
        case 'Discord':
            return 'comment'
        case 'Trello':
            return 'trello'
        case 'Slack':
            return 'slack'
        case 'SendGrid':
            return 'send'
        case 'Twilio':
            return 'twinkle-star'
        default:
            return 'gitlab'
    }
}