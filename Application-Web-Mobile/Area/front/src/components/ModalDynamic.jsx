import React, { useState, useEffect } from 'react';
import ModalCreateServiceDiscord from './../components/ModalCreateServiceDiscord';
import ModalCreateServiceTrello from './../components/ModalCreateServiceTrello';
import ModalCreateServiceUnknown from './../components/ModalCreateServiceUnknown';
import ModalCreateServiceSendgrid from './../components/ModalCreateServiceSendgrid';
import ModalCreateServiceSlack from './../components/ModalCreateServiceSlack';
import ModalCreateServiceGitlab from './../components/ModalCreateServiceGitLab';
import ModalCreateServiceGithub from './../components/ModalCreateServiceGitHub';

function ModalDynamic(props) {
    const components = {
        Discord: ModalCreateServiceDiscord,
        Trello: ModalCreateServiceTrello,
        Twilio: ModalCreateServiceUnknown,
        SendGrid: ModalCreateServiceSendgrid,
        Slack: ModalCreateServiceSlack,
        GitLab: ModalCreateServiceGitlab,
        GitHub: ModalCreateServiceGithub,
        Unknown: ModalCreateServiceUnknown
    }
    const ModalService = components[props.service || 'Unknown'];
    return (<ModalService choice={props.choice} service={props.service}/>)
}
export default ModalDynamic;