import React, { useState } from 'react';
import axios from 'axios';
import querystring from 'querystring';
import { Alert, SelectPicker, Button } from 'rsuite';
import { API_URL } from '../config'
import ModalCreateService from '../components/ModalDynamic';
const qs = require('qs');

function CreateArea() {
    const [selectedReaction, setSelectedReaction] = useState('');
    const [selectedAction, setSelectedAction] = useState('');
    const [firstService, setFirstService] = useState('');
    const [secondService, setSecondService] = useState('');
    const [listServices, setListServices] = useState([]);
    const [actionList, setActionList] = useState([]);
    const [reactionList, setReactionList] = useState([]);
    const [firstChoice, setFirstChoice] = useState(false);
    const [secondChoice, setSecondChoice] = useState(false);
    
    const getListServices = async () => {
        try {
            const searchTab = [];
            const datas = await axios.get(`${API_URL}service/getServices`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
            console.log(datas);
            datas.data.servicesList[0].user.forEach(service => {
                searchTab.push({
                    'label': service.name,
                    'value': service.name,
                       'id': service.id
                });
            });
            setListServices(searchTab);
        } catch (e) {
            console.log(e);
            if (!!e.response === true)
                Alert.error(e.response.data.error);
            else
                Alert.error(e);
        }
    }
    const setArea = async (type, setChangement) => {
        if (!firstService.length || !secondService.length) {
            Alert.warning('Please select two services before');
            return;
        }
        try {
            const searchTab = [];
            const datas = await axios.get(`${API_URL}service/getServices/${type}/${firstService}/${secondService}`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
            const servicesReactionData = datas.data['services' + type.charAt(0).toUpperCase() + type.slice(1)];
            for (const [key] of Object.entries(servicesReactionData)) {
                servicesReactionData[key].forEach(reactions => {
                    reactions.forEach(reaction => {
                        console.log(reaction)
                        searchTab.push({
                            'label': reaction,
                            'value': reaction,
                        });
                    });
                });
            }
            setChangement(searchTab);
        } catch (e) {
            console.log(e)
            if (e.response.data.error)
                Alert.error(e.response.data.error);
            else
                Alert.error(e);
        }
    }
    const selectArea = () => {
        return (
            <div style={{ display: 'flex', justifyContent: 'space-evenly' }}>
                <div style={{ margin: '50px' }}>
                    <p><b>When this happens...</b></p>
                    <SelectPicker placeholder='Select a Trigger' onSelect={(serviceSelected) => setSelectedAction(serviceSelected)} data={actionList} onClick={() => setArea('action', setActionList)} style={{ width: 224 }} />
                </div>
                <div style={{ margin: '50px' }}>
                    <p><b>then do this!</b></p>
                    <SelectPicker placeholder='Select an Action' onSelect={(serviceSelected) => setSelectedReaction(serviceSelected)} data={reactionList} onClick={() => setArea('reaction', setReactionList)} style={{ width: 224 }} />
                </div>
            </div>
        );
    }
    const sendArea = async() => {
        try {
            const data1 = firstChoice;
            const data2 = secondChoice;
            let idFirst;
            let idSecond;
            if (firstService === "GitHub") {
                data1.events = selectedAction
            }
            if (secondService === "GitHub") {
                data2.events = selectedReaction
            }
            if (firstService !== 'Twilio' && firstService !== 'Slack') {
                idFirst = await axios.post(
                    `${API_URL}service/${firstService}/setService`, qs.stringify(data1), {
                        headers: {
                            Authorization: `Bearer ${localStorage.getItem('token')}`,
                            'Content-Type': 'application/x-www-form-urlencoded',
                        }
                    }
                )
            }
            if (secondService !== 'Twilio' && secondService !== 'Slack') {
                idSecond = await axios.post(
                    `${API_URL}service/${secondService}/setService`, qs.stringify(data2), {
                        headers: {
                            Authorization: `Bearer ${localStorage.getItem('token')}`,
                            'Content-Type': 'application/x-www-form-urlencoded',
                        }
                    }
                )
            }
            if (firstService === 'Slack') {
                idFirst = {
                    data: {
                        success: {
                            _id: data1.data.success._id
                        }
                    }
                }
            }
            if (secondService === 'Slack') {
                idSecond = {
                    data: {
                        success: {
                            _id: data2.data.success._id
                        }
                    }
                }
            }
            console.log(idFirst, idSecond);
            const data3 = qs.stringify({
                serviceActionName: firstService, serviceActionId: qs.stringify(data1) === '' ? 'nan' : idFirst.data.success._id,
                actionType: selectedAction, serviceReactionName: secondService,
                serviceReactionId: qs.stringify(data2) === '' ? 'nan' : idSecond.data.success._id, reactionType: selectedReaction,
                title: 'DAMN'
            });
            await axios.post(
                `${API_URL}area/setLink`, data3, {
                    headers: {
                        Authorization: `Bearer ${localStorage.getItem('token')}`,
                        'Content-Type': 'application/x-www-form-urlencoded',
                    }
                }
            )
        } catch(e) {
            console.log(e);
        }
    }
    return (
        <div style={{ width: '100%', height: '100%', overflow: 'hidden', position: 'absolute', display: 'flex', flexDirection: 'column', justifyContent: 'center', alignItems: 'center' }}>
            <div style={{ display: 'flex', justifyContent: 'space-evenly' }}>
                <div style={{ margin: '50px' }}>
                    <p><b>Connect this app...</b></p>
                    <SelectPicker placeholder='Search for an app' onSelect={(serviceSelected) => setFirstService(serviceSelected)} data={listServices} onClick={getListServices} style={{ width: 224 }} />
                </div>
                <div style={{ margin: '50px' }}>
                    <p><b>with this one!</b></p>
                    <SelectPicker placeholder='Search for an app' onSelect={(serviceSelected) => setSecondService(serviceSelected)} data={listServices} onClick={getListServices} style={{ width: 224 }} />
                </div>
            </div>
            {firstService.length > 0 && <ModalCreateService service={firstService} choice={setFirstChoice} ></ModalCreateService>}
            {firstService.length > 0 && secondService.length > 0 && <ModalCreateService service={secondService} choice={setSecondChoice} ></ModalCreateService>}
            {firstService.length > 0 && secondService.length > 0 && firstChoice && selectArea()}
            {selectedAction.length > 0 && selectedReaction.length > 0 && firstChoice && secondChoice && <Button onClick={sendArea} appearance="primary">Make a AREA!</Button>}
        </div>
    );
};

export default CreateArea;