import React, { useState, useEffect } from 'react';
import { Modal, Icon, Button, ButtonToolbar, Input, Alert, Notification } from "rsuite";
import axios from 'axios';
import qs from 'querystring';
import { API_URL } from '../config'

function notificationSuccessOauth(which, color) {
    Notification.open({
        duration: 10000,
        description: (
            <div style={{ display: 'flex', flexDirection: 'row', alignItems: 'center' }}>
                <Icon icon={which.toLowerCase()} style={{ color: color, fontSize: 40 }} />
                <div style={{ lineHeight: "4", borderLeft: "1px solid lightgrey", height: "50px", marginLeft: '10px', marginRight: '20px' }}></div>
                <div><Icon icon="check" style={{ color: 'green' }} />  Successfully logged in with <b>{which}</b> !</div>
            </div>
        )
    });
}

function ModalCreateServiceTwillio(props) {
	const [show, setShow] = useState(true);
    const [number, setNumber] = useState('');
    const addServiceTwilio = async () => {
        const data = qs.stringify({
            nameServices: 'Twilio', accessToken: 'empty',
            refreshToken: 'empty', tokenType: 'empty',
            phoneNumber: number
        });
        try {
        await axios.post(
            `${API_URL}service/createService`, data, {
                headers: {
                    Authorization: `Bearer ${localStorage.getItem('token')}`,
                    'Content-Type': 'application/x-www-form-urlencoded',
                }
            }
        )
        notificationSuccessOauth('Twilio added', '#007AC0');
        close();
        } catch(e) {
            Alert.error('Twilio already set')
        }
    }
	const close = () => {
		setShow(false);
	}
	// useEffect(() => {
	// }, []);
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>Twilio SMS</Modal.Title>
				</Modal.Header>
				<Modal.Body>
                    <p>Number phone</p>
                    <Input onChange={(e) => {setNumber(e); console.log(e)}} placeholder='0760040723'></Input>
				</Modal.Body>
				<Modal.Footer>
                    {number.length === 12 && <Button onClick={addServiceTwilio}>Confirm number</Button>}
				</Modal.Footer>
			</Modal>
		</div>
	);
}

export default ModalCreateServiceTwillio;
