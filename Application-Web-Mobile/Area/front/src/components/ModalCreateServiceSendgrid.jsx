import React, { useState, useEffect } from 'react';
import { Modal, Icon, Button, ButtonToolbar, Input } from "rsuite";
import axios from 'axios';
import qs from 'querystring';
import { API_URL } from '../config'

function ModalCreateServiceSendgrid(props) {
	const [show, setShow] = useState(true);
    const [subject, setSubject] = useState('');
    const [body, setBody] = useState('');
    const [receiver, setReceiver] = useState('');
    const setServiceSendGrid = async () => {
        const data = {
            object: subject,
            message: body, recipientEmail: receiver
        };
        props.choice(data);
        console.log(data);
        close()
    }
	const close = () => {
		setShow(false);
	}
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>Sendgrid Email</Modal.Title>
				</Modal.Header>
				<Modal.Body>
                    <p>Subject</p>
                    <Input onChange={(e) => {setSubject(e)}} placeholder='Create your account'></Input>
                    <p>Body</p>
                    <Input onChange={(e) => {setBody(e)}} placeholder='Welcome to area, please cre...'></Input>
                    <p>Receiver</p>
                    <Input onChange={(e) => {setReceiver(e)}} placeholder='yannis.coulibaly@epitech.eu'></Input>
				</Modal.Body>
				<Modal.Footer>
                    <Button onClick={setServiceSendGrid}>Confirm Email</Button>
				</Modal.Footer>
			</Modal>
		</div>
	);
}

export default ModalCreateServiceSendgrid;
