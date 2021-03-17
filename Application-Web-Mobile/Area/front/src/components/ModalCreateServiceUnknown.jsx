import React, { useState, useEffect } from 'react';
import { Modal, Icon, Button, ButtonToolbar, Input } from "rsuite";
import axios from 'axios';
import qs from 'querystring';
import { API_URL } from '../config'

function ModalCreateServiceTwillio(props) {
	const [show, setShow] = useState(false);
    const test = () => {
        props.choice({});
        console.log('Choice setted')
    }
    useEffect(() => {
        test();
    }, [])
	return (
		<div className="modal-container">
			<Modal show={show}>
				<Modal.Header>
				</Modal.Header>
				<Modal.Body>
				</Modal.Body>
				<Modal.Footer>
				</Modal.Footer>
			</Modal>
		</div>
	);
}

export default ModalCreateServiceTwillio;
