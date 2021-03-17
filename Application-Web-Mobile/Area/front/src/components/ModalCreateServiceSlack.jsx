import React, { useState, useEffect } from 'react';
import { Modal, Icon, Button, ButtonToolbar } from "rsuite";
import axios from 'axios';
import querystring from 'querystring';
import { API_URL } from '../config'

function ModalCreateService(props) {
	const [show, setShow] = useState(true);
	const [listDisplay, setListDisplay] = useState([]);
	const [selectedBoard, setSelectedBoard] = useState(false);
	const [listColumnDisplay, setListColumnDisplay] = useState([]);
	const [selectedBoardColumn, setSelectedBoardColumn] = useState(false);
	const close = () => {
		setShow(false);
	}
	const getTeam = async () => {
		const datas = await axios.get(`${API_URL}service/Slack/getRegisteredTeam`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		await setListDisplay(datas.data.success);
		console.log(datas);
	}
	const finale = async(elem) => {
		const data = {
			data : {
				success: {
					_id: elem.idService,
					serverName: elem.serverName
				}
			}
		}
		props.choice(data);
		console.log(data);
		close();
	}
	useEffect(() => {
		getTeam();
	}, []);
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>Slack</Modal.Title>
				</Modal.Header>
				<Modal.Body>
					<div style={{display: 'flex', justifyContent: 'space-around', alignItems: 'baseline'}}>
					{listDisplay.map((elem) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
									<p>{elem.serverName}</p>
									<Button appearance='primary' onClick={() => finale(elem)}>Select this board</Button>
								</div>
						)})
					}
					</div>
				</Modal.Body>
				<Modal.Footer>
				</Modal.Footer>
			</Modal>
		</div>
	);
}

export default ModalCreateService;
