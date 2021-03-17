import React, { useState, useEffect } from 'react';
import { Modal, Icon, Button, ButtonToolbar } from "rsuite";
import axios from 'axios';
import querystring from 'querystring';
import { API_URL } from '../config'

function ModalCreateServiceDiscord(props) {
	const [show, setShow] = useState(true);
	const [listDisplay, setListDisplay] = useState([]);
	const [selectedServer, setSelectedServer] = useState(false);
	const [listChannelDisplay, setListChannelDisplay] = useState([]);
	const [selectedChannel, setSelectedChannel] = useState(false);
	// const [selectedBoardColumn, setSelectedBoardColumn] = useState(false);
	const close = () => {
		setShow(false);
	}
	const getRepos = async () => {
		const datas = await axios.get(`${API_URL}service/gitlab/getRepos`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		await setListDisplay(datas.data.success);
		console.log(datas.data.success)
	}
	const finale = async(repo) => {
		const data = {
			projectId: repo.id,
			projectName: repo.name,
		}
		props.choice(data);
		close();
	}
	useEffect(() => {
		getRepos();
	}, []);
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>Gitlab</Modal.Title>
				</Modal.Header>
				<Modal.Body>
				<div style={{display: 'flex', flexWrap: 'wrap', flexBasis: '33.3', justifyContent: 'space-around', alignItems: 'baseline', textAlign: 'center'}}>
					{listDisplay.map((server) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
									<p>{server.name}</p>
									<p>{server.description.slice(0, 30) + (server.description.length > 30 ? "..." : "")}</p>
									<Button appearance='primary' onClick={() => finale(server)}>Select this Server</Button>
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

export default ModalCreateServiceDiscord;
