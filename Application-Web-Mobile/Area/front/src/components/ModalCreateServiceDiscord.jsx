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
	const getServers = async () => {
		const datas = await axios.get(`${API_URL}service/Discord/getServer`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		await setListDisplay(datas.data);
		console.log(datas);
	}
	const getChannels = async (server) => {
		setSelectedServer(server)
		const datas = await axios.get(`${API_URL}service/Discord/getChannels/${server.serverId}`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		setListChannelDisplay(datas.data.success);
	}
	const finale = async(channel) => {
		const data = {
			serverId: selectedServer.serverId,
			serverName: selectedServer.serverName,
			channelName: channel.name,
			channelId: channel.id
		}
		console.log('Discord', data)
		props.choice(data);
	}
	useEffect(() => {
		getServers();
	}, []);
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>Discord</Modal.Title>
				</Modal.Header>
				<Modal.Body>
				<div style={{display: 'flex', flexWrap: 'wrap', flexBasis: '33.3', justifyContent: 'space-around', alignItems: 'baseline'}}>
					{listDisplay.map((server) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
									<p>{server.serverName}</p>
									<Button appearance='primary' onClick={() => getChannels(server)}>Select this Server</Button>
								</div>
						)})
					}
					</div>
					<div style={{marginTop: '25%', display: 'flex', justifyContent: 'space-around', alignItems: 'baseline', flexWrap: 'wrap', flexBasis: '33.3'}}>
					{listChannelDisplay.map((elem) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
									<p>{elem.name}</p>
									<Button appearance='primary' onClick={() => { finale(elem); close() }}>Select this Channel</Button>
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
