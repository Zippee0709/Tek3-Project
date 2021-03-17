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
	const getBoard = async () => {
		const datas = await axios.get(`${API_URL}service/${'Trello'}/getBoards`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		await setListDisplay(datas.data.success);
	}
	const getColumnBoard = async (board) => {
		setSelectedBoard(board)
		const datas = await axios.get(`${API_URL}service/${'Trello'}/getListOfBoard/${board.id}`, { headers: { Authorization: `Bearer ${localStorage.getItem('token')}` } });
		setListColumnDisplay(datas.data.success);
	}
	const finale = async(elem) => {
		const data = {
			boardName: selectedBoard.name,
			boardId: selectedBoard.id,
			listName: elem.name,
			listId: elem.id
		}
		console.log('Trello', data)
		props.choice(data);
	}
	useEffect(() => {
		getBoard();
	}, []);
	return (
		<div className="modal-container">
			<Modal show={show} onHide={close}>
				<Modal.Header>
				<Modal.Title>{props.serviceName}</Modal.Title>
				</Modal.Header>
				<Modal.Body>
					<div style={{display: 'flex', justifyContent: 'space-around', alignItems: 'baseline'}}>
					{listDisplay.map((elem) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center'}}>
									<p>{elem.name}</p>
									<Button appearance='primary' onClick={() => getColumnBoard(elem)}>Select this board</Button>
								</div>
						)})
					}
					</div>
					{<div style={{marginTop: '25%', display: 'flex', justifyContent: 'space-around', alignItems: 'baseline', flexWrap: 'wrap'}}>
					{listColumnDisplay.map((elem) => {
						return (
								<div style={{display: 'flex', flexDirection: 'column', alignItems: 'center', flexBasis: '33.3'}}>
									<p>{elem.name}</p>
									<Button appearance='primary' onClick={() => { finale(elem); close() }}>Select this Column</Button>
								</div>
						)})
					}
					</div>}
				</Modal.Body>
				<Modal.Footer>
				</Modal.Footer>
			</Modal>
		</div>
	);
}

export default ModalCreateService;
