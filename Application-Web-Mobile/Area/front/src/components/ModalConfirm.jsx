import React from "react";

import { Modal, Icon, Button } from "rsuite";

import { deleteLink } from "../apiUtils/loginAndMore";

import "../pages/utils.css";

function Confirm({ close, show, link }) {
  const handleDelete = async () => {
    await deleteLink({ serviceId: link.id });
    close();
  };
  return (
    <div className="modal-container">
      <Modal size={"xs"} show={show} onHide={close}>
        <Modal.Header>
          <Modal.Title>Confirmation</Modal.Title>
        </Modal.Header>
        <Modal.Body>
          <Icon
            icon="remind"
            style={{
              color: "#ffb300",
              marginLeft: "10px",
              marginRight: "10px",
              fontSize: 24,
            }}
          />
          {"  "}
          Are you sure you want delete this link?
        </Modal.Body>
        <Modal.Footer>
          <Button onClick={handleDelete} appearance="primary">
            YES
          </Button>
          <Button onClick={close} appearance="primary" color="red">
            NO
          </Button>
        </Modal.Footer>
      </Modal>
    </div>
  );
}

export default Confirm;
