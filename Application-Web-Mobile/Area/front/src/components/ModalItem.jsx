import React, { useState } from "react";

import { Modal, IconButton, Icon, Tooltip, Whisper } from "rsuite";

import ModalConfirm from "./ModalConfirm";

import { getWidgetDetail } from "../apiUtils/loginAndMore";
import { chooseIconsToRender } from "../apiUtils/chooseIcons";

import "../pages/utils.css";

const tooltip = <Tooltip>Delete link</Tooltip>;

const empty = (
  <div
    style={{
      display: "flex",
      justifyContent: "center",
      width: "100%",
    }}
  >
    <div className="listCardWorflowEmpty" style={{ width: "70%" }}>
      <h1
        style={{
          fontFamily: "sans-serif",
          fontWeight: "bold",
          color: "black",
          fontSize: 25,
        }}
      >
        {" "}
        <Icon
          icon="dropbox"
          size="2x"
          style={{ marginRight: "10px", color: "red" }}
        />
        Ooops! You don't have link
      </h1>
    </div>
  </div>
);

function ModalItem({ close, show, widget }) {
  const [showConfirm, setShowConfirm] = useState(false);
  const [linkActive, setLinkActive] = useState({});
  const [detail, setDetail] = useState([]);
  const [get, setGet] = useState(true);

  const closeConfirm = () => {
    setShowConfirm(false);
  };

  const openConfirm = (link) => {
    setShowConfirm(true);
    setLinkActive(link);
  };

  const getWidgetDetailFunc = async () => {
    const res = await getWidgetDetail({ serviceId: widget.id });
    setDetail(res);
  };

  if (widget && show && get) {
    setGet(false);
    getWidgetDetailFunc();
  }

  const handleClose = () => {
    setGet(true);
    setDetail([]);
    close();
  };

  const renderItem = () => {
    if (detail === undefined || detail?.length === 0) {
      return empty;
    }
    return detail?.map((el, idx) => {
      return (
        <div
          key={idx}
          className="listCardWorflow"
          style={{ justifyContent: "space-between" }}
        >
          <div
            style={{
              display: "flex",
              flexDirection: "row",
              alignContent: "center",
              alignItems: "center",
              marginLeft: "3%",
              width: "60%",
            }}
          >
            <div
              style={{
                display: "flex",
                flexDirection: "row",
                alignContent: "center",
                alignItems: "center",
                width: "100%",
              }}
            >
              <Icon
                icon={chooseIconsToRender(el.serviceActionName)}
                size="2x"
                style={{
                  marginRight: "10px",
                  marginLeft: "10px",
                  color: "#0275d8",
                }}
              />
              <Whisper
                placement="top"
                trigger="hover"
                speaker={<Tooltip>{el.serviceActionName}</Tooltip>}
              >
                <h4>{el.serviceActionType}</h4>
              </Whisper>
              <Icon
                size={"2x"}
                style={{
                  color: "#0275d8",
                  marginLeft: "10px",
                }}
                icon="long-arrow-right"
              />
              <Icon
                icon={chooseIconsToRender(el.serviceReactionName)}
                size="2x"
                style={{
                  marginRight: "10px",
                  color: "#0275d8",
                  marginLeft: "10px",
                }}
              />
              <Whisper
                placement="top"
                trigger="hover"
                speaker={<Tooltip>{el.serviceReactionName}</Tooltip>}
              >
                <h4>{el.serviceReactionType}</h4>
              </Whisper>
            </div>
          </div>
          <div className="flexRow">
            <div style={{ marginRight: "10px", marginLeft: "20px" }}>
              <Whisper placement="top" trigger="hover" speaker={tooltip}>
                <IconButton
                  size="lg"
                  icon={<Icon style={{ color: "red" }} icon="close" />}
                  circle
                  onClick={() => openConfirm(el)}
                />
              </Whisper>
            </div>
          </div>
        </div>
      );
    });
  };

  return (
    <>
      <div className="modal-container">
        <Modal size={"lg"} show={show} onHide={handleClose}>
          <Modal.Header>
            <Modal.Title>{widget.name} Link</Modal.Title>
          </Modal.Header>
          <Modal.Body>{renderItem()}</Modal.Body>
        </Modal>
      </div>
      <ModalConfirm show={showConfirm} close={closeConfirm} link={linkActive} />
    </>
  );
}

export default ModalItem;
