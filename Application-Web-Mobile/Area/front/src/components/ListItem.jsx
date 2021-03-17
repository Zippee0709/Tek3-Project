import React, { useState } from "react";

import { Link } from "react-router-dom";

import { Icon, IconButton } from "rsuite";

import ModalItem from "./ModalItem";

import { chooseIconsToRender } from "../apiUtils/chooseIcons";
import "../pages/utils.css";

const empty = (
  <div
    style={{
      display: "flex",
      justifyContent: "center",
    }}
  >
    <div className="listCardWorflowEmpty">
      <h1
        style={{
          fontFamily: "sans-serif",
          fontWeight: "bold",
          color: "black",
          fontSize: 25,
          marginTop: "2%",
        }}
      >
        {" "}
        <Icon
          icon="dropbox"
          size="2x"
          style={{ marginRight: "10px", color: "red" }}
        />
        Ooops! You don't have workflows
      </h1>
      <h1
        style={{
          fontFamily: "sans-serif",
          fontWeight: "bold",
          fontSize: 17,
          textAlign: "center",
          color: "grey",
        }}
      >
        It's seem you don't have created a workflow
      </h1>
      <Link
        to="/create"
        style={{
          marginBottom: "2%",
        }}
      >
        Clich here for create a workflow
      </Link>
    </div>
  </div>
);

function ListItem({ widgets }) {
  const [show, setShow] = useState(false);
  const [widgetActive, setWidgetActive] = useState({});

  const close = () => {
    setShow(false);
  };

  const open = (widget) => {
    setShow(true);
    setWidgetActive(widget);
  };

  if (widgets === undefined || widgets?.length === 0) {
    return empty;
  }

  return widgets?.map((widget, index) => {
    return (
      <div
        key={index * 3}
        style={{
          width: "100%",
          justifyContent: "center",
          display: "flex",
        }}
      >
        <div
          className="listCardWorflow"
          style={{ justifyContent: "space-between" }}
        >
          <div
            style={{
              display: "flex",
              flexDirection: "row",
              alignContent: "center",
              alignItems: "center",
            }}
          >
            <Icon
              icon={chooseIconsToRender(widget.name)}
              size="2x"
              style={{
                marginRight: "10px",
                color: "#0275d8",
                marginLeft: "10px",
              }}
            />
            <h1
              style={{
                fontFamily: "sans-serif",
                fontWeight: "bold",
                color: "black",
                fontSize: 25,
              }}
            >
              {" "}
              {widget.name}
            </h1>
          </div>
          <div className="flexRow" style={{ width: "13%" }}>
            <div className="flexColumn" style={{ marginRight: "10%" }}>
              <h4>1</h4>
              <p>Connection</p>
            </div>
            <div style={{ marginRight: "10%" }} className="flexColumn">
              <h4>{widget.nbrArea}</h4>
              <p style={{ marginBottom: "2px" }}>Link</p>
            </div>
            <div style={{ marginRight: "10%" }}>
              <IconButton
                size="lg"
                icon={<Icon icon="angle-right" />}
                circle
                onClick={() => open(widget)}
              />
            </div>
          </div>
        </div>
        <ModalItem show={show} close={close} widget={widgetActive} />
      </div>
    );
  });
}

export default ListItem;
