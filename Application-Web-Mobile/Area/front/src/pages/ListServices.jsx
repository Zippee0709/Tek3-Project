import React, { useEffect, useState } from "react";

import { Icon } from "rsuite";

import mainLogo from "../assets/area_logo.png";

import { getWidgets } from "../apiUtils/loginAndMore";

import ListItem from "../components/ListItem";
import "./utils.css";

function ListServices({ handleSelect }) {
  const [widgets, setWidgets] = useState([]);
  const [updateNav, setUpdateNav] = useState(true);

  useEffect(() => {
    async function getWidgetsFunc() {
      const res = await getWidgets();
      setWidgets(res);
    }

    if (updateNav) {
      handleSelect("3");
      setUpdateNav(false);
    } else {
      getWidgetsFunc();
    }
  }, [setWidgets, handleSelect, setUpdateNav, updateNav]);
  return (
    <>
      <div
        style={{
          alignItems: "center",
          alignSelf: "center",
          justifyContent: "center",
          display: "flex",
          marginTop: "1%",
        }}
      >
        <img
          src={mainLogo}
          alt="fireSpot"
          style={{ width: "200px", height: "200px" }}
        />
        <div>
          <h1 className="title">Your</h1>
          <h1 className="title">
            workflow{" "}
            <Icon
              icon="dropbox"
              size="3x"
              style={{ marginRight: "10px", color: "black" }}
            />
          </h1>
        </div>
      </div>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          width: "100%",
          height: "100%",
        }}
      >
        <ListItem widgets={widgets} />
      </div>
    </>
  );
}

export default ListServices;
