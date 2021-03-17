import React, { useEffect, useState } from "react";

import { useHistory } from "react-router-dom";

import { logout } from "../apiUtils/loginAndMore";

import { Icon } from "rsuite";

import mainLogo from "../assets/area_logo.png";
import "./utils.css";

function Logout({ handleSelect }) {
  const [updateNav, setUpdateNav] = useState(true);
  const history = useHistory();

  useEffect(() => {
    async function logoutFunc({ history }) {
      await logout({ history });
    }

    if (updateNav) {
      handleSelect("5");
      setUpdateNav(false);
    } else if (history) {
      logoutFunc({ history });
    }
  }, [history, setUpdateNav, updateNav, handleSelect]);

  return (
    <div>
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
          <h1 className="title">Logout</h1>
          <h1 className="title">
            Good bye{" "}
            <Icon
              icon="smile-o"
              size="3x"
              style={{ marginRight: "10px", color: "black" }}
            />
          </h1>
        </div>
      </div>
      <div
        style={{
          display: "flex",
          flexDirection: "row",
          justifyContent: "center",
          marginTop: "5%",
        }}
      >
        <img
          className="logoutRoundCircle"
          style={{ borderRadius: "900px" }}
          src="https://media.giphy.com/media/VJrw9pfsOE63EPgoaE/giphy.gif"
          width={"30%"}
          alt="good bye..."
        />
      </div>
    </div>
  );
}

export default Logout;
