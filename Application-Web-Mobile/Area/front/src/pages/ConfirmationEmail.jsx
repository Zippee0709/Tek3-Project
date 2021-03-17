import React, { useEffect } from "react";
import axios from "axios";

import { useParams, useHistory } from "react-router-dom";

import querystring from "querystring";
import { Alert } from "rsuite";

import mainLogo from "../assets/area_logo.png";

import { API_URL } from "../config";

function ConfirmationEmail() {
  const { token } = useParams();
  const history = useHistory();

  useEffect(() => {
    async function confirmFunc() {
      try {
        const confirmResponse = await axios.post(
          `${API_URL}user/confirmation`,
          querystring.stringify({
            userConfirmationToken: token,
          }),
          { headers: { "Content-Type": "application/x-www-form-urlencoded" } }
        );
        console.log(confirmResponse);
        Alert.success(confirmResponse.data.success);
        setTimeout(() => {
          history.push("/login");
        }, 2000);
      } catch (e) {
        Alert.error(e.response.data.error);
        setTimeout(() => {
          history.push("/login");
        }, 2000);
      }
    }
    confirmFunc();
  });

  return (
    <div
      style={{
        width: "100%",
        height: "100%",
        overflow: "hidden",
        position: "absolute",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
      }}
    >
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
        }}
      >
        <img
          src={mainLogo}
          alt="fireSpot"
          style={{ width: "250px", height: "250px" }}
        />
        <h1>Confirmation Email</h1>
      </div>
    </div>
  );
}

export default ConfirmationEmail;
