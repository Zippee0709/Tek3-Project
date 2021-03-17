import React, { useEffect } from "react";

import { Link } from "react-router-dom";

import { Icon } from "rsuite";

import mainLogo from "../assets/area_logo.png";
import "./utils.css";

function ClientAPK() {
  // useEffect(() => {
  //   async function openVolumes() {
  //     try {
  //       const confirmResponse = await axios.post(
  //         `${API_URL}user/confirmation`,
  //         querystring.stringify({
  //           userConfirmationToken: token,
  //         }),
  //         { headers: { "Content-Type": "application/x-www-form-urlencoded" } }
  //       );
  //       console.log(confirmResponse);
  //       Alert.success(confirmResponse.data.success);
  //       setTimeout(() => {
  //         history.push("/login");
  //       }, 2000);
  //     } catch (e) {
  //       Alert.error(e.response.data.error);
  //       setTimeout(() => {
  //         history.push("/login");
  //       }, 2000);
  //     }
  //   }
  //   openVolumes();
  // });

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
          <h1 className="title">Download</h1>
          <h1 className="title">
            APK area{" "}
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
          justifyContent: "center",
          flexDirection: "column",
          alignContent: "center",
          alignItems: "center",
        }}
      >
        <div
          style={{
            display: "flex",
            flexDirection: "row",
            justifyContent: "center",
            width: "100%",
          }}
        >
          <div className="homeCardWorflow" style={{ marginRight: "2%" }}>
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
                icon="arrow-circle-o-down"
                size="2x"
                style={{ marginRight: "10px", color: "#0275d8" }}
              />
              Mobile version
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
              Want download android apk?
            </h1>
            <Link
              style={{ marginBottom: "2%" }}
              to="/release/app-arm64-v8a-release.apk"
              target="_blank"
              download
            >
              Clich here for download arm64 apk
            </Link>
            <Link
              style={{ marginBottom: "2%" }}
              to="/release/app-armeabi-v7a-release.apk"
              target="_blank"
              download
            >
              Clich here for download armeabi apk
            </Link>
            <Link
              style={{ marginBottom: "2%" }}
              to="/release/app-x86_64-release.apk"
              target="_blank"
              download
            >
              Clich here for download x86_64 apk
            </Link>
          </div>
        </div>
      </div>
    </>
  );
}

export default ClientAPK;
