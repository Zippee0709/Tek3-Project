import React from "react";

import { Link } from "react-router-dom";

import { Icon } from "rsuite";

import mainLogo from "../assets/area_logo.png";
import "./utils.css";

function Home() {
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
          <h1 className="title">Welcome</h1>
          <h1 className="title">
            to area{" "}
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
                icon="plus-circle"
                size="2x"
                style={{ marginRight: "10px", color: "#0275d8" }}
              />
              Create your own Worflow
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
              Know exactly what you want to build?
            </h1>
            <Link
              to="/create"
              style={{
                marginBottom: "2%",
              }}
            >
              Clich here for create your own workflow
            </Link>
          </div>
          <div className="homeCardWorflow">
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
                style={{ marginRight: "10px", color: "#0275d8" }}
              />
              Your list of Worflows
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
              You have already create workflow?
            </h1>
            <Link
              to="/widgets"
              style={{
                marginBottom: "2%",
              }}
            >
              Clich here for see your list of workflow
            </Link>
          </div>
        </div>
        <div className="homeCardWorflow">
          <h1
            style={{
              fontFamily: "sans-serif",
              fontWeight: "bold",
              color: "black",
              fontSize: 25,
              marginTop: "2%",
            }}
          >
            <Icon
              icon="arrow-circle-o-down"
              size="2x"
              style={{ marginRight: "10px", color: "#0275d8" }}
            />
            Mobile version available
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
            You can use a mobile version of AREA availalbe on playstore
          </h1>
          <Link
            style={{ marginBottom: "2%" }}
            to="/client.apk"
          >
            Clich here for get the mobile version
          </Link>
        </div>
      </div>
    </>
  );
}

export default Home;
