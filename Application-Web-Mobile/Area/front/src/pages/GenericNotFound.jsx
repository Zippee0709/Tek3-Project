import React from "react";
import { Button } from "rsuite";
import { Link } from "react-router-dom";

function GenericNotFound() {
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
        <h1>Page Not Found</h1>
        <h2>404 Error</h2>
        <Link to={(location) => ({ ...location, pathname: "/" })}>
          <Button appearance="primary">Back to home</Button>
        </Link>
      </div>
    </div>
  );
}

export default GenericNotFound;
