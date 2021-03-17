import React, { useState } from "react";
import { BrowserRouter as Router, Switch, Route } from "react-router-dom";
import Test from "./pages/Test";
import * as firebase from "firebase";
import Login from "./pages/Login";
import Register from "./pages/Register";
import Home from "./pages/Home";
import ListServices from "./pages/ListServices";
import GenericNotFound from "./pages/GenericNotFound";
import PrivateRoute from "./components/privateRoute";
import ConfirmationEmail from "./pages/ConfirmationEmail";
import CreateArea from "./pages/CreateArea";
import Logout from "./pages/Logout";
import Navbar from "./components/Navbar";
import ClientAPK from "./pages/ClientAPK";
import firebaseConfig from "./firebase.config";

import "./App.css";
import "rsuite/dist/styles/rsuite-default.css";

firebase.default.initializeApp(firebaseConfig);

function RouterArea() {
  const [activeKey, setActiveKey] = useState("1");

  const handleSelect = (eventKey) => {
    setActiveKey(eventKey);
  };

  return (
    <Router>
      <Switch>
        <PrivateRoute
          path="/services"
          component={() => <Test handleSelect={handleSelect} />}
        />
        <Route path="/login" component={Login} />
        <Route path="/register" component={Register} />
        <Route exact path="/client.apk" component={ClientAPK} />
        <Route
          path="/logout"
          component={() => <Logout handleSelect={handleSelect} />}
        />
        <Route
          path="/widgets"
          component={() => <ListServices handleSelect={handleSelect} />}
        />
        <Route
          exact
          path="/user/confirmation/:token"
          component={ConfirmationEmail}
        />
        <PrivateRoute exact path="/" component={Home} />
        <PrivateRoute
          exact
          path="/create"
          component={() => <CreateArea handleSelect={handleSelect} />}
        />
        <Route path="*" exact={true} component={GenericNotFound} />
      </Switch>
      {localStorage.getItem("token") && (
        <Navbar handleSelect={handleSelect} activeKey={activeKey} />
      )}
    </Router>
  );
}

export default RouterArea;
