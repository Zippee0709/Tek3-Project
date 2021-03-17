import React, { useState, useEffect } from "react";
import axios from "axios";
import { Link, useHistory } from "react-router-dom";
import {
  Alert,
  Form,
  FormGroup,
  ControlLabel,
  FormControl,
  HelpBlock,
  ButtonToolbar,
  Button,
} from "rsuite";

import { API_URL } from "../config";

import querystring from "querystring";

function Register() {
  const [pseudo, setPseudo] = useState("");
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const history = useHistory();

  useEffect(() => {
    const token = JSON.parse(localStorage.getItem("token"));
    if (token) {
      Alert.success("Already Logged");
      setTimeout(() => {
        history.push("/");
      }, 1000);
    }
  });

  const register = async () => {
    try {
      const registerResponse = await axios.post(
        `${API_URL}user/register`,
        querystring.stringify({
          pseudo,
          email,
          password,
        }),
        { headers: { "Content-Type": "application/x-www-form-urlencoded" } }
      );
      Alert.success(
        registerResponse.data.success +
          "An confirmation email was sent please check your email"
      );
      setTimeout(() => {
        history.push("/login");
      }, 1000);
    } catch (e) {
      Alert.error(e.response.data.error);
    }
  };
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
      <Form>
        <FormGroup>
          <ControlLabel>Pseudo</ControlLabel>
          <FormControl onChange={(pseudo) => setPseudo(pseudo)} name="pseudo" />
          <HelpBlock>Required</HelpBlock>
        </FormGroup>
        <FormGroup>
          <ControlLabel>Email</ControlLabel>
          <FormControl
            onChange={(email) => setEmail(email)}
            name="email"
            type="email"
          />
          <HelpBlock tooltip>Required</HelpBlock>
        </FormGroup>
        <FormGroup>
          <ControlLabel>Password</ControlLabel>
          <FormControl
            onChange={(password) => setPassword(password)}
            name="password"
            type="password"
          />
        </FormGroup>
        <FormGroup>
          <ButtonToolbar>
            <Button appearance="primary" onClick={register}>
              Register
            </Button>
            <Link to={(location) => ({ ...location, pathname: "/login" })}>
              <Button appearance="default">Login</Button>
            </Link>
          </ButtonToolbar>
        </FormGroup>
      </Form>
    </div>
  );
}

export default Register;
