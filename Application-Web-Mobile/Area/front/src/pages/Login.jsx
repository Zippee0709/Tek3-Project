import React, { useState, useEffect } from "react";
import { Link, useHistory } from "react-router-dom";
import {
  Form,
  FormGroup,
  ControlLabel,
  FormControl,
  HelpBlock,
  ButtonToolbar,
  Button,
  Alert,
} from "rsuite";

import * as firebase from "firebase";

import {
  login,
  checkEmailConfirmed,
  loginAuth2,
} from "../apiUtils/loginAndMore";

function Login(props) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [redirectUrl, setRedirectUrl] = useState({});
  const history = useHistory();

  useEffect(() => {
    const { location } = props;
    const { state } = location;
    setRedirectUrl(state);
    const token = JSON.parse(localStorage.getItem("token"));
    if (token) {
      Alert.success("Already Logged");
      setTimeout(() => {
        history.push("/");
      }, 1000);
    }
  });

  const loginCallAPI = async () => {
    await login({ history, email, password, redirectUrl });
    await checkEmailConfirmed({ history });
  };

  const signInWithGoogle = () => {
    const provider = new firebase.default.auth.GoogleAuthProvider();
    firebase.default
      .auth()
      .setPersistence(firebase.default.auth.Auth.Persistence.SESSION)
      .then(() => {
        firebase.default
          .auth()
          .signInWithPopup(provider)
          .then(async (res) => {
            console.log(res);
            const data = {
              email: res.user.email,
              pseudo: res.user.displayName,
              firebaseId: res.user.uid,
            };
            await loginAuth2({ data, history });
          })
          .catch((e) => Alert.error(e.message));
      });
  };

  const signInWithGithub = () => {
    const provider = new firebase.default.auth.GithubAuthProvider();
    provider.addScope("repo");
    provider.setCustomParameters({
      allow_signup: "false",
    });
    firebase.default
      .auth()
      .setPersistence(firebase.default.auth.Auth.Persistence.SESSION)
      .then(() => {
        firebase.default
          .auth()
          .signInWithPopup(provider)
          .then(async (res) => {
            const data = {
              email: res.user.email,
              pseudo:
                res.user.displayName === null
                  ? res.user.email.split("@")[0]
                  : res.user.displayName,
              firebaseId: res.user.uid,
            };
            await loginAuth2({ data, history });
          })
          .catch((e) => Alert.error(e.message));
      });
  };

  const signInWithMicrosoft = () => {
    const provider = new firebase.default.auth.OAuthProvider("microsoft.com");
    provider.setCustomParameters({
      tenant: "901cb4ca-b862-4029-9306-e5cd0f6d9f86",
    });
    firebase.default
      .auth()
      .signInWithPopup(provider)
      .then(async (res) => {
        const data = {
          email: res.user.email,
          pseudo:
            res.user.displayName === null
              ? res.user.email.split("@")[0]
              : res.user.displayName,
          firebaseId: res.user.uid,
        };
        await loginAuth2({ data, history });
      })
      .catch((e) => {
        // Handle error.
        Alert.error(e.message);
      });
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
            <Button appearance="primary" onClick={loginCallAPI}>
              Login
            </Button>
            <Link to={(location) => ({ ...location, pathname: "/register" })}>
              <Button appearance="default">Register</Button>
            </Link>
          </ButtonToolbar>
        </FormGroup>
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            alignContent: "flex-start",
            alignItems: "flex-start",
            justifyContent: "center",
          }}
        >
          <div
            style={{
              display: "flex",
              flexDirection: "row",
              justifyContent: "space-between",
            }}
          >
            <Button
              onClick={() => signInWithGoogle()}
              appearance="default"
              className="googleBtn"
              type="button"
              style={{ marginTop: "10px" }}
            >
              <img
                src="https://upload.wikimedia.org/wikipedia/commons/5/53/Google_%22G%22_Logo.svg"
                alt="logo"
                width={30}
                height={30}
                style={{ marginRight: "10px" }}
              />
              Login With Google
            </Button>
            <Button
              onClick={() => signInWithGithub()}
              appearance="default"
              type="button"
              className="googleBtn"
              style={{ marginTop: "10px", marginLeft: "10px" }}
            >
              <img
                src="https://upload.wikimedia.org/wikipedia/commons/thumb/9/91/Octicons-mark-github.svg/1024px-Octicons-mark-github.svg.png"
                alt="logo"
                width={30}
                height={30}
                style={{ marginRight: "10px" }}
              />
              Login With Github
            </Button>
          </div>
          <div
            style={{
              width: "100%",
              textAlign: "center",
            }}
          >
            <Button
              onClick={() => signInWithMicrosoft()}
              appearance="default"
              type="button"
              className="googleBtn"
              style={{
                marginTop: "10px",
              }}
            >
              <img
                src="https://www.flaticon.com/svg/vstatic/svg/732/732221.svg?token=exp=1615043963~hmac=fab28d0f91a20cd8527f56e0072958ae"
                alt="logo"
                width={30}
                height={30}
                style={{ marginRight: "10px" }}
              />
              Login With Microsoft
            </Button>
          </div>
        </div>
      </Form>
    </div>
  );
}

export default Login;
