import React from "react";
import { Sidenav, Nav, Icon } from "rsuite";

import { Link } from "react-router-dom";

export default class Navbar extends React.Component {
  constructor() {
    super();
    this.state = {
      expanded: false,
      // activeKey: '1'
    };
    this.handleToggle = this.handleToggle.bind(this);
    // this.handleSelect = this.handleSelect.bind(this);
  }
  handleToggle() {
    this.setState({
      expanded: !this.state.expanded,
    });
  }
  handleToggleMouse(who) {
    if (this.state.expanded && who === "enter") {
      return;
    }
    if (!this.state.expanded && who === "leave") {
      return;
    }
    this.handleToggle();
  }
  // handleSelect(eventKey) {
  //     this.setState({
  //         activeKey: eventKey
  //     });
  // }
  render() {
    const { expanded } = this.state;
    const { activeKey, handleSelect } = this.props;
    return (
      <div style={{ width: 250 }}>
        <Sidenav
          appearance="default"
          expanded={expanded}
          defaultOpenKeys={["3", "4"]}
          activeKey={activeKey}
          onSelect={handleSelect}
          onChange={this.handleToggle}
          onMouseEnter={() => this.handleToggleMouse("enter")}
          onMouseLeave={() => this.handleToggleMouse("leave")}
          style={{
            background: "white",
            justifyContent: "center",
            position: "absolute",
            top: 1,
            width: expanded && 250,
          }}
        >
          <Sidenav.Body>
            <Nav>
              <Nav.Item
                componentClass={Link}
                to="/"
                eventKey="1"
                icon={<Icon icon="dashboard" />}
              >
                Home
              </Nav.Item>
              <Nav.Item
                eventKey="6"
                componentClass={Link}
                to="/services"
                icon={<Icon icon="connectdevelop" />}
              >
                Connect services
              </Nav.Item>
              <Nav.Item
                eventKey="2"
                componentClass={Link}
                to="/create"
                icon={<Icon icon="plus-circle" />}
              >
                Create a workflow
              </Nav.Item>
              <Nav.Item
                eventKey="3"
                componentClass={Link}
                to="/widgets"
                icon={<Icon icon="dropbox" />}
              >
                Your workflows
              </Nav.Item>
              <Nav.Item
                eventKey="4"
                componentClass={Link}
                to="/profil"
                icon={<Icon icon="user" />}
              >
                Profil
              </Nav.Item>
              <Nav.Item
                eventKey="5"
                componentClass={Link}
                to="/logout"
                icon={<Icon icon="sign-out" />}
              >
                Logout
              </Nav.Item>
              {/* <Dropdown
                                placement="rightStart"
                                eventKey="4"
                                title="Settings"
                                icon={<Icon icon="gear-circle" />}
                            >
                                <Dropdown.Item componentClass={Link} to="/profil" icon={<Icon icon="user" />} eventKey="4-2">Profil</Dropdown.Item>
                                <Dropdown.Item componentClass={Link} to="/logout" icon={<Icon icon="sign-out" />} eventKey="4-3">Logout</Dropdown.Item>
                            </Dropdown> */}
            </Nav>
          </Sidenav.Body>
        </Sidenav>
      </div>
    );
  }
}
