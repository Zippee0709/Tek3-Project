-- CREATE DATABASE dashboard;

\c dashboard

CREATE TABLE Users (
    user_id SERIAL PRIMARY KEY not null,
    user_firstname varchar(128) null,
    user_lastname varchar(128) null,
    user_mail varchar(128) not null UNIQUE,
    user_password varchar(256) null,
    user_image varchar(256) null,
    user_google_access_token varchar(256) null,
    user_google_refresh_token varchar(256) null
);

CREATE TABLE Widgets (
    widget_id SERIAL PRIMARY KEY not null,
    widget_name varchar(256) not null UNIQUE,
    widget_description varchar(4000) not null,
    widget_image_url varchar(256) null,
    widget_extra varchar(256) null
);

CREATE TABLE Services (
    service_id SERIAL PRIMARY KEY not null,
    service_name varchar(256) not null UNIQUE,
    service_description varchar(4000) not null,
    service_image_url varchar(256) null,
    service_extra varchar(256) null
);

CREATE TABLE UserWidgetInstance (
    user_widget_instance_id SERIAL PRIMARY KEY NOT NULL,
    user_id int NOT NULL REFERENCES Users (user_id),
    widget_id int NOT NULL REFERENCES Widgets (widget_id),
    parameters varchar(2500) NOT NULL
);

CREATE TABLE UserServiceLink (
    user_service_link_id SERIAL PRIMARY KEY NOT NULL,
    user_id int NOT NULL REFERENCES Users (user_id),
    service_id int NOT NULL REFERENCES Services (service_id)
);

CREATE TABLE ServiceWidgetLink (
    service_widget_link_id SERIAL PRIMARY KEY NOT NULL,
    service_id int NOT NULL REFERENCES Services (service_id),
    widget_id int NOT NULL REFERENCES Widgets (widget_id)
);

INSERT INTO Services(service_name, service_description, service_image_url)
    VALUES ('Google', 'Everybody know him: Google', 'https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/1200px-Google_%22G%22_Logo.svg.png'),
    ('Weather', 'Weather informations', 'https://openweathermap.org/themes/openweathermap/assets/img/logo_white_cropped.png'),
    ('Images', 'Get an random image of your choice', 'https://images.vexels.com/media/users/3/143437/isolated/preview/390e394e1ea17f2b8361c8a16d88364e-magnifying-glass-simple-icon-by-vexels.png'),
    ('News', 'Stay informed about news', 'https://apogeedigital.com/wp-content/uploads/2015/10/the-new-york-times-logo-vert.png');

INSERT INTO widgets(widget_name, widget_description)
    VALUES ('Current weather', 'Access current weather for any location'),
    ('Hourly forecast', 'Hourly forecast is available for 5 days for any city'),
    ('Daily forecast', 'Access daily forecast for any location for anywhere'),
    ('Gmail Overview', 'Watch your email depend on your selection'),
    ('Image Viewer', 'See some random content about what you want to see ! '),
    ('Search News', 'Search your favorite subject with TheNewYorkTimes');

INSERT INTO ServiceWidgetLink(service_id, widget_id) VALUES (2, 1), (2, 2), (2, 3), (1, 4), (4, 6), (3, 5);