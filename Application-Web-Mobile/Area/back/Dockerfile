FROM node:alpine

WORKDIR '/app'

COPY package.json .
RUN npm install nodemon -g
RUN npm install bcrypt
RUN npm install twilio
RUN npm install

COPY . .

CMD ["nodemon", "-L", "server.js"]


#docker run -it -p 3000:3000 -v app/node_modules -v $(pwd):/app 8a5f85458af7
#append tests to docker compose
#docker exec -it container_id npm run test

