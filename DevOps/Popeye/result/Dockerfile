FROM node:12-alpine

WORKDIR /result
COPY package.json /result
RUN npm install
COPY . /result

EXPOSE 80
CMD ["node", "server.js"]