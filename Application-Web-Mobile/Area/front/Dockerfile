FROM node:alpine

WORKDIR '/app'

COPY package.json .
RUN npm install -g npm@7.5.4
RUN npm install --legacy-peer-deps 

COPY . .

CMD cp -r /workspace/static/release/ /app/public ; npm run start

#docker run -it -p 3000:3000 -v app/node_modules -v $(pwd):/app 8a5f85458af7
#append tests to docker compose
#docker exec -it container_id npm run test

