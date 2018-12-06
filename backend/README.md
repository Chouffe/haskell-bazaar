# haskell-bazaar

## Development

Building the development Dockerfile `Dockerfile.dev` will take a long time because it needs to compile all haskell packages. Run the `docker-compose up` command and go get a coffee or two while it compiles...

* To get a working dev environment run the following command to start the database and the API:
```
docker-compose up
```
* Start a ghci repl
```
make repl
```
* Lint the code
```
make lint
```
* Connect to the database with psql
```
make psql
```
* Run tests
```
make test
```

## Deployment

### Frontend

The frontend assets are compiled and then uploaded to a S3 bucket
```
cd frontend
./deploy.sh
```

### Backend

* Build the production docker image
```
make build
```
* Run the deploy script: TODO

## Testing

```
make test
```
