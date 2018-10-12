# haskell-bazaar

## Deployment

### Frontend

The frontend assets are compiled and then uploaded to a S3 bucket
```
cd frontend
./deploy.sh
```

### Backend

* Build the environment builder image first
```
make docker-build-env
```
* Build the `haskell-bazaar-server` image
```
make docker-build
```

### Docker compose

* Run the services
```
docker-compose up
```

## Testing

```
make test
```
