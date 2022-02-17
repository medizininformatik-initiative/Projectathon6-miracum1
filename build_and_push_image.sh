#!/bin/bash
set -e 
set -o pipefail

## Get version tag and registry-prefix from .env:
source ./.env

## Should the docker building process build without caching? (true/false)
docker_build_no_cache=false

printf "\n\n##################################\n"
printf "Building images with version tag $VERSION_TAG"
printf "\n##################################\n"

printf "\n\nPlease insert your login credentials to registry: $REGISTRY_PREFIX ...\n"
docker login

IMAGE_NAME=projectathon6-miracum1
printf "\n\n##################################\n"
printf "$IMAGE_NAME"
printf "\n##################################\n"
printf "\nPulling cached $IMAGE_NAME image\n"
## Pull latest image for caching:
docker pull $REGISTRY_PREFIX/$IMAGE_NAME
## Build new image (latest):
printf "\n\nBuilding $REGISTRY_PREFIX/$IMAGE_NAME image (latest):\n"

docker build \
    --progress=plain \
    --no-cache=${docker_build_no_cache} \
    --label "org.label-schema.name=joundso/$IMAGE_NAME" \
    --label "org.label-schema.vsc-url=https://github.com/joundso/Projectathon6-miracum1" \
    --label "org.label-schema.vcs-ref=$(git rev-parse HEAD)" \
    --label "org.label-schema.version=$(date -u +'%Y-%m-%dT%H:%M:%SZ')" \
    -f ./Dockerfile \
    -t $REGISTRY_PREFIX/$IMAGE_NAME . 2>&1 | tee ./log_$IMAGE_NAME.log

printf "\n\nPushing $IMAGE_NAME image (latest)\n"
## Push new image as new 'latest':
docker push "$REGISTRY_PREFIX/$IMAGE_NAME"

## also tag it with the new tag:
docker tag $REGISTRY_PREFIX/$IMAGE_NAME $REGISTRY_PREFIX/$IMAGE_NAME:$VERSION_TAG
## and also push this (tagged) image:
printf "\n\nPushing $IMAGE_NAME image ($VERSION_TAG)\n"
docker push "$REGISTRY_PREFIX/$IMAGE_NAME:$VERSION_TAG"

echo 'Hooray :-)'
exit 0
